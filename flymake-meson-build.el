;;; flymake-meson-build.el --- Flymake integration for Meson build system -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alain Delplanque

;; Maintainer: Alain Delplanque <alaindelplanque@mailoo.org>
;; Author: Alain Delplanque <alaindelplanque@mailoo.org>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords:
;; URL: https://github.com/adelplanque/flymake-meson-build

;; This file is NOT part of GNU Emacs.

;;; Licence:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates Flymake with the Meson build system.
;; It extracts compilation commands from Ninja (Meson's backend)
;; using `ninja -t commands` and sets `flymake-cc-command` accordingly,
;; enabling accurate on-the-fly syntax checking.

;; Usage:

;; (require 'flymake-meson-build)
;; (add-hook 'c-mode-common-hook #'flymake-meson-build-setup)
;; (add-hook 'c-mode-common-hook #'flymake-mode)

;; Configuration:

;; `flymake-meson-build-builddirs` is a list of relative directory names
;; where Meson's build output may reside, searched from the project root
;; (the directory containing `meson.build`).

;; By default, this list is '("build" "builddir").

;; To customize globally, add in your init.el:
;;
;; (setq flymake-meson-build-builddirs '("build" "builddir" "custom-build"))

;; To customize per-project, create a `.dir-locals.el` file at the project root:
;;
;; ((c++-mode . ((flymake-meson-build-builddirs . ("custom-build" "builddir")))))
;; ((c-mode   . ((flymake-meson-build-builddirs . ("custom-build" "builddir")))))

;; Known limitations:

;; * Does not allow direct check of header files.
;; * Has only been tested on c and c++ projects in a Linux environment

;;; Code:

(require 'flymake)

(defgroup flymake-meson-build nil
  "Flymake backend for the Meson build system."
  :group 'flymake)

(defcustom flymake-meson-build-builddirs '("build" "flymake-meson-build-builddir")
  "List of possible Meson build directories relative to the project root."
  :type '(repeat string)
  :group 'flymake-meson-build)

(defvar-local flymake-meson-build-topsrcdir nil
  "Root directory of the meson project.")

(defvar-local flymake-meson-build-builddir nil
  "Build directory of the meson project.")

(defconst flymake-meson-build--mode-language-alist
  '((c-mode . "c")
    (c++-mode . "c++"))
  "An alist associating Emacs mode with gcc language.")

(defun flymake-meson-build--topsrcdir ()
  "Return top src directory for meson project."
  (let ((filename (buffer-file-name)))
    (when filename
      (let ((dir (file-name-directory filename)))
        (while (and (not (equal dir "/"))
                    (not (file-exists-p (concat dir "meson.build"))))
          (setq dir (file-name-directory (directory-file-name dir))))
        (when (file-exists-p (concat dir "meson.build"))
          (while (and (not (equal dir "/"))
                      (file-exists-p (concat (file-name-directory (directory-file-name dir))
                                             "meson.build")))
            (setq dir (file-name-directory (directory-file-name dir))))
          dir)))))

(defun flymake-meson-build--get-builddir (topsrcdir)
  "Search for the Meson build directory for the given TOPSRCDIR.

Combine TOPSRCDIR with each entry in `flymake-meson-build-builddirs`,
and return the first existing directory found.  Return nil if none exists."
  (when (and topsrcdir
             (listp flymake-meson-build-builddirs))
    (seq-find
     #'file-directory-p
     (mapcar (lambda (dir)
               (expand-file-name dir topsrcdir))
             flymake-meson-build-builddirs))))

(defun flymake-meson-build--get-ninja-command ()
  "Try to determine compile command to be used to compile current buffer file."
  (let* ((builddir flymake-meson-build-builddir)
         (filename (file-relative-name (buffer-file-name) flymake-meson-build-builddir)))
    (with-temp-buffer
      (call-process "ninja" nil t nil "-C" builddir "-t" "commands")
      (goto-char (point-min))
      (if (search-forward filename nil t)
          (buffer-substring (line-beginning-position) (line-end-position))))))

(defun flymake-meson-build--cc-command ()
  "Try to determine command to check syntax."
  (let ((cmd (flymake-meson-build--get-ninja-command)))
    (unless cmd (user-error "Cannot determine build command"))
    (let* ((filename (file-relative-name (buffer-file-name) flymake-meson-build-builddir))
           (lang (alist-get major-mode flymake-meson-build--mode-language-alist))
           (toks (append `("env" "-C" ,flymake-meson-build-builddir)
                         (seq-filter (lambda (x) (not (string= x filename)))
                                     (split-string cmd))
                         `("-fsyntax-only" "-x" ,lang "-fno-diagnostics-color" "-"))))
      toks)))

;;;###autoload
(defun flymake-meson-build-setup (&optional buf)
  "Setup flymake for meson build system in buffer BUF or current buffer.
When the file belongs to a meson project, adapt the `flymake-cc-command'
variable to dynamically determine the compilation command from the ninja rules
file.  Does nothing if the file does not appear to be part of a meson project."
  (interactive)
  (unless buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (unless flymake-meson-build-topsrcdir
      (setq flymake-meson-build-topsrcdir (flymake-meson-build--topsrcdir)))
    (unless flymake-meson-build-builddir
      (setq flymake-meson-build-builddir
            (flymake-meson-build--get-builddir flymake-meson-build-topsrcdir)))
    (if flymake-meson-build-builddir
        (setq-local flymake-cc-command #'flymake-meson-build--cc-command)
      (message "Unable to determine the build directory for the Meson project"))))

(provide 'flymake-meson-build)
;;; flymake-meson-build.el ends here
