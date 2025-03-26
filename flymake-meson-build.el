;;; flymake-meson-build.el --- Flymake for meson build system -*- lexical-binding: t -*-

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

;; Determines the compilation command to be used by flymake for a meson project.

;; flymake-meson-build first finds the build command used by ninja to build the
;; file, and then adapts it for use by flymake.

;; Usage:

;; (require 'flymake-meson-build)
;; (add-hook 'c-mode-common-hook #'flymake-meson-build-setup)
;; (add-hook 'c-mode-common-hook #'flymake-mode)

;; Configuration:

;; By default flymake-meson-build considers that the project's build directory
;; is "build".  To indicate another directory, you must modify the
;; `flymake-meson-build-builddir' variable, for example by including a
;; .dir-local.el file at the root of the project:

;; ((c++-common . ((flymake-meson-build-builddir . "custom-builddir"))))

;; Known limitations:

;; * Does not allow direct check of header files.
;; * Has only been tested on c and c++ projects in a linux environment

;;; Code:

(require 'flymake)

(defvar-local flymake-meson-build-topsrcdir nil
  "Root directory of the meson project.")
(defvar-local flymake-meson-build-builddir "build"
  "Relative path of meson builddir to the meson root project directory.")

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

(defun flymake-meson-build--get-ninja-command ()
  "Try to determine compile command to be used to compile current buffer file."
  (let* ((builddir (concat flymake-meson-build-topsrcdir flymake-meson-build-builddir))
         (filename (file-relative-name (buffer-file-name) builddir)))
    (with-temp-buffer
      (call-process "ninja" nil t nil "-C" builddir "-t" "commands")
      (goto-char (point-min))
      (if (search-forward filename nil t)
          (buffer-substring (line-beginning-position) (line-end-position))))))

(defun flymake-meson-build--cc-command ()
  "Try to determine command to check syntax."
  (let ((cmd (flymake-meson-build--get-ninja-command)))
    (unless cmd (user-error "Cannot determine build command"))
    (let* ((builddir (concat flymake-meson-build-topsrcdir flymake-meson-build-builddir))
           (filename (file-relative-name (buffer-file-name) builddir))
           (lang (alist-get major-mode flymake-meson-build--mode-language-alist))
           (toks (append `("env" "-C" ,builddir)
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
    (if flymake-meson-build-topsrcdir
        (set (make-local-variable 'flymake-cc-command) #'flymake-meson-build--cc-command)
      (message "Cannot determine root directory for a meson project"))))

(provide 'flymake-meson-build)
;;; flymake-meson-build.el ends here
