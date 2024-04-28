Presentation
============

Determines the compilation command to be used by flymake for a meson project.

flymake-meson-build first finds the build command used by ninja to build the
file, and then adapts it for use by flymake.

Usage
=====

```lisp
(require 'flymake-meson-build)
(add-hook 'c-mode-common-hook #'flymake-meson-build-setup)
(add-hook 'c-mode-common-hook #'flymake-mode)
```

Configuration
=============

By default flymake-meson-build considers that the project's build directory
is "build".  To indicate another directory, you must modify the
`flymake-meson-build-builddir' variable, for example by including a
.dir-local.el file at the root of the project:

```lisp
((c++-common . ((flymake-meson-build-builddir . "custom-builddir"))))
```

Known limitations
=================

* Does not allow direct check of header files.
* Has only been tested on c and c++ projects in a linux environment
