# [GL-]TK Omni for Common Lisp


## Table of Contents

1. [Introduction](#introduction)
2. [File contents](#file-contents)
3. [Getting started](#getting-started)
4. [The TK-USER package](#the-tk-user-package)
5. [The GLT package](#the-glt-package)
6. [Example deployment](#example-deployment)
7. [License information](#license-information)


## Introduction

_GL-TK Omni_ is a Common Lisp system extending [Andrej Vodopivec's
**_CL-SIMPLE-TK_** package](https://github.com/andrejv/cl-simple-tk)
with a few adjustments, some additional functions and macros found in
a new `TK-USER` package, and a `GLT` package [currently only for MS
Windows] with the experimental ability to embed OpenGL (using [3b's
**_CL-OPENGL_** package](https://github.com/3b/cl-opengl)) into Tk
canvases spawned by the system.


## File contents

This repository contains the ASDF system `CL-GL-TKO` and a directory
of required files and libraries (currently targeting Win64*) that may
be placed into the main path of any project to include the CL-GL-TKO
package.  These dependencies include `Tcl/Tk 8.6 binaries` [that were
compiled by the author] for Win64, a `lib/` directory containing both
standard Tcl/Tk libraries and the Tk `Img` module (also for Win64),
and an `images/` directory used by the included Tk demos [accessible
with `(tk-user:list-demos)`.  If OpenGL is available on the user's
platform, experimental support is demonstrated with `(glt:test-glt)`].

_*_ *The package may be used with Tcl/Tk binaries and libraries on other
platforms if they are supplied by the user in place of those included
here.  OpenGL integration with the GLT package is currently only
implemented for Windows, and will not work on other systems.*


## Getting started

To get started, copy the `cl-gl-tko` folder from
`asdf-package-dir-files` to a location where the local ASDF
installation can find it.  Additionally, copy all the items from
`project-dir-files-win64` (or their equivalents if using a platform
other than Win64) to a lisp project working directory (typically the
project's main folder).  Ensure the :simple-tk package is available to
ASDF, as well as :cl-opengl (the latter may be loaded with Quicklisp).
Now, GL-TK Omni may be loaded with `(asdf:load-system :cl-gl-tko)`.
This will provide the `tk-user`, `glt`, and `demopanels` packages and
enable calling the demonstration functions mentioned above.


## The TK-USER package

In addition to mirroring all the exported symbols from CL-SIMPLE-TK,
the TK-USER package contains some additional functionality in the form
of a few lisp-flavored wrappers to Tcl/Tk operations (an attempt was
made to keep some consistency with the ideas of CL-SIMPLE-TK but it
may not have been totally successful) and some macros which help
demonstrate applications.  A handful of Tk demos have been translated
into Common Lisp to further illustrate use of the library, available
with `(tk-user:list-demos)`.  This package aims for cross-platform
compatibility, although currently it has only been tested under
Windows 10 using 64-bit Tcl/Tk binaries compiled by the author with
Visual Studio 2015 (available from this repository).

Following is a list of other new symbols available from this package:
```
font-deformat 
font-exists 
grid-remove 
proc-exists
style-configure 
style-lookup 
textconsole 
title-bar-offset 
update
update-idletasks 
wgeom 
parse-wgeom 
window-attributes
window-configure-fn 
window-delete-fn 
window-overrideredirect
window-parent 
window-root 
window-rootp 
window-toplevelp
window-wait-visibility 
window-winfo-id 
window-winfo-ismapped
window-winfo-viewable 
button-panel 
with-button-panel 
with-debug-window
with-main-window 
with-mfd 
with-scrollable-frame 
with-toplevel-or-root
```


## The GLT package

Under Windows, it is possible to use the GLT package to set up a
rendering context for OpenGL and bind it to the window display
context[s] of one or more Tk canvases. A draw function may be tied
into the Tcl/Tk mainloop by looping it in calls to `#'tk:after`,
allowing the user to write rendering code using CL-OPENGL. A canvas is
activated as the current GL display context with
`window-gl-set-current`, and rendered results are swapped onto the
canvas display with `(glt:window-swap-buffers <canvas>)` or
`(glt:swap-all-buffers)`.

Following is a list of symbols exported from this package:
```
:window-swap-buffers
:window-gl-set-current
:swap-all-buffers
:window-bind-gl
```


## Example deployment

The `example-deployment` archive contains these files, copies of ASDF
and all the required system dependencies, and a `package.lisp` file
that shows how everything may be deployed to a single directory.  On a
64-bit Windows platform, users can load `package.lisp` and then should
be able to immediately call `(asdf:load-system :cl-gl-tko)` without
setting up any other files or paths.  The handful of Tk demos that have
been ported to this system may be subsequently viewed with 
`(tk-user:list-demos)` (these do not require Windows or OpenGL).


## License information

GL-TK Omni for Common Lisp is (c) Copyright 2018 Darren W. Ringer
<dwringer@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
