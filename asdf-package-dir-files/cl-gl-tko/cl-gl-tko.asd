;; GL-TK Omni for Common Lisp [CL-GL-TKO]
;; Copyright 2018 Darren W. Ringer <dwringer@gmail.com>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
(asdf:defsystem :cl-gl-tko
    :description "GL-TK Omni - Extensions to CL-SIMPLE-TK w/OpenGL"
    :author      "Darren Ringer <dwringer@gmail.com>"
    :license     "MIT"
    :depends-on  (:simple-tk :cl-opengl)
    :components  ((:module "src"
			   :serial     t
			   :components ((:file "tk-user")
					#+win32 (:file "glt")
					(:file "demopanels")
					(:file "demos/label")
					(:file "demos/button")
					(:file "demos/unicodeout")
					(:file "demos/check")
					(:file "demos/radio")
					(:file "demos/puzzle")
					#+win32 (:file "demos/test")
					(:file "list-demos")))))
