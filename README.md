STR
===

STR is a minimal library to ease strings rendering in Common Lisp.

Alternatives
------------

Reviewing some options for rendering multiple strings into one, we find
it quite difficult to keep the sources short and easy to read.

    (let ((name "world")
          (answer 42))
      (string=
    
	;; Concatenate only works with strings
        (concatenate 'string "Hello " name " ! The answer is " answer))
    
        ;; Format has some nice syntax
        (format nil "The answer is ~A." value)
    
        ;; Eventually it comes down to this
        (with-output-to-string (s)
          (write-string "Hello " s)
          (write-string name s)
          (write-string " ! The answer is " s)
          (princ answer s))
    
        ;; STR
	(str "Hello " world " ! The answer is " s)))

License
-------

Copyright 2013,2014 LowH <code@lowh.net>

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
