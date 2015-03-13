;;
;;  STR  -  minimal library to ease strings rendering
;;
;;  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :str)

;;  Char

(defmacro case-char (char &body cases)
  (let ((g!char (gensym "CHAR-")))
    `(let ((,g!char ,char))
       (cond
	 ,@(mapcar (lambda (case-decl)
		     (destructuring-bind (match &rest match-body) case-decl
		       (typecase match
			 (string `((find ,g!char ,match) ,@match-body))
			 (null `((null ,g!char) ,@match-body))
			 (t `(,match ,@match-body)))))
		   cases)))))
