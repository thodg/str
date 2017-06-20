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

;;  STR

(defgeneric atom-str (x))

(defmethod atom-str (x)
  (format nil "~A" x))

(defmethod atom-str ((x null))
  "")

(defmethod atom-str ((x symbol))
  (string-downcase (symbol-name x)))

(defmethod atom-str ((x character))
  (make-string 1 :initial-element x))

(defmethod atom-str ((x string))
  x)

(defmethod atom-str ((x pathname))
  (namestring x))

(defmethod atom-str ((x integer))
  (format nil "~D" x))

(defun walk-str (fn str)
  (labels ((walk (x)
             (typecase x
               (string (funcall fn x))
               (sequence (map nil #'walk x))
               (t (funcall fn (atom-str x))))))
    (walk str)))

(defun str (&rest parts)
  (with-output-to-string (s)
    (walk-str (lambda (x) (write-string x s))
              parts)))

(define-compiler-macro str (&whole form &rest parts)
  (let ((merged (rope-merge parts)))
    (if (eq merged parts)
        form
        `(str ,@merged))))

(defun write-str (stream &rest parts)
  (walk-str (lambda (x) (write-string x stream))
            parts))

(define-compiler-macro write-str (&whole form stream &rest parts)
  (let ((merged (rope-merge parts)))
    (if (eq merged parts)
        form
        `(write-str ,stream ,@merged))))

(defun join-str (glue &rest parts)
  (let (g)
    (with-output-to-string (out)
      (walk-str (lambda (x)
                  (when g
                    (write-str out glue))
                  (write-string x out)
                  (setq g t))
                parts))))

;;  SYM

(defun sym (&rest parts)
  (intern (with-output-to-string (s)
            (walk-str (lambda (x)
                        (write-string (string-upcase x) s))
                      parts))))

(defun kw (&rest parts)
  (intern (with-output-to-string (s)
            (walk-str (lambda (x)
                        (write-string (string-upcase x) s))
                      parts))
          :keyword))
