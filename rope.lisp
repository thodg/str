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

;;  Ropes

(defun rope-merge (rope)
  (let (result last str merged)
    (labels ((collect (x)
	       (if last
		   (setf (cdr last) (cons x nil)
			 last (cdr last))
		   (setf last (cons x nil)))
	       (unless result
		 (setf result last))
	       (setf str nil))
	     (collect-str ()
	       (when str
                 (typecase str
                   (stream (collect (get-output-stream-string str))
                           (setf merged t
                                 str nil))
                   (t (collect str)
                      (setf str nil))))))

      (dolist (x rope)
	(when (or (characterp x)
                  (keywordp x)
                  (numberp x))
          (setq x (the string (atom-str x))))
	(when (and (consp x)
                   (eq 'quote (car x)))
          (setq x (the string (atom-str (cadr x)))))
	(cond ((stringp x)
	       (typecase str
		 (string (let ((w str))
			   (setf str (make-string-output-stream))
			   (write-string w str))
			 (write-string x str))
		 (stream (write-string x str))
		 (null (setf str x))))
	      ((null x)
	       (setf merged t))
	      (t
	       (collect-str)
	       (collect x))))
      (collect-str)
      (if merged
	  (values result t)
	  (values rope nil)))))

(defun rope-nmerge (rope)
  (labels ((iter (x)
             (let ((y (cadr x)))
               (when (or (characterp y)
                         (keywordp y)
                         (numberp y))
                 (setq y (the string (atom-str y))))
               (when (and (consp y)
                          (eq 'quote (car y)))
                 (setq y (the string (atom-str (cadr y)))))
               (cond ((and (stringp (car x))
                           (stringp y))
                      (setf (car x) (concatenate 'string (car x) y)
                            (cdr x) (cddr x))
                      (iter x))
                     ((cdr x)
                      (iter (cdr x)))))))
    (iter rope))
  rope)

(defun write-rope (rope &optional (stream *standard-output*))
  (dolist (x rope)
    (write-string x stream)))

(define-compiler-macro write-rope (&whole form rope &rest stream)
  (let ((merged (rope-merge rope)))
    (if (eq merged rope)
	form
	`(write-rope ,merged ,@stream))))
