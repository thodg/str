# STR

STR is a minimal library to ease strings rendering in Common Lisp.

## Alternatives

Reviewing some options for rendering multiple strings into one, I found
it difficult to keep the sources short and easy to read.

```lisp
(let ((name "world")
      (answer 42))
  (string=

    ;; Concatenate only works with strings
    (concatenate 'string "Hello " name " ! The answer is " answer "."))

    ;; Format has some nice syntax
    (format nil "Hello ~A ! The answer is ~D." name value)

    ;; Eventually it comes down to this
    (with-output-to-string (s)
      (write-string "Hello " s)
      (write-string name s)
      (write-string " ! The answer is " s)
      (princ answer s)
      (write-char #\. s))

    ;; Short, readable and fast :
    (str "Hello " world " ! The answer is " answer "."))
```

## The STR dictionnary

#### Generic ATOM-STR

**Syntax :**
`(defmethod atom-str ((atom TYPE)) ...)`

Renders a string for an atom.

Specialize this function if you want to use a string rendering
specific to STR. This is generally not needed as `atom-str` uses
the Lisp printer by default with a number of differences :
* Numbers are rendered in decimal form.
* Symbols are rendered downcased.
* Pathnames are rendered using `namestring`.


#### Macro CASE-CHAR

**Syntax :**
`(case-char char &body cases)`

Like the `case` macro, additionally accepts strings for keys.

**Examples**
```lisp
(case-char #\c
  ("abc" 'abc)
  ("123" '123)
  (" " t))
=> 'ABC
```


#### Function ROPE-MERGE

**Syntax :**
`(rope-merge rope)`  =>  *merged-rope, merged-p*

`rope-merge` returns a fresh rope where all litteral components are
converted to strings using `atom-str` and contiguous strings are merged.
Non keyword symbols are left untouched. If no merging occured then `rope`
is returned untouched.

```lisp
(str:rope-merge '("Hello" "," #\Space world " ! " 1000 " " 'greetings " to you."))
=> ("Hello, " WORLD " ! 1000 greetings to you.")
   T
```


#### Function ROPE-NMERGE
**Syntax :**
`(rope-nmerge rope)`  =>  *merged-rope, merged-p*

`rope-nmerge` is like `rope-merge` except that `rope` is modified
destructively.


#### Function STR

**Syntax :**
`(str &rest parts)`  =>  *string*

Assemble `parts` into a string by walking down recursively using `walk-str`.
Each part is converted into a string using `atom-str`.

Litteral parts like strings, numbers and keywords are merged at compilation
time using `rope-merge`.


#### Function WALK-STR

**Syntax :**
`(walk-str fn &rest parts)`

Walk down `parts` recursively, applying `fn` to each atom it encounters.
Atoms are converted to string using `atom-str` before being passed to `fn`.

This function is useful to avoid consing temporary strings. It processes a
whole tree of strings without consing at all.


#### Function WRITE-STR

**Syntax :**
`(write-str stream &rest parts)`  =>  *string*

Write `parts` into `stream` by walking down recursively using `walk-str`.
Each part is converted into a string using `atom-str`.

Litteral parts like strings, numbers and keywords are merged at compilation
time using `rope-merge`.


## License

Copyright 2013,2014,2015 Thomas de Grivel <thoxdg@gmail.com>

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
