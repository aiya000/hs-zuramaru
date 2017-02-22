# eliningen
eliningen (エリ人間) is a lisp processor :smile:

`leiningen` + `Eli Ayase` = `eliningen` :thinking:


# First goals
- [ ] literals
    - [ ] integer
        - [ ] natural number
            - `10`
            - `+10`
        - [ ] negative number
            - `-10`
    - [ ] float
        - `1.0`
        - `+1.0`
        - `-2.0`
    - [ ] boolean
        - `true`
        - `false`
    - [ ] character
        - `'a'`
        - `'あ'`
    - [ ] string
        - `"abc"`
        - `"あ\nb"`
    - [ ] quote
        - `'1`
        - `'(1 2)`
        - `'(1 (+ 2 3))`
- [ ] macros
    - [ ] let
    - [ ] defun
        - `(defun foo () (+ 1 2))`
        - `(defun bar (a b) (+ a b))`
    - [ ] defvar
        - `(defvar *baz* 10)`
        - `(defvar *boo* (+ 10 20))`
    - [ ] lambda
        - `(lambda () 1)`
        - `(lambda (a b c) '(c b a))`
- [ ] functions
    - [ ] print
    - [ ] concat
    - [ ] +
    - [ ] -
    - [ ] *
    - [ ] /

# Future
- literals
    - dictionary
    - float exponential notation
    - integer hexadecimal notation
- functions
    - recurse
- comment
