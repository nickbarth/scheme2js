(use srfi-1)
(use srfi-13)
(use utils)
(use test)
(use regex)
(use srfi-9) ;; match

(define-record token name type)

; lexemes : string -> []string
(define (lexemes str)
  (string-split 
    (string-substitute "\\)" " ) "
    (string-substitute "\\(" " ( " str #f) #f)))
(test "lexemes" (lexemes "(print 1 2 3)")  '( "(" "print" "1" "2" "3" ")" ))

; tokenizer : []string -> []token
(define (tokenizer str)
  (map (lambda (l)
    (cond
      ; punctuation
      ((string= l "(") '('l_bracket))
      ((string= l ")") '('r_bracket))
      ((string= l "'") '('list))

      ((string= l "#t") '('true))
      ((string= l "#f") '('false))

      ((string= l "+") '('plus))
      ((string= l "-") '('minus))
      ((string= l "/") '('divide))
      ((string= l "*") '('times))


      ; literals 
      ((string= l "*") '('times))

      (else  1)))
   (lexemes str)))
(tokenizer "(1)")


; tokenize : []string hash string
(define (tokenize chars mode tokens)
  (if (null? chars) tokens
    (let ((ch (car chars)))
      (cond
        ; whitespace
        ((and (eq? 'read-char mode) (string= ch " "))
          (tokenize (cdr chars) 'read-char (append '('whitespace) tokens)))
        ; punctuation
        ((and (eq? 'read-char mode) (string= ch "("))
          (tokenize (cdr chars) 'read-char (append '('l_bracket) tokens)))
        ((and (eq? 'read-char mode) (string= ch ")"))
          (tokenize (cdr chars) 'read-char (append '('r_bracket) tokens)))
        (else
          (tokenize (cdr chars) 'read-char (append '('invalid) tokens)))))))
(tokenize (string-chop "(1)" 1) 'read-char '())


; scanner : string -> []struct{ hash, value }
(define (scanner str)
  (tokenize #f 'read-char (string-chop str 1) '()))
(scanner "(1)")
