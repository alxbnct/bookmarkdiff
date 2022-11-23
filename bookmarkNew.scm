(import (prefix html-parser hp:))
(import srfi-1 srfi-69 srfi-13 srfi-14)

(define (html->alist->stdout filename)
  (define links '(#f))
  (set! links '(#f))
  (define html->txt
    (let* (
	     (parse (hp:make-html-parser
	       'start: (lambda (tag attrs seed virtual?)
			 (when (not (null? attrs))
			   (let ((link (assq 'href attrs)))
			     (set! links (append links (if (not (eq? link #f))
							   (list (cdr link))
							   '()))))))
	       'end: (lambda (tag attrs pseed seed virtual?) seed)
	       'text: (lambda (text seed)
			(when (not (null? links))
			  (when (and (not (eq? (last links) #f))
				     (not (string=? ""
						    (string-trim-right text
						     (or char-set:whitespace #\newline #\tab)))))
			    (append! (last links) (list text))))))))
      (lambda o
	(apply parse '() o)
	(cdr links))))
  (with-input-from-file filename
    (lambda () (html->txt (current-input-port))) #:text))
