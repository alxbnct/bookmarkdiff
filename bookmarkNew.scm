(import (prefix html-parser hp:))
(import srfi-1 srfi-69 srfi-13 srfi-14)

(define (html->alist->stdout filename)
  (define html->txt
    (let* ((links '())
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
	links)))
  (with-input-from-file filename
    (lambda () (html->txt (current-input-port))) #:text))

;; Remove duplicate entries of two hash tables from the larger
;; hash table and return it
(define (hash-table-remove-duplicate h1 h2)
  (define (start-remove)
    (let ((hs1 (hash-table-size h1))
          (hs2 (hash-table-size h2)))
      (cond ((= hs1 hs2) (void))
            ((> hs1 hs2) (remove-dup h1 h2))
            ((< hs1 hs2) (remove-dup h2 h1)))))

  (define (remove-dup larger-ht smaller-ht)
   ;; (let ((ret (hash-table-copy larger-ht))	;This is equivalent as follows.
    (let ((ret larger-ht)
          (key-lst (hash-table-keys smaller-ht)))
      (let lp ((lst key-lst))
        (if (null? lst)
            ret
            (let ((key (car lst)))
              ;; (print key)
              (when (hash-table-exists? ret key)
                  (hash-table-delete! ret key))
              (lp (cdr lst)))))))

  (start-remove))
