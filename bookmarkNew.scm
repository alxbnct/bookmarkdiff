(import (prefix html-parser hp:)
        srfi-1 srfi-69 srfi-13 srfi-14
        (chicken process-context)
        (chicken file)
        (chicken file posix)
        (chicken string)
        getopt-long)

(define (html->link-alist filename)
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

;; write-hash-table-diff->file :: stirng -> hash-table -> file
(define (write-hash-table-diff->file filename diff)
  ;; NOTICE: The terminated here-doc string EOF should be in one line without
  ;; other charactors, for example `EOF)` is not terminating the here-doc stirng.
  (define meta #<<EOF
<!DOCTYPE NETSCAPE-Bookmark-file-1>
<!-- This is an automatically generated file.
   It will be read and overwritten.
   DO NOT EDIT! -->
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<meta http-equiv="Content-Security-Policy" content="default-src 'self'; script-src 'none'; img-src data: *; object-src 'none'"></meta>
<TITLE>Bookmarks</TITLE>
<H1>Bookmarks Menu</H1>

<DL><p>
<DT><H3>Mozilla Firefox</H3>
<DL><p>
EOF
)
  (with-output-to-file filename
    (lambda () (display meta)
      (let lp ((diff-alist (hash-table->alist diff)))
        (if (null? diff-alist)
            (void)
            (begin
              (let ((link (caar diff-alist))
                    (desc (cadar diff-alist)))
                (display "\n<DT><A HREF=\"")
                (display link)
                (display "\">")
                (display desc)
                (display "</A>"))
              (lp (cdr diff-alist)))))
      (display "\n</DL><p>\n</DL>"))
    #:text))

(define grammar
  `((input-file-a "Input file a"
                  (required #t)
                  (value #t)
                  (single-char #\a)
                  (value (required FILE)
                         (predicate ,string?)))
    (input-file-b "Input file b"
                  (required #t)
                  (value #t)
                  (single-char #\b)
                  (value (required FILE)
                         (predicate ,string?)))
    (output-file "Output file name"
                 (required #f)
                 (value #t)
                 (single-char #\o)
                 (value (required FILE)
                        (predicate ,string?)))))

(define (main)
  (let* ((options (cdr (parse-command-line-options)))
         (input-file-a (alist-ref 'input-file-a options))
         (input-file-b (alist-ref 'input-file-b options))
         (output-filename (let ((o (alist-ref 'output-file options)))
                            (cond ((eq? o #f)
                                   (if (file-exists? "output")
                                       (begin (display "File `output` exists, select another name via -o flag\n"
                                                       (current-error-port))
                                              (exit 1))
                                       "output"))
                                  ((file-exists? o)
                                   (begin (display "File exists, exiting\n" (current-error-port))
                                          (exit 1)))
                                  (else o))))
         (h1 (condition-case
                 (alist->hash-table (html->link-alist input-file-a))
               ((exn file) (begin (display (conc "File " input-file-a " does not exit\n"))
                                  (exit 1)))))
         (h2 (condition-case
                 (alist->hash-table (html->link-alist input-file-b))
               ((exn file) (begin (display (conc "File " input-file-b " does not exit\n"))
                                  (exit 1)))))
         (diff (hash-table-remove-duplicate h1 h2)))
    (write-hash-table-diff->file output-filename diff)))

(define (parse-command-line-options)
  (condition-case
    (getopt-long (command-line-arguments)
                      grammar)
    ((exn) (begin (print (usage grammar))
                  (exit 1)))))

(main)
