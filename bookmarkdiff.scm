(import (prefix html-parser hp:)
        srfi-1 srfi-69 srfi-13 srfi-14
        srfi-19-date srfi-19-io
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
			  (let ((trimed-text (string-trim-right text
								(or char-set:whitespace #\newline #\tab))))
			    (cond ((and (not (eq? (last links) #f))
					(not (string=? "" trimed-text)))
				   (append! (last links) (list text)))
				  ((and (string=? "" trimed-text)
					(= (length (last links)) 1))
				   (append! (last links) (list "Untitled")))))
			  #;
			  (when (and (not (eq? (last links) #f)) ;
			  (not (string=? "" ;
			  (string-trim-right text ;
			  (or char-set:whitespace #\newline #\tab))))) ;
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
    (remove-dup h2 h1))

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

(define-syntax dotimes
  (syntax-rules ()
    ((_ (index maxval) body ...)
     (do ((index 0 (+ 1 index)))
         ((= index maxval))
       body ...))))

;; write-hash-table-diff->file :: stirng -> hash-table -> file
(define (write-hash-table-diff->file filename diff dir)
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

EOF
)
  (with-output-to-file filename
    (lambda ()
      (display meta)
       (if (eq? dir #f)
         (display (conc "<DL><p>\n<DT><H3>" (format-date "~Y-~m-~d" (current-date))
                        "</H3>\n<DL><p>\n"))
         (let ((dir (string-split dir "/")))
	   (display (conc "<H1>" (car dir) "</H1>\n"))
           (let lp ((dir (cdr dir)))
             (when (not (eq? dir '()))
               (display (conc  "<DL><p>\n<DT><H3>" (car dir) "</H3>\n"))
               (lp (cdr dir))))))
       (let lp ((diff-alist (hash-table->alist diff)))
         (if (null? diff-alist)
             (void)
             (begin
               (let ((link (caar diff-alist))
                     (desc (cadar diff-alist)))
                 (display (conc "\n<DT><A HREF=\"" link "\">" desc "</A>")))
               (lp (cdr diff-alist)))))
       (if (not (eq? dir #f))
           (let ((dir (string-split dir "/")))
             (dotimes (i (length dir))
               (display "\n</DL><p>")))
         (display "\n</DL><p>\n</DL>\n")))
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
                        (predicate ,string?)))
    (directory "Directory for new bookmarks"
                        (required #f)
                        (value #t)
                        (single-char #\d)
                        (value (required DIR)
                               (predicate ,string?)))))

(define (main)
  (let* ((options (cdr (parse-command-line-options)))
         (input-file-a (alist-ref 'input-file-a options)) ; The target I want to sync to
         (input-file-b (alist-ref 'input-file-b options)) ; The source to sync
         (output-filename (let ((o (alist-ref 'output-file options)))
                            (cond ((eq? o #f)
                                   (if (file-exists? "output.html")
                                       (begin (display "File `output.html` exists, select another name via -o flag\n"
                                                       (current-error-port))
                                              (exit 1))
                                       "output.html"))
                                  ((file-exists? o)
                                   (begin (display "File exists, exiting\n" (current-error-port))
                                          (exit 1)))
                                  (else o))))
         (directory (alist-ref 'directory options))
         (h1 (condition-case
                 (alist->hash-table (html->link-alist input-file-a))
               ((exn file) (begin (display (conc "File " input-file-a " does not exit\n"))
                                  (exit 1)))))
         (h2 (condition-case
                 (alist->hash-table (html->link-alist input-file-b))
               ((exn file) (begin (display (conc "File " input-file-b " does not exit\n"))
                                  (exit 1)))))
         (diff (hash-table-remove-duplicate h1 h2)))
    (write-hash-table-diff->file output-filename diff directory)))

(define (parse-command-line-options)
  (condition-case
    (getopt-long (command-line-arguments)
                      grammar)
    ((exn) (begin (display (conc "Usage:\n" (usage grammar)))
                  (exit 1)))))

(main)
