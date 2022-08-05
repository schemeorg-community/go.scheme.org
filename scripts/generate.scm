(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme read)
        (scheme write)
        (only (srfi 1) append-map filter-map)
        (only (srfi 13) string-prefix?))

(define (string-append-map proc str)
  (let loop ((i 0) (acc ""))
    (if (= i (string-length str)) acc
        (loop (+ i 1) (string-append acc (proc (string-ref str i)))))))

(define (read-all)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

(define (display-sxml x)
  (define (display* . xs) (for-each display xs))
  (define (display-char char)
    (let* ((cc (char->integer char))
           (ok? (case char ((#\& #\< #\> #\") #f) (else (<= #x20 cc #x7e)))))
      (if ok? (display char) (display* "&#" cc ";"))))
  (define (display-attribute attribute)
    (display* " " (car attribute) "=\"")
    (string-for-each display-char (cadr attribute))
    (display "\""))
  (cond ((pair? x)
         (display* "<" (car x))
         (let ((body (cond ((and (pair? (cdr x))
                                 (pair? (cadr x))
                                 (eq? '@ (car (cadr x))))
                            (for-each display-attribute (cdr (cadr x)))
                            (cddr x))
                           (else (cdr x)))))
           (display ">")
           (for-each display-sxml body)
           (unless (memq (car x) '(meta))
             (display* "</" (car x) ">"))))
        ((string? x)
         (string-for-each display-char x))
        (else (error "Bad:" x))))

(define (nginx-url-encode str)
  (string-append-map
   (lambda (char)
     (case char
       ((#\# #\% #\& #\+ #\? #\space)
        (let ((hex (string-upcase (number->string (char->integer char) 16))))
          (string-append "%" hex)))
       (else
        (string char))))
   str))

(define (nginx-double-quote str)
  (string-append
   "\""
   (string-append-map
    (lambda (char)
      (case char
        ((#\\ #\") (string #\\ char))
        (else (string char))))
    str)
   "\""))

(define (write-nginx-map-entry pair)
  (newline)
  (write-string (nginx-double-quote (nginx-url-encode (car pair))))
  (newline)
  (write-string (nginx-double-quote (cdr pair)))
  (write-string ";")
  (newline))

(define (head? obj head)
  (and (list? obj) (equal? head (car obj))))

(define (dehead obj head)
  (if (head? obj head) (cdr obj) (error "Expecting" head)))

(define (get-string alist property)
  (let loop ((alist alist) (value #f))
    (cond ((null? alist)
           (or value (error "No" property)))
          ((equal? property (caar alist))
           (if value
               (error "Duplicate" property)
               (let ((entry (car alist)))
                 (if (and (= 2 (length entry))
                          (equal? property (car entry))
                          (string? (cadr entry)))
                     (loop (cdr alist) (cadr entry))
                     (error "Not a string" property)))))
          (else
           (loop (cdr alist) value)))))

(define (normalize-uri uri)
  (if (string-prefix? "//" uri)
      (string-append "https:" uri)
      uri))

(define (entry->pair entry)
  (let ((entry (dehead entry 'entry)))
    (cons (get-string entry 'id)
          (normalize-uri (get-string entry 'uri)))))

(define (groups->map groups)
  (append-map (lambda (group)
                (let ((group (dehead group 'group)))
                  (filter-map (lambda (entry)
                                (and (head? entry 'entry)
                                     (entry->pair entry)))
                              group)))
              groups))

(define (group->html-section group)
  (let ((group (dehead group 'group)))
    `(section
      (h2 ,(get-string group 'title))
      (table
       (@ (class "files no-border"))
       ,@(filter-map (lambda (entry)
                       (and (head? entry 'entry)
                            (let* ((entry (dehead entry 'entry))
                                   (id (get-string entry 'id)))
                              `(tr (td (a (@ (href ,id))
                                          ,id))
                                   (td ,(get-string entry 'intent))))))
                     group)))))

(define (menu-sxml items)
  `(header
    (ul (@ (class "menu"))
        ,@(map (lambda (i) `(li (a (@ (href ,(cadr i))) ,(car i))))
               items))))

(define (main)
  (let ((groups (with-input-from-file "go.pose" read-all)))
    (with-output-to-file "nginx/map.conf"
      (lambda ()
        (for-each write-nginx-map-entry (groups->map groups))))
    (with-output-to-file "www/index.html"
      (lambda ()
        (display-sxml
         `(html
           (@ (lang "en"))
           (head
            (title "Go Scheme")
            (link (@ (rel "stylesheet") (href "/schemeorg.css")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1"))))
           (body
            ,(menu-sxml
              '(("Home" "//www.scheme.org/")
                ("Docs" "//docs.scheme.org/")
                ("Community" "//community.scheme.org/")
                ("Standards" "//standards.scheme.org/")
                ("Implementations" "//get.scheme.org/")))
            (h1 (@ (id "logo"))
                "Go Scheme")
            (p "A URL shortening and social bookmarking service.")
            ,@(map group->html-section groups)
            (hr)
            (p
             "Anyone may submit a new URL by making a pull request"
             " or by sending an email to the schemeorg mailing list."
             " Links should focus on technical content."))))))))

(main)
