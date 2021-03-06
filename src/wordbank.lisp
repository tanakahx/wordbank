#!/bin/sh
#|
exec sbcl --core $SBCL_CORE --script $0 $0 "$@"
|#

#|
  wordbank.lisp - main program

  The MIT License (MIT)
  
  Copyright (c) 2014 Hiroyuki Tanaka <h.tanaka.mail@gmail.com>
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  
  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package :cl-user)
(defpackage wordbank
  (:use :cl)
  (:export :query-item-list
           :query-meaning-list
           :new-db
           :main))
(in-package :wordbank)

(defun script-p ()
  "Return T if this code is executed in script mode."
  (member (pathname-name *load-truename*)
          sb-ext:*posix-argv*
          :test #'(lambda (x y) (search x y :test #'equalp))))

(when (script-p)
  (let* ((*standard-output* (make-broadcast-stream))
         (*error-output* *standard-output*))
    (ql:quickload "drakma")
    (ql:quickload "cl-ppcre")
    (ql:quickload "cl-fad")))

(defparameter *debug-output* nil)
(defparameter *prompt-message* "Input search word or :q to quit")
(defparameter *prompt-string* "WORDBANK> ")
(defparameter *top-n* 10)

;; Data base
(defparameter *db* (make-hash-table :test #'equal))
(defparameter *db-file* #P"~/.wordbank.db")

;; Entry point of this program
(defun main (&optional args)
  (declare (ignore args))
  (format t "~a~%" *prompt-message*)
  (break-loop (prompt-handler) #'prompt-read-line #'(lambda (x) (string= x ":q")) *prompt-string*))

(defun prompt-handler ()
  "State of this closure
IDLE:   Send a query of a requested word and print the titles of the word.
SELECT: Print the meaning of the requested word.
"
  (let ((state 'idle)
        items)
    (labels ((rec (in)
               (case state
                 (idle
                  (cond ((string= in ":w")
                         (save-db)
                         (return-from rec (format nil "Save DB in ~a." *db-file*)))
                        ((string= in ":t")
                         (return-from rec
                           (with-output-to-string (s)
                             (loop for x in (nthmost *top-n* *db*)
                                do (destructuring-bind (word (meaning . count)) x
                                     (format s (make-string 80 :initial-element #\-))
                                     (format s "~%(~a) ~a~%~a~%" count word meaning))))))
                        ((string= in "") (return-from rec "")))
                  (setf state 'select)
                  ;; When strict search mode, if the word is found in DB then pick it up from DB.
                  (unless (wildcard-p in)
                    (let ((meaning (search-meaning-list in)))
                      (when meaning
                        (update-meaning-count meaning)
                        (setf items (list (cons nil in))) ; a car part is dummy
                        (return-from rec (rec "0")))))
                  (setf items (query-item-list (search-word in)))
                  (if items
                      (if (or (= (length items) 1) (string= in (cdr (first items))))
                          (return-from rec (rec "0"))
                          (with-output-to-string (s)
                            (loop
                               for item in items
                               for i from 0
                               do (format s "[~2d] ~a~%" i (cdr item))
                               finally (format s "~%Select a number listed above (upto ~d)" i))))
                      (progn (setf state 'idle)
                             (format nil "No entry is found"))))
                 (select
                  (setf state 'idle)
                  (let* ((item (nth (parse-integer in) items))
                         (meanings (search-meaning-list (cdr item))))
                    (unless meanings
                      (format *debug-output* "New DB entry: \"~a\"~%" (cdr item))
                      (setf meanings (query-meaning-list (car item)))
                      (add-word-meaning (cdr item) meanings))
                    (with-output-to-string (s)
                      (format s "[~a] ~a~%" in (cdr item))
                      (format s "~a~%" (car meanings))
                      (format s "~%~a" *prompt-message*)))))))
      #'rec)))

(defun wildcard-p (in)
  (cl-ppcre:scan "\\*" in))

(defun search-word (in)
  "If '*' is specified in IN, split IN with '*' and return the first element as a search word."
  (car (cl-ppcre:split "\\*" in)))

;; I/O utilities from On Lisp
(defun prompt-read-line (&rest args)
  (apply #'format *query-io* args)
  (force-output *query-io*)
  (read-line *query-io*))

(defun break-loop (fn prompt quit &rest args)
  (when (cl-fad:file-exists-p *db-file*)
    (load-db))
  (loop
     (let ((in (apply prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~a~%" (funcall fn in))))))

;; DB manipulation functions
(defun search-meaning-list (word)
  (multiple-value-bind (value present-p) (gethash word *db*)
    (when present-p value)))

(defun update-meaning-count (meaning)
  (incf (cdr meaning)))

(defun add-word-meaning (word meaning)
  (setf (gethash word *db*) meaning))

(defun clear-db ()
  (setf *db* (make-hash-table :test #'equal)))

(defun new-db ()
  (clear-db)
  (with-open-file (out *db-file* :direction :output :if-exists :supersede)))

(defun save-db ()
  (with-open-file (out *db-file* :direction :output :if-exists :supersede)
    (maphash #'(lambda (k v) (format out "~s~%" (list k v))) *db*)))

(defun load-db ()
  (clear-db)
  (with-open-file (in *db-file*)
    (loop for line = (read in nil)
       while line do (add-word-meaning (car line) (cadr line)))))

(defun nthmost (nth db)
  (let (lst)
    (maphash #'(lambda (k v) (push (list k v) lst)) db)
    (setf lst (sort lst #'> :key #'cdadr))
    (let ((len (min nth (length lst))))
      (subseq lst 0 len))))

;; Dejizo REST API
;; (1) Search IDs of the word
;; http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite?Dic=EJdict&Word=word-to-be-searched&Scope=HEADWORD&Match=STARTWITH&Merge=AND&Prof=XHTML&PageSize=20&PageIndex=0
;; (2) Get the meaning of the ID
;; http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite?Dic=EJdict&Item=ID&Loc=&Prof=XHTML

(defparameter *query-page-size* 20)
(defparameter *query-word-uri* 
  "http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite?Dic=EJdict&Word=~a&Scope=HEADWORD&Match=STARTWITH&Merge=AND&Prof=XHTML&PageSize=~a&PageIndex=0")
(defparameter *query-item-uri*
  "http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite?Dic=EJdict&Item=~a&Loc=&Prof=XHTML")

(defun query-item-list (word)
  (item-list (query-word word)))

(defun query-meaning-list (item)
  "Return a cons cell whose car holds meaning of the word and
whose cdr holds the number of search count."
  (let ((meanings (meaning-list (query-item item))))
    (cons (car meanings) 1)))

(defun query-item (id)
  (http-query (http-query-item-uri id)))

(defun query-word (word)
  (http-query (http-query-word-uri word)))

(defun http-query-word-uri (word)
  (http-query-uri *query-word-uri* word *query-page-size*))

(defun http-query-item-uri (id)
  (http-query-uri *query-item-uri* id))

(defun http-query-uri (uri &rest val)
  (with-output-to-string (stream)
    (apply #'format (nconc (list stream uri) val))))

(defun http-query (uri)
  "Send HTTP request to URI with Dejizo REST API and return the HTML body."
  (format *debug-output* "URI: ~a~%" uri)
  (multiple-value-bind (body) (drakma:http-request uri)
    body))

(defparameter *regex-item-id* "<ItemID>(\\d+)</ItemID>")
(defparameter *regex-item-title* "<span.*>(.*)</span>")
(defparameter *regex-meaning* "<div>([^<]+)</div>")

(defun item-list (html)
  "Parse HTML and return a list of dotted pairs which contain ItemIDs in car and Title in cdr."
  (mapcar #'cons (item-id html) (item-title html)))

(defun meaning-list (html)
  "Parse HTML and return a list of meanings."
  (parse-item *regex-meaning* html))

(defun item-id (html)
  (parse-item *regex-item-id* html))

(defun item-title (html)
  (parse-item *regex-item-title* html))

(defun parse-item (regex html)
  (let (acc)
    (cl-ppcre:do-register-groups (m) (regex html)
      (push m acc))
    (nreverse acc)))

(when (script-p)
   (main (cddr sb-ext:*posix-argv*)))
