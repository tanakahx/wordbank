#!/bin/sh
#|
exec sbcl --core $SBCL_CORE --script $0 $0 "$@"
|#

(in-package :cl-user)
(defpackage wordbank
  (:use :cl)
  (:export :query-item-list
           :query-meaning-list
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
    (ql:quickload "cl-ppcre")))

(defparameter *debug-output* nil)
(defparameter *prompt-message* "Input search word or :q to quit")
(defparameter *prompt-string* "WORDBANK> ")

;; Entry point of this program
(defun main (&optional args)
  (declare (ignore args))
  (format t "~a~%" *prompt-message*)
  (break-loop (prompt-handler) #'(lambda (x) (eq x :q)) *prompt-string*))

(defun prompt-handler ()
  "State of this closure
IDLE:   Send a query of a requested word and print the titles of the word.
SELECT: Print the meaning of the requested word.
"
  (let ((state 'idle)
        items)
    (labels ((rec (in)
        (case state
          (idle (setf state 'select) 
                (setf items (query-item-list in))
                (if items
                    (if (string= (symbol-name in) (string-upcase (cdr (first items))))
                        (rec 0)
                        (with-output-to-string (s)
                          (loop
                             for item in items
                             for i from 0
                             do (format s "[~2d] ~a~%" i (cdr item))
                             finally (format s "~%Select a number listed above (upto ~d)" i))))
                    (progn (setf state 'idle)
                           (format nil "No entry is found"))))
          (select (setf state 'idle) 
                  (let* ((item (nth in items))
                         (meanings (query-meaning-list (car item))))
                    (with-output-to-string (s)
                      (format s "[~2d] ~a~%" in (cdr item))
                      (loop
                           for m in meanings
                           for i from 0
                           do (format s "~a~%" m)
                           finally (format s "~%~a" *prompt-message*))))))))
      #'rec)))

;; I/O utilities from On Lisp
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (force-output *query-io*)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (loop
     (let ((in (apply #'prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~a~%" (funcall fn in))))))

;; Dejizo REST API
;; (1) Search IDs of the word
;; http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite?Dic=EJdict&Word=word-to-be-searched&Scope=HEADWORD&Match=STARTWITH&Merge=AND&Prof=XHTML&PageSize=20&PageIndex=0
;; (2) Get the meaning of the ID
;; http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite?Dic=EJdict&Item=ID&Loc=&Prof=XHTML

(defparameter *query-word-uri* 
  "http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite?Dic=EJdict&Word=~a&Scope=HEADWORD&Match=STARTWITH&Merge=AND&Prof=XHTML&PageSize=20&PageIndex=0")
(defparameter *query-item-uri*
  "http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite?Dic=EJdict&Item=~a&Loc=&Prof=XHTML")

(defun query-item-list (word)
  (item-list (query-word word)))

(defun query-meaning-list (item)
  (meaning-list (query-item item)))

(defun query-item (id)
  (http-query (http-query-item-uri id)))

(defun query-word (word)
  (http-query (http-query-word-uri word)))

(defun http-query-word-uri (word)
  (http-query-uri *query-word-uri* word))

(defun http-query-item-uri (id)
  (http-query-uri *query-item-uri* id))

(defun http-query-uri (uri val)
  (with-output-to-string (stream)
    (format stream uri val)))

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
