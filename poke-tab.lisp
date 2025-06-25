;; (require :asdf)
;; (ql:quickload :sqlite)
;; (use-package :sqlite)
;; (asdf:load-system :clwebserv)

(defpackage :poke-tab
  (:use :cl :uiop :sqlite :clwebserv)
  (:export :main
           :random-poke
           :put-data
           :params))

(in-package :poke-tab)

(defparameter *db* nil)

(defun parse-args (args)
  (when (car args)
      (let ((car-sym (subseq (car args) 1)))
        (if (and (cdr args) (equal (subseq (cadr args) 0 1) "-"))
            (cons `(,car-sym ,nil) (parse-args (cdr args)))
            (cons `(,car-sym ,(cadr args)) (parse-args (cddr args)))))))

(let ((images nil)
      (images-count nil))
  (defun random-poke (path)
    (if images-count
        (let ((poke (aref images (random images-count))))
          (format nil "<img class=\"sprite-image\" src=\"/sprites/~a\"/> <div class=\"sprite-name\">~a</div>"
                  (file-namestring poke)
                  (string-capitalize (substitute-if #\Space (lambda (char) (equal char #\-)) (pathname-name poke)))))
        (let ((images-list (uiop:directory-files path)))
          (setf images-count (list-length images-list))
          (setf images (make-array `(,images-count)
                                   :initial-contents images-list))
          (random-poke path)))))

(defun categ-items-unite (categories items)
  (labels ((select-items (category_id items)
             (remove-if-not (lambda (item) (eq category_id (cadddr item))) items)))
    (if (car categories)
        (cons (cons (select-items (caar categories) items)
                    (car categories))
              (categ-items-unite (cdr categories) items))
        nil)))

(defun put-data (params)
  (apply #'concatenate
         `(string .
           ,(loop for category in (cdr (assoc 'categories params))
                  collect "<div class=\"category container-shadow\">"
                  collect (format nil "<div class=\"category-name\" style=\"color: ~a\">~a</div>" (cadddr category) (caddr category))
                  collect "<hr/>"
                  collect "<div class=\"category-items-container\">"
                  collect (apply #'concatenate `(string . ,(loop for item in (car category)
                                                                 collect (format nil
                                                                                 "<a class=\"category-item\" href=\"~a\">~a</a>"
                                                                                 (caddr item)
                                                                                 (cadr item)))))
                  collect "</div></div>"))))

(defun send-poke (params)
  (let* ((categories (execute-to-list *db* "select * from categories"))
         (items (execute-to-list *db* "select * from items"))
         (params `((categories . ,(categ-items-unite categories items)) . ,params)))
    (send 'temp "resources/index.html" :params params)))

(defun db-new (params)
  (if (equal (cdr (assoc 'type params)) "category")
      (execute-non-query *db* "insert into categories (title, color) values (?, ?)"
                         (cdr (assoc 'title params))
                         (format nil "#~a" (cdr (assoc 'color params))))
      (execute-non-query *db* "insert into items (title, category_id, url) values (?, (select id from categories where title like ?), ?)"
                         (cdr (assoc 'title params))
                         (cdr (assoc 'category params))
                         (cdr (assoc 'url params))))
  (send 'data "{\"success\": true, \"message\": \"Successfully added<br/>Page will be reloaded in 2 sec\"}" :header '((status . "200 OK") ("Content-Type" . "application/json"))))

(defun db-delete (params)
  (execute-non-query *db* "pragma foreign_keys = on")
  (if (equal (cdr (assoc 'type params)) "category")
      (execute-non-query *db* "delete from categories where title like ?" (cdr (assoc 'title params)))
      (execute-non-query *db* "delete from items where title like ?" (cdr (assoc 'title params))))
  (send 'data "{\"success\": true, \"message\": \"Successfully deleted<br/>Page will be reloaded in 2 sec\"}" :header '((status . "200 OK") ("Content-Type" . "application/json"))))

(defun req-handler (path params)
  (cond ((or (equal path "/index.html") (equal path "/"))
         (send-poke params))
        ((equal path "/new") (db-new params))
        ((equal path "/delete") (db-delete params))
        ((equal path "/test") (send 'data "<h1>Some test!</h1>" :header '((status . "200 OK") ("Content-Type" . "text/html"))))
        (t (send 'file (format nil "resources/~a" (subseq path 1))))))

(defun main ()
  (in-package :poke-tab)
  (let* ((args (parse-args (command-line-arguments)))
         (port (cadr (assoc "p" args :test #'string=)))
         (db-path (cadr (assoc "d" args :test #'string=))))
    (cond ((eq port nil) (princ "You need to specify port with -p flag"))
          ((eq db-path nil) (princ "You need to specify database path with -d flag"))
          ('otherwise (setf *random-state* (make-random-state t))
                      (setf *db* (connect db-path))
                      (serv 8888 #'req-handler)))))
