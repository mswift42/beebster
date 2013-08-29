;;;; beebster.lisp

(in-package #:beebster)



(defun show-bbc ()
  (let* ((str (drakma:http-request "http://www.bbc.co.uk/iplayer/search?q=lake"))
	 (document (chtml:parse str (cxml-stp:make-builder))))
    (stp:do-recursively (a document)
      (when (and (typep a 'stp:element)
		 (equal (stp:local-name a) "span")
		 (equal (stp:attribute-value a "class") "title"))
	(format t "~A:~%~A~%"
		(stp:string-value a)
 		(stp:attribute-value a "class"))))))


(defun search-bbc (term)
  (let* ((query (list (cons "q" term))))
    (drakma:http-request "http://www.bbc.co.uk/iplayer/search"
			 :parameters query)))


(defun bbc-title (term)
  (let ((document (chtml:parse (search-bbc term) (cxml-stp:make-builder))))
    (stp:do-recursively (i document)
      (when (and (typep i 'stp:element)
		 (equal (stp:local-name i) "span")
		 (equal (stp:attribute-value i "class") "title"))
	(format t "~A:~%" (string-trim
			   '(#\Newline #\Tab) (stp:string-value i)))))))
(defun get-thumbs (page)
  (all-matches-as-strings "([a-z].{15}jpg)" page))



(define-easy-handler (easy-demo :uri "/lisp/hello"
                                :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "test Hunchentoot/cl-who"))
     (:body
      (:h1 "Hello, world!")
      (:p "This is my Lisp web server, running on Hunchentoot,"
          " as described in "
          (:a :href
              "http://newartisans.com/blog_files/hunchentoot.primer.php"
              "this blog entry")
          " on Common Lisp and Hunchentoot."))
     (:p "this is some text from me")
     (:img :src "http://31.media.tumblr.com/c6eb147b5c3d60f10c5480302c85a7d3/tumblr_mrxvhlcPpO1qzs4roo1_500.jpg"
	 :alt "black forest"  :height "300" :width "400"
      )
     (:h3 "and a header")
     (loop for j below 4 do
	  (htm
	   (:p "more text")))
     )))



