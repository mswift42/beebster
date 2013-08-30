 ;;;; beebster.lisp

(in-package #:beebster)


(defun highlights ()
  "rss feed of the iplayer highlight page"
  (drakma:http-request "http://feeds.bbc.co.uk/iplayer/highlights/tv"))



(defun search-bbc (term)
  "return html-string of search term"
  (let* ((query (list (cons "q" term))))
    (drakma:http-request "http://www.bbc.co.uk/iplayer/search"
			 :parameters query)))


(defun bbc-title (term)
  "search for title in bbc-page."
  (let ((document (chtml:parse (search-bbc term) (cxml-stp:make-builder))))
    (stp:do-recursively (i document)
      (when (and (typep i 'stp:element)
		 (equal (stp:local-name i) "span")
		 (equal (stp:attribute-value i "class") "title"))
	(format t "~A:~%" (string-trim
			   '(#\Newline #\Tab) (stp:string-value i)))))))
(defun get-thumbs (page)
  (all-matches-as-strings "([a-z].{15}jpg)" page))



(defun highlights-img ()
  "return the url of the highlights thumbnails."
  (mapcar #'(lambda (x) (all-matches-as-strings "h.*jpg" x))
	  (all-matches-as-strings "img.*jpg" (highlights))))



(define-easy-handler (easy-demo :uri "/lisp/hello"
                                :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "test Hunchentoot/cl-who"))
     (:body
      (:h1 "Hello, world!")
      (:p "Foray into pa rsing bbc and displaying it's thumbnails.")
      (:p "this is some text from me")
      ( :p :br "and more text")
      (:img :class "pic" :src "http://ichef.bbci.co.uk/programmeimages/episode/p01djws9_640_360.jpg" :height "360" :width "640" )
      (:h3 "and a header")
      (loop for j below 4 do
	   (htm
	    (:p "more text")))
      (:table :border 0 :cellpadding 4
	      (loop for i from 1 to 5 do
		   (htm
		    (:tr :align "left" 
			 (loop for j from 1 to 2 do
			      (htm
			       (:td (:img :src (first
						(nth (* i j) (highlights-img)))
					  :width) "400" :height "300")))))))))))

(push (create-static-file-dispatcher-and-handler "/first.css" "/home/martin/quicklisp/local-projects/beebster/first.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/black-forest.jpg" "/home/martin/Pictures/black_forest_in_germany-wide.jpg") *dispatch-table*)
(defmacro page-template ((&key title link) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:title ,title)
       (:link :type "text/css" :rel "stylesheet"
	      :href "/first.css"))
      (:body ,@body))))

(define-easy-handler (test-2 :uri "/highlights"
			     :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (page-template
      
      (:title "Highlights")
      
    (:h3 :id "header" "Highlights")
    (:table :border 0 :cellpadding 8
	    (:tr :align "center"
		 (htm
		  (:td :class "img"
		       (:img :src (image-by-number 0) :width 200 :height 160 :title (title-by-number 0)))
		  (:td :class "t1" (fmt (title-by-number 0)))
		  (:td :class "img"
		       (:img :src (image-by-number 1) :width 200 :height 160
			     :title (title-by-number 1)))
		  (:td :class "t1" (fmt (title-by-number 1)) :wrap-block t)
		  (:td :class "img"
		       (:img :src (image-by-number 2) :width 200 :height 160
			     :title (title-by-number 2)))
		  (:td :class "t1" (fmt (title-by-number 2)) :wrap-block t)))
	    (:tr :align "center"
		 (htm
		  (:td :class "img"
		       (:img :src (image-by-number 3) :width 200 :height 160
			     :title (title-by-number 3)))
		  (:td :class "t1"
		       (fmt (title-by-number 3)))
		  (:td :class "img"
		       (:img :src (image-by-number 4) :width 200
			     :height 160 :title (title-by-number 4)))
		  (:td :class "t1"
		       (fmt (title-by-number 4)))
		  (:td :class "img"
		       (:img :src (image-by-number 5) :width 200
			     :height 160 :title (title-by-number 5)))
		  (:td :class "t1"
		       (fmt (title-by-number 5)))))
	    (:tr :align "center"
		 (loop for i from 6 to 8 do
		      (htm
		       (:td :class "img"
			    (:img :src (image-by-number i) :width 200
				  :height 160 :title (title-by-number i)))
		       (:td :class "t1"
			    (fmt (title-by-number i))))))
	    (:tr :align "center"
		 (htm
		  (:td :class "img"
		       (:img :src (image-by-number 9) :width 200
			     :height 160 :title (title-by-number 9)))
		  (:td :class "t1"
		       (fmt (title-by-number 9))))))))

(defun highlights-img-capture ()
  (mapcar #'(lambda (x) (all-matches-as-strings "[A-Z].*" x))
	  (all-matches-as-strings "alt=&quot.*&quot" (highlights))))

(defun image-by-number (n)
  (first (nth n (highlights-img))))

(defun title-by-number (n)
  (first (nth n (highlights-img-capture))))





