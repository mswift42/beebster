;;;; beebster.lisp

(in-package #:beebster)


(defun highlights ()
  "rss feed of the iplayer highlight page"
  (drakma:http-request "http://feeds.bbc.co.uk/iplayer/highlights/tv"))



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
     (:img :src "http://ichef.bbci.co.uk/programmeimages/episode/p01djws9_640_360.jpg" :height "360" :width "640" )
     (:h3 "and a header")
     (loop for j below 4 do
	  (htm
	   (:p "more text")))
     (:table :border 0 :cellpadding 4
      (loop for i below 10 do
	   (htm
	    (:img :src (first (nth i (highlights-img)))
		  :height "300" :width "400" :alt
		  (first (nth i (highlights-img-capture))))))))))

(defun highlights-img-capture ()
  (mapcar #'(lambda (x) (all-matches-as-strings "[A-Z].*" x))
	  (all-matches-as-strings "alt=&quot.*&quot" (highlights))))



