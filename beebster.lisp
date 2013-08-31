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

(defparameter *iplayer-command*
  "get-iplayer --nocopyright --listformat \"<index> <pid> <thumbnail> <name> <episode>\"")

(defun search-iplayer (term)
  (butlast (all-matches-as-strings "[0-9].*"
				   (inferior-shell:run/s
				    (concatenate 'string *iplayer-command* " " term)))))

(defun get-thumb-from-search (string)
  "return thumbnail address in search-iplayer string."
  (all-matches-as-strings "http.*jpg" string))

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

(defparameter *selection* "")


;; (define-easy-handler (easy-demo :uri "/lisp/hello"
;;                                 :default-request-type :get)
;;     ((state-variable :parameter-type 'string))
;;   (with-html-output-to-string (*standard-output* nil :prologue t)
;;     (:html
;;      (:head (:title "test Hunchentoot/cl-who")
;; 	    (:script (str (ps:ps (defun greeting-callback ()
;; 				   (alert "Hoden!"))))))
;;      (:body
;;       (:h1 "Hello, world!")
;;       (:p "Foray into parsing bbc and displaying it's thumbnails.")
;;       (:p "this is some text from me")
;;       ( :p :br "and more text")
;;       (:img :class "pic" :src "http://ichef.bbci.co.uk/programmeimages/episode/p01djws9_640_360.jpg" :height "360" :width "640" )
;;       (:h3 "and a header")
;;       (:a :href "#" :onclick (ps:ps (greeting-callback))
;; 	  "Pimmel!" )
      
;;       ))))

(push (create-static-file-dispatcher-and-handler "/first.css" "second.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/black-forest.jpg" "/home/martin/Pictures/black_forest_in_germany-wide.jpg") *dispatch-table*)
(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:title ,title)
       (:link :type "text/css" :rel "stylesheet"
	      :href "/first.css"))
      (:body ,@body))))

(define-easy-handler (iplayer-search :uri "/search"
			     :default-request-type :both)
    ((state-variable :parameter-type 'string))
  (page-template
      (:title "iplayer search")
    
    (htm (:a :class "ms" :href "/highlights"  "highlights")
	 (:a :class "ms" :href "/sitcoms" "sitcom"))
    (:h3 :id "header" "Search")
    (:p "you should see some text.")
    (:p (:form
	 :method :post
	 (:table :border 0 :cellpadding 2
	  (:tr (:td  :style "text-align:right;color:#e2e2e5" (str "Search"))
	       (:td (:input :type :text :style "float:left"
			    :name "state-variable"
			    :value state-variable))
	       (:td (:input :type :submit :value "Submit"))))
	 (display-results (str state-variable))))
    (setf *selection* state-variable)
    (:p (str state-variable))
    (htm
     (test-display))
    (display-results *selection*)
    
    ))

(defun display-results (searchterm)
  (let* ((sl (search-iplayer searchterm))
	 (rows (values (ceiling (/ (length sl) 3))))
	 (imgs (mapcar #'get-thumb-from-search sl)))
    (with-html-output (*standard-output* nil :prologue t)
      (:p "this is new text")
      (loop for i from 0 to 3 do
	   (htm
	    (:tr :align "center"
		 (loop for j from 0 to 2 do
		      (htm
		       (:td :class "img"
			    (:img :src (first (nth (+ i j) imgs))))))))))
    ))

(defun test-display ()
  (with-html-output-to-string (s)
    (:p "only text is working")))

(define-easy-handler (test-2 :uri "/highlights"
			     :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (page-template
      
      (:title "Highlights")
      
    (:h3 :id "header" "Highlights")
    (:table :border 0 :cellpadding 8
     (loop for i from 0 to 6 by 3 do
       (htm
	(:tr :align "center"
	  (loop for j to 2 do
	   (htm
	    (:td :class "img"
		 (:img :src (image-by-number (+ i j)) :width 180 :height 140
		       :title (title-by-number (+ i j))  ))
	    (:td :class "t1"
		 (fmt (title-by-number (+ i j)))))))))
     (:tr :align "center"
	  (:td :class "img"
	       (:a :href "/search" (:img :src (image-by-number 9) :width 180 :height 140)
		     :title (title-by-number 9) :onclick (push (image-by-number 9) *selection*) 
		     ))
	  (:td :class "t1"
	       (fmt (title-by-number 9)))))))

(defun highlights-img-capture ()
  (mapcar #'(lambda (x) (all-matches-as-strings "[A-Z].*" x))
	  (all-matches-as-strings "alt=&quot.*&quot" (highlights))))

(defun image-by-number (n)
  (first (nth n (highlights-img))))

(defun title-by-number (n)
  (first (nth n (highlights-img-capture))))






