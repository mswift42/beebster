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

(defparameter *selection* '())

(defparameter *iplayer-command*
  "get-iplayer --nocopyright --limitmatches 20 --listformat \"<index> <pid> <thumbnail> <name> <episode>\"")

(defun search-iplayer (term)
  "use get-iplayer to search for program."
  (if term
      (butlast (all-matches-as-strings "[0-9].*"
				       (inferior-shell:run/s
					(concatenate 'string *iplayer-command* " " term))))
      nil))

(defun get-thumb-from-search (string)
  "return thumbnail address in search-iplayer string."
  (all-matches-as-strings "http.*jpg" string))

(defun get-title-and-episode (string)
  "return list of titles from search-iplayer string."
  (all-matches-as-strings "[A-Z].*" string))

(defun bbc-title (term)
  "search for title in bbc-page."
  (let ((document (chtml:parse (search-bbc term) (cxml-stp:make-builder))))
    (stp:do-recursively (i document)
      (when (and (typep i 'stp:element)
		 (equal (stp:local-name i) "span")
		 (equal (stp:attribute-value i "class") "title"))
	(format t "~A:~%" (string-trim
			   '(#\Newline #\Tab) (stp:string-value i)))))))


(defun highlights-img ()
  "return the url of the highlights thumbnails."
  (mapcar #'(lambda (x) (all-matches-as-strings "h.*jpg" x))
	  (all-matches-as-strings "img.*jpg" (highlights))))



(push (create-static-file-dispatcher-and-handler "/first.css" "second.css") *dispatch-table*)

 (defparameter *user-search* '())
(defvar *user-search-length* 0)

(defmacro ah (dest desc)
  `(with-html-output (*standard-output* nil)
     (:a :href  ,dest ,desc)))

(defmacro iplayer-img (class source title alt)
  `(with-html-output (*standard-output* nil)
     (:img :class ,class :src ,source
	   :title ,title :alt ,alt :width 150 :height 84)))

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:title ,title)
       (:link :type "text/css" :rel "stylesheet"
	      :href "/first.css "))
      (:body ,@body))))

(defun times-4 (n)
  (* n 4))

(define-easy-handler (iplayer-search :uri "/search"
			     :default-request-type :both)
    ((searchterm :parameter-type 'string))
  (page-template
      (:title "iplayer search")
    (htm (:a :class "ms" :href "/highlights"  "highlights")
	 (:a :class "ms" :href "/sitcoms" "sitcom"))
    (:h3 :id "header" "Search")
    (:p (:form
	 :method :post
	 (:table :border 0 :cellpadding 2
	  (:tr (:td  :style "text-align:right;color:#e2e2e5" (str "Search"))
	       (:td (:input :type :text :style "float:left"
			    :name "searchterm"
			    :value searchterm))
	       (:td (:input :type :submit :value "Submit" ))))))
    (display-results (funcall 'search-iplayer searchterm))))

(defun display-results (list)
  (let ((imgs (mapcar #'get-thumb-from-search list))
	(desc (mapcar #'get-title-and-episode list)))
    (loop for i from 0 to (- (length list)3) by 3 while i do
	 (with-html-output (*standard-output* nil)
	   (:table :border 0 :cellpadding 2
		   (:tr
		    (:td (iplayer-img "img" (first (nth i imgs))
				      "text" "text"))
		    (:td :class "t1" (fmt (first (nth i desc))))))))))


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






