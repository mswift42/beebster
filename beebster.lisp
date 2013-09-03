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
  "get-iplayer --nocopyright --limitmatches 50 --listformat \"<index> <pid> <thumbnail> <name> <episode>\"")

(defun search-iplayer (term)
  "use get-iplayer to search for program."
  (if term
      (butlast
       (all-matches-as-strings "[0-9].*"
			       (inferior-shell:run/s
				(concatenate 'string *iplayer-command* " " term))))
      nil))

(defun get-thumb-from-search (string)
  "return thumbnail address in search-iplayer string."
  (all-matches-as-strings "http.*jpg" string))

(defun get-title-and-episode (string)
   "return list of titles from search-iplayer string."
  (all-matches-as-strings "[A-Z].*" string))

(defun get-index-from-search (string)
  "return index from search-iplayer string."
  (all-matches-as-strings "^[0-9]*" string))

(defun bbc-title (term)
  "search for title in bbc-page."
  (let ((document (chtml:parse (search-bbc term) (cxml-stp:make-builder))))
    (stp:do-recursively (i document)
      (when (and (typep i 'stp:element)
		 (equal (stp:local-name i) "span")
		 (equal (stp:attribute-value i "class") "title"))
	(format t "~A:~%" (string-trim
			   '(#\Newline #\Tab) (stp:string-value i)))))))

(defun iplayer-download-command (index)
  "concatenate index to download command"
  (concatenate 'string "get-iplayer -g --nocopyright --output=~/Videos/" " " index " --flvstreamer /usr/bin/flvstreamer"))

(defun highlights-img ()
  "return the url of the highlights thumbnails."
  (mapcar #'(lambda (x) (all-matches-as-strings "h.*jpg" x))
	  (all-matches-as-strings "img.*jpg" (highlights))))

(push (create-static-file-dispatcher-and-handler
       "/first.css" "second.css") *dispatch-table*)


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
    (display-results (search-iplayer searchterm))))

(defun display-results (list)
  "loop through list to display thumbnail and title
   in a table with 3 columns."
  (let ((imgs (mapcar #'get-thumb-from-search list))
	(desc (mapcar #'get-title-and-episode list))
	(ind  (mapcar #'get-index-from-search list)))
    (if list
     (with-html-output (*standard-output* nil)
      (:div :id "rtable"
       (loop for i in imgs and  a from 0 do 
 	(htm
	 (:div :id "table"
	 (:div :class "tablecell"
	  (:div :class "t1"
		(:a :href (get-url
			   (first (nth a ind))) (:img :class "img" :src (first i))))
	  (:div :class "t1"
		(fmt (first (nth a desc)))))))) 
       (:div :class "clear" "&nbsp;")))
     (with-html-output (*standard-output* nil)
       (:p "No matches found.")))))

(define-easy-handler (info :uri "/info")
    (index)
  (page-template
	(:title "Info")
      (:h3 :id "header" "Info")
      (display-image-and-info index))) 

(define-easy-handler (download :uri "/download")
    (index)
    (page-template
	(:title "Download")
      (download-index index)
      (redirect "/search")))

(defun download-index (index)
  "download get-iplayer Programme by index."
  (gt:make-thread
   (lambda () (run/s (iplayer-download-command index))
	   (gt:make-thread (lambda ()
			     (with-html-output
				 (*standard-output* nil)
			       (redirect "/search")))))))

(defun display-image-and-info (index)
  "show thumbnail and get-iplayer's long description."
  (let ((ind (load-thumbnail-for-index index)))
    (with-html-output (*standard-output* nil)
      (:img :src (first ind))
      (:div :class "iplayerinfo "
	    (:p (fmt (second ind))))
      (:div :class "download"
	    (:a :id "menu" :href (get-download-url index) "Download")))))

(defun get-download-url (index)
  (concatenate 'string "/download?index=" index))

(defun load-thumbnail-for-index (index)
  "grep address for thumbnail size4 and description for entered index"
  (let ((ind (run/s (get-info index))))
    (list (first (all-matches-as-strings "htt.*"
					 (first (all-matches-as-strings
						 "thumbnail4.*" ind))))
	  (first (all-matches-as-strings "[A-Z].*"
					 (first (all-matches-as-strings
						 "desc:.*" ind)))))))

(defun get-url (index)
  "return /info url string concatenated with the index"
  (concatenate 'string "/info?index=" index ))

(defun get-info (index)
  "return get-iplayer info command for given index"
  (concatenate 'string "get-iplayer -i" " " (prin1-to-string index)))

(define-easy-handler (test-2 :uri "/highlights"
			     :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (page-template
      (:title "Highlights")
    (:h3 :id "header" "Highlights")
    (:table :class "results" :border 0 :cellpadding 8
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






(fiveam:run!)
