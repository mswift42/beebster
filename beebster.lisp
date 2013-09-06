
;;;; beebster.lisp

(in-package #:beebster)


(defparameter *iplayer-command*
  "get-iplayer --nocopyright --limitmatches 50 --listformat \"<index> <pid> <thumbnail> <name> <episode>\"")

(setf *js-string-delimiter* #\")
(defparameter *categories*
  '("popular" "highlights" "films" "nature"  "crime" "sitcom" "sport"))

(defun search-categories (cat)
  "use get-iplayer to list all programmes in a category."
  (if cat
      (butlast
       (all-matches-as-strings "[0-9].*"
			       (inferior-shell:run/s
				(concatenate 'string *iplayer-command* " "
					     "--category " cat))))
      nil))

(defun search-iplayer (term)
  "use get-iplayer to search for program."
  (if term
      (butlast
       (all-matches-as-strings
	"[0-9].*"
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



(defun iplayer-download-command (index)
  "concatenate index to download command"
  (concatenate 'string "get_iplayer -g --nocopyright --output=~/Videos/" " " index " --flvstreamer /usr/bin/flvstreamer")) ;; the --flvstreamer part
;; is only needed with some versions of rtmpdump, that do not work with
;; iplayer's site. If you have a 'vanilla' version of rtmpdump installed
;; you can delete this.

;; (defun highlights-img ()
;;   "return the url of the highlights thumbnails."
;;   (mapcar #'(lambda (x) (all-matches-as-strings "h.*jpg" x))
;; 	  (all-matches-as-strings "img.*jpg" (highlights))))

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
    (loop for i in *categories* do
	 (htm (:a :class "ms" :href (concatenate 'string "/" i) (str i))))
    (:br)
    (:br)
    (:h3 :id "header" "Search")
    (:br)
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
  "loop through list to display thumbnail ,title
   and description in  2 columns."
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
			   (first (nth a ind)))
		    (:img :class "img" :src (first i))))
	  (:div :class "t1"
		(fmt (first (nth a desc)))))))) 
       (:div :class "clear" "&nbsp;")))
     (with-html-output (*standard-output* nil)
       (:p "No matches found.")))))


(defmacro category-template (url cat header)
  "macro for category links."
  `(define-easy-handler (,cat :uri ,url)
       ()
     (page-template
	 (:title ,header)
       (:h3 :id "header" ,header)
       (display-results (search-categories ,header)))))

(category-template "/popular" popular "Popular")
(category-template "/films" films "Films")
(category-template "/highlights" highlights "Highlights")
(category-template "/crime" crime "Crime")
(category-template "/nature" nature "Nature")
(category-template "/sitcom" sitcom "Sitcoms")
(category-template "/sport" sport "Sport")

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
  (bt:make-thread (lambda ()
		    (run/s (iplayer-download-command index)))
		  :name "download")
  
  (with-html-output (*standard-output* nil)
    (redirect "/search")))

(defun display-image-and-info (index)
  "show thumbnail and get-iplayer's long description."
  (let ((ind (load-thumbnail-for-index index)))
    (with-html-output (*standard-output* nil)
      (:div :class "infotitle"
	    (:p (fmt (third ind))))
      (:div :class "infothumb"
	    (:img :src (first ind)))
      (:div :class "iplayerinfo "
	    (:p (fmt (second ind))))
      (:a :class "download" :href (get-download-url index) "Download")
      (ps-inline (alert (third ind))))))

(defun get-download-url (index)
  "return url address for entered programme"
  (concatenate 'string "/download?index=" index))

(defun load-thumbnail-for-index (index)
  "grep address for thumbnail size4 and description for entered index"
  (let ((ind (run/s (get-info index))))
    (list (first (all-matches-as-strings "htt.*"
					 (first (all-matches-as-strings
						 "thumbnail4.*" ind))))
	  (first (all-matches-as-strings "[A-Z].*"
					 (first (all-matches-as-strings
						 "desc:.*" ind))))
	  (first (all-matches-as-strings "[A-Z].*"
					 (first (all-matches-as-strings
						 "title:.*" ind)))))))

(defun get-url (index)
  "return /info url string concatenated with the index"
  (concatenate 'string "/info?index=" index ))

(defun get-info (index)
  "return get-iplayer info command for given index"
  (concatenate 'string "get-iplayer -i" " " (prin1-to-string index)))






(fiveam:run!)
