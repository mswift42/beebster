;;;; beebster.lisp

(in-package #:beebster)

;; iplayer command that returns only the wanted categories in one line
;; (get_iplayer --listformat "<index> <pid>") -> "1233 pn0232323"
(defparameter *iplayer-command*
  "get_iplayer --nocopyright --limitmatches 50 --listformat \"<index> <pid> <thumbnail> <name> <episode>\"")

(setf *js-string-delimiter* #\")
(defparameter *categories*
  '("popular" "highlights" "films" "nature"  "crime" "sitcom" "sport"))

(defun search-categories (cat)
  "use get_iplayer to list all programmes in a category."
  (if cat
      (butlast
       (all-matches-as-strings "[0-9].*"
			       (inferior-shell:run/s
				(concatenate 'string *iplayer-command* " "
					     "--category " cat))))
      nil))

(defun search-iplayer (term)
  "use get_iplayer to search for program."
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
   (all-matches-as-strings "[A-Z0-9].*"
			   (regex-replace "\\-"
					  (first (all-matches-as-strings
						  "jpg.*" string))
					  "")))
 
(defun get-index-from-search (string)
  "return index from search-iplayer string."
  (all-matches-as-strings "^[0-9]*" string))

(defun iplayer-download-command (index)
  "concatenate index to download command"
  (concatenate 'string "get_iplayer -g --nocopyright --output=\"$HOME/Videos\"" " " index " --flvstreamer /usr/bin/flvstreamer")) ;; the --flvstreamer part
;; is only needed with some versions of rtmpdump, that do not work with
;; iplayer's site. If you have a 'vanilla' version of rtmpdump installed
;; you can delete this.

;; tell Hunchentoot which css file to use.
(push (create-static-file-dispatcher-and-handler
       "/first.css" "second.css") *dispatch-table*)


(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
       (:head
       (:title ,title)
       (:link :type "text/css" :rel "stylesheet"
	      :href "/first.css "))
      (:body ,@body))))

;; Start Page; search for programmes or visit category links
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
      (with-html-output (*standard-output* nil)
	(:p "Downloading: " (str index))
	(:a :class "ms" :href (get-kill-url index) "Cancel"))
      (download-index index)
      ;(redirect "/search")
      ))

(define-easy-handler (kd :uri "/kt")
    (index)
  (page-template
      (:title "")
    (with-html-output (*standard-output* nil)
      (:p "Stopping download of : " (str index))
      (kill-download (format nil "~A" index))
      (kill-download index)
      (kill-download (format nil "~A" index))
      (dotimes (i 10) (kill-download (format nil "~A" index)))
      (sleep 2)
      (dotimes (i 10) (kill-download (format nil "~A" index)))
      (dotimes (i 10) (kill-download index))
      (htm
       (:p "stopping download of "))
      (sleep 2  )
      (redirect "/search"))))

(defparameter *active-downloads* '())

(defun test-thread ()
  "get to grips with bt:threads"
  (bt:make-thread (lambda () (sleep 30)) :name "sleep"))

(defun kill-download (name)
  "search list of all runnings threads. If thread name
   is equal to 'name' kill thread."
  (dolist (i (bt:all-threads))
    (if (equal name (bt:thread-name i))
	(bt:destroy-thread i))))


(defun download-index (index)
  "download get_iplayer programme by index, using
   bt-threads."
  (let ((thread-1 (bt:make-thread (lambda ()
				    (run/s (iplayer-download-command index)))
				  :name (format nil "~A" index))))
    (push thread-1 *active-downloads*)))

(defun display-image-and-info (index)
  "show thumbnail and get_iplayer's long description."
  (let ((ind (load-thumbnail-for-index index)))
    (with-html-output (*standard-output* nil)
      (:div :class "infotitle"
	    (:p (fmt (third ind))))
      (:div :class "infothumb"
	    (:img :src (first ind)))
      (:div :class "iplayerinfo "
	    (:p (fmt (second ind))))
      (:a :class "download" :href (get-download-url index) "Download"))))

(defun get-download-url (index)
  "return url address for entered programme"
  (concatenate 'string "/download?index=" index))

(defun get-kill-url (index)
  "return string with /kt concatanated with index"
  (concatenate 'string "/kt?index=" index))

(defun load-thumbnail-for-index (index)
  "grep address for thumbnail size4 and description for entered index"
  (let ((ind (run/s (get-info index))))
    (list (first (all-matches-as-strings "htt.*"
					 (first (all-matches-as-strings
						 "thumbnail4.*" ind))))
	  (first (all-matches-as-strings "[A-Z].*"
					 (first (all-matches-as-strings
						 "desc:.*" ind))))
	  (first (all-matches-as-strings "[A-Z0-9].*"
					 (first (all-matches-as-strings
						 "title:.*" ind)))))))

(defun get-url (index)
  "return /info url string concatenated with the index"
  (concatenate 'string "/info?index=" index ))

(defun get-info (index)
  "return get_iplayer info command for given index"
  (concatenate 'string "get_iplayer -i" " " (prin1-to-string index)))


(fiveam:run!) ;; Run tests from tests.lisp
