 ;;;; beebster.lisp

(in-package #:beebster)

;; iplayer command that returns only the wanted categories in one line
;; (get_iplayer --listformat "<index> <pid>") -> "1233 pn0232323"
(defparameter *iplayer-command*
  "get_iplayer --nocopyright --limitmatches 50 --listformat \"<index> <pid> <thumbnail> <name> <episode>\"")

;; All programmes which have been downloaded and are older than 30
;; days have to be deleted. get-iplayer records the date of every
;; download and prints a notice if a recorded programme > 30 days
;; has not been deleted yet.
(defparameter *delete-string*
  "These programmes should be deleted:")

(defun old-recordings-p (string)
  "Does get-iplayer complain about recorded programmes > 30 days?"
  (all-matches *delete-string* string))


(defparameter *categories*
  '("search" "popular" "highlights" "films" "nature"
    "crime" "sitcom" "sport" "thriller"))

(defmacro join (string &rest args)
  "concatenate strings."
  `(concatenate 'string ,string ,@args))

(defun search-categories (cat)
  "use get_iplayer to list all-programmes in a category."
  (if (null cat) nil
      (let ((result (inferior-shell:run/s
		     (join *iplayer-command* " "
				  "--category " cat))))
	(if (old-recordings-p result)
	    (butlast
	     (all-matches-as-strings "[0-9A-Z].*" result
				     :start (first (old-recordings-p result))))
	    (butlast
	     (all-matches-as-strings "[0-9].*"
				     result))))))

(defun search-iplayer (term)
  "use get_iplayer to search for program."
  (if (null term) nil
      (let ((result (inferior-shell:run/s
		     (join *iplayer-command* " " term))))
	(if (old-recordings-p result)
	    (butlast (all-matches-as-strings "[0-9A-Z].*" result
					     :start (first (old-recordings-p result))))
	    (butlast
	     (all-matches-as-strings "[0-9].*"
				     result))))))
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
  (join "get_iplayer -g --nocopyright --output=\"$HOME/Videos\"" " " index " --flvstreamer /usr/bin/flvstreamer")) ;; the --flvstreamer part
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
	 (htm (:a :class "ms" :href (join "/" i) (str i))))
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
			    :value searchterm))))))
    (display-results (search-iplayer searchterm))))
 
(defun display-results (list)
  "check if search contaings iplayer's warning notice for 
   expired programmes. If not, and if search is succesful
   loop through list to display thumbnail and title
   in 2 columns."
  (cond
    ((null list)
     (with-html-output (*standard-output* nil)
       (:p "No matches found.")))
    ((all-matches *delete-string* (first list))
     (with-html-output (*standard-output* nil)
       (:div :id "rtable"
	(loop for i in (butlast list) do
	 (htm
	  (:div :class "delete"
		(:p (str i))))))))
    (t
     (let ((imgs (mapcar #'get-thumb-from-search list))
	   (desc (mapcar #'get-title-and-episode list))
	   (ind  (mapcar #'get-index-from-search list)))
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
	    (:div :class "clear" "&nbsp;")))))))

(defmacro category-template (url cat header)
  "macro for category links."
  `(define-easy-handler (,cat :uri ,url)
       ()
     (page-template
	 (:title ,header)
       (loop for i in *categories* do
	 (htm (:a :class "ms" :href (join "/" i) (str i))))
       (:h3 :id "header" ,header)
       (display-results (search-categories ,header)))))

(category-template "/popular" popular "Popular")
(category-template "/films" films "Films")
(category-template "/highlights" highlights "Highlights")
(category-template "/crime" crime "Crime")
(category-template "/nature" nature "Nature")
(category-template "/sitcom" sitcom "Sitcoms")
(category-template "/sport" sport "Sport")
(category-template "/thriller" thriller "Thriller")

(define-easy-handler (info :uri "/info")
    (index)
  (page-template
   (:title "Info")
   (loop for i in *categories* do
	(htm (:a :class "ms" :href (join "/" i) (str i))))
      (:h3 :id "header" "Info")
      (display-image-and-info index))) 

(define-easy-handler (download :uri "/download")
    (index)
    (page-template
	(:title "Download")
      (with-html-output (*standard-output* nil)
	(:p "Downloading: " (str index))
	(:a :class "ms" :href (get-kill-url index) "Cancel"))
      (download-index index)))

(define-easy-handler (kd :uri "/kt")
    (index)
  (page-template
      (:title "")
    (with-html-output (*standard-output* nil)
      (:p "Stopping download of : " (str index))
      (kill-download *active-downloads*)
      (sleep 2)
      (dotimes (i 3) (kill-download *active-downloads*))
      (htm
       (:p "stopping download of "))
      (sleep 2  )
      (redirect "/search"))))

(defparameter *active-downloads* nil)

(defun kill-download (name)
  "search list of all runnings threads. If thread name
   is equal to 'name' kill thread."
  (bt:destroy-thread name))


(defun download-index (index)
  "download get_iplayer programme by index, using
   bt-threads."
  (let ((thread-1 (bt:make-thread (lambda ()
				    (run/s (iplayer-download-command index)))
				  :name (format nil "~A" index))))
    (setf  *active-downloads* thread-1)))


(defun display-image-and-info (index)
  "for given index display title, long description,
   thumbnail and download-link."
  (destructuring-bind (thumb desc title)
      (load-thumbnail-for-index index)
    (with-html-output (*standard-output* nil)
      (:div :class "infotitle"
	    (:p (fmt title)))
      (:div :class "infothumb"
	    (:img :src thumb))
      (:a :class "download" :href (get-download-url index) "Download")
      (:div :class "iplayerinfo"
	    (:p (fmt desc))))))

(defun get-download-url (index)
  "return url address for entered programme"
  (join "/download?index=" index))

(defun get-kill-url (index)
  "return string with /kt concatanated with index"
  (join "/kt?index=" index))

(defun load-thumbnail-for-index (index)
  "grep url for thumbnail-size4,title and description for entered index"
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
  (join "/info?index=" index ))

(defun get-info (index)
  "return get_iplayer info command for given index"
  (join "get_iplayer -i" " " (prin1-to-string index)))

(defun download-modes (string)
  "build list of possible download-modes for a given index."
  (append (all-matches-as-strings "flashhd1=[0-9]*" string)
	  (all-matches-as-strings "flashvhigh1=[0-9]*" string)
	  (all-matches-as-strings "flashhigh1=[0-9]*" string)
	  (all-matches-as-strings "flashlow1=[0-9]*" string)))


;; Assigning a parameter to hunchentoot instance to facilitate
;; stopping the server.
(defparameter *web-server*
  (setf *web-server* (make-instance 'easy-acceptor :port 4242)))


(defparameter *refresh*
  "get-iplayer --refresh")

(defun main ()
  "refresh get-iplayer index and start hunchentoot"
  (bt:make-thread (lambda () (run/s *refresh*)))
  (start *web-server*))





