(require :sb-posix)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '("drakma" "closure-html" "cxml-stp"))

(defun show-usage () 
  (format t "Usage: myfitnessdata USERNAME PASSWORD~%~%")
  (format t "  USERNAME  Your MyFitnessPal username~%")
  (format t "  PASSWORD  Your MyFitnessPal password~%~%")
  (format t "Example:~%~%")
  (format t "  ./myfitnessdata bob b0bsp4ss! weights.csv~%~%"))

(defun get-page (page-num username password)
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
    (drakma:http-request "http://www.myfitnesspal.com/account/login"
  			 :method :post
  			 :parameters `(("username" . ,username) ("password" . ,password))
  			 :cookie-jar cookie-jar)
    (let ((url (concatenate 'string "http://www.myfitnesspal.com/measurements/edit?type=1&page=" (write-to-string page-num))))
      (let ((body (drakma:http-request url :cookie-jar cookie-jar)))
	(if (search "No measurements found." body)
	    nil
	  body)))))

(defun scrape-body (body)
  (let ((valid-xhtml (chtml:parse body (cxml:make-string-sink))))
    (let ((xhtml-tree (chtml:parse valid-xhtml (cxml-stp:make-builder))))
      (scrape-xhtml xhtml-tree))))

(defun scrape-xhtml (xhtml-tree)
  (setq results nil)
  (stp:do-recursively (element xhtml-tree)
		      (when (and (typep element 'stp:element)
				 (equal (stp:local-name element) "tr"))
			(if (scrape-row element)
			    (setq results (append results (list (scrape-row element)))))))
  results)			  

(defun scrape-row (row)
  (if (equal 4 (stp:number-of-children row))
      (let ((measurement-type (nth-child-data 0 row))
	    (measurement-date (nth-child-data 1 row))
	    (measurement-value (nth-child-data 2 row)))
	(if (not (equal measurement-type "Measurement"))
	    (cons measurement-date measurement-value)))))

(defun nth-child-data (number row)
  (stp:data (stp:nth-child 0 (stp:nth-child number row))))

(defun scrape-page (page-num username password)
  (let ((body (get-page page-num username password)))
    (if (not (string= nil body))
	(progn
	  (append (scrape-body body)
		  (scrape-page (+ 1 page-num) username password))))))

(if (= (length sb-ext:*posix-argv*) 3)
    (let ((username (nth 0 sb-ext:*posix-argv*))
	  (password (nth 1 sb-ext:*posix-argv*)))
      (scrape-page 1 username password))
  (show-usage))
