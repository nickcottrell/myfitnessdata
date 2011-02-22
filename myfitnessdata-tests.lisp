(defpackage :myfitnessdata-tests
  (:use :common-lisp))

(in-package :myfitnessdata-tests)

(load "myfitnessdata.lisp")
(ql:quickload '("lisp-unit"))

(lisp-unit:define-test 
 logged-in-nil-when-no-cookies
 (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
   (lisp-unit:assert-false (myfitnessdata:logged-in? cookie-jar))))

(lisp-unit:define-test
 logged-in-nil-when-cookies-from-wrong-domain
 (let ((cookie-jar (make-instance 'drakma:cookie-jar))
       (cookie (make-instance 'drakma:cookie :name "known_user" :domain "example.com")))
   (setf (drakma:cookie-jar-cookies cookie-jar) (list cookie))
   (lisp-unit:assert-false (myfitnessdata:logged-in? cookie-jar))))

(lisp-unit:define-test
 logged-in-t-when-proper-cookies
 (let ((cookie-jar (make-instance 'drakma:cookie-jar))
       (cookie (make-instance 'drakma:cookie :name "known_user" :domain "www.myfitnesspal.com")))
   (setf (drakma:cookie-jar-cookies cookie-jar) (list cookie))
   (lisp-unit:assert-true (myfitnessdata:logged-in? cookie-jar))))

(lisp-unit:run-tests 
 logged-in-nil-when-no-cookies
 logged-in-nil-when-cookies-from-wrong-domain
 logged-in-t-when-proper-cookies)
