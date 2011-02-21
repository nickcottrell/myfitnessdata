(defpackage :myfitnessdata-tests
  (:use :common-lisp))

(in-package :myfitnessdata-tests)

(load "myfitnessdata.lisp")
(ql:quickload '("lisp-unit"))

(lisp-unit:define-test logged-in-false
    (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
	  (lisp-unit:assert-false (myfitnessdata:logged-in? cookie-jar))))

(lisp-unit:run-tests logged-in-false)
