;; MyFitnessData - a CSV web scraper for the MyFitnessPal website.
;; Copyright (C) 2011 "Duncan Bayne" <dhgbayne@gmail.com>
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

(load "myfitnessdata-tests.lisp")

(in-package :myfitnessdata)

(defmacro run-tests-and-quit (&body tests)
  `(if (lisp-unit:run-tests ,@tests)
       (sb-ext:quit :unix-status 1)
     (sb-ext:quit :unix-status 0)))

(run-tests-and-quit myfitnessdata-tests:logged-in-nil-when-no-cookies
		    myfitnessdata-tests:logged-in-nil-when-cookies-from-wrong-domain
		    myfitnessdata-tests:logged-in-t-when-cookies-from-right-domain
		    myfitnessdata-tests:make-csv-from-list
		    myfitnessdata-tests:make-csv-from-list-is-sorted)