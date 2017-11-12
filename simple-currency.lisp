;; Copyright (c) 2013, Peter Wood
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
    
;;     Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cies)

(defvar *currencies* (make-hash-table :test 'equal))

(defvar *stored-currency-hash* "stored-currency-hash")

(defun parse-rational (string)
  (destructuring-bind (int frac)
      (split-sequence:split-sequence #\. string)
    (+ (parse-integer int)
       (/ (parse-integer frac)
          (expt 10 (length frac))))))

(defun make-date (string)
  (destructuring-bind (yr mo da)
      (mapcar #'parse-integer
              (split-sequence:split-sequence #\- string))
    (simple-date:encode-date yr mo da)))

(defun ccydb-date ()
  (gethash "date" *currencies*))

(defun normalize-currency-designator (kw)
  (cond ((stringp kw)
         (string-upcase kw))
        ((keywordp kw)
         (symbol-name kw))
        (t (error "Don't know how to normalize ~S." kw))))

(defun update-currency-hash ()
  "Returns the updated hash table stored in global variable
*CURRENCIES* if the update is believed to be successful, or restores
the old stored hash table if the call to ECB failed and returns the
old table, or NIL if both options failed for any reason."
  (let ((root (ignore-errors 
               (plump:parse 
                (dex:get  "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")))))
    (if root  
	(progn
	  (mapcar (lambda (c)
		    (let ((cdata (plump:attributes c)))
		      (cond ((= (hash-table-count cdata) 2)
			     (setf (gethash (gethash "currency" cdata) *currencies*)
				   (parse-rational (gethash "rate" cdata))))
			    ((= (hash-table-count cdata) 1)
			     (setf (gethash "date" *currencies*)
				   (make-date (gethash "time" cdata))))
			    (t nil)))) (plump:get-elements-by-tag-name root "Cube"))
          (setf (gethash "EUR" *currencies*) 1)
          (cl-store:store *currencies* *stored-currency-hash*)
          *currencies*)
        (if (ignore-errors (setf *currencies* 
                                 (cl-store:restore *stored-currency-hash*)))
            *currencies* nil))))

(defun available-currencies ()
  (let (cies)
    (loop for k being each hash-key of *currencies* using (hash-value v)
         do (when (rationalp v)
              (push k cies))
         finally (return cies))))

(defun getbase (curr)
  (gethash curr *currencies*))

(defun interval-since-update (date)
  (multiple-value-bind (x y z da mo yr)
      (get-decoded-time)
    (declare (ignore x y z))
    (simple-date:time-subtract 
     (simple-date:encode-date yr mo da)
     date)))

(defgeneric convert (amount from to)
  (:documentation "Converts AMOUNT between currencies FROM and TO,
which should be keywords designating 3 letter currency codes.  Returns
2 values: a rational number representing the converted amount in TO
currency, and a SIMPLE-DATE:INTERVAL representing the interval since
the currency hash was last updated.  For a list of available currency
codes call (AVAILABLE-CURRENCIES)."))

(defmethod convert ((amount number) (from t) (to t))
  (declare (inline normalize-currency-designator getbase))
  (let ((nfrom (normalize-currency-designator from))
        (nto (normalize-currency-designator to))
        (date (gethash "date" *currencies*)))
    (values (* amount (/ (getbase nto) (getbase nfrom)))
            (interval-since-update date))))

(defmethod convert ((amount number) (from (eql :eur)) (to t))
  (declare (inline normalize-currency-designator getbase))
  (let ((nto (normalize-currency-designator to))
        (date (gethash "date" *currencies*)))
    (values (* amount (getbase nto))
            (interval-since-update date))))

(defmethod convert ((amount number) (from t) (to (eql :eur)))
  (declare (inline normalize-currency-designator getbase))
  (let ((nfrom (normalize-currency-designator from))
        (date (gethash "date" *currencies*)))
    (values (* amount (/ 1 (getbase nfrom)))
            (interval-since-update date))))

(defgeneric get-quote (amount from to)
  (:documentation "Return 3 values: 1) The conversion as a rational,
  2) the rate for converting from FROM to TO as a rational, and 3) a
  SIMPLE-DATE:INTERVAL representing the interval since the currency
  hash was last updated."))

(defmethod get-quote ((amount number) (from t) (to t))
  (declare (inline normalize-currency-designator getbase))
  (let* ((nfrom (normalize-currency-designator from))
        (nto (normalize-currency-designator to))
        (rate (/ (getbase nto) (getbase nfrom)))
        (date (gethash "date" *currencies*)))
    (values (* amount rate) rate (interval-since-update date))))

(defmethod get-quote ((amount number) (from (eql :eur)) (to t))
  (declare (inline normalize-currency-designator getbase))
  (let* ((nto (normalize-currency-designator to))
        (date (gethash "date" *currencies*))
        (rate (getbase nto)))
    (values (* amount rate) rate (interval-since-update date))))

(defmethod get-quote ((amount number) (from t) (to (eql :eur)))
  (declare (inline normalize-currency-designator getbase))
  (let* ((nfrom (normalize-currency-designator from))
         (date (gethash "date" *currencies*))
         (rate (/ 1 (getbase nfrom))))
    (values (* amount rate) rate (interval-since-update date))))

;; These assume that 2 digits after the decimal are correct for the
;; currency and we don't give a damn about type of rounding
;; used. Yeeha!

(defun 2dd-round (rat)
  (unless (rationalp rat)
    (error "I require a RAT! I was given ~S." rat))
  (/ (round (* rat 100)) 100))

(defun display-currency (amount &optional ccode)
  (let ((am (2dd-round amount)))
    (if ccode 
        (format nil "~A ~$" (normalize-currency-designator ccode) am)
        (format nil "~$" am))))

(defmacro with-currency (currency conversion-form)
  (let (binds converted-form)
    (labels ((triple (list)
               (and (car list)
                    (cadr list)
                    (caddr list)))
             (tree-replace (tree)
               (if (atom tree) 
                   tree
                   (if (and (triple tree)
                            (eql (car tree) 'convert)
                            (numberp (cadr tree))
                            (keywordp (caddr tree)))
                       (let ((var (gensym))
                             (bind `(convert ,(cadr tree) ,currency ,(caddr tree))))
                         (setf binds (push (list var bind) binds))
                         `(display-currency ,var ,(caddr tree)))
                       (let ((head (car tree))
                             (tail (cdr tree)))
                         (cons (tree-replace head)
                               (tree-replace tail)))))))
      (setf converted-form (tree-replace conversion-form))
      `(let ,binds
         ,converted-form))))
                            



