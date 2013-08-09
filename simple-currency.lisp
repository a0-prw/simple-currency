(in-package :cies)

(defvar *currencies* (make-hash-table :test 'equal))

(defun parse-rational (string)
  (destructuring-bind (int frac)
      (split-sequence:split-sequence #\. string)
    (+ (parse-integer int)
       (/ (parse-integer frac)
          (expt 10 (length frac))))))

(defun normalize-currency-designator (kw)
  (cond ((stringp kw)
         (string-upcase kw))
        ((keywordp kw)
         (symbol-name kw))
        (t (error "Don't know how to normalize ~S." kw))))

(defun update-currency-hash ()
  (let ((xml (ignore-errors 
               (xmls:parse 
                (drakma:http-request 
                 "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")))))
    (when xml
      (destructuring-bind ((jnk1 jnk2 (jnk3 ((dvar date)) &rest currencies)))
          (xmls:xmlrep-find-child-tags "Cube" xml)
        (declare (ignore jnk1 jnk2 jnk3 dvar))
        (setf (gethash "date" *currencies*) date)
        (mapcar #'(lambda (curr)
                    (destructuring-bind (jnk ((ratevar rate) (currvar curr)))
                        curr
                      (declare (ignore jnk ratevar currvar))
                      (setf (gethash curr *currencies*) (parse-rational rate))))
                currencies)))))

(defun available-currencies ()
  (let (cies)
    (loop for k being each hash-key of *currencies* using (hash-value v)
         do (when (rationalp v)
              (push k cies))
         finally (return cies))))

(defun getbase (curr)
  (gethash curr *currencies*))

(defgeneric convert (amount from to))

(defmethod convert ((amount number) (from t) (to t))
  (declare (inline normalize-currency-designator getbase))
  (let ((nfrom (normalize-currency-designator from))
        (nto (normalize-currency-designator to)))
    (* amount (/ (getbase nto) (getbase nfrom)))))


;; (defun convert (amount from to)
;;   (declare (inline normalize-currency-designator getbase))
;;   (let ((nfrom (normalize-currency-designator from))
;;         (nto (normalize-currency-designator to)))
;;     (* amount (/ (getbase nto) (getbase nfrom)))))

;; (defun imf-5-day ()
;;   (let ((strm
;;          (drakma:http-request 
;;           "http://www.imf.org/external/np/fin/data/rms_five.aspx?tsvflag=Y" 
;;           :want-stream t)))
;;         strm))

;; (unless (find-package :cl-csv)
;;   (ql:quickload :cl-csv))

;; (asdf:load-system :cl-csv-data-table)

;; (defun imf-data ()
;;   (cl-csv:read-csv (imf-5-day) :separator #\Tab))

;;(cl-csv:get-data-table-from-csv-list (imf-data))






