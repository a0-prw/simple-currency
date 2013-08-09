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
  (destructuring-bind (yr mo da) ;;CHECK
      (mapcar #'parse-integer
              (split-sequence:split-sequence #\- string))
    (simple-date:encode-date yr mo da)))

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
  (let ((xml (ignore-errors 
               (xmls:parse 
                (drakma:http-request 
                 "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")))))
    (if xml
        (destructuring-bind ((jnk1 jnk2 (jnk3 ((dvar date)) &rest currencies)))
            (xmls:xmlrep-find-child-tags "Cube" xml)
          (declare (ignore jnk1 jnk2 jnk3 dvar))
          (clrhash *currencies*)
          (setf (gethash "date" *currencies*) (make-date date))
          (mapcar #'(lambda (curr)
                      (destructuring-bind (jnk ((ratevar rate) (currvar curr)))
                          curr
                        (declare (ignore jnk ratevar currvar))
                        (setf (gethash curr *currencies*) (parse-rational rate))))
                  currencies)
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
  (:documentation "Converts AMOUNT between currencies FROM and TO, which should be
keywords designating 3 letter currency codes.  Returns 2 values: a
rational number representing the converted amount in TO currency, and
a SIMPLE-DATE:INTERVAL representing the interval since the currency
hash was last updated.  For a list of available currency codes
call (AVAILABLE-CURRENCIES)."))

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






