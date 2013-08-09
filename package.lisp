(defpackage :simple-currency
  (:use :cl)
  (:nicknames :cies)
  (:export :update-currency-hash
           :available-currencies
           :convert :*stored-currency-hash*))
