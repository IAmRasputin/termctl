(defpackage #:termctl
  (:use #:cl)
  (:import-from :alexandria :if-let)
  (:import-from :cl-user :quit)
  (:export :start-display))
