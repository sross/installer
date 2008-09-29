(in-package :cl-user)

(defpackage #:installer
  (:use :cl :sysdef :md5 :drakma :gzip-stream :archive :puri :cl-ppcre)
  (:export #:install #:update #:upgrade #:add-definition #:remove-definition
   #:*proxy-server* #:*proxy-authorization*))



