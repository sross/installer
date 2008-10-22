(in-package :sysdef-user)

(define-system :installer (serial-system)
  (:version 0 1 2)
  (:components "package" "installer")
  (:needs :drakma (:mb.sysdef :version (> 0 1 3)) :puri :cl-ppcre :md5 :gzip-stream :archive))

