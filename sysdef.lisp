(in-package :sysdef-user)

(define-system :installer ()
  (:version 0 1)
  (:components "package" "installer")
  (:requires :drakma :mb.sysdef :puri :cl-ppcre :md5 :gzip-stream :archive))

