;;; We don't define an `uninstall` action since it's a portability nightmare trying
;;; to delete directories.

;; TODO: How should this behave with patches?
;;       upgrade action should download patches for a system
;;       upgrade should take a :system argument



;; move it and all systems onto mudballs.com
;; remove all local systems except for the initially required ones.

;; test
;; add upgrade and remove
;; tag release :)

(in-package #:installer)

;; Utils
(defun dbg (fmt-ctrl &rest fmt-args)
  (apply 'format *debug-io* fmt-ctrl fmt-args)
  (fresh-line *debug-io*))

;; Specials
(defvar *proxy-server* nil)
(defvar *proxy-authorization* nil)


;; Conditions
(define-condition requested-file-unavailable (serious-condition)
  ((url :initarg :url :initform "Unknown" :reader url-of))
  (:report (lambda (c s)
             (format s "The file requested from ~A is not available." (url-of c)))))

(define-condition md5sum-mismatch (error)
  ((received :initarg :received) (expected :initarg :expected) (system :initarg :system))
  (:report (lambda (c s)
             (with-slots (received expected system) c
               (format s "Md5 mismatch when downloading system ~S. Expected ~S but received ~S"
                       system expected received)))))

(define-condition no-provider (error)
  ((system :initarg system :reader system-of))
  (:report (lambda (c s)
             (format s "Attempting to download system ~A failed as it has no provider."
                     (system-of c)))))

(define-condition download-invalid (error)
  ((system :initarg :system :reader system-of))
  (:report (lambda (c s)
             (format s "Components missing from download of ~A.~%The following components where not present after the download: ~{~S~^, ~}."
                     (system-of c) (remove-if 'component-exists-p (components-of (system-of c)))))))

;; Util Functions
(defun copy-stream (input output)
  (let ((buf (make-array (min (expt 2 16) array-dimension-limit)
                         :element-type '(unsigned-byte 8))))
    (loop for bytes-read = (read-sequence buf input)
          until (zerop bytes-read) :do
          (write-sequence buf output :end bytes-read))))

;; This has been `borrowed` from Nathan Froyds excellent ironclad package.
(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'character))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits "0123456789abcdef"))
    (loop with string = (make-string (* length 2)
                                     :element-type element-type)
          for i from start below end
          for j from 0 below (* length 2) by 2
          do (let ((byte (aref vector i)))
               (setf (aref string j)
                     (aref hexdigits (ldb (byte 4 4) byte))
                     (aref string (1+ j))
                     (aref hexdigits (ldb (byte 4 0) byte))))
          finally (return string))))

(defun check-md5sum (source system)
  (when (md5sum-of system)
    (dbg "Verifying md5sum.")
    (let ((md5 (byte-array-to-hex-string (md5sum-file source))))
      (unless (string= (md5sum-of system) md5)
        (error 'md5sum-mismatch :expected (md5sum-of system) :received md5 :system system))
      (dbg "Md5sum matches."))))

(defmacro with-temp-file ((var-name) &body body)
  (let ((gfile (gensym)))
    `(let* ((,gfile (create-temp-file))
            (,var-name ,gfile))
       (ensure-directories-exist ,var-name)
       (unwind-protect (progn ,@body)
         (when (probe-file ,gfile) (delete-file ,gfile))))))

(defun create-temp-file ()
  (let ((path (merge-pathnames (make-pathname :directory '(:relative "tmp")
                                              :name (format nil "download~D" (random 1000)) :type "lisp")
                               *root-pathname*)))
    (ensure-directories-exist path)
    (if (probe-file path)
        (create-temp-file)
        path)))
        
;; EXPORTED METHODS

;; It doesn't make sense defining install as an action since
;; the value of the action mechanism is in the recursive `descend into children`
;; behaviour which this action doesn't need.
(defgeneric install (name &key version file)
  (:method :before (name &key version file)
   (declare (ignore version))
   (when file (error "File based installation is not supported yet, don't worry we're working on it.")))
  (:method ((name symbol) &key version &allow-other-keys)
   (install (find-system name :version version)))
  (:method ((name string) &key version &allow-other-keys)
   (install (find-system name :version version)))
  (:method ((system system) &key &allow-other-keys)
   (when (supportedp system)
     (dbg "Installing System ~A." system)
     (if (component-exists-p system)
         (dbg "System ~A already present, skipping." system)
         (with-temp-file (system-source)
           (download-source system :into system-source)
           (check-md5sum system-source system)
           (extract-source system system-source)
           (verify-download system)))
     (download-dependencies system)
     (perform system 'load-action))))

(defmethod verify-download ((system system))
  (unless (every 'component-exists-p (components-of system))
    (error 'download-invalid :system system)))

(defmethod dependent-systems-of ((system system))
  (mapcar 'cdr (dependencies-of (make-instance 'action) system)))
  
(defmethod download-dependencies ((system system))
  (when (dependent-systems-of system)
    (dbg "Processing Dependencies.")
    (mapcar 'install (dependent-systems-of system))))

(defmethod system-file-name ((system system))
  (format nil "~(~A.~A.~A~)" (name-of system) (sysdef::version-string system)  "mb"))

(defmethod download-url-of ((system system))
  (merge-uris (uri (format nil "~{~A~^/~}"
                           (list "systems" (system-file-name system))))
              (url-of (provider-of system))))

(defmethod download-source (system &key into)
  (dbg "Downloading source for ~A from ~A." system (download-url-of system))
  (unless (provider-of system)
    (error 'no-provider :system system))
  (let ((url (download-url-of system)))
    (multiple-value-bind (stream status)
        (http-request url :proxy *proxy-server* :want-stream t
                      :proxy-basic-authorization *proxy-authorization*)
      (with-open-stream (input stream)
        (unless (= status 200)
          (error 'requested-file-unavailable :url url))
        (with-open-file (output into :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
          (copy-stream input output))))))

(defun extract-source (system source)
  (dbg "Extracting to ~S." (component-pathname system))
  (let ((target-dir (component-pathname system)))
    (gzip-stream:with-open-gzip-file (ins source)
      (archive:with-open-archive (archive ins)
        (let ((*default-pathname-defaults* (pathname target-dir)))
          (archive:do-archive-entries (entry archive) 
            (archive:extract-entry archive entry)))))))

(defun split-uri-path (uri)
  "Splits the path of URI into 2 values. A directory list and a name+type."
  (let ((split (remove "" (split "/" (uri-path uri)) :test 'string=)))
    (let ((dir (butlast split))
          (name (car (last split))))
      (when (null name)
        (error "Uri ~S does not seem to contain a path with a filename." uri))
      (values dir name))))

(defun url-to-path (url)
  (let ((uri (uri url)))
    (multiple-value-bind (dir name) (split-uri-path uri)
      (merge-pathnames (merge-pathnames (pathname name)
                                        (make-pathname :directory (list* :relative (uri-host uri) dir)))
                       *sysdef-path*))))

;; FIXME: This currently binds us to http transports which loses in the long run.
(defun path-to-url (path)
  (let* ((enough (enough-namestring path *sysdef-path*)))
    (format nil "http://~{~A/~}~A"
            (remove-if #'keywordp (pathname-directory enough) :count 1)
            (file-namestring enough))))

(defun add-definition (url &key reload)
  (let ((path (url-to-path url)))
    (when (and (probe-file path) (not reload))
      (return-from add-definition t))
    (ensure-directories-exist path)
    (multiple-value-bind (stream status)
        (http-request url :proxy *proxy-server* :proxy-basic-authorization *proxy-authorization*
                      :want-stream t)
      (with-open-stream (input stream)
        (unless (= status 200)
          (error 'requested-file-unavailable :url url))
        (with-open-file (output path :element-type '(unsigned-byte 8) :direction :output
                                :if-exists :supersede)
          (copy-stream input output))))
    t))

(defun remove-definition (url)
  (let ((path (url-to-path url)))
    (when (probe-file path)
      (let ((provider (sysdef::definition-file-provider path)))
        (delete-file path)
        (setf sysdef::*systems*
              (remove-if (lambda (sys)
                           (eql (provider-of sys) provider))
                         sysdef::*systems*))))))

(defun update (url)
  (add-definition url :reload t))

(defun upgrade ()
  (dolist (comp (all-files (find-system :sysdef-definitions)))
    (with-simple-restart (ignore "Ignore")
      (update (path-to-url (input-file comp)))))

  ;; TODO: Write me
  ;; Download latest version of installed systems
  ;; OR
  ;; download patches for installed systems
  )

;; EOF

;(pathname-directory (enough-namestring "foo/bar/baz.lisp" #P""))
;(path-to-url (url-to-path "http://mudballs.com/stable/mudballs.lisp"))
;(add-definition "http://mudballs.com/stable/mudballs.lisp")
;(remove-definition "http://mudballs.com/stable/mudballs.lisp")

;(url-to-path "http://mudballs.com/stable/mudballs.lisp")




