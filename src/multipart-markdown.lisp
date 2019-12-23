(in-package :multipart-markdown)

(defun slurp (path)
  "Read string from file"
  (with-open-file (s path :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun spit (path content)
  "Write string to file"
  (with-open-file 
    (out path :direction :output :if-exists :supersede)
    (write-line content out)))

(defun parse-multipart-marker (src)
  
  )

(defun pack (path-md)
  
  )

(defun unpack (path-md)
  
  )




(in-package :cl-user)

