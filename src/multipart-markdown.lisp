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

(defun join (sep str-list)
  (format nil (concatenate 'string "~{~A~^" sep "~}") str-list))

(defun load-markdown (path)
  (slurp path))

(defun split-markdown-to-lines (md)
  (ppcre:split #\Newline md))

(defun parse-marker (line)
  (ppcre:register-groups-bind (m) 
    ("<!--[\\s]*(.*)[\\s]*-->" line) 
    (ppcre:split #\Space (string-trim '(#\Space) m))))

(defun partitionize-markdown (lines) 
  "Partitionize markdown"
  (let ((mds (list))
        (md nil)
        (line nil)
        (marker nil))
    (loop for i from 0 below (length lines) do
          (progn
            ; Parse marker
            (setf marker (parse-marker (nth i lines)))
            
            (if (and (not (equal marker nil)) 
                     (equal "@file" (nth 0 marker)))
              (progn
                (setf md (list :filename (nth 1 marker) :content (list)))
                (setf mds (append mds (list md))))
              (setf (getf md :content) (append (getf md :content) (list (nth i lines)))))))
    mds))

(defun demultipart-markdown (path)
  (mapc
    (lambda (m)
      (spit (getf m :filename) 
            (join (list #\Newline) (getf m :content))))
    (partitionize-markdown (split-markdown-to-lines (load-markdown path)))))


(defun pack (path-md)
  
  )

(defun unpack (path-md)
  
  )




(in-package :cl-user)

