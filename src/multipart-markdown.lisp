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
  "Join list of strings"
  (format nil (concatenate 'string "~{~A~^" sep "~}") str-list))

(defun mapi (fn ls)
  (mapcar fn ls (alexandria:iota (length ls))))

(defun load-markdown (path)
  "Read markdown to string"
  (slurp path))

(defun split-markdown-to-lines (md)
  "Split markdown content to lines"
  (ppcre:split #\Newline md))

(defun parse-marker (line)
  (ppcre:register-groups-bind (m) 
    ("<!--[\\s]*(.*)[\\s]*-->" line) 
    (ppcre:split #\Space (string-trim '(#\Space) m))))

(defun parse-directive (path)
  "Parse multipart-markdown directive"
  (let* ((lines (split-markdown-to-lines (load-markdown path)))
         (markers (list)))
    (remove-if-not #'car
                   (mapi (lambda (line i) 
                           (cons (parse-marker line) i))
                         lines))))

(defun partitionize (lines) 
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
              (setf (getf md :content) (append (getf md :content) 
                                               (list (nth i lines)))))))
    mds))

(defun write-markdown-to-file (md)
  "Write markdown to file"
  (spit (getf md :filename) 
        (join (list #\Newline) (getf md :content))))

(defun unpack (path-md-index)
  "Unpackage resources from a multipart-markdown"
  (mapc #'write-markdown-to-file 
        (partitionize (split-markdown-to-lines 
                        (load-markdown path-md-index))))
  nil)

(defun pack (path-md-dir)
  "Package resources to a multipart-markdown"
  nil)

(in-package :cl-user)

