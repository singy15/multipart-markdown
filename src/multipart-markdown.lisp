(in-package :multipart-markdown)

(defvar +pattern-part+ 
  (concatenate 'string 
               "<!--[\\s]*begin-part[\\s]*(.*)[\\s]*-->"
               "([\\s\\S]*?)"
               "<!--[\\s]*end-part[\\s]*-->"))

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

(defun unpack (path)
  "Unpackage multipart-markdown using regular expression"

  (let ((target (slurp path))
        (part (list)))
    ;; Parse multipart markdown
    (ppcre:do-matches (s e +pattern-part+ target nil) 
      (ppcre:register-groups-bind 
        (header body) 
        (+pattern-part+ (subseq target s e)) 
        (setf part (append part (list (list :header header :body body))))))
    
    ;; Output to files
    (mapc 
      (lambda (m)
        (let ((header (ppcre:split #\Space (string-trim '(#\Space) (getf m :header)))))
          ;; type = markdown
          (when (equal (car header) "markdown")
            (ensure-directories-exist (cadr header))
            (spit (cadr header) (getf m :body)))
          
          ;; type = image
          (when (equal (car header) "image")
            (ensure-directories-exist (cadr header))
            (write-base64 (cadr header) (getf m :body)))))
      part)))

(defun scan-link (path)
  "Scan link"
  
  (let ((target (slurp path))
        (pattern-markdown "^[\\s]*\\[(.*)\\]\\((.*)\\)")
        (pattern-image "^[\\s]*\\!\\[(.*)\\]\\((.*)\\)")
        (links (list)))
    (mapc
      (lambda (line) 
        ;; Scan markdown link
        (ppcre:do-matches
          (s e pattern-markdown line nil)
          (ppcre:register-groups-bind (name link) (pattern-markdown (subseq line s e)) 
            (setf links (append links (list (list :type :markdown :name name :link link))))))
        
        ;; Scan image link
        (ppcre:do-matches
          (s e pattern-image line nil)
          (ppcre:register-groups-bind (name link) (pattern-image (subseq line s e)) 
            (setf links (append links (list (list :type :image :name name :link link)))))))
      (ppcre:split #\Newline target))
    links))

(defun to-base64 (path) 
  "Convert file to base64 string"
  (let ((bytes (list)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (loop :for b := (read-byte in nil -1)
            :until (= -1 b)
            :do (progn
                  (setf bytes (append bytes (list b))))))
    (with-output-to-string (out) 
      (s-base64:encode-base64-bytes (coerce bytes 'vector) out))))

(defun write-base64 (path base64)
  (with-open-file (out path
                     :direction :output 
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
    (mapc (lambda (b) (write-byte b out))
          (with-input-from-string (in base64) 
            (coerce (s-base64:decode-base64-bytes in) 'list)))))

(defun path-html2md (path)
  (concatenate 'string
    (namestring (cl-fad:pathname-directory-pathname (pathname path)))
    (pathname-name (pathname path))
    ".md"))

(defun recursive-pack (path link-type)
  "Package markdown recursively"
  (join
    (list #\Newline)
    (list
      (if (equal link-type :markdown)
         (format nil "<!-- begin-part markdown ~A -->" (path-html2md path))
         (format nil "<!-- begin-part image ~A -->" path))
      (if (equal link-type :markdown)
          (string-trim '(#\Space #\Newline #\Tab) (slurp (path-html2md path)))(to-base64 path))
      (if (equal link-type :markdown)
         (format nil "<!-- end-part -->~%")
         (format nil "<!-- end-part -->~%"))
      (if (equal link-type :markdown)
         (reduce
           (lambda (memo x)
             (concatenate 'string memo (recursive-pack (getf x :link) (getf x :type))))
           (scan-link (path-html2md path))
           :initial-value "")
         ""))))

(defun pack (path-out path-index-md)
  "Package resources to a multipart-markdown"
  (let ((packed (recursive-pack path-index-md :markdown)))
    (spit path-out packed)
    packed))

(defun main ()
  (let ((command (nth 1 sb-ext:*posix-argv*)))
    (cond ((equal command "pack") (apply #'pack (cddr sb-ext:*posix-argv*)))
          ((equal command "unpack") (apply #'unpack (cddr sb-ext:*posix-argv*))))))

(in-package :cl-user)

