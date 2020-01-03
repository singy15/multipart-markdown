(in-package :multipart-markdown)

;;; Pattern for parsing
(defvar +pattern-part+ 
  (concatenate 'string 
               "<!--[\\s]*begin-part[\\s]*(.*)[\\s]*-->"
               "([\\s\\S]*?)"
               "<!--[\\s]*end-part[\\s]*-->"))

;;; Read a file as string.
(defun slurp (path)
  (with-open-file (s path :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

;;; Write string to file.
;;; Character encoding is not specified.
(defun spit (path content)
  (with-open-file 
    (out path :direction :output :if-exists :supersede)
    (write-line content out)))

;;; Join list of strings with separater.
(defun join (sep str-list)
  (format nil (concatenate 'string "~{~A~^" sep "~}") str-list))

;;; Unpackage multipart-markdown using regular expression.
(defun unpack (path)
  (let ((target (slurp path))
        (part (list)))
    ;; Parse multipart markdown.
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
          ;; decode base64 string
          (when (equal (car header) "image")
            (ensure-directories-exist (cadr header))
            (write-base64 (cadr header) (getf m :body)))))
      part)))

;;; Check the path is local or remote.
(defun p-remote-path (path)
  (ppcre:scan "^(http://|https://|ftp://).*$" path))

;;; Scan links.
(defun scan-link (path)
  (let ((target (slurp path))
        (pattern-markdown "^[\\s]*\\[(.*)\\]\\((.*)\\)")
        (pattern-image "^[\\s]*\\!\\[(.*)\\]\\((.*)\\)")
        (links (list)))
    (mapc
      (lambda (line) 
        ;; Match markdown link pattern.
        (ppcre:do-matches
          (s e pattern-markdown line nil)
          (ppcre:register-groups-bind (name link) (pattern-markdown (subseq line s e)) 
            (when (not (p-remote-path link))
              (setf links (append links (list (list :type :markdown :name name :link link)))))))
        
        ;; Match image link pattern.
        (ppcre:do-matches
          (s e pattern-image line nil)
          (ppcre:register-groups-bind (name link) (pattern-image (subseq line s e)) 
            (when (not (p-remote-path link))
              (setf links (append links (list (list :type :image :name name :link link))))))))
      (ppcre:split #\Newline target))
    links))

;;; Convert binary file to base64 string
(defun to-base64 (path) 
  (let ((bytes (list)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (loop :for b := (read-byte in nil -1)
            :until (= -1 b)
            :do (progn
                  (setf bytes (append bytes (list b))))))
    (with-output-to-string (out) 
      (s-base64:encode-base64-bytes (coerce bytes 'vector) out))))

;;; Write binary file by base64 string
(defun write-base64 (path base64)
  (with-open-file (out path
                     :direction :output 
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
    (mapc (lambda (b) (write-byte b out))
          (with-input-from-string (in base64) 
            (coerce (s-base64:decode-base64-bytes in) 'list)))))

;;; Convert the link for .html to a link for .md.
(defun path-html2md (path)
  (concatenate 'string
    (namestring (cl-fad:pathname-directory-pathname (pathname path)))
    (pathname-name (pathname path))
    ".md"))

;;; TODO: too complex
;;; TODO: convert only links for markdown
;;; Package markdown recursively.
(defun recursive-pack (path link-type)
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

;;; Package resources to a multipart-markdown.
(defun pack (path-out path-index-md)
  (let ((packed (recursive-pack path-index-md :markdown)))
    (spit path-out packed)
    packed))

;;; Program entry point.
(defun main ()
  (let ((command (nth 1 sb-ext:*posix-argv*)))
    (cond ((equal command "pack") (apply #'pack (cddr sb-ext:*posix-argv*)))
          ((equal command "unpack") (apply #'unpack (cddr sb-ext:*posix-argv*))))))

(in-package :cl-user)

