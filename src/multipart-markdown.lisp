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


(defun split-markdown-to-lines (md)
  "Split markdown content to lines"
  (ppcre:split #\Newline md))

(defun parse-marker (line)
  (ppcre:register-groups-bind (m) 
    (*directive-pattern* line) 
    (ppcre:split #\Space (string-trim '(#\Space) m))))






(defparameter *directive-pattern* "<!--[\\s]*(\\(.*\\))[\\s]*-->")

(defun parse-source-line (line)
  "Parse line"
  (let ((dir nil))
    (setf dir (ppcre:register-groups-bind (match) 
                (*directive-pattern* line) 
                match))
    (if dir
        (read-from-string dir)
        (list 'text :content line))))

(defun make-eval-context ()
  (list
    :content-buffer nil
    :current-part-name nil
    :current-mode nil
    :part (list)))

(defun d-begin-part (context prms)
  (setf (getf context :content-buffer) nil)
  (setf (getf context :current-part-name) (getf prms :name))
  (setf (getf context :current-mode) :part)
  context)

(defun d-text (context prms)
  (setf (getf context :content-buffer)
        (concatenate 'string 
                     (getf context :content-buffer) 
                     (if (getf context :content-buffer) 
                         (list #\Newline) 
                         "")
                     (getf prms :content)))
  context)

(defun d-end-part (context prms)
  (setf (getf context :part) 
        (append (getf context :part)
                (list (list :name (getf context :current-part-name)
                            :content (getf context :content-buffer)))))
  (setf (getf context :content-buffer) nil)
  (setf (getf context :current-part-name) nil)
  (setf (getf context :current-mode) nil)
  context)

(defun eval-source-line (source-line context)
  "Evaluate source line"
  
  (let ()
    ;; begin-part
    (when (eql (car source-line) 'begin-part)
      (funcall #'d-begin-part context (cdr source-line)))
    
    ;; text
    (when (eql (car source-line) 'text)
      (funcall #'d-text context (cdr source-line)))

    ;; end-part
    (when (eql (car source-line) 'end-part)
      (funcall #'d-end-part context (cdr source-line))))
  context)

(defun source-multipart-markdown (path)
  (mapcar #'parse-source-line 
          (split-markdown-to-lines (slurp path))))

(defun eval-source-list (source-list env) nil)

(defun read-multipart-markdown (path)
  "Read multipart-markdown"
  (let* ((v-path path)
         (v-content (slurp path))
         (v-source-list (mapcar #'parse-source-line 
                                (split-markdown-to-lines v-content)))
         (v-part (list 1 2 3)))
    (list :path v-path
          :content v-content
          :directive v-source-list
          :part v-part)))

; (defun parse-multipart-markdown (content)
;   "Parse multipart-markdown directive"
;   (let* ((lines (split-markdown-to-lines content)))
;     (remove-if-not #'car
;                    (mapi (lambda (line i) 
;                            (cons (parse-marker line) i))
;                          lines))))

(defun compile-directive (dir)
  
  )

(defun eval-directive (dirs)
  (loop for i from 0 below (length dirs) do
        (progn
          (format t "~A~%" (nth i dirs)))))

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


(defparameter *pattern-part* 
  (concatenate 'string 
               "<!--[\\s]*begin-part[\\s]*(.*)[\\s]*-->"
               "([\\s\\S]*?)"
               "<!--[\\s]*end-part[\\s]*-->"))

(defun unpack-regexp (path)
  "Unpackage multipart-markdown using regular expression"

  (let ((target (slurp path))
        (part (list)))
    ;; Parse multipart markdown
    (ppcre:do-matches (s e *pattern-part* target nil) 
      (ppcre:register-groups-bind 
        (header body) 
        (*pattern-part* (subseq target s e)) 
        (setf part (append part (list (list :header header :body body))))))
    
    ;; Output to files
    (mapc 
      (lambda (m)
        (let ((header (ppcre:split #\Space (string-trim '(#\Space) (getf m :header)))))
          ;; type = markdown
          (when (equal (car header) "markdown")
            (spit (cadr header) (getf m :body)))))
      part)))

(defun unpack (path-md-index)
  "Unpackage resources from a multipart-markdown"
  (mapc #'write-markdown-to-file 
        (partitionize 
          (split-markdown-to-lines 
            (getf (read-multipart-markdown path-md-index) :content))))
  nil)

(defun scan-link (path)
  "Scan link"
  
  (let ((target (slurp path))
        (pattern-markdown "[^\\!]*\\[(.*)\\]\\((.*)\\)")
        (pattern-image "\\!\\[(.*)\\]\\((.*)\\)")
        (links (list)))
    ;; Scan markdown link
    (ppcre:do-matches
      (s e pattern-markdown target nil)
      (ppcre:register-groups-bind (name link) (pattern-markdown (subseq target s e)) 
        (setf links (append links (list (list :type :markdown :name name :link link))))))
    
    ;; Scan image link
    (ppcre:do-matches
      (s e pattern-image target nil)
      (ppcre:register-groups-bind (name link) (pattern-image (subseq target s e)) 
        (setf links (append links (list (list :type :image :name name :link link))))))
    
    links))


(defun pack (path-md-dir)
  "Package resources to a multipart-markdown"
  nil)



(in-package :cl-user)

