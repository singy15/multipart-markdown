(defsystem "multipart-markdown"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cl-ppcre" "alexandria" "s-base64" "cl-fad")
  :components ((:module "src"
                :components
                ((:file "package-multipart-markdown")
                 (:file "multipart-markdown"))))
  :description ""
  :in-order-to ((test-op (test-op "multipart-markdown/tests"))))

(defsystem "multipart-markdown/tests"
  :author ""
  :license ""
  :depends-on ("multipart-markdown"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for multipart-markdown"
  :perform (test-op (op c) (symbol-call :rove :run c)))
