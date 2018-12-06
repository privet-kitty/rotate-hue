(defsystem "rotate-hue"
  :version "0.0.1"
  :author "Hugo I."
  :license "MIT"
  :depends-on ("opticl" "dufy")
  :components ((:module "src"
                :components
                ((:file "rotate-hue"))))
  :build-operation program-op
  :entry-point "rotate-hue:main"
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "rotate-hue/test"))))

(defsystem "rotate-hue/test"
  :depends-on ("rotate-hue"
               "fiveam")
  :components ((:file "test"))
  :description "Test system for rotate-hue"
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run! 'rotate-hue/test:main-suite)")))
