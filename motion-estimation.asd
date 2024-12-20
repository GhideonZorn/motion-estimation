(asdf:defsystem "motion-estimation"
  :version "0.0.1"
  :author "Ghislain Bonnard"
  :license ""
  :depends-on (:imago
               :py4cl)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "pixel-wise")
                 (:file "bma")
                 (:file "hsl")
                 (:file "display"))))
  :description "TVID TP's implementation"
  :in-order-to ((test-op (test-op "motion-estimation/tests"))))

(asdf:defsystem "motion-estimation/tests"
  :author ""
  :license ""
  :depends-on ("motion-estimation"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for motion-estimation TP's"
  :perform (test-op (op c) (symbol-call :rove :run c)))
