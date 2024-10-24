(defsystem "motion-estimation"
  :version "0.0.1"
  :author "Ghislain Bonnard"
  :license ""
  :depends-on (:imago
               :py4cl)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "TVID TP1 implementation"
  :in-order-to ((test-op (test-op "motion-estimation/tests"))))

(defsystem "motion-estimation/tests"
  :author ""
  :license ""
  :depends-on ("motion-estimation"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for motion-estimation TP"
  :perform (test-op (op c) (symbol-call :rove :run c)))
