(ns com.phronemophobic.clobber.modes.clojure.evaluator
  (:refer-clojure :exclude [load-file]))

(defprotocol IEvaluator
  (evaluate [_ m])
  (load-file [_ m])
  (completions [_ m])
  ;; (interrupt?)
  (doc [_ m])
  (arglist [_ m]))

