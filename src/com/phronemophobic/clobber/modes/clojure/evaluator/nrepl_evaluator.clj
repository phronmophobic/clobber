(ns com.phronemophobic.clobber.modes.clojure.evaluator.nrepl-evaluator
  (:refer-clojure :exclude [load-file])
  (:require [com.phronemophobic.clobber.modes.clojure.evaluator
             :as evaluator
             :refer [IEvaluator]]
            [clojure.core.async :as async]
            [membrane.component :refer [defeffect]]
            [nrepl.transport :as transport]
            [nrepl.core :as nrepl]))


;; how do nrepl servers like bb and jank handle
;; reader conditionals?

;; https://cljdoc.org/d/nrepl/nrepl/1.8.0/doc/development/building-clients

(defn ^:private send-sync [conn msg]
  (let [{:keys [write-ch read-mult]} conn]
    
      (let [ret-ch (async/chan 10)
            msg (update msg :id (fn [id]
                                  (or id (str (random-uuid)))))
            msg-id (:id msg)]
        (try
          (async/tap read-mult ret-ch)
          (async/>!! write-ch msg)
          (loop [msgs []]
            (if-let [msg (async/<!! ret-ch)]
              (if (= (:id msg)
                     msg-id)
                (let [msgs (conj msgs msg)]
                  ;; status key signifies end of chain
                  (if (:status msg)
                    msgs
                    (recur msgs)))
                ;; else, msg id does not match. continue
                (recur msgs))
              ;; else, msg is nil. ret-ch is closed
              msgs))
          (finally
            (async/untap read-mult ret-ch)
            (async/close! ret-ch))))))

(comment
  
  (def my-conn x-106191) 

  (send-sync my-conn {:op "describe"})
  (send-sync my-conn {:op "lookup"
                      :ns "user"

                      :sym "foo"})
  

  
  (->> (send-sync my-conn {:op "eval"
                      :code "42"})
       (keep :value)
       last)
  
  
  (def my-evaluator (nrepl-evaluator my-conn))
   (evaluator/evaluate my-evaluator {:code "(range 10000000)"})
  )

(defn evaluate [{:keys [ns conn eval-opts]}
                {:keys [source-path source code line col]}]
  (let [responses (send-sync conn 
                             (merge {:op "eval"
                                     :code code
                                     :ns ns}
                                    eval-opts))

        result (->> responses
                    (keep :value)
                    last)
        
        err (->> responses
                 (keep :ex)
                 last)
        
        result (try
                 (read-string result)
                 (catch Exception e
                   result))]
    [err result]))

(defn doc [{:keys [conn ns]}
           {:keys [sym]}]
  (let [responses 
        (send-sync conn
                   {:op "lookup"
                    :sym sym
                    :ns ns})
        docinfo (->> responses
                     (keep :info)
                     first)]
    (when docinfo
      (assoc docinfo :type :var))))

(comment)

(defn completions [{:keys [ns conn]}
                   {:keys [prefix]}]
  (let [responses 
        (send-sync conn
                   {:op "completions"
                    :prefix prefix
                    :ns ns})]
    (-> responses
        first
        :completions)))

(comment
  (completions {:conn my-conn}
               {:ns "user"
                :prefix "clojure.string/"})
  ,)

(defn arglist [{:keys [ns conn]}
               {:keys [sym]}]
  (let [responses 
        (send-sync conn
                   {:op "lookup"
                    :sym sym
                    :ns ns})
        docinfo (->> responses
                     (keep :info)
                     first)]
    (:arglists docinfo)))

(defn load-file [{:keys [conn eval-opts]}
                 {:keys [source-path source code]}]
  (let [responses (send-sync conn 
                             (merge {:op "load-file"
                                     :file code
                                     :file-name source
                                     :file-path source-path}
                                    eval-opts))
        result (->> responses
                    (keep :value)
                    last)
        
        result (try
                 (read-string result)
                 (catch Exception e
                   result))]
    [nil result]))

(defrecord NREPLEvaluator [ns conn eval-opts]
  IEvaluator
  (evaluate [this m]
    (evaluate this m))
  (completions [this m]
    (completions this m))
  (doc [this m]
    (doc this m))
  (arglist [this m]
    (arglist this m))
  (load-file [this m]
    (load-file this m)))

(defn nrepl-evaluator [ns conn]
  (->NREPLEvaluator 
   ns
   conn 
   {:nrepl.middleware.print/buffer-size 1024
    :nrepl.middleware.print/quota 2048}))

(comment

  (require '[nrepl.core :as nrepl]
           '[nrepl.transport :as transport])
  (with-open [conn (nrepl/connect :port 1234)]
    (transport/send conn {:op "eval" :code "(defn foo \"asdf\" ([x] x))"})
    (transport/send conn {:op "lookup" :sym "foo"})
    (loop [msgs []]
      (if-let [msg (transport/recv conn 1000)]
        (recur (conj msgs msg))
        msgs)))
,)

