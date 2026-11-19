(ns com.phronemophobic.clobber.modes.clojure.evaluator.eval-evaluator
  (:refer-clojure :exclude [load-file])
  (:require [com.phronemophobic.clobber.modes.clojure.evaluator
             :as evaluator
             :refer [IEvaluator]]
            clojure.java.doc.api
            clojure.reflect
            compliment.core
            [clojure.string :as str]
            [com.phronemophobic.source-logging-pushback-reader :as slpr]))



(defn evaluate [{:keys [source-path source code read-opts line col ns]}]
  (push-thread-bindings 
   (cond-> {#'*repl* true}
     ns (assoc #'*ns* (the-ns ns))
     source-path (assoc Compiler/SOURCE_PATH source-path
                        Compiler/SOURCE source)))
  (try
    (let [rdr (slpr/source-logging-pushback-reader code line col)
          form (read read-opts rdr)]
      [nil (eval form)])
    (catch Throwable e
      [e nil])
    (finally
      (pop-thread-bindings)))
  
  )

(comment
  
  (evaluate {:source-path "foo/bar.clj"
             :source "bar.clj"
             :code "(/ 1 0 )"
             :line 40
             :col 10
             :ns (the-ns 'user)
             :read-opts {:read-cond :allow
                         :features #{:clj}}})
  ,)

(comment
  (doc-string {:ns 'user
               :sym "when"})
  ,)

(defn doc-string [{:keys [ns]
                   sym-str :sym
                   :as m}]
  (let [sym (read-string sym-str)
        v (ns-resolve ns sym)
        mta (meta v)
        doc (:doc mta)]
    (if v #_(or (seq? (:arglists mta))
            (string? doc))
      {:name sym
       :type :var
       :ns ns
       :doc doc
       :arglists (:arglists mta)}

      ;; try javadoc
      (if-let [jdoc-data (binding [*ns* ns]
                           (try
                             (clojure.java.doc.api/javadoc-data-fn
                              sym-str
                              nil)
                             (catch Exception e
                               (tap> e)
                               nil)))]
        (assoc jdoc-data
               :type :javadoc)

        ;; try compliment
        (let [docstring (compliment.core/documentation
                         sym-str
                         ns)]
          {:type :string
           :string docstring})))))

(defn completions [{:keys [ns prefix ]}]

  (let [completions (into
                     []
                     (compliment.core/completions prefix
                                                  {:ns ns
                                                   :sort-order :by-name}))]
    completions))

(defn arglist [{:keys [ns sym ]}]
  (let [sym (read-string sym)
        v (ns-resolve ns sym)
                        
        arglists (-> v meta :arglists)]
    (if arglists
      arglists
      ;; else 
      ;; couldn't find arglist from metadata.
      ;; check if it is a method value
      (when (qualified-symbol? sym)
        (when-let [cls (ns-resolve ns (symbol (namespace sym)) )]
          (when (class? cls)
            (let [method-name (let [sym-name (clojure.core/name sym)]
                                (if (str/starts-with? sym-name ".")
                                  (subs sym-name 1)
                                  sym-name))
                  {:keys [members]} (clojure.reflect/type-reflect cls)
                  arglists (into []
                                 (comp (filter (fn [{:keys [name]}]
                                                 (= (clojure.core/name name) 
                                                    method-name)))
                                       (filter :parameter-types)
                                       (map (fn [{:keys [parameter-types flags]}]
                                              (if (:static flags)
                                                parameter-types
                                                (cons "this" parameter-types)))))
                                 members)]
              arglists)))))))
(comment
  (arglist {:ns (the-ns 'user)
            :sym "String/.replace" })
  ,)

(defn load-file [{:keys [ source-path source code]}]
  (let [rdr (slpr/source-logging-pushback-reader code nil nil)]
    (try
      (binding [*repl* true]
        [nil (clojure.lang.Compiler/load rdr source-path source)])
      (catch Throwable e
        [e nil]))))


(defrecord EvalEvaluator [ns read-opts]
  IEvaluator
  (evaluate [_ {:keys [source-path source code] :as m}]
    (evaluate (assoc m
                     :ns ns
                     :read-opts read-opts)))
  (completions [_ m]
    (completions (assoc m :ns ns)))
  (doc [_ m]
    (doc-string (assoc m :ns ns)))
  (load-file [_ m]
    (load-file m))
  (arglist [_ m]
    (arglist (assoc m :ns ns))))

(defn eval-evaluator
  ([ns]
   (eval-evaluator ns {:read-cond :allow
                       :features #{:clj}}))
  ([ns read-opts]
   (->EvalEvaluator ns read-opts)))

