(ns com.phronemophobic.clobber.modes.clojure.evaluator.eval-evaluator
  (:refer-clojure :exclude [load-file])
  (:require [com.phronemophobic.clobber.modes.clojure.evaluator
             :as evaluator
             :refer [IEvaluator]]
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
                   :as m }]
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
      #_(let [arglists (when (seq? (:arglists mta))
                       (str/join
                        "\n"
                        (eduction
                         (map pr-str)
                         (:arglists mta))))
            docstring [sym-str
                       "\n"
                       arglists
                       "\n"
                       (when (string? doc)
                         doc)]]
        docstring)
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
        #_(let [docstring 
              (if-let [selected (seq (or (:selected-method jdoc-data)
                                         (:selected-constructor jdoc-data)))]
                (str/join
                 "\n\n"
                 (eduction
                  (map (fn [{:keys [clojure-call return-type signature description]}]
                         (str clojure-call "\n"
                              return-type " " signature "\n\n"
                              description)))
                  selected))
                
                ;; else show class javadoc)
                (str (:classname jdoc-data)
                     "\n\n"
                     (:class-description-md jdoc-data)
                     "\n\nConstructors:\n\n"
                     (str/join
                      "\n\n"
                      (eduction
                       (map (fn [{:keys [ signature description]}]
                              (str signature "\n" description)))
                       (:constructors jdoc-data)))
                     "\n\nMethods:\n\n"
                     (str/join
                      "\n\n"
                      (eduction
                       (map (fn [{:keys [return-type signature description]}]
                              (str return-type " " signature "\n" description)))
                       (:methods jdoc-data)))))]
          docstring)
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
            (let [
                  ;; base-style (:base-style editor)
                  ;; cursor-index
                  ;; (loop [index 1]
                  ;;   (if (>= index named-child-count)
                  ;;     (dec index)
                  ;;     (let [argnode (.getNamedChild parent-coll index)]
                  ;;       (if (<= cursor-byte (.getEndByte argnode))
                  ;;         (dec index)
                  ;;         (recur (inc index))))))
                  
                  method-name (let [sym-name (clojure.core/name sym)]
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
                                 members)
                  
                  ;; ps (into [(str 
                  ;;            sym
                  ;;            " ")]
                  ;;          (comp (map (fn [arglist]
                  ;;                       ["["
                  ;;                        (into []
                  ;;                              (comp
                  ;;                               (map-indexed (fn [i method-sym]
                  ;;                                              (let [s (name method-sym)]
                  ;;                                                (if (= i cursor-index)
                  ;;                                                  {:text s
                  ;;                                                   :style (assoc base-style :text-style/font-style {:font-style/weight :bold})}
                  ;;                                                  s))))
                  ;;                               (interpose " "))
                  ;;                              arglist)
                  ;;                        "]"]))
                  ;;                (interpose " "))
                  ;;          arglists)
                  ;; p (para/paragraph
                  ;;    ps
                  ;;    nil
                  ;;    {:paragraph-style/text-style (:base-style editor)})
]
              arglists
              #_(dispatch! :update $editor
                         (fn [editor]
                           (assoc-in editor [:status :status] p)))))))
      )))
(comment
  (arglist {:ns (the-ns 'user)
            :sym "String/.replace" })
  ,)

(defn load-file [{:keys [ source-path source code]}]
  (let [
        ;; eval-ns-name ns
        
        #_#_[;; path relative to class-path  
         source-path
         ;; filename
         source-name] (when-let [^File f file]
                        [(ns-sym->resource-path eval-ns-name (util/file-ext f))
                         (.getName f)])
        
        
        ;; ^Rope
        ;; rope (:rope editor)
        rdr (slpr/source-logging-pushback-reader code nil nil)
        #_(LineNumberingPushbackReader.
             (StringReader. #_(.toString rope)
                            code))]
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

