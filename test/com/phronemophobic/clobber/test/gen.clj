(ns com.phronemophobic.clobber.test.gen
  (:require
   [clojure.edn :as edn]
   [clojure.test :as t]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.spec.test.alpha :as stest]
   [clojure.test.check.properties :as prop]
   [clojure.test.check :as tc]
   [clojure.datafy :as d]
   [com.phronemophobic.clobber.modes.clojure.ui :as cui]
   [com.phronemophobic.clobber.util :as util]
   [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
   [com.phronemophobic.clobber.modes.text :as text-mode]
   )
  (:import
   (org.treesitter TSInputEncoding TSNode)
   com.ibm.icu.text.BreakIterator
   io.lacuna.bifurcan.Rope))

(defn check-cursor
  "Check that all the cursor offsets match."
  [editor]
  (let [^Rope rope (:rope editor)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        
        cursor (:cursor editor)
        cursor-char (:char cursor)
        
        calculated-cursor
        (loop [char-index 0
               row 0
               last-line-char 0]
          (let [next-char (.following bi char-index)]
            (if (or (= -1 next-char)
                    (= cursor-char char-index))
              (let [diff-string (-> rope 
                                    (.subSequence 0 char-index)
                                    .toString)]
                {:byte (-> diff-string .getBytes alength)
                 :char char-index
                 :point (util/num-points diff-string)
                 :row row
                 :column-byte (-> (.subSequence rope last-line-char char-index)
                                  .toString
                                  .getBytes
                                  alength)})
              (if (= \newline (.charAt rope char-index))
                (recur next-char
                       (inc row)
                       next-char)
                (recur next-char
                       row
                       last-line-char)))))]
    (when (not= calculated-cursor
                (select-keys cursor [:byte :char :point :row
                                     :column-byte]))
      (tap> {:editor-cursor (:cursor editor)
            :calculated-cursor calculated-cursor})
      (throw (ex-info "Mismatched cursor"
                      {:editor editor
                       :calculated-cursor calculated-cursor})))
    true))

(def ^:private tree-data-xform
  (comp (map d/datafy)
        (map #(select-keys % [:start-point :end-point :type :end-byte :error? :start-byte]))))
(defn ^:private tree->data [t]
  (into [] tree-data-xform (util/tree-reducible t)))

(defn ^:private tree-equals?
  "Checks if the tree sitter tree's are mostly equal. 

  Does not check for node contents.
  ie. \"(1)\" and \"(2)\" will be treated as equal since they have the same node types
  in the same positions."
  [t1 t2]
  (= (tree->data t1)
     (tree->data t2)))


(defn check-parse
  "Check that current parse matches a fresh parse.

  Many editor operations do incremental parses. Make sure the
  incremental parses are the same as if the parse was done from scratch."
  [editor]
  (let [fresh-tree (let [parser (:parser editor)

                         reader (util/->RopeReader (:rope editor))
                         buf (:buf editor)
                         tree (.parse parser buf nil reader TSInputEncoding/TSInputEncodingUTF8)]
                     tree)
        et (tree->data (:tree editor))
        ct (tree->data fresh-tree)]
    (when (not= et ct)
      (tap> {:editor-tree et
             :calculated-tree ct})
      (println (.getRootNode (:tree editor))
               "\n"
               (.getRootNode fresh-tree))
      (throw (ex-info "Mismatched Parse Tree"
                      {:editor editor
                       :calculated-tree ct})))
    true))


(defn apply-ops [editor op-list]
  (reduce (fn [editor op]
            (let [f (if (vector? op)
                      (let [[f & args] op]
                        #(apply f % args))
                      op)]
              (let [new-editor (cui/editor-upkeep editor f)]
                ;; we should eventually check to make sure
                ;; the editor works in the presence of
                ;; invalid parses
                (when (util/first-by (keep (fn [^TSNode node]
                                             (when (or (.isMissing node)
                                                       (.hasError node))
                                               true)))
                                     (util/tree-reducible (:tree new-editor)))
                  (throw (ex-info "Missing or error tree"
                                  {:type ::error})))
                new-editor)))
          editor
          op-list))

(defmacro ignore-bad-parses [& body]
  `(try
     ~@body
    (catch Exception e#
      (if (= ::error (-> e# ex-data :type))
        true
        (throw e#)))))

(defn check-cursor-prop [ops]
  (prop/for-all [op-list ops]
                (let [editor (cui/make-editor)]
                  (ignore-bad-parses
                   (check-cursor (apply-ops editor op-list))))))


(defn check-parse-prop [ops]
  (prop/for-all [op-list ops]
                (let [editor (cui/make-editor)]
                  (ignore-bad-parses
                    (check-parse (apply-ops editor op-list))))))

(def
  testspec
  (s/spec
   (s/or :static (disj (set (vals clojure-mode/key-bindings))
                       ;; These can produce invalid parse trees
                       ;; investigate those edge cases later
                       #'com.phronemophobic.clobber.modes.text/editor-delete-char
                       #'com.phronemophobic.clobber.modes.text/editor-delete-backward-char)
         :dynamic
         (s/tuple 
          #{#'text-mode/editor-self-insert-command}
          #{"a" "👩‍👩‍👦‍👦" "\n" " "}))))



(comment
  
  (let [aresults (tc/quick-check 
                  10000
                  (check-cursor-prop (s/gen
                                      (s/coll-of testspec
                                                 :into []
                                                 :max-count 50))))]
    (def results aresults)
    (clojure.pprint/pprint
     (-> aresults
         :shrunk
         :smallest))
    (tap> results)
    nil)

  (let [aresults (tc/quick-check 
                  10000
                  (check-parse-prop (s/gen
                                    (s/coll-of testspec
                                               :into []
                                               :max-count 50))))]
    (def results aresults)
    (clojure.pprint/pprint
     
     (-> aresults
         :shrunk
         :smallest))
    (tap> results)
    nil)
  
  ,)


(comment
  (tap> results)

  (check-cursor
   (apply-ops (-> (cui/make-editor)) 
              (-> results :shrunk :smallest first)))

  (check-parse
   (apply-ops (-> (cui/make-editor)) 
              (-> results :shrunk :smallest first)))
  
  (apply-ops (cui/make-editor) 
             (-> results :shrunk :smallest first))


  (tap> (apply-ops (cui/make-editor)
                   (-> results :shrunk :smallest first)))


  (-> (apply-ops (cui/make-editor) 
                 (-> results :shrunk :smallest first))
      :tree
      .getRootNode
      str
      println)


  (check-parse
   (apply-ops (cui/make-editor) bad))

  (check-cursor
   (apply-ops (cui/make-editor) bad))


  

  ,)

