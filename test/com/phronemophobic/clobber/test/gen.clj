(ns com.phronemophobic.clobber.test.gen
  (:require
   [clojure.edn :as edn]
   [clojure.test :as t]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.spec.test.alpha :as stest]
   [clojure.test.check.properties :as prop]
   [clojure.test.check :as tc]
   [com.phronemophobic.clobber.modes.clojure.ui :as cui]
   [com.phronemophobic.clobber.util :as util]
   [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
   [com.phronemophobic.clobber.modes.text :as text-mode]
   )
  (:import
   com.ibm.icu.text.BreakIterator
   io.lacuna.bifurcan.Rope))

(defn check-cursor [editor]
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


(defn apply-ops [editor op-list]
  (reduce (fn [editor op]
            (let [f (if (vector? op)
                      (let [[f & args] op]
                        #(apply f % args))
                      op)]
              (cui/editor-upkeep editor f)))
          editor
          op-list))

(defn check-cursor-prop [ops]
  (prop/for-all [op-list ops]
                (let [editor (-> (cui/make-editor)
                                 (text-mode/editor-set-string ""))]
                  (let [fut (future
                              (check-cursor (apply-ops editor op-list)))]
                    (deref fut 100 false)))))

(def
  testspec
  (s/spec
   (s/or :static (disj (set (vals clojure-mode/key-bindings))
                       #'clojure-mode/editor-indent-region)
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
         :smallest)))
  
  ,)

(def bad
  [#'com.phronemophobic.clobber.modes.clojure/editor-paredit-open-square
   #'com.phronemophobic.clobber.modes.clojure/editor-paredit-open-curly
   #'com.phronemophobic.clobber.modes.clojure/editor-paredit-close-square
   #'com.phronemophobic.clobber.modes.text/editor-delete-char
   #'com.phronemophobic.clobber.modes.text/editor-move-beginning-of-line
   #'com.phronemophobic.clobber.modes.clojure/editor-paredit-wrap-round
   #'com.phronemophobic.clobber.modes.clojure/editor-paredit-close-square])


(comment
  (tap> results)

  (apply-ops (cui/make-editor) 
             (-> results :shrunk :smallest first))


  (tap> (apply-ops (cui/make-editor)
                   (-> results :shrunk :smallest first)))

  (check-cursor
   (apply-ops (-> (cui/make-editor)) 
              (-> results :shrunk :smallest first)))


  (check-cursor
   (apply-ops (-> (cui/make-editor)
                  (text-mode/editor-set-string "")) 
              bad))

  ,)




