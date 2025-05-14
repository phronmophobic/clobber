(ns com.phronemophobic.clobber.modes.clojure.ui
  (:require [clojure.java.io :as io]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [com.phronemophobic.membrandt :as ant]
            [ropes.core :as ropes]
            [com.phronemophobic.viscous :as viscous]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util :as util])
  (:import (org.treesitter TSLanguage
                           TSQuery
                           TSParser
                           TSTree
                           TSTreeCursor
                           TSNode
                           TSPoint
                           TSReader
                           TSQueryCapture
                           TSQueryCursor
                           TSQueryMatch
                           TreeSitterClojure
                           TreeSitterJson
                           TSInputEdit
                           TSInputEncoding)
           java.nio.charset.Charset
           java.util.Arrays
           java.nio.ByteBuffer
           java.util.regex.Pattern
           com.ibm.icu.text.BreakIterator
           ))

#_(defonce recompile (virgil/compile-java ["/Users/adrian/workspace/bifurcan/src"]))
(import 'io.lacuna.bifurcan.Rope)

(defeffect ::update-editor [{:keys [$editor op]}]
  (dispatch! :update $editor op))

(defeffect ::editor-eval-top-form [{:keys [editor $editor]}]
  (future
    (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
          
          {cursor-byte :byte
           cursor-char :char
           cursor-point :point
           cursor-row :row
           cursor-column :column} cursor]
      (let [root-node (.getRootNode tree)
            cursor (TSTreeCursor. root-node)
            idx (.gotoFirstChildForByte cursor cursor-byte)]
        (if (= -1 idx)
          editor
          (try
            (let [node (.currentNode cursor)
                  line-number (-> node
                                  .getEndPoint
                                  .getRow)]
              (binding [*ns* (:eval-ns editor)]
                (let [form (read-string (util/node->str rope node))
                      val (eval form)]
                  (dispatch! :update $editor
                             update :line-val
                             (fn [m]
                               (let [line-val (get m rope)]
                                 {rope (assoc line-val line-number (viscous/wrap val))}))))))
            (catch Exception e
              (prn e))))))))


(def special-keys {"DEL" :backspace
                   "RET" :enter
                   "<right>" :right
                   "<left>" :left
                   "<up>" :up
                   "<down>" :down})

(defn key-chord->map [s]
  (loop [cs (seq s)
         chord []
         press nil]
    (if cs
      (let [c (first cs)]
        (case c
          \C
          (if (= \- (second cs))
            (recur (nnext cs)
                   chord
                   (assoc press :ctrl? true))
            (recur (next cs)
                   chord
                   (assoc press :key c)))

          \M
          (if (= \- (second cs))
            (recur (nnext cs)
                   chord
                   (assoc press :meta? true))
            (recur (next cs)
                   chord
                   (assoc press :key c)))

          \space
          (recur (next cs)
                 (conj chord press)
                 nil)


          ;; else
          (if-let [[s key] (some (fn [[s key]]
                                   (when (= (seq s)
                                            (take (count s) cs))
                                     [s key]))
                                 special-keys)]
            (recur (seq (drop (count s) cs))
                   chord
                   (assoc press :key key))
            (recur (next cs) chord (assoc press :key c)))))
      (if press
        (conj chord press)
        chord))))

(defn key-bindings->keytree [key-bindings]
  (reduce
   (fn [m [chord-str var]]
     (let [chord (key-chord->map chord-str)]
       (assoc-in m chord var)))
   {}
   key-bindings))

(defn my-save [editor]
  (-> editor
      (text-mode/editor-self-insert-command "save!")))



(defn make-editor
  ([]
   (make-editor (TreeSitterClojure.)))
  ([lang]
   (#_map->Editor
    identity
    {:tree nil
     ;; all offsets in terms of bytes
     ;; :offsets {:lines nil
     ;;           :graphemes nil}
     :cursor {:byte 0
              :char 0
              :point 0
              :row 0
              :column 0}
     :viewport {:start-line 0
                :num-lines 40}
     :paragraph nil
     :base-style #:text-style {:font-families ["Menlo"]
                               :font-size 12}
     :eval-ns *ns*
     :rope Rope/EMPTY
     :language lang
     :viscous? true
     :parser (doto (TSParser.)
               (.setLanguage lang))
     :buf (byte-array 4096)})))

;; Tree sitter uses these strings
;; and requires they not be garbage collected
;; and they do not retain a strong reference.
;; tldr: make sure these queries don't get garbage collected
;;       or redefined
#_(defonce lang->highlight-queries
    {clojure-lang highlight-queries
     json-lang json-highlight-queries})

(def base-style #:text-style {:font-families ["Menlo"]
                              :font-size 12})


(defn ^:private add-node-to-paragraph [rope p offset end-byte-offset ^TSNode node base-style style]
  (let [;; add any unmatched text as unadorned
        start-byte (min end-byte-offset (.getStartByte node))
        end-byte (min end-byte-offset (.getEndByte node))

        p (if (> start-byte offset)
            (conj p
                  (util/rope->str rope offset start-byte))
            p)

        ;; add matched text
        ;; ; matches can overlap
        p (if (and (> end-byte offset)
                   (> end-byte start-byte))
            (let [color (get clojure-mode/text-colors style)
                  chunk-text (util/rope->str rope (max start-byte offset) end-byte)
                  chunk (if color
                          {:text chunk-text
                           :style (assoc base-style
                                         :text-style/color color)}
                          chunk-text)
                  p (conj p chunk)]
              p)
            ;; else already covered by previous overlapping match
            p)]
    [p (max offset end-byte)]))


(def builtin?
  #{"def" "defn" "defui" "for" "do" "doseq" "let" "recur" "if" "when" "loop" "and" "or" "doto" "defrecord" "extend-protocol" "defonce" "defprotocol" "defmulti" "defmethod" "ns" "import" "require"} )
(defn highlighted-text [
                        ;;lang highlight-queries
                        ^TSQueryCursor qc
                        ^TSQuery query
                        base-style ^TSTree tree
                        ^Rope
                        rope
                        start-byte-offset
                        end-byte-offset]
  (let [_ (.setByteRange qc start-byte-offset end-byte-offset)
        _ (.exec qc query (.getRootNode tree))
        matches (.getCaptures qc)
        paragraph (loop [p []
                         offset start-byte-offset]
                    (if (.hasNext matches)
                      (let [match (.next matches)
                            ^TSQueryCapture
                            capture (aget (.getCaptures match) (.getCaptureIndex match))
                            capture-name (.getCaptureNameForId query (.getIndex capture))
                            node (.getNode capture)]
                        (if (= capture-name "list")
                          ;; (recur p offset)
                          (let [ ;; check if the first child is def or defn
                                first-child (.getNamedChild node 0)]
                            (if (and (not (.isNull first-child))
                                     (= "sym_lit" (.getType first-child)))
                              (let [s (util/node->str rope first-child)]
                                (if (builtin? s)
                                  (let [[p end-byte] (add-node-to-paragraph rope p offset end-byte-offset first-child base-style "defn")]
                                    (recur p end-byte))
                                  (recur p offset)))
                              ;; else
                              (recur p offset)))
                          ;; else
                          (let [

                                ;; add any unmatched text as unadorned
                                start-byte (min end-byte-offset (.getStartByte node))
                                end-byte (min end-byte-offset (.getEndByte node))

                                p (if (> start-byte offset)
                                    (conj p
                                          (util/rope->str rope offset start-byte))
                                    p)

                                ;; add matched text
                                ;; ; matches can overlap
                                p (if (> end-byte offset)
                                    (let [color (get clojure-mode/text-colors capture-name)
                                          chunk-text (util/rope->str rope (max start-byte offset) end-byte)
                                          chunk (if color
                                                  {:text chunk-text
                                                   :style (assoc base-style
                                                                 :text-style/color color)}
                                                  chunk-text)
                                          p (conj p chunk)]
                                      p)
                                    ;; else already covered by previous overlapping match
                                    p)]
                            (recur p
                                   end-byte
                                   ))))
                      ;; else
                      (let [p (if (< offset end-byte-offset)
                                (conj p
                                      (util/rope->str rope offset end-byte-offset))
                                p)]
                        p)))
        ]
    paragraph))



(defn cursor-view [^Rope rope para cursor]
  (let [cursor-char (- (:char cursor)
                       (get para :char-offset 0))
        rope (.sliceBytes rope
                          (:start-byte-offset para)
                          (:end-byte-offset para))
        {:keys [x y width height] :as rect}
        (cond

          (zero? (.size rope))
          (first
           (para/get-rects-for-range (assoc para :paragraph " ")
                                     0 1
                                     :max
                                     :tight))

          (>= cursor-char (.length rope))
          (first
           (para/get-rects-for-range (assoc para :paragraph [(:paragraph para)
                                                             " "])
                                     cursor-char (inc cursor-char)
                                     :max
                                     :tight))

          :else
          (let [
                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))

                next-char (.following bi cursor-char)
                diff-string (-> (.subSequence rope cursor-char next-char)
                                .toString)]
            (first
             (para/get-rects-for-range para cursor-char (+ cursor-char (.length diff-string))
                                       :max
                                       :tight))))
        ]
    (if (not rect)
      (println "no cursor rect!")
      (let [width (if (zero? width)
                    (-> (para/get-rects-for-range (assoc para :paragraph " ")
                                                  0 1
                                                  :max
                                                  :tight)
                        first
                        :width)
                    width)]
        (ui/translate x y
                      (ui/filled-rectangle
                       [0.5725490196078431
                        0.5725490196078431
                        0.5725490196078431
                        0.4]
                       width height))))))

(defn editor->paragraph [editor]
  (let [tree (:tree editor)
        lang (:language editor)
        ^Rope
        rope (:rope editor)

        {:keys [start-line num-lines]} (:viewport editor)
        end-line (+ start-line num-lines)
        
        start-byte-offset (util/find-byte-offset-for-line tree rope start-line)
        end-byte-offset (util/find-byte-offset-for-line tree rope end-line)
        char-offset (-> (.sliceBytes rope 0 start-byte-offset)
                        .toCharSequence
                        .length)
        para (para/paragraph (highlighted-text (TSQueryCursor.)
                                               (TSQuery. lang
                                                         clojure-mode/highlight-queries)
                                               (:base-style editor)
                                               tree
                                               (:rope editor)
                                               start-byte-offset
                                               end-byte-offset)
                             nil
                             {:paragraph-style/text-style (:base-style editor)})
        para (assoc para
                    :char-offset char-offset
                    :start-byte-offset start-byte-offset
                    :end-byte-offset end-byte-offset)]
    para))


(defui line-val-view [{:keys [line-val editor para]}]
  (let [{:keys [tree rope base-style viscous?]} editor]
    (into []
          (map (fn [[line val]]
                 (let [byte-start-index (util/find-byte-offset-for-line tree rope line)
                       byte-end-index (util/find-byte-offset-for-line tree rope (inc line))
                       char-offset (get para :char-offset 0)
                       rects (para/get-rects-for-range para
                                                       (- (util/byte-index->char-index rope byte-start-index)
                                                          char-offset)
                                                       (- (util/byte-index->char-index rope byte-end-index)
                                                          char-offset)
                                                       :max
                                                       :tight)]
                   (when (seq rects)
                     (let [x (transduce (map (fn [{:keys [x width] :as r}]
                                               (+ x width)))
                                        max
                                        0
                                        rects)
                           y (transduce (map :y) max (-> rects first :y) (rest rects))

                           offset 4]
                       (if viscous?
                         (ui/translate (- (+ x 10) offset) (- y offset)
                                       (let [inspector-extra (get extra [::inspector [line val]])]
                                         (ui/vertical-layout
                                          (viscous/inspector
                                           {:obj val
                                            :width (get inspector-extra :width 40)
                                            :height (get inspector-extra :height 1)
                                            :show-context? (get inspector-extra :show-context?)
                                            :extra inspector-extra}))))
                         (ui/translate (+ x 10) y 
                                       (-> (make-editor)
                                           (assoc :base-style base-style)
                                           (text-mode/editor-self-insert-command
                                            "=> ")
                                           (text-mode/editor-self-insert-command
                                            (pr-str @val))
                                           (editor->paragraph)))))))))
          line-val)))


(defui editor-view [{:keys [editor]}]
  (when-let [tree (:tree editor)]
    (let [lang (:language editor)
          rope (:rope editor)
          para (editor->paragraph editor)
          line-vals (when-let [line-val (if (:instarepl? editor)
                                          (-> editor :line-val first second)
                                          (-> editor :line-val (get rope)))]
                      (line-val-view
                       {:editor editor
                        :para para
                        :line-val line-val}))]
      [(cursor-view rope para (:cursor editor))
       para
       line-vals])))


(defn init-search-forward [editor]
  (assoc editor
         ::search {:initial-cursor (:cursor editor)
                   :initial-rope (:rope editor)}))

#_(defeffect ::init-search-forward [{:keys [$editor]}]
    (dispatch! :update $editor
               (fn [editor]
                 (assoc editor
                        ::search {:initial-cursor (:cursor editor)
                                  :initial-rope (:rope editor)}))))

(defn cancel-search-forward [editor]
  (let [initial-cursor (-> editor ::search :initial-cursor)
        editor (if initial-cursor
                 (assoc editor :cursor initial-cursor)
                 editor)]
    (text-mode/editor-update-viewport
     (dissoc editor ::search))))

(defeffect ::cancel-search-forward [{:keys [$editor]}]
  (dispatch! :update $editor
             (fn [editor]
               (let [initial-cursor (-> editor ::search :initial-cursor)
                     editor (if initial-cursor
                              (assoc editor :cursor initial-cursor)
                              editor)]
                 (text-mode/editor-update-viewport
                  (dissoc editor ::search)))
               )))

(defn finish-search-forward [editor]
  (dissoc editor ::search))

(def clojure-keytree
  (key-bindings->keytree
   (assoc clojure-mode/key-bindings
          "C-x C-s" #'my-save
          "C-M-x" ::editor-eval-top-form
          "C-s" #'init-search-forward)))

(defeffect ::finish-search-forward [{:keys [$editor]}]
  (dispatch! :update $editor
             (fn [editor]
               (dissoc editor ::search))))

(defn editor-search-forward [editor query]
  (let [search-state (::search editor)
        _ (assert search-state)
        
        search-cursor (or (:cursor search-state)
                          (:initial-cursor search-state))
        search-index (:char search-cursor)
        regexp (Pattern/compile query
                                (bit-or
                                 Pattern/CASE_INSENSITIVE
                                 Pattern/LITERAL))
        ^Rope
        rope (:rope editor)
        matcher (.matcher regexp rope)

        match (if (.find matcher search-index)
                (.start matcher)
                (when (.find matcher 0)
                  (.start matcher)))]
    (if match
      (let [ ;; calculate new cursor
            s (-> (.subSequence rope 0 match)
                  .toString)

            {:keys [row column]} (util/count-points s)

            cursor {:byte (alength (.getBytes s "utf-8"))
                    :char (.length s)
                    :point (util/num-points s)
                    :row row
                    :column column}]
        (-> editor
            (assoc-in [::search :query] query)
            (assoc :cursor cursor)
            (text-mode/editor-update-viewport)))
      (assoc-in editor
                [::search :query] query))))

(defn editor-repeat-search-forward [editor]
  (let [search-state (::search editor)
        _ (assert search-state)

        query (:query search-state)
        
        search-cursor (:cursor editor)
        search-index (:char search-cursor)
        regexp (Pattern/compile query
                                (bit-or
                                 Pattern/CASE_INSENSITIVE
                                 Pattern/LITERAL))
        ^Rope
        rope (:rope editor)
        matcher (.matcher regexp rope)

        match (if (.find matcher search-index)
                (.start matcher)
                (when (.find matcher 0)
                  (.start matcher)))
        ;; now do it again
        match (when match
                (if (.find matcher)
                  (.start matcher)
                  (when (.find matcher 0)
                    (.start matcher))))]
    (if match
      (let [ ;; calculate new cursor
            s (-> (.subSequence rope 0 match)
                  .toString)

            {:keys [row column]} (util/count-points s)

            cursor {:byte (alength (.getBytes s "utf-8"))
                    :char (.length s)
                    :point (util/num-points s)
                    :row row
                    :column column}]
        (-> editor
            (assoc-in [::search :query] query)
            (assoc :cursor cursor)
            (text-mode/editor-update-viewport)))
      (assoc-in editor
                [::search :query] query))))

(defeffect ::append-search-forward [{:keys [$editor s]}]
  (dispatch! :update $editor
             (fn [editor]
               (let [query (str (-> editor
                                    ::search
                                    :query)
                                s)]
                 (editor-search-forward editor query)))))

(defeffect ::repeat-search-forward [{:keys [$editor s]}]
  (dispatch! :update $editor
             (fn [editor]
               (editor-repeat-search-forward editor))))

(defui wrap-search [{:keys [editor body]
                     :as this}]
  (let [search-state (::search editor)
        body (if search-state
               (ui/on
                :key-event
                (fn [key scancode action mods]
                  (when (#{:press :repeat} action)
                    (let [ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
                      (cond 

                        (and ctrl?
                             (= (char key) \G))
                        [[::cancel-search-forward this]]

                        (and ctrl?
                             (= (char key) \S))
                        [[::repeat-search-forward this]]))))
                :key-press
                (fn [s]
                  (cond

                    (= s :enter)
                    [[::finish-search-forward this]]

                    (string? s)
                    [[::append-search-forward (assoc this :s s)]]))
                (ui/vertical-layout
                 body
                 (ui/label (:query search-state))))

               body
               #_(ui/wrap-on
                  :key-event
                  (fn [handler key scancode action mods]
                    (when (#{:press :repeat} action)
                      (let [alt? (not (zero? (bit-and ui/ALT-MASK mods)))
                            super? (not (zero? (bit-and ui/SUPER-MASK mods)))
                            shift? (not (zero? (bit-and ui/SHIFT-MASK mods)))
                            ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))

                            search? (and ctrl?
                                         (= (char key) \S))]
                        (if search?
                          [[::init-search-forward this]]
                          (handler key scancode action mods)))))
                  body))]
    body))

(defeffect ::update-instarepl [{:keys [editor $editor]}]
  (future
    (try
      (let [ ;; insta-state (::insta editor)
            line-val (:line-val editor)
            rope (:rope editor)]
        
        (when (not= rope
                    (-> line-val first first))
          (let [^TSTree
                tree (:tree editor)
                root-node (.getRootNode tree)
                cursor (TSTreeCursor. root-node)
                
                line-vals (when (.gotoFirstChild cursor)
                            (binding [*ns* (:eval-ns editor)]
                              (loop [bindings {}
                                     line-vals {}]
                                (let [node (.currentNode cursor)
                                      s (util/node->str rope node)
                                      form (read-string s)
                                      [binding-sym form] (if (list? form)
                                                           (case (first form)

                                                             def [(second form) (nth form 2)]
                                                             defn [(second form) `(fn ~(second form) ~@(nthrest form 2))]
                                                             ;; else
                                                             [nil form])
                                                           [nil form])
                                      form `(fn [~@(keys bindings)]
                                              ~form)
                                      [success result]
                                      (try
                                        (let [f (eval form)
                                              result (apply f (vals bindings))]
                                          [true result])
                                        (catch Exception e
                                          [false e]))

                                      bindings (if binding-sym
                                                 (assoc bindings binding-sym result)
                                                 bindings)

                                      line (-> node
                                               .getEndPoint
                                               .getRow)
                                      line-vals (assoc line-vals line (viscous/wrap result))]
                                  (if (not success)
                                    line-vals
                                    (if (.gotoNextSibling cursor)
                                      (recur bindings line-vals)
                                      line-vals))))))]
            (dispatch! :update $editor
                       (fn [editor]
                         (assoc editor :line-val {rope line-vals}))))))
      (catch Exception e
        #_(prn e)))))

(defui wrap-instarepl [{:keys [editor body]
                        :as this}]
  (if (:instarepl? editor)
    (let [body (ui/wrap-on
                :key-event
                (fn [handler key scancode action mods]
                  (cons
                   [::update-instarepl this]
                   (handler key scancode action mods)))
                :key-press
                (fn [handler s]
                  (cons
                   [::update-instarepl this]
                   (handler s)))
                body)]
      body)
    body))

(def uppercase {\, \<
                \. \>
                \/ \?
                \; \:
                \' \"
                \[ \{
                \] \}
                \\ \|
                \1 \!
                \2 \@
                \3 \#
                \4 \$
                \5 \%
                \6 \^
                \7 \&
                \8 \*
                \9 \(
                \0 \)
                \- \_
                \= \+
                \` \~})

(defui clojure-keymap [{:keys [bindings body editor]}]
  (let [next-bindings (get extra ::next-bindings)
        find-match
        (fn [key-press]
          (if-let [match (get (or next-bindings bindings) key-press)]
            (if (map? match)
              [[:set $next-bindings match]]
              [[:set $next-bindings nil]
               (if (keyword? match)
                 [match {:editor editor
                         :$editor $editor}]
                 [::update-editor {:op match
                                   :editor editor
                                   :$editor $editor}])])
            ;; no match
            (when next-bindings
              [[:set $next-bindings nil]])))]
    (ui/on
     :key-press
     (fn [s]
       (let [intents (find-match {:key (if (string? s)
                                         (first s)
                                         s)})]
         (if (seq intents)
           intents
           (when (and (not next-bindings)
                      (string? s)
                      (-> (.getBytes ^String s)
                          first
                          pos?))
             [[::update-editor {:op #(text-mode/editor-self-insert-command % s)
                                :$editor $editor}]]))))
     :key-event
     (fn [key scancode action mods]
       (when (#{:press :repeat} action)
         (let [alt? (not (zero? (bit-and ui/ALT-MASK mods)))
               super? (not (zero? (bit-and ui/SUPER-MASK mods)))
               shift? (not (zero? (bit-and ui/SHIFT-MASK mods)))
               ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))

               key-press (cond-> {:key (if shift?
                                         (let [c (char key)]
                                           (get uppercase c))
                                         (Character/toLowerCase (char key)))}
                           alt? (assoc :meta? true)
                           super? (assoc :super? true)
                           ctrl? (assoc :ctrl? true))]
           (when (or alt?
                     super?
                     ctrl?)
             (find-match key-press)))))
     body)))

(defui code-editor [{:keys [editor
                            ^:membrane.component/contextual
                            focus]
                     :as this}]
  (let [body (editor-view {:editor editor})

        structure-state (get editor ::structure-state)
        body (ui/wrap-on
              :mouse-down
              (fn [handler [mx my :as mpos]]
                (cons
                 [::do-structure-click {:editor editor
                                        :x mx
                                        :y my
                                        :$structure-state $structure-state}]
                 (handler mpos)))
              :mouse-move
              (fn [handler [mx my]]
                (let [intents (handler [mx my])]
                  (if (seq intents)
                    intents
                    [[::do-structure {:editor editor
                                      :x mx
                                      :y my
                                      :$structure-state $structure-state}]])))
              body)

        focused? (= $editor focus)
        body (if focused?
               (clojure-keymap {:bindings clojure-keytree
                                :editor editor
                                :body body})
               #_(ui/on
                  :key-event
                  (fn [key scancode action mods]
                    #_(when (#{:press :repeat} action)
                        (let [alt? (not (zero? (bit-and ui/ALT-MASK mods)))
                              super? (not (zero? (bit-and ui/SUPER-MASK mods)))
                              shift? (not (zero? (bit-and ui/SHIFT-MASK mods)))
                              ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
                          (cond

                            (and ctrl?
                                 alt?)
                            (case (char key)
                              \X
                              [[::editor-eval-top-form this]]

                              \F
                              [[:update $editor #(editor-paredit-forward %)]]

                              \B
                              [[:update $editor #(editor-paredit-backward %)]]


                              nil)

                            ctrl?
                            (case (char key)
                              \A
                              [[:update $editor #(editor-move-beginning-of-line %)]]

                              \D
                              [[:update $editor #(editor-paredit-forward-delete %)]]

                              \E
                              [[:update $editor #(editor-move-end-of-line %)]]
                              
                              \F
                              [[:update $editor #(editor-forward-char %)]]

                              \B
                              [[:update $editor #(editor-backward-char %)]]

                              \J
                              [[:update $editor #(-> %
                                                     (editor-self-insert-command "\n")
                                                     (editor-indent))]]

                              \K
                              [[:update $editor #(editor-paredit-kill %)]]

                              \L
                              [[:update $editor #(editor-recenter-top-bottom %)]]

                              \N
                              [[:update $editor #(editor-next-line %)]]

                              \O
                              [[:update $editor #(editor-open-line %)]]

                              \P
                              [[:update $editor #(editor-previous-line %)]]

                              \V
                              [[:update $editor #(editor-scroll-down %)]]

                              ;; else
                              nil)

                            alt?
                            (case (char key)

                              \B
                              [[:update $editor #(editor-backward-word %)]]
                              
                              \F
                              [[:update $editor #(editor-forward-word %)]]

                              \V
                              [[:update $editor #(editor-scroll-up %)]]

                              
                              \,
                              (when shift?
                                [[:update $editor #(editor-beginning-of-buffer %)]])

                              \.
                              (when shift?
                                [[:update $editor #(editor-end-of-buffer %)]])

                              \9
                              (when shift?
                                [[:update $editor #(editor-paredit-wrap-round %)]])

                              \S
                              [[:update $editor #(editor-paredit-splice-sexp %)]]

                              ;; else
                              nil)

                            :else
                            nil
                            #_(case (char key)

                                
                                (case key
                                  (39 262) ;; right
                                  [[:update $editor editor-forward-char ]]

                                  #_left (37 263)
                                  [[:update $editor editor-backward-char ]]

                                  
                                  
                                  ;; (40 264)
                                  ;; ;; down

                                  ;; ;; up
                                  ;; (38 265)
                                  
                                  ;; else
                                  nil
                                  #_[[::tap key (char key)]]))))))
                  :key-press
                  (fn [s])
                  #_(fn [s]
                      ;; (tap> s)

                      (case s
                        "\""
                        [[:update $editor #(editor-paredit-doublequote %)]]
                        "["
                        [[:update $editor #(editor-paredit-open-square %)]]
                        "]"
                        [[:update $editor #(editor-paredit-close-square %)]]
                        "{"
                        [[:update $editor #(editor-paredit-open-curly %)]]
                        "}"
                        [[:update $editor #(editor-paredit-close-curly %)]]
                        "("
                        [[:update $editor #(editor-paredit-open-round %)]]
                        ")"
                        [[:update $editor #(editor-paredit-close-round %)]]

                        (cond
                          

                          (and (string? s)
                               (-> (.getBytes ^String s)
                                   first
                                   pos?))
                          [[:update $editor #(editor-self-insert-command % s)]]

                          (= :backspace s)
                          [[:update $editor editor-delete-backward-char]]

                          (= :enter s)
                          [[:update $editor #(-> %
                                                 (editor-self-insert-command "\n")
                                                 (editor-indent))]]

                          (= :right s)
                          [[:update $editor #(editor-forward-char %)]]

                          (= :up s)
                          [[:update $editor #(editor-previous-line %)]]

                          (= :down s)
                          [[:update $editor #(editor-next-line %)]]

                          (= :tab s)
                          [[:update $editor editor-indent]]
                          
                          (= :left s)
                          [[:update $editor #(editor-backward-char %)]]

                          :else
                          (do #_(tap> s)
                              nil)))
                      )
                  body)
               (let [[w h] (ui/bounds body)
                     gray 0.98]
                 [(ui/filled-rectangle [gray gray gray]
                                       (max 800 w) (max 100 h))
                  body]))

        body (if focused?
               (wrap-search
                {:editor editor
                 :$body nil
                 :body body})
               body)

        body (if focused?
               (wrap-instarepl
                {:editor editor
                 :$body nil
                 :body body})
               body)
        
        body (ui/wrap-on
              :mouse-down
              (fn [handler mpos]
                (let [intents (handler mpos)]
                  (cons [:set $focus $editor]
                        intents)))
              body)

        ]
    (ui/vertical-layout
     (ui/label (pr-str (:cursor editor)))
     (when (:structure? editor)
       [(ui/spacer 100 200)
        (ui/vertical-layout
         (ant/button {:text "make-fn"
                      :on-click (fn []
                                  [[::make-fn
                                    {:$editor $editor
                                     :editor editor
                                     :structure-state structure-state}]])})
         (when-let [args (:args structure-state)]
           (ui/on
            :mouse-down
            (fn [_]
              [[:update $structure-state assoc :args #{}]])
            (ui/label (:args structure-state))))         
         (ui/label (:node-str structure-state)))])
     body)))

(def var-ast
  {:op :var,
   :assignable? false,
   ;; :var #'com.phronemophobic.membrane.spreadsheet/a,
   :o-tag java.lang.Object})
(defn free-variables [form]
  (let [free (volatile! #{})]
    (ana.jvm/analyze
     form
     (ana.jvm/empty-env)
     {:passes-opts
      (assoc ana.jvm/default-passes-opts
             :validate/unresolvable-symbol-handler
             (fn [_ sym _]
               (vswap! free conj sym)
               (assoc var-ast :form sym)))})
    @free))

(defeffect ::make-fn [{:keys [$editor editor structure-state]}]
  (future
    (try
      (let [^TSTree
            tree (:tree editor)
            rope (:rope editor)
            root-node (.getRootNode tree)
            cursor (TSTreeCursor. root-node)

            cursor-byte (-> editor :cursor :byte)
            

            ^TSNode
            result-node (util/named-child-for-byte tree cursor-byte)
            result-str (util/node->str rope result-node )
            result-form (binding [*ns* (:eval-ns editor)]
                          (read-string result-str))
            
            forms (when (.gotoFirstChild cursor)
                    (binding [*ns* (:eval-ns editor)]
                      (loop [forms []]
                        (let [node (.currentNode cursor)]
                          (if (< (.getEndByte node)
                                 cursor-byte)
                            (let [node-str (util/node->str rope node)
                                  form (read-string node-str)
                                  def? (and (list? form)
                                            (= 'def (first form))
                                            (= 3 (count form)))
                                  forms (if def?
                                          (conj forms [(second form)
                                                       (nth form 2)])
                                          forms)]
                              (if (.gotoNextSibling cursor)
                                (recur forms)
                                forms))
                            forms)))))

            args (:args structure-state)

            bindings (loop [forms (-> forms reverse seq)
                            deps (free-variables result-form)
                            bindings []]
                       (if forms
                         (let [[name form] (first forms)
                               [bindings deps]
                               (if (and (contains? deps name)
                                        (not (contains? args name)))
                                 (let [deps (into deps(free-variables form))
                                       bindings (conj bindings
                                                      [name form])]
                                   [bindings deps])
                                 [bindings deps])]

                           (recur (next forms)
                                  deps
                                  bindings))
                         bindings))
            fn-form `(~'defn ~'foo [~@args]
                      (~'let ~(into []
                                    cat
                                    (reverse bindings))
                       ~result-form))

            ]

        (dispatch! :update $editor
                   (fn [editor]
                     (-> editor
                         text-mode/editor-move-end-of-line
                         (text-mode/editor-self-insert-command "\n")
                         (clojure-mode/editor-indent)
                         (text-mode/editor-self-insert-command
                          (with-out-str
                            (clojure.pprint/pprint fn-form)))))))
      (catch Exception e
        (prn e))))
  )

(defeffect ::do-structure [{:keys [editor x y $structure-state]}]
  (future
    (try
      (let [^Rope
            rope (:rope editor)]
        (when (pos? (.size rope))
          (let [para (editor->paragraph editor)
                [char-index affinity] (para/glyph-position-at-coordinate para x y)
                rope (.sliceBytes rope
                                  (:start-byte-offset para)
                                  (:end-byte-offset para))

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))

                char-index (if (zero? affinity)
                             (.preceding bi char-index)
                             char-index)
                
                byte-index (let [s (.toString (.subSequence rope 0 char-index))
                                 bs (.getBytes s "utf-8")]

                             (alength bs))
                byte-index (+ byte-index (:start-byte-offset para))


                ^TSTree
                tree (:tree editor)
                root-node (.getRootNode tree)
                node (.getNamedDescendantForByteRange root-node byte-index byte-index)]
            (when node
              (let [node-str (util/node->str (:rope editor) node)
                    node-str (str/join
                              "\n"
                              (cons
                               (.getType node)
                               (eduction
                                (take 4)
                                (str/split-lines
                                 node-str))))]
                (dispatch! :update $structure-state assoc :node-str node-str))))))
      (catch Exception e
        (prn e)))))

(defeffect ::do-structure-click [{:keys [editor x y $structure-state]}]
  (future
    (try
      (let [^Rope
            rope (:rope editor)]
        (when (pos? (.size rope))
          (let [para (editor->paragraph editor)
                [char-index affinity] (para/glyph-position-at-coordinate para x y)
                rope (.sliceBytes rope
                                  (:start-byte-offset para)
                                  (:end-byte-offset para))
                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))

                char-index (if (zero? affinity)
                             (.preceding bi char-index)
                             char-index)
                
                byte-index (let [s (.toString (.subSequence rope 0 char-index))
                                 bs (.getBytes s "utf-8")]

                             (alength bs))
                byte-index (+ byte-index (:start-byte-offset para))


                ^TSTree
                tree (:tree editor)
                root-node (.getRootNode tree)
                node (.getNamedDescendantForByteRange root-node byte-index byte-index)]
            (when (= "sym_name" (.getType node))
              (let [node-str (util/node->str (:rope editor) node)
                    sym (symbol node-str)]
                (dispatch! :update $structure-state update :args
                           (fn [args]
                             (if args
                               (if (contains? args sym)
                                 (disj args sym)
                                 (conj args sym))
                               #{sym}))))))))
      (catch Exception e
        (prn e)))))








(defui debug [{:keys [editor]}]
  (let [
        
        font-family (get extra :font-family)
        font-size (get extra :font-size 12)
        viscous? (get editor :viscous?)
        instarepl? (get extra :instarepl?)
        structure? (get extra :structure?)

        $editor $editor
        editor (-> editor
                   (assoc :instarepl? instarepl?)
                   (assoc :structure? structure?)
                   (assoc-in
                    [:base-style :text-style/font-families] (if font-family
                                                              [font-family]
                                                              ["Menlo"]))
                   (assoc-in
                    [:base-style :text-style/font-size] (or font-size 12)))]
    (ui/flex-layout
     [(ui/flex-layout
       [(ant/button {:text  "tap>"
                     :on-click (fn []
                                 [[::tap editor]])})
        (ant/button {:text  "clear"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor)
                                       (text-mode/editor-self-insert-command "\n\n\n\n\n\n")
                                       (assoc :cursor {:byte 0
                                                       :char 0
                                                       :point 0
                                                       :row 0
                                                       :column 0})
                                       (dissoc :structure-state )
                                       (text-mode/editor-update-viewport))
                                   ]])})
        (ant/button {:text  "load"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor)
                                       (text-mode/editor-self-insert-command
                                        (slurp (io/resource "com/phronemophobic/clobber.clj")))
                                       (assoc :cursor {:byte 0
                                                       :char 0
                                                       :point 0
                                                       :row 0
                                                       :column 0})
                                       (text-mode/editor-update-viewport))
                                   ]])})
        (ant/button {:text  "load-json"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor (TreeSitterJson.))
                                       (text-mode/editor-self-insert-command
                                        (slurp (io/as-url
                                                "https://raw.githubusercontent.com/sogaiu/tree-sitter-clojure/refs/heads/master/src/grammar.json")))
                                       (assoc :cursor {:byte 0
                                                       :char 0
                                                       :point 0
                                                       :row 0
                                                       :column 0})
                                       (text-mode/editor-update-viewport))
                                   ]])})]
       {:direction :row
        :gap 4
        :align :center})
      (ant/radio-bar
       {:options (into []
                       (map (fn [s]
                              {:text s
                               :value s}))
                       ["Menlo"
                        "Webdings"
                        "Savoye LET"
                        "Fira Code"
                        "Comic Sans MS"])
        :selection font-family})
      (ui/flex-layout
       [(ui/label "font-size")
        (ant/number-slider
         {:integer? true
          :width 200
          :min 12
          :max 40
          :value font-size})
        ]
       {:direction :row
        :gap 4
        :align :center})
      (ui/flex-layout
       [(basic/checkbox
         {:checked? viscous?})
        (ui/label "viscous?")]
       {:direction :row
        :gap 4
        :align :center})
      (ui/flex-layout
       [(basic/checkbox
         {:checked? instarepl?})
        (ui/label "instarepl?")]
       {:direction :row
        :gap 4
        :align :center})
      (ui/flex-layout
       [(basic/checkbox
         {:checked? structure?})
        (ui/label "structure?")]
       {:direction :row
        :gap 4
        :align :center})
      (code-editor {:editor editor
                    :$editor $editor})]
     
     
     {:direction :column
      :gap 8})))



(defn dev []
  (dev/add-component-as-applet #'debug
                               {:editor (-> (make-editor)
                                            (text-mode/editor-self-insert-command
                                             (slurp (io/resource "com/phronemophobic/easel.clj")))
                                            (assoc :eval-ns (the-ns 'com.phronemophobic.easel))
                                            (assoc :cursor {:byte 0
                                                            :char 0
                                                            :point 0
                                                            :row 0
                                                            :column 0})
                                            (text-mode/editor-update-viewport))})
  ,)

(defeffect ::tap [& args]
  (case (count args)
    0 nil
    1 (tap> (first args))
    2 (tap> (vec args))))
