(ns com.phronemophobic.clobber.modes.clojure
  (:require [com.phronemophobic.clobber.modes.text :as text-mode]
            [clojure.edn :as edn]
            [com.phronemophobic.clobber.util :as util]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.treesitter TSTree
                           TSInputEdit
                           TSInputEncoding
                           TSTreeCursor
                           TreeSitterClojure
                           TSParser
                           TSPoint
                           TSNode)
           com.ibm.icu.text.BreakIterator
           clojure.lang.LineNumberingPushbackReader
           java.io.StringReader
           io.lacuna.bifurcan.Rope))



(def coll-node-types #{"map_lit" "list_lit" "set_lit" "vec_lit"})


(defn ^:private debug-node [editor node]
  (if node
    (assoc editor :debug-node {(:rope editor) node})
    (dissoc editor :debug-node)))

(def highlight-queries
  ;; tree-sitter-clojure/queries/highlights.scm
  "

  (list_lit) @list

;; Literals

(num_lit) @number

[
  (char_lit)
  (str_lit)
] @string

[
 (bool_lit)
 (nil_lit)
] @constant.builtin

(kwd_lit) @constant

;; Comments

(comment) @comment



;; Treat quasiquotation as operators for the purpose of highlighting.

[
 \"'\"
 \"`\"
 \"~\"
 \"@\"
 \"~@\"
] @operator
"



  )

(def text-colors
  {"number" [0.06666667014360428 0.4000000059604645 0.2666666805744171]
   "string" [0.6666666865348816 0.06666667014360428 0.06666667014360428]
   "constant.builtin" [0.20000000298023224 0.0 0.6666666865348816]
   "constant" [0.46666666865348816 0.0 0.5333333611488342]
   "comment" [0.6666666865348816 0.3333333432674408 0.0]
   "defn" [0.7019607843137254 0.050980392156862744 1.0]
   })


(defn editor-paredit-wrap-round [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor

        ^TSNode
        wrap-node (util/named-child-for-byte tree cursor-byte)]
    (if wrap-node
      (let [;; first, insert ")" add end of node
            editor (text-mode/editor-insert editor ")" (.getEndByte wrap-node))

            ;; next goto and insert "("
            target-byte (.getStartByte wrap-node)
            editor (text-mode/editor-goto-byte editor target-byte)
            editor (text-mode/editor-self-insert-command editor "(")]
        editor)
      
      (text-mode/editor-self-insert-command editor "()"))))

(defn editor-paredit-splice-sexp [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor

        ^TSNode
        wrap-node (transduce
                   (comp
                    (filter (fn [^TSNode node]
                              (> (-> node .getEndByte)
                                 cursor-byte)))
                    (filter (fn [^TSNode node]
                              (let [type (.getType node)]
                                (and (.isNamed node)
                                     (or (= "str_lit" type)
                                         (contains? coll-node-types type))))))
                    (take-while (fn [^TSNode node]
                                  (< (-> node .getStartByte)
                                     cursor-byte))))
                   (completing
                    (fn [best-match node]
                      node))
                   nil
                   (util/tree-reducible tree))]
    (if wrap-node
      (let [;; first, delete ")" add end of node

            target-byte (.getEndByte wrap-node)
            diff-rope (.sliceBytes rope cursor-byte target-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            rc-offset (util/count-row-column-bytes diff-string)

            new-cursor-row (+ cursor-row (:row rc-offset))
            new-cursor-column-byte (if (pos? (:row rc-offset))
                                     (:column-byte rc-offset)
                                     (+ (:column-byte rc-offset) cursor-column-byte))

            new-tree (when-let [^TSTree
                                tree tree]
                       (let [tree (.copy tree)]
                         (.edit tree (TSInputEdit. (dec target-byte) target-byte (dec target-byte)
                                                   (TSPoint. new-cursor-row (dec new-cursor-column-byte))
                                                   (TSPoint. new-cursor-row new-cursor-column-byte)
                                                   (TSPoint. new-cursor-row (dec new-cursor-column-byte))))
                         tree))

            ^Rope
            new-rope (.concat (.sliceBytes rope 0 (dec target-byte))
                              (.sliceBytes rope target-byte (.numBytes rope)))

            ;; next, delete "(" at start of node
            target-byte (.getStartByte wrap-node)
            editor (-> (assoc editor
                              :tree new-tree
                              :rope new-rope)
                       (text-mode/editor-snip target-byte (inc target-byte)))

            diff-rope (.sliceBytes rope target-byte cursor-byte)
            diff-string (.toString diff-rope)
            rc-offset (util/count-row-column-bytes diff-string)

            new-cursor {:byte (dec cursor-byte)
                        :char (dec cursor-char)
                        :point (dec cursor-point)
                        :row cursor-row
                        :column-byte (if (pos? (:row rc-offset))
                                       cursor-column-byte
                                       (dec cursor-column-byte))}]
        (assoc editor :cursor new-cursor))
      
      editor)))


(defn editor-paredit-close-coll [editor close-char]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor]
    (if (= cursor-byte (.numBytes rope))
      editor
      (let [root-node (.getRootNode tree)
            cursor (TSTreeCursor. root-node)
            ^TSNode
            parent-coll-node
            (loop [parent nil]
              (let [idx (.gotoFirstChildForByte cursor cursor-byte)
                    node (.currentNode cursor)]
                (if (or (= -1 idx)
                        (> (.getStartByte node)
                           cursor-byte))
                  parent
                  (if (contains? coll-node-types (.getType node))
                    (recur node)
                    (recur parent)))))]
        (if (not parent-coll-node)
          editor
          (let [;; starting from (dec end-byte)
                ;; go backwards until we hit newlines or spaces
                ;; (we're essentially looking for commas)
                end-byte (.getEndByte parent-coll-node)
                
                editor (text-mode/editor-goto-byte editor (dec end-byte))
                ^Rope
                rope (:rope editor)
                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))
                
                start-char-index (-> editor :cursor :char)
                char-index
                (loop [char-index start-char-index]
                  (let [prev-char (.preceding bi char-index)]
                    (cond
                      (= -1 prev-char) char-index
                      (#{\space \newline} (.charAt rope prev-char)) (recur prev-char)
                      :else char-index)))
                ;; char-index diff is same as bytes
                ;; because we're only counting spaces and newlines
                target-byte (- (dec end-byte)
                               (- start-char-index
                                  char-index))
                editor (-> editor
                           (text-mode/editor-goto-byte target-byte)
                           (text-mode/editor-snip target-byte (dec end-byte))
                           (text-mode/editor-forward-char))]
            editor))))))

(defn editor-paredit-open-coll [editor open-char close-char]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor]
    (-> editor
        (text-mode/editor-self-insert-command (str open-char close-char))
        (text-mode/editor-backward-char))))

(defn editor-cider-eval-defun-at-point [editor]
  editor)
(defn editor-cider-eval-last-sexp [editor]
  editor)
(defn editor-paredit-forward [editor]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        next-node (util/next-named-child-for-byte tree cursor-byte)]

    (if (not next-node)
      editor
      (let [target-byte (.getEndByte next-node)

            diff-rope (.sliceBytes rope cursor-byte target-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            rc-offset (util/count-row-column-bytes diff-string)

            new-cursor-row (+ cursor-row (:row rc-offset))
            new-cursor-column-byte (if (pos? (:row rc-offset))
                                     (:column-byte rc-offset)
                                     (+ (:column-byte rc-offset) cursor-column-byte))

            new-cursor {:byte target-byte
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point diff-points)
                        :row new-cursor-row
                        :column-byte new-cursor-column-byte}]
        (assoc editor
               :cursor new-cursor)))))

(defn editor-paredit-backward [editor]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        previous-node (util/previous-named-child-for-byte tree cursor-byte)]
    (if (not previous-node)
      editor
      (let [target-byte (.getStartByte previous-node)

            diff-rope (.sliceBytes rope target-byte cursor-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            rc-offset (util/count-row-column-bytes diff-string)

            new-cursor-row (- cursor-row (:row rc-offset))
            new-cursor-char (- cursor-char (.length diff-string))

            bi (doto (BreakIterator/getCharacterInstance)
                 (.setText rope))
            new-cursor-column-byte (if (pos? (:row rc-offset))
                                     (loop [char-index new-cursor-char]
                                       (let [prev-char (.preceding bi char-index)]
                                         (if (or (= -1 prev-char)
                                                 (= \newline (.charAt rope prev-char)))
                                           (-> (.subSequence rope char-index new-cursor-char)
                                               .toString
                                               .getBytes
                                               alength)
                                           (recur prev-char))))
                                     (- cursor-column-byte (:column-byte rc-offset)))

            new-cursor {:byte target-byte
                        :char new-cursor-char
                        :point (- cursor-point diff-points)
                        :row new-cursor-row
                        :column-byte new-cursor-column-byte}]
        (assoc editor
               :cursor new-cursor)))))

(defn editor-paredit-doublequote [editor]
  (let [^Rope rope (:rope editor)

        cursor-char (-> editor :cursor :char)

        ;; check if we're trying to close a string 
        closing-quote?
        (when (and (< cursor-char (.length rope))
                   (= \" (.charAt rope cursor-char)))
          (let [root-node (.getRootNode ^TSTree (:tree editor))
                cursor (TSTreeCursor. root-node)
                
                cursor-byte (-> editor :cursor :byte)]
            (when (util/skip-to-byte-offset cursor cursor-byte)
              (loop []
                (let [node (.currentNode cursor)]
                  (cond 
                    (> (.getStartByte node) cursor-byte)
                    false
                    
                    (= cursor-byte (dec (.getEndByte node)))
                    (= "str_lit" (.getType node))
                    
                    :else (when (util/goto-next-dfs-node cursor)
                            (recur))))))))]
    (if closing-quote?
      (text-mode/editor-forward-char editor)
      ;; else just insert string 
      (-> editor
          (text-mode/editor-self-insert-command "\"\"")
          (text-mode/editor-backward-char)))))


(defn editor-paredit-open-square [editor]
  (editor-paredit-open-coll editor \[ \]))

(defn editor-paredit-close-square [editor]
  (editor-paredit-close-coll editor \]))

(defn editor-paredit-open-round [editor]
  (editor-paredit-open-coll editor \( \)))

(defn editor-paredit-close-round [editor]
  (editor-paredit-close-coll editor \)))

(defn editor-paredit-open-curly [editor]
  (editor-paredit-open-coll editor \{ \}))

(defn editor-paredit-close-curly [editor]
  (editor-paredit-close-coll editor \}))

(declare editor-paredit-forward-delete)
(defn editor-paredit-kill [editor]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor]
    (cond
      (>= (:point cursor) (.size rope)) editor

      ;; check if we're at the end of line
      (= \newline
         (-> rope (.charAt cursor-char)))
      (-> editor
          (text-mode/editor-append-clipboard (Rope/from "\n"))
          (editor-paredit-forward-delete))

      :else
      (let [root-node (.getRootNode tree)
            cursor (TSTreeCursor. root-node)
            ^TSNode
            parent-coll-node
            (loop [parent root-node]
              (let [idx (.gotoFirstChildForByte cursor cursor-byte)
                    node (.currentNode cursor)]
                (if (or (= -1 idx)
                        (>= (.getStartByte node)
                            cursor-byte))
                  parent
                  (if (or (contains? coll-node-types (.getType node))
                          (= "str_lit" (.getType node)))
                    (recur node)
                    (recur parent)))))

            end-byte-offset
            ;; if parent ends on current line
            ;; just kill everything to the end of the parent
            (if (= (-> parent-coll-node .getEndPoint .getRow)
                   cursor-row)
              (if (= "source" (.getType parent-coll-node))
                (-> parent-coll-node .getEndByte)
                ;; else some coll type node
                (-> parent-coll-node .getEndByte dec))

              ;; else
              ;; if there's an element that
              ;; ends past the current line
              ;; kill everything to the end of that node
              ;; if all elements end before the end of the line
              ;; just kill to the end of the line
              (let [cursor (doto (TSTreeCursor. parent-coll-node)
                             (.gotoFirstChild))

                    ^TSNode
                    last-node-spanning-a-line
                    (loop []
                      (let [current-node (.currentNode cursor)]
                        (cond
                          (.isNull current-node) nil

                          ;; this node starts before cursor
                          (< (-> current-node
                                 .getStartByte)
                             cursor-byte)
                          (when (.gotoNextSibling cursor)
                            (recur))

                          ;; this node starts on a subsequent
                          (> (-> current-node
                                 .getStartPoint
                                 .getRow)
                             cursor-row)
                          nil

                          ;; this node ends on a subsequent line
                          (> (-> current-node
                                 .getEndPoint
                                 .getRow)
                             cursor-row)
                          current-node

                          :else (when (.gotoNextSibling cursor)
                                  (recur)))))]
                (if last-node-spanning-a-line
                  (-> last-node-spanning-a-line
                      .getEndByte)
                  ;; just find the end of line
                  (let [bi (doto (BreakIterator/getCharacterInstance)
                             (.setText rope))
                        
                        char-index (loop [char-index cursor-char
                                          all-whitespace? true]
                                     (let [next-char (.following bi char-index)]
                                       (if (= -1 next-char)
                                         char-index
                                         (let [c (.charAt rope char-index)]
                                           (case c
                                             \newline (if all-whitespace?
                                                        next-char
                                                        char-index)
                                             \space (recur next-char
                                                           all-whitespace?)
                                             ;; else
                                             (recur next-char false))))))]
                    (if (= char-index cursor-char)
                      cursor-byte
                      (let [diff-string (-> (.subSequence rope cursor-char char-index )
                                            .toString)
                            num-bytes (alength (.getBytes diff-string "utf-8"))]
                        (+ cursor-byte num-bytes)))))))]
        (if (= end-byte-offset cursor-byte)
          editor
          (let [new-tree (when-let [^TSTree
                                    tree tree]
                           (let [tree (.copy tree)]
                             (.edit tree (TSInputEdit. cursor-byte end-byte-offset cursor-byte
                                                       (TSPoint. cursor-row cursor-column-byte)
                                                       (TSPoint. cursor-row (+ cursor-column-byte
                                                                               (- end-byte-offset
                                                                                  cursor-byte)))
                                                       (TSPoint. cursor-row cursor-column-byte)))
                             tree))

                new-rope (.concat (.sliceBytes rope 0 cursor-byte)
                                  (.sliceBytes rope end-byte-offset (.numBytes rope)))
                clipboard-rope (.sliceBytes rope cursor-byte end-byte-offset)

                reader (util/->RopeReader new-rope)
                new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
            (-> editor
                (text-mode/editor-append-clipboard clipboard-rope)
                (assoc :tree new-tree
                       :paragraph nil
                       :rope new-rope))))))))



(defn editor-paredit-forward-delete [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
    (if (>= (:point cursor) (.size rope))
      editor
      (let [{cursor-byte :byte
             cursor-char :char
             cursor-row :row
             cursor-point :point
             cursor-column-byte :column-byte} cursor
            current-char (.charAt rope cursor-char)]
        (case current-char
          (\( \[ \{)
          (text-mode/editor-forward-char editor)
          
          (\) \] \} \")
          (let [ ;; check if empty and delete whole coll

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))

                prev-char-offset (.preceding bi cursor-char)]
            (cond
              (= -1 prev-char-offset) (text-mode/editor-forward-char editor)

              (= ^char (get {\) \(, \] \[, \} \{, \" \"} current-char)
                 (.charAt rope prev-char-offset))
              ;; delete whole coll
              (let [new-tree (when-let [^TSTree tree tree]
                               (let [tree (.copy tree)]
                                 (.edit tree (TSInputEdit. (dec cursor-byte) (inc cursor-byte) (dec cursor-byte)
                                                           (TSPoint. cursor-row (dec cursor-column-byte))
                                                           (TSPoint. cursor-row (inc cursor-column-byte))
                                                           (TSPoint. cursor-row (dec cursor-column-byte))))
                                 tree))

                    new-rope (.concat (.slice rope 0 (dec cursor-point))
                                      (.slice rope (inc cursor-point) (.size rope)))
                    reader (util/->RopeReader new-rope)
                    new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8)

                    new-cursor {:byte (dec cursor-byte)
                                :char (dec cursor-char)
                                :point (dec cursor-point)
                                :row cursor-row
                                :column-byte (dec cursor-column-byte)}]
                (assoc editor
                       :tree new-tree
                       :cursor new-cursor
                       :rope new-rope))

              :else (text-mode/editor-forward-char editor)))

          ;; else
          (let [bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))
                next-char (.following bi cursor-char)

                diff-string (-> (.subSequence rope cursor-char next-char)
                                .toString)
                diff-bytes (alength (.getBytes diff-string "utf-8"))
                next-byte (+ cursor-byte diff-bytes)
                next-point (+ cursor-point (util/num-points diff-string))

                newline? (= \newline (.charAt rope cursor-char))

                new-tree (when-let [^TSTree tree tree]
                           (let [tree (.copy tree)]
                             (.edit tree (TSInputEdit. cursor-byte next-byte cursor-byte
                                                       (TSPoint. cursor-row cursor-column-byte)
                                                       (if newline?
                                                         (TSPoint. (inc cursor-row) 0)
                                                         (TSPoint. cursor-row (inc cursor-column-byte)))
                                                       (TSPoint. cursor-row cursor-column-byte)))
                             tree))

                new-rope (.concat (.slice rope 0 cursor-point)
                                  (.slice rope next-point (.size rope)))
                reader (util/->RopeReader new-rope)
                new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
            (assoc editor
                   :tree new-tree
                   :cursor cursor
                   :paragraph nil
                   :rope new-rope)))))))




(defn editor-paredit-backward-delete [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
    (if (<= (:byte cursor) 0)
      editor
      (let [{cursor-byte :byte
             cursor-char :char
             cursor-row :row
             cursor-point :point
             cursor-column-byte :column-byte} cursor
            bi (doto (BreakIterator/getCharacterInstance)
                 (.setText rope))
            prev (.preceding bi cursor-char)
            prev-char (.charAt rope prev)]
        (case prev-char
          (\) \] \})
          (text-mode/editor-backward-char editor)

          (\[ \" \{ \()
          ;; check if empty and delete whole coll
          (cond
            (= (.length rope) cursor-char) (text-mode/editor-backward-char editor)

            (= ^char (get {\( \), \[ \], \{ \}, \" \"} prev-char)
               (.charAt rope cursor-char))
            ;; delete whole coll
            (let [new-tree (when-let [^TSTree tree tree]
                             (let [tree (.copy tree)]
                               (.edit tree (TSInputEdit. (dec cursor-byte) (inc cursor-byte) (dec cursor-byte)
                                                         (TSPoint. cursor-row (dec cursor-column-byte))
                                                         (TSPoint. cursor-row (inc cursor-column-byte))
                                                         (TSPoint. cursor-row (dec cursor-column-byte))))
                               tree))

                  new-rope (.concat (.slice rope 0 (dec cursor-point))
                                    (.slice rope (inc cursor-point) (.size rope)))
                  reader (util/->RopeReader new-rope)
                  new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8)

                  new-cursor {:byte (dec cursor-byte)
                              :char (dec cursor-char)
                              :point (dec cursor-point)
                              :row cursor-row
                              :column-byte (dec cursor-column-byte)}]
              (assoc editor
                     :tree new-tree
                     :cursor new-cursor
                     :rope new-rope))

            :else (text-mode/editor-backward-char editor))

          ;;else
          (let [diff-string (-> (.subSequence rope prev cursor-char)
                                .toString)
                diff-bytes (alength (.getBytes diff-string "utf-8"))

                prev-byte (- cursor-byte diff-bytes)
                prev-char (.charAt rope prev)
                prev-point (- cursor-point (util/num-points diff-string))
                rc-offset (util/count-row-column-bytes diff-string)
                

                new-cursor-row (- cursor-row (:row rc-offset))
                new-cursor-char (- cursor-char (.length diff-string))

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))
                new-cursor-column-byte (if (pos? (:row rc-offset))
                                         (loop [char-index new-cursor-char]
                                           (let [prev-char (.preceding bi char-index)]
                                             (if (or (= -1 prev-char)
                                                     (= \newline (.charAt rope prev-char)))
                                               (-> (.subSequence rope char-index new-cursor-char)
                                                   .toString
                                                   .getBytes
                                                   alength)
                                               (recur prev-char))))
                                         (- cursor-column-byte (:column-byte rc-offset)))

                new-tree (when-let [^TSTree tree tree]
                           (let [tree (.copy tree)]

                             (.edit tree (TSInputEdit. prev-byte cursor-byte prev-byte
                                                       (TSPoint. new-cursor-row new-cursor-column-byte)
                                                       (TSPoint. cursor-row cursor-column-byte)
                                                       (TSPoint. new-cursor-row new-cursor-column-byte)))
                             tree))

                new-rope (.concat (.slice rope 0 prev-point)
                                  (.slice rope cursor-point (.size rope)))

                reader (util/->RopeReader new-rope)
                new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
            (assoc editor
                   :tree new-tree
                   :cursor {:byte prev-byte
                            :char prev
                            :point prev-point
                            :row new-cursor-row
                            :column-byte new-cursor-column-byte}
                   :paragraph nil
                   :rope new-rope)))))))

;; indentation
;; modeled after https://github.com/weavejester/cljfmt/blob/master/docs/INDENTS.md
(def ^:private indent-config
  (let [indents (with-open [reader (io/reader (io/resource "cljfmt/indents/clojure.clj"))
                            reader (java.io.PushbackReader. reader)]
                  (edn/read reader))
        inner-rules (into []
                          (mapcat
                           (fn [[sym rules]]
                             (eduction
                              (filter #(= :inner (first %)))
                              (map (fn [[_inner depth index]]
                                     [depth (cond-> {:sym sym}
                                              index (assoc :index index))]))
                              rules)))
                          indents)
        max-depth (transduce (map first) max 0 inner-rules)

        
        inner-rules-index (into []
                                (map (fn [n]
                                       (into {}
                                             (keep (fn [[depth rule]]
                                                     (when (= depth n)
                                                       [(name (:sym rule)) (:index rule)])))
                                             inner-rules)))
                                (range (inc max-depth)))

        block-index
        (into {}
              (keep (fn [[sym rules]]
                      (when-let [[_block index :as block-rule] (util/first-by
                                                                (filter #(= :block (first %)))
                                                                rules)]
                        [(name sym) index])))
              indents)]
    {;; vector of [ {sym -> index} for depth 0, {sym -> index} for depth 1, ...] 
     :inner-rules-index inner-rules-index
     ;; map of {sym -> index}
     :block-index block-index}) )


(defn ^:private matches-inner? [parent rope]
  (loop [^TSNode node parent
         child nil
         inner-rules-index (seq (:inner-rules-index indent-config))]
    (when (and inner-rules-index
               (not (.isNull node)))
      (let [m (first inner-rules-index)
            first-child (.getNamedChild node 0)
            sym-name (when (and (not (.isNull first-child))
                                (= "sym_lit" (.getType first-child)))
                       (util/rope->str rope
                                       (.getStartByte first-child)
                                       (.getEndByte first-child)))]
        (if-let [[_ index] (and sym-name
                                (find m sym-name))]
          (if index 
            (and child
                 (TSNode/eq child (.getNamedChild node (inc index))))
            true)
          ;; else
          (recur (.getParent node)
                 node
                 (next inner-rules-index)))))))

(defn ^:private matches-def? [^TSNode parent rope]
  (let [first-child (.getNamedChild parent 0)
        sym-name (when (and (not (.isNull first-child))
                            (= "sym_lit" (.getType first-child)))
                   (util/rope->str rope
                                   (.getStartByte first-child)
                                   (.getEndByte first-child)))]
    (when sym-name
      (str/starts-with? sym-name "def"))))

(defn ^:private matches-block? [^TSNode parent rope]
  (when (= "list_lit" (.getType parent))
    (let [first-child (.getNamedChild parent 0)
          sym-name (when (and (not (.isNull first-child))
                              (= "sym_lit" (.getType first-child)))
                     (util/rope->str rope
                                     (.getStartByte first-child)
                                     (.getEndByte first-child)))]
      (when-let [block-index (get (:block-index indent-config) sym-name)]
        (let [parent-row (-> parent .getStartPoint .getRow)
              child-count (dec (.getNamedChildCount parent))]
          (cond
            (< child-count block-index) 4
            (= child-count block-index) 2
            :else
            (let [child (.getNamedChild parent (inc block-index))
                  child-row (-> child .getStartPoint .getRow)]
              (when (not= parent-row
                          child-row)
                ;; make sure child is first on its line
                (loop [index block-index]
                  (if (neg? index)
                    2
                    (let [node (.getNamedChild parent index)]
                      (if (= child-row
                             (-> node .getStartPoint .getRow))
                        false
                        (recur (dec index))))))))))))))


(defn ^:private editor-indent* [editor ^TSNode parent indent]
  (let [;; calculate parent offset in grapheme clusters
        ^Rope rope (:rope editor)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        parent-offset (loop [offset 0
                             char-index (.length (.sliceBytes rope 0 (-> parent .getStartByte)))]
                        (let [prev-char (.preceding bi char-index)]
                          (cond
                            (= -1 prev-char) offset
                            (= \newline (.charAt rope prev-char)) offset
                            :else (recur (inc offset)
                                         prev-char))))

        bol-editor (text-mode/editor-move-beginning-of-line editor)
        rope-length (.length rope)
        current-offset
        (loop [offset 0
               char-index (-> bol-editor :cursor :char)]
          (if (>= char-index rope-length)
            offset
            (let [char (.charAt rope char-index)]
              (if (= \space char)
                (recur (inc offset)
                       (.following bi char-index))
                offset))))


        indent-diff (- (+ parent-offset indent)
                       current-offset)

        cursor-column-byte (-> editor :cursor :column-byte)
        
        next-cursor-offset (if (> cursor-column-byte current-offset)
                             (+ (-> editor :cursor :byte) indent-diff)
                             (+ (-> bol-editor :cursor :byte) current-offset indent-diff))

        ;; fix the number of spaces
        next-editor 
        (cond
          (pos? indent-diff) (text-mode/editor-insert bol-editor (str/join (repeat indent-diff " ")))

          (neg? indent-diff) (let [cursor-byte (-> bol-editor :cursor :byte)]
                               (text-mode/editor-snip bol-editor cursor-byte (+ cursor-byte (- indent-diff))))
          :else bol-editor)

        next-editor (text-mode/editor-goto-byte next-editor next-cursor-offset)]
    next-editor))


(defn ^:private editor-indent-coll* [editor ^TSNode parent-coll-node]
  (let [indent (if (= "set_lit" (.getType parent-coll-node))
                 2
                 1)]
    (editor-indent* editor parent-coll-node indent)))


(defn ^:private editor-indent-default*
  "Indents the current line using the default style from cljfmt."
  [editor ^TSNode parent-coll-node]
  ;; indent matches first arg if on the same line as parent
  ;; otherwise 1 space
  (let [arg-node (.getNamedChild parent-coll-node 1)]
    (if (and (not (.isNull arg-node))
             (= (-> parent-coll-node .getStartPoint .getRow)
                (-> arg-node .getStartPoint .getRow)))
      ;; indent to arg node
      (let [^Rope rope (:rope editor)
            bi (doto (BreakIterator/getCharacterInstance)
                 (.setText rope))
            arg-offset (loop [offset 0
                              char-index (.length (.sliceBytes rope 0 (-> arg-node .getStartByte)))]
                         (let [prev-char (.preceding bi char-index)]
                           (cond
                             (= -1 prev-char) offset
                             (= \newline (.charAt rope prev-char)) offset
                             :else (recur (inc offset)
                                          prev-char))))

            bol-editor (text-mode/editor-move-beginning-of-line editor)
            rope-length (.length rope)
            ;; find number of spaces to first element on line
            current-offset
            (loop [offset 0
                   char-index (-> bol-editor :cursor :char)]
              (if (>= char-index rope-length)
                offset
                (let [char (.charAt rope char-index)]
                  (if (= \space char)
                    (recur (inc offset)
                           (.following bi char-index))
                    offset))))


            indent-diff (- arg-offset
                           current-offset)

            cursor-column-byte (-> editor :cursor :column-byte)
            next-cursor-offset (if (> cursor-column-byte current-offset)
                                 (+ cursor-column-byte indent-diff)
                                 (+ current-offset indent-diff))
            

            ;; fix the number of spaces
            next-editor 
            (cond
              (pos? indent-diff) (text-mode/editor-insert bol-editor (str/join (repeat indent-diff " ")))

              (neg? indent-diff) (let [cursor-byte (-> bol-editor :cursor :byte)]
                                   (text-mode/editor-snip bol-editor cursor-byte (+ cursor-byte (- indent-diff))))
              :else bol-editor)

            ;; move forward
            next-editor (loop [n next-cursor-offset
                               e next-editor]
                          (if (pos? n)
                            (recur (dec n)
                                   (text-mode/editor-forward-char e))
                            e))]
        next-editor)

      ;; else indent normally
      (editor-indent-coll* editor parent-coll-node))))


(defn editor-indent
  "Indents the current line.

  If the cursor is before the first element on the line,
  move the cursor to the indentation. Otherwise,
  fix the indentation of the current line and keep the
  same relative cursor position."
  [editor]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        root-node (.getRootNode tree)
        cursor (TSTreeCursor. root-node)
        ;; find a collection node that starts before the current line
        ;; and ends on or after the current line
        ^TSNode
        parent-coll-node
        (when (.gotoFirstChild cursor)
          ;; skip all top level nodes that end
          ;; before current line
          (when (loop []
                  (if (< (-> (.currentNode cursor)
                             .getEndPoint
                             .getRow)
                         cursor-row)
                    (when (.gotoNextSibling cursor)
                      (recur))
                    true))
            (loop [match nil]
              (let [node (.currentNode cursor)]
                (if (>= (-> node .getStartPoint .getRow)
                        cursor-row)
                  match
                  ;; else, started before this line
                  (let [match (if (and (contains? coll-node-types (.getType node))
                                       (>= (-> node .getEndPoint .getRow)
                                           cursor-row))
                                node
                                match)]
                    (when (util/goto-next-dfs-node cursor)
                      (recur match))))))))]
    (if (not parent-coll-node)
      (editor-indent* editor root-node 0)
      ;; do indent
      (if (= "list_lit" (.getType parent-coll-node))
        (if-let [block-indent (matches-block? parent-coll-node (:rope editor))]
          (editor-indent* editor parent-coll-node block-indent)
          (if (or (matches-inner? parent-coll-node (:rope editor))
                  (matches-def? parent-coll-node (:rope editor)))
            (editor-indent* editor parent-coll-node 2)
            ;; else
            (editor-indent-default* editor parent-coll-node)))
        ;; use normal coll indent
        (editor-indent-coll* editor parent-coll-node)))))

(defn paredit-newline [editor]
  (-> editor
      (text-mode/editor-self-insert-command "\n")
      (editor-indent)))

(defn editor-indent-region [editor]
  (let [
        cursor (:cursor editor)
        {:keys [start-row end-row]}
        (if-let [select-cursor (:select-cursor editor)]
          {:start-row (min (:row select-cursor)
                           (:row cursor))
           :end-row (max (:row select-cursor)
                         (:row cursor))}
          ;; indent next form
          (if-let [node (util/next-named-child-for-byte (:tree editor) (-> editor :cursor :byte))]
            {:start-row (:row cursor)
             :end-row (-> node .getEndPoint .getRow)}
            {:start-row (:row cursor)
             :end-row (:row cursor)}))]
    (if (< start-row (:row cursor))
      (let [;; backup to start row
            ;; and then indent going forward
            editor (text-mode/editor-previous-line
                    editor (- (:row cursor)
                              start-row))
            editor (loop [n (- end-row start-row)
                          editor editor]
                     (if (pos? n)
                       (recur (dec n)
                              (-> editor
                                  text-mode/editor-next-line
                                  editor-indent))
                       editor))]
        editor)
      ;; else
      (let [;; indent current line and save cursor
            ;; indent subsequent lines,
            ;; reset cursor
            editor (editor-indent editor)
            cursor (:cursor editor)

            editor (loop [n (- end-row start-row)
                          editor editor]
                     (if (pos? n)
                       (recur (dec n)
                              (-> editor
                                  text-mode/editor-next-line
                                  editor-indent))
                       editor))
            editor (text-mode/editor-goto-row-col
                    editor
                    (:row cursor)
                    ;; slightly wrong
                    ;; should be a grapheme cluster offset
                    (:column-byte cursor))]
        editor))))

(def word-whitespace
  #{\space \newline \tab \. \- \_ \( \) \[ \] \{ \} \, \'  \` \# \" \/})

(defn editor-backward-word [editor]
  (let [cursor (:cursor editor)
        ^Rope rope (:rope editor)
        
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        
        

        ;; go backwards past whitespace
        char-index
        (loop [char-index (:char cursor)]
          (let [prev-char (.preceding bi char-index)]
            (cond
              (= -1 prev-char) char-index

              (contains? word-whitespace
                         (.charAt rope prev-char))
              (recur prev-char)

              :else char-index)))
        
        ;; go back until you reach whitespace again
        char-index
        (loop [char-index char-index]
          (let [prev-char (.preceding bi char-index)]
            (cond
              (= -1 prev-char) char-index

              (contains? word-whitespace
                         (.charAt rope prev-char))
              char-index

              :else (recur prev-char))))
        
        diff-string (-> (.subSequence rope char-index (:char cursor))
                        .toString)
        
        byte-index (- (:byte cursor)
                      (alength (.getBytes diff-string "utf-8")))]
    (text-mode/editor-goto-byte editor byte-index)))


(defn editor-forward-word [editor]
  (let [cursor (:cursor editor)
        ^Rope rope (:rope editor)
        
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        
        ;; go past whitespace
        rope-length (.length rope)
        char-index
        (loop [char-index (:char cursor)]
          (cond
            (= rope-length char-index) char-index
            
            (contains? word-whitespace
                       (.charAt rope char-index))
            (recur (.following bi char-index))
            
            :else char-index))
        
        
        ;; go until you reach whitespace again
        char-index
        (loop [char-index char-index]
          (cond
            (= rope-length char-index) char-index
            
            (contains? word-whitespace
                       (.charAt rope char-index))
            char-index
            
            :else (recur (.following bi char-index))))
        

        diff-string (-> (.subSequence rope (:char cursor) char-index)
                        .toString)
        
        byte-index (+ (:byte cursor)
                      (alength (.getBytes diff-string "utf-8")))]
    (text-mode/editor-goto-byte editor byte-index)))

(def kill-skip-whitespace #{\space \newline \tab \( \) \[ \] \{ \} \" })

(defn paredit-backward-kill-word [editor]
  (let [cursor (:cursor editor)
        ^Rope rope (:rope editor)
        
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        
        rope-length (.length rope)
        char-index
        (loop [char-index (:char cursor)]
          (let [prev-char (.preceding bi char-index)]
            (cond
              (= -1 prev-char) char-index

              (contains? kill-skip-whitespace
                         (.charAt rope prev-char))
              (recur prev-char)

              :else char-index)))

        
        snip-char char-index

        char-index
        (loop [char-index char-index]
          (let [prev-char (.preceding bi char-index)]
            (cond
              (= -1 prev-char) char-index

              (contains? kill-skip-whitespace
                         (.charAt rope prev-char))
              char-index

              :else (recur prev-char))))

        diff-bytes (-> (.subSequence rope char-index (:char cursor))
                       .toString
                       (.getBytes "utf-8")
                       alength)
        
        editor (text-mode/editor-goto-byte editor (- (:byte cursor) diff-bytes))
        
        diff-bytes (-> (.subSequence rope char-index snip-char)
                       .toString
                       (.getBytes "utf-8")
                       alength)
        
        start-byte (-> editor :cursor :byte)
        editor (text-mode/editor-snip editor 
                                      start-byte
                                      (+ start-byte diff-bytes))]
    editor))

(defn paredit-forward-kill-word [editor]
  (let [cursor (:cursor editor)
        ^Rope rope (:rope editor)
        
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        
        ;; go past whitespace
        rope-length (.length rope)
        char-index
        (loop [char-index (:char cursor)]
          (cond
            (= rope-length char-index) char-index
            
            (contains? kill-skip-whitespace
                       (.charAt rope char-index))
            (recur (.following bi char-index))
            
            :else char-index))
        
        new-cursor-char char-index

        ;; go until you reach whitespace again
        char-index
        (loop [char-index char-index]
          (cond
            (= rope-length char-index) char-index
            
            (contains? kill-skip-whitespace
                       (.charAt rope char-index))
            char-index
            
            :else (recur (.following bi char-index))))
        
        diff-bytes (-> (.subSequence rope (:char cursor) new-cursor-char)
                       .toString
                       (.getBytes "utf-8")
                       alength)
        editor (text-mode/editor-goto-byte editor (+ (:byte cursor) diff-bytes))
        
        diff-bytes (-> (.subSequence rope new-cursor-char char-index)
                       .toString
                       (.getBytes "utf-8")
                       alength)
        
        start-byte (-> editor :cursor :byte)
        editor (text-mode/editor-snip editor
                                      start-byte
                                      (+ start-byte diff-bytes))]
    editor))

(defn ^:private do-slurp* [editor ^TSNode coll-node ^TSNode slurp-node]
  (let [coll-end-byte (.getEndByte coll-node)
        slurp-end-byte (.getEndByte slurp-node)
        end-coll-str (case (.getType coll-node)
                       "str_lit" "\""
                       "vec_lit" "]"
                       "list_lit" ")"
                       ("set_lit" "map_lit") "}")]
    (-> editor
        (text-mode/editor-insert end-coll-str slurp-end-byte)
        (text-mode/editor-snip (dec coll-end-byte) coll-end-byte))))

(defn paredit-forward-slurp-sexp [editor]
  ;; if current parent-coll can't slurp
  ;; then slurp the parent of that node.
  (let [root-node (.getRootNode ^TSTree (:tree editor))
        cursor (TSTreeCursor. root-node)
        
        cursor-byte (-> editor :cursor :byte)

        ;; find enclosing node
        ^TSTreeCursor
        cursor
        (when (util/skip-to-byte-offset cursor cursor-byte)
          (loop [match-cursor nil]
            (let [node (.currentNode cursor)]
              (if (>= (-> node .getStartByte)
                      cursor-byte)
                match-cursor
                (if (and (< cursor-byte (.getEndByte node))
                         (or (contains? coll-node-types (.getType node))
                             (= "str_lit" (.getType node))))
                  (let [match-cursor (.copy cursor)]
                    (if (util/goto-next-dfs-node cursor)
                      (recur match-cursor)
                      match-cursor))
                  ;; else
                  (when (util/goto-next-dfs-node cursor)
                    (recur match-cursor)))))))]
    (if cursor
      (let [;; find node to slurp
            [coll-node slurp-node :as result]
            (loop []
              (let [coll-node (.currentNode cursor)
                    slurp-node (loop []
                                 (when (.gotoNextSibling cursor)
                                   (let [node (.currentNode cursor)]
                                     (if (.isNamed node)
                                       node
                                       (recur)))))]
                (if slurp-node
                  [coll-node slurp-node]
                  (when (.gotoParent cursor)
                    (recur)))))]
        (if result
          (do-slurp* editor coll-node slurp-node)
          editor))

      editor)))

(defn paredit-forward-barf-sexp [editor]
  (let [root-node (.getRootNode ^TSTree (:tree editor))
        cursor (TSTreeCursor. root-node)
        
        cursor-byte (-> editor :cursor :byte)
        
        ;; find enclosing node
        ^TSNode
        node
        (when (util/skip-to-byte-offset cursor cursor-byte)
          (loop [match nil]
            (let [node (.currentNode cursor)]
              (if (>= (-> node .getStartByte)
                      cursor-byte)
                match
                (if (and (< cursor-byte (.getEndByte node))
                         (contains? coll-node-types (.getType node)))
                  (if (util/goto-next-dfs-node cursor)
                    (recur node)
                    node)
                  ;; else
                  (when (util/goto-next-dfs-node cursor)
                    (recur match)))))))]
    (or
     (when node
       (let [named-count (.getNamedChildCount node)]
         (when (pos? named-count)
           (let [barf-node (.getNamedChild node (dec named-count))
                 coll-end-str (case (.getType node)
                                "vec_lit" "]"
                                "list_lit" ")"
                                ("set_lit" "map_lit") "}")]
             (if (= 1 named-count)
               (let [coll-end-byte (.getEndByte node)
                     coll-start-byte (+ (.getStartByte node)
                                        (if (= "set_lit" (.getType node))
                                          2
                                          1))
                     add-space? (= coll-start-byte (.getStartByte barf-node))
                     
                     insert-str (if add-space?
                                  (str coll-end-str " ")
                                  coll-end-str)]
                 (-> editor
                     (text-mode/editor-snip (dec coll-end-byte) coll-end-byte)
                     (text-mode/editor-goto-byte coll-start-byte)
                     (text-mode/editor-insert insert-str)))
               ;; barf last elem
               (let [coll-end-byte (.getEndByte node)
                     new-last-node (.getNamedChild node (- named-count 2))
                     new-last-node-end-byte (.getEndByte new-last-node)]
                 (-> editor
                     (text-mode/editor-snip (dec coll-end-byte) coll-end-byte)
                     (text-mode/editor-goto-byte new-last-node-end-byte)
                     (text-mode/editor-insert coll-end-str))))))))
     editor)))



(defn paredit-semicolon
  "Comments the next form.

  Pushes collection end characters to next line. "
  [editor]
  (let [cursor-byte (-> editor :cursor :byte)
        cursor-row (-> editor :cursor :row)
        root-node (.getRootNode ^TSTree (:tree editor))
        cursor (TSTreeCursor. root-node)

        ;; find enclosing coll if it
        ;; ends on the same line
        ^TSNode
        parent-coll-node
        (transduce
         (comp (filter (fn [^TSNode node]
                         (.isNamed node)))
               (take-while (fn [^TSNode node]
                             (<= (-> node .getStartPoint .getRow)
                                 cursor-row))))
         (completing
          (fn [result ^TSNode node]
            (if (and (contains? coll-node-types (.getType node))
                     (< (.getStartByte node)
                        cursor-byte)
                     (> (.getEndByte node)
                        cursor-byte)
                     (= cursor-row (-> node .getEndPoint .getRow)))
              node
              result)))
         nil
         (when (util/skip-to-byte-offset cursor cursor-byte)
           (util/tree-cursor-reducible cursor)))]
    (if parent-coll-node
      (let [end-byte (-> parent-coll-node
                         .getEndByte
                         dec)
            cursor (:cursor editor)]
        (-> editor
            (text-mode/editor-self-insert-command ";")
            (text-mode/editor-goto-byte (inc end-byte))
            text-mode/editor-delete-horizontal-space
            paredit-newline
            (assoc :cursor cursor)
            text-mode/editor-forward-char))
      ;; else
      (text-mode/editor-self-insert-command editor ";"))))


(declare editor-toggle-comment-line)
(defn editor-comment-region [editor]
  (let [cursor (:cursor editor)
        {:keys [start-row end-row start-byte]}
        (if-let [select-cursor (:select-cursor editor)]
          {:start-row (min (:row select-cursor)
                           (:row cursor))
           :start-byte (min (:byte select-cursor)
                            (:byte cursor))
           :end-row (max (:row select-cursor)
                         (:row cursor))}
          {:start-row (:row cursor)
           :start-byte (:byte cursor)
           :end-row (:row cursor)})
        
        ;; check if first row has a comment
        comment-node
        (util/first-by
         (comp (filter (fn [^TSNode node]
                         (.isNamed node)))
               (filter (fn [^TSNode node]
                         (= (-> node .getStartPoint .getRow)
                            start-row)))
               (filter (fn [^TSNode node]
                         (= "comment" (.getType node))))
               (take-while (fn [^TSNode node]
                             (<= (-> node .getStartPoint .getRow)
                                 start-row))))
         
         (let [^TSTree tree (:tree editor)
               tree-cursor (-> tree
                               .getRootNode
                               (TSTreeCursor.))]
           (when (util/skip-to-byte-offset tree-cursor start-byte)
             (util/tree-cursor-reducible tree-cursor))))]
    (if comment-node
      ;; uncomment
      (let [;; find the rows with comments
            
            comment-starts
            (into
             []
             (comp (take-while (fn [^TSNode node]
                                 (<= (-> node .getStartPoint .getRow)
                                     end-row)))
                   (filter (fn [^TSNode node]
                             (= "comment" (.getType node))))
                   (filter (fn [^TSNode node]
                             (>= (-> node .getStartPoint .getRow)
                                 start-row)))
                   (map (fn [^TSNode node]
                          (.getStartByte node))))
             (let [^TSTree tree (:tree editor)
                   tree-cursor (-> tree
                                   .getRootNode
                                   (TSTreeCursor.))]
               (when (util/skip-to-byte-offset tree-cursor start-byte)
                 (util/tree-cursor-reducible tree-cursor))))
            
            editor (if (not= start-row (:row cursor))
                     (assoc editor :cursor (:select-cursor editor))
                     editor)
            
            editor
            (loop [editor editor
                   offset 0
                   comment-starts (seq comment-starts)]
              (if comment-starts
                (let [comment-start-byte (first comment-starts)
                      
                      next-editor (-> editor
                                      (text-mode/editor-goto-byte 
                                       (+ comment-start-byte
                                          offset))
                                      (editor-toggle-comment-line))
                      offset (+ offset
                                (- (.numBytes ^Rope (:rope next-editor))
                                   (.numBytes ^Rope (:rope editor))))]
                  (recur next-editor
                         offset
                         (next comment-starts)))
                ;; else
                editor))]
        editor)
      ;; comment
      ;; insert comments at start cursor 
      ;; insert comments for subsequent rows
      ;; only only rows that start or end that row
      ;; use min column-byte of those rows
      (let [
            
            merge-point (fn [min-cols [row col] ]
                          (if-let [col' (get min-cols row)]
                            (if (< col col')
                              (assoc min-cols row col)
                              min-cols)
                            (assoc min-cols row col)))

            min-cols
            (transduce
             (comp 
              (take-while (fn [^TSNode node]
                            (<= (-> node .getStartPoint .getRow)
                                end-row)))
              (mapcat (fn [^TSNode node]
                        (let [start (.getStartPoint node)
                              end (.getEndPoint node)
                              end-col (.getColumn end)]
                          (if (zero? end-col)
                            [[(.getRow start) (.getColumn start)]]
                            [[(.getRow start) (.getColumn start)]
                             [(.getRow end) (dec end-col)]]))))
              (filter (fn [[row col]]
                        (and (>= row start-row)
                             (<= row end-row)))))

             (completing
              (fn [min-cols pt]
                (merge-point min-cols pt)))
             nil
             
             (let [^TSTree tree (:tree editor)
                   tree-cursor (-> tree
                                   .getRootNode
                                   (TSTreeCursor.))]
               (when (util/skip-to-byte-offset tree-cursor start-byte)
                 (util/tree-cursor-reducible tree-cursor))))
            
            minnest-col (when (seq min-cols)
                          (apply min (vals min-cols)))
            
            editor (if (not= start-row (:row cursor))
                     (assoc editor :cursor (:select-cursor editor))
                     editor)
            
            editor
            (loop [editor editor
                   n (inc (- end-row start-row))]
              (if (pos? n)
                (let [current-row (-> editor :cursor :row)]
                  (recur (if (get min-cols current-row)
                           (-> editor
                               text-mode/editor-move-beginning-of-line
                               (text-mode/editor-forward-char minnest-col)
                               (text-mode/editor-self-insert-command ";; ")
                               text-mode/editor-next-line)
                           (text-mode/editor-next-line editor))
                         (dec n)))
                ;; else
                editor))
            
            editor (text-mode/editor-goto-row-col editor
                                                  (:row cursor)
                                                  ;; slightly wrong
                                                  ;; should be a grapheme cluster offset
                                                  (:column-byte cursor))]
        editor))))

(defn editor-toggle-comment-line [editor]
  (let [
        cursor-byte (-> editor :cursor :byte)
        cursor-row (-> editor :cursor :row)
        root-node (.getRootNode ^TSTree (:tree editor))
        cursor (TSTreeCursor. root-node)
        
        ;; need to find first node on the line
        ;; also need to find enclosing coll
        ;;      that end on the same line
        {:keys [^TSNode enclosing-comment-node
                line-node]}
        (transduce
         (comp (filter (fn [^TSNode node]
                         (.isNamed node)))
               (filter (fn [^TSNode node]
                         (= (-> node .getStartPoint .getRow)
                            cursor-row)))
               (take-while (fn [^TSNode node]
                             (<= (-> node .getStartPoint .getRow)
                                 cursor-row))))
         (completing
          (fn [result ^TSNode node]
            (if (= "comment" (.getType node))
              (reduced {:enclosing-comment-node node})
              (if (:line-node result)
                result
                {:line-node node}))))
         nil
         (when (util/skip-to-byte-offset cursor cursor-byte)
           (util/tree-cursor-reducible cursor)))
        ]
    (if enclosing-comment-node
      (let [start-byte (.getStartByte enclosing-comment-node)
            
            editor (text-mode/editor-goto-byte editor start-byte)
            ^Rope rope (:rope editor)
            rope-length (.length rope)
            ;; semicolons are single bytes
            diff-bytes
            (loop [n 0
                   char-index (-> editor :cursor :char)]
              (if (and (< char-index rope-length)
                       (= \; (.charAt rope char-index)))
                (recur (inc n)
                       (inc char-index))
                n))
            
            cursor-byte (-> editor :cursor :byte)]
        (-> editor
            (text-mode/editor-snip cursor-byte
                                   (+ cursor-byte diff-bytes))
            editor-indent))
      ;; else
      (-> editor
          text-mode/editor-move-beginning-of-line
          editor-indent
          paredit-semicolon
          (text-mode/editor-self-insert-command "; ")))))

(defn editor-toggle-comment
  "Comments the line if not commented. Otherwise, uncomment.
  
  Also applies indentation"
  [editor]
  (let [select-cursor (:select-cursor editor)]
    (if (and select-cursor
             (not= (:row select-cursor)
                   (-> editor :cursor :row)))
      (editor-comment-region editor)
      (editor-toggle-comment-line editor))))

(defn reflection-warnings [editor]
  (let [[;; path relative to class-path  
         source-path
         ;; filename
         source-name] [""
                       ;; .cljc extension required for reader conditionals
                       (if-let [f (:file editor)]
                         (util/file-ext f)
                         "foo.cljc")]
        
        rdr (LineNumberingPushbackReader.
             (StringReader. (str (:rope editor))))]
    
    (binding [*warn-on-reflection* true
              *err* (java.io.StringWriter.)]
      (clojure.lang.Compiler/load rdr source-path source-name)
      (str *err*))))

(defn parse-warnings
  "Given the output log of reflection warnings,
  Return the [row col] for each warning."
  [warnings]
  (let [lines (str/split-lines warnings)]
    (into []
          (keep (fn [line]
                 (when-let [[_ row col] (re-find #":([0-9]+):([0-9]+) " line)]
                   [(dec (parse-long row)) (parse-long col)])))
          lines)))

(defn editor-next-reflection-warning [editor]
  (let [warnings (-> editor
                     reflection-warnings
                     parse-warnings)
        
        editor-row (-> editor :cursor :row)
        next-warning (util/first-by 
                      (filter (fn [[row col]]
                                (> row editor-row)))
                      warnings)]
    (if-let [[row col] next-warning]
      (text-mode/editor-goto-row-col editor row col)
      editor)))

(def key-bindings
  { ;; "C-M-x" editor-eval-top-form
   "C-M-f" #'editor-paredit-forward
   "C-M-b" #'editor-paredit-backward
   "C-a" #'text-mode/editor-move-beginning-of-line
   "M-m" #'text-mode/editor-back-to-indentation
   "C-u C-d" #'text-mode/editor-delete-char
   "C-d" #'editor-paredit-forward-delete
   "C-e" #'text-mode/editor-move-end-of-line
   "C-f" #'text-mode/editor-forward-char
   "C-b" #'text-mode/editor-backward-char

   "C-j" #'paredit-newline
   "C-k" #'editor-paredit-kill
   "C-l" #'text-mode/editor-recenter-top-bottom
   "C-n" #'text-mode/editor-next-line
   "C-o" #'text-mode/editor-open-line
   "C-p" #'text-mode/editor-previous-line
   "C-v" #'text-mode/editor-scroll-down
   "C-x h" #'text-mode/editor-mark-whole-buffer
   "C-x C-x" #'text-mode/editor-exchange-point-and-mark
   
   "C-c r" #'editor-next-reflection-warning

   "M-d" #'paredit-forward-kill-word
   "M-b" #'editor-backward-word
   "M-f" #'editor-forward-word
   "M-l" #'text-mode/editor-downcase-word
   "M-u" #'text-mode/editor-upcase-word
   "M-c" #'text-mode/editor-capitalize-word
   "M-v" #'text-mode/editor-scroll-up
   
   "M-<" #'text-mode/editor-beginning-of-buffer
   "M->" #'text-mode/editor-end-of-buffer
   "M-(" #'editor-paredit-wrap-round
   "M-s" #'editor-paredit-splice-sexp

   "\"" #'editor-paredit-doublequote
   "[" #'editor-paredit-open-square
   "]" #'editor-paredit-close-square
   "{" #'editor-paredit-open-curly
   "}" #'editor-paredit-close-curly
   "(" #'editor-paredit-open-round
   ")" #'editor-paredit-close-round

   "C-u DEL" #'text-mode/editor-delete-backward-char
   "DEL" #'editor-paredit-backward-delete
   "M-DEL" #'paredit-backward-kill-word
   "RET" #'paredit-newline
   "<right>" #'text-mode/editor-forward-char
   "<up>" #'text-mode/editor-previous-line
   "<down>" #'text-mode/editor-next-line
   "<left>" #'text-mode/editor-backward-char
   "TAB" #'editor-indent
   "C-M-q" #'editor-indent-region

   "C-s" #'text-mode/editor-isearch-forward
   "C-r" #'text-mode/editor-isearch-backward
   "C-y" #'text-mode/editor-yank
   "C-SPC" #'text-mode/editor-set-mark
   "C-u C-SPC" #'text-mode/editor-pop-mark
   "C-w" #'text-mode/editor-kill-region
   "M-w" #'text-mode/editor-save-region
   ;; "C-x C-s" editor-save-buffer
   "M-;" #'editor-toggle-comment

   "M-SPC" #'text-mode/editor-single-space
   "M-\\" #'text-mode/editor-delete-horizontal-space

   ";" #'paredit-semicolon

   "C-c )" #'paredit-forward-slurp-sexp
   "C-c C-)" #'paredit-forward-slurp-sexp
   "C-c }" #'paredit-forward-barf-sexp
   "C-_" #'text-mode/editor-undo

   "M-{" #'text-mode/editor-backward-paragraph
   "M-}" #'text-mode/editor-forward-paragraph

   ,})

(defn make-editor []
  (let [lang (TreeSitterClojure.)]
    {:tree nil
     :cursor {:byte 0
              :char 0
              :point 0
              :row 0
              :column-byte 0}
     :rope Rope/EMPTY
     :language lang
     :parser (doto (TSParser.)
               (.setLanguage lang))
     :buf (byte-array 4096)}))
