(ns com.phronemophobic.clobber.modes.clojure
  (:require [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util :as util]
            [clojure.string :as str])
  (:import (org.treesitter TSTree
                           TSInputEdit
                           TSInputEncoding
                           TSTreeCursor
                           TSParser
                           TSPoint
                           TSNode)
           com.ibm.icu.text.BreakIterator))

#_(defonce recompile (virgil/compile-java ["/Users/adrian/workspace/bifurcan/src"]))
(import 'io.lacuna.bifurcan.Rope)

(def coll-node-types #{"map_lit" "list_lit" "set_lit" "vec_lit"})




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

   ;; json
   "string.special.key" [0.46666666865348816 0.0 0.5333333611488342]
   
   ;; "operator"
   })


(defn editor-paredit-wrap-round [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor

        ^TSNode
        wrap-node (util/named-child-for-byte tree cursor-byte)]
    (if wrap-node
      (let [;; first, insert ")" add end of node
            editor (text-mode/editor-insert editor ")" (.getEndByte wrap-node))

            ;; next goto and insert "("
            target-byte (.getStartByte wrap-node)
            editor (text-mode/editor-goto-byte editor target-byte)
            editor (text-mode/editor-self-insert-command editor "(")]
        (text-mode/editor-update-viewport editor))
      
      (text-mode/editor-self-insert-command editor "()"))))

(defn editor-paredit-splice-sexp [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor

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
            point-offset (util/count-points diff-string)

            new-cursor-row (+ cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (+ (:column point-offset) cursor-column))

            new-tree (when-let [^TSTree
                                tree tree]
                       (let [tree (.copy tree)]
                         (.edit tree (TSInputEdit. (dec target-byte) target-byte (dec target-byte)
                                                   (TSPoint. new-cursor-row (dec new-cursor-column))
                                                   (TSPoint. new-cursor-row new-cursor-column)
                                                   (TSPoint. new-cursor-row (dec new-cursor-column))))
                         tree))

            ^Rope
            new-rope (.concat (.sliceBytes rope 0 (dec target-byte))
                              (.sliceBytes rope target-byte (.numBytes rope)))

            ;; next, delete "(" at start of node
            target-byte (.getStartByte wrap-node)
            diff-rope (.sliceBytes rope target-byte cursor-byte)
            diff-string (.toString diff-rope)
            ;; diff-bytes (- cursor-byte target-byte)
            ;; diff-char (.length diff-string)
            ;; diff-points (.size diff-rope)
            point-offset (util/count-points diff-string)

            new-cursor-row (- cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (- cursor-column (:column point-offset)))

            new-tree (do
                       (.edit new-tree (TSInputEdit. target-byte (inc target-byte) target-byte 
                                                     (TSPoint. new-cursor-row new-cursor-column)
                                                     (TSPoint. new-cursor-row (inc new-cursor-column))
                                                     (TSPoint. new-cursor-row new-cursor-column)
                                                     ))
                       new-tree)

            new-rope (.concat (.sliceBytes new-rope 0 target-byte)
                              (.sliceBytes new-rope (inc target-byte) (.numBytes new-rope)))
            
            reader (util/->RopeReader new-rope)
            new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )

            new-cursor {:byte (dec cursor-byte)
                        :char (dec cursor-char)
                        :point (dec cursor-point)
                        :row cursor-row
                        :column (if (pos? (:row point-offset))
                                  cursor-column
                                  (dec cursor-column))}]
        (text-mode/editor-update-viewport
         (assoc editor
                :tree new-tree
                :rope new-rope
                :cursor new-cursor)))
      
      editor)))


(defn editor-indent [editor]
  ;; Find enclosing coll literal
  ;; if enclosing coll is on the same line, keep climbing
  ;; 
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor]
    (let [root-node (.getRootNode tree)
          node (.getDescendantForByteRange root-node cursor-byte cursor-byte)
          ^TSNode
          parent-coll-node
          (loop [node node]
            (if (.isNull node)
              nil
              (if (contains? coll-node-types (.getType node))
                (let [node-row (-> node
                                   .getStartPoint
                                   .getRow)]
                  
                  (if (not= node-row cursor-row)
                    (do
                      node)
                    (recur (.getParent node))))
                (recur (.getParent node)))))]
      
      (if (not parent-coll-node)
        editor
        ;; do indent
        (let [relative-indent (if (= "list_lit"
                                     (.getType parent-coll-node))
                                2
                                1)
              indent (+ relative-indent
                        (-> parent-coll-node
                            .getStartPoint
                            .getColumn))

              ;; This can be optimized
              ;; do the straightforward, naive thing for now

              ;; 1. find start of line
              ;; 2. move forward to indent
              ;; 3. add or remove spaces until
              ;;    first non-whitespace

              bi (doto (BreakIterator/getCharacterInstance)
                   (.setText rope))

              ;; 1. find start of line
              char-index
              (loop [idx cursor-char]
                (let [prev-idx (.preceding bi idx)]
                  (if (or (= -1 prev-idx)
                          (= \newline (.charAt rope prev-idx)))
                    idx
                    (recur prev-idx))))

              
              diff-string (.toString (.subSequence rope char-index cursor-char))
              
              byte-index (- cursor-byte (alength (.getBytes diff-string "utf-8")))
              point-index (- cursor-point (util/num-points diff-string))

              ;; move forward until newline or non whitespace
              num-spaces (loop [idx char-index]
                           (let [c (.charAt rope idx)]
                             (if (= \space c)
                               (recur (inc idx))
                               (- idx char-index))))

              
              

              ;; measure indent in bytes for now
              indent-diff (- indent num-spaces)
              ]
          (if (zero? indent-diff)
            ;; need to also move cursor, even if
            ;; spaces aren't added or removed
            (let [new-cursor (if (<= cursor-column num-spaces) 
                               {:byte (+ byte-index indent)
                                :char (+ char-index indent)
                                :point (+ point-index indent)
                                :row cursor-row
                                :column indent}
                               cursor)]
              (assoc editor
                     :cursor new-cursor))
            (let [new-rope (if (pos? indent-diff)
                             ;; this should be .insert?
                             (.concat (.sliceBytes rope 0 byte-index)
                                      (.concat (Rope/from (str/join (repeat indent-diff " ")))
                                               (.sliceBytes rope byte-index (.numBytes rope))))

                             ;; else 
                             (.concat (.sliceBytes rope 0 byte-index)
                                      (.sliceBytes rope
                                                   (- byte-index indent-diff)
                                                   (.numBytes rope))))

                  
                  
                  
                  new-tree (when-let [^TSTree tree tree]
                             (let [tree (.copy tree)
                                   input-edit (if (pos? indent-diff)
                                                (TSInputEdit. byte-index byte-index (+ byte-index indent-diff)
                                                              (TSPoint. cursor-row 0)
                                                              (TSPoint. cursor-row 0)
                                                              (TSPoint. cursor-row indent-diff))
                                                (TSInputEdit. byte-index (- byte-index indent-diff) byte-index
                                                              (TSPoint. cursor-row 0)
                                                              (TSPoint. cursor-row (- indent-diff))
                                                              (TSPoint. cursor-row 0)))]
                               (.edit tree input-edit)
                               tree))
                  reader (util/->RopeReader new-rope)
                  new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )

                  new-cursor (if (<= cursor-column num-spaces) 
                               {:byte (+ byte-index indent)
                                :char (+ char-index indent)
                                :point (+ point-index indent)
                                :row cursor-row
                                :column indent}
                               {:byte (+ cursor-byte indent-diff)
                                :char (+ cursor-char indent-diff)
                                :point (+ cursor-point indent-diff)
                                :row cursor-row
                                :column (+ cursor-column indent-diff)})]
              (assoc editor
                     :tree new-tree
                     :paragraph nil
                     :rope new-rope
                     :cursor new-cursor)))
          )
        ))))

(defn editor-paredit-close-coll [editor close-char]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor]
    (if (= cursor-byte (.size rope))
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
          (let [end-byte (.getEndByte parent-coll-node)
                diff-rope (.sliceBytes rope cursor-byte end-byte)
                diff-string (.toString diff-rope)
                diff-bytes (- end-byte cursor-byte)
                diff-char (.length diff-string)
                diff-points (.size diff-rope)
                point-offset (util/count-points diff-string)

                new-cursor-row (+ cursor-row (:row point-offset))
                new-cursor-column (if (pos? (:row point-offset))
                                    (:column point-offset)
                                    (+ (:column point-offset) cursor-column))]
            (text-mode/editor-update-viewport
             (assoc editor
                    :cursor {:byte (+ cursor-byte diff-bytes)
                             :char (+ cursor-char diff-char)
                             :point (+ cursor-point diff-points)
                             :row new-cursor-row
                             :column new-cursor-column}))))))))

(defn editor-paredit-open-coll [editor open-char close-char]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor]
    (if (or (= (.numBytes rope)
               cursor-byte)
            (not= (.charAt rope cursor-char)
                  open-char))
      (-> editor
          (text-mode/editor-self-insert-command (str open-char close-char))
          (text-mode/editor-backward-char))
      (text-mode/editor-forward-char editor))))

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
         cursor-column :column} cursor
        next-node (util/next-named-child-for-byte tree cursor-byte)]

    (if (not next-node)
      editor
      (let [target-byte (.getEndByte next-node)

            diff-rope (.sliceBytes rope cursor-byte target-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            point-offset (util/count-points diff-string)

            new-cursor-row (+ cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (+ (:column point-offset) cursor-column))

            new-cursor {:byte target-byte
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point diff-points)
                        :row new-cursor-row
                        :column new-cursor-column}]
        (text-mode/editor-update-viewport
         (assoc editor
                :cursor new-cursor))))))

(defn editor-paredit-backward [editor]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        previous-node (util/previous-named-child-for-byte tree cursor-byte)]
    (if (not previous-node)
      editor
      (let [target-byte (.getStartByte previous-node)

            diff-rope (.sliceBytes rope target-byte cursor-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            point-offset (util/count-points diff-string)

            new-cursor-row (- cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (- cursor-column (:column point-offset)))

            new-cursor {:byte target-byte
                        :char (- cursor-char (.length diff-string))
                        :point (- cursor-point diff-points)
                        :row new-cursor-row
                        :column new-cursor-column}]
        (text-mode/editor-update-viewport
         (assoc editor
                :cursor new-cursor))))))

(defn editor-paredit-doublequote [editor]
  (editor-paredit-open-coll editor \" \"))

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
         cursor-column :column} cursor]
    (cond
      (>= (:point cursor) (.size rope)) editor

      ;; check if we're at the end of line
      (= \newline
         (-> rope (.charAt cursor-char)))
      (editor-paredit-forward-delete editor)

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

                        char-index (loop [char-index cursor-char]
                                     (let [next-char (.following bi char-index)]
                                       (cond
                                         (= -1 next-char) char-index
                                         (= \newline (.charAt rope char-index)) char-index
                                         :else (recur next-char))))]
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
                                                       (TSPoint. cursor-row cursor-column)
                                                       (TSPoint. cursor-row (+ cursor-column
                                                                               (- end-byte-offset
                                                                                  cursor-byte)))
                                                       (TSPoint. cursor-row cursor-column)))
                             tree))

                new-rope (.concat (.sliceBytes rope 0 cursor-byte)
                                  (.sliceBytes rope end-byte-offset (.numBytes rope)))

                reader (util/->RopeReader new-rope)
                new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]

            (assoc editor
                   :tree new-tree
                   :paragraph nil
                   :rope new-rope)))))))



(defn editor-paredit-forward-delete [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
    (if (>= (:point cursor) (.size rope))
      editor
      (let [{cursor-byte :byte
             cursor-char :char
             cursor-row :row
             cursor-point :point
             cursor-column :column} cursor
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
                                                           (TSPoint. cursor-row (dec cursor-column))
                                                           (TSPoint. cursor-row (inc cursor-column))
                                                           (TSPoint. cursor-row (dec cursor-column))))
                                 tree))

                    new-rope (.concat (.slice rope 0 (dec cursor-point))
                                      (.slice rope (inc cursor-point) (.size rope)))
                    reader (util/->RopeReader new-rope)
                    new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8)

                    new-cursor {:byte (dec cursor-byte)
                                :char (dec cursor-char)
                                :point (dec cursor-point)
                                :row cursor-row
                                :column (dec cursor-column)}]
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
                                                       (TSPoint. cursor-row cursor-column)
                                                       (if newline?
                                                         (TSPoint. (inc cursor-row) 0)
                                                         (TSPoint. cursor-row (inc cursor-column)))
                                                       (TSPoint. cursor-row cursor-column)))
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
             cursor-column :column} cursor
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
                                                         (TSPoint. cursor-row (dec cursor-column))
                                                         (TSPoint. cursor-row (inc cursor-column))
                                                         (TSPoint. cursor-row (dec cursor-column))))
                               tree))

                  new-rope (.concat (.slice rope 0 (dec cursor-point))
                                    (.slice rope (inc cursor-point) (.size rope)))
                  reader (util/->RopeReader new-rope)
                  new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8)

                  new-cursor {:byte (dec cursor-byte)
                              :char (dec cursor-char)
                              :point (dec cursor-point)
                              :row cursor-row
                              :column (dec cursor-column)}]
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
                newline? (= prev-char \newline)
                

                new-cursor-row (if newline?
                                 (dec cursor-row)
                                 cursor-row)
                new-cursor-column (if newline?
                                    (loop [n 0]
                                      (let [start (.previous bi)]
                                        (if (or (= BreakIterator/DONE start)
                                                (= \newline (.charAt rope start)))
                                          n
                                          (recur (inc n)))))
                                    (dec cursor-column))


                new-tree (when-let [^TSTree tree tree]
                           (let [tree (.copy tree)]
                             (.edit tree (TSInputEdit. cursor-byte cursor-byte prev-byte
                                                       (TSPoint. cursor-row cursor-column)
                                                       (TSPoint. cursor-row cursor-column)
                                                       (TSPoint. new-cursor-row new-cursor-column)))
                             tree))

                new-rope (.concat (.slice rope 0 prev-point)
                                  (.slice rope cursor-point (.size rope)))

                reader (util/->RopeReader new-rope)
                new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
            (text-mode/editor-update-viewport
             (assoc editor
                    :tree new-tree
                    :cursor {:byte prev-byte
                             :char prev
                             :point prev-point
                             :row new-cursor-row
                             :column new-cursor-column}
                    :paragraph nil
                    :rope new-rope))))))))

(defn paredit-newline [editor]
  (-> editor
      (text-mode/editor-self-insert-command "\n")
      (editor-indent)))



(def key-bindings
  { ;; "C-M-x" editor-eval-top-form
   "C-M-f" #'editor-paredit-forward
   "C-M-b" #'editor-paredit-backward
   "C-a" #'text-mode/editor-move-beginning-of-line
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


   "M-b" #'text-mode/editor-backward-word
   "M-f" #'text-mode/editor-forward-word
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
   "RET" #'paredit-newline
   "<right>" #'text-mode/editor-forward-char
   "<up>" #'text-mode/editor-previous-line
   "<down>" #'text-mode/editor-next-line
   "<left>" #'text-mode/editor-backward-char
   "TAB" #'editor-indent

   "C-c a" identity
   "C-c b" identity

   ;; "C-x C-s" editor-save-buffer

   })

