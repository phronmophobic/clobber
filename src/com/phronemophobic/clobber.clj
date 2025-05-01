(ns com.phronemophobic.clobber
  (:require [clojure.java.io :as io]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [ropes.core :as ropes]
            [com.phronemophobic.viscous :as viscous])
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
           ;; java.text.BreakIterator
           java.nio.ByteBuffer
           io.lacuna.bifurcan.Rope))

(import 'com.ibm.icu.text.BreakIterator)
(require 'virgil)
(defonce recompile (virgil/compile-java ["/Users/adrian/workspace/bifurcan/src"]))
;; https://lacuna.io/docs/bifurcan/io/lacuna/bifurcan/Rope.html

(def coll-node-types #{"map_lit" "list_lit" "set_lit" "vec_lit"})

(defn dtap [& args]
  (future (Thread/sleep 100)
          (if (= (count args)
                 1)
            (tap> (first args))
            (tap> args))))

(defn num-points [^String s]
  (let [len (.length s)]
    (loop [i 0
           point-count 0]
      (if (< i len)
        (if (Character/isHighSurrogate (.charAt s i))
          (recur (inc i) point-count)
          (recur (inc i) (inc point-count)))
        point-count))))

(extend-protocol p/Datafiable
  TSTree
  (datafy [^TSTree tree]
    {:root-node (.getRootNode tree)
     :language (.getLanguage tree)})
  TSNode
  (datafy [^TSNode node]
    (if (.isNull node)
      {:tree (.getTree node)
       :null? (.isNull node)}
      {:tree (.getTree node)
       :child-count (.getChildCount node)
       :named-child-count (.getNamedChildCount node)
       :type (.getType node)
       :symbol (.getSymbol node)
       :null? (.isNull node)
       :named? (.isNamed node)
       :missing? (.isMissing node)
       :extra? (.isExtra node)
       :error? (.hasError node)
       :start-byte (.getStartByte node)
       :end-byte (.getEndByte node)
       :start-point (.getStartPoint node)
       :end-point (.getEndPoint node)
       :parent (.getParent node)}))
  TSPoint
  (datafy [^TSPoint point]
    {:row (.getRow point)
     :column (.getColumn point)})
  TSQueryMatch
  (datafy [^TSQueryMatch match]
    {:id (.getId match)
     :pattern-index (.getPatternIndex match)
     :capture-index (.getCaptureIndex match)
     :capture (aget (.getCaptures match) (.getCaptureIndex match))
     :captures (.getCaptures match)})
  ;; TSQuery
  ;; (datafy [^TSQuery query]
  ;;   {:})
  TSQueryCapture
  (datafy [^TSQueryCapture capture]
    {:node (.getNode capture)
     :index (.getIndex capture)}))


(def s "👻👩‍👩‍👦‍👦")
#_(def s
  #_(pr-str
   '(do "asdf"
        1 2 #{}
        (def foo 42)))
  (slurp (io/resource "com/phronemophobic/easel.clj")))

(defonce clojure-lang (TreeSitterClojure.))
(defonce json-lang (TreeSitterJson.))
(defonce parser (doto (TSParser.)
              (.setLanguage clojure-lang)))


(defrecord StringReader [^String s]
  TSReader
  (read [this buf offset position]
    (if (>= offset (.length s) )
      0
      (let [bb (ByteBuffer/wrap buf)]
        (.put bb (.getBytes s))
        (.length s)))))

(defrecord ByteArrayReader [^bytes ba]
  TSReader
  (read [this buf offset position]
    (if (>= offset (alength ba))
      0
      (let [len (min (- (alength ba) offset) (alength buf))]
        (System/arraycopy ba offset buf 0 len)
        len))))

(defrecord RopeReader [^Rope r]
  TSReader
  (read [this buf offset position]
    (let [dest (ByteBuffer/wrap buf)
          bytes-read
          (loop [iter (.bytes r)
                 offset offset
                 bytes-read 0]
            (let [dest-remaining (.remaining dest)]
              (if (pos? dest-remaining)
                (if (.hasNext iter)
                  (let [src ^ByteBuffer (.next iter)
                        src-remaining (.remaining src)]
                    (if (< offset src-remaining)
                      (let [copy-size (min dest-remaining (- src-remaining offset))
                            copy-bb (.slice src offset copy-size)]
                        (.put dest copy-bb)
                        (recur iter 0 copy-size))
                      ;; else
                      (recur iter (- offset src-remaining) bytes-read)))
                  ;; no more buffers from iter
                  bytes-read)
                ;; no space left in buf
                bytes-read)))]

      bytes-read)))

(defn parse
  "Debug function. don't use."
  [lang s]
  (let [parser (doto (TSParser.)
                 (.setLanguage lang))

        reader (->RopeReader (Rope/from s))
        buf (byte-array 256)
        tree (.parse parser buf nil reader TSInputEncoding/TSInputEncodingUTF8)]
    tree))

       ;; final AtomicBoolean edited = new AtomicBoolean(false);
       ;;  parser.reset();
       ;;  byte[] buf = new byte[1024];
       ;;  String newJsonSrc = "[1, null, 4]";
       ;;  TSReader reader = new TSReader() {
       ;;      @Override
       ;;      public int read(byte[] buf, int offset, TSPoint position) {
       ;;          ByteBuffer byteBuffer = ByteBuffer.wrap(buf);
       ;;          if(edited.get()){
       ;;              if(offset >= newJsonSrc.length()){
       ;;                  return 0;
       ;;              }

       ;;              byteBuffer.put(newJsonSrc.getBytes());
       ;;              return newJsonSrc.length();
       ;;          }else {
       ;;              if(offset >= JSON_SRC.length()){
       ;;                  return 0;
       ;;              }
       ;;              ByteBuffer charBuffer = ByteBuffer.wrap(buf);
       ;;              charBuffer.put(JSON_SRC.getBytes());
       ;;              return JSON_SRC.length();
       ;;          }
       ;;      }
       ;;  };
       ;;  tree = parser.parse(buf, null, reader, TSInputEncoding.TSInputEncodingUTF8);
       ;;  assertEquals(1, tree.getRootNode().getChildCount());
       ;;  assertEquals(2, tree.getRootNode().getNamedChild(0).getNamedChildCount());
       ;;  int editStart = 0;
       ;;  int editEnd = 1;
       ;;  tree.edit(new TSInputEdit(editStart, editStart, editEnd,
       ;;          new TSPoint(0, editStart), new TSPoint(0, editStart), new TSPoint(0, editEnd)));
       ;;  edited.set(true);
       ;;  TSTree newTree = parser.parse(buf, tree, reader, TSInputEncoding.TSInputEncodingUTF8);
       ;;  TSRange[] ranges = TSTree.getChangedRanges(tree, newTree);
       ;;  assertTrue(ranges.length > 0);
       ;;  assertEquals(12, ranges[0].getEndByte());


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

(def json-highlight-queries
  ;; tree-sitter-clojure/queries/highlights.scm
  (slurp "../tree-sitter-json/queries/highlights.scm" )
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

;; Tree sitter uses these strings
;; and requires they not be garbage collected
;; and they do not retain a strong reference.
;; tldr: make sure these queries don't get garbage collected
;;       or redefined
(defonce lang->highlight-queries
  {clojure-lang highlight-queries
   json-lang json-highlight-queries})

(def base-style #:text-style {:font-families ["Menlo"]
                              :font-size 12})
(defn bb-put [^ByteBuffer buffer ^ByteBuffer array] (.put buffer array))

(defn rope->str
  "Converts `r` to string from bytes offsets.

  Assumes `r` contains `byte-start` and `byte-end`."
  [^Rope r byte-start byte-end]
  (let [to-read (- byte-end byte-start)
        dest (ByteBuffer/allocate to-read)]
    (loop [iter (.bytes r)
           offset byte-start
           to-read to-read]
      (when (pos? to-read)
        (let [src ^ByteBuffer (.next iter)
              src-remaining (.remaining src)]
          (if (< offset src-remaining)
            (let [copy-size (min to-read (- src-remaining offset))
                  copy-bb  (.slice src ^int offset ^int copy-size)]
              (.put dest copy-bb)
              (recur iter 0 (- to-read copy-size)))
            ;; else
            (recur iter (- offset src-remaining) to-read)))))
    (String. (.array dest) "utf-8")))

(defn node->str
  "Extracts the TSNode `node` as a string from the corresponding Rope."
  [^Rope rope ^TSNode  node]
  (-> (.sliceBytes rope
                   (.getStartByte node)
                   (.getEndByte node))
      .toString))

(defn byte-index->char-index [^Rope rope byte-index]
  (let [rope (.sliceBytes rope 0 byte-index)
        cs (.toCharSequence rope)]
    (.length cs)))



(defn find-byte-offset-for-line [^TSTree tree ^Rope rope target-line]
  (if (zero? target-line)
    0
    (let [ ;; find first node past line
          root-node (.getRootNode tree)
          cursor (TSTreeCursor. root-node)

          point (TSPoint. target-line 0)
          ^TSNode
          node
          (loop [node root-node]
            (let [idx (.gotoFirstChildForPoint cursor point)]
              (if (= -1 idx)
                ;; loop through children to see if any are past
                ;; current point
                (let [child (when (.gotoFirstChild cursor)
                              (loop []
                                (let [child (.currentNode cursor)
                                      child-end-row (-> child
                                                        .getEndPoint
                                                        .getRow)]
                                  (if (>= child-end-row target-line)
                                    child
                                    (when (.gotoNextSibling cursor)
                                      (recur))))))]
                  (or child node))
                (recur (.currentNode cursor)))))

          [byte-offset line] (let [start-row (.getRow (.getStartPoint node))]
                               (if (>= start-row target-line)
                                 [(.getStartByte node)
                                  start-row]
                                 [(.getEndByte node)
                                  (.getRow (.getEndPoint node))]))]
      (if (> target-line line)
        (.numBytes rope)
        (let [;; now backtrack
              cs (.toCharSequence (.sliceBytes rope 0 byte-offset))
              bi (doto (BreakIterator/getCharacterInstance)
                   (.setText cs))

              char-index (loop [char-index (.last bi)
                                line line]
                           (let [prev-char-index (.preceding bi char-index)]

                             (if (= (.charAt cs prev-char-index)
                                    \newline)
                               (if (= line target-line)
                                 char-index
                                 (recur prev-char-index
                                        (dec line)))
                               (recur prev-char-index
                                      line))
                             ))
              byte-offset (- byte-offset
                             (-> (.subSequence cs char-index (.length cs))
                                 (.toString)
                                 (.getBytes "utf-8")
                                 alength))]
          byte-offset)))))



(defn ^:private add-node-to-paragraph [rope p offset end-byte-offset ^TSNode node style]
  (let [;; add any unmatched text as unadorned
        start-byte (min end-byte-offset (.getStartByte node))
        end-byte (min end-byte-offset (.getEndByte node))

        p (if (> start-byte offset)
            (conj p
                  (rope->str rope offset start-byte))
            p)

        ;; add matched text
        ;; ; matches can overlap
        p (if (and (> end-byte offset)
                   (> end-byte start-byte))
            (let [color (get text-colors style)
                  chunk-text (rope->str rope (max start-byte offset) end-byte)
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
                              (let [s (node->str rope first-child)]
                                (if (#{"def" "defn" "let" "recur" "if" "when" "loop" "and" "or" "doto" "defrecord" "extend-protocol" "defonce" "defprotocol" "defmulti" "defmethod"} s)
                                  (let [[p end-byte] (add-node-to-paragraph rope p offset end-byte-offset first-child "defn")]
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
                                          (rope->str rope offset start-byte))
                                    p)

                                ;; add matched text
                                ;; ; matches can overlap
                                p (if (> end-byte offset)
                                    (let [color (get text-colors capture-name)
                                          chunk-text (rope->str rope (max start-byte offset) end-byte)
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
                                      (rope->str rope offset end-byte-offset))
                                p)]
                        p)))
        ]
    paragraph))



(comment

  (dev/add-component-as-applet #'debug {})
  ,)


(defprotocol ICodeEditor
  (forward-char [_])
  (backward-char [_])
  (forward-word [_])
  (backward-word [_])
  (forward-paragraph [_])
  (backward-paragraph [_])
  (move-beginning-of-line [_])
  (move-end-of-line [_])
  (next-line [_])
  (previous-line [_])

  (cider-eval-defun-at-point [_])
  (cider-eval-last-sexp [_])

  (paredit-forward [_])
  (paredit-backward [_])
  (paredit-open-square [_])
  (paredit-close-square [_])
  (paredit-open-round [_])
  (paredit-close-round [_])
  (paredit-open-curly [_])
  (paredit-close-curly [_])
  (paredit-kill [_])

  (set-mark-command [_])
  (kill-region [_])
  ;; backspace
  (paredit-backward-delete [_])
  ;; normal typing
  (self-insert-command [_ s])
  (goto-line [_ n])
  (end-of-buffer [_])
  (beginning-of-buffer [_])
  (isearch-forward [_])
  (kill-ring-save [_])

  )

(defn count-points
  "Counts the number or rows and columns of s.

  returns {:row row :column column}."
  [^String s]
  (let [bi (doto (BreakIterator/getCharacterInstance)
             (.setText s))]
    (loop [row 0
           column 0
           start (.first bi)]
      (let [end (.next bi)]
        (if (not= end BreakIterator/DONE)
          (let [newline? (and (= 1 (- end start))
                              (= (.charAt s start) \newline))]
            (if newline?
              (recur (inc row)
                     0
                     end)
              (recur row
                     (inc column)
                     end)))
          {:row row
           :column column})))))

(defn editor-update-viewport [editor]
  (let [row (-> editor :cursor :row)
        {:keys [start-line num-lines]} (:viewport editor)]
    (cond

      (< row start-line)
      (assoc-in editor [:viewport :start-line]
                (max 0 (- start-line
                          (quot num-lines 2))))

      (> row (+ start-line num-lines))
      (assoc-in editor
                [:viewport :start-line]
                (+ start-line
                   (quot num-lines 2)))

      :else editor)))

(defn editor-recenter-top-bottom [editor]
  (let [row (-> editor :cursor :row)
        {:keys [start-line num-lines]} (:viewport editor)

        middle (max 0
                    (- row
                       (quot num-lines 2)))
        top row
        bottom (max 0
                    (inc (- row num-lines)))]

    (cond
      ;; middle, go to top
      (= start-line middle)
      (assoc-in editor
                [:viewport :start-line]
                top)


      ;; at top, go to bottom
      (= start-line top)
      (assoc-in editor
                [:viewport :start-line]
                bottom)

      ;; goto middle
      :else
      (assoc-in editor
                [:viewport :start-line]
                middle))))

(defn editor-forward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        next-char (.following bi cursor-char)]
    (if (= -1 next-char)
      editor
      (let [diff-string (-> (.subSequence cs cursor-char next-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (+ cursor-char (.length diff-string))
                         :point (+ cursor-point (num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))



(defn editor-backward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence cs prev-char cursor-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (dec cursor-row)
                             cursor-row)

            new-cursor-column (if newline?
                                (loop [column 0
                                       index (.previous bi)]
                                  (if (>= index 0)
                                    (let [c (.charAt cs index)]
                                      (if (= \newline c)
                                        column
                                        (recur (inc column)
                                               (.previous bi))))
                                    column))
                                (dec cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (- cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (- cursor-char (.length diff-string))
                         :point (- cursor-point (num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))

(defn editor-forward-word [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getWordInstance)
             (.setText cs))

        next-char (.following bi cursor-char)]
    (if (= -1 next-char)
      editor
      (let [diff-string (-> (.subSequence cs cursor-char next-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (+ cursor-char (.length diff-string))
                         :point (+ cursor-point (num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))
(defn editor-backward-word [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getWordInstance)
             (.setText cs))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence cs prev-char cursor-char )
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (- cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (- cursor-char (.length diff-string))
                         :point (- cursor-point (num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))
(defn editor-forward-paragraph [editor]
  editor)
(defn editor-backward-paragraph [editor]
  editor)
(defn editor-move-beginning-of-line [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        char-index (loop [char-index cursor-char]
                     (let [prev-char (.preceding bi char-index)]
                       (if (or (= -1 prev-char)
                               (= \newline (.charAt cs prev-char)))
                         char-index
                         (recur prev-char))))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence cs char-index cursor-char)
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))
            new-cursor-column (- cursor-column num-bytes)]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (- cursor-byte num-bytes)
                         :char (- cursor-char (.length diff-string))
                         :point (- cursor-point (num-points diff-string))
                         :row cursor-row
                         :column new-cursor-column})))))
  )
(defn editor-move-end-of-line [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        char-index (loop [char-index cursor-char]
                     (let [next-char (.following bi char-index)]
                       (cond
                         (= -1 next-char) char-index
                         (= \newline (.charAt cs char-index)) char-index
                         :else (recur next-char))))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence cs cursor-char char-index )
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))
            new-cursor-column (+ cursor-column num-bytes)]
        (assoc editor
               :cursor {:byte (+ cursor-byte num-bytes)
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point (num-points diff-string))
                        :row cursor-row
                        :column new-cursor-column})))))

(defn editor-previous-line
  ([editor]
   (editor-previous-line editor 1))
  ([editor n]
   (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-target-column :target-column
          cursor-column :column} cursor

         target-column (or cursor-target-column
                           cursor-column)

         cs (.toCharSequence rope)
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText cs))

         ;; find previous newline twice
         [lines char-index] (loop [char-index cursor-char
                                   lines 0]
                              (let [prev-char (.preceding bi char-index)]
                                (cond
                                  (= -1 prev-char) [lines char-index]
                                  (= \newline (.charAt cs prev-char)) (if (= n lines)
                                                                        [lines char-index]
                                                                        (recur prev-char (inc lines)))
                                  :else (recur prev-char lines))))

         ;; keep going until target column
         [char-index column]
         (loop [char-index char-index
                column 0]
           (let [next-char (.following bi char-index)]
             (cond
               (= -1 next-char) [char-index column]
               (= \newline (.charAt cs char-index)) [char-index column]
               (= column target-column) [char-index column]
               :else (recur next-char (inc column)))))]
     (if (= char-index cursor-char)
       editor
       (let [diff-string (-> (.subSequence cs char-index cursor-char)
                             .toString)
             num-bytes (alength (.getBytes diff-string "utf-8"))
             new-cursor-row (- cursor-row lines)]
         (editor-update-viewport
          (assoc editor
                 :cursor {:byte (- cursor-byte num-bytes)
                          :char (- cursor-char (.length diff-string))
                          :point (- cursor-point (num-points diff-string))
                          :row new-cursor-row
                          :target-column target-column
                          :column column})))))))

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

              cs (.toCharSequence rope)

              cs (.toCharSequence rope)
              bi (doto (BreakIterator/getCharacterInstance)
                   (.setText cs))

              ;; 1. find start of line
              char-index
              (loop [idx cursor-char]
                (let [prev-idx (.preceding bi idx)]
                  (if (or (= -1 prev-idx)
                          (= \newline (.charAt cs prev-idx)))
                    idx
                    (recur prev-idx))))

              
              diff-string (.toString (.subSequence cs char-index cursor-char))
              
              byte-index (- cursor-byte (alength (.getBytes diff-string "utf-8")))
              point-index (- cursor-point (num-points diff-string))

              ;; move forward until newline or non whitespace
              num-spaces (loop [idx char-index]
                           (let [c (.charAt cs idx)]
                             (if (= \space c)
                               (recur (inc idx))
                               (- idx char-index))))

              
              

              ;; measure indent in bytes for now
              indent-diff (- indent num-spaces)
]
          (if (zero? indent-diff)
            ;; need to also move cursor, even if
            ;; spaces aren't added or removed
            editor
            (let [
                  
                  new-rope (if (pos? indent-diff)
                             (.concat (.sliceBytes rope 0 byte-index)
                                      (.concat (Rope/from (str/join (repeat indent-diff " ")))
                                               (.sliceBytes rope byte-index (.size rope))))

                             ;; else 
                             (.concat (.sliceBytes rope 0 byte-index)
                                      (.sliceBytes rope
                                                   (- byte-index indent-diff)
                                                   (.size rope))))

                  
                  
                  
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
                  reader (->RopeReader new-rope)
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

(def eval-ns *ns*)
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
          ;; do indent
          (try
            (let [node (.currentNode cursor)
                  line-number (-> node
                                  .getEndPoint
                                  .getRow)]
              (binding [*ns* eval-ns]
                (let [form (read-string (node->str rope node))
                      val (eval form)]
                  (dispatch! :update $editor
                             update :line-val
                             (fn [m]
                               (let [line-val (get m rope)]
                                 {rope (assoc line-val line-number (viscous/wrap val))}))))))
            (catch Exception e
              (prn e))))))))






(defn editor-self-insert-command [editor ^String s]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        ;; cursor is { :byte, :row, :column }
        sbytes (.getBytes s "utf-8")
        
        point-offset (count-points s)
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        new-cursor-row (+ cursor-row (:row point-offset))
        new-cursor-column (if (pos? (:row point-offset))
                            (:column point-offset)
                            (+ (:column point-offset) cursor-column))


        new-tree (when-let [^TSTree
                            tree tree]
                   (let [tree (.copy tree)]
                     (.edit tree (TSInputEdit. cursor-byte cursor-byte (+ cursor-byte (alength sbytes))
                                               (TSPoint. cursor-row cursor-column)
                                               (TSPoint. cursor-row cursor-column)
                                               (TSPoint. new-cursor-row new-cursor-column)))
                     tree))

        new-rope (.insert rope ^int cursor-point s)
        
        reader (->RopeReader new-rope)
        new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
    (editor-update-viewport
     (assoc editor
            :tree new-tree
            :cursor {:byte (+ cursor-byte (alength sbytes))
                     :char (+ cursor-char (.length s))
                     :point (+ cursor-point (num-points s))
                     :row new-cursor-row
                     :column new-cursor-column}
            :paragraph nil
            :rope new-rope))))

(defn editor-open-line [editor]
  (-> editor
      (editor-self-insert-command "\n")
      (assoc :cursor (:cursor editor))))

(defn editor-next-line
  ([editor]
   (editor-next-line editor 1))
  ([editor n]
   (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-target-column :target-column
          cursor-column :column} cursor

         target-column (or cursor-target-column
                           cursor-column)

         cs (.toCharSequence rope)
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText cs))

         ;; find newline
         [lines char-index] (loop [char-index cursor-char
                                   lines 0]
                              (let [next-char (.following bi char-index)]
                                (cond
                                  ;; last line. don't move
                                  (= -1 next-char) [lines cursor-char]
                                  (= \newline (.charAt cs char-index)) (let [lines (inc lines)]
                                                                         (if (= lines n)
                                                                           [lines next-char]
                                                                           (recur next-char lines)))
                                  :else (recur next-char lines))))

         ;; keep going until target column
         [char-index column]
         (if (= cursor-char char-index)
           [char-index cursor-column]
           (loop [char-index char-index
                  column 0]
             (let [next-char (.following bi char-index)]
               (cond

                 (= -1 next-char) [char-index column]
                 (= \newline (.charAt cs char-index)) [char-index column]
                 (= column target-column) [char-index column]
                 :else (recur next-char (inc column))))))]
     (if (= char-index cursor-char)
       editor
       (let [diff-string (-> (.subSequence cs cursor-char char-index )
                             .toString)
             num-bytes (alength (.getBytes diff-string "utf-8"))
             new-cursor-column (+ cursor-column num-bytes)
             new-cursor-row (+ cursor-row lines)]
         (editor-update-viewport
          (assoc editor
                 :cursor {:byte (+ cursor-byte num-bytes)
                          :char (+ cursor-char (.length diff-string))
                          :point (+ cursor-point (num-points diff-string))
                          :row new-cursor-row
                          :target-column target-column
                          :column column})))))))

(defn editor-scroll-down [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-next-line editor to-scroll)))

(defn editor-scroll-up [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-previous-line editor to-scroll)))


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
                point-offset (count-points diff-string)

                new-cursor-row (+ cursor-row (:row point-offset))
                new-cursor-column (if (pos? (:row point-offset))
                                    (:column point-offset)
                                    (+ (:column point-offset) cursor-column))]
            (editor-update-viewport
             (assoc editor
                    :cursor {:byte (+ cursor-byte diff-bytes)
                             :char (+ cursor-char diff-char)
                             :point (+ cursor-point diff-points)
                             :row new-cursor-row
                             :column new-cursor-column}))))))))

(defn editor-cider-eval-defun-at-point [editor]
  editor)
(defn editor-cider-eval-last-sexp [editor]
  editor)
(defn editor-paredit-forward [editor]
  editor)
(defn editor-paredit-backward [editor]
  editor)

(defn editor-paredit-doublequote [editor]
  ;; need to check if inside string
  (-> editor
      (editor-self-insert-command "\"\"")
      (editor-backward-char)))
(defn editor-paredit-open-square [editor]
  ;; need to check if inside string
  (-> editor
      (editor-self-insert-command "[]")
      (editor-backward-char)))
(defn editor-paredit-close-square [editor]
  (editor-paredit-close-coll editor \]))
(defn editor-paredit-open-round [editor]
  (-> editor
      (editor-self-insert-command "()")
      (editor-backward-char)))
(defn editor-paredit-close-round [editor]
  (editor-paredit-close-coll editor \)))
(defn editor-paredit-open-curly [editor]
  (-> editor
      (editor-self-insert-command "{}")
      (editor-backward-char)))
(defn editor-paredit-close-curly [editor]
  (editor-paredit-close-coll editor \}))


(defn editor-paredit-kill [editor]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor]
    (if (= cursor-byte (.size rope))
      editor
      (if false ;; (= close-char (.nth rope cursor-point))
        (assoc editor
               :cursor {:byte (+ cursor-byte 1)
                        :char (+ cursor-char 1)
                        :point (+ cursor-point 1)
                        :row cursor-row
                        :column (+ cursor-column 1)})
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
                  point-offset (count-points diff-string)

                  new-cursor-row (+ cursor-row (:row point-offset))
                  new-cursor-column (if (pos? (:row point-offset))
                                      (:column point-offset)
                                      (+ (:column point-offset) cursor-column))]
              (assoc editor
                     :cursor {:byte (+ cursor-byte diff-bytes)
                              :char (+ cursor-char diff-char)
                              :point (+ cursor-point diff-points)
                              :row new-cursor-row
                              :column new-cursor-column}))))))))
(defn editor-set-mark-command [editor]
  editor)
(defn editor-kill-region [editor]
  editor)
(defn editor-paredit-backward-delete [editor]
  editor)

(defn editor-paredit-forward-delete [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
    (if (>= (:byte cursor) (.size rope))
      editor
      (let [{cursor-byte :byte
             cursor-char :char
             cursor-row :row
             cursor-point :point
             cursor-column :column} cursor
            ;; s (String. (rope->bytes rope) "utf-8")
            cs (.toCharSequence rope)
            bi (doto (BreakIterator/getCharacterInstance)
                 (.setText cs))
            next-char (.following bi cursor-char)
            
            diff-string (-> (.subSequence cs cursor-char next-char)
                            .toString)
            diff-bytes (alength (.getBytes diff-string "utf-8"))
            ;; prev-byte (- cursor-byte (alength (.getBytes (subs s prev cursor-char) "utf-8")))
            next-byte (+ cursor-byte diff-bytes)
            next-point (+ cursor-point (num-points diff-string))

            newline? (= \newline (.charAt cs cursor-char))

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
            reader (->RopeReader new-rope)
            new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
        (assoc editor
               :tree new-tree
               :cursor cursor
               :paragraph nil
               :rope new-rope)))))





(defn editor-delete-backward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
      (if (<= (:byte cursor) 0)
        editor
        (let [{cursor-byte :byte
               cursor-char :char
               cursor-row :row
               cursor-point :point
               cursor-column :column} cursor
              ;; s (String. (rope->bytes rope) "utf-8")
              cs (.toCharSequence rope)
              bi (doto (BreakIterator/getCharacterInstance)
                   (.setText cs))
              prev (.preceding bi cursor-char)
              diff-string (-> (.subSequence cs prev cursor-char)
                              .toString)
              diff-bytes (alength (.getBytes diff-string "utf-8"))
              ;; prev-byte (- cursor-byte (alength (.getBytes (subs s prev cursor-char) "utf-8")))
              prev-byte (- cursor-byte diff-bytes)
              prev-char (.charAt cs prev)
              prev-point (- cursor-point (num-points diff-string))
              newline? (= prev-char \newline)
            

              new-cursor-row (if newline?
                               (dec cursor-row)
                               cursor-row)
              new-cursor-column (if newline?
                                  (loop [n 0]
                                    (let [start (.previous bi)]
                                      (if (or (= BreakIterator/DONE start)
                                              (= \newline (.charAt cs start)))
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

              reader (->RopeReader new-rope)
              new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
          (editor-update-viewport
           (assoc editor
                  :tree new-tree
                  :cursor {:byte prev-byte
                           :char prev
                           :point prev-point
                           :row new-cursor-row
                           :column new-cursor-column}
                  :paragraph nil
                  :rope new-rope))))))


(defn editor-goto-line [editor n]
  editor)
(defn editor-end-of-buffer [editor]
  editor)
(defn editor-beginning-of-buffer [editor]
  editor)
(defn editor-isearch-forward [editor]
  editor)
(defn editor-kill-ring-save [editor]
  editor)

(defrecord Editor [tree cursor paragraph rope buf]
  ICodeEditor
  (forward-char [this]
    (editor-forward-char this))
  (backward-char [this]
    (editor-backward-char this))
  (forward-word [this]
    (editor-forward-word this))
  (backward-word [this]
    (editor-backward-word this))
  (forward-paragraph [this]
    (editor-forward-paragraph this))
  (backward-paragraph [this]
    (editor-backward-paragraph this))
  (next-line [this]
    (editor-next-line this))
  (previous-line [this]
    (editor-previous-line this))
  (move-beginning-of-line [this]
    (editor-move-beginning-of-line this))
  (move-end-of-line [this]
    (editor-move-end-of-line this))
  (cider-eval-defun-at-point [this]
    (editor-cider-eval-defun-at-point this))
  (cider-eval-last-sexp [this]
    (editor-cider-eval-last-sexp this))
  (paredit-forward [this]
    (editor-paredit-forward this))
  (paredit-backward [this]
    (editor-paredit-backward this))
  (paredit-open-square [this]
    (editor-paredit-open-square this))
  (paredit-close-square [this]
    (editor-paredit-close-square this))
  (paredit-open-round [this]
    (editor-paredit-open-round this))
  (paredit-close-round [this]
    (editor-paredit-close-round this))
  (paredit-open-curly [this]
    (editor-paredit-open-curly this))
  (paredit-close-curly [this]
    (editor-paredit-close-curly this))
  (paredit-kill [this]
    (editor-paredit-kill this))
  (set-mark-command [this]
    (editor-set-mark-command this))
  (kill-region [this]
    (editor-kill-region this))
  (paredit-backward-delete [this]
    (editor-paredit-backward-delete this))
  (self-insert-command [this s]
    (editor-self-insert-command this s))
  (goto-line [this n]
    (editor-goto-line this n))
  (end-of-buffer [this]
    (editor-end-of-buffer this))
  (beginning-of-buffer [this]
    (editor-beginning-of-buffer this))
  (isearch-forward [this]
    (editor-isearch-forward this))
  (kill-ring-save [this]
    (editor-kill-ring-save this)))

(defn make-editor
  ([]
   (make-editor clojure-lang))
  ([lang]
   (map->Editor
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
     :rope Rope/EMPTY
     :language lang
     :parser (doto (TSParser.)
               (.setLanguage lang))
     :buf (byte-array 4096)})))

(comment

  (def editor
    (-> (make-editor clojure-lang)
        (self-insert-command "hi")
        (editor-delete-backward-char)
        (self-insert-command "hi")
        (editor-delete-backward-char)
        ;;tap>
        ))
  (highlighted-text (TSQueryCursor.)
                    (TSQuery. clojure-lang highlight-queries)
                    base-style
                    (:tree editor)
                    (:rope editor))
  ,)


(comment
  (require '[membrane.ui :as ui]
           '[membrane.skia.paragraph :as para])
  
  (dev/add-component-as-applet (fn [o]
                                 (para/paragraph (:s o)))
                               {:s s})

  ,)
(defn cursor-view [^Rope rope para cursor]
  (let [cursor-char (- (:char cursor)
                       (get para :char-offset 0))
        rope (.sliceBytes rope
                          (:start-byte-offset para)
                          (:end-byte-offset para))
        cs (.toCharSequence rope)
        {:keys [x y width height] :as rect}
        (cond

          (zero? (.size rope))
          (first
           (para/get-rects-for-range (assoc para :paragraph " ")
                                     0 1
                                     :max
                                     :tight))

          (>= cursor-char (.length cs))
          (first
           (para/get-rects-for-range (assoc para :paragraph [(:paragraph para)
                                                             " "])
                                     cursor-char (inc cursor-char)
                                     :max
                                     :tight))

          :else
          (let [
                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText cs))

                next-char (.following bi cursor-char)
                diff-string (-> (.subSequence cs cursor-char next-char)
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
        
        start-byte-offset (find-byte-offset-for-line tree rope start-line)
        end-byte-offset (find-byte-offset-for-line tree rope end-line)
        char-offset (-> (.sliceBytes rope 0 start-byte-offset)
                        .toCharSequence
                        .length)
        para (para/paragraph (highlighted-text (TSQueryCursor.)
                                               (TSQuery. lang
                                                         (lang->highlight-queries lang))
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


(defui line-val-view [{:keys [tree rope para line-val]}]
  (into []
        (map (fn [[line val]]
               (let [byte-start-index (find-byte-offset-for-line tree rope line)
                     byte-end-index (find-byte-offset-for-line tree rope (inc line))
                     char-offset (get para :char-offset 0)
                     rect (first
                           (para/get-rects-for-range para
                                                     (- (byte-index->char-index rope byte-start-index)
                                                        char-offset)
                                                     (- (byte-index->char-index rope byte-end-index)
                                                        char-offset)
                                                     :max
                                                     :tight))]
                 (when rect
                   (let [{:keys [x y width height]} rect
                         offset 4]
                     
                     #_(ui/translate (- (+ x width 10) offset) (- y offset)
                                   (let [inspector-extra (get extra [::inspector [line val]])]
                                     (ui/vertical-layout
                                      (viscous/inspector
                                       {:obj val
                                        :width (get inspector-extra :width 40)
                                        :height (get inspector-extra :height 1)
                                        :show-context? (get inspector-extra :show-context?)
                                        :extra inspector-extra})))
                                   #_(-> (make-editor)
                                       (editor-self-insert-command
                                        "=> ")
                                       (editor-self-insert-command
                                        (pr-str val))
                                       (editor->paragraph)))
                     (ui/translate (+ x width 10)  y 
                                   (-> (make-editor)
                                       (editor-self-insert-command
                                        "=> ")
                                       (editor-self-insert-command
                                        (pr-str @val))
                                       (editor->paragraph))))))))
        line-val))


(defui editor-view [{:keys [editor]}]
  (when-let [tree (:tree editor)]
    (let [lang (:language editor)
          rope (:rope editor)
          para (editor->paragraph editor)
          line-vals (when-let [line-val (-> editor :line-val (get rope))]
                      (line-val-view
                       {:tree tree
                        :rope rope
                        :para para
                        :line-val line-val}))]
      [(cursor-view rope para (:cursor editor))
       para
       line-vals])))



(defui code-editor [{:keys [editor
                            ^:membrane.component/contextual
                             focus]
                     :as this}]
  (let [body (editor-view {:editor editor})
        focused? (= $editor focus)
        body (if focused?
               (ui/on
                :key-event
                (fn [key scancode action mods]
                  (when (#{:press :repeat} action)
                   (let [ ;; ctrl? (not (zero? (bit-and 2r10 mods)))
                         alt? (not (zero? (bit-and ui/ALT-MASK mods)))
                         super? (not (zero? (bit-and ui/SUPER-MASK mods)))
                         shift? (not (zero? (bit-and ui/SHIFT-MASK mods)))
                         ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
                     (cond

                       (and ctrl?
                            alt?)
                       (case (char key)
                         \X
                         [[::editor-eval-top-form this]]

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
                         

                         ;; else
                         nil
                         )

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
                (fn [s]
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
                      (do (tap> s)
                          nil)))
                  )
                body)
               [(ui/filled-rectangle [1 0 1 0.1]
                                     100 100)
                body])

        body (ui/on
              :mouse-down
              (fn [_]
                [[:set $focus $editor]])
              body)

        structure-state (get extra :structure-state)]
    (ui/vertical-layout
     (ui/label (pr-str (:cursor editor)))
     #_[(ui/spacer 100 100)
      (ui/label structure-state)]
     (ui/on
      :mouse-move
      (fn [[mx my]]
        [[::do-structure {:editor editor
                          :x mx
                          :y my
                          :$structure-state $structure-state}]])
      body))))

(defeffect ::do-structure [{:keys [editor x y $structure-state]}]
  (future
    (when (pos? (.size (:rope editor)))
      (let [para (editor->paragraph editor)
            [char-index affinity] (para/glyph-position-at-coordinate para x y)
            cs (.toCharSequence (:rope editor))
            bi (doto (BreakIterator/getCharacterInstance)
                 (.setText cs))
            char-index (if (zero? affinity)
                         (.following cs char-index)
                         char-index)
            
            byte-index (let [s (.toString (.subSequence cs 0 char-index))
                             bs (.getBytes s "utf-8")]
                         (alength bs))

            rope (:rope editor)
            tree (:tree editor)
            root-node (.getRootNode tree)
            node (.getNamedDescendantForByteRange root-node byte-index byte-index)
            node-start-byte (.getStartByte node)
            node-end-byte (.getEndByte node)
            node-str (.toString (.sliceBytes rope node-start-byte node-end-byte))]
        (when (not= node-end-byte (.size rope))
         (dispatch! :set $structure-state node-str))))))

(defui debug [{:keys [editor]}]
  (ui/vertical-layout
   (ui/button "tap>"
              (fn []
                [[::tap editor]]))
   (code-editor {:editor editor})))


(comment
  (dev/add-component-as-applet #'debug
                               {:editor (-> (make-editor clojure-lang)
                                            #_(editor-self-insert-command
                                               #_(slurp (io/resource "com/phronemophobic/easel.clj"))
                                               ";; break break break
;; break break break
;; break break break
;; break break break")
                                            (editor-self-insert-command s)
                                            (assoc :cursor {:byte 0
                                                            :char 0
                                                            :point 0
                                                            :row 0
                                                            :column 0})
                                            (editor-update-viewport))})


  (require '[clj-async-profiler.core :as prof])

  (let [editor (-> (make-editor clojure-lang)
                   (self-insert-command s))]
    (quick-bench
     (-> editor
         ;;(editor-delete-backward-char)
         (self-insert-command "\nfoo"))))

  ;; Profile the following expression:
  (let [editor (-> (make-editor clojure-lang)
                   (self-insert-command s))]
    (prof/profile
     (dotimes [i 50]
       (let []
         (-> editor
             ;;(editor-delete-backward-char)
             (self-insert-command "\nfoo"))
            
         nil))))

  (let [s (slurp "/var/tmp/rodo.json")
        lines (str/split-lines s)
        s (str/join "\n" (take 1000 lines))
        
        editor (-> (make-editor json-lang)
                   (self-insert-command s))]
    (let []
      (quick-bench
       (let [editor (-> editor
                        ;;(editor-delete-backward-char)
                        (self-insert-command "\nfoo"))]
         #_(editor-view editor)))
      
      nil))

  (prof/profile (dotimes [i 50]
                  (let [editor (-> (make-editor clojure-lang)
                                   (self-insert-command "42"))]
                    
                    
                    nil)))


  (prof/serve-ui 8080)                  ; Serve on port 8080


  ,)




;; BreakIterator requires java Character sequences
;; https://docs.oracle.com/en/java/javase/23/docs/api/java.base/java/lang/Character.html#unicode
;; The char data type (and therefore the value that a Character object encapsulates) are based on the original Unicode specification, which defined characters as fixed-width 16-bit entities. The Unicode Standard has since been changed to allow for characters whose representation requires more than 16 bits. 

;; A char value, therefore, represents Basic Multilingual Plane (BMP) code points, including the surrogate code points, or code units of the UTF-16 encoding. An int value represents all Unicode code points, including supplementary code points.



(comment
  (quick-bench
      (let [editor (-> (make-editor clojure-lang)
                       (editor-self-insert-command
                        (slurp (io/resource "com/phronemophobic/easel.clj"))))
            tree (:tree editor)
            lang (:language editor)]
        (para/paragraph (highlighted-text (TSQueryCursor.)
                                          (TSQuery. lang
                                                    (lang->highlight-queries lang))
                                          (:base-style editor)
                                          tree
                                          (:rope editor))
                        nil
                        {:paragraph-style/text-style (:base-style editor)})))
  

  (def mybi (doto (BreakIterator/getCharacterInstance)
              (.setText (-> (Rope/from s)
                            .toCharSequence))))
  (map #(.preceding mybi %) (range 10))

  (let [s (slurp (io/resource "com/phronemophobic/easel.clj"))
        editor (-> (make-editor clojure-lang)
                   (editor-self-insert-command s))]
    (quick-bench
        (-> (editor-delete-backward-char editor))))
  
  ,)

(defeffect ::tap [& args]
  (case (count args)
    0 nil
    1 (tap> (first args))
    2 (tap> (vec args))))


(defn ^:private query
  "Simple helper for testing tree sitter queries."
  [s query]
  (let [rope (Rope/from s)
        qc (TSQueryCursor.)
        q (TSQuery. clojure-lang query)
        ^TSTree
        tree (parse clojure-lang s)
        _ (.exec qc q (.getRootNode tree))
        matches (.getCaptures qc)]
    (loop [ret []
           ]
      (if (.hasNext matches)
        (let [match (.next matches)
              ^TSQueryCapture
              capture (aget (.getCaptures match) (.getCaptureIndex match))
              capture-name (.getCaptureNameForId q (.getIndex capture))
              node (.getNode capture)

              match-info {:name capture-name
                          :s (node->str rope node)}]
          (recur (conj ret match-info)))
        ;; else
        ret))))

(comment
  (query "(defn foo [] )"
           "((list_lit
.
(sym_lit) @call_name
)



        ) @funcall

(vec_lit) @vec

   ")
  (first (:arglists (meta #'defn )))
  [name doc-string? attr-map? [params*] prepost-map? body]
  )


