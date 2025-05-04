(ns com.phronemophobic.clobber
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
            [clojure.tools.analyzer.jvm :as ana.jvm])
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

(def eval-ns *ns*)
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
  (slurp
   #_(io/resource "com/phronemophobic/easel.clj")
   (io/resource "com/phronemophobic/clobber.clj")))

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




(defn ^:private goto-next-dfs-node
  "Navigates `cursor` to the next node availble in dfs order.

  Returns `true` if another node is available, false otherwise."
  [^TSTreeCursor cursor]
  (cond
    (.gotoFirstChild cursor) true
    (.gotoNextSibling cursor) true

    :else
    (loop []
      (if (.gotoParent cursor)
        (if (.gotoNextSibling cursor)
          true
          (recur))
        false))))

(defn tree-cursor-reducible
  ([cursor]
   (tree-cursor-reducible cursor goto-next-dfs-node))
  ([^TSTreeCursor cursor next-node]
   (reify
     clojure.lang.IReduceInit
     (reduce [this f init]
       (loop [result init]
         (let [node (.currentNode cursor)
               result (f result node)]
           (if (reduced? result)
             @result
             (if (next-node cursor)
               (recur result)
               result))))))))

(defn node-reducible [^TSNode node]
  (tree-cursor-reducible (TSTreeCursor. node)))

(defn tree-reducible
  "Returns an instance of IReduce init for the nodes of tree.

  The (source) node is skipped.

  `next-node`: if provided, should be a function
               that takes a TSTreeCursor and navigates
               to the next node. defaults to `goto-next-dfs-node`."
  ([tree]
   (tree-reducible tree goto-next-dfs-node))
  ([^TSTree tree next-node]
   (let [cursor (TSTreeCursor. (.getRootNode tree))]
     (if (.gotoFirstChild cursor)
       (tree-cursor-reducible cursor next-node)
       (reify
         clojure.lang.IReduceInit
         (reduce [this f init]
           init))))))

(defn first-by [xf coll]
  (transduce xf (completing (fn [_ x] (reduced x))) nil coll))

#_(defn next-named-child-for-byte
  "Depth first search for the first named node that *starts on or after* byte."
  ^TSNode
  [^TSTree tree byte-offset]
  ;; It seems like all tree searches are linear
  ;; so this doesn't seem worse than any other method.
  ;; the builtin methods for TSNode and TSTreeCursor
  ;; seem awkard for this purpose, so we implement our own.
  ;; It's possible that using queries for _ or (_)
  ;; to find unnamed and named nodes respectively
  ;; might be faster, but I'm just assuming they're not.
  (let [;; The docs say
        ;; "You can access every node in a syntax tree using the
        ;;  TSNode APIs described earlier, but if you need to access
        ;;  a large number of nodes, the fastest way to do so is with a tree cursor."
        ;; I'm not sure I believe them, but we'll use a cursor over the TSNode API.
        cursor (TSTreeCursor. (.getRootNode tree))

        node (when (.gotoFirstChild cursor)
               (loop []
                 (let [current-node (.currentNode cursor)]
                   (if (and (>= (-> current-node .getStartByte)
                                byte-offset)
                            (.isNamed current-node))
                     current-node
                     (if (> (-> current-node .getEndByte)
                            byte-offset)
                       (if (.gotoFirstChild cursor)
                         (recur)
                         (when (and (.gotoParent cursor)
                                    (.gotoNextSibling cursor))
                           (recur)))
                       (if (.gotoNextSibling cursor)
                         (recur)
                         (when (and (.gotoParent cursor)
                                    (.gotoNextSibling cursor))
                           (recur))))))))]
    node))

(defn next-named-child-for-byte
  ^TSNode
  [^TSTree tree byte-offset]
  (let [cursor (TSTreeCursor. (.getRootNode tree))]
    (.gotoFirstChild cursor)
    (loop []
      (when (< (.getEndByte (.currentNode cursor))
               byte-offset)
        (.gotoNextSibling cursor)
        (recur)))

    (transduce
     (comp
      (filter (fn [^TSNode node]
                (.isNamed node)))
      (filter (fn [^TSNode node]
                (> (.getEndByte node)
                   byte-offset))))
     (completing
      (fn [^TSNode result ^TSNode node]
        (if (> (.getStartByte node)
               byte-offset)
          (reduced
           (if (and result
                    (or (< (.getEndByte result)
                           (.getEndByte node))
                        (= (.getStartByte result)
                           byte-offset)))
             result
             node))
          node)))
     nil
     (tree-cursor-reducible cursor))))

(defn previous-named-child-for-byte
  ^TSNode
  [^TSTree tree byte-offset]
  (transduce
   (comp
    (filter (fn [^TSNode node]
              (.isNamed node))))
   (completing
    (fn [^TSNode best-match ^TSNode node]
      (if (>= (.getStartByte node)
              byte-offset)
        (reduced best-match)
        node)))
   nil
   (tree-reducible
    tree
    (fn [^TSTreeCursor cursor]
      (if (<= (.getEndByte (.currentNode cursor))
              byte-offset)
        (.gotoNextSibling cursor)
        (goto-next-dfs-node cursor))))))



(def atom-lit-types #{"char_lit" "str_lit" "bool_lit" "nil_lit" "kwd_lit" "num_lit" "sym_lit"})
(defn atom-lit? [^TSNode node]
  (contains? atom-lit-types (.getType node)))

(defn named-child-for-byte
  "Depth first search for the smallest named atom node that contains `byte-offset` 
  or the next named node after byte."
  ^TSNode
  [^TSTree tree byte-offset]
  (transduce
   (filter (fn [^TSNode node]
             (> (-> node .getEndByte)
                byte-offset)))
   (completing
    (fn [^TSNode best-match ^TSNode node]
      (if (>= (-> node .getStartByte)
              byte-offset)
        (if best-match
          (reduced best-match)
          (if (.isNamed node)
            (reduced node)
            nil))
        (if (and (.isNamed node)
                 (atom-lit? node))
          node
          best-match))))
   nil
   (tree-reducible tree)))

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



(defn ^:private add-node-to-paragraph [rope p offset end-byte-offset ^TSNode node base-style style]
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
                              (let [s (node->str rope first-child)]
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
      (let [;; check if small scroll works
            new-start-line (max 0 (- start-line
                                     (quot num-lines 2)))

            ;; if not, just center view
            new-start-line (if (< row new-start-line)
                             (max 0
                                  (- row
                                     (quot num-lines 2)))
                             new-start-line)]
        (assoc-in editor [:viewport :start-line] new-start-line))

      (> row (+ start-line num-lines))
      (let [new-start-line (+ start-line
                              (quot num-lines 2))

            new-start-line (if (> row (+ new-start-line num-lines))
                             (max 0
                                  (- row
                                     (quot num-lines 2)))
                             new-start-line
                             )]
       (assoc-in editor [:viewport :start-line] new-start-line))

      :else editor)))

(declare editor-self-insert-command)
(defn editor-paredit-wrap-round [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor

        ^TSNode
        wrap-node (named-child-for-byte tree cursor-byte)]
    (if wrap-node
      (let [;; first, insert ")" add end of node

            target-byte (.getEndByte wrap-node)
            diff-rope (.sliceBytes rope cursor-byte target-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            point-offset (count-points diff-string)

            new-cursor-row (+ cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (+ (:column point-offset) cursor-column))

            new-tree (when-let [^TSTree
                                tree tree]
                       (let [tree (.copy tree)]
                         (.edit tree (TSInputEdit. target-byte target-byte (inc target-byte)
                                                   (TSPoint. new-cursor-row new-cursor-column)
                                                   (TSPoint. new-cursor-row new-cursor-column)
                                                   (TSPoint. new-cursor-row (inc new-cursor-column))))
                         tree))

            ^Rope
            new-rope (.insert rope ^int (+ cursor-point diff-points) ^CharSequence '")")

            ;; next, insert "(" at start of node
            target-byte (.getStartByte wrap-node)
            diff-rope (.sliceBytes rope cursor-byte target-byte)
            diff-string (.toString diff-rope)
            diff-bytes (- target-byte cursor-byte)
            diff-char (.length diff-string)
            diff-points (.size diff-rope)
            point-offset (count-points diff-string)

            new-cursor-row (+ cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (+ (:column point-offset) cursor-column))

            new-tree (do
                       (.edit new-tree (TSInputEdit. target-byte target-byte (inc target-byte)
                                                     (TSPoint. new-cursor-row new-cursor-column)
                                                     (TSPoint. new-cursor-row new-cursor-column)
                                                     (TSPoint. new-cursor-row (inc new-cursor-column))))
                       new-tree)

            new-rope (.insert new-rope ^int (+ cursor-point diff-points) ^CharSequence '"(")

            reader (->RopeReader new-rope)
            new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )

            new-cursor {:byte (inc (+ cursor-byte diff-bytes))
                        :char (inc (+ cursor-char diff-char))
                        :point (inc (+ cursor-point diff-points))
                        :row new-cursor-row
                        :column (inc new-cursor-column)}]
        (editor-update-viewport
         (assoc editor
                     :tree new-tree
                     :rope new-rope
                     :cursor new-cursor)))
      
      (editor-self-insert-command editor "()"))))

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
                   (tree-reducible tree))]
    (if wrap-node
      (let [;; first, delete ")" add end of node

            target-byte (.getEndByte wrap-node)
            diff-rope (.sliceBytes rope cursor-byte target-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            point-offset (count-points diff-string)

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
            point-offset (count-points diff-string)

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
            
            reader (->RopeReader new-rope)
            new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )

            new-cursor {:byte (dec cursor-byte)
                        :char (dec cursor-char)
                        :point (dec cursor-point)
                        :row cursor-row
                        :column (if (pos? (:row point-offset))
                                  cursor-column
                                  (dec cursor-column))}]
        (editor-update-viewport
         (assoc editor
                :tree new-tree
                :rope new-rope
                :cursor new-cursor)))
      
      editor)))


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
                                   lines 0
                                   ;; only used when hitting
                                   ;; end of buffer
                                   last-line-char nil]
                              (let [next-char (.following bi char-index)]
                                (cond
                                  ;; last line. go to last line char
                                  (= -1 next-char) [lines last-line-char]
                                  (= \newline (.charAt cs char-index)) (let [lines (inc lines)]
                                                                         (if (= lines n)
                                                                           [lines next-char]
                                                                           (recur next-char lines next-char)))
                                  :else (recur next-char lines last-line-char))))

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

(defn editor-paredit-open-coll [editor open-char close-char]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        cs (.toCharSequence rope)
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor]
    (if (or (= (.numBytes rope)
               cursor-byte)
            (not= (.charAt cs cursor-char)
                  open-char))
      (-> editor
          (editor-self-insert-command (str open-char close-char))
          (editor-backward-char))
      (editor-forward-char editor))))

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
        next-node (next-named-child-for-byte tree cursor-byte)]

    (if (not next-node)
      editor
      (let [target-byte (.getEndByte next-node)

            diff-rope (.sliceBytes rope cursor-byte target-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            point-offset (count-points diff-string)

            new-cursor-row (+ cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (+ (:column point-offset) cursor-column))

            new-cursor {:byte target-byte
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point diff-points)
                        :row new-cursor-row
                        :column new-cursor-column}]
        (editor-update-viewport
         (assoc editor
                :cursor new-cursor))))))

(defn editor-paredit-backward [editor]
  (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        previous-node (previous-named-child-for-byte tree cursor-byte)]
    (if (not previous-node)
      editor
      (let [target-byte (.getStartByte previous-node)

            diff-rope (.sliceBytes rope target-byte cursor-byte)
            diff-string (.toString diff-rope)
            diff-points (.size diff-rope)
            point-offset (count-points diff-string)

            new-cursor-row (- cursor-row (:row point-offset))
            new-cursor-column (if (pos? (:row point-offset))
                                (:column point-offset)
                                (- cursor-column (:column point-offset)))

            new-cursor {:byte target-byte
                        :char (- cursor-char (.length diff-string))
                        :point (- cursor-point diff-points)
                        :row new-cursor-row
                        :column new-cursor-column}]
        (editor-update-viewport
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
         (-> rope .toCharSequence (.charAt cursor-char)))
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
                  (let [cs (.toCharSequence rope)
                        bi (doto (BreakIterator/getCharacterInstance)
                             (.setText cs))

                        char-index (loop [char-index cursor-char]
                                     (let [next-char (.following bi char-index)]
                                       (cond
                                         (= -1 next-char) char-index
                                         (= \newline (.charAt cs char-index)) char-index
                                         :else (recur next-char))))]
                    (if (= char-index cursor-char)
                      cursor-byte
                      (let [diff-string (-> (.subSequence cs cursor-char char-index )
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

                reader (->RopeReader new-rope)
                new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]

            (assoc editor
                   :tree new-tree
                   :paragraph nil
                   :rope new-rope)))))))

(defn editor-set-mark-command [editor]
  editor)
(defn editor-kill-region [editor]
  editor)
(defn editor-paredit-backward-delete [editor]
  editor)


(defn editor-paredit-forward-delete [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
    (if (>= (:point cursor) (.size rope))
      editor
      (let [{cursor-byte :byte
             cursor-char :char
             cursor-row :row
             cursor-point :point
             cursor-column :column} cursor
            ;; s (String. (rope->bytes rope) "utf-8")
            cs (.toCharSequence rope)
            current-char (.charAt cs cursor-char)]
        (case current-char
          (\( \[ \{)
          (editor-forward-char editor)
          
          (\) \] \} \")
          (let [ ;; check if empty and delete whole coll

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText cs))

                prev-char-offset (.preceding bi cursor-char)]
            (cond
              (= -1 prev-char-offset) (editor-forward-char editor)

              (= ^char (get {\) \(, \] \[, \} \{, \" \"} current-char)
                 (.charAt cs prev-char-offset))
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
                    reader (->RopeReader new-rope)
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

              :else (editor-forward-char editor)))

          ;; else
          (let [bi (doto (BreakIterator/getCharacterInstance)
                     (.setText cs))
                next-char (.following bi cursor-char)

                diff-string (-> (.subSequence cs cursor-char next-char)
                                .toString)
                diff-bytes (alength (.getBytes diff-string "utf-8"))
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
                   :rope new-rope)))))))




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
              prev-char (.charAt cs prev)]
          (case prev-char
            (\) \] \})
            (editor-backward-char editor)

            (\[ \" \{ \()
            ;; check if empty and delete whole coll
            (cond
              (= (.length cs) cursor-char) (editor-backward-char editor)

              (= ^char (get {\( \), \[ \], \{ \}, \" \"} prev-char)
                 (.charAt cs cursor-char))
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
                    reader (->RopeReader new-rope)
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

              :else (editor-backward-char editor))

            ;;else
            (let [diff-string (-> (.subSequence cs prev cursor-char)
                                  .toString)
                  diff-bytes (alength (.getBytes diff-string "utf-8"))

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
                      :rope new-rope))))))))


(defn editor-goto-line [editor n]
  editor)

(defn editor-end-of-buffer [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        ;; could probably be more efficient
        {:keys [row column]} (count-points (.toString rope))]
    (editor-update-viewport
     (assoc editor
            :cursor {:byte (.numBytes rope)
                     :char (-> rope .toCharSequence .length)
                     :point (-> rope .size)
                     :row row
                     :column column}))))

(defn editor-beginning-of-buffer [editor]
  (editor-update-viewport
   (assoc editor
          :cursor {:byte 0
                   :char 0
                   :point 0
                   :row 0
                   :column 0})))
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


(defui line-val-view [{:keys [line-val editor para]}]
  (let [{:keys [tree rope base-style viscous?]} editor]
    (into []
          (map (fn [[line val]]
                 (let [byte-start-index (find-byte-offset-for-line tree rope line)
                       byte-end-index (find-byte-offset-for-line tree rope (inc line))
                       char-offset (get para :char-offset 0)
                       rects (para/get-rects-for-range para
                                                       (- (byte-index->char-index rope byte-start-index)
                                                          char-offset)
                                                       (- (byte-index->char-index rope byte-end-index)
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
                           (ui/translate (+ x 10)  y 
                                         (-> (make-editor)
                                             (assoc :base-style base-style)
                                             (editor-self-insert-command
                                              "=> ")
                                             (editor-self-insert-command
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


(defeffect ::init-search-forward [{:keys [$editor]}]
  (dispatch! :update $editor
             (fn [editor]
               (assoc editor
                      ::search {:initial-cursor (:cursor editor)
                                :initial-rope (:rope editor)}))))

(defeffect ::cancel-search-forward [{:keys [$editor]}]
  (dispatch! :update $editor
             (fn [editor]
               (let [initial-cursor (-> editor ::search :initial-cursor)
                     editor (if initial-cursor
                              (assoc editor :cursor initial-cursor)
                              editor)]
                 (editor-update-viewport
                  (dissoc editor ::search))))))

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
        cs (.toCharSequence rope)
        matcher (.matcher regexp cs)

        match (if (.find matcher search-index)
                (.start matcher)
                (when (.find matcher 0)
                  (.start matcher)))]
    (if match
      (let [ ;; calculate new cursor
            s (-> (.subSequence cs 0 match)
                  .toString)

            {:keys [row column]} (count-points s)

            cursor {:byte (alength (.getBytes s "utf-8"))
                    :char (.length s)
                    :point (num-points s)
                    :row row
                    :column column}]
        (-> editor
            (assoc-in [::search :query] query)
            (assoc :cursor cursor)
            (editor-update-viewport)))
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
        cs (.toCharSequence rope)
        matcher (.matcher regexp cs)

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
            s (-> (.subSequence cs 0 match)
                  .toString)

            {:keys [row column]} (count-points s)

            cursor {:byte (alength (.getBytes s "utf-8"))
                    :char (.length s)
                    :point (num-points s)
                    :row row
                    :column column}]
        (-> editor
            (assoc-in [::search :query] query)
            (assoc :cursor cursor)
            (editor-update-viewport)))
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
               (ui/wrap-on
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
                            (binding [*ns* eval-ns]
                              (loop [bindings {}
                                     line-vals {}]
                                (let [node (.currentNode cursor)
                                      s (node->str rope node)
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
               (ui/on
                :key-event
                (fn [key scancode action mods]
                  (when (#{:press :repeat} action)
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
            result-node (named-child-for-byte tree cursor-byte)
            result-str (node->str rope result-node )
            result-form (binding [*ns* eval-ns]
                          (read-string result-str))
            
            forms (when (.gotoFirstChild cursor)
                    (binding [*ns* eval-ns]
                      (loop [forms []]
                        (let [node (.currentNode cursor)]
                          (if (< (.getEndByte node)
                                 cursor-byte)
                            (let [node-str (node->str rope node)
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
                           editor-move-end-of-line
                           (editor-self-insert-command "\n")
                           (editor-indent)
                           (editor-self-insert-command
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
                cs (.toCharSequence rope)

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText cs))

                char-index (if (zero? affinity)
                             (.preceding bi char-index)
                             char-index)
              
                byte-index (let [s (.toString (.subSequence cs 0 char-index))
                                 bs (.getBytes s "utf-8")]

                             (alength bs))
                byte-index (+ byte-index (:start-byte-offset para))


                ^TSTree
                tree (:tree editor)
                root-node (.getRootNode tree)
                node (.getNamedDescendantForByteRange root-node byte-index byte-index)]
            (when node
              (let [node-str (node->str (:rope editor) node)
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
                cs (.toCharSequence rope)

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText cs))

                char-index (if (zero? affinity)
                             (.preceding bi char-index)
                             char-index)
              
                byte-index (let [s (.toString (.subSequence cs 0 char-index))
                                 bs (.getBytes s "utf-8")]

                             (alength bs))
                byte-index (+ byte-index (:start-byte-offset para))


                ^TSTree
                tree (:tree editor)
                root-node (.getRootNode tree)
                node (.getNamedDescendantForByteRange root-node byte-index byte-index)]
            (when (= "sym_name" (.getType node))
              (let [node-str (node->str (:rope editor) node)
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
        viscous? (get extra :viscous?)
        instarepl? (get extra :instarepl?)
        structure? (get extra :structure?)

        $editor $editor
        editor (-> editor
                   (assoc :viscous? viscous?)
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
                                   (-> (make-editor clojure-lang)
                                       (editor-self-insert-command "\n\n\n\n\n\n")
                                       (assoc :cursor {:byte 0
                                                       :char 0
                                                       :point 0
                                                       :row 0
                                                       :column 0})
                                       (dissoc :structure-state )
                                       (editor-update-viewport))
                                   ]])})
        (ant/button {:text  "load"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor clojure-lang)
                                       (editor-self-insert-command
                                        (slurp (io/resource "com/phronemophobic/clobber.clj")))
                                       (assoc :cursor {:byte 0
                                                       :char 0
                                                       :point 0
                                                       :row 0
                                                       :column 0})
                                       (editor-update-viewport))
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


(comment
  (dev/add-component-as-applet #'debug
                               {:editor (-> (make-editor clojure-lang)
                                            #_(editor-self-insert-command
                                               #_(slurp (io/resource "com/phronemophobic/easel.clj"))
                                               ";; break break break
;; break break break
;; break break break
;; break break break")
                                            (editor-self-insert-command
                                             (slurp (io/resource "com/phronemophobic/clobber.clj")))
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
  ([s query]
   (com.phronemophobic.clobber/query s query nil nil))
  ([s query start-byte-offset]
   (com.phronemophobic.clobber/query s query start-byte-offset nil))
  ([s query start-byte-offset end-byte-offset]
   (let [rope (Rope/from s)
         qc (TSQueryCursor.)
         q (TSQuery. clojure-lang query)
         ^TSTree
         tree (parse clojure-lang s)
         _ (when start-byte-offset
             (let [end-byte-offset (or end-byte-offset (.numBytes rope))]
               (.setByteRange qc start-byte-offset end-byte-offset)))
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
                           :named (.isNamed node)
                           :type (.getType node)
                           :s (node->str rope node)}]
           (recur (conj ret match-info)))
         ;; else
         ret)))))


