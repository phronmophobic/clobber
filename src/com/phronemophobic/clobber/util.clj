(ns com.phronemophobic.clobber.util
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
           com.ibm.icu.text.BreakIterator
           java.nio.charset.Charset
           java.util.Arrays
           ;; java.text.BreakIterator
           java.nio.ByteBuffer
           java.util.regex.Pattern
           io.lacuna.bifurcan.Rope
           ))

(comment
  (require 'virgil)
  (defonce recompile (virgil/compile-java ["/Users/adrian/workspace/bifurcan/src"]))
  ,)

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
       :start-point (d/datafy (.getStartPoint node))
       :end-point (d/datafy (.getEndPoint node))
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

(defn dtap [& args]
  (future (Thread/sleep 20)
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

(defn num-bytes [^String s]
  (-> s (.getBytes "utf-8") alength))

(defn count-points
  "Counts the number or rows and columns of s.

  returns {:row row :column column}."
  [^CharSequence s]
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

(defn count-grapheme-clusters
  "Counts the number of grapheme clusters in s."
  [^CharSequence s]
  (let [bi (doto (BreakIterator/getCharacterInstance)
             (.setText s)
             (.first))]
    (loop [cnt 0]
      (let [end (.next bi)]
        (if (not= end BreakIterator/DONE)
          (recur (inc cnt) )
          cnt)))))



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
  ([s]
   (parse (TreeSitterClojure.) s))
  ([lang s]
   (let [parser (doto (TSParser.)
                  (.setLanguage lang))

         reader (->RopeReader (Rope/from s))
         buf (byte-array 256)
         tree (.parse parser buf nil reader TSInputEncoding/TSInputEncodingUTF8)]
     tree)))

(defn rope->str
  "Converts `r` to string from bytes offsets.

  Assumes `r` contains `byte-start` and `byte-end`."
  [^Rope r ^long byte-start ^long byte-end]
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
                  copy-bb  (.slice src  offset copy-size)]
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
  (let [rope (.sliceBytes rope 0 byte-index)]
    (.length rope)))

(defn goto-next-dfs-node
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

(defn skip-to-byte-offset
  "Navigates `cursor` past all top level nodes
  that end before `byte-offset`.

  Returns `true` if navigation is successful, falsy otherwise."
  [cursor byte-offset]
  (and (.gotoFirstChild cursor)
       (loop []
         (if (< (.getEndByte (.currentNode cursor))
                byte-offset)
           (when (.gotoNextSibling cursor)
             (recur))
           true))))

(defn next-named-child-for-byte
  ^TSNode
  [^TSTree tree byte-offset]
  (let [cursor (TSTreeCursor. (.getRootNode tree))]
    (when (skip-to-byte-offset cursor byte-offset)
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
       (tree-cursor-reducible cursor)))))

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
  (let [cursor (TSTreeCursor. (.getRootNode tree))]
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
     (when (skip-to-byte-offset cursor byte-offset)
       (tree-cursor-reducible cursor)))))

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
              rope (.sliceBytes rope 0 byte-offset)
              bi (doto (BreakIterator/getCharacterInstance)
                   (.setText rope))

              char-index (loop [char-index (.last bi)
                                line line]
                           (let [prev-char-index (.preceding bi char-index)]

                             (if (= (.charAt rope prev-char-index)
                                    \newline)
                               (if (= line target-line)
                                 char-index
                                 (recur prev-char-index
                                        (dec line)))
                               (recur prev-char-index
                                      line))
                             ))
              byte-offset (- byte-offset
                             (-> (.subSequence rope char-index (.length rope))
                                 (.toString)
                                 (.getBytes "utf-8")
                                 alength))]
          byte-offset)))))


#_(defn ^:private query
    "Simple helper for testing tree sitter queries."
    ([s query]
     (com.phronemophobic.clobber.util/query s query nil nil))
    ([s query start-byte-offset]
     (com.phronemophobic.clobber.util/query s query start-byte-offset nil))
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
