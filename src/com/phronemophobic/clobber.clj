(ns com.phronemophobic.clobber
  (:require [clojure.java.io :as io]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [ropes.core :as ropes])
  (:import (org.treesitter TSLanguage
                           TSQuery
                           TSParser
                           TSTree
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
           java.util.Arrays
           java.text.BreakIterator
           java.nio.ByteBuffer))

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

(ropes/rope [1 2 3])

(def clojure-lang (TreeSitterClojure.))
(def json-lang (TreeSitterJson.))
(def parser (doto (TSParser.)
              (.setLanguage clojure-lang)))

(def s
  #_(pr-str
   '(do "asdf"
        1 2 #{}
        (def foo 42)))
  (slurp (io/resource "com/phronemophobic/easel.clj")))



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

(defrecord RopeReader [r]
  TSReader
  (read [this buf offset position]
    (let [rcount (count r)]
      (if (>= offset rcount)
        0
        (let [bytes-read (min (alength buf)
                              (- rcount offset))]
          (loop [n 0]
            (if (< n bytes-read)
              
              (do
                (aset ^bytes buf n ^byte (nth r (+ n offset)))
                (recur (inc n)))
              bytes-read)))))))

(defn parse
  "Debug function. don't use."
  [lang bs]
  (let [parser (doto (TSParser.)
                 (.setLanguage lang))

        reader (->RopeReader (ropes/rope bs))
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
  ";; Literals\n\n(num_lit) @number\n\n[\n  (char_lit)\n  (str_lit)\n] @string\n\n[\n (bool_lit)\n (nil_lit)\n] @constant.builtin\n\n(kwd_lit) @constant\n\n;; Comments\n\n(comment) @comment\n\n;; Treat quasiquotation as operators for the purpose of highlighting.\n\n[\n \"'\"\n \"`\"\n \"~\"\n \"@\"\n \"~@\"\n] @operator\n"
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

   ;; json
   "string.special.key" [0.46666666865348816 0.0 0.5333333611488342]
   
   ;; "operator"
   })
(def lang->highlight-queries
  {clojure-lang highlight-queries
   json-lang json-highlight-queries})

(def base-style #:text-style {:font-families ["Menlo"]
                              :font-size 12})

(defn rope->str [r start end]
  #_(String.
   (byte-array
    (eduction
     (map #(nth r %))
     (range start end))
    
    ;; ropes/view seems to be slightly broken
    #_(ropes/view rope offset start-byte))
   "utf-8")

  (String.
   (byte-array
    (ropes/view r start end))
   "utf-8"))

(defn highlighted-text [
                        ;;lang highlight-queries
                        ^TSQueryCursor qc
                        ^TSQuery query
                        base-style ^TSTree tree rope]
  (let [
        ;; query (TSQuery. lang highlight-queries)
        ;; qc (TSQueryCursor.)

        ;; _ (.setByteRange qc 35 (alength bs))
        _ (.exec qc query (.getRootNode tree))
        matches (.getCaptures qc)
        paragraph (loop [p []
                         offset 0
                         matches matches]
                    (if (.hasNext matches)
                      (let [match (.next matches)
                            ^TSQueryCapture
                            capture (aget (.getCaptures match) (.getCaptureIndex match))
                            capture-name (.getCaptureNameForId query (.getIndex capture))
                            node (.getNode capture)

                            ;; add any unmatched text as unadorned
                            start-byte (.getStartByte node)
                            end-byte (.getEndByte node)

                            p (if (> start-byte offset)
                                (conj p
                                      (rope->str rope offset start-byte))
                                p)

                            ;; add matched text
                            ;;;; matches can overlap
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
                               matches))
                      ;; else
                      (let [p (if (< offset (count rope))
                                (conj p
                                      (rope->str rope offset (count rope)))
                                p)]
                        p)))
        ]
    paragraph))


(defui debug [{}]
  #_(let [hover (get extra ::hover-info)
        p (para/paragraph [{:placeholder {:width 10 :height 0}}
                           (highlighted-text json-lang
                                             json-highlight-queries
                                             (str "#" s "#" ))]
                          nil
                          #:paragraph-style {:text-style #:text-style {:font-families ["Menlo"]}})]
    [
     (ui/label hover)
     (ui/translate
      0 100
      (ui/on
       :mouse-move
       (fn [[mx my]]
         [[:set $hover (para/glyph-position-at-coordinate p mx my)]])
       p))]))

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

(defn editor-forward-char [editor]
  editor)
(defn editor-backward-char [editor]
  editor)
(defn editor-forward-word [editor]
  editor)
(defn editor-backward-word [editor]
  editor)
(defn editor-forward-paragraph [editor]
  editor)
(defn editor-backward-paragraph [editor]
  editor)
(defn editor-move-beginning-of-line [editor]
  editor)
(defn editor-move-end-of-line [editor]
  editor)
(defn editor-cider-eval-defun-at-point [editor]
  editor)
(defn editor-cider-eval-last-sexp [editor]
  editor)
(defn editor-paredit-forward [editor]
  editor)
(defn editor-paredit-backward [editor]
  editor)
(defn editor-paredit-open-square [editor]
  editor)
(defn editor-paredit-close-square [editor]
  editor)
(defn editor-paredit-open-round [editor]
  editor)
(defn editor-paredit-close-round [editor]
  editor)
(defn editor-paredit-open-curly [editor]
  editor)
(defn editor-paredit-close-curly [editor]
  editor)
(defn editor-paredit-kill [editor]
  editor)
(defn editor-set-mark-command [editor]
  editor)
(defn editor-kill-region [editor]
  editor)
(defn editor-paredit-backward-delete [editor]
  editor)

(defn editor-self-insert-command [editor ^String s]
  (let [{:keys [tree cursor paragraph rope buf ^TSParser parser]} editor
        
        ;; cursor is { :byte, :row, :column }
        sbytes (.getBytes s "utf-8")
        point-offset (count-points s)
        {cursor-byte :byte
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

        new-rope (ropes/insert rope
                               cursor-byte
                               sbytes)
        reader (->RopeReader new-rope)
        new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
    (assoc editor
           :tree new-tree
           :cursor {:byte (+ cursor-byte (alength sbytes))
                    :row new-cursor-row
                    :column new-cursor-column}
           :paragraph nil
           :rope new-rope)))



(defn editor-delete-backward-char [editor]
  (let [{:keys [tree cursor paragraph rope buf ^TSParser parser]} editor]
    (if (<= (:byte cursor) 0)
      editor
      (let [{cursor-byte :byte
             cursor-row :row
             cursor-column :column} cursor
            s (String. (byte-array rope) "utf-8")
            bi (doto (BreakIterator/getCharacterInstance)
                 (.setText s))
            prev (.preceding bi cursor-byte)
            prev-char (.charAt s prev)
            newline? (= prev-char \newline)
            

            new-cursor-row (if newline?
                             (dec cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                (loop [n 0]
                                  (let [start (.previous bi)]
                                    (if (or (= BreakIterator/DONE start)
                                            (= \newline (.charAt s start)))
                                      n
                                      (recur (inc n)))))
                                (dec cursor-column))


            new-tree (when-let [^TSTree tree tree]
                       (let [tree (.copy tree)]
                         (.edit tree (TSInputEdit. cursor-byte cursor-byte prev
                                              (TSPoint. cursor-row cursor-column)
                                              (TSPoint. cursor-row cursor-column)
                                              (TSPoint. new-cursor-row new-cursor-column)))
                         tree))
            
            new-rope (if (= (count rope) cursor-byte)
                       ;; workaround for https://github.com/IGJoshua/ropes/issues/2
                       (ropes/snip rope
                                   prev)

                       (ropes/snip rope
                                   prev
                                   cursor-byte))
            reader (->RopeReader new-rope)
            new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
        (assoc editor
               :tree new-tree
               :cursor {:byte prev
                        :row new-cursor-row
                        :column new-cursor-column}
               :paragraph nil
               :rope new-rope)))))



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
     :cursor {:byte 0
              :row 0
              :column 0}
     :paragraph nil
     :base-style #:text-style {:font-families ["Menlo"]
                               :font-size 12}
     :rope (ropes/rope)
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

(defn editor-view [editor]
  (let [lang (:language editor)]
    (para/paragraph (highlighted-text (TSQueryCursor.)
                                      (TSQuery. lang
                                                (lang->highlight-queries lang))
                                      (:base-style editor)
                                      (:tree editor)
                                      (:rope editor))
                    nil
                    {:paragraph-style/text-style (:base-style editor)}))
  )

(comment
  (let [editor (-> (make-editor clojure-lang)
                   (self-insert-command s))]
    (-> editor
        ;;(editor-delete-backward-char)
        (self-insert-command "hi"))
    
    nil)

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
