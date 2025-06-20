(ns com.phronemophobic.clobber.modes.markdown.wysiwyg
  (:require [clojure.java.io :as io]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.viscous :as viscous]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [com.phronemophobic.clobber.modes.markdown :as markdown-mode]
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
                           TreeSitterMarkdown
                           TSInputEdit
                           TSInputEncoding)
           java.nio.charset.Charset
           java.util.Arrays
           java.nio.ByteBuffer
           java.util.regex.Pattern
           com.ibm.icu.text.BreakIterator
           clojure.lang.LineNumberingPushbackReader
           java.io.File
           java.io.StringReader
           io.lacuna.bifurcan.Rope))


(defn editor-upkeep [editor op]
  ;; if cursor or rope changed, editor-update-viewport
  ;; potentially update undo history
  ;; if rope changed, unset selection cursor
  (let [new-editor (op editor)

        rope-changed? (not (identical? (:rope editor)
                                       (:rope new-editor)))
        cursor-changed? (not (identical? (:cursor editor)
                                         (:cursor new-editor)))

        new-editor (if rope-changed?
                     (-> new-editor
                         (dissoc :select-cursor))
                     new-editor)
        new-editor (if (or rope-changed?
                           cursor-changed?)
                     (text-mode/editor-update-viewport new-editor)
                     new-editor)
        new-editor (if rope-changed?
                     (text-mode/editor-append-history new-editor editor)
                     new-editor)

        ;; support consecutive undos
        new-history-index (-> new-editor :history :index)
        new-editor (if (and new-history-index
                            (= new-history-index
                               (-> editor :history :index)))
                     (update new-editor :history dissoc :index)
                     new-editor)]
    new-editor))

(defn editor-cancel [editor]
  (dissoc editor :select-cursor))

(defn editor-set-height [editor height]
  (let [base-style (:base-style editor)
        row-height (* (:text-style/height base-style)
                      (:text-style/font-size base-style))
        num-lines (max 0
                       (- (quot height row-height)
                          ;; reserve two lines for status bar
                          ;; and one line for cursor info
                          4))]
    (-> editor
        (assoc-in [:viewport :num-lines] (long num-lines))
        (assoc-in [:viewport :height] height))))


(defeffect ::editor-paste [{:keys [$editor s]}]
  (dispatch! :update $editor
             (fn [editor]
               (editor-upkeep editor
                              #(text-mode/editor-self-insert-command % s )))))

(defeffect ::update-editor [{:keys [$editor op] :as m}]
  (dispatch! :update $editor editor-upkeep op))

(defeffect ::temp-status [{:keys [$editor msg]}]
  (future
    (dispatch! :update $editor
               (fn [editor]
                 (assoc-in editor [:status :temp] msg)))
    (Thread/sleep 4000)
    (dispatch! :update $editor
               (fn [editor]
                 (if (= msg (-> editor :status :temp))
                   (update editor :status dissoc :temp)
                   editor)))))


(def special-keys {"DEL" :backspace
                   "RET" :enter
                   "SPC" \space
                   "TAB" :tab
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

(defeffect ::save-editor [{:keys [editor $editor]}]
  (future
    (when-let [file (:file editor)]
      (let [^Rope rope (:rope editor)
            source (.toString rope)]
        (spit file source)
        (dispatch! ::temp-status {:$editor $editor
                                  :msg "saved."}))))
  nil)

(defui file-picker [{:keys [base-style folder
                            focused?
                            width]
                     :as this}]
  (let [base-style (or base-style 
                       #:text-style
                       {:font-families ["Menlo"]
                        :font-size 12
                        :height 1.2
                        :height-override true})

        current-folder (get extra :current-folder folder)
        
        offset (get extra :offset 0)
        search-str (get extra :search-str "")
        fs (into 
            []
            (comp (filter (fn [f]
                            (str/includes? (.getName f)
                                           search-str)))
                  (drop offset))
            (sort (.listFiles current-folder)))
        ps (into 
            [{:text (str (.getCanonicalPath current-folder) "/")
              :style (assoc base-style :text-style/color [0 0 0.843])}
             search-str
             " | "]
            (comp (map (fn [f]
                         
                         (if (.isFile f)
                           (.getName f)
                           {:text (str (.getName f) "/")
                            :style (assoc base-style :text-style/color [0.909 0.2784 0.3411  ])}
                           )))
                  (interpose " | "))
            fs)

        p (para/paragraph
           ps
           width
           {:paragraph-style/text-style base-style})
        
        p (if focused?
            (ui/on
             :key-event
             (fn [key scancode action mods]
               (when (#{:press :repeat} action)
                 (let [ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
                   (cond 
                     
                     (and ctrl?
                          (= (char key) \S))
                     [[:update $offset 
                       (fn [offset]
                         (if (>= offset (count fs))
                           0
                           (inc offset)))]]))))
             
             :key-press
             (fn [s]
               (cond
                 
                 (= s :enter)
                 (when-let [f (first fs)]
                   (if (.isFile f)
                     [[::select-file {:file f}]]
                     [[:set $current-folder f]
                      [:set $search-str ""]
                      [:set $offset 0]]))
                 
                 (= s :backspace)
                 (if (= search-str "")
                   (when-let [parent (-> current-folder
                                         .getCanonicalFile
                                         .getParentFile)]
                     [[:set $current-folder parent]
                      [:set $offset 0]])
                   [[:update $search-str
                     (fn [s]
                       (subs s 0 (max 0 (- (count s) 1))))]
                    [:set $offset 0]])
                 
                 (string? s)
                 [[:update $search-str str s]
                  [:set $offset 0]]))
             p)
            ;; else
            p)]
    p))

(defeffect ::file-picker [{:keys [editor $editor]}]
  (dispatch! :update $editor
             (fn [editor]
               (-> editor
                   (assoc-in [:status :file-picker] {}))))
  ;; ugly
  (dispatch! :set 
             ['(keypath :membrane.component/context)
              '(keypath :focus)]
             [$editor :file-picker])
  nil)

(defeffect ::reload-editor [{:keys [editor $editor]}]
  (future
    (when-let [file (:file editor)]
      (let [source (slurp file)
            previous-source (.toString (:rope editor))]
        (dispatch! :set $editor
          (editor-upkeep editor
          #(text-mode/editor-set-string % source))))))
  nil)


(defn make-editor
  ([{:keys [file source] :as m}]
   (let [editor (make-editor)
         
         editor (cond-> editor
                  file (assoc :file file)
                  source (text-mode/editor-self-insert-command source))

         editor (-> editor
                    (assoc :cursor {:byte 0
                                    :char 0
                                    :point 0
                                    :row 0
                                    :column-byte 0})
                    (text-mode/editor-update-viewport))]
     editor))
  ([]
   (let [language (TreeSitterMarkdown.)]
     {:tree nil
      :cursor {:byte 0
               :char 0
               :point 0
               :row 0
               :column-byte 0}
      :viewport {:start-line 0
                 :num-lines 40}
      :paragraph nil
      :base-style #:text-style {:font-size 12
      :height 1.2
      :height-override true}
      :eval-ns *ns*
      :rope Rope/EMPTY
      :language language
      :viscous? true
      :parser (doto (TSParser.)
                (.setLanguage language))
      :buf (byte-array 4096)})))


(defn make-editor-from-file [f]
  (let [source (slurp f)]
    (make-editor {:file f
                  :source source})))


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

(defn merge-styles [styles]
  (apply merge-with
         (fn [s1 s2]
           (if (map? s1)
             (merge-styles [s1 s2])
             s2))
         styles))

(defn styles-by-index [styles]
  (let [all-styles
        (into []
              (comp
               (map-indexed
                (fn [i m]
                  (eduction
                   (map-indexed
                    (fn [j [[start end] style]]
                      [{:op :start
                        :index start
                        :style style
                        :id [i j]}
                       {:op :end
                        :index end
                        :style style
                        :id [i j]}]))
                   cat
                   m)))
               cat)
              styles)
        by-index (sort-by :index all-styles)]
    by-index))

(defn styled-text [rope base-style styles start-byte-offset end-byte-offset]
  (let [by-index (styles-by-index styles)]
    (loop [offset start-byte-offset
           by-index (seq by-index)
           ;; unprioritized for now
           active-styles {}
           p []]
      (if (and by-index
               (< offset end-byte-offset))
        (let [event (first by-index)
              end-offset (min (:index event) end-byte-offset)
              p (if (> end-offset offset)
                  (let [text (util/rope->str rope offset end-offset)]
                    (conj p
                          (if (seq active-styles)
                            (let [style (merge-styles (cons base-style (vals active-styles)))]
                              {:text text
                               :style style})
                            text)))
                  ;; else
                  p)

              active-styles (if (= :start (:op event))
                              (assoc active-styles (:id event) (:style event))
                              (dissoc active-styles (:id event)))]
          (recur (max end-offset offset)
                 (next by-index)
                 active-styles
                 p))
        ;;else
        (if (< offset end-byte-offset)
          (let [text (util/rope->str rope offset end-byte-offset)]
            (conj p
                  (if (seq active-styles)
                    (let [style (merge-styles (cons base-style (vals active-styles)))]
                      {:text text
                       :style style})
                    text)))
          p)))))

(defn selection-style [editor viewport]
  (when-let [select-cursor (:select-cursor editor)]
    (let [cursor (:cursor editor)
          cursor-byte (:byte cursor)
          select-cursor-byte (:byte select-cursor)

          start-byte (min cursor-byte select-cursor-byte)
          end-byte (max cursor-byte select-cursor-byte)]
      {[start-byte end-byte] {:text-style/background-color {:color [1.0 0.8431372549019608 0.5294117647058824 0.7]}}})))

(defn debug-selection-style [editor viewport]
  (when-let [^TSNode debug-node (get (:debug-node editor)
                                     (:rope editor))]
    (let [start-byte (.getStartByte debug-node )
          end-byte (.getEndByte debug-node)]
      {[start-byte end-byte] {:text-style/background-color {:color [1.0 0.8431372549019608 0.5294117647058824 0.7]}}})))


(defn syntax-style [editor
                    {:keys [start-byte-offset end-byte-offset]}]
  nil)

(defn highlight-search [editor {:keys [char-offset start-byte-offset end-byte-offset]}]
  (when-let [^java.util.regex.MatchResult
             match (-> editor ::search :match)]
    (when (>= (.start match)
              char-offset)
      (let [^Rope
            rope (:rope editor)
            diff-string (.toString
                         (.subSequence rope char-offset (.start match)))
            diff-bytes (alength (.getBytes diff-string "utf-8"))

            start-byte (+ start-byte-offset diff-bytes)
            end-byte (+ start-byte (alength (.getBytes (.group match) "utf-8")))]
        {[start-byte end-byte] {:text-style/background-color {:color [0 0 1 0.2]}}})))
  )




(defn named-node-children [^TSNode parent]
  (let [c (.getNamedChildCount parent)]
    (into []
          (map (fn [i]
                 (.getNamedChild parent i)))
          (range c))))



(defmulti node->styled-text* (fn [ctx ^TSNode node]
                               (.getType node)))

(defn ^:private style-children [ctx ^TSNode parent]
  (into []
        (map #(node->styled-text* ctx %))
        (named-node-children parent)))

(defmethod node->styled-text* "paragraph" [ctx ^TSNode node]
  (conj
   (style-children ctx node)
   "\n"))

(defmethod node->styled-text* "line_break" [ctx ^TSNode node]
  {:style (:style ctx)
   :text "\n"})

(defmethod node->styled-text* "code_span" [ctx ^TSNode node]
  (let [style (merge-styles [(:style ctx)
                             #:text-style
                             {:color [0.7137254901960784 0.23529411764705882 0.32941176470588235]
                              :background-color {:color [0.9647058823529412
                                                         0.9647058823529412
                                                         0.9647058823529412]}
                              :font-families ["Menlo"]}])
        ctx (assoc ctx :style style)]
    (into []
          (map #(node->styled-text* ctx %))
          (named-node-children node)))
  )

(defmethod node->styled-text* "fenced_code_block" [ctx ^TSNode node]
  (conj
   (style-children ctx node)
   "\n"))

(defmethod node->styled-text* "code_fence_content" [ctx ^TSNode node]
  (let [style (merge-styles [(:style ctx)
                             #:text-style
                             {:font-families ["Menlo"]}])
        ctx (assoc ctx :style style)]
    (into []
          (map #(node->styled-text* ctx %))
          (named-node-children node)))
  )

(defmethod node->styled-text* "tight_list" [ctx ^TSNode node]
  (conj
   (style-children ctx node)
   "\n"))

(defmethod node->styled-text* "loose_list" [ctx ^TSNode node]
  (conj
   (style-children node)
   "\n"))

(defmethod node->styled-text* "list_item" [ctx ^TSNode node]
  (style-children ctx node))

(defmethod node->styled-text* "list_marker" [ctx ^TSNode node]
  {:text " • "
   :style (:style ctx)
   :node {:type (.getType node)}})

(defmethod node->styled-text* "document" [ctx ^TSNode node]
  (style-children ctx node))

(defmethod node->styled-text* "text" [ctx ^TSNode node]
  {:text 
   (util/rope->str (:rope ctx)
                   (max (:start-byte ctx) (.getStartByte node))
                   (min (:end-byte ctx) (.getEndByte node)))
   :style (:style ctx)})


(defmethod node->styled-text* "soft_line_break" [ctx ^TSNode node]
  (with-meta
   {:text "\n"
    :style (:style ctx)}
   {:node {:type (.getType node)}}))

(defmethod node->styled-text* "link" [ctx ^TSNode node]
  (let [style (merge-styles [(:style ctx)
                             #:text-style
                             {:color [0.0 0.36470588235294116 0.8156862745098039]}])
        ctx (assoc ctx :style style)
        rope (:rope ctx)]
    
    (with-meta
      (style-children ctx node)
      {:node
       (into {:type (.getType node)}
             (keep (fn [i]
                     (let [child (.getNamedChild node i)]
                       (case (.getType child)
                         "link_text" [:text (util/node->str rope child)]
                         "link_destination" [:destination (util/node->str rope child)]
                         nil))))
             (range (.getNamedChildCount node)))})))

(defmethod node->styled-text* "uri_autolink" [ctx ^TSNode node]
  (let [style (merge-styles [(:style ctx)
                             #:text-style
                             {:color [0.0 0.36470588235294116 0.8156862745098039]}])
        ctx (assoc ctx :style style)
        rope (:rope ctx)]
    
    (with-meta
      (style-children ctx node)
     {:node
      (into {:type (.getType node)}
            (keep (fn [i]
                   (let [child (.getNamedChild node i)]
                     (case (.getType child)
                       "link_text" [:text (util/node->str rope child)]
                       "link_destination" [:destination (util/node->str rope child)]
                       nil))))
            (range (.getNamedChildCount node)))})))

(defmethod node->styled-text* "link_text" [ctx ^TSNode node]
  (with-meta (style-children ctx node)
    {:node {:type (.getType node)}}))



(defmethod node->styled-text* "link_destination" [ctx ^TSNode node]
  ;; this space has been intentionally left blank.
  )

(defmethod node->styled-text* "atx_heading" [ctx ^TSNode node]
  
  (let [htype (.getNamedChild node 0)
        hnum (case (.getType htype)
               "atx_h1_marker" 1
               "atx_h2_marker" 2
               "atx_h3_marker" 3
               "atx_h4_marker" 4
               "atx_h5_marker" 5
               "atx_h6_marker" 6)
        base-style (:style ctx)
        style (merge-styles [base-style
                             #:text-style
                             {
                              :font-size 60
                              :font-style #:font-style{:weight :bold}
                              }
                             ])
        ctx (assoc ctx
                   :style style
                   :start-byte (+ 2 (.getStartByte node)))
        child (.getNamedChild node 1)]
    (when (not (.isNull child))
      [(node->styled-text* ctx child)
       {:text "\n\n"
        :style base-style}])))



(defmethod node->styled-text* "strong_emphasis" [ctx ^TSNode node]
  (let [style (merge-styles [(:style ctx)
                             #:text-style
                             {:font-style #:font-style{:weight :bold}}])
        ctx (assoc ctx :style style)]
    (style-children ctx node)))

(def my-base-style #:text-style
  {:font-families ["Menlo"]
   :font-size 12
   :height 1.2
   :height-override true})

(defmethod node->styled-text* "atx_h1_marker" [ctx ^TSNode node]
  nil)

(defmethod node->styled-text* "heading_content" [ctx ^TSNode node]
  (style-children ctx node))

(defn node->styled-text [ctx ^TSNode node]
  (node->styled-text* ctx node))

#_(do
  (ns-unmap *ns* 'node->styled-text)
  (ns-unmap *ns* 'node->styled-text*))


(comment
  (let [rope (Rope/from "[foo](bar)")]
  (-> (util/parse (TreeSitterMarkdown.) rope)
    .getRootNode
      
      (node->styled-text
     rope
     #:text-style
     {
     :font-size 12
     :height 1.2
     :height-override true}
     0
     (.numBytes rope)) 
      (->> (tree-seq seqable? seq )
           (keep meta))

      tap>)
  )
  ,)

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

        base-style (:base-style editor)
        
        ;; p (node->styled-text (.getRootNode tree) rope base-style start-byte-offset end-byte-offset)
        para (para/paragraph (.toString rope) nil {:paragraph-style/text-style (:base-style editor)})
        para (assoc para
                    :char-offset char-offset
                    :start-byte-offset start-byte-offset
                    :end-byte-offset end-byte-offset)]
    para))


#_(defeffect ::select-file [{:keys [file]}]
    (dispatch! :com.phronemophobic.easel/add-applet
               {:make-applet
                (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                  #(f % {:file file}))}))

(defui editor-view [{:keys [editor
                            ^:membrane.component/contextual
                            focus]}]
  (when-let [tree (:tree editor)]
    (let [lang (:language editor)
          rope (:rope editor)
          para (editor->paragraph editor)

          status-bar (when-let [status (:status editor)]
                       (when-let [height (-> editor :viewport :height)]
                         (let [view (or 
                                     (when (and (:file-picker status)
                                                (:file editor))
                                       (ui/on
                                        ::select-file
                                        (fn [m]
                                          [[::select-file m]
                                           [:update $editor update :status dissoc :file-picker]])
                                        
                                        (ui/wrap-on
                                         :key-event
                                         (fn [handler key scancode action mods]
                                           (when (#{:press :repeat} action)
                                             (let [ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
                                               (cond 
                                                 
                                                 (and ctrl?
                                                      (= (char key) \G))
                                                 [[:update $editor update :status dissoc :file-picker]
                                                  [:set $focus $editor]]
                                                 
                                                 :else (handler key scancode action mods)))))
                                         
                                         (file-picker {:folder (.getParentFile (:file editor))
                                                       :extra (:file-picker status)
                                                       :base-style (:base-style editor)
                                                       :focused? (= [$editor :file-picker] focus)}))))
                                     (:temp status)
                                     (:status status))
                               status-bar (if (string? view)
                                            (para/paragraph view nil {:paragraph-style/text-style (:base-style editor)})
                                            view)]
                           (ui/translate 0
                                         (- height 8 (ui/height status-bar))
                                         status-bar))))]
      [(cursor-view rope para (:cursor editor))
       para
       status-bar])))


(defn init-search-forward [editor]
  (assoc editor
         ::search {:initial-cursor (:cursor editor)
                   :initial-rope (:rope editor)}))

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
   (assoc markdown-mode/key-bindings
          "C-x C-s" ::save-editor
          "C-x C-f" ::file-picker
          "C-_" #'text-mode/editor-undo
          "C-g" #'editor-cancel
          "C-c b" #(text-mode/editor-self-insert-command % "•")
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
                (.toMatchResult matcher)
                (when (.find matcher 0)
                  (.toMatchResult matcher)))]
    (if match
      (let [
            ;; calculate new cursor
            s (-> (.subSequence rope 0 (.start match))
                  .toString)

            {:keys [row column-byte]} (util/count-row-column-bytes s)

            cursor {:byte (alength (.getBytes s "utf-8"))
                    :char (.length s)
                    :point (util/num-points s)
                    :row row
                    :column-byte column-byte}]
        (-> editor
            (assoc-in [::search :query] query)
            (assoc-in [::search :match] match)
            (assoc :cursor cursor)
            (text-mode/editor-update-viewport)))
      (-> editor
          (assoc-in [::search :query] query)
          (assoc-in [::search :match] nil)))))

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
                (.toMatchResult matcher)
                (when (.find matcher 0)
                  (.toMatchResult matcher)))
        ;; now do it again
        match (when match
                (if (.find matcher)
                  (.toMatchResult matcher)
                  (when (.find matcher 0)
                    (.toMatchResult matcher))))]
    (if match
      (let [ ;; calculate new cursor
            s (-> (.subSequence rope 0 (.start match))
                  .toString)

            {:keys [row column-byte]} (util/count-row-column-bytes s)

            cursor {:byte (alength (.getBytes s "utf-8"))
                    :char (.length s)
                    :point (util/num-points s)
                    :row row
                    :column-byte column-byte}]
        (-> editor
            (assoc-in [::search :query] query)
            (assoc :cursor cursor)
            (assoc-in [::search :match] match)
            (text-mode/editor-update-viewport)))
      (-> editor
          (assoc-in [::search :query] query)
          (assoc-in [::search :match] nil)))))

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
        modifiers (get extra ::modifiers)
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
       (let [intents (if (empty? modifiers)
                       (when (not (#{:left_shift :right_shift}
                                   s))
                         (find-match {:key (if (string? s)
                                             (first s)
                                             s)}))
                       (if (keyword? s)
                         (find-match
                          (cond-> {:key s}
                            (:alt modifiers) (assoc :meta? true)
                            (:super modifiers) (assoc :super? true)
                            (:ctrl? modifiers) (assoc :ctrl? true)))))]
         (if (seq intents)
           intents
           (when (and (not next-bindings)
                      (string? s)
                      (empty? modifiers)
                      (-> (.getBytes ^String s)
                          first
                          pos?))
             [[::update-editor {:op #(text-mode/editor-self-insert-command % s)
                                :$editor $editor}]]))))
     :key-event
     (fn [key scancode action mods]
       (let [alt? (not (zero? (bit-and ui/ALT-MASK mods)))
             super? (not (zero? (bit-and ui/SUPER-MASK mods)))
             shift? (not (zero? (bit-and ui/SHIFT-MASK mods)))
             ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
         (if (#{:press :repeat} action)
           (let [key (if shift?
                       (let [c (char key)]
                         (get uppercase c))
                       (Character/toLowerCase (char key)))
                 key-press (cond-> {:key key}
                             alt? (assoc :meta? true)
                             super? (assoc :super? true)
                             ctrl? (assoc :ctrl? true))]
             (when (and key
                        (or alt?
                            super?
                            ctrl?))
               (cons
                [:update $modifiers (fn [xs] (cond-> (or xs #{})
                                               alt? (conj :alt)
                                               super? (conj :super)
                                               ctrl? (conj :ctrl?)))]
                (find-match key-press))))
           ;; release action
           [[:update $modifiers (fn [xs] (cond-> (or xs #{})
                                           (not alt?) (disj :alt)
                                           (not super?) (disj :super)
                                           (not ctrl?) (disj :ctrl?)))]])))
     body)))


(defui code-editor [{:keys [editor
                            ^:membrane.component/contextual
                            focus
                            edit?]
                     :as this}]
  (let [body (editor-view {:editor editor
                           ;; hack, otherwise, $editor is slightly different
                           :$editor $editor})

        
        focused? (= $editor focus)
               
        body (if focused?
               (clojure-keymap {:bindings clojure-keytree
                                :editor editor
                                :$editor $editor
                                :body body})
               (let [[w h] (ui/bounds body)
                     gray 0.98]
                 [(when (not= [$editor :file-picker]
                              focus)
                    (ui/filled-rectangle [gray gray gray]
                                         (max 800 w) (max 100 h)))
                  body]))
               
        body (if focused?
               (wrap-search
                {:editor editor
                 :$body nil
                 :body body})
               body)

        body (ui/on-clipboard-paste
              (fn [s]
                [[::editor-paste {:editor editor
                                  :$editor $editor
                                  :s s}]])
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
     ;;(ui/label (pr-str (:cursor editor)))

     #_(when-let [tree (:tree editor)]
       (para/paragraph
        (-> tree (.getRootNode) str)

        400))
     
     (when-let [tree (:tree editor)]
       (try
         (para/paragraph (node->styled-text
                          {:rope (:rope editor)
                           :style (:base-style editor)
                           :start-byte 0
                           :end-byte (.numBytes (:rope editor))}
                          (-> tree .getRootNode))
                         1000)
         (catch Exception e
           (prn e)
           ;;           (tap> e)
           nil)))
     (when edit?
       [body
        (ui/spacer 0 50)]))))




(defui debug [{:keys [editor]}]
  (let [
        
        font-family (get extra :font-family)
        font-size (get extra :font-size 12)
        viscous? (get editor :viscous?)
        instarepl? (get extra :instarepl?)
        structure? (get extra :structure?)

        $editor $editor
        editor (cond-> editor
                 font-family (assoc-in
                              [:base-style :text-style/font-families] [font-family])
                 
                 font-size (assoc-in
                            [:base-style :text-style/font-size] font-size))]
    (ui/flex-layout
     [(ui/flex-layout
       [(ant/button {:text  "tap>"
                     :on-click (fn []
                                 [[::tap editor]])})
        (ant/button {:text  "clear"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor)
                                     (assoc :eval-ns (:eval-ns editor))
                                       (text-mode/editor-self-insert-command "\n\n\n\n\n\n")
                                       (assoc :cursor {:byte 0
                                                       :char 0
                                                       :point 0
                                                       :row 0
                                                       :column-byte 0})
                                       (dissoc :structure-state )
                                       (text-mode/editor-update-viewport))
                                   ]])})
                                 #_(ant/button {:text  "load"
                                   :on-click (fn []
                                       [[:set $editor
                                        (-> (make-editor)
                                            (text-mode/editor-self-insert-command
                                                       (slurp (io/resource "com/phronemophobic/clobber.clj")))
                                                       (assoc :cursor {:byte 0
                                                                       :char 0
                                                                       :point 0
                                            :row 0
                                         :column-byte 0})
                                       (text-mode/editor-update-viewport))
                                   ]])})
        (ant/button {:text  "reload"
                     :on-click (fn []
                                 [[::reload-editor {:editor editor
                                                    :$editor $editor}]])})
]
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
                        "Helvetica"
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
      (code-editor {:editor editor
                    :$editor $editor})]
     
     
     {:direction :column
      :gap 8})))



(defn dev  
  ([]
   (let [editor (-> (make-editor)
                    (text-mode/editor-self-insert-command "")
                    (assoc :cursor {:byte 0
                                    :char 0
                                    :point 0
                                    :row 0
                                    :column-byte 0})
                    (text-mode/editor-update-viewport))
         ;; editor (if file
         ;;          (assoc editor :file file)
         ;;          editor)
         ]
     (dev/add-component-as-applet #'debug
                                  {:editor editor}))))

(defeffect ::tap [& args]
  (case (count args)
    0 nil
    1 (tap> (first args))
    2 (tap> (vec args))))



