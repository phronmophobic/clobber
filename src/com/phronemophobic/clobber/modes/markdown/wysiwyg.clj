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
            [com.phronemophobic.clobber.modes.markdown :as markdown-mode]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util :as util]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
            [com.phronemophobic.clobber.util.ui :as util.ui])
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
        new-editor (if rope-changed?
                     (-> new-editor
                         (assoc :last-change (java.time.Instant/now)))
                     new-editor)

        ;; this could be improved
        ;; probably want some specific
        ;; support for keeping and clearing state
        ;; for repeated commands.
        popmode? (-> new-editor :mark :popmode?)
        new-editor (if (and popmode?
                            (= popmode?
                               (-> editor :mark :popmode?)))
                     (update new-editor :mark dissoc :popmode?)
                     new-editor)

        ;; support consecutive undos
        new-history-index (-> new-editor :history :index)
        new-editor (if (and new-history-index
                            (= new-history-index
                               (-> editor :history :index)))
                     (update new-editor :history dissoc :index)
                     new-editor)
        
        new-editor (util.ui/upkeep-search-ui editor new-editor)]
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

(defn ^:private wrap-memo1 [f]
  (let [last-call (volatile! nil)]
    (fn [o]
      (if-let [[last-arg last-ret] @last-call]
        (if (identical? last-arg o)
          last-ret
          (let [ret (f o)]
            (vreset! last-call [o ret])
            ret))
        (let [ret (f o)]
          (vreset! last-call [o ret])
          ret)))))

(defeffect ::update-editor [{:keys [$editor op] :as m}]
  (dispatch! :update $editor editor-upkeep (wrap-memo1 op)))

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

(defeffect ::save-editor [{:keys [editor $editor]}]
  (future
    (when-let [file (:file editor)]
      (let [^Rope rope (:rope editor)
            source (.toString rope)
            now (java.time.Instant/now)]
        (spit file source)
        (dispatch! :update $editor 
                   assoc
                   :last-file-load now
                   :last-save now)
        (dispatch! ::temp-status {:$editor $editor
                                  :msg "saved."}))))
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

(def key-bindings
  (assoc markdown-mode/key-bindings
         "C-x C-s" ::save-editor
         "C-x C-f" ::util.ui/file-picker
         "M-x" ::util.ui/mx-selector
         ;; "C-c C-d" ::show-doc
         "C-c b" ::update-bindings
         "C-g" #'editor-cancel
         "C-c t" ::tap-editor))



(defeffect ::update-bindings [{:keys [$editor]}]
  (dispatch! :update
             $editor
             (fn [editor]
               (update editor :key-bindings
                       (fn [bindings]
                         (merge bindings
                                markdown-mode/key-bindings
                                key-bindings)))))
  (dispatch! ::temp-status {:$editor $editor 
                            :msg "Bindings updated!"}))


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
      :key-bindings key-bindings  
      :base-style #:text-style {:font-size 12
      :height 1.2
      :height-override true}
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

(defmethod node->styled-text* "hard_line_break" [ctx ^TSNode node]
  {:style (:style ctx)
   :text "\n"})

(defmethod node->styled-text* "code_span" [ctx ^TSNode node]
  (let [style (util.ui/merge-styles [(:style ctx)
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

(defmethod node->styled-text* "info_string" [ctx ^TSNode node]
  ;; used for fenced code blocks
  )


(defmethod node->styled-text* "fenced_code_block" [ctx ^TSNode node]
  (conj
   (style-children ctx node)
   "\n"))

(defmethod node->styled-text* "code_fence_content" [ctx ^TSNode node]
  (let [style (util.ui/merge-styles [(:style ctx)
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
   (style-children ctx node)
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

(defmethod node->styled-text* "backslash_escape" [ctx ^TSNode node]
  nil)



(defmethod node->styled-text* "soft_line_break" [ctx ^TSNode node]
  (with-meta
    {:text "\n"
     :style (:style ctx)}
    {:node {:type (.getType node)}}))

(defmethod node->styled-text* "link" [ctx ^TSNode node]
  (let [style (util.ui/merge-styles [(:style ctx)
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
  (let [style (util.ui/merge-styles [(:style ctx)
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
                        "text" [:text (util/node->str rope child)]
                        "link_destination" [:destination (util/node->str rope child)]
                        nil))))
            (range (.getNamedChildCount node)))})))

(defmethod node->styled-text* "link_text" [ctx ^TSNode node]
  (with-meta (style-children ctx node)
    {:node {:type (.getType node)}}))


(defmethod node->styled-text* "link_title" [ctx ^TSNode node]
  (with-meta (style-children ctx node)
    {:node {:type (.getType node)}}))

(defmethod node->styled-text* "image_description" [ctx ^TSNode node]
  (with-meta (style-children ctx node)
    {:node {:type (.getType node)}}))

(defmethod node->styled-text* "image" [ctx ^TSNode node]
  (let [style (util.ui/merge-styles [(:style ctx)
                                     #:text-style
                                     {:color [0.0 0.36470588235294116 0.8156862745098039]}])
        ctx (assoc ctx :style style)
        rope (:rope ctx)]
    (style-children ctx node)))


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
        header-font-sizes {1 32
                           2 24
                           3 18
                           4 16
                           5 13
                           6 10}
        font-size (if-let [base-font-size (-> (:base-style ctx)
                                              :text-style/font-size)]
                    (long (* base-font-size
                             (case hnum
                               1 2
                               2 1.5
                               3 1.17
                               4 1
                               5 0.83
                               6 0.67
                               ;; else
                               1)))
                    (get header-font-sizes hnum 16))
        base-style (:style ctx)
        style (util.ui/merge-styles [base-style
                                     #:text-style
                                     {
                                     :font-size font-size
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
  (let [style (util.ui/merge-styles [(:style ctx)
                                     #:text-style
                                     {:font-style #:font-style{:weight :bold}}])
        ctx (assoc ctx :style style)]
    (style-children ctx node)))


(defmethod node->styled-text* "emphasis" [ctx ^TSNode node]
  (let [style (util.ui/merge-styles [(:style ctx)
                                     #:text-style
                                     {:font-style #:font-style{:slant :italic}}])
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

(defmethod node->styled-text* "thematic_break" [ctx ^TSNode node]
  {:text (str "\n\n" (util/node->str (:rope ctx) node) "\n\n")
   :style (:style ctx)})

(defn node->styled-text [ctx ^TSNode node]
  (node->styled-text* (assoc ctx
                             :base-style (:style ctx))
                      node))

#_(do
  (ns-unmap *ns* 'node->styled-text)
  (ns-unmap *ns* 'node->styled-text*))

(defn styled-text-length [styled-text]
  (loop [stack (list styled-text)
         offset 0]
    (if stack
      (let [node (first stack)]
        (cond
          (string? node) (recur (next stack)
                                (+ offset (count node)))
          (vector? node) (recur (into (next stack) node)
                                offset)
          (map? node) (recur (next stack)
                             (+ offset (count (:text node))))
          (nil? node) (recur (next stack)
                             offset)
          :else (throw (ex-info "Unknown type"
                                {:styled-text styled-text
                                 :node node}))))
      ;; else
      offset)))

(defn styled-text->handlers [styled-text]
  (loop [stack (list styled-text)
         offset 0
         handlers []]
    (if stack
      (let [node (first stack)
            mta (meta node)
            handlers (if (#{"uri_autolink" "link"} (-> mta :node :type))
                       (conj handlers
                             {:char-start offset
                              :char-end (+ offset (styled-text-length node))
                              :event [(:node mta)]})
                       handlers)]
        (cond
          (string? node) (recur (next stack)
                                (+ offset (count node))
                                handlers)
          (vector? node) (recur (into (next stack) (reverse node))
                                offset
                                handlers)
          (map? node) (recur (next stack)
                             (+ offset (count (:text node)))
                             handlers)
          (nil? node) (recur (next stack)
                             offset
                             handlers)
          :else (throw (ex-info "Unknown type"
                                {:styled-text styled-text
                                 :node node}))))
      ;; else
      handlers)))


(defn wrap-events
  [p]
  (ui/on
   :mouse-down
   (fn [[mx my]]
     (let [styled-text (:paragraph p)
           handlers (styled-text->handlers styled-text)]
       (util/first-by
        (comp (mapcat (fn [{:keys [char-start char-end event]}]
                        (let [rects (para/get-rects-for-range p 
                                                              char-start
                                                              char-end
                                                              :max
                                                              :max)]
                          (eduction
                           (map #(assoc % :event event))
                           rects))))
              (keep (fn [{:keys [event x y width height]}]
                      (when (and (>= mx x)
                                 (>= my y)
                                 (< mx (+ x width))
                                 (< my (+ y height)))
                        [[::markdown-event event]]))))
        handlers)))
   p))



(comment
  (let [rope (Rope/from "https://foo.com/foo.png")
        tree (util/parse (TreeSitterMarkdown.) rope)
        node (.getRootNode tree)
        styled-text (node->styled-text
                     {:rope rope
                      :style #:text-style
                      {
                      :font-size 12
                      :height 1.2
                      :height-override true}
                      :start-byte 0
                      :end-byte (.numBytes rope)}
                     node)]
    (with-meta (para/paragraph styled-text)
               {:view true})
    #_(tap> (styled-text->handlers styled-text)))
  ,)



(defui editor-view [{:keys [editor focused?]}]
  
  (let [lang (:language editor)
        rope (:rope editor)
        para (util.ui/editor->paragraph editor)
        para (assoc para :width (:width editor))
        para (assoc-in para
                       [:paragraph-style
                        :paragraph-style/max-lines] (-> editor :viewport :num-lines))
        
        status-bar (when-let [status (:status editor)]
                     (when-let [height (-> editor :viewport :text-height)]
                       (let [view (or (:temp status)
                                      (:status status))
                             status-bar (if (string? view)
                                          (para/paragraph view nil {:paragraph-style/text-style (:base-style editor)})
                                          view)]
                         (ui/translate 0
                                       (- height 8 (ui/height status-bar))
                                       status-bar))))]
    [(when focused?
       (util.ui/cursor-view rope para (:cursor editor)))
     para
     status-bar]))


(defui code-editor [{:keys [editor
                            ^:membrane.component/contextual
                            focused?
                            width
                            edit?]
                     :as this}]
  (let [body (editor-view {:editor editor
                           :focused? focused?})

        
        file-picker-state (::util.ui/file-picker-state editor)
        search-state (::text-mode/search editor)
        ui (::util.ui/ui editor)
               
        body 
        (cond
          (not focused?)
          (let [[w h] (ui/bounds body)
                gray 0.98]
            [(ui/filled-rectangle [gray gray gray]
                                  (max 800 w) (max 100 h))
             body])
          
          ui
          (assoc ui
                 :body body
                 :focused? focused?
                 :editor editor
                 :$editor $editor
                 :extra (:extra ui)
                 :$extra [$editor (com.rpl.specter/must ::util.ui/ui) '(keypath :extra)] 
                 :update-editor-intent ::update-editor
                 :context context
                 :$context $context)
          
          
          :else
          (key-binding/wrap-editor-key-bindings 
           {:key-bindings (:key-bindings editor)
            :editor editor
            :update-editor-intent ::update-editor
            :body body
            :$body nil}))
               
        body (ui/wrap-on
              :mouse-down
              (fn [handler mpos]
                (let [intents (handler mpos)]
                  (cons [:com.phronemophobic.clobber.modes.clojure.ui/request-focus]
                        intents)))
              body)


        body (if focused?
               (ui/on-clipboard-copy
                (fn []
                  [[::editor-copy {:editor editor
                                   :$editor $editor}]])
                (ui/on-clipboard-paste
                 (fn [s]
                   [[::editor-paste {:editor editor
                                     :$editor $editor
                                     :s s}]])
                 
                 body))
               ;; else
               body)
        
        ]
    (ui/vertical-layout
     (when-let [tree (:tree editor)]
       (try
         (para/paragraph (node->styled-text
                          {:rope (:rope editor)
                           :style (:base-style editor)
                           :start-byte 0
                           :end-byte (.numBytes (:rope editor))}
                          (-> tree .getRootNode))
                         width)
         (catch Exception e
           (prn e)
           ;;           (tap> e)
           nil)))
     (when edit?
       [body
        (ui/spacer 0 50)]))))

(defn editor->styled-text
  "Given an editor, return styled 
  text suitable for passing to para/paragraph.
  
  Editor must have a :tree parsed using TreeSitterMarkdown."
  [editor]
  (let [^TSTree
        tree (:tree editor)]
    (node->styled-text
     {:rope (:rope editor)
      :style (:base-style editor)
      :start-byte 0
      :end-byte (.numBytes (:rope editor))}
     (-> tree .getRootNode))))






