(ns com.phronemophobic.clobber.modes.text.ui
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
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
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

(defeffect ::editor-paste [{:keys [$editor s]}]
  (dispatch! :update $editor
             (fn [editor]
               (editor-upkeep editor
                              #(text-mode/editor-self-insert-command % s )))))

(defeffect ::save-editor [{:keys [editor $editor]}]
  (future
    (when-let [file (:file editor)]
      (let [^Rope rope (:rope editor)
            source (.toString rope)]
        (spit file source)
        (dispatch! ::temp-status {:$editor $editor
                                  :msg "saved."}))))
  nil)

(defeffect ::tap-editor [{:keys [editor $editor]}]
  (tap> editor)
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
  ([]
   (-> (text-mode/make-editor)
       (assoc :viewport {:num-lines 52
                         :start-line 0
                         :height 811}))))

(defn make-editor-from-file [f]
  (let [source (slurp f)]
    (-> (make-editor)
        (assoc :file f))))


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


(defn ^:private find-line-byte-offsets
  "Find byte offsets for the given lines. `lines` should be sorted in ascending order."
  [tree rope lines]
  (if tree
    (into []
          (map #(util/find-byte-offset-for-line tree rope %))
          lines)
    ;; else, do it the hard way
    
    (let [newline-byte (byte \newline)
          bytes (eduction
                 (mapcat  
                  (fn [^ByteBuffer bb]
                    (let [arr (.array bb )]
                      (eduction
                       (drop (.arrayOffset bb))
                       arr))))
                 (iterator-seq (.bytes ^Rope rope)))
          bytes-iter (.iterator ^Iterable bytes)]
      (loop [byte-offsets []
             current-line 0
             current-byte 0
             line (first lines)
             lines (next lines)]

        (if (= line current-line)
          (let [line (first lines)
                lines (next lines)
                byte-offsets (conj byte-offsets current-byte)]
            (if line
              (recur byte-offsets
                     current-line
                     current-byte
                     line 
                     lines)
              byte-offsets))
          (if (.hasNext bytes-iter)
            (let [b (.next bytes-iter)
                  next-byte (inc current-byte)
                  next-line (if (= b newline-byte)
                              (inc current-line)
                              current-line)]
              (recur byte-offsets
                     next-line
                     next-byte
                     line
                     lines))
            ;; else
            byte-offsets))))))

(defn editor->paragraph [editor]
  (let [tree (:tree editor)
        lang (:language editor)
        ^Rope
        rope (:rope editor)

        {:keys [start-line num-lines]} (:viewport editor)
        end-line (+ start-line num-lines)

        ;; there's probably a better way to do this
        ;; but this seems to work for now.
        start-editor (-> editor
                         (text-mode/editor-move-beginning-of-line)
                         (update :cursor dissoc :target-column-byte)
                         (text-mode/editor-previous-line (- (-> editor :cursor :row)
                                                            start-line)))
        start-byte-offset (-> start-editor :cursor :byte)
        end-byte-offset (-> editor
                            (text-mode/editor-move-beginning-of-line)
                            (update :cursor dissoc :target-column-byte)
                            (text-mode/editor-next-line (- end-line
                                                           (-> editor :cursor :row)))
                            (text-mode/editor-move-end-of-line)
                            :cursor
                            :byte)
        
        char-offset (-> start-editor :cursor :char)

        base-style (:base-style editor)
        
        ;; p (node->styled-text (.getRootNode tree) rope base-style start-byte-offset end-byte-offset)
        para (para/paragraph (-> (.sliceBytes rope start-byte-offset end-byte-offset)
                                 .toString)
                             nil {:paragraph-style/text-style (:base-style editor)})
        para (assoc para
                    :char-offset char-offset
                    :start-byte-offset start-byte-offset
                    :end-byte-offset end-byte-offset)]
    para))

(defeffect ::select-file [{:keys [file]}]
  #_(dispatch! :com.phronemophobic.easel/add-applet
             {:make-applet
              (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                #(f % {:file file}))}))

(defui editor-view [{:keys [editor
                            ^:membrane.component/contextual
                            focus]}]
  
  (let [lang (:language editor)
        rope (:rope editor)
        para (editor->paragraph editor)
        
        status-bar (when-let [status (:status editor)]
                     (when-let [height (-> editor :viewport :height)]
                       (let [view (or (:temp status)
                                      (:status status))
                             status-bar (if (string? view)
                                          (para/paragraph view nil {:paragraph-style/text-style (:base-style editor)})
                                          view)]
                         (ui/translate 0
                                       (- height 8 (ui/height status-bar))
                                       status-bar))))]
    [(cursor-view rope para (:cursor editor))
     para
     status-bar]))


(def key-tree
  (key-binding/key-bindings->key-tree
   (assoc text-mode/key-bindings
          "C-x C-s" ::save-editor
          ;; "C-x C-f" ::file-picker
          ;; "C-c C-d" ::show-doc
          "C-g" #'editor-cancel
          "C-c t" ::tap-editor)))


(defui text-editor [{:keys [editor
                            ^:membrane.component/contextual
                            focus]
                     :as this}]
  (let [body (editor-view {:editor editor
                           ;; hack, otherwise, $editor is slightly different
                           :$editor $editor})

        focused? (= $editor focus)
        body (if focused?
               (key-binding/wrap-editor-key-tree {:key-tree key-tree
                                                  :editor editor
                                                  ;; hack, otherwise, $editor is slightly different
                                                  :$editor $editor
                                                  :update-editor-intent ::update-editor
                                                  :body body
                                                  :$body nil})
               ;; else
               (let [[w h] (ui/bounds body)
                     gray 0.98]
                 [(when (not= [$editor :file-picker]
                              focus)
                    (ui/filled-rectangle [gray gray gray]
                                         (max 800 w) (max 100 h)))
                  body]))

        
        body (ui/wrap-on
              :mouse-down
              (fn [handler mpos]
                (let [intents (handler mpos)]
                  (cons [:set $focus $editor]
                        intents)))
              body)

        body (ui/on-clipboard-paste
              (fn [s]
                [[::editor-paste {:editor editor
                                  :$editor $editor
                                  :s s}]])
              body)]
    body))


(defui debug [{:keys [editor]}]
  (let [
        
        font-family (get extra :font-family)
        font-size (get extra :font-size 12)
        $editor $editor
        editor (-> editor
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
                                     (assoc :eval-ns (:eval-ns editor))
                                     (assoc :cursor {:byte 0
                                                     :char 0
                                                     :point 0
                                                     :row 0
                                                     :column 0})
                                     (text-mode/editor-update-viewport))]])})
        (ant/button {:text  "reload"
                     :on-click (fn []
                                 [[::reload-editor {:editor editor
                                                    :$editor $editor}]])})
        (ant/button {:text  "load-json"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor)
                                       (text-mode/editor-self-insert-command
                                        (slurp (io/as-url
                                                "https://raw.githubusercontent.com/sogaiu/tree-sitter-clojure/refs/heads/master/src/grammar.json")))
                                       (text-mode/editor-beginning-of-buffer)
                                       (text-mode/editor-update-viewport))
                                   ]])})
        (ant/button {:text  "load-easel"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor)
                                       (text-mode/editor-self-insert-command
                                        (slurp (io/resource
                                                "com/phronemophobic/easel.clj")))
                                       (text-mode/editor-beginning-of-buffer)
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
      
      (text-editor {:editor editor
                    :$editor $editor})]
     
     
     {:direction :column
      :gap 8})))



(defn dev
  ([f]
   (let [editor (-> (make-editor)
                    (text-mode/editor-self-insert-command
                     (slurp f))
                    (text-mode/editor-beginning-of-buffer)
                    (text-mode/editor-update-viewport))]
     (dev/add-component-as-applet #'debug
                                  {:editor editor}))
   )
  ([]
   (let [
         
         editor (-> (make-editor)
                    (text-mode/editor-update-viewport))
]
     (dev/add-component-as-applet #'debug
                                  {:editor editor}))))

(defeffect ::tap [& args]
  (case (count args)
    0 nil
    1 (tap> (first args))
    2 (tap> (vec args))))

(comment
  (dev)
  (dev (io/file "Readme.md"))

  ,)
