(ns com.phronemophobic.clobber.util.ui
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
            [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
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
                           ;; TreeSitterClojure
                           TreeSitterJson
                           TreeSitterHtml
                           TreeSitterMarkdown
                           TreeSitterCpp
                           TSInputEdit
                           TSInputEncoding)
           java.nio.charset.Charset
           java.util.Arrays
           java.nio.ByteBuffer
           java.util.regex.Pattern
           com.ibm.icu.text.BreakIterator
           clojure.lang.LineNumberingPushbackReader
           java.time.Duration
           java.time.Instant
           java.io.File
           java.io.StringReader
           io.lacuna.bifurcan.Rope))

(def default-theme
  {"punctuation"
   #:text-style{:color
                [0.3058823529411765
                 0.3058823529411765
                 0.3058823529411765]},
   "string" #:text-style{:color [0.0 0.5294117647058824 0.0]},
   "punctuation.delimiter"
   #:text-style{:color
                [0.3058823529411765
                 0.3058823529411765
                 0.3058823529411765]},
   "type.builtin"
   {:text-style/color [0.0 0.37254901960784315 0.37254901960784315],
    :font-style #:font-style{:weight :bold}},
   "constant"
   #:text-style{:color [0.5294117647058824 0.37254901960784315 0.0]},
   "operator"
   {:text-style/color
    [0.3058823529411765 0.3058823529411765 0.3058823529411765],
    :font-style #:font-style{:weight :bold}},
   "attribute"
   {:text-style/color [0.6862745098039216 0.0 0.0],
    :font-style #:font-style{:slant :italic}},
   "string.special"
   #:text-style{:color [0.0 0.5294117647058824 0.5294117647058824]},
   "tag" #:text-style{:color [0.0 0.0 0.5294117647058824]},
   "module"
   #:text-style{:color [0.6862745098039216 0.5294117647058824 0.0]},
   "punctuation.bracket"
   #:text-style{:color
                [0.3058823529411765
                 0.3058823529411765
                 0.3058823529411765]},
   "punctuation.special"
   #:text-style{:color
                [0.3058823529411765
                 0.3058823529411765
                 0.3058823529411765]},
   "variable"
   #:text-style{:color
                [0.8156862745098039
                 0.8156862745098039
                 0.8156862745098039]},
   "keyword"
   #:text-style{:color [0.37254901960784315 0.0 0.8431372549019608]},
   "number"
   {:text-style/color [0.5294117647058824 0.37254901960784315 0.0],
    :font-style #:font-style{:weight :bold}},
   "property" #:text-style{:color [0.6862745098039216 0.0 0.0]},
   "function"
   #:text-style{:color [0.0 0.37254901960784315 0.8431372549019608]},
   "function.builtin"
   {:text-style/color [0.0 0.37254901960784315 0.8431372549019608],
    :font-style #:font-style{:weight :bold}},
   "type"
   #:text-style{:color [0.0 0.37254901960784315 0.37254901960784315]},
   "constant.builtin"
   {:text-style/color [0.5294117647058824 0.37254901960784315 0.0],
    :font-style #:font-style{:weight :bold}},
   "property.builtin"
   {:text-style/color [0.6862745098039216 0.0 0.0],
    :font-style #:font-style{:weight :bold}},
   "comment"
   {:text-style/color
    [0.5411764705882353 0.5411764705882353 0.5411764705882353],
    :font-style #:font-style{:slant :italic}},
   "variable.parameter"
   #:text-style{:color
                [0.8156862745098039
                 0.8156862745098039
                 0.8156862745098039]},
   "constructor"
   #:text-style{:color [0.6862745098039216 0.5294117647058824 0.0]},
   "variable.builtin"
   {:text-style/color
    [0.8156862745098039 0.8156862745098039 0.8156862745098039],
    :font-style #:font-style{:weight :bold}}})



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
          (let [bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))

                next-char (.following bi cursor-char)
                diff-string (-> (.subSequence rope cursor-char next-char)
                                .toString)]
            (first
             (para/get-rects-for-range para cursor-char (+ cursor-char (.length diff-string))
                                       :max
                                       :tight))))]
    (if (not rect)
      (throw (ex-info "No cursor rect."
                      {}))
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
                       :style style
                       })
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

(defn highlight-search [editor {:keys [char-offset start-byte-offset end-byte-offset]}]
  (when-let [^java.util.regex.MatchResult
             match (-> editor ::text-mode/search :match)]
    (when (>= (.start match)
              char-offset)
      (let [^Rope
            rope (:rope editor)
            diff-string (.toString
                         (.subSequence rope char-offset (.start match)))
            diff-bytes (alength (.getBytes diff-string "utf-8"))

            start-byte (+ start-byte-offset diff-bytes)
            end-byte (+ start-byte (alength (.getBytes (.group match) "utf-8")))]
        {[start-byte end-byte] {:text-style/background-color {:color [1 0 1 0.2]}}})))
  )

(defn syntax-style [editor
                    ;; queries
                    ^TSQuery
                    query
                    theme
                    {:keys [start-byte-offset end-byte-offset]}]
  (let [^TSQueryCursor qc (TSQueryCursor.)
        ;; ^TSQuery query (TSQuery. (:language editor)
        ;;                          queries)


        base-style (:base-style editor)
        ^TSTree tree (:tree editor)
        ^Rope rope (:rope editor)

        _ (.setByteRange qc start-byte-offset end-byte-offset)
        _ (.exec qc query (.getRootNode tree))
        matches (.getCaptures qc)
        styles (loop [offset start-byte-offset
                      styles {}]
                 (if (.hasNext matches)
                   (let [match (.next matches)
                         ^TSQueryCapture
                         capture (aget (.getCaptures match) (.getCaptureIndex match))
                         capture-name (.getCaptureNameForId query (.getIndex capture))
                         node (.getNode capture)]
                       ;; else
                       (let [start-byte (min end-byte-offset (.getStartByte node))
                             end-byte (min end-byte-offset (.getEndByte node))

                             styles (if (> end-byte start-byte)
                                      (let [style (get theme capture-name)]
                                        (assoc styles [start-byte end-byte] style))
                                      ;; else
                                      styles)]
                         (recur end-byte
                                styles)))
                   ;; else
                   styles))]
    styles))

(comment
  (let [editor (-> (clojure-mode/make-editor)
                   (text-mode/editor-self-insert-command "(defn foo \n[42])"))]
    (syntax-style editor
                 clojure-mode/highlight-queries
                 default-theme
                 {:start-byte-offset 0
                  :end-byte-offset (-> editor :rope .numBytes)}))
  ,)


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
        extent {:start-byte-offset start-byte-offset
                :end-byte-offset end-byte-offset
                :char-offset char-offset}

        base-style (:base-style editor)
        

        text (styled-text rope
                          (:base-style editor)
                          [(when tree
                             (when-let [query (:query editor)]
                               (when-let [theme (:theme editor)]
                                 (syntax-style editor
                                               query
                                               theme
                                               extent))))
                           (selection-style editor extent)
                           (highlight-search editor extent)]
                          start-byte-offset
                          end-byte-offset)
        para (para/paragraph text
                             nil {:paragraph-style/text-style (:base-style editor)})
        para (assoc para
                    :char-offset char-offset
                    :start-byte-offset start-byte-offset
                    :end-byte-offset end-byte-offset)]
    para))

(defeffect ::update-search-editor [{:keys [$editor op $main-editor main-editor update-editor-intent]}]
  (dispatch! :update $editor op)
  (let [search-editor (dispatch! :get $editor)]
    (dispatch! update-editor-intent 
               {:editor main-editor
                :$editor $main-editor
                :op #(text-mode/editor-isearch-forward % (.toString (:rope search-editor)))})))

(defui search-bar [{:keys [editor update-editor-intent] :as m}]
  (let [search-state (::text-mode/search editor)
        search-editor (get search-state ::search-editor)
        search-editor-body (para/paragraph 
                            ["isearch forward: "
                             (.toString (:rope search-editor))]
                            nil
                            {:paragraph-style/text-style
                             (:base-style editor)})]
    (ui/on
     ::cancel-search
     (fn [m]
       [[update-editor-intent {:editor editor
                               :$editor $editor
                               :op #'text-mode/editor-cancel-search}]])
     ::finish-search
     (fn [m]
       [[update-editor-intent {:editor editor
                               :$editor $editor
                               :op #'text-mode/editor-finish-search-forward}]])
     ::repeat-search
     (fn [m]
       [[update-editor-intent {:editor editor
                               :$editor $editor
                               :op #'text-mode/editor-isearch-forward}]])
     ::update-search-editor
     (fn [m]
       [[::update-search-editor (assoc m 
                                       :$main-editor $editor
                                       :main-editor editor
                                       :update-editor-intent update-editor-intent)]])
     (key-binding/wrap-editor-key-bindings
      {:body search-editor-body
       :$body nil
       :editor search-editor
       :update-editor-intent ::update-search-editor
       :key-bindings {"C-g" ::cancel-search
                      "RET" ::finish-search
                      "C-s" ::repeat-search
                      "DEL" #'text-mode/editor-delete-backward-char}}))))


(defn upkeep-search-ui [editor new-editor]
  (if (and (::text-mode/search new-editor)
           (not (::text-mode/search editor)))
    (update new-editor ::text-mode/search
            (fn [m]
              (assoc m ::search-editor (text-mode/make-editor))))
    new-editor))

(defui file-picker [{:keys [editor update-editor-intent] :as m}]
  (let [file-picker-state (::file-picker-state editor)
        ^File current-folder (get file-picker-state :current-folder)
        
        search-editor (get file-picker-state :search-editor)
        base-style (:base-style search-editor)
        offset (get extra :offset 0)
        search-str (-> search-editor :rope .toString)
        search-str-lower (str/lower-case search-str)

        fs (into 
            []
            (comp (filter (fn [^File f]
                            (str/includes? (str/lower-case (.getName f))
                                           search-str-lower)))
                  (drop offset))
            (sort (.listFiles current-folder)))
        ps (into 
            [{:text (str (.getCanonicalPath current-folder) "/")
              :style (assoc base-style :text-style/color [0 0 0.843])}
             search-str
             " | "]
            (comp (map-indexed 
                   (fn [i ^File f]
                     (let [style (if (zero? i)
                                   (assoc base-style 
                                          :text-style/font-style
                                          {:font-style/weight :bold})
                                   base-style)]
                       (if (.isFile f)
                         {:style style
                          :text (.getName f)}
                         {:text (str (.getName f) "/")
                          :style (assoc style :text-style/color [0.909 0.2784 0.3411])}))))
                  (interpose " | "))
            fs)
        
        search-editor-body 
        (para/paragraph
         ps
         nil
         {:paragraph-style/text-style base-style})]

    (ui/on
     ::cancel-search
     (fn [m]
       [[update-editor-intent {:editor editor
                               :$editor $editor
                               :op #(dissoc % ::file-picker-state)}]])
     ::select-file
     (fn [m]
       (when-let [^File f (first fs)]
         (if (.isFile f)
           [[::select-file {:file f}]
            [update-editor-intent {:editor editor
                               :$editor $editor
                               :op #(dissoc % ::file-picker-state)}]]
           [[:set $current-folder f]
            [:update $search-editor (fn [editor]
                                      (-> editor
                                          (text-mode/editor-beginning-of-buffer)
                                          (text-mode/editor-set-string "")))]
            [:set $offset 0]])))
     ::next-offset
     (fn [m]
       [[:update $offset 
                       (fn [offset]
                         (if (> offset (count fs))
                           0
                           (inc offset)))]])
     ::backspace
     (fn [m]
       (if (= search-str "")
         (when-let [parent (-> current-folder
                               .getCanonicalFile
                               .getParentFile)]
           [[:set $current-folder parent]
            [:set $offset 0]])
         [[:update $search-editor #'text-mode/editor-delete-backward-char]
          [:set $offset 0]]))
     ::update-search-editor
     (fn [{:keys [;; shadowing with $editor is buggy
                  ;; use $search-editor directly
                  #_$editor
                  op] :as m}]
       
       [[:update $search-editor op]])
     (key-binding/wrap-editor-key-bindings
      {:body search-editor-body
       :$body nil
       :editor search-editor
       :update-editor-intent ::update-search-editor
       :key-bindings {"C-g" ::cancel-search
                      "RET" ::select-file
                      "C-s" ::next-offset
                      "DEL" ::backspace }}))))


(defeffect ::select-file [{:keys [file]}]
  (dispatch! :com.phronemophobic.easel/add-applet
             {:make-applet
              (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                #(f % {:file file}))}))

(defeffect ::file-picker [{:keys [editor $editor]}]
  (dispatch! :update $editor
             (fn [editor]
               (if-let [file (:file editor)]
                 (assoc editor ::file-picker-state
                        {:current-folder (.getParentFile ^File (:file editor))
                         :search-editor (-> (text-mode/make-editor)
                                            (assoc :base-style (:base-style editor)))})
                 ;; else
                 editor))))

(defui status-bar [{:keys [editor]}]
  (let [^File file (:file editor)
        cursor (:cursor editor)
        ps [(when file
              (.getName file))
            " ("
            (-> cursor :row str)
            ","
            (-> cursor :column-byte str)
            ") "
            (str (:eval-ns editor))]]
    (para/paragraph ps
                    nil
                    {:paragraph-style/text-style (:base-style editor)})))

