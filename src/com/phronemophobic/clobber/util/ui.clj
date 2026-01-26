(ns com.phronemophobic.clobber.util.ui
  (:require [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [clojure.java.io :as io]
            [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.clobber.util :as util])
  (:import (org.treesitter TSQuery
                           TSTree
                           TSNode
                           TSQueryCapture
                           TSQueryCursor)
           com.ibm.icu.text.BreakIterator
           java.io.File
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


(defn editor-set-height [editor height]
  (let [base-style (:base-style editor)
        row-height (* (:text-style/height base-style)
                      (:text-style/font-size base-style))
        adjusted-height (if-let [adjust-height (-> editor ::ui :adjust-height)]
                          (adjust-height height)
                          height)
        num-lines (max 0
                       (- (quot adjusted-height row-height)
                          ;; reserve two lines for status bar
                          ;; and one line for cursor info
                          2))]
    (-> editor
        (assoc-in [:viewport :num-lines] (long num-lines))
        (assoc-in [:viewport :text-height] (* row-height num-lines))
        (assoc-in [:viewport :height] height)
        (assoc-in [:viewport :adjusted-height] adjusted-height))))

(defn ui-upkeep [old-editor new-editor]
  (let [old-ui (::ui old-editor)
        new-ui (::ui new-editor)
        
        new-editor (if (and (or (nil? new-ui) (nil? old-ui))
                            (or new-ui old-ui))
                     (if-let [height (-> new-editor :viewport :height)]
                       (editor-set-height new-editor height)
                       new-editor)
                     new-editor)
        new-editor (if (and old-ui (not= old-ui new-ui))
                     (if-let [close-fn (::close-fn old-ui)]
                       (close-fn new-editor)
                       new-editor)
                     new-editor)]
    new-editor))

(defn cursor-view [^Rope rope para cursor]
  (let [cursor-char (- (:char cursor)
                       (get para :char-offset 0))
        rope (or (when-let [end-byte-offset (:end-byte-offset para)]
                   (when-let [start-byte-offset (:start-byte-offset para)]
                     (.sliceBytes rope
                                  start-byte-offset
                                  end-byte-offset)))
                 rope)
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
      (println "No cursor rect.")
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
                :op (fn [editor]
                      (if (= (-> editor ::text-mode/search :direction)
                             :backward)
                        (text-mode/editor-isearch-backward editor (.toString ^Rope (:rope search-editor)))
                        (text-mode/editor-isearch-forward editor (.toString ^Rope (:rope search-editor)))))})))

(defui search-bar [{:keys [editor update-editor-intent body] :as m}]
  (let [search-state (::text-mode/search editor)
        search-editor (get search-state ::search-editor)
        direction (if (= :backward (:direction search-state))
                    :backward
                    :forward)
        search-editor-body (para/paragraph 
                            [(if (= direction :backward)
                               "isearch backward: "
                               "isearch forward: ")
                             (.toString ^Rope (:rope search-editor))]
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
     ::repeat-search-forward
     (fn [m]
       [[update-editor-intent {:editor editor
                               :$editor $editor
                               :op #'text-mode/editor-isearch-forward}]])
     ::repeat-search-backward
     (fn [m]
       [[update-editor-intent {:editor editor
                               :$editor $editor
                               :op #'text-mode/editor-isearch-backward}]])
     ::update-search-editor
     (fn [m]
       [[::update-search-editor (assoc m 
                                       :$main-editor $editor
                                       :main-editor editor
                                       :update-editor-intent update-editor-intent)]])
     (ui/vertical-layout
      body
      (key-binding/wrap-editor-key-bindings
       {:body search-editor-body
        :$body nil
        :editor search-editor
        :update-editor-intent ::update-search-editor
        :key-bindings {"C-g" ::cancel-search
                       "RET" ::finish-search
                       "C-s" ::repeat-search-forward
                       "C-r" ::repeat-search-backward
                       "DEL" #'text-mode/editor-delete-backward-char}})))))


(defn upkeep-search-ui [editor new-editor]
  (cond
    (and (::text-mode/search new-editor)
         (not (::text-mode/search editor))
         (not (::ui new-editor)))
    (-> new-editor
        (update ::text-mode/search
                (fn [m]
                  (assoc m ::search-editor (text-mode/make-editor))))
        (assoc ::ui (search-bar {:adjust-height
                                 (fn [height]
                                   (max 0 (- height 20)))
                                 :extra {}})))
    
    (and (not (::text-mode/search new-editor))
         (::text-mode/search editor))
    (dissoc new-editor ::ui)

    :else new-editor))

(def ^:private user-home*
  (delay (System/getProperty "user.home")))
(defui file-picker [{:keys [editor update-editor-intent body] :as m}]
  (let [file-picker-state (::file-picker-state editor)
        ^File current-folder (get file-picker-state :current-folder)
        
        search-editor (get file-picker-state :search-editor)
        base-style (:base-style search-editor)
        offset (get extra :offset 0)
        search-str (.toString ^Rope (:rope search-editor))
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
                               :op #(dissoc % ::file-picker-state ::ui)}]])
     ::select-file
     (fn [m]
       (let [^File f (if (and (= search-str "~")
                              @user-home*)
                       (io/file @user-home*)
                       (first fs))]
         (when f
           (if (.isFile f)
             [[update-editor-intent {:editor editor
                                     :$editor $editor
                                     :op #(dissoc % ::file-picker-state ::ui)}]
              [::select-file {:file f}]]
             [[:set $current-folder f]
              [:update $search-editor (fn [editor]
                                        (-> editor
                                            (text-mode/editor-beginning-of-buffer)
                                            (text-mode/editor-set-string "")))]
              [:set $offset 0]]))))
     ::next-offset
     (fn [m]
       [[:update $offset 
                       (fn [offset]
                         (if (<= (count fs) 1)
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
     ::goto-root
     (fn [m]
       (if (= search-str "")
         [[:set $current-folder (io/file "/")]
          [:set $offset 0]]
         (let [^File 
               f (if (and (= search-str "~")
                          @user-home*)
                   (io/file @user-home*)
                   (first fs))]
           (when (and f (File/.isDirectory f))
             [[:set $current-folder f]
              [:update $search-editor (fn [editor]
                                        (-> editor
                                            (text-mode/editor-beginning-of-buffer)
                                            (text-mode/editor-set-string "")))]
              [:set $offset 0]]))))
     ::kill-word-backward
     (fn [m]
       (if (= search-str "")
         (when-let [parent (-> current-folder
                               .getCanonicalFile
                               .getParentFile)]
           [[:set $current-folder parent]
            [:set $offset 0]])
         [[:update $search-editor (fn [editor]
                                      (-> editor
                                          (text-mode/editor-beginning-of-buffer)
                                          (text-mode/editor-set-string "")))]
          [:set $offset 0]]))
     ::update-search-editor
     (fn [{:keys [;; shadowing with $editor is buggy
                  ;; use $search-editor directly
                  #_$editor
                  op] :as m}]
       
       [[:set $offset 0]
        [:update $search-editor op]])
     (ui/vertical-layout
      body
      (key-binding/wrap-editor-key-bindings
       {:body search-editor-body
        :$body nil
        :editor search-editor
        :update-editor-intent ::update-search-editor
        :key-bindings {"C-g" ::cancel-search
                       "RET" ::select-file
                       "M-DEL" ::kill-word-backward
                       "/" ::goto-root
                       "C-s" ::next-offset
                       "DEL" ::backspace }})))))


(defeffect ::select-file [{:keys [file]}]
  (dispatch! :com.phronemophobic.easel/add-applet
             {:make-applet
              (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                #(f % {:file file}))}))

(defeffect ::file-picker [{:keys [editor $editor update-editor-intent]}]
  (dispatch! (if update-editor-intent
               update-editor-intent
               (if (instance? org.treesitter.TreeSitterClojure (:language editor))
                 :com.phronemophobic.clobber.modes.clojure.ui/update-editor
                 :com.phronemophobic.clobber.modes.text.ui/update-editor))
             {:editor editor
              :$editor $editor
              :op (fn [editor]
                    (let [parent-file (if-let [file (:file editor)]
                                        (.getParentFile ^File file)
                                        (clojure.java.io/file "."))
                          editor (assoc editor
                                        ::file-picker-state
                                        {:current-folder parent-file
                                         :search-editor (-> (text-mode/make-editor)
                                                            (assoc :base-style (:base-style editor)))}
                                        ::ui (file-picker {:adjust-height (fn [height]
                                                                            (max 0
                                                                                 (- height 20)))
                                                           :$editor $editor
                                                           :extra {}
                                                           :$extra [$editor (com.rpl.specter/must ::ui) '(keypath :extra)]}))]
                      editor))}))

(defui status-bar [{:keys [editor width]}]
  (let [^File file (:file editor)
        cursor (:cursor editor)
        ps [(when file
              (.getName file))
            " ("
            (-> cursor :row str)
            ","
            (-> cursor :column-byte str)
            ") "
            (when (:instarepl? editor)
              "⚡")
            (str (:eval-ns editor))]
        
        body (para/paragraph ps
                             width
                             {:paragraph-style/text-style (:base-style editor)})
        body (if width
               (let [gray 0.85]
                  (ui/fill-bordered [gray gray gray]
                                 1
                                 body))
               body)]
    body))

(defeffect ::cancel-replace [{:keys [$editor update-editor-intent]}]
  (dispatch! update-editor-intent {:$editor $editor
                                   :op (fn [editor]
                                         (dissoc editor ::ui))}))



(defui find-replace-view [{:keys [focused? body update-editor-intent editor]}]
  (let [{:keys [from-editor to-editor subfocus]} extra]
    (ui/on
     ::update-replace-editor
     (fn [m]
       [[:update (:$editor m) (:op m)]])
     (let [base-style (:base-style from-editor)
           from-editor-para (para/paragraph
                             (str (:rope from-editor))
                             nil
                             {:paragraph-style/text-style base-style})
           
           focus (when focused?
                   (or subfocus :from))
           
           from-editor-cursor (when (= focus :from)
                                (cursor-view (:rope from-editor)
                                             from-editor-para
                                             (:cursor from-editor)))
           
           to-editor-para (para/paragraph
                           (str (:rope to-editor))
                           nil
                           {:paragraph-style/text-style base-style})
           to-editor-cursor (when (= focus :to)
                              (cursor-view (:rope to-editor)
                                           to-editor-para
                                           (:cursor to-editor)))
           
           find-replace-ui (ui/table-layout
                            [[(ant/button {:text "Find"
                                           :size :small
                                           :on-click
                                           (fn []
                                             [[::find-replace-find {}]])})
                              (ui/on
                               :mouse-down
                               (fn [_]
                                 [[:set $subfocus :from]])
                               (ui/bordered
                                4
                                [(ui/spacer 100 0)
                                 [from-editor-para
                                  from-editor-cursor]]))]
                             [(ant/button {:text "Replace"
                                           :on-click
                                           (fn []
                                             [[::find-replace-replace {}]])
                                           :size :small})
                              (ui/on
                               :mouse-down
                               (fn [_]
                                 [[:set $subfocus :to]])
                               (ui/bordered
                                4
                                [(ui/spacer 100 0)
                                 [to-editor-para
                                  to-editor-cursor]]))]]
                            4 3)
           find-replace-ui (if focused?
                             (ui/on
                              ::cancel-replace
                              (fn [_]
                                [[::cancel-replace {:$editor $editor
                                                    :update-editor-intent update-editor-intent}]])
                              ::find-replace-focus-other
                              (fn [_]
                                [[:update $subfocus (fn [subfocus]
                                                      (if (= subfocus :to)
                                                        :from
                                                        :to))]])

                              (key-binding/wrap-editor-key-bindings
                               {:body find-replace-ui
                                :$body nil
                                :editor (if (= focus :from)
                                          from-editor
                                          to-editor)
                                :$editor (if (= focus :from)
                                           $from-editor
                                           $to-editor)
                                :update-editor-intent ::update-replace-editor
                                :key-bindings {"C-g" ::cancel-replace
                                               "RET" ::find-replace-find
                                               "TAB" ::find-replace-focus-other
                                               "DEL" #'text-mode/editor-delete-backward-char}}))
                             find-replace-ui)
           find-replace-ui
           (ui/on
            ::find-replace-replace
            (fn [_]
              [[update-editor-intent {:$editor $editor
                                      :op (fn [editor]
                                            (text-mode/editor-replace editor 
                                                                      (str (:rope to-editor))))}]])
            ::find-replace-find
            (fn [_]
              [[update-editor-intent {:$editor $editor
                                      :op (fn [editor]
                                            (text-mode/editor-isearch-forward editor 
                                                                              (str (:rope from-editor))))}]])
            find-replace-ui)]
       (ui/vertical-layout
        body
        find-replace-ui)))))

(defeffect ::show-find-replace [{:keys [$editor update-editor-intent] :as m}]
  (dispatch! update-editor-intent
             {:$editor $editor
              :op (fn [editor]
                    (let [ui (find-replace-view 
                              {:extra {:from-editor (text-mode/make-editor)
                                       :to-editor (text-mode/make-editor)}
                               ::close-fn
                               (fn [editor]
                                 (text-mode/editor-finish-search-forward editor))})
                          ui (assoc ui :adjust-height
                                    (fn [height]
                                      (max 0 (- height (ui/height ui)))))]
                      (assoc editor ::ui ui)))}))

(defui find-replace-view+focus [{:keys [focus-id] :as m}]
  (let [focus (:focus context)
        [cw ch] (:membrane.stretch/container-size context)]
    (ui/wrap-on
     :mouse-down
     (fn [handler mpos]
       (let [focus-intents (if focus-id
                             [[:com.phronemophobic.easel/request-focus focus-id]]
                             (let [fid (Object.)]
                               [[:set $focus-id fid]
                                [:com.phronemophobic.easel/request-focus fid]]))]
         (into focus-intents (handler mpos))))
     (ui/fixed-bounds
      [cw ch]
      (find-replace-view {:extra extra
                          :focused? (and focus-id (identical? focus focus-id))})))))


(comment
  
  (dev/add-component-as-applet
   #'find-replace-view+focus
   {:extra {:from-editor (text-mode/make-editor)
            :to-editor (text-mode/make-editor)}
    :focus-id nil})
  
  ,)



(defeffect ::goto-line [{:keys [$editor update-editor-intent line-str]}]
  (let [line (parse-long line-str)]
    (dispatch! update-editor-intent
               {:$editor $editor
                :op (fn [editor]
                      (let [editor (dissoc editor ::ui)]
                        (if line
                          (text-mode/editor-goto-row-col editor line 0)
                          ;; parse failed
                          editor)))})))

(defui goto-line-view [{:keys [body editor update-editor-intent]}]
  (let [line-number-editor (:line-number-editor extra)
        
        line-str (str (:rope line-number-editor))
        input-view (ui/no-events
                    (ant/text-input*
                     {:flex-layout.stretch/width 75
                      :focused? true
                      :cursor (-> line-number-editor
                                  :cursor
                                  :char)
                      :text line-str}))
        input-view [(ui/filled-rectangle [1 1 1]
                                         (ui/width input-view)
                                         (ui/height input-view))
                    input-view]
        input-view (ui/center input-view (ui/bounds body))

        input-view 
        (ui/on
         ::update-replace-editor
         (fn [m]
           [[:update $line-number-editor (:op m)]])
         ::cancel-goto-line
         (fn [m]
           [[update-editor-intent {:editor editor
                                   :$editor $editor
                                   :op #(dissoc % ::ui)}]])
         ::goto-line
         (fn [m]
           [[::goto-line {:$editor $editor
                          :update-editor-intent update-editor-intent
                          :line-str line-str}]])
         (key-binding/wrap-editor-key-bindings
                    {:body input-view
                     :$body nil
                     :editor line-number-editor
                     :update-editor-intent ::update-replace-editor
                     :key-bindings {"C-g" ::cancel-goto-line
                                    "RET" ::goto-line
                                    "DEL" #'text-mode/editor-delete-backward-char}}))]
    [body
     input-view]))

(defeffect ::show-goto-line [{:keys [$editor update-editor-intent] :as m}]
  (dispatch! update-editor-intent
             {:$editor $editor
              :op (fn [editor]
                    (let []
                      (assoc editor ::ui
                             (goto-line-view {:extra {:line-number-editor (text-mode/make-editor)}}))))}))

(comment
  
  (dev/add-component-as-applet #'goto-line-view {:extra {:line-number-editor (text-mode/make-editor)}
                                                 :body (ui/spacer 400 400)})
  ,)


(defeffect ::insert-rectangle [{:keys [$editor update-editor-intent s]}]
  (dispatch! update-editor-intent
             {:$editor $editor
              :op (fn [editor]
                    (let [editor (dissoc editor ::ui)]
                      (text-mode/editor-string-insert-rectangle editor s)))}))

(defui insert-rectangle-view [{:keys [body editor update-editor-intent]}]
  (let [string-editor (:string-editor extra)
        
        editor-width (:width editor)
        s (str (:rope string-editor))
        input-view (ui/no-events
                    (ant/text-input*
                     {:flex-layout.stretch/width (max 0
                                                      (- editor-width 80))
                      :focused? true
                      :cursor (-> string-editor
                                  :cursor
                                  :char)
                      :text s}))
        input-view [(ui/filled-rectangle [1 1 1]
                                         (ui/width input-view)
                                         (ui/height input-view))
                    input-view]
        
        [bw bh] (ui/bounds body)
        input-view (ui/center input-view [editor-width bh])

        input-view 
        (ui/on
         ::update-replace-editor
         (fn [m]
           [[:update $string-editor (:op m)]])
         ::cancel-insert-rectangle
         (fn [m]
           [[update-editor-intent {:editor editor
                                   :$editor $editor
                                   :op #(dissoc % ::ui)}]])
         ::insert-rectangle
         (fn [m]
           [[::insert-rectangle {:$editor $editor
                                 :update-editor-intent update-editor-intent
                                 :s s}]])
         (key-binding/wrap-editor-key-bindings
                    {:body input-view
                     :$body nil
                     :editor string-editor
                     :update-editor-intent ::update-replace-editor
                     :key-bindings {"C-g" ::cancel-insert-rectangle
                                    "RET" ::insert-rectangle
                                    "DEL" #'text-mode/editor-delete-backward-char}}))]
    [body
     input-view]))

(defeffect ::show-insert-rectangle [{:keys [$editor update-editor-intent] :as m}]
  (dispatch! update-editor-intent
             {:$editor $editor
              :op (fn [editor]
                    (assoc editor ::ui
                           (insert-rectangle-view {:extra {:string-editor (text-mode/make-editor)}})))}))