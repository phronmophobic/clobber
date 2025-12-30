(ns com.phronemophobic.clobber.modes.text.ui
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
            [com.phronemophobic.clobber.util.ui :as util.ui])
  (:import (org.treesitter TSQuery
                           TSParser)
           java.io.File
           io.lacuna.bifurcan.Rope))

(defn ^:private file-ext [^File f]
  (let [fname (.getName f)
        idx (.lastIndexOf fname ".")]
    (when (not= -1 idx)
      (subs fname idx))))

(defn set-language [editor lang-kw]
  (let [lang-str (name lang-kw)
        class-name (str "org.treesitter.TreeSitter" (str/capitalize lang-str))
        class (Class/forName class-name)
        lang (.newInstance class)
        queries (slurp (io/resource (str "com/phronemophobic/clobber/queries/tree-sitter-"
                                         lang-str
                                         "/highlights.scm")))
        query (TSQuery. lang queries)]
    (assoc editor
           :language lang
           :parser (doto (TSParser.)
                     (.setLanguage lang))
           :buf (byte-array 4096)
           :theme util.ui/default-theme
           :queries queries
           :query query)))

(def suffix->language-kw
  {".json" :json
   ".sh" :bash
   ".cpp" :cpp
   ".html" :html
   ".css" :css
   ".java" :java
   ".js" :javascript
   ".py" :python
   ".c" :c
   ".h" :c})

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
                     new-editor)
        
        new-editor (util.ui/upkeep-search-ui editor new-editor)
        new-editor (util.ui/ui-upkeep editor new-editor)]
    new-editor))



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

(defeffect ::editor-copy [{:keys [$editor editor]}]
  (when-let [select-cursor (:select-cursor editor)]
    (let [cursor (:cursor editor)
          start-byte (min (:byte select-cursor)
                          (:byte cursor))
          end-byte (max (:byte select-cursor)
                        (:byte cursor))
          
          ^Rope rope (:rope editor) 
          selected-text (-> (.sliceBytes rope start-byte end-byte)
                            .toString)]
      (dispatch! :clipboard-copy selected-text)
      (dispatch! :update $editor
                 (fn [editor]
                   (editor-upkeep editor
                                  #(dissoc % :select-cursor)))))))

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
            previous-source (.toString ^Rope (:rope editor))]
        (dispatch! :set $editor
          (editor-upkeep editor
          #(text-mode/editor-set-string % source))))))
  nil)

(defeffect ::file-picker [{:keys [$editor editor] :as m}]
  (dispatch! ::util.ui/file-picker (assoc m
                                          :update-editor-intent ::update-editor)))

(defn editor-cancel [editor]
  (dissoc editor :select-cursor ::util.ui))

(def key-bindings
  (assoc text-mode/key-bindings
         "C-x C-s" ::save-editor
         "C-x C-f" ::file-picker
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
                                key-bindings))))))

(defn make-editor
  ([{:keys [file source] :as m}]
   (let [editor (-> (make-editor)
                    #_(assoc :background-chan (editor-background-runner)))
         
         ;; contents
         editor
         (cond
           source (text-mode/editor-insert editor source 0 0 0)
           
           file (let [source (slurp file)
                      last-file-load (java.time.Instant/now)
                      editor (if-let [lang-kw (suffix->language-kw (file-ext file))]
                               (set-language editor lang-kw)
                               editor)]
                  (-> editor
                      (text-mode/editor-insert source 0 0 0)
                      (assoc :last-file-load last-file-load)))
           
           :else editor)
         
         ;; set separately from contents:
         ;; - may override file from eval-ns
         ;; - may be explicitly provided with :source
         editor (if file
                  (assoc editor :file file)
                  editor)]
     editor))
  ([]
   (-> (text-mode/make-editor)
       (assoc :viewport {:num-lines 52
                         :start-line 0}
              :key-bindings key-bindings
              :base-style
              #:text-style
              {:font-families ["Menlo"]
              :font-size 12
              :height 1.2
              :height-override true}))))

(defn make-editor-from-file [f]
  (let [source (slurp f)]
    (-> (make-editor)
        (assoc :file f))))



(defui editor-view [{:keys [editor]}]
  
  (let [lang (:language editor)
        rope (:rope editor)
        para (util.ui/editor->paragraph editor)
        
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
    [(util.ui/cursor-view rope para (:cursor editor))
     para
     status-bar]))


(defui text-editor [{:keys [editor
                            focused?]
                     :as this}]
  (let [body (editor-view {:editor editor
                           :focused? focused?})
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
        body (ui/vertical-layout
              body
              (util.ui/status-bar {:editor editor
                                   :width (:width editor)}))]
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


(defeffect ::tap [& args]
  (case (count args)
    0 nil
    1 (tap> (first args))
    2 (tap> (vec args))))

(comment
  (dev)
  (dev (io/file "Readme.md"))
  (dev (io/file "../../todo.org"))

  ,)
