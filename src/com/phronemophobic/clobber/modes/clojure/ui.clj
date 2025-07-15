(ns com.phronemophobic.clobber.modes.clojure.ui
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
            compliment.core
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util :as util]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding])
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
           java.time.Duration
           java.time.Instant
           java.io.File
           java.io.StringReader
           io.lacuna.bifurcan.Rope))

(defn ^:private ns-sym->resource-path
  ([ns-sym]
   (ns-sym->resource-path ns-sym ".clj"))
  ([ns-sym ext]
   (let [resource-path (let [parts (-> ns-sym
                                       name
                                       (str/split #"\."))]
                         (str (str/join "/"
                                        (eduction
                                         (map #(str/replace % "-" "_"))
                                         parts))
                              ext))]
     resource-path)))

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
  (dispatch! :update $editor editor-upkeep op)
  ;; check and do background updates
  (let [editor (dispatch! :get $editor)]
    (when-let [ch (:background-chan editor)]
      (async/put! ch {:editor editor
                      :$editor $editor
                      :dispatch! dispatch!}))))

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

(defn ^:private file-ext [^File f]
  (let [fname (.getName f)
        idx (.lastIndexOf fname ".")]
    (when (not= -1 idx)
      (subs fname idx))))

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


(defeffect ::editor-eval-last-sexp [{:keys [editor $editor]}]
  (future
    (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
          {cursor-byte :byte
           cursor-char :char
           cursor-point :point
           cursor-row :row
           cursor-column-byte :column-byte} cursor
          node (util/previous-named-child-for-byte tree cursor-byte)]
      (when node
        (let [eval-ns (:eval-ns editor)
              eval-ns-name (ns-name eval-ns)
              line-number (-> node
                              .getEndPoint
                              .getRow)
              
              [;; path relative to class-path  
               source-path
               ;; filename
               source-name]
              (when-let [^File f (:file editor)]
                [(ns-sym->resource-path eval-ns-name (file-ext f))
                 (.getName f)])
              
              ;; tree-sitter line numbers are 0 indexed
              ;; these source line numbers are 1 indexed
              ;; we are adding a line for the ns form
              rdr (doto (LineNumberingPushbackReader.
                         (StringReader. 
                          (if (pos? line-number)
                            (str (pr-str
                                  `(ns ~eval-ns-name))
                                 "\n"
                                 (util/node->str rope node))
                            ;; else
                            (util/node->str rope node))))
                    (.setLineNumber line-number))
              
              val (clojure.lang.Compiler/load rdr source-path source-name)
              
              temp-view (ui/translate 0 -4
                                      (viscous/inspector
                                       {:obj (viscous/wrap val)
                                        :width 40
                                        :height 1
                                        :show-context? false}))]
          (dispatch! :update $editor
                     update :line-val
                     (fn [m]
                       (let [line-val (get m rope)]
                         {rope (assoc line-val line-number (viscous/wrap val))})))
          (dispatch! ::temp-status {:$editor $editor
                                    :msg temp-view}))))))

(defeffect ::editor-eval-top-form [{:keys [editor $editor]}]
  (future
    (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
          
          {cursor-byte :byte
           cursor-char :char
           cursor-point :point
           cursor-row :row
           cursor-column-byte :column-byte} cursor]
      (let [root-node (.getRootNode tree)
            cursor (TSTreeCursor. root-node)
            idx (.gotoFirstChildForByte cursor cursor-byte)]
        (if (= -1 idx)
          editor
          (try
            (let [eval-ns (:eval-ns editor)
                  eval-ns-name (ns-name eval-ns)
                  node (.currentNode cursor)
                  line-number (-> node
                                  .getEndPoint
                                  .getRow)
                  
                  [;; path relative to class-path  
                   source-path
                   ;; filename
                   source-name]
                  (when-let [^File f (:file editor)]
                    [(ns-sym->resource-path eval-ns-name (file-ext f))
                     (.getName f)])
                  
                  ;; tree-sitter line numbers are 0 indexed
                  ;; these source line numbers are 1 indexed
                  ;; we are adding a line for the ns form
                  rdr (doto (LineNumberingPushbackReader.
                             (StringReader. 
                              (if (pos? line-number)
                                (str (pr-str
                                      `(ns ~eval-ns-name))
                                     "\n"
                                     (util/node->str rope node))
                                ;; else
                                (util/node->str rope node))))
                        (.setLineNumber line-number))

                  val (clojure.lang.Compiler/load rdr source-path source-name)
                  
                  temp-view (ui/translate 0 -4
                                          (viscous/inspector
                                           {:obj (viscous/wrap val)
                                            :width 40
                                            :height 1
                                            :show-context? false}))]
              (dispatch! :update $editor
                         update :line-val
                         (fn [m]
                           (let [line-val (get m rope)]
                             {rope (assoc line-val line-number (viscous/wrap val))})))
              (dispatch! ::temp-status {:$editor $editor
                                        :msg temp-view})
              )
            (catch Exception e
              (dispatch! ::temp-status {:$editor $editor
                                        :msg "Exception!"})
              (prn e))))))))

(defeffect ::load-buffer [{:keys [editor $editor]}]
  (future
    (let [eval-ns (:eval-ns editor)
          eval-ns-name (ns-name eval-ns)
          
          [;; path relative to class-path  
           source-path
           ;; filename
           source-name] (when-let [^File f (:file editor)]
                          [(ns-sym->resource-path eval-ns-name (file-ext f))
                           (.getName f)])
          
          
          rdr (LineNumberingPushbackReader.
               (StringReader. (-> editor :rope .toString)))]
      (try
        (clojure.lang.Compiler/load rdr source-path source-name)
        (dispatch! ::temp-status {:$editor $editor
                                  :msg "buffer loaded."})
        (catch Exception e
          (dispatch! ::temp-status {:$editor $editor
                                    :msg "Exception!"})
          (prn e))))))

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
                   :last-save now))
      (dispatch! ::temp-status {:$editor $editor
                                :msg "saved."})))
  nil)

(defeffect ::tap-editor [{:keys [editor $editor]}]
  (tap> editor)
  nil)

(defui doc-viewer [{:keys [docstring]}]
  (let [width (-> context
                  :membrane.stretch/container-size
                  first)]
    (para/paragraph docstring width)))

(defeffect ::show-doc [{:keys [editor $editor]}]
  (when-let [^TSTree tree (:tree editor)]
    (let [tc (TSTreeCursor. (.getRootNode tree))
          cursor (:cursor editor)
          cursor-byte (:byte cursor)
          rope (:rope editor)

          ^TSNode
          node
          (transduce
           (comp (take-while (fn [^TSNode node]
                               (<= (-> node .getStartByte)
                                   cursor-byte)))
                 (filter (fn [^TSNode node]
                           (>= (-> node .getEndByte)
                               cursor-byte)))
                 (filter (fn [^TSNode node]
                           (= "sym_lit" (.getType node)))))
           (completing
            (fn [result node]
              node))
           nil
           (when (util/skip-to-byte-offset tc cursor-byte)
             (util/tree-cursor-reducible tc)))]
      (when node
        (let [node-string (util/node->str rope node)
              sym (read-string node-string)
              v (ns-resolve (:eval-ns editor) sym)
              mta (meta v)
              doc (:doc mta)]
          (when (or (seq? (:arglists mta))
                    (string? doc))
            (let [arglists (when (seq? (:arglists mta))
                             (str/join
                              "\n"
                              (eduction
                               (map pr-str)
                               (:arglists mta))))]
              ;; This should really be a more general
              ;; intent that easel's implementation catches and implements
              (dispatch! 
               :com.phronemophobic.easel/add-applet
               {:id ::doc-viewer
                :make-applet
                (fn [_]
                  ((requiring-resolve 
                    'com.phronemophobic.easel/map->ComponentApplet)
                   {:label "Doc"
                    :component-var #'doc-viewer
                    :initial-state {:docstring 
                                    [node-string
                                     "\n"
                                     arglists
                                     "\n"
                                     (when (string? doc)
                                       doc)]}}))}))))))))

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

        ^File current-folder (get extra :current-folder folder)
        
        offset (get extra :offset 0)
        search-str (get extra :search-str "")
        fs (into 
            []
            (comp (filter (fn [^File f]
                            (str/includes? (.getName f)
                                           search-str)))
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
                          :style (assoc style :text-style/color [0.909 0.2784 0.3411  ])}))))
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
                 (when-let [^File f (first fs)]
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
            previous-source (.toString ^Rope (:rope editor))]
        (dispatch! :set $editor
          (editor-upkeep editor
          #(text-mode/editor-set-string % source))))))
  nil)

(defn update-arglist [{:keys [$editor editor dispatch!] :as m}]
  ;; TODO: should try to cache results
  (when-let [^TSTree tree (:tree editor)]
    (let [rope (:rope editor)
          tc (TSTreeCursor. (.getRootNode tree))
          cursor (:cursor editor)
          cursor-byte (:byte cursor)
          
          ;; find parent coll
          ^TSNode
          parent-coll
          (transduce
           (comp (take-while (fn [^TSNode node]
                               (< (-> node .getStartByte)
                                  cursor-byte)))
                 (filter (fn [^TSNode node]
                           (contains? clojure-mode/coll-node-types (.getType node))
                           ))
                 (filter (fn [^TSNode node]
                           (> (-> node .getEndByte)
                              cursor-byte))))
           (completing
            (fn [result node]
              node))
           nil
           (when (util/skip-to-byte-offset tc cursor-byte)
             (util/tree-cursor-reducible tc)))]
      (if (and parent-coll (= "list_lit" (.getType parent-coll)))
        ;; try to find arglist
        (let [named-child-count (.getNamedChildCount parent-coll)]
          (when (pos? named-child-count)
            (let [first-child (.getNamedChild parent-coll 0)]
              (when (and (= "sym_lit" (.getType first-child)))
                (let [sym (read-string 
                           (util/node->str rope first-child))
                      v (ns-resolve (:eval-ns editor) sym)
                      
                      arglists (-> v meta :arglists)]
                  (when arglists
                    (let [cursor-index
                          (loop [index 1]
                            (if (>= index named-child-count)
                              (dec index)
                              (let [argnode (.getNamedChild parent-coll index)]
                                (if (<= cursor-byte (.getEndByte argnode))
                                  (dec index)
                                  (recur (inc index))))))
                          
                          base-style (:base-style editor)
                          ps (into [(str 
                                     (pr-str v)
                                     " ")]
                                   (comp
                                    (map (fn [arglist]
                                           ["["
                                            (into []
                                                  (comp
                                                   (map-indexed (fn [i node]
                                                                  (let [s (pr-str node)]
                                                                    (if (= i cursor-index)
                                                                      {:text s
                                                                       :style (assoc base-style :text-style/font-style {:font-style/weight :bold})}
                                                                      s))))
                                                   (interpose " "))
                                                  arglist)
                                            "]"]))
                                    (interpose " "))
                                   arglists)
                          p (para/paragraph
                             ps
                             nil
                             {:paragraph-style/text-style (:base-style editor)})]
                      (dispatch! :update $editor
                                 (fn [editor]
                                   (assoc-in editor [:status :status] p))))))))))
        ;; else, remove arglist
        (when (-> editor :status :status)
          (dispatch! :update $editor
                     (fn [editor]
                       (assoc-in editor [:status :status] nil))))))))

(declare editor->paragraph)
(defn update-completions [{:keys [$editor editor dispatch!] :as m}]
  (when (::completion editor)
    (let [^TSTree tree (:tree editor)
          rope (:rope editor)
          tc (TSTreeCursor. (.getRootNode tree))
          cursor (:cursor editor)
          cursor-byte (:byte cursor)
          
          ^TSNode
          [sym-node slash?]
          (transduce
           (comp (take-while (fn [^TSNode node]
                               (<= (-> node .getStartByte)
                                   cursor-byte)))
                 (filter (fn [^TSNode node]
                           (let [type (.getType node)]
                             (or (= "sym_lit" type)
                                 (= "/" type)))))
                 (filter (fn [^TSNode node]
                           ;; inc because of slashes
                           (>= (inc 
                                (-> node .getEndByte))
                               cursor-byte))))
           (completing
            (fn [[sym-node slash? last-node :as result] node]
              (let [type (.getType node)]
                (if (= "sym_lit" type)
                  (if (>= (-> node .getEndByte)
                          cursor-byte)
                    [node false nil]
                    [nil false node])
                  ;; special handling for incomplete symbols
                  ;; that end with a / like "clojure.string/"
                  (if (and (= "/" type)
                           last-node
                           (= (.getEndByte last-node)
                              (.getStartByte node)))
                    [last-node true nil]
                    result)))))
           
           nil
           (when (util/skip-to-byte-offset tc 
                                           ;; make sure to check include
                                           ;; nodes ending 1 byte before
                                           ;; current cursor byte
                                           ;; because of slash
                                           (dec cursor-byte))
             (util/tree-cursor-reducible tc)))]
      (if sym-node
        (let [sym-str (util/node->str rope sym-node)
              sym-str (if slash?
                        (str sym-str "/")
                        sym-str)
              completions (into
                           []
                           (compliment.core/completions sym-str
                                                        {:ns (:eval-ns editor)}))]
          
          (if (seq completions)
            (let [para (editor->paragraph editor)
                  
                  sym-cursor (-> (text-mode/editor-goto-byte editor (.getStartByte sym-node))
                                 :cursor)
                  sym-char(:char sym-cursor)

                  start-char (- sym-char
                                (:char-offset para))
                  end-char (let [bi (doto (BreakIterator/getCharacterInstance)
                                      (.setText rope))]
                             (- (.following bi sym-char)
                                (:char-offset para)))
                  
                  {:keys [x y width height] :as rect}
                  (first
                   (para/get-rects-for-range para start-char end-char
                                             :max
                                             :tight))]
              ;; view can get very slightly out of sync from editor
              ;; but that's ok
              (dispatch! :update $editor
                         (fn [editor]
                           (assoc editor
                                  ::completion
                                  {:completions completions
                                   :->view (fn [{:keys [completions offset]}]
                                             (ui/translate
                                              x (+ y height)
                                              (ui/fill-bordered
                                               [0.9 0.9 0.9]
                                               4
                                               (ui/vertical-layout
                                                (para/paragraph (str/join
                                                                "\n"
                                                                (eduction
                                                                 (map :candidate)
                                                                 (drop (or offset 0))
                                                                 (take 20)
                                                                 completions))
                                                               nil
                                                               {:paragraph-style/text-style (:base-style editor)})
                                                (when (> (count completions)
                                                         (+ (or offset 0) 20))
                                                  (para/paragraph "..."
                                                                  nil
                                                                  {:paragraph-style/text-style (:base-style editor)}))))))
                                   :offset 0}))))
            (dispatch! :update $editor dissoc ::completion)))
        ;; else
        (when (::completion editor)
          (dispatch! :update $editor dissoc ::completion))))))

(defn editor-background-runner []
  (let [ch (async/chan (async/sliding-buffer 1))]
    (async/thread
     (try
       (loop [last-editor nil]
         (let [msg (async/<!! ch)
               editor (:editor msg)

               rope-or-cursor-changed?
               (not= (select-keys editor [:rope :cursor]) 
                     (select-keys last-editor [:rope :cursor]))]
           (when rope-or-cursor-changed?
             (try
               (update-arglist msg)
               (catch Exception e
                 (tap> e))))
           
           (when (or rope-or-cursor-changed?
                     ;; there's probably a better way to do this
                     (and (= {} (::completion editor))
                          (not (::completion last-editor))))
             (try
               (update-completions msg)
               (catch Exception e
                 (tap> e))))
           
           (recur editor)))
       (catch Throwable e
         (prn e))))
    ch))


(defn make-editor
  ([{:keys [file eval-ns source] :as m}]
   (let [editor (-> (make-editor)
                    (assoc :background-chan (editor-background-runner)))
         
         editor (cond-> editor
                  file (assoc :file file)
                  eval-ns (assoc :eval-ns eval-ns )
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
   (let [lang (TreeSitterClojure.)
         parser (doto (TSParser.)
                (.setLanguage (TreeSitterClojure.)))
         buf (byte-array 4096)
         rope Rope/EMPTY
         tree (.parse parser buf nil (util/->RopeReader rope) TSInputEncoding/TSInputEncodingUTF8)]
     {:tree tree
      :cursor {:byte 0
               :char 0
               :point 0
               :row 0
               :column-byte 0}
      :viewport {:start-line 0
                 :num-lines 40}
      :paragraph nil
      :base-style #:text-style {:font-families ["Menlo"]
                                :font-size 12
                                :height 1.2
                                :height-override true}
      :eval-ns *ns*
      :rope rope
      :language lang
      :viscous? true
      :parser parser
      :buf buf})))


(defn ^:private guess-ns [source]
  (let [[_ ns-str] (re-find #"\(ns ([a-z0-9A-A.\-]+)" source)]
    (when ns-str
      (symbol ns-str))))

(defn make-editor-from-file [f]
  (let [last-file-load (java.time.Instant/now)
        source (slurp f)
        eval-ns (when-let [ns-sym (guess-ns source)]
                  (create-ns ns-sym))]
    (-> (make-editor 
         (cond-> {:file f
                  :source source}
           eval-ns (assoc :eval-ns eval-ns)))
        (assoc :last-file-load last-file-load))))

(defn make-editor-from-ns [ns]
  (let [eval-ns (the-ns ns)
        ns-sym (ns-name eval-ns)
        resource-path (ns-sym->resource-path ns-sym)
        resource (io/resource resource-path)
        source (slurp resource)
        file (when (= "file" (.getProtocol resource))
               (io/as-file resource))
        last-file-load (when file
                         (java.time.Instant/now))
        source (when resource
                 (slurp resource))]
    (cond-> (make-editor 
             (cond-> {:eval-ns eval-ns}
               file (assoc :file file)
               source (assoc :source source)))
      last-file-load (assoc :last-file-load last-file-load))))



;; Tree sitter uses these strings
;; and requires they not be garbage collected
;; and they do not retain a strong reference.
;; tldr: make sure these queries don't get garbage collected
;;       or redefined
#_(defonce lang->highlight-queries
    {clojure-lang highlight-queries
     json-lang json-highlight-queries})

(defn paren-highlight [editor para]
  (let [cursor-byte (-> editor :cursor :byte)

        byte-index
        (let [cursor (TSTreeCursor. (.getRootNode ^TSTree (:tree editor)))]
          (when (.gotoFirstChild cursor)
            (when (loop []
                    (if (< (.getEndByte (.currentNode cursor))
                             cursor-byte)
                      (when (.gotoNextSibling cursor)
                        (recur))
                      true))
              (loop []
                (let [node (.currentNode cursor)
                      start-byte (.getStartByte node)
                      end-byte (.getEndByte node)]
                  (cond

                    (> start-byte cursor-byte) nil
                    (= start-byte cursor-byte) (if (clojure-mode/coll-node-types (.getType node))
                                                 (dec end-byte)
                                                 nil)

                    (and (= end-byte cursor-byte)
                         (clojure-mode/coll-node-types (.getType node)))
                    start-byte

                    :else (when (util/goto-next-dfs-node cursor)
                            (recur))))))))
        ]
    (when (and byte-index
               (>= byte-index (:start-byte-offset para))
               (< byte-index (:end-byte-offset para)))
      (let [^Rope rope (:rope editor)
            diff-rope (.sliceBytes rope (:start-byte-offset para) byte-index)
            char-index (.length diff-rope)
            {:keys [x y width height] :as rect}
            (first
             (para/get-rects-for-range para char-index (inc char-index)
                                       :max
                                       :tight))]
        (if (not rect)
          (println "no paren rect!")
          (ui/translate x y
                        (ui/filled-rectangle
                         [0.3725490196078431 0.8431372549019608 0.8431372549019608]
                         width height)))))))

(def builtin?
  #{"def" "defn" "fn" "defui" "for" "do" "doseq" "let" "recur" "if" "when" "loop" "and" "or" "doto" "defrecord" "reify" "if-let" "extend-protocol" "defonce" "defprotocol" "defmulti" "defmethod" "ns" "import" "require"} )

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
                    ;; ;;lang highlight-queries
                    ;; ^TSQueryCursor qc
                    ;; ^TSQuery query
                    ;; editor
                    ;; start-byte-offset
                    ;; end-byte-offset
                    {:keys [start-byte-offset end-byte-offset]}]
  (let [^TSQueryCursor qc (TSQueryCursor.)
        ^TSQuery query (TSQuery. (:language editor)
                                 clojure-mode/highlight-queries)


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
                     (if (= capture-name "list")
                       (let [;; check if the first child is def or defn
                             first-child (.getNamedChild node 0)]
                         (if (and (not (.isNull first-child))
                                  (= "sym_lit" (.getType first-child)))
                           (let [s (util/node->str rope first-child)]
                             (if (builtin? s)
                               (let [start-byte (min end-byte-offset (.getStartByte first-child))
                                     end-byte (min end-byte-offset (.getEndByte first-child))
                                     color (get clojure-mode/text-colors "defn")
                                     style {:text-style/color color}]
                                 (recur end-byte
                                        (if (> end-byte start-byte)
                                          (assoc styles [start-byte end-byte] style)
                                          styles)))
                               (recur offset styles)))
                           ;; else
                           (recur offset styles)))
                       ;; else
                       (let [start-byte (min end-byte-offset (.getStartByte node))
                             end-byte (min end-byte-offset (.getEndByte node))

                             styles (if (> end-byte start-byte)
                                      (let [color (get clojure-mode/text-colors capture-name)]
                                        (assoc styles [start-byte end-byte] {:text-style/color color}))
                                      ;; else
                                      styles)]
                         (recur end-byte
                                styles))))
                   ;; else
                   styles))]
    styles))

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
        viewport {:start-byte-offset start-byte-offset
                  :char-offset char-offset
                  :end-byte-offset end-byte-offset}

        p (styled-text rope
                       (:base-style editor)
                       [(syntax-style editor viewport)
                        (selection-style editor viewport)
                        (debug-selection-style editor viewport)
                        (highlight-search editor viewport)]
                       start-byte-offset
                       end-byte-offset)
        para (para/paragraph p nil {:paragraph-style/text-style (:base-style editor)})
        para (assoc para
                    :char-offset char-offset
                    :start-byte-offset start-byte-offset
                    :end-byte-offset end-byte-offset)]
    para))


(defui line-val-view [{:keys [line-val editor para]}]
  (let [{:keys [tree rope base-style viscous?]} editor]
    (into []
          (map (fn [[line val]]
                 (let [byte-start-index (util/find-byte-offset-for-line tree rope line)
                       byte-end-index (util/find-byte-offset-for-line tree rope (inc line))
                       char-offset (get para :char-offset 0)
                       rects (para/get-rects-for-range para
                                                       (- (util/byte-index->char-index rope byte-start-index)
                                                          char-offset)
                                                       (- (util/byte-index->char-index rope byte-end-index)
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
                       (if (-> val viscous/-unwrap meta :view)
                         (ui/translate (+ x 10) y
                                       (viscous/-unwrap val))
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
                           (ui/translate (+ x 10) y 
                                         (-> (make-editor)
                                             (assoc :base-style base-style)
                                             (text-mode/editor-self-insert-command
                                              "=> ")
                                             (text-mode/editor-self-insert-command
                                              (pr-str @val))
                                             (editor->paragraph))))))))))
          line-val)))




(defeffect ::select-file [{:keys [file]}]
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
          paren-highlight-view (paren-highlight editor para)
          line-vals (when-let [line-val (if (:instarepl? editor)
                                          (-> editor :line-val first second)
                                          (-> editor :line-val (get rope)))]
                      (line-val-view
                       {:editor editor
                        :para para
                        :line-val line-val}))

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
                                         
                                         (file-picker {:folder (.getParentFile ^File (:file editor))
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
       paren-highlight-view
       para
       line-vals
       status-bar
       (when-let [completion (-> editor
                                 ::completion)]
         (when-let [->view (:->view completion)]
           (->view completion)))])))


(defn init-search-forward [editor]
  (assoc editor
         ::search {:initial-cursor (:cursor editor)
                   :initial-rope (:rope editor)}))

#_(defeffect ::init-search-forward [{:keys [$editor]}]
    (dispatch! :update $editor
               (fn [editor]
                 (assoc editor
                        ::search {:initial-cursor (:cursor editor)
                                  :initial-rope (:rope editor)}))))

(defn cancel-search-forward [editor]
  (let [initial-cursor (-> editor ::search :initial-cursor)
        editor (if initial-cursor
                 (assoc editor :cursor initial-cursor)
                 editor)]
    (text-mode/editor-update-viewport
     (dissoc editor ::search))))

(defeffect ::cancel-search-forward [{:keys [$editor]}]
  (dispatch! ::update-editor 
             {:$editor $editor
              :op (fn [editor]
                    (let [initial-cursor (-> editor ::search :initial-cursor)
                          editor (if initial-cursor
                                   (assoc editor :cursor initial-cursor)
                                   editor)]
                      (text-mode/editor-update-viewport
                       (dissoc editor ::search))))}))

(defn finish-search-forward [editor]
  (-> editor
      (text-mode/editor-push-mark (-> editor ::search :initial-cursor))
      (dissoc ::search)))

(defeffect ::finish-search-forward [{:keys [$editor]}]
  (dispatch! ::update-editor
             {:op finish-search-forward
              :$editor $editor}))

(defeffect ::show-completions [{:keys [$editor]}]
  (dispatch! ::update-editor
             {:op (fn [editor]
                    (if (::completion editor)
                      (update editor ::completion
                              (fn [{:keys [offset completions] :as m}]
                                (let [offset (or offset 0)
                                      next-offset (+ offset 10)]
                                  (if (>= next-offset (count completions))
                                    (assoc m :offset 0)
                                    (assoc m :offset next-offset)))))
                      (assoc editor ::completion {})))
              :$editor $editor}))

(def clojure-key-tree
  (key-binding/key-bindings->key-tree
   (assoc clojure-mode/key-bindings
          "C-x C-s" ::save-editor
          "C-x C-f" ::file-picker
          "C-c C-d" ::show-doc
          "C-g" #'editor-cancel
          "C-c t" ::tap-editor
          "C-c C-k" ::load-buffer
          "M-TAB" ::show-completions
          ;; "C-c C-v" ::editor-paste
          "C-M-x" ::editor-eval-top-form
          "C-x C-e" ::editor-eval-last-sexp
          "C-s" #'init-search-forward)))

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
  (dispatch! ::update-editor
             {:$editor $editor
              :op (fn [editor]
                    (let [query (str (-> editor
                                         ::search
                                         :query)
                                     s)]
                      (-> editor
                          (editor-search-forward query)
                          (assoc :search/last-search query))))}))

(defeffect ::repeat-search-forward [{:keys [$editor s]}]
  (dispatch! ::update-editor
             {:$editor $editor
              :op (fn [editor]
                    (if (-> editor ::search :query)
                      (editor-repeat-search-forward editor)
                      (if-let [query (:search/last-search editor)]
                        (editor-search-forward editor query)
                        editor)))}))

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
                            (binding [*ns* (:eval-ns editor)]
                              (loop [bindings {}
                                     line-vals {}]
                                (let [node (.currentNode cursor)
                                      s (util/node->str rope node)
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

(defui code-editor [{:keys [editor
                            ^:membrane.component/contextual
                            focus]
                     :as this}]
  (let [body (editor-view {:editor editor
                           ;; hack, otherwise, $editor is slightly different
                           :$editor $editor})

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
               (key-binding/wrap-editor-key-tree {:key-tree clojure-key-tree
                                                  :editor editor
                                                  ;; hack, otherwise, $editor is slightly different
                                                  :$editor $editor
                                                  :update-editor-intent ::update-editor
                                                  :body body
                                                  :$body nil})
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
               body)]
    (ui/vertical-layout
     ;;(ui/label (pr-str (:cursor editor)))
     #_(ui/flex-layout
       [(basic/checkbox
         {:checked? (:structure? editor)})
        (ui/label "structure?")]
      {:direction :row
       :gap 4
       :align :center})
     (when (:structure? editor)
       [(ui/spacer 100 150)
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
            result-node (util/named-child-for-byte tree cursor-byte)
            result-str (util/node->str rope result-node )
            result-form (binding [*ns* (:eval-ns editor)]
                          (read-string result-str))
            
            forms (when (.gotoFirstChild cursor)
                    (binding [*ns* (:eval-ns editor)]
                      (loop [forms []]
                        (let [node (.currentNode cursor)]
                          (if (< (.getEndByte node)
                                 cursor-byte)
                            (let [node-str (util/node->str rope node)
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
                         text-mode/editor-move-end-of-line
                         (text-mode/editor-self-insert-command "\n")
                         (clojure-mode/editor-indent)
                         (text-mode/editor-self-insert-command
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

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))

                char-index (if (zero? affinity)
                             (.preceding bi char-index)
                             char-index)
                
                byte-index (let [s (.toString (.subSequence rope 0 char-index))
                                 bs (.getBytes s "utf-8")]

                             (alength bs))
                byte-index (+ byte-index (:start-byte-offset para))


                ^TSTree
                tree (:tree editor)
                root-node (.getRootNode tree)
                node (.getNamedDescendantForByteRange root-node byte-index byte-index)]
            (when node
              (let [node-str (util/node->str (:rope editor) node)
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
                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))

                char-index (if (zero? affinity)
                             (.preceding bi char-index)
                             char-index)
                
                byte-index (let [s (.toString (.subSequence rope 0 char-index))
                                 bs (.getBytes s "utf-8")]

                             (alength bs))
                byte-index (+ byte-index (:start-byte-offset para))


                ^TSTree
                tree (:tree editor)
                root-node (.getRootNode tree)
                node (.getNamedDescendantForByteRange root-node byte-index byte-index)]
            (when (= "sym_name" (.getType node))
              (let [node-str (util/node->str (:rope editor) node)
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
        viscous? (get editor :viscous?)
        instarepl? (get extra :instarepl?)
        structure? (get extra :structure?)

        $editor $editor
        editor (-> editor
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
        (ant/button {:text  "load"
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
        (ant/button {:text  "load-json"
                     :on-click (fn []
                                 [[:set $editor
                                   (-> (make-editor (TreeSitterJson.))
                                       (text-mode/editor-self-insert-command
                                        (slurp (io/as-url
                                                "https://raw.githubusercontent.com/sogaiu/tree-sitter-clojure/refs/heads/master/src/grammar.json")))
                                       (assoc :cursor {:byte 0
                                                       :char 0
                                                       :point 0
                                                       :row 0
                                                       :column-byte 0})
                                       (text-mode/editor-update-viewport))
                                   ]])})]
       {:direction :row
        :gap 4
        :align :center})
      #_(ant/radio-bar
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
      #_(ui/flex-layout
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
      #_(ui/flex-layout
       [(basic/checkbox
         {:checked? viscous?})
        (ui/label "viscous?")]
       {:direction :row
        :gap 4
        :align :center})
      #_(ui/flex-layout
       [(basic/checkbox
         {:checked? instarepl?})
        (ui/label "instarepl?")]
       {:direction :row
        :gap 4
        :align :center})
      #_(ui/flex-layout
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



#_(defn dev
  ([]
   (dev 'com.phronemophobic.clobber.modes.clojure.ui))
  ([ns]
   (let [eval-ns (the-ns ns)
         ns-sym (ns-name eval-ns)
         resource-path (let [parts (-> ns-sym
                                       name
                                       (str/split #"\."))]
                         (str (str/join "/" parts) ".clj"))
         resource (io/resource resource-path)
         source (slurp resource)
         file (when (= "file" (.getProtocol resource))
                (io/as-file resource))

         editor (-> (make-editor)
                    (text-mode/editor-self-insert-command source)
                    (assoc :eval-ns eval-ns)
                    (assoc :cursor {:byte 0
                                    :char 0
                                    :point 0
                                    :row 0
                                    :column 0})
                    (text-mode/editor-update-viewport))
         editor (if file
                  (assoc editor :file file)
                  editor)]
     (dev/add-component-as-applet #'debug
                                  {:editor editor}))))

(defeffect ::tap [& args]
  (case (count args)
    0 nil
    1 (tap> (first args))
    2 (tap> (vec args))))

(defn ^:private within-last-5-seconds? [instant]
  (let [now (Instant/now)
        duration (Duration/between instant now)]
    (and (not (.isNegative duration))
         (<= (.getSeconds duration) 5))))


(defn ^:private file-changed [dispatch! $editor]
  ;; only reload if last-file-load is after
  ;; last-change
  (let [editor (dispatch! :get $editor)
        last-change (:last-change editor)]
    (when-let [^File file (:file editor)]
      (when-let [^Instant last-file-load (:last-file-load editor)]
        (let [last-save (:last-save editor)
              we-just-saved? (and last-save
                                  (within-last-5-seconds? last-save)
                                  (= (.length file)
                                     (.numBytes ^Rope (:rope editor))))]
          (when (and (not we-just-saved?)
                     (or (not last-change)
                         (= last-file-load last-change)
                         (.isAfter last-file-load last-change)))
            (let [update-time (java.time.Instant/now)
                  source (slurp file)]
              (dispatch! ::temp-status {:$editor $editor
                                        :msg "reverting..."})
              (dispatch! :update
                         $editor
                         (fn [editor]
                           (let [old-cursor (:cursor editor)
                                 
                                 editor (-> editor
                                            (assoc :rope Rope/EMPTY)
                                            (assoc :cursor {:byte 0
                                                            :char 0
                                                            :point 0
                                                            :row 0
                                                            :column-byte 0})
                                            (text-mode/editor-self-insert-command source)
                                            (assoc :last-file-load update-time
                                                   :last-change update-time))
                                 
                                 end-cursor (:cursor editor)
                                 editor (if (or (< (:row old-cursor)
                                                   (:row end-cursor))
                                                (and (= (:row old-cursor)
                                                        (:row end-cursor))
                                                     (<= (:column-byte old-cursor)
                                                         (:column-byte end-cursor))))
                                          (text-mode/editor-goto-row-col editor
                                                                         (:row old-cursor)
                                                                         (:column-byte old-cursor))
                                          editor)
                                 editor (text-mode/editor-update-viewport editor)]
                             editor))))))))))

(defeffect ::auto-reload-file [{:keys [editor $editor]}]
  (when-let [^File file (:file editor)]
    (when-let [unwatch (::auto-reload-unwatch editor)]
      (unwatch))
    (let [;; keep this as an optional dependency
          watch @(requiring-resolve 'nextjournal.beholder/watch)
          stop @(requiring-resolve 'nextjournal.beholder/stop)
          
          path (.getCanonicalPath file)
          watch-fn (fn [{:keys [path type] :as m}]
                     (when (= :modify type)
                       (file-changed dispatch! $editor)))
          watcher (watch watch-fn path)
          unwatch (fn []
                    (stop watcher))]
      (dispatch! :update $editor assoc ::auto-reload-unwatch unwatch))))

(defeffect ::auto-reload-file-unwatch [{:keys [$editor editor]}]
  (when-let [unwatch (::auto-reload-unwatch editor)]
    ;; there is a race condition here, but
    ;; we're ignoring it for now.
    (dispatch! :update $editor dissoc ::auto-reload-unwatch)
    (unwatch)))


