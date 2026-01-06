(ns com.phronemophobic.clobber.modes.clojure.ui
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async]
            clojure.pprint
            [membrane.ui :as ui]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.viscous :as viscous]
            compliment.core
            [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util :as util]
            [com.phronemophobic.clobber.util.ui :as util.ui]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding])
  (:import (org.treesitter TSQuery
                           TSParser
                           TSTree
                           TSTreeCursor
                           TSNode
                           TSQueryCapture
                           TSQueryCursor
                           TreeSitterClojure
                           TSInputEncoding)
           (java.util.jar JarFile)
           com.ibm.icu.text.BreakIterator
           clojure.lang.LineNumberingPushbackReader
           java.time.Duration
           java.time.Instant
           java.io.File
           java.io.StringReader
           io.lacuna.bifurcan.Rope))

(declare clojure-key-bindings)

(def ^:private
  default-font-families
  ["Menlo"
   (membrane.skia/logical-font->font-family :monospace)])

(def ^:private
  default-text-style
  #:text-style {:font-families default-font-families
                :font-size 12
                :height 1.2
                :height-override true})

(def ^:private default-paragraph-style
  {:paragraph-style/text-style default-text-style})

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
                     new-editor)

        new-editor (util.ui/upkeep-search-ui editor new-editor)
        new-editor (util.ui/ui-upkeep editor new-editor)]
    new-editor))

(defn editor-cancel [editor]
  (-> editor
      (dissoc :select-cursor)
      (dissoc ::util.ui/ui)
      (dissoc ::completion)))

(defn editor-set-height [editor height]
  (let [base-style (:base-style editor)
        row-height (* (:text-style/height base-style)
                      (:text-style/font-size base-style))
        num-lines (max 0
                       (- (quot height row-height)
                          ;; reserve two lines for status bar
                          ;; and one line for cursor info
                          2))]
    (-> editor
        (assoc-in [:viewport :num-lines] (long num-lines))
        (assoc-in [:viewport :text-height] (* row-height num-lines))
        (assoc-in [:viewport :height] height))))

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
  ;; some editing commands (like indent) can be slow
  ;; memoizing the most recent value helps in the case
  ;; where there is contention in the root atom.
  (dispatch! :update $editor editor-upkeep (wrap-memo1 op))
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

(defeffect ::update-bindings [{:keys [$editor]}]
  (dispatch! :update
             $editor
             (fn [editor]
               (update editor :key-bindings
                       (fn [bindings]
                         (merge bindings
                                clojure-key-bindings)))))
  (dispatch! ::temp-status {:$editor $editor
                            :msg "Bindings updated!"}))

(defeffect ::editor-eval-and-tap-last-sexp [m]
  (dispatch! ::editor-eval-last-sexp (assoc m :tap? true)))

(defeffect ::editor-eval-and-print-last-sexp [m]
  (dispatch! ::editor-eval-last-sexp (assoc m :print? true)))

(defeffect ::editor-eval-last-sexp [{:keys [editor $editor tap? print?]}]
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
                [(ns-sym->resource-path eval-ns-name (util/file-ext f))
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
              
              [err val] (try
                          [nil (clojure.lang.Compiler/load rdr source-path source-name)]
                          (catch Exception e
                            [e nil]))
              
              ]
          (if err
            (do 
              (dispatch! ::temp-status {:$editor $editor
                                        :msg
                                        (str (-> err class .getName) ": " (.getMessage err) "\n"
                                             (-> err .getCause .getMessage))})
              (tap> err)
              (prn err))
            ;; else no err
            (let [temp-view (ui/translate 0 -4
                                      (viscous/inspector
                                       {:obj (viscous/wrap val)
                                        :width 40
                                        :height 1
                                        :show-context? false}))]
              (when tap?
                (tap> val))

              (dispatch! :update $editor
                         (fn [editor]
                           (let [editor (update editor
                                                :line-val
                                                (fn [m]
                                                  (let [line-val (get m rope)]
                                                    {rope (assoc line-val line-number (viscous/wrap val))})))
                                 editor (if print?
                                          (text-mode/editor-insert editor (with-out-str (clojure.pprint/pprint val)))
                                          editor)]
                             editor)))
              (dispatch! ::temp-status {:$editor $editor
                                    :msg temp-view}))))))))

(defeffect ::editor-eval-top-form [{:keys [editor $editor]}]
  (future
    (let [{:keys [^TSTree tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
          
          {cursor-byte :byte
           cursor-char :char
           cursor-point :point
           cursor-row :row
           cursor-column-byte :column-byte} cursor
          root-node (.getRootNode tree)
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
                  [(ns-sym->resource-path eval-ns-name (util/file-ext f))
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
                                      :msg (str (-> e class .getName) ": " (.getMessage e) "\n"
                                                (-> e .getCause .getMessage))})
            (tap> e)
            (prn e)))))))

(defeffect ::load-buffer [{:keys [editor $editor]}]
  (future
    (let [eval-ns (:eval-ns editor)
          eval-ns-name (ns-name eval-ns)
          
          [;; path relative to class-path  
           source-path
           ;; filename
           source-name] (when-let [^File f (:file editor)]
                          [(ns-sym->resource-path eval-ns-name (util/file-ext f))
                           (.getName f)])
          
          
          ^Rope
          rope (:rope editor)
          rdr (LineNumberingPushbackReader.
               (StringReader. (.toString rope)))]
      (try
        (clojure.lang.Compiler/load rdr source-path source-name)
        (dispatch! ::temp-status {:$editor $editor
                                  :msg "buffer loaded."})
        (catch Exception e
          (tap> e)
          (dispatch! ::temp-status {:$editor $editor
                                    :msg
                                    (str (-> e class .getName) ": " (.getMessage e) "\n"
                                         (-> e .getCause .getMessage))})
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
    (para/paragraph docstring width default-paragraph-style)))

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
  (when-let [eval-ns (:eval-ns editor)]
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
                (when (= "sym_lit" (.getType first-child))
                  (let [sym (read-string 
                             (util/node->str rope first-child))
                        v (ns-resolve eval-ns sym)
                        
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
                         (assoc-in editor [:status :status] nil)))))))))

(declare editor->paragraph)
(defn update-completions [{:keys [$editor editor dispatch!] :as m}]
  (when (::completion editor)
    (let [^TSTree tree (:tree editor)
          ^Rope
          rope (:rope editor)
          tc (TSTreeCursor. (.getRootNode tree))
          cursor (:cursor editor)
          cursor-byte (:byte cursor)
          
          
          [^TSNode sym-node slash?]
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
            (fn [[sym-node slash? ^TSNode last-node :as result] ^TSNode node]
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
                                                        {:ns (:eval-ns editor)
                                                         :sort-order :by-name}))]
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
                           (if (and (= 1 (count completions))
                                    (= rope (:rope editor))
                                    ;; don't autocomplete while typing. just for a fresh completion call
                                    (= {} (::completion editor)))
                             ;; if there's just one completion
                             ;; and the completion is still valid
                             ;; then autocomplete
                             (-> (text-mode/editor-self-insert-command editor (subs (-> completions first :candidate) (count sym-str)))
                                 (dissoc ::completion))
                             ;; else
                             (assoc editor
                                    ::completion
                                    {:completions completions
                                     :prefix sym-str
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
                                     :offset 0})))))
            (dispatch! :update $editor dissoc ::completion)))
        ;; else
        (when (::completion editor)
          (dispatch! :update $editor dissoc ::completion))))))

(declare update-instarepl)
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
           
            (when (and 
                   (:instarepl? editor)
                   (not= (:rope editor)
                         (:rope last-editor)))
              (update-instarepl msg))
           
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

(defn ^:private guess-ns [source]
  (let [[_ ns-str] (re-find #"\(ns ([a-z0-9A-A.\-]+)" source)]
    (when ns-str
      (symbol ns-str))))

(defn make-editor
  ([{:keys [file ns source] :as m}]
   (let [editor (make-editor)

         ;; contents
         editor
         (cond
           source (text-mode/editor-insert editor source 0 0 0)

           file (let [source (slurp file)
                      last-file-load (java.time.Instant/now)]
                  (-> editor
                      (text-mode/editor-insert source 0 0 0)
                      (assoc :last-file-load last-file-load)))

           ns (let [eval-ns (the-ns ns)
                    ns-sym (ns-name eval-ns)
                    resource-path (ns-sym->resource-path ns-sym)
                    resource (io/resource resource-path)
                    source (slurp resource)
                    file (when (= "file" (.getProtocol resource))
                           (io/as-file resource))
                    last-file-load (when file
                                     (java.time.Instant/now))
                    source (when resource
                             (slurp resource))
                    editor (assoc editor :eval-ns eval-ns)]
                (cond-> editor
                  file (assoc :file file)
                  source (text-mode/editor-insert source 0 0 0)
                  last-file-load (assoc :last-file-load last-file-load)))
           :else editor)

         ;; set separately from contents:
         ;; - may override file from eval-ns
         ;; - may be explicitly provided with :source
         editor (if file
                  (assoc editor :file file)
                  editor)


         ;; eval-ns
         editor (if-not (:eval-ns editor)
                  (if-let [eval-ns (:eval-ns m)]
                    (assoc editor :eval-ns eval-ns)
                    (let [eval-ns (when-let [ns-sym (guess-ns (:rope editor))]
                                    (create-ns ns-sym))]
                      (if eval-ns
                        (assoc editor :eval-ns eval-ns)
                        editor)))
                  editor)]
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
      :base-style default-text-style
      ;; this starts a thread.
      ;; should probably move somewhere else.
      :background-chan (editor-background-runner)
      :rope rope
      :language lang
      :viscous? true
      :parser parser
      :key-bindings clojure-key-bindings
      :buf buf})))




(defn make-editor-from-file [f]
  (make-editor {:file f}))

(defn make-editor-from-ns [ns]
  (make-editor {:ns ns}))

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

(defn syntax-style [editor
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

        p (util.ui/styled-text rope
                               (:base-style editor)
                               [(syntax-style editor viewport)
                                (util.ui/selection-style editor viewport)
                                (util.ui/debug-selection-style editor viewport)
                                (util.ui/highlight-search editor viewport)]
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
                                       (ui/try-draw
                                        (viscous/-unwrap val)
                                        (fn [draw e] nil)))
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


(defeffect ::drop-val [{:keys [pos obj $editor editor para]}]
  (when-let [eval-ns (:eval-ns editor)]
    (when-let [val (-> obj :x)]
      (let [char-offset (:char-offset para)
            [x y] pos
            char-index (+ (:char-offset para)
                          (para/glyph-index para x y))

            value-source (let [x @val]
                           (if (keyword? x)
                             (pr-str x)
                             (let [sym (gensym "x-")]
                               (intern eval-ns sym x)
                               (name sym))))

            event-editor editor]
        (dispatch! ::update-editor
                   {:$editor $editor
                    :op (fn [editor]
                          (-> editor 
                              (text-mode/editor-goto-char char-index)
                              (text-mode/editor-self-insert-command value-source)))})))))

(defui editor-view [{:keys [editor
                            focused?]}]
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
                       (when-let [height (-> editor :viewport :text-height)]
                         (let [view (or 
                                     (:temp status)
                                     (:status status))
                               status-bar (if (string? view)
                                            (para/paragraph view nil {:paragraph-style/text-style (:base-style editor)})
                                            view)]
                           (ui/translate 0
                                         (- height 8 (ui/height status-bar))
                                         status-bar))))]
      (dnd/on-drop
       (fn [pos obj]
         [[::drop-val {:pos pos
                       :obj obj
                       :editor editor
                       :$editor $editor
                       :para para}]])
       [(util.ui/cursor-view rope para (:cursor editor))
        paren-highlight-view
        para
        line-vals
        status-bar
        (when-let [completion (-> editor
                                  ::completion)]
          (when-let [->view (:->view completion)]
            (->view completion)))]))))

(defeffect ::show-completions [{:keys [$editor]}]
  (dispatch! ::update-editor
             {:op (fn [editor]
                    (if-let [{:keys [completions prefix]} (::completion editor)]
                      (if (= 1 (count completions))
                        (-> (text-mode/editor-self-insert-command editor (subs (-> completions first :candidate) (count prefix)))
                            (dissoc ::completion))
                        (update editor ::completion
                              (fn [{:keys [offset completions] :as m}]
                                (let [offset (or offset 0)
                                      next-offset (+ offset 10)]
                                  (if (>= next-offset (count completions))
                                    (assoc m :offset 0)
                                    (assoc m :offset next-offset))))))
                      (assoc editor ::completion {})))
              :$editor $editor}))

(defn ^:private shared-prefix
  [^String prefix ^String completion]
  (let [plen (.length prefix)
        clen (.length completion)
        shared-len 
        (loop [idx 0]
          (if (or (>= idx plen)
                  (>= idx clen))
            idx
            (let [pc1 (.charAt prefix idx)
                  cc1 (.charAt completion idx)]
              (if (not= pc1 cc1)
                idx
                (if (Character/isHighSurrogate pc1)
                  (let [idx* (inc idx)]
                    (if (not= (.charAt prefix idx*)
                              (.charAt completion idx*))
                      idx
                      (recur (inc idx*))))
                  (recur (inc idx)))))))]
    (if (= shared-len plen)
      prefix
      (subs prefix 0 shared-len))))

(defeffect ::indent-or-complete [{:keys [$editor]}]
  (dispatch! ::update-editor
             {:op (fn [editor]
                    (if-let [{:keys [completions prefix]} (::completion editor)] 
                      (if (= 1 (count completions))
                        (-> (text-mode/editor-self-insert-command editor (subs (-> completions first :candidate) (count prefix)))
                            (dissoc ::completion))
                        ;; else
                        (let [longest-shared-prefix (transduce
                                                     (comp (map :candidate)
                                                           (filter #(str/starts-with? % prefix)))
                                                     (completing shared-prefix)
                                                     (-> completions first :candidate)
                                                     (rest completions))]
                          (if (= prefix longest-shared-prefix)
                            editor
                            (text-mode/editor-self-insert-command editor (subs longest-shared-prefix (count prefix))))))
                      (clojure-mode/editor-indent editor)))
              :$editor $editor}))


(defeffect ::jump-to-definition [{:keys [editor]}]
  (let [^TSTree tree (:tree editor)
        rope (:rope editor)
        tc (TSTreeCursor. (.getRootNode tree))
        cursor (:cursor editor)
        cursor-byte (:byte cursor)
        
        ^TSNode
         sym-node
        
        (util/first-by
         (comp (take-while (fn [^TSNode node]
                             (<= (-> node .getStartByte)
                                 cursor-byte)))
               (filter (fn [^TSNode node]
                         (let [type (.getType node)]
                           (= "sym_lit" type))))
               (filter (fn [^TSNode node]
                         (>= (-> node .getEndByte)
                             cursor-byte))))
         
         
         (when (util/skip-to-byte-offset tc cursor-byte)
           (util/tree-cursor-reducible tc)))]
    (when sym-node
      (let [sym-str (util/node->str rope sym-node)
            sym (read-string sym-str)
            v (ns-resolve (:eval-ns editor) sym)
            mta (meta v)]
        (when-let [file (:file mta)]
          (when (string? file)
            (when-let [resource (io/resource file)]
              (case (.getProtocol resource)
                
                "jar"
                (let [jar-url-str (.getPath resource)
                      jar-url (io/as-url jar-url-str)
                      [jar-path file-entry-path] (str/split (.getPath jar-url) #"!/" 2)]
                  (when-let [jar (JarFile. ^String jar-path)]
                    (when-let [entry (.getJarEntry jar file-entry-path)]
                      (let [contents (with-open [is (.getInputStream jar entry)]
                                       (slurp is))]
                        (dispatch! :com.phronemophobic.easel/add-applet
                                   {:make-applet
                                    (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                                      #(f % (merge {:source contents
                                                    :mode :clojure}
                                                   (when-let [line (:line mta)]
                                                     {:line (max 0 (dec line))}))))})))))

                "file"
                (let [file (io/as-file  resource)]
                  (when (and file
                             (.exists file))
                    (dispatch! :com.phronemophobic.easel/add-applet
                                                       {:make-applet
                              (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                                #(f % (merge {:file file
                                              :mode :clojure}
                                             (when-let [line (:line mta)]
                                               {:line (max 0 (dec line))}))))})))

                ("http" "https") nil
                
                ;; else
                nil))))))))

(defn editor-toggle-instarepl [editor]
  (update editor :instarepl? 
          (fn [b]
            (if b false true))))

(defn editor-press-start [editor]
  (let [style (if (= ["Press Start 2P"]
                     (-> editor
                         :base-style
                         :text-style/font-families))
                default-text-style
                #:text-style
                {:font-families ["Press Start 2P"]
                :font-size 22
                :height 1.2
                :height-override true})]
    (-> editor
        (assoc :base-style style)
        (editor-set-height (:height editor)))))

(defn editor-decrease-font-size [editor]
  (-> editor
      (update-in [:base-style :text-style/font-size] dec)
      (editor-set-height (:height editor))))

(defn editor-increase-font-size [editor]
  (-> editor
      (update-in [:base-style :text-style/font-size] inc)
      (editor-set-height (:height editor))))

(defeffect ::toggle-instarepl [{:keys [$editor]}]
  (dispatch! ::update-editor
             {:$editor $editor
              :op editor-toggle-instarepl})
  (let [editor (dispatch! :get $editor)
        msg (if (:instarepl? editor)
              "Instarepl on."
              "Instarepl off.")]
    (dispatch! ::temp-status {:$editor $editor
                              :msg msg})))

(defeffect ::file-picker [{:keys [$editor editor] :as m}]
  (dispatch! ::util.ui/file-picker (assoc m
                                          :update-editor-intent ::update-editor)))

(defeffect ::show-find-replace [{:keys [$editor editor] :as m}]
  (dispatch! ::util.ui/show-find-replace
             (assoc m
                    :update-editor-intent ::update-editor)))

(defeffect ::show-goto-line [{:keys [$editor editor] :as m}]
  (dispatch! ::util.ui/show-goto-line
             (assoc m
                    :update-editor-intent ::update-editor)))

(def clojure-key-bindings
  (assoc clojure-mode/key-bindings
         "C-x C-s" ::save-editor
         "C-x C-f" ::file-picker
         "C-c C-d" ::show-doc
         "C-c i" ::open-instarepl
         "C-u C-c i" ::toggle-instarepl
         "C-g" #'editor-cancel
         "C-c t" ::tap-editor
         "C-c C-k" ::load-buffer
         "M-TAB" ::show-completions
         "TAB" ::indent-or-complete
         "M-%" ::show-find-replace
         "M-g M-g" ::show-goto-line
         ;; "C-c C-v" ::editor-paste
         "C-M-x" ::editor-eval-top-form
         "C-x C-e" ::editor-eval-last-sexp
         "C-u C-x C-e" ::editor-eval-and-tap-last-sexp
         "C-c C-p" ::editor-eval-and-print-last-sexp
         "C-c b" ::update-bindings
         "C-c C-o" :com.phronemophobic.easel.tap-watcher/clear-taps
         "C-c p" #'editor-press-start
         "C-c +" #'editor-increase-font-size
         "S-=" #'editor-increase-font-size
         "C-c -" #'editor-decrease-font-size
         "S--" #'editor-decrease-font-size
         "M-." ::jump-to-definition))


(defn update-instarepl [{:keys [editor $editor dispatch!] :as msg}]
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
              
              [cache line-vals]
              (when (.gotoFirstChild cursor)
                (binding [*ns* (:eval-ns editor)]
                  (loop [bindings {}
                         line-vals {}
                         forms []
                         cache (if-let [cache (-> editor ::insta-results ::cache)]
                                 @cache
                                 {})]
                    
                    (let [node (.currentNode cursor)]
                      
                      (if (= "comment" (.getType node))
                        (if (.gotoNextSibling cursor)
                          (recur bindings line-vals forms cache)
                          [cache line-vals])
                        (let [s (util/node->str rope node)
                              forms (conj forms s)
                              
                              [success binding-sym result :as ret]
                              (if-let [ret (get cache forms)]
                                ret
                                (let [form (read-string s)
                                      [binding-sym form] (if (list? form)
                                                           (case (first form)
                                                             
                                                             def [(second form) (nth form 2)]
                                                             defn [(second form) 
                                                                   `(fn ~(second form)
                                                                      ~@(if (string? (nth form 2))
                                                                          (nthrest form 3)
                                                                          (nthrest form 2)))]
                                                             ;; else
                                                             [nil form])
                                                           [nil form])
                                      binding-sym## (gensym "bindings_")
                                      let-bindings 
                                      (into []
                                            (comp
                                             (map-indexed 
                                              (fn [i sym]
                                                `[~sym (nth ~binding-sym## ~i)]))
                                             cat)
                                            (keys bindings))
                                      
                                      form `(fn [~binding-sym##]
                                              (let ~let-bindings
                                                ~form))]
                                  
                                  (try
                                    (let [f (eval form)
                                          result (f (vec (vals bindings)))]
                                      [true binding-sym result])
                                    (catch Exception e
                                      [false binding-sym e]))))
                              cache (assoc cache forms ret)
                              
                              bindings (if binding-sym
                                         (assoc bindings binding-sym result)
                                         bindings)
                              
                              line (-> node
                                       .getEndPoint
                                       .getRow)
                              line-vals (assoc line-vals line (viscous/wrap result))]
                          
                          (if (not success)
                            [cache line-vals]
                            (if (.gotoNextSibling cursor)
                              (recur bindings line-vals forms cache)
                              [cache line-vals]))))))))]
          
          (dispatch! :update $editor
                     (fn [editor]
                       (assoc editor
                              :line-val {rope line-vals}
                              ::insta-results {::cache (viscous/wrap cache)}))))))
    (catch Throwable e
      #_(prn e))))

(defui code-editor [{:keys [editor
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
                  (cons [::request-focus]
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

(defn ^:private truncate-front [s n]
  (let [len (count s)]
    (if (> len n)
      (subs s (- len n))
      s)))
(defeffect ::open-instarepl [{:keys [editor]}]
  (let [editor-label (:label editor)
        editor (-> (make-editor)
                   (assoc :eval-ns (:eval-ns editor)
                          :instarepl? true))]
    (dispatch! :com.phronemophobic.easel/add-applet
               {:make-applet
                (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                  #(f % {:editor editor
                         :label (str 
                                 "⚡"
                                 (truncate-front editor-label 12))
                         :ui code-editor}))})))

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


