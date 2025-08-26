(ns com.phronemophobic.clobber.util.ui.key-binding
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
                           ;; TreeSitterJson
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
          
          \S
          (if (= \- (second cs))
            (recur (nnext cs)
                   chord
                   (assoc press :super? true))
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

(defn key-bindings->key-tree [key-bindings]
  (reduce
   (fn [m [chord-str intent]]
     (let [chord (key-chord->map chord-str)]
       
       (assoc-in m chord
                 ;; wrap intent so we can tell it apart from a regular map
                 {::intent intent})))
   {}
   key-bindings))


(comment
  (def clojure-keytree
  (key-bindings->keytree
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
          "C-s" #'init-search-forward))))


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


(def normalize-key
  {:enter :enter
   :kp_enter :enter
   :backspace :backspace
   :left :left
   :right :right
   :up :up
   :down :down
   :tab :tab})

(defui wrap-chord [{:keys [body]}]
  (let [modifiers (get extra ::modifiers)]
    (ui/on
     :key-event
     (fn [key scancode action mods]
       (let [alt? (not (zero? (bit-and ui/ALT-MASK mods)))
             super? (not (zero? (bit-and ui/SUPER-MASK mods)))
             shift? (not (zero? (bit-and ui/SHIFT-MASK mods)))
             ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))
             caps-lock? (not (zero? (bit-and ui/CAPS-LOCK-MASK mods)))]

         (if (#{:press :repeat} action)
           (let [normalized-key (if-let [k (get normalize-key (get skia/keymap key))]
                                  k
                                  ;; else
                                  (cond
                                    (and caps-lock?
                                         (not (or alt? super? ctrl?)))
                                    (let [c (char key)]
                                      (if shift?
                                        (or (get uppercase c)
                                            c)
                                        c))

                                    shift? (let [c (char key)]
                                             (or (get uppercase c)
                                                 c))
                                    
                                    :else (Character/toLowerCase (char key))))
                 chord (cond-> {:key normalized-key}
                         alt? (assoc :meta? true)
                         super? (assoc :super? true)
                         ctrl? (assoc :ctrl? true))]
             (let [intents [[:update $modifiers (fn [xs] (cond-> (or xs #{})
                                                           alt? (conj :alt?)
                                                           super? (conj :super?)
                                                           ctrl? (conj :ctrl?)))]]
                   mod-keys #{:left_shift
                              :left_control
                              :left_alt
                              :left_super
                              :right_shift
                              :right_control
                              :right_alt
                              :right_super
                              :caps_lock}
                   intents (if (not (contains? mod-keys (get skia/keymap key)))
                             (conj intents
                                   [::chord-press {:chord chord}])
                             intents)]
               intents))
           ;; release action
           [[:update $modifiers (fn [xs] (cond-> (or xs #{})
                                           (not alt?) (disj :alt?)
                                           (not super?) (disj :super?)
                                           (not ctrl?) (disj :ctrl?)))]])))
     body)))

(defui wrap-key-tree [{:keys [body key-tree current-key-binding]}]
  (let [sub-key-tree (get extra ::sub-key-tree)
        body
        (ui/on
         ::chord-press
         (fn [{:keys [chord]}]
           (if-let [match (get (or sub-key-tree key-tree) chord)]
             (if-let [intent (::intent match)]
               [[::press {:intent intent}]
                [:set $current-key-binding []]
                [:set $sub-key-tree nil]]
               ;; else assume a sub key-tree 
               [[:set $sub-key-tree match]
                [:update $current-key-binding
                 (fn [xs chord]
                   (conj (or xs []) chord))
                 chord]])
             ;; miss!
             [[:set $sub-key-tree nil]
              [:set $current-key-binding []]
              [::miss {:key-binding (conj (or current-key-binding []) chord)}]]))
         (wrap-chord {:body body
                      :$body nil}))]
    body))

(defui wrap-key-bindings [{:keys [body key-bindings]}]

  (let [key-tree-cache (get extra :key-tree-cache)
        key-tree (get key-tree-cache key-bindings)]
    (if key-tree
      (wrap-key-tree {:body body
                      :key-tree key-tree})
      (ui/on-key-event
       (fn [key scancode action mods]
         (let [key-tree (key-bindings->key-tree key-bindings)
               elem (wrap-key-tree {:body body
                                    :key-tree key-tree})
               intents (ui/key-event elem key scancode action mods)]
           (cons [:set $key-tree-cache {key-bindings key-tree}]
                 intents)))
       body))))


(defui wrap-editor-key-bindings [{:keys [key-bindings body editor update-editor-intent]}]
  (let [body (wrap-key-bindings
              {:body body
               :$body nil
               :key-bindings key-bindings})
        body (ui/on
              ::miss
              (fn [{:keys [key-binding]}]
                (when (= 1 (count key-binding))
                  (let [chord (first key-binding)
                        key (:key chord)]
                    (when (and key
                               (not (:meta? chord))
                               (not (:ctrl? chord))
                               (not (:super? chord))
                               (not (keyword? key)))
                      [[(or update-editor-intent ::update-editor)
                        {:op #(text-mode/editor-self-insert-command % (str key))
                         :$editor $editor}]]))))
              ::press
              (fn [{:keys [intent]}]
                (if (keyword? intent)
                  [[intent {:editor editor
                            :$editor $editor}]]
                  [[(or update-editor-intent ::update-editor)
                    {:op intent
                     :editor editor
                     :$editor $editor}]]))
              body)]
    body))



(defui debug-key-chords [{:keys []}]
  (let [chords (get extra ::chords [])
        focus (:focus context)
        body (ui/rectangle 100 100)
        body (if (= $extra focus)
               (wrap-chord {:body body
                            :$body nil})
               (ui/on
                :mouse-down
                (fn [_]
                  [[:set $focus $extra]])
                body))]
    (ui/on
     ::chord-press
     (fn [m]
       [[:update $chords conj m]])
     
     (ui/vertical-layout
      (ant/button {:text "Reset"
                   :on-click (fn []
                               [[:set $chords []]])})
      (para/paragraph 
       (str/join "\n"
                 (eduction
                  (map pr-str)
                  chords))
       400
       {:paragraph-style/text-style
        #:text-style
        {:font-families ["Menlo"]
         :font-size 12
         :height 1.2
         :height-override true}})
      
      body))))


(defui debug-key-bindings [{:keys [key-tree]}]
  (let [bindings (get extra ::bindings [])
        focus (:focus context)
        body (ui/rectangle 100 100)
        body (if (= $extra focus)
               (wrap-key-tree {:body body
                               :$body nil
                               :key-tree key-tree})
               (ui/on
                :mouse-down
                (fn [_]
                  [[:set $focus $extra]])
                body))]
    (ui/on
     ::miss
     (fn [m]
       [[:update $bindings conj {:miss m}]])
     ::press
     (fn [m]
       [[:update $bindings conj {:press m}]])
     
     (ui/vertical-layout
      (ant/button {:text "Reset"
                   :on-click (fn []
                               [[:set $bindings []]])})
      (para/paragraph 
       (str/join "\n"
                 (eduction
                  (map pr-str)
                  bindings))
       400
       {:paragraph-style/text-style
        #:text-style
        {:font-families ["Menlo"]
        :font-size 12
        :height 1.2
        :height-override true}})
      
      body))))



(comment
  (dev/add-component-as-applet
   #'debug-key-chords
     {})
  
  
  (dev/add-component-as-applet
   #'debug-key-bindings
     {:key-tree
      (key-bindings->key-tree 
       {"C-X C-s" {:my-map ::foo}
        "S-RET" :command+enter})})

  ,)


