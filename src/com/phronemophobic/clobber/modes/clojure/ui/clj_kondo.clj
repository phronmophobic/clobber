(ns com.phronemophobic.clobber.modes.clojure.ui.clj-kondo
  (:require [membrane.ui :as ui]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [com.phronemophobic.clobber.util.ui :as util.ui]
            [clj-kondo.core :as clj-kondo]
            [com.phronemophobic.clobber.modes.clojure.ui :as-alias cui]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui])
  (:import java.io.ByteArrayInputStream
           io.lacuna.bifurcan.Rope))

(defui clj-kondo-view [{:keys [focused? body editor]}]
  (let [kondo-state (::clj-kondo editor)
        {:keys [finding-index findings]
         :or {finding-index 0}} kondo-state
        finding (nth findings finding-index nil)
        
        base-style (:base-style editor)
        
        view 
        (ui/vertical-layout
         (para/paragraph
          [{:text (str (:type finding))
            :style (util.ui/merge-styles
                    [(get util.ui/default-theme "keyword")
                     base-style])}
           (str " (" (:row finding) ", "(:col finding) ")")
           "\n"
           (:message finding)]
          nil
          {:paragraph-style/text-style base-style})
         
         (ui/horizontal-layout
          
          (ui/on
           :mouse-down
           (fn [_]
             [[::cui/update-editor {:$editor $editor
                                    :op
                                    (fn [editor]
                                      (dissoc editor ::util.ui/ui))}]])
           (icon.ui/icon {:name "close"}))

          (ui/on
           :mouse-down
           (fn [_]
             [[::cui/update-editor {:$editor $editor
                                    :op
                                    (fn [editor]
                                      (update-in editor
                                                 [::clj-kondo :finding-index]
                                                 (fn [n]
                                                   (let [newn (dec (or n 0))]
                                                     (if (neg? newn)
                                                       (dec (count (-> editor ::clj-kondo :findings)))
                                                       newn)))))}]
              [::run-clj-kondo {:$editor $editor}]])
           (icon.ui/icon {:name "left-circle"}))
          (ui/on
           :mouse-down
           (fn [_]
             [[::cui/update-editor {:$editor $editor
                                    :op
                                    (fn [editor]
                                      (update-in editor
                                                 [::clj-kondo :finding-index]
                                                 (fn [n]
                                                   (let [newn (inc (or n 0))]
                                                     (if (>= newn (count (-> editor ::clj-kondo :findings)))
                                                       0
                                                       newn)))))}]
              [::run-clj-kondo {:$editor $editor}]])
           (icon.ui/icon {:name "right-circle"}))
          (ui/label (str " " finding-index " / " (count findings) " "))))
        
        view
        (key-binding/wrap-editor-key-bindings 
         {:key-bindings (:key-bindings editor)
          :editor editor
          :update-editor-intent ::cui/update-editor
          :body view
          :$body nil})]
    (ui/vertical-layout
     body
     view)))


(defeffect ::run-clj-kondo [{:keys [$editor] :as m}]
  (future
    ;; do this asynchronously
    ;; results may be slightly out of date
    ;; but that's ok
    (try
      (let [editor (dispatch! :get $editor)
            rope (:rope editor)

        
            kondo-state (::clj-kondo editor)

            findings (if (= rope (:rope kondo-state))
                       (:findings kondo-state)
                       (let [buf (byte-array (Rope/.numBytes rope))
                             _ (Rope/.copyBytes rope 0 buf 0 (alength buf))
                             bais (ByteArrayInputStream/new buf)
                             results (binding [*in* bais]
                                       (clj-kondo/run! {:lint ["-"]}))]

                         (:findings results)))
            finding-index (if (= rope (:rope kondo-state))
                            (:finding-index kondo-state)
                            ;; pick the first finding that starts after the current cursor
                            (let [{:keys [row column-byte]} (:cursor editor)]
                              (loop [index 0
                                     findings (seq findings)]
                                (if findings
                                  (let [finding (first findings)
                                        {finding-row :row
                                         finding-col :col} finding]
                                    (if (or (> (dec finding-row) row)
                                            (and (= (dec finding-row) row)
                                                 (>= (dec finding-col) column-byte)))
                                      index
                                      (recur (inc index)
                                             (next findings))))
                                  ;; else, reached the end, start at beginning
                                  0))))
            finding (nth findings finding-index nil)
            
            clj-kondo-highlights (when finding
                                   (let [editor (text-mode/editor-goto-row-col editor (dec (:row finding)) (dec (:col finding)))
                                         start-cursor (:cursor editor)
                                         editor (text-mode/editor-goto-row-col editor (dec (:end-row finding)) (dec (:end-col finding)))
                                         end-cursor (:cursor editor)]
                                     {[(:byte start-cursor)
                                       (:byte end-cursor)]
                                      {:text-style/background-color {:color [0 0 1.0 0.2]}}}))]

        (dispatch! ::cui/update-editor
                   {:editor editor
                    :$editor $editor
                    :op (fn [editor]
                          (let [editor (assoc editor
                                              ::highlights clj-kondo-highlights
                                              ::clj-kondo {:findings findings
                                                           :rope rope
                                                           :finding-index finding-index})
                                editor (if finding
                                         (text-mode/editor-goto-row-col editor (dec (:row finding)) (dec (:col finding)))
                                         editor)
                                
                                editor (if (::util.ui/ui editor)
                                         editor
                                         (let [ui (clj-kondo-view
                                                   {::util.ui/close-fn (fn [editor]
                                                                         (dissoc editor ::clj-kondo))})
                                               ui (assoc ui :adjust-height
                                                         (fn [height]
                                                           (max 0 (- height (ui/height ui)))))
                                               
                                               editor (assoc editor ::util.ui/ui ui)]
                                           editor))]
                            editor))})
        (dispatch! :repaint!))
      (catch Exception e
        (tap> e)))))


(comment
  
  (dev/add-component-as-applet
   #'clj-kondo-view
   {:update-editor-intent ::test-update-editor-intent
    :editor (assoc x-325250
                   :clj-kondo/findings x-325243)})
  

  ,)

