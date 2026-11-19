(ns com.phronemophobic.clobber.util.ui.mx
  (:require com.rpl.specter
            [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
            [com.phronemophobic.clobber.util.ui :as-alias util.ui])
  (:import io.lacuna.bifurcan.Rope))

(defui mx-selector [{:keys [editor update-editor-intent body] :as m}]
  (let [mx-ui-state (::mx-ui-state editor)
        
        search-editor (get mx-ui-state :search-editor)
        base-style (:base-style search-editor)
        offset (get extra :offset 0)
        search-str (.toString ^Rope (:rope search-editor))
        search-str-lower (str/lower-case search-str)
        
        mx-commands (:mx-commands mx-ui-state)

        commands (into 
                  []
                  (comp (keep (fn [intent]
                                (when (str/includes? (str/lower-case (name intent))
                                                     search-str-lower)
                                  intent)))
                        (drop offset))
                  mx-commands)
        ps (into 
            [{:text "M-x "
              :style (assoc base-style :text-style/color [0.05 0.05 0.85])}
             search-str
             (when (seq search-str)
               " | ")]
            (comp (map-indexed 
                   (fn [i intent]
                     (let [style (if (zero? i)
                                   (assoc base-style 
                                          :text-style/font-style
                                          {:font-style/weight :bold})
                                   base-style)]
                       {:style style
                        :text (name intent)})))
                  (interpose " | "))
            commands)
        
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
                               :op #(dissoc % ::mx-ui-state ::util.ui/ui)}]])
     ::select-command
     (fn [m]
       
       (when-let [intent (first commands)]
         [[update-editor-intent {:editor editor
                                 :$editor $editor
                                 :op #(dissoc % ::mx-ui-state ::util.ui/ui)}]
          [intent {:editor editor
                   :$editor $editor}]]))
     ::next-offset
     (fn [m]
       [[:update $offset 
         (fn [offset]
           (if (<= (count commands) 1)
             0
             (inc offset)))]])
     ::backspace
     (fn [m]
       [[:update $search-editor #'text-mode/editor-delete-backward-char]
        [:set $offset 0]])
     ::kill-word-backward
     (fn [m]
       [[:update $search-editor (fn [editor]
                                  (-> editor
                                      (text-mode/editor-beginning-of-buffer)
                                      (text-mode/editor-set-string "")))]
        [:set $offset 0]])
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
                       "RET" ::select-command
                       "M-DEL" ::kill-word-backward
                       "/" ::goto-root
                       "C-s" ::next-offset
                       "DEL" ::backspace }})))))

(defeffect ::mx-selector [{:keys [editor $editor update-editor-intent]}]
  (dispatch! (if update-editor-intent
               update-editor-intent
               (if (instance? org.treesitter.TreeSitterClojure (:language editor))
                 :com.phronemophobic.clobber.modes.clojure.ui/update-editor
                 :com.phronemophobic.clobber.modes.text.ui/update-editor))
             {:editor editor
              :$editor $editor
              :op (fn [editor]
                    (let [editor (assoc editor
                                        ::mx-ui-state
                                        {:search-editor (-> (text-mode/make-editor)
                                                            (assoc :base-style (:base-style editor)))
                                         :mx-commands (into [] (sort-by (fn [intent]
                                                                          [(name intent) (namespace intent)])
                                                                        (:mx-commands editor)))}
                                        ::util.ui/ui (mx-selector {:adjust-height (fn [height]
                                                                                    (max 0
                                                                                         (- height 20)))
                                                                   :$editor $editor
                                                                   :extra {}
                                                                   :$extra [$editor (com.rpl.specter/must ::util.ui/ui) '(keypath :extra)]}))]
                      editor))}))
