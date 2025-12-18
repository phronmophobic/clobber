(ns com.phronemophobic.clobber.editor
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [membrane.component :refer [defeffect defui]]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.modes.text.ui :as tui]
            [com.phronemophobic.clobber.modes.org :as org-mode]
            [com.phronemophobic.clobber.modes.org.ui :as oui]
            [com.phronemophobic.clobber.modes.clojure.ui :as cui])
  (:import java.io.File))


(defn ^:private file-ext [^File f]
  (let [fname (.getName f)
        idx (.lastIndexOf fname ".")]
    (when (not= -1 idx)
      (subs fname idx))))



(defn guess-mode
  [{:keys [file source] :as opts}]
  (let [mode
        (cond
          (:mode opts) (:mode opts)

          (or (when file
                (#{".edn" ".clj" ".cljc"} (file-ext file)))
              (:ns opts))
          :clojure
          
          (= ".org" (when file
                      (file-ext file)))
          :org

          :else :text)]
    mode))

(defn editor-ui [mode]
  (case mode
    :clojure #'com.phronemophobic.clobber.modes.clojure.ui/code-editor
    :org #'com.phronemophobic.clobber.modes.org.ui/org-editor
    ;; else
    #'com.phronemophobic.clobber.modes.text.ui/text-editor))

(defn make-editor
  "Creates a new editor with associated ui.
  
  Returns a map with `:editor` and `:ui`.
  
  If `:mode` is provided, use that mode. Otherwise, try to guess the right mode.
  
  If `:source` is provided, use that. Otherwise, load from `:file`."
  ([opts]
   (case (or (:mode opts)
             (guess-mode opts))
     :clojure (cui/make-editor opts)
     
     :org (oui/make-editor opts)
     
     ;; else
     (tui/make-editor opts))))