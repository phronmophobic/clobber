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
  "Creates a new editor.
  
  Returns a map with `:editor` and `:ui`.
  
  If `:mode` is provided, use that mode. Otherwise, try to guess the right mode.
  
  If `:source` is provided, us that. Otherwise, load from `:file`."
  [{:keys [file source mode] :as opts}]
  (let [mode
        (cond
          (or (= mode :clojure)
              (when file
                (#{".edn" ".clj" ".cljc"} (file-ext file)))
              (:ns opts))
          :clojure
          
          (= ".org" (when file
                      (file-ext file)))
          :org

          :else :text)]
    (case mode
      :clojure
      {:editor (cui/make-editor opts)
       :ui #'com.phronemophobic.clobber.modes.clojure.ui/code-editor}
      
      :org
      {:editor (oui/make-editor opts)
       :ui #'com.phronemophobic.clobber.modes.org.ui/org-editor}

      ;; else
      {:editor (tui/make-editor opts)
       :ui #'com.phronemophobic.clobber.modes.text.ui/text-editor})))





