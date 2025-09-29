(ns com.phronemophobic.clobber.modes.clojure.ui.stacktrace
  (:require [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [membrane.skia.paragraph :as para]
            clojure.main
            [membrane.component :refer [defui defeffect]]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.clobber.util :as util]
            [com.phronemophobic.clobber.util.ui :as util.ui]
            [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.modes.clojure.ui :as cui]
            [com.phronemophobic.viscous :as viscous]            
            [com.phronemophobic.membrandt :as ant])
  (:import io.lacuna.bifurcan.Rope))


(defn element->info [^StackTraceElement element]
  (let [classname (.getClassName element)
        [ns fn-name & _] (-> classname clojure.main/demunge (str/split #"/" 3))
        var (when (and ns fn-name)
              (resolve (symbol ns fn-name)))
        mta (when var
              (meta var))]
    {:var var
     :meta mta}))

(defn ->paragraph [{:keys [ns line height]}]
  (let [editor (-> (cui/make-editor {:ns ns})
                   (text-mode/editor-goto-line line)
                   (text-mode/editor-move-end-of-line)
                   (text-mode/editor-set-mark)
                   (text-mode/editor-move-beginning-of-line)
                   (text-mode/editor-recenter-top-bottom))
        editor (if height
                 (cui/editor-set-height editor height)
                 editor)]
    (cui/editor->paragraph editor)))

(def ->paragraph-memo (memoize ->paragraph))

 
(defui stacktrace-viewer [{:keys [^Throwable exception
                                  paragraph]}]
  (let [[cw ch] (:membrane.stretch/container-size context)
        list-height (quot ch 4)
        editor-height (- ch list-height)

        trace
        (into []
              (map (fn [element]
                     (let [{:keys [var meta]} (element->info element)
                           line (.getLineNumber element)]
                       (if (and var line)
                         (let [fn-name (-> var .sym name)
                               ns (-> var .ns ns-name name)]
                           (ui/on
                            :mouse-move (fn [_]
                                          [[:set $paragraph
                                            (->paragraph-memo {:ns (.ns var)
                                                               :line (dec line)
                                                               :height editor-height})]])
                            
                            (para/paragraph 
                             [fn-name " " ns])))
                         ;; else
                         (para/paragraph (.getClassName element))))))
              (.getStackTrace exception))
        
]
    [(dnd/on-drop
      (fn [pos obj]
        (dev/dtap obj)
        
        (when-let [x* (:x obj)]
          
          (let [x @x*]
            (when (instance? Throwable x)
              [[:set $paragraph nil]
               [:set $exception x]]))))
      (ui/fixed-bounds [cw ch] nil))
     (ui/vertical-layout
     (basic/scrollview
      {:scroll-bounds [(- cw 20) list-height]
       :body (ui/flex-layout
              trace
              {:direction :column})})
     paragraph)]))

(comment
  
  (dev/add-component-as-applet
   #'stacktrace-viewer
   {:exception mye2})
  
  '[] 42
  
  (def mytimeline x-119362 )
  (def mye2 (:throwable (nth mytimeline 15297)))
  ,)