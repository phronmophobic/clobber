(ns com.phronemophobic.clobber.modes.clojure.ui.stacktrace
  (:require [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [membrane.skia.paragraph :as para]
            clojure.main
            [clojure.java.io :as io]
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
  (let [class-name (.getClassName element)
        [ns fn-name & _] (-> class-name clojure.main/demunge (str/split #"/" 3))
        var (when (and ns fn-name)
              (resolve (symbol ns fn-name)))
        mta (when var
              (meta var))

        path* (-> class-name
                  (str/replace #"\$.*$" "")
                  (str/replace #"\." "/"))
        resource (some
                  (fn [suffix]
                    (io/resource (str path* suffix)))
                  [".clj" ".cljc"])]
    {:var var
     :class-name class-name
     :ns-name ns
     :resource resource
     :meta mta}))


(defn ->paragraph [{:keys [ns line height resource] :as m}]
  (when (not (or ns resource))
    (throw (ex-info "Expected ns or resource"
                    m)))
  (let [editor (if ns
                 (cui/make-editor {:ns ns})
                 (cui/make-editor {:file resource}))
        editor (-> editor
                   (text-mode/editor-goto-line line)
                   (text-mode/editor-move-end-of-line)
                   (text-mode/editor-set-mark)
                   (text-mode/editor-move-beginning-of-line)
                   (text-mode/editor-recenter-top-bottom))
        editor (if height
                 (cui/editor-set-height editor height)
                 editor)]
    (cui/editor->paragraph editor)))

(defeffect ::open-editor [{:keys [ns line file] :as m}]
  (dispatch! 
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
       handler
       m))}))

(defui stacktrace-viewer [{:keys [^Throwable exception
                                  paragraph]}]
  (let [[cw ch] (:membrane.stretch/container-size context)
        list-height (quot ch 4)
        editor-height (- ch list-height)

        paragraph-cache (get extra [::paragraph-cache exception] {})

        trace
        (when exception
          (into []
                (map (fn [element]
                       (let [{:keys [var meta ns-name resource]} (element->info element)
                             line (.getLineNumber element)]
                         (if (and line (or var resource))
                           (let [fn-name (when var
                                           (-> var .sym name))]
                             (ui/on
                              :mouse-down (fn [_]
                                            [[::open-editor {:ns (when var
                                                                   
                                                                   (.ns var))
                                                             :resource resource
                                                             :url resource
                                                             :line (dec line)
                                                             :height editor-height}]])
                              :mouse-move (fn [_]
                                            
                                            (let [pinfo {:ns (when var
                                                               (.ns var))
                                                         :resource resource
                                                         :line (dec line)
                                                         :height editor-height}]
                                              (if-let [p (get paragraph-cache pinfo)]
                                                [[:set $paragraph p]]
                                                (let [p (->paragraph pinfo)]
                                                  [[:set $paragraph p]
                                                   [:update $paragraph-cache assoc pinfo p]]))))
                              (para/paragraph 
                               [fn-name " " ns-name])))
                           ;; else
                           (para/paragraph 
                            [(.getClassName element)])))))
                (.getStackTrace exception)))]
    [(dnd/on-drop
      (fn [pos obj]
        (when-let [x* (:x obj)]
          (let [x @x*]
            (when (instance? Throwable x)
              [[:set $paragraph nil]
               [:set $exception x]]))))
      (ui/fixed-bounds [cw ch] nil))
     (ui/vertical-layout
      (when exception
        (ui/vertical-layout
         (para/paragraph
          (ex-message exception))
         (ui/filled-rectangle 
          [0.75 0.75 0.75 ]
          cw
          3)
         (ui/spacer 0 10)
         (viscous/inspector 
          {:obj (viscous/->APWrapped exception)}))
        
        
        )
      (basic/scrollview
       {:scroll-bounds [(- cw 20) list-height]
        :body (ui/flex-layout
               trace
               {:direction :column})})
      paragraph)]))

(comment
  (dev/add-component-as-applet
   #'stacktrace-viewer
   {:exception nil})

  (dev/add-component-as-applet
   #'stacktrace-viewer
   {:exception mye2})
  
  '[] 42
  
  (def mytimeline x-119362 )
  (def mye2 (:throwable (nth mytimeline 15297)))
  ,)