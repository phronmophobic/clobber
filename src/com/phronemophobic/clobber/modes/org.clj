(ns com.phronemophobic.clobber.modes.org
  (:require [com.phronemophobic.clobber.modes.text :as text-mode]
            [clojure.edn :as edn]
            [com.phronemophobic.clobber.util :as util]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.treesitter TSTree
                           TSInputEdit
                           TSInputEncoding
                           TSTreeCursor
                           TreeSitterOrg
                           TSParser
                           TSPoint
                           TSNode)
           com.ibm.icu.text.BreakIterator
           clojure.lang.LineNumberingPushbackReader
           java.io.StringReader
           io.lacuna.bifurcan.Rope))



(defn org-return [editor]
  editor)

(def key-bindings
  (assoc text-mode/key-bindings
         "M-RET" #'org-return))

(defn make-editor []
  (let [lang (TreeSitterOrg.)]
    {:tree nil
     :cursor {:byte 0
              :char 0
              :point 0
              :row 0
              :column-byte 0}
     :rope Rope/EMPTY
     :language lang
     :parser (doto (TSParser.)
               (.setLanguage lang))
     :buf (byte-array 4096)}))


