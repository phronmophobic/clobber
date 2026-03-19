(ns com.phronemophobic.clobber.modes.markdown
  (:require [com.phronemophobic.clobber.modes.text :as text-mode]
            [clojure.edn :as edn]
            [com.phronemophobic.clobber.util :as util]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.treesitter TSTree
                           TSInputEdit
                           TSInputEncoding
                           TSTreeCursor
                           TreeSitterMarkdown
                           TSParser
                           TSPoint
                           TSNode)
           com.ibm.icu.text.BreakIterator
           io.lacuna.bifurcan.Rope))


(def key-bindings
  (merge text-mode/key-bindings
         {}
         ;; "M-TAB" #'org-indent-left
         ;; "TAB" #'org-indent-right
         ;; "M-<right>" #'org-indent-right
         ;; "M-<left>" #'org-indent-left
         ;; "M-RET" #'org-return
         ))

(comment
  
  (-> (util/parse 
       (TreeSitterMarkdown.)
       "### foo
       123
       ```clojure
       (+ 1 2 3)     
       ```
       `alksdjf`
       ")
      (.getRootNode)
      println)
  ,)

