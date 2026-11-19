(ns com.phronemophobic.clobber.modes.markdown
  (:require [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util :as util])
  (:import (org.treesitter TreeSitterMarkdown)))


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

