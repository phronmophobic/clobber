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
           com.ibm.icu.text.BreakIterator))

(import 'io.lacuna.bifurcan.Rope)


(defn editor-newline [editor]
  (text-mode/editor-self-insert-command editor "\n"))

(def key-bindings
  { ;; "C-M-x" editor-eval-top-form

   "C-a" #'text-mode/editor-move-beginning-of-line
   "C-d" #'text-mode/editor-delete-char
   "C-e" #'text-mode/editor-move-end-of-line
   "C-f" #'text-mode/editor-forward-char
   "C-b" #'text-mode/editor-backward-char

   "C-j" #'editor-newline
   "RET" #'editor-newline
   
   ;; "C-k" #'editor-paredit-kill
   "C-l" #'text-mode/editor-recenter-top-bottom
   "C-n" #'text-mode/editor-next-line
   "C-o" #'text-mode/editor-open-line
   "C-p" #'text-mode/editor-previous-line
   "C-v" #'text-mode/editor-scroll-down

   ;; "M-d" #'paredit-forward-kill-word
   ;; "M-b" #'editor-backward-word
   ;; "M-f" #'editor-forward-word
   "M-v" #'text-mode/editor-scroll-up
   
   "M-<" #'text-mode/editor-beginning-of-buffer
   "M->" #'text-mode/editor-end-of-buffer

   "DEL" #'text-mode/editor-delete-backward-char
   ;; "M-DEL" #'paredit-backward-kill-word
   ;; "RET" #'paredit-newline
   "<right>" #'text-mode/editor-forward-char
   "<up>" #'text-mode/editor-previous-line
   "<down>" #'text-mode/editor-next-line
   "<left>" #'text-mode/editor-backward-char
   ;; "TAB" #'editor-indent
   ;; "C-M-q" #'editor-indent-region

   "C-y" #'text-mode/editor-yank
   "C-SPC" #'text-mode/editor-set-mark
   "C-w" #'text-mode/editor-kill-region
   "M-w" #'text-mode/editor-save-region
   ;; "C-x C-s" editor-save-buffer

   "M-SPC" #'text-mode/editor-single-space
   "M-\\" #'text-mode/editor-delete-horizontal-space})

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

