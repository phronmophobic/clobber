(ns com.phronemophobic.clobber
  (:require [clojure.java.io :as io]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defeffect defui]]
            [com.phronemophobic.membrandt :as ant]
            [ropes.core :as ropes]
            [com.phronemophobic.viscous :as viscous]
            [clojure.tools.analyzer.jvm :as ana.jvm])
  (:import (org.treesitter TSLanguage
                           TSQuery
                           TSParser
                           TSTree
                           TSTreeCursor
                           TSNode
                           TSPoint
                           TSReader
                           TSQueryCapture
                           TSQueryCursor
                           TSQueryMatch
                           TreeSitterClojure
                           TreeSitterJson
                           TSInputEdit
                           TSInputEncoding)
           java.nio.charset.Charset
           java.util.Arrays
           ;; java.text.BreakIterator
           java.nio.ByteBuffer
           io.lacuna.bifurcan.Rope))

(import 'com.ibm.icu.text.BreakIterator)
(require 'virgil)
#_(defonce recompile (virgil/compile-java ["/Users/adrian/workspace/bifurcan/src"]))
(import 'io.lacuna.bifurcan.Rope)
;; https://lacuna.io/docs/bifurcan/io/lacuna/bifurcan/Rope.html

#_(def eval-ns *ns*)




(def s "👻👩‍👩‍👦‍👦")
(count-points s)
(-> (Rope/from s) .size)
(.length s)
(alength (.getBytes s "utf-8"))

#_(def s
    #_(pr-str
       '(do "asdf"
            1 2 #{}
            (def foo 42)))
    (slurp
     #_(io/resource "com/phronemophobic/easel.clj")
     (io/resource "com/phronemophobic/clobber.clj")))

;; (defonce clojure-lang (TreeSitterClojure.))
;; (defonce json-lang (TreeSitterJson.))
;; (defonce parser (doto (TSParser.)
;;               (.setLanguage clojure-lang)))










(comment

  (dev/add-component-as-applet #'debug {})
  ,)


#_(defprotocol ICodeEditor
  (forward-char [_])
  (backward-char [_])
  (forward-word [_])
  (backward-word [_])
  (forward-paragraph [_])
  (backward-paragraph [_])
  (move-beginning-of-line [_])
  (move-end-of-line [_])
  (next-line [_])
  (previous-line [_])

  (cider-eval-defun-at-point [_])
  (cider-eval-last-sexp [_])

  (paredit-forward [_])
  (paredit-backward [_])
  (paredit-open-square [_])
  (paredit-close-square [_])
  (paredit-open-round [_])
  (paredit-close-round [_])
  (paredit-open-curly [_])
  (paredit-close-curly [_])
  (paredit-kill [_])

  (set-mark-command [_])
  (kill-region [_])
  ;; backspace
  (paredit-backward-delete [_])
  ;; normal typing
  (self-insert-command [_ s])
  (goto-line [_ n])
  (end-of-buffer [_])
  (beginning-of-buffer [_])
  (isearch-forward [_])
  (kill-ring-save [_])

  )




#_(defrecord Editor [tree cursor paragraph rope buf]
  ICodeEditor
  (forward-char [this]
    (editor-forward-char this))
  (backward-char [this]
    (editor-backward-char this))
  (forward-word [this]
    (editor-forward-word this))
  (backward-word [this]
    (editor-backward-word this))
  (forward-paragraph [this]
    (editor-forward-paragraph this))
  (backward-paragraph [this]
    (editor-backward-paragraph this))
  (next-line [this]
    (editor-next-line this))
  (previous-line [this]
    (editor-previous-line this))
  (move-beginning-of-line [this]
    (editor-move-beginning-of-line this))
  (move-end-of-line [this]
    (editor-move-end-of-line this))
  (cider-eval-defun-at-point [this]
    (editor-cider-eval-defun-at-point this))
  (cider-eval-last-sexp [this]
    (editor-cider-eval-last-sexp this))
  (paredit-forward [this]
    (editor-paredit-forward this))
  (paredit-backward [this]
    (editor-paredit-backward this))
  (paredit-open-square [this]
    (editor-paredit-open-square this))
  (paredit-close-square [this]
    (editor-paredit-close-square this))
  (paredit-open-round [this]
    (editor-paredit-open-round this))
  (paredit-close-round [this]
    (editor-paredit-close-round this))
  (paredit-open-curly [this]
    (editor-paredit-open-curly this))
  (paredit-close-curly [this]
    (editor-paredit-close-curly this))
  (paredit-kill [this]
    (editor-paredit-kill this))
  (set-mark-command [this]
    (editor-set-mark-command this))
  (kill-region [this]
    (editor-kill-region this))
  (paredit-backward-delete [this]
    (editor-paredit-backward-delete this))
  (self-insert-command [this s]
    (editor-self-insert-command this s))
  (goto-line [this n]
    (editor-goto-line this n))
  (end-of-buffer [this]
    (editor-end-of-buffer this))
  (beginning-of-buffer [this]
    (editor-beginning-of-buffer this))
  (isearch-forward [this]
    (editor-isearch-forward this))
  (kill-ring-save [this]
    (editor-kill-ring-save this)))

(defn make-editor
  ([]
   (make-editor clojure-lang))
  ([lang]
   (map->Editor
    {:tree nil
     ;; all offsets in terms of bytes
     ;; :offsets {:lines nil
     ;;           :graphemes nil}
     :cursor {:byte 0
              :char 0
              :point 0
              :row 0
              :column 0}
     :viewport {:start-line 0
                :num-lines 40}
     :paragraph nil
     :base-style #:text-style {:font-families ["Menlo"]
                               :font-size 12}
     :rope Rope/EMPTY
     :language lang
     :parser (doto (TSParser.)
               (.setLanguage lang))
     :buf (byte-array 4096)})))




;; BreakIterator requires java Character sequences
;; https://docs.oracle.com/en/java/javase/23/docs/api/java.base/java/lang/Character.html#unicode
;; The char data type (and therefore the value that a Character object encapsulates) are based on the original Unicode specification, which defined characters as fixed-width 16-bit entities. The Unicode Standard has since been changed to allow for characters whose representation requires more than 16 bits. 

;; A char value, therefore, represents Basic Multilingual Plane (BMP) code points, including the surrogate code points, or code units of the UTF-16 encoding. An int value represents all Unicode code points, including supplementary code points.




