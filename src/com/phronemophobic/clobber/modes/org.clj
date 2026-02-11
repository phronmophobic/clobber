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
  (let [^TSTree
        tree (:tree editor)
        line (-> editor :cursor :row)
        cursor-byte (-> editor :cursor :byte )
        
        tree-cursor
        (doto (TSTreeCursor. (TSTree/.getRootNode tree))
          (util/skip-to-byte-offset cursor-byte))

        parent-list-node  (transduce
                           (comp
                            (take-while
                             (fn [node]
                               (<= (-> node TSNode/.getStartByte)
                                   cursor-byte)))
                            (filter
                             (fn [node]
                               (let [end-byte (-> node TSNode/.getEndByte)]
                                 (and
                                  (<= cursor-byte end-byte)
                                  (= "listitem" (TSNode/.getType node)))))))
                           (completing
                            (fn [old-node new-node]
                              new-node))
                           nil
                           (util/tree-cursor-reducible tree-cursor))]

    (if parent-list-node
      (let [offset (-> parent-list-node
                       (TSNode/.getStartPoint)
                       (TSPoint/.getColumn))
            insert-string (str
                           "\n"
                           (str/join "" (repeat offset " "))
                           "- ")]
        (text-mode/editor-self-insert-command editor insert-string))
      ;; else
      editor)))

(defn org-indent-right [editor]
  (let [original-cursor (:cursor editor)
        editor (-> editor
                   (text-mode/editor-move-beginning-of-line)
                   (text-mode/editor-insert "  "))
        
        editor (update editor
                       :cursor
                       (fn [cursor]
                         (reduce (fn [m k]
                                   (assoc m k (+ 2 (get original-cursor k))))
                                 cursor
                                 [:byte :char :point :column-byte])))]
    editor))



(defn org-indent-left [editor]
  (let [original-cursor (:cursor editor)
        editor (text-mode/editor-move-beginning-of-line editor)
        [num-deletes editor]
        (loop [num-deletes 0
               editor editor]
          (let [char (Rope/.charAt (:rope editor) (-> editor :cursor :char))]
            (if (= \space char)
              (let [next-num-deletes (inc num-deletes)
                    next-editor (text-mode/editor-delete-char editor)]
                (if (>= next-num-deletes 2)
                  [next-num-deletes next-editor]
                  (recur (inc num-deletes) next-editor)))
              [num-deletes editor])))
        
        editor (update editor
                       :cursor
                       (fn [cursor]
                         (reduce (fn [m k]
                                   (assoc m k (- (get original-cursor k)
                                                 num-deletes)))
                                 cursor
                                 [:byte :char :point :column-byte])))]
    editor))

(def key-bindings
  (assoc text-mode/key-bindings
         "M-TAB" #'org-indent-left
         "TAB" #'org-indent-right
         "M-<right>" #'org-indent-right
         "M-<left>" #'org-indent-left
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

