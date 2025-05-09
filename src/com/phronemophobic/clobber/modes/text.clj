(ns com.phronemophobic.clobber.modes.text
  (:require [com.phronemophobic.clobber.util :as util])
  (:import com.ibm.icu.text.BreakIterator
           (org.treesitter TSTree
                           TSInputEdit
                           TSInputEncoding
                           TSParser
                           TSPoint)))

#_(defonce recompile (virgil/compile-java ["/Users/adrian/workspace/bifurcan/src"]))
(import 'io.lacuna.bifurcan.Rope)


(defn editor-update-viewport [editor]
  (let [row (-> editor :cursor :row)
        {:keys [start-line num-lines]} (:viewport editor)]
    (cond

      (< row start-line)
      (let [ ;; check if small scroll works
            new-start-line (max 0 (- start-line
                                     (quot num-lines 2)))

            ;; if not, just center view
            new-start-line (if (< row new-start-line)
                             (max 0
                                  (- row
                                     (quot num-lines 2)))
                             new-start-line)]
        (assoc-in editor [:viewport :start-line] new-start-line))

      (> row (+ start-line num-lines))
      (let [new-start-line (+ start-line
                              (quot num-lines 2))

            new-start-line (if (> row (+ new-start-line num-lines))
                             (max 0
                                  (- row
                                     (quot num-lines 2)))
                             new-start-line
                             )]
        (assoc-in editor [:viewport :start-line] new-start-line))

      :else editor)))

(defn editor-recenter-top-bottom [editor]
  (let [row (-> editor :cursor :row)
        {:keys [start-line num-lines]} (:viewport editor)

        middle (max 0
                    (- row
                       (quot num-lines 2)))
        top row
        bottom (max 0
                    (inc (- row num-lines)))]

    (cond
      ;; middle, go to top
      (= start-line middle)
      (assoc-in editor
                [:viewport :start-line]
                top)


      ;; at top, go to bottom
      (= start-line top)
      (assoc-in editor
                [:viewport :start-line]
                bottom)

      ;; goto middle
      :else
      (assoc-in editor
                [:viewport :start-line]
                middle))))

(defn editor-forward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        next-char (.following bi cursor-char)]
    (if (= -1 next-char)
      editor
      (let [diff-string (-> (.subSequence cs cursor-char next-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (+ cursor-char (.length diff-string))
                         :point (+ cursor-point (util/num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))



(defn editor-backward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence cs prev-char cursor-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (dec cursor-row)
                             cursor-row)

            new-cursor-column (if newline?
                                (loop [column 0
                                       index (.previous bi)]
                                  (if (>= index 0)
                                    (let [c (.charAt cs index)]
                                      (if (= \newline c)
                                        column
                                        (recur (inc column)
                                               (.previous bi))))
                                    column))
                                (dec cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (- cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (- cursor-char (.length diff-string))
                         :point (- cursor-point (util/num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))

(defn editor-forward-word [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getWordInstance)
             (.setText cs))

        next-char (.following bi cursor-char)]
    (if (= -1 next-char)
      editor
      (let [diff-string (-> (.subSequence cs cursor-char next-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (+ cursor-char (.length diff-string))
                         :point (+ cursor-point (util/num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))
(defn editor-backward-word [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getWordInstance)
             (.setText cs))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence cs prev-char cursor-char )
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (- cursor-byte (alength (.getBytes diff-string "utf-8")))
                         :char (- cursor-char (.length diff-string))
                         :point (- cursor-point (util/num-points diff-string))
                         :row new-cursor-row
                         :column new-cursor-column}))))))
(defn editor-forward-paragraph [editor]
  editor)
(defn editor-backward-paragraph [editor]
  editor)
(defn editor-move-beginning-of-line [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        char-index (loop [char-index cursor-char]
                     (let [prev-char (.preceding bi char-index)]
                       (if (or (= -1 prev-char)
                               (= \newline (.charAt cs prev-char)))
                         char-index
                         (recur prev-char))))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence cs char-index cursor-char)
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))
            new-cursor-column (- cursor-column num-bytes)]
        (editor-update-viewport
         (assoc editor
                :cursor {:byte (- cursor-byte num-bytes)
                         :char (- cursor-char (.length diff-string))
                         :point (- cursor-point (util/num-points diff-string))
                         :row cursor-row
                         :column new-cursor-column})))))
  )
(defn editor-move-end-of-line [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        cs (.toCharSequence rope)
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText cs))

        char-index (loop [char-index cursor-char]
                     (let [next-char (.following bi char-index)]
                       (cond
                         (= -1 next-char) char-index
                         (= \newline (.charAt cs char-index)) char-index
                         :else (recur next-char))))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence cs cursor-char char-index )
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))
            new-cursor-column (+ cursor-column num-bytes)]
        (assoc editor
               :cursor {:byte (+ cursor-byte num-bytes)
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point (util/num-points diff-string))
                        :row cursor-row
                        :column new-cursor-column})))))

(defn editor-previous-line
  ([editor]
   (editor-previous-line editor 1))
  ([editor n]
   (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-target-column :target-column
          cursor-column :column} cursor

         target-column (or cursor-target-column
                           cursor-column)

         cs (.toCharSequence rope)
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText cs))

         ;; find previous newline twice
         [lines char-index] (loop [char-index cursor-char
                                   lines 0]
                              (let [prev-char (.preceding bi char-index)]
                                (cond
                                  (= -1 prev-char) [lines char-index]
                                  (= \newline (.charAt cs prev-char)) (if (= n lines)
                                                                        [lines char-index]
                                                                        (recur prev-char (inc lines)))
                                  :else (recur prev-char lines))))

         ;; keep going until target column
         [char-index column]
         (loop [char-index char-index
                column 0]
           (let [next-char (.following bi char-index)]
             (cond
               (= -1 next-char) [char-index column]
               (= \newline (.charAt cs char-index)) [char-index column]
               (= column target-column) [char-index column]
               :else (recur next-char (inc column)))))]
     (if (= char-index cursor-char)
       editor
       (let [diff-string (-> (.subSequence cs char-index cursor-char)
                             .toString)
             num-bytes (alength (.getBytes diff-string "utf-8"))
             new-cursor-row (- cursor-row lines)]
         (editor-update-viewport
          (assoc editor
                 :cursor {:byte (- cursor-byte num-bytes)
                          :char (- cursor-char (.length diff-string))
                          :point (- cursor-point (util/num-points diff-string))
                          :row new-cursor-row
                          :target-column target-column
                          :column column})))))))


(defn editor-self-insert-command [editor ^String s]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        ;; cursor is { :byte, :row, :column }
        sbytes (.getBytes s "utf-8")
        
        point-offset (util/count-points s)
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        new-cursor-row (+ cursor-row (:row point-offset))
        new-cursor-column (if (pos? (:row point-offset))
                            (:column point-offset)
                            (+ (:column point-offset) cursor-column))


        new-tree (when-let [^TSTree
                            tree tree]
                   (let [tree (.copy tree)]
                     (.edit tree (TSInputEdit. cursor-byte cursor-byte (+ cursor-byte (alength sbytes))
                                               (TSPoint. cursor-row cursor-column)
                                               (TSPoint. cursor-row cursor-column)
                                               (TSPoint. new-cursor-row new-cursor-column)))
                     tree))

        new-rope (.insert rope ^int cursor-point s)
        
        reader (util/->RopeReader new-rope)
        new-tree (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 )]
    (editor-update-viewport
     (assoc editor
            :tree new-tree
            :cursor {:byte (+ cursor-byte (alength sbytes))
                     :char (+ cursor-char (.length s))
                     :point (+ cursor-point (util/num-points s))
                     :row new-cursor-row
                     :column new-cursor-column}
            :paragraph nil
            :rope new-rope))))

(defn editor-open-line [editor]
  (-> editor
      (editor-self-insert-command "\n")
      (assoc :cursor (:cursor editor))))

(defn editor-next-line
  ([editor]
   (editor-next-line editor 1))
  ([editor n]
   (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-target-column :target-column
          cursor-column :column} cursor

         target-column (or cursor-target-column
                           cursor-column)

         cs (.toCharSequence rope)
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText cs))

         ;; find newline
         [lines char-index] (loop [char-index cursor-char
                                   lines 0
                                   ;; only used when hitting
                                   ;; end of buffer
                                   last-line-char nil]
                              (let [next-char (.following bi char-index)]
                                (cond
                                  ;; last line. go to last line char
                                  (= -1 next-char) (if (nil? last-line-char)
                                                     ;; started on last line
                                                     [0 cursor-char]
                                                     [lines last-line-char])
                                  (= \newline (.charAt cs char-index)) (let [lines (inc lines)]
                                                                         (if (= lines n)
                                                                           [lines next-char]
                                                                           (recur next-char lines next-char)))
                                  :else (recur next-char lines last-line-char))))

         ;; keep going until target column
         [char-index column]
         (if (= cursor-char char-index)
           [char-index cursor-column]
           (loop [char-index char-index
                  column 0]
             (let [next-char (.following bi char-index)]
               (cond

                 (= -1 next-char) [char-index column]
                 (= \newline (.charAt cs char-index)) [char-index column]
                 (= column target-column) [char-index column]
                 :else (recur next-char (inc column))))))]
     (if (= char-index cursor-char)
       editor
       (let [diff-string (-> (.subSequence cs cursor-char char-index )
                             .toString)
             num-bytes (alength (.getBytes diff-string "utf-8"))
             new-cursor-column (+ cursor-column num-bytes)
             new-cursor-row (+ cursor-row lines)]
         (editor-update-viewport
          (assoc editor
                 :cursor {:byte (+ cursor-byte num-bytes)
                          :char (+ cursor-char (.length diff-string))
                          :point (+ cursor-point (util/num-points diff-string))
                          :row new-cursor-row
                          :target-column target-column
                          :column column})))))))

(defn editor-scroll-down [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-next-line editor to-scroll)))

(defn editor-scroll-up [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-previous-line editor to-scroll)))



(defn editor-set-mark-command [editor]
  editor)
(defn editor-kill-region [editor]
  editor)

(defn editor-goto-line [editor n]
  editor)

(defn editor-end-of-buffer [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        ;; could probably be more efficient
        {:keys [row column]} (util/count-points (.toString rope))]
    (editor-update-viewport
     (assoc editor
            :cursor {:byte (.numBytes rope)
                     :char (-> rope .toCharSequence .length)
                     :point (-> rope .size)
                     :row row
                     :column column}))))

(defn editor-beginning-of-buffer [editor]
  (editor-update-viewport
   (assoc editor
          :cursor {:byte 0
                   :char 0
                   :point 0
                   :row 0
                   :column 0})))
(defn editor-isearch-forward [editor]
  editor)
(defn editor-kill-ring-save [editor]
  editor)
