(ns com.phronemophobic.clobber.modes.text
  (:require [com.phronemophobic.clobber.util :as util]
            [clojure.string :as str])
  (:import com.ibm.icu.text.BreakIterator
           java.util.regex.Pattern
           (org.treesitter TSTree
                           TSInputEdit
                           TSInputEncoding
                           TSParser
                           TSPoint)
           io.lacuna.bifurcan.Rope))

(defn editor-goto-byte
  "Moves the :cursor of editor to `byte-index`. Does not change anything else."
  [editor byte-index]
  (let [cursor (:cursor editor)
        ^Rope
        rope (:rope editor)
        {cursor-byte :byte} cursor

        cursor
        (cond
          (= cursor-byte byte-index) cursor

          (> byte-index cursor-byte)
          (let [diff-rope (.sliceBytes rope cursor-byte byte-index)
                diff-string (.toString diff-rope)
                rc-offset (util/count-row-column-bytes diff-string)
                new-char (+ (:char cursor)
                            (.length diff-string))
                new-point (+ (:point cursor)
                             (util/num-points diff-string))
                new-cursor-row (+ (:row cursor) (:row rc-offset))
                new-cursor-column-byte (if (pos? (:row rc-offset))
                                         (:column-byte rc-offset)
                                         (+ (:column-byte rc-offset) (:column-byte cursor)))]
            {:byte byte-index
             :char new-char
             :point new-point
             :row new-cursor-row
             :column-byte new-cursor-column-byte})

          ;; byte-index before cursor
          :else
          (let [diff-rope (.sliceBytes rope byte-index cursor-byte)
                diff-string (.toString diff-rope)
                new-char (- (:char cursor)
                            (.length diff-string))
                new-point (- (:point cursor)
                             (util/num-points diff-string))

                rc-offset (util/count-row-column-bytes diff-string)
                new-cursor-row (- (:row cursor)
                                  (:row rc-offset))

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))
                new-cursor-column-byte
                (loop [char-index new-char]
                  (let [prev-char-index (.preceding bi char-index)]
                    (if (or (= -1 prev-char-index)
                            (= \newline (.charAt rope prev-char-index)))
                      (-> (.subSequence rope char-index new-char)
                          .toString
                          (.getBytes)
                          alength)
                      ;; else
                      (recur prev-char-index))))]
            {:byte byte-index
             :char new-char
             :point new-point
             :row new-cursor-row
             :column-byte new-cursor-column-byte}))]
    (assoc editor
           :cursor cursor)))

(defn editor-goto-char
  "Moves the :cursor of editor to `char-index`. Does not change anything else."
  [editor char-index]
  (let [cursor (:cursor editor)
        ^Rope
        rope (:rope editor)
        {cursor-char :char} cursor

        cursor
        (cond
          (= cursor-char char-index) cursor

          (> char-index cursor-char)
          (let [
                diff-string (-> (.subSequence rope cursor-char char-index)
                                .toString)
                rc-offset (util/count-row-column-bytes diff-string)
                new-byte (+ (:byte cursor) (-> (.getBytes diff-string) alength))
                new-char char-index
                new-point (+ (:point cursor)
                             (util/num-points diff-string))
                new-cursor-row (+ (:row cursor) (:row rc-offset))
                new-cursor-column-byte (if (pos? (:row rc-offset))
                                         (:column-byte rc-offset)
                                         (+ (:column-byte rc-offset) (:column-byte cursor)))]
            {:byte new-byte
             :char new-char
             :point new-point
             :row new-cursor-row
             :column-byte new-cursor-column-byte})

          ;; char-index before cursor
          :else
          (let [diff-string (-> (.subSequence rope char-index cursor-char)
                                .toString)
                new-byte (- (:byte cursor)
                            (-> diff-string .getBytes alength))
                new-char char-index
                new-point (- (:point cursor)
                             (util/num-points diff-string))
                
                rc-offset (util/count-row-column-bytes diff-string)
                new-cursor-row (- (:row cursor)
                                  (:row rc-offset))

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))
                new-cursor-column-byte
                (loop [char-index new-char]
                  (let [prev-char-index (.preceding bi char-index)]
                    (if (or (= -1 prev-char-index)
                            (= \newline (.charAt rope prev-char-index)))
                      (-> (.subSequence rope char-index new-char)
                          .toString
                          (.getBytes)
                          alength)
                      ;; else
                      (recur prev-char-index))))]
            {:byte new-byte
             :char new-char
             :point new-point
             :row new-cursor-row
             :column-byte new-cursor-column-byte}))]
    (assoc editor
           :cursor cursor)))

(defn editor-insert
  "Inserts `s`.

  Inserts at current cursor if `byte-index` is not provided.

  If `row` and `column-byte` are not provided. Calculate the row and column-byte automatically"
  ([editor s]
   (let [cursor (:cursor editor)]
     (editor-insert editor s
                    (:byte cursor)
                    (:row cursor)
                    (:column-byte cursor))))
  ([editor ^String s byte-index]
   (let [cursor (:cursor editor)
         ^Rope
         rope (:rope editor)
         {cursor-byte :byte} cursor

         {:keys [row column-byte]} (if (= cursor-byte byte-index)
                                     {:row (:row cursor)
                                      :column-byte (:column-byte cursor)}
                                     (let [diff-rope (if (> byte-index cursor-byte)
                                                       (.sliceBytes rope cursor-byte byte-index)
                                                       (.sliceBytes rope byte-index cursor-byte))
                                           diff-string (.toString diff-rope)

                                           rc-offset (util/count-row-column-bytes diff-string)

                                           new-cursor-row (+ (:row cursor) (:row rc-offset))
                                           new-cursor-column-byte (if (pos? (:row rc-offset))
                                                                    (:column-byte rc-offset)
                                                                    (+ (:column-byte rc-offset) (:column-byte cursor)))]
                                       {:row new-cursor-row
                                        :column-byte new-cursor-column-byte}))]
     (editor-insert editor s byte-index row column-byte)))
  ([editor ^String s byte-index row column-byte]
   (let [sbytes (.getBytes s "utf-8")
         rc-offset (util/count-row-column-bytes s)
         delta-rows (:row rc-offset)
         
         new-row (+ row delta-rows)
         new-column-byte (if (pos? delta-rows)
                           (:column-byte rc-offset)
                           (+ (:column-byte rc-offset) column-byte))

         tree (:tree editor)
         new-tree (when-let [^TSTree
                             tree tree]
                    (let [tree (.copy tree)]
                      (.edit tree (TSInputEdit. byte-index byte-index (+ byte-index (alength sbytes))
                                                (TSPoint. row column-byte)
                                                (TSPoint. row column-byte)
                                                (TSPoint. new-row new-column-byte)))
                      tree))

         ^Rope
         rope (:rope editor)
         new-rope (.insertAtByte rope ^int byte-index s)

         ^TSParser
         parser (:parser editor)
         new-tree (when parser
                    (let [reader (util/->RopeReader new-rope)]
                      (.parse parser (:buf editor) new-tree reader TSInputEncoding/TSInputEncodingUTF8)))]
     (assoc editor
            :rope new-rope
            :tree new-tree))))

(defn editor-snip
  "Removes bytes between [`start-byte`, `end-byte`)."
  ([editor start-byte end-byte]
   (let [cursor (:cursor editor)
         ^Rope
         rope (:rope editor)
         {cursor-byte :byte} cursor

         {:keys [row column-byte]} (if (= cursor-byte start-byte)
                                     {:row (:row cursor)
                                      :column-byte (:column-byte cursor)}
                                     (let [diff-rope (if (> start-byte cursor-byte)
                                                       (.sliceBytes rope cursor-byte start-byte)
                                                       (.sliceBytes rope start-byte cursor-byte))

                                           rc-offset (util/count-row-column-bytes diff-rope)

                                           new-cursor-row (+ (:row cursor) (:row rc-offset))
                                           new-cursor-column-byte (if (pos? (:row rc-offset))
                                                                    (:column-byte rc-offset)
                                                                    (+ (:column-byte rc-offset) (:column-byte cursor)))]
                                       {:row new-cursor-row
                                        :column-byte new-cursor-column-byte}))

         diff-rope (.sliceBytes rope start-byte end-byte)
         rc-offset (util/count-row-column-bytes diff-rope)
         delta-rows (:row rc-offset)
         
         end-row (+ row delta-rows)
         end-column-byte (if (pos? delta-rows)
                           (:column-byte rc-offset)
                           (+ (:column-byte rc-offset) column-byte))

         ^TSTree
         tree (:tree editor)
         new-tree (when-let [tree tree]
                    (let [tree (.copy tree)]
                      (.edit tree (TSInputEdit. start-byte end-byte start-byte
                                                (TSPoint. row column-byte)
                                                (TSPoint. end-row end-column-byte)
                                                (TSPoint. row column-byte)))
                      tree))


         new-rope (.concat (.sliceBytes rope 0 start-byte)
                           (.sliceBytes rope end-byte (.numBytes rope)))

         ^TSParser
         parser (:parser editor)
         new-tree (when parser
                    (let [reader (util/->RopeReader new-rope)]
                      (.parse parser (:buf editor) new-tree reader TSInputEncoding/TSInputEncodingUTF8)))]
     (assoc editor
            :rope new-rope
            :tree new-tree)))
  )

;; target column is in grapheme clusters
(defn editor-goto-row-col [editor target-row target-column]
  (let [^Rope rope (:rope editor)
        
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        
        ;; find newline
        [row char-index]
        (loop [char-index 0
               lines 0]
          (if (= lines target-row)
            [lines char-index]
            
            (let [next-char (.following bi char-index)]
              (cond
                ;; last line. go to last line char
                (= -1 next-char) [lines  char-index]
                (= \newline (.charAt rope char-index)) (recur next-char (inc lines))
                :else (recur next-char lines)))))
        
        ;; keep going until target column
        line-start-char char-index
        [char-index column-byte]
        (loop [char-index char-index
               n 0]
          (let [next-char (.following bi char-index)]
            (if (or (= -1 next-char)
                    (= \newline (.charAt rope char-index))
                    (= n target-column))
              [char-index (if (zero? row)
                            (-> (.subSequence rope 0 char-index)
                                .toString
                                .getBytes
                                alength)
                            (-> (.subSequence rope line-start-char char-index)
                                .toString
                                .getBytes
                                alength))]
              (recur next-char
                     (inc n)))))
        diff-string (-> (.subSequence rope 0 char-index)
                        .toString)
        num-bytes (alength (.getBytes diff-string "utf-8"))]
    (assoc editor
           :cursor {:byte num-bytes
                    :char char-index
                    :point (util/num-points diff-string)
                    :row row
                    :column-byte column-byte})))

(defn editor-set-string
  "Updates the editor's contents, but nothing else.

  Notably, the cursor remains unchanged."
  [editor s]
  (let [rope (Rope/from s)
        ^TSParser
        parser (:parser editor)
        tree (when parser
               (let [reader (util/->RopeReader rope)]
                 (.parse parser (:buf editor) nil reader TSInputEncoding/TSInputEncodingUTF8)))]
    (assoc editor
           :rope rope
           :tree tree)))

(defn editor-clear [editor]
  (-> editor
      (assoc :rope Rope/EMPTY)
      (dissoc :tree)
      (assoc :cursor {:byte 0
                      :char 0
                      :point 0
                      :row 0
                      :column-byte 0})))

;; (defn editor-snip [editor])
;; (defn editor-slice [editor])
;; (defn editor-concat [editor])

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

      (>= row (+ start-line num-lines))
      (let [new-start-line (+ start-line
                              (quot num-lines 2))

            new-start-line (if (>= row (+ new-start-line num-lines))
                             (max 0
                                  (- row
                                     (quot num-lines 2)))
                             new-start-line)]
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

(defn editor-forward-char
  ([editor]
   (editor-forward-char editor 1))
  ([editor n]
   (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-column-byte :column-byte} cursor
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

         rope-length (.length rope)
         [end-char new-cursor-row new-cursor-column-byte]
         (loop [n n
                char-index cursor-char
                new-cursor-row cursor-row
                last-line-char nil]
           (cond
             (or (>= char-index rope-length)
                 (<= n 0))
             (let [new-cursor-column-byte (if last-line-char
                                            (-> (.subSequence rope last-line-char char-index)
                                                .toString
                                                .getBytes
                                                alength)
                                            (+ cursor-column-byte
                                               (-> (.subSequence rope cursor-char char-index)
                                                   .toString
                                                   .getBytes
                                                   alength)))]
               [char-index new-cursor-row new-cursor-column-byte])
             

             (= \newline (.charAt rope char-index))
             (let [next-char (.following bi char-index)]
               (recur (dec n)
                      next-char
                      (inc new-cursor-row)
                      next-char))

             :else
             (recur (dec n)
                    (.following bi char-index)
                    new-cursor-row
                    last-line-char)))

         diff-string (-> (.subSequence rope cursor-char end-char)
                         .toString)]
     (assoc editor
            :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                     :char (+ cursor-char (.length diff-string))
                     :point (+ cursor-point (util/num-points diff-string))
                     :row new-cursor-row
                     :column-byte new-cursor-column-byte}))))

(defn editor-delete-backward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
    (if (<= (:byte cursor) 0)
      editor
      (let [{cursor-byte :byte
             cursor-char :char
             cursor-row :row
             cursor-point :point
             cursor-column-byte :column-byte} cursor
            bi (doto (BreakIterator/getCharacterInstance)
                 (.setText rope))
            prev (.preceding bi cursor-char)
            diff-string (-> (.subSequence rope prev cursor-char)
                            .toString)
            diff-bytes (alength (.getBytes diff-string "utf-8"))

            prev-byte (- cursor-byte diff-bytes)
            prev-char (.charAt rope prev)
            prev-point (- cursor-point (util/num-points diff-string))
            newline? (= prev-char \newline)
            
            new-cursor-row (if newline?
                             (dec cursor-row)
                             cursor-row)

            new-char prev
            new-cursor-column-byte (if newline?
                                     (loop [char-index new-char]
                                       (let [prev-char (.preceding bi char-index)]
                                         (if (or (= -1 prev-char)
                                                 (= \newline (.charAt rope prev-char)))
                                           (-> (.subSequence rope char-index new-char)
                                               .toString
                                               .getBytes
                                               alength)
                                           (recur prev-char))))
                                     (- cursor-column-byte diff-bytes))

            new-tree (when-let [^TSTree tree tree]
                       (let [tree (.copy tree)]
                         (.edit tree (TSInputEdit. prev-byte cursor-byte prev-byte
                                                   (TSPoint. new-cursor-row new-cursor-column-byte)
                                                   (TSPoint. cursor-row cursor-column-byte)
                                                   (TSPoint. new-cursor-row new-cursor-column-byte)))
                         tree))

            new-rope (.concat (.slice rope 0 prev-point)
                              (.slice rope cursor-point (.size rope)))

            reader (util/->RopeReader new-rope)
            new-tree (when parser
                       (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 ))]
        (assoc editor
               :tree new-tree
               :cursor {:byte prev-byte
                        :char prev
                        :point prev-point
                        :row new-cursor-row
                        :column-byte new-cursor-column-byte}
               :paragraph nil
               :rope new-rope)))))

(defn editor-delete-char
  ([editor]
   (editor-delete-char editor 1))
  ([editor n]
   (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         {cursor-byte :byte
          cursor-char :char
          cursor-row :row
          cursor-point :point
          cursor-column-byte :column-byte} cursor
         
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

         [char-index old-end-row old-end-column-byte]
         (loop [char-index cursor-char
                row cursor-row
                last-line-char nil
                n n]
           (if (zero? n)
             [char-index row (if last-line-char
                               (-> (.subSequence rope last-line-char char-index)
                                   .toString
                                   .getBytes
                                   alength)
                               (+ cursor-column-byte
                                  (-> (.subSequence rope cursor-char char-index)
                                      .toString
                                      .getBytes
                                      alength)))]
             (let [next-char (.following bi char-index)]
               (cond
                 (= -1 next-char) [char-index row
                                   (if last-line-char
                                     (-> (.subSequence rope last-line-char char-index)
                                         .toString
                                         .getBytes
                                         alength)
                                     (+ cursor-column-byte
                                        (-> (.subSequence rope cursor-char char-index)
                                            .toString
                                            .getBytes
                                            alength)))]
                 
                 (= \newline (.charAt rope char-index)) (recur next-char (inc row) next-char (dec n))
                 :else (recur next-char row last-line-char (dec n))))))

         diff-string (-> (.subSequence rope cursor-char char-index)
                         .toString)
         diff-bytes (alength (.getBytes diff-string "utf-8"))

         end-byte (+ cursor-byte diff-bytes)
         new-tree (when-let [^TSTree tree tree]
                    (let [tree (.copy tree)]
                      (.edit tree (TSInputEdit. cursor-byte end-byte cursor-byte
                                                (TSPoint. cursor-row cursor-column-byte)
                                                (TSPoint. old-end-row old-end-column-byte)
                                                (TSPoint. cursor-row cursor-column-byte)))
                      tree))

         new-rope (.concat (.sliceBytes rope 0 cursor-byte)
                           (.sliceBytes rope end-byte (.numBytes rope)))
         
         reader (util/->RopeReader new-rope)
         new-tree (when parser
                    (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 ))]
     (assoc editor
            :tree new-tree
            :paragraph nil
            :rope new-rope))))



(defn editor-backward-char [editor]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence rope prev-char cursor-char)
                            .toString)
            diff-bytes (alength (.getBytes diff-string "utf-8"))
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (dec cursor-row)
                             cursor-row)

            new-cursor-char prev-char
            new-cursor-column-byte (if newline?
                                     (loop [char-index prev-char]
                                       (let [prev-char (.preceding bi char-index)]
                                         (if (or (= -1 prev-char)
                                                 (= \newline (.charAt rope prev-char)))
                                           (-> (.subSequence rope char-index new-cursor-char)
                                               .toString
                                               .getBytes
                                               alength)
                                           (recur prev-char))))
                                     (- cursor-column-byte
                                        diff-bytes))]
        (assoc editor
               :cursor {:byte (- cursor-byte diff-bytes)
                        :char new-cursor-char
                        :point (- cursor-point (util/num-points diff-string))
                        :row new-cursor-row
                        :column-byte new-cursor-column-byte})))))

(defn editor-forward-word [editor]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        bi (doto (BreakIterator/getWordInstance)
             (.setText rope))

        next-char (.following bi cursor-char)]
    (if (= -1 next-char)
      editor
      (let [diff-string (-> (.subSequence rope cursor-char next-char)
                            .toString)

            rc-offset (util/count-row-column-bytes diff-string)
            new-cursor-row (+ cursor-row (:row rc-offset))
            new-cursor-column-byte (if (pos? (:row rc-offset))
                                     (:column-byte rc-offset)
                                     (+ (:column-byte rc-offset) cursor-column-byte))]
        (assoc editor
               :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point (util/num-points diff-string))
                        :row new-cursor-row
                        :column-byte new-cursor-column-byte})))))
(defn editor-backward-word [editor]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        bi (doto (BreakIterator/getWordInstance)
             (.setText rope))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence rope prev-char cursor-char )
                            .toString)

            rc-offset (util/count-row-column-bytes diff-string)
            new-cursor-row (- cursor-row (:row rc-offset))
            new-cursor-char prev-char
            new-cursor-column-byte (if (pos? (:row rc-offset))
                                     (loop [char-index prev-char]
                                       (let [prev-char (.preceding bi char-index)]
                                         (if (or (= -1 prev-char)
                                                 (= \newline (.charAt rope prev-char)))
                                           (-> (.subSequence rope char-index new-cursor-char)
                                               .toString
                                               .getBytes
                                               alength)
                                           (recur prev-char))))
                                     (- cursor-column-byte
                                        (:column-byte rc-offset)))]
        (assoc editor
               :cursor {:byte (- cursor-byte (alength (.getBytes diff-string "utf-8")))
                        :char new-cursor-char
                        :point (- cursor-point (util/num-points diff-string))
                        :row new-cursor-row
                        :column-byte new-cursor-column-byte})))))




(defn editor-backward-paragraph [editor]
  (let [^Rope
        rope (:rope editor)
        bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

        cursor (:cursor editor)
        cursor-char (:char cursor)
        ;; loop backward past newline
        char-index
        (loop [char-index cursor-char]
                     (let [prev-char (.preceding bi char-index)]
                       (cond
                         (= -1 prev-char) char-index
                         (= \newline (.charAt rope prev-char)) prev-char
                         :else (recur prev-char))))

        ;; goto to next newline where the whole line
        ;; is whitespace
        [char-index column-byte] 
        (loop [char-index char-index
               all-whitespace? true
               last-end-line char-index]
          (let [prev-char (.preceding bi char-index)]
            (if (= -1 prev-char)
              [char-index 0]
              (let [c (.charAt rope prev-char)]
                (cond
                  (= \newline c) (if all-whitespace?
                                   [last-end-line (-> (.subSequence rope char-index last-end-line)
                                                      .toString
                                                      .getBytes
                                                      alength)]
                                   (recur prev-char true prev-char))
                  (= \space c) (recur prev-char all-whitespace? last-end-line)
                  :else (recur prev-char false last-end-line))))))
        
        diff-string (-> (.subSequence rope char-index cursor-char)
                        .toString)]
    (assoc editor
           :cursor {:byte (- (:byte cursor) (alength (.getBytes diff-string "utf-8")))
                    :char char-index
                    :point (- (:point cursor) (util/num-points diff-string))
                    :row (- (:row cursor)
                            (:row (util/count-row-column-bytes diff-string)))
                    :column-byte column-byte})))


(defn editor-forward-paragraph [editor]
  (let [^Rope
        rope (:rope editor)
        bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

        cursor (:cursor editor)
        cursor-char (:char cursor)
        ;; loop forward past newline
        char-index (loop [char-index cursor-char]
                     (let [next-char (.following bi char-index)]
                       (cond
                         (= -1 next-char) char-index
                         (= \newline (.charAt rope char-index)) next-char
                         :else (recur next-char))))

        ;; goto to next newline where the whole line
        ;; is whitespace
        char-index (loop [char-index char-index
                          all-whitespace? true]
                     (let [next-char (.following bi char-index)]
                       (if (= -1 next-char)
                         char-index
                         (let [c (.charAt rope char-index)]
                           (cond
                             (= \newline c) (if all-whitespace?
                                              char-index
                                              (recur next-char true))
                             (= \space c) (recur next-char all-whitespace?)
                             :else (recur next-char false))))))
        
        diff-string (-> (.subSequence rope cursor-char char-index)
                        .toString)
        
        rc-offset (util/count-row-column-bytes diff-string)
        new-cursor-column-byte (if (pos? (:row rc-offset))
                                 (:column-byte rc-offset)
                                 (+ (:column-byte rc-offset) (:column-byte cursor)))]
    (assoc editor
           :cursor {:byte (+ (:byte cursor) (alength (.getBytes diff-string "utf-8")))
                    :char char-index
                    :point (+ (:point cursor) (util/num-points diff-string))
                    :row (+ (:row cursor)
                            (:row (util/count-row-column-bytes diff-string)))
                    :column-byte new-cursor-column-byte})))

(defn editor-move-beginning-of-line [editor]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))

        char-index (loop [char-index cursor-char]
                     (let [prev-char (.preceding bi char-index)]
                       (if (or (= -1 prev-char)
                               (= \newline (.charAt rope prev-char)))
                         char-index
                         (recur prev-char))))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence rope char-index cursor-char)
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))
            new-cursor-column-byte (- cursor-column-byte num-bytes)]
        (assoc editor
               :cursor {:byte (- cursor-byte num-bytes)
                        :char (- cursor-char (.length diff-string))
                        :point (- cursor-point (util/num-points diff-string))
                        :row cursor-row
                        :column-byte new-cursor-column-byte}))))
  )
(defn editor-move-end-of-line [editor]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))

        char-index (loop [char-index cursor-char]
                     (let [next-char (.following bi char-index)]
                       (cond
                         (= -1 next-char) char-index
                         (= \newline (.charAt rope char-index)) char-index
                         :else (recur next-char))))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence rope cursor-char char-index )
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))
            new-cursor-column-byte (+ cursor-column-byte num-bytes)]
        (assoc editor
               :cursor {:byte (+ cursor-byte num-bytes)
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point (util/num-points diff-string))
                        :row cursor-row
                        :column-byte new-cursor-column-byte})))))

(defn editor-back-to-indentation [editor]
  (let [{:keys [cursor ^Rope rope] :as editor}
        (editor-move-beginning-of-line editor)

        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor

        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))

        char-index (loop [char-index cursor-char]
                     (let [next-char (.following bi char-index)]
                       (cond
                         (= -1 next-char) char-index
                         (#{\space \tab} (.charAt rope char-index)) (recur next-char)
                         :else char-index)))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence rope cursor-char char-index )
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))
            new-cursor-column-byte (+ cursor-column-byte num-bytes)]
        (assoc editor
               :cursor {:byte (+ cursor-byte num-bytes)
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point (util/num-points diff-string))
                        :row cursor-row
                        :column-byte new-cursor-column-byte})))))

(defn editor-previous-line
  ([editor]
   (editor-previous-line editor 1))
  ([editor n]
   (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-target-column-byte :target-column-byte
          cursor-column-byte :column-byte} cursor

         target-column-byte (or cursor-target-column-byte
                                cursor-column-byte)

         bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

         ;; find previous newline twice
         [lines char-index] (loop [char-index cursor-char
                                   lines 0]
                              (let [prev-char (.preceding bi char-index)]
                                (cond
                                  (= -1 prev-char) [lines char-index]
                                  (= \newline (.charAt rope prev-char)) (if (= n lines)
                                                                          [lines char-index]
                                                                          (recur prev-char (inc lines)))
                                  :else (recur prev-char lines))))

         ;; keep going until target column-byte
         ;; target byte doesn't even make sense
         ;; treat target byte as target grapheme cluster for now
         line-start-char char-index
         [char-index column-byte]
         (loop [char-index char-index
                n 0]
           (let [next-char (.following bi char-index)]
             (if (or (= -1 next-char)
                     (= \newline (.charAt rope char-index))
                     (= n target-column-byte)
                     ;; hack because target-column-byte
                     ;; doesn't quite make sense
                     (> next-char cursor-char))
               [char-index (-> (.subSequence rope line-start-char char-index)
                               .toString
                               .getBytes
                               alength)]
               (recur next-char
                      (inc n)))))]
     (if (= char-index cursor-char)
       editor
       (let [diff-string (-> (.subSequence rope char-index cursor-char)
                             .toString)
             num-bytes (alength (.getBytes diff-string "utf-8"))
             new-cursor-row (- cursor-row lines)]
         (assoc editor
                :cursor {:byte (- cursor-byte num-bytes)
                         :char (- cursor-char (.length diff-string))
                         :point (- cursor-point (util/num-points diff-string))
                         :row new-cursor-row
                         :target-column-byte target-column-byte
                         :column-byte column-byte}))))))


(defn editor-self-insert-command [editor ^String s]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor

        rc-offset (util/count-row-column-bytes s)
        new-cursor-row (+ cursor-row (:row rc-offset))
        new-cursor-column-byte (if (pos? (:row rc-offset))
                                 (:column-byte rc-offset)
                                 (+ (:column-byte rc-offset) cursor-column-byte))]
    (-> editor
        (assoc :cursor {:byte (+ cursor-byte (util/num-bytes s))
                        :char (+ cursor-char (.length s))
                        :point (+ cursor-point (util/num-points s))
                        :row new-cursor-row
                        :column-byte new-cursor-column-byte})
        (editor-insert s cursor-byte cursor-row cursor-column-byte))))

(defn editor-downcase-word [editor]
  (let [start-editor editor
        start-cursor (:cursor editor)
        editor (editor-forward-word editor)
        end-cursor (:cursor editor)
        
        start-byte (:byte start-cursor)
        end-byte (:byte end-cursor)
        _ (assert (>= end-byte start-byte))
        word (-> (.sliceBytes ^Rope (:rope start-editor) start-byte end-byte)
                 .toString)
        
        editor (-> start-editor
                   (editor-snip start-byte end-byte)
                   (editor-self-insert-command (str/lower-case word)))]
    editor))

(defn editor-upcase-word [editor]
  (let [start-editor editor
        start-cursor (:cursor editor)
        editor (editor-forward-word editor)
        end-cursor (:cursor editor)
        
        start-byte (:byte start-cursor)
        end-byte (:byte end-cursor)
        _ (assert (>= end-byte start-byte))
        word (-> (.sliceBytes ^Rope (:rope start-editor) start-byte end-byte)
                 .toString)
        
        editor (-> start-editor
                   (editor-snip start-byte end-byte)
                   (editor-self-insert-command (str/upper-case word)))]
    editor))

(defn editor-capitalize-word [editor]
  (let [start-editor editor
        start-cursor (:cursor editor)
        editor (editor-forward-word editor)
        end-cursor (:cursor editor)
        
        start-byte (:byte start-cursor)
        end-byte (:byte end-cursor)
        _ (assert (>= end-byte start-byte))
        word (-> (.sliceBytes ^Rope (:rope start-editor) start-byte end-byte)
                 .toString)
        
        editor (-> start-editor
                   (editor-snip start-byte end-byte)
                   (editor-self-insert-command (str/capitalize word)))]
    editor))

(defn editor-append-history [editor past]
  (update editor :history
          (fn [history]
            (let [log (or (:log history)
                          [])
                  log (conj log past)]
              (assoc history :log log)))))

(defn editor-undo*old [editor]
  ;; not sure if history should
  ;; jump back to old editor with new history
  ;; or update current editor with a subselect
  ;; of keys from the old editor like
  ;; :tree, :rope, :viewport, :cursor, etc.
  (if (seq (-> editor :history :log))
    (let [history (:history editor)
          log (:log history)
          index (max
                 0
                 (dec
                  (or (:index history)
                      (count log))))

          old-editor (nth log index)
          new-history (assoc history
                             :index index)]
      (assoc old-editor
             :history new-history))
    ;; else
    editor))

(defn editor-undo [editor]
  ;; not sure if history should
  ;; jump back to old editor with new history
  ;; or update current editor with a subselect
  ;; of keys from the old editor like
  ;; :tree, :rope, :viewport, :cursor, etc.
  ;; Update on above:
  ;; Most keys in the editor map should not be updated on
  ;;  undo. The viewport might have changed size since the last edit.
  ;; For now, try to only update content when undoing.
  (if (seq (-> editor :history :log))
    (let [history (:history editor)
          log (:log history)
          index (max
                 0
                 (dec
                  (or (:index history)
                      (count log))))

          old-editor (nth log index)
          new-history (assoc history
                             :index index)
          new-editor (assoc editor
                            :history new-history
                            :rope (:rope old-editor)
                            :cursor (:cursor old-editor))

          new-editor (if (= (-> old-editor :viewport :num-lines)
                            (-> new-editor :viewport :num-lines))
                       (assoc-in new-editor [:viewport :start-line]
                                 (-> old-editor :viewport :start-line))
                       (editor-update-viewport new-editor))

          new-editor (if (:tree new-editor)
                       (assoc new-editor :tree (:tree old-editor))
                       new-editor)]
      new-editor
      #_(assoc old-editor
               :history new-history))
    ;; else
    editor))

(defn editor-append-clipboard [editor rope]
  (update editor :clipboard
          (fn [clipboard]
            ;; todo: be smarter about this
            (let [clips (or (:clips clipboard)
                            [])
                  clips (conj clips rope)
                  clips (if (> (count clips) 5)
                          (subvec clips (- (count clips) 5) (count clips))
                          clips)]
              {:clips clips}))))

(defn editor-yank [editor]
  (let [^Rope rope (-> editor :clipboard :clips peek)]
    (if rope
      ;; todo: be smarter about this
      ;; converting from rope to string is unnecessary
      (editor-self-insert-command editor (.toString rope))
      editor)))



(defn editor-push-mark 
  ([editor]
   (editor-push-mark editor (:cursor editor)))
  ([editor cursor]
   (update editor :mark
           (fn [mark]
             (let [history (conj (or (:history mark) [])
                                 cursor)
                   history-count (count history)
                   
                   history (if (> history-count 5)
                             (subvec history
                                     (- history-count 5)
                                     (count history))
                             history)]
               (assoc mark :history history))))))

(defn editor-pop-mark [editor]
  (let [mark (:mark editor)
        history (:history mark)]
    (if (seq history)
      (let [{:keys [row column-byte]} (peek history)]
        (-> editor
            ;; goto-row-col takes a grapheme cluster index for col
            ;; cheat for now.
            (editor-goto-row-col row column-byte)
            (update-in [:mark :history] pop)
            (update-in [:mark :popmode?] (fn [pm]
                                           (if pm
                                             (inc pm)
                                             0)))))
      ;; else
      editor)))

(defn editor-set-mark [editor]
  (if (-> editor :mark :popmode?)
    (editor-pop-mark editor)
    ;; else
    (if (:select-cursor editor)
      (dissoc editor :select-cursor)
      (assoc editor :select-cursor (:cursor editor)))))

(defn editor-mark-whole-buffer [editor]
  (let [end-editor (editor-goto-byte editor (.numBytes ^Rope (:rope editor)))]
    (assoc editor
           :select-cursor (:cursor end-editor)
           :cursor {:byte 0
                    :char 0
                    :point 0
                    :row 0
                    :column-byte 0})))

(defn editor-kill-line [editor]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column-byte :column-byte} cursor
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))

        char-index (loop [char-index cursor-char
                          all-whitespace? true]
                     (let [next-char (.following bi char-index)]
                       (if (= -1 next-char)
                         char-index
                         (let [c (.charAt rope char-index)]
                           (case c
                             \newline (if all-whitespace?
                                        next-char
                                        char-index)
                             \space (recur next-char
                                           all-whitespace?)
                             ;; else
                             (recur next-char false))))))]
    (if (= char-index cursor-char)
      editor
      (let [diff-string (-> (.subSequence rope cursor-char char-index )
                            .toString)
            num-bytes (alength (.getBytes diff-string "utf-8"))]
        
        (-> editor
            (editor-append-clipboard (Rope/from diff-string))
            (editor-snip cursor-byte (+ cursor-byte num-bytes)))))))

(defn editor-kill-region [editor]
  (if-let [select-cursor (:select-cursor editor)]
    (let [cursor (:cursor editor)
          cursor-byte (:byte cursor)
          select-cursor-byte (:byte select-cursor)

          start-byte (min cursor-byte select-cursor-byte)
          end-byte (max cursor-byte select-cursor-byte)

          ^Rope
          rope (:rope editor)
          clip (.sliceBytes rope start-byte end-byte)

          editor (-> editor
                     (editor-goto-byte start-byte)
                     (editor-snip start-byte end-byte)
                     (editor-append-clipboard clip))]
      editor)
    ;; else
    editor))

(defn editor-kill-word [editor]
  (let [start-byte (-> editor :cursor :byte)
        end-byte (-> editor
                     (editor-forward-word)
                     :cursor
                     :byte)]
    (editor-snip editor start-byte end-byte)))

(defn editor-backward-kill-word [editor]
  (let [end-byte (-> editor :cursor :byte)
        editor (editor-backward-word editor)]
    (editor-snip editor
                 (-> editor :cursor :byte)
                 end-byte)))

(defn editor-save-region [editor]
  (if-let [select-cursor (:select-cursor editor)]
    (let [cursor (:cursor editor)
          cursor-byte (:byte cursor)
          select-cursor-byte (:byte select-cursor)
          start-byte (min cursor-byte select-cursor-byte)
          end-byte (max cursor-byte select-cursor-byte)
          ^Rope
          rope (:rope editor)
          clip (.sliceBytes rope start-byte end-byte)
          editor (-> editor
                     (editor-append-clipboard clip)
                     (dissoc :select-cursor))]
      editor)
    ;; else
    editor))

(defn editor-exchange-point-and-mark [editor]
  (if-let [select-cursor (:select-cursor editor)]
    (assoc editor
           :cursor select-cursor
           :select-cursor (:cursor editor))
    ;; else
    editor))

(defn editor-open-line [editor]
  (-> editor
      (editor-self-insert-command "\n")
      (assoc :cursor (:cursor editor))))

(defn editor-next-line
  ([editor]
   (editor-next-line editor 1))
  ([editor n]
   (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-target-column-byte :target-column-byte
          cursor-column-byte :column-byte} cursor

         target-column-byte (or cursor-target-column-byte
                                cursor-column-byte)

         bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

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
                                  (= \newline (.charAt rope char-index)) (let [lines (inc lines)]
                                                                           (if (= lines n)
                                                                             [lines next-char]
                                                                             (recur next-char lines next-char)))
                                  :else (recur next-char lines last-line-char))))

         ;; keep going until target column-byte
         ;; target byte doesn't even make sense
         ;; treat target byte as target grapheme cluster for now
         [char-index column-byte]
         (if (pos? lines)
           (let [line-start-char char-index]
             (loop [char-index char-index
                    n 0]

               (let [next-char (.following bi char-index)]
                 (if (or (= -1 next-char)
                         (= \newline (.charAt rope char-index))
                         (= n target-column-byte))
                   [char-index (-> (.subSequence rope line-start-char char-index)
                                   .toString
                                   .getBytes
                                   alength)]
                   (recur next-char
                          (inc n))))))
           [cursor-char cursor-column-byte])]
     (if (= char-index cursor-char)
       editor
       (let [diff-string (-> (.subSequence rope cursor-char char-index )
                             .toString)
             num-bytes (alength (.getBytes diff-string "utf-8"))
             new-cursor-column-byte (+ cursor-column-byte num-bytes)
             new-cursor-row (+ cursor-row lines)]
         (assoc editor
                :cursor {:byte (+ cursor-byte num-bytes)
                         :char (+ cursor-char (.length diff-string))
                         :point (+ cursor-point (util/num-points diff-string))
                         :row new-cursor-row
                         :target-column-byte target-column-byte
                         :column-byte column-byte}))))))

(defn editor-scroll-down [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-next-line editor to-scroll)))

(defn editor-scroll-up [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-previous-line editor to-scroll)))


(defn editor-goto-line [editor n]
  (-> editor
      (assoc :cursor {:byte 0
                      :char 0
                      :point 0
                      :row 0
                      :column-byte 0})
      (editor-next-line n)))

(defn editor-end-of-buffer [editor]
  (let [{:keys [cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        ;; could probably be more efficient
        {:keys [row column-byte]} (util/count-row-column-bytes (.toString rope))]
    
    (-> editor
        (editor-push-mark)
        (assoc :cursor {:byte (.numBytes rope)
                        :char (-> rope .toCharSequence .length)
                        :point (-> rope .size)
                        :row row
                        :column-byte column-byte}))))

(defn editor-beginning-of-buffer [editor]
  (-> editor
      (editor-push-mark)
      (assoc :cursor {:byte 0
                      :char 0
                      :point 0
                      :row 0
                      :column-byte 0})))

(defn editor-kill-ring-save [editor]
  editor)

(defn editor-isearch-forward 
  ([editor]
   (if (::search editor)
     (let [search-state (::search editor)
           query (:query search-state)]
       (if query
         (let [search-cursor (:cursor editor)
               search-index (:char search-cursor)
               regexp (Pattern/compile query
                                       (bit-or
                                        Pattern/CASE_INSENSITIVE
                                        Pattern/LITERAL))
               ^Rope
               rope (:rope editor)
               matcher (.matcher regexp rope)
               
               match (if (.find matcher search-index)
                       (.toMatchResult matcher)
                       (when (.find matcher 0)
                         (.toMatchResult matcher)))
               ;; now do it again
               match (when match
                       (if (.find matcher)
                         (.toMatchResult matcher)
                         (when (.find matcher 0)
                           (.toMatchResult matcher))))
               
               search-state (-> search-state
                                (assoc :query query
                                       :match match
                                       :direction :forward))
               
               editor (assoc editor ::search search-state)]
           (if match
             (let [ ;; calculate new cursor
                   s (-> (.subSequence rope 0 (.start match))
                         .toString)
                   
                   {:keys [row column-byte]} (util/count-row-column-bytes s)
                   
                   cursor {:byte (alength (.getBytes s "utf-8"))
                           :char (.length s)
                           :point (util/num-points s)
                           :row row
                           :column-byte column-byte}]
               (assoc editor :cursor cursor))
             ;; else, no match
             editor))
         (if-let [query (:search/last-search editor)]
           (editor-isearch-forward editor query)
           editor)))
     ;; No current query, try to repeat last search.
     ;; this may seem like it's not doing anything,
     ;; but this can signal to the UI that a search is happening
     (assoc editor ::search {:initial-cursor (:cursor editor)
                             :initial-rope (:rope editor)
                             :direction :forward})))
  ([editor query]
   (let [search-state (or (::search editor)
                          {:initial-cursor (:cursor editor)
                           :initial-rope (:rope editor)
                           :direction :forward})
         
         search-cursor (or (:cursor search-state)
                           (:initial-cursor search-state))
         search-index (:char search-cursor)
         regexp (Pattern/compile query
                                 (bit-or
                                  Pattern/CASE_INSENSITIVE
                                  Pattern/LITERAL))
         ^Rope
         rope (:rope editor)
         matcher (.matcher regexp rope)
         
         match (if (.find matcher search-index)
                 (.toMatchResult matcher)
                 (when (.find matcher 0)
                   (.toMatchResult matcher)))
         
         search-state (-> search-state
                          (assoc :query query
                                 :match match
                                 :direction :forward))
         
         editor (assoc editor ::search search-state)]
     (if match
       (let [
             ;; calculate new cursor
             s (-> (.subSequence rope 0 (.start match))
                   .toString)
             
             {:keys [row column-byte]} (util/count-row-column-bytes s)
             
             cursor {:byte (alength (.getBytes s "utf-8"))
                     :char (.length s)
                     :point (util/num-points s)
                     :row row
                     :column-byte column-byte}]
         (assoc editor :cursor cursor))
       ;; else, no match
       editor))))

(defn ^:private reverse-char-sequence 
  ^CharSequence
  [^CharSequence cs]
  (reify 
    CharSequence
    (length [_ ]
      (.length cs))
    (charAt [_ idx]
      (.charAt cs (dec (- (.length cs) idx))))
    (subSequence [_ start  end]
      (let [len (.length cs)]
        (reverse-char-sequence
         (.subSequence cs
                       (- len end)
                       (- len start)))))
    (toString [this]
      (.toString (StringBuilder. this)))))

(defn ^:private ->match-result 
  ^java.util.regex.MatchResult
   [start end group]
  (reify
    java.util.regex.MatchResult
    (start [_] start)
    (end [_] end)
    (group [_] group)))

(defn editor-isearch-backward
  ([editor]
   (if (::search editor)
     (let [search-state (::search editor)
           query (:query search-state)]
       (if query
         (let [search-cursor (:cursor editor)
                                 
               ^Rope
               rope (:rope editor)
               search-index (- (.length rope) (:char search-cursor))
               
               ;; There is no built in way to search backwards with regex (afaik)
               ;; Instead, search the reversed string and convert the indices.
               regexp (Pattern/compile (str/reverse query)
                                       (bit-or
                                        Pattern/CASE_INSENSITIVE
                                        Pattern/LITERAL))

               reversed-cs  (reverse-char-sequence rope)
               matcher (.matcher regexp reversed-cs)
               
               match (if (.find matcher search-index)
                       (.toMatchResult matcher)
                       (when (.find matcher 0)
                         (.toMatchResult matcher)))
               
               unreversed-match (when match
                                  (->match-result 
                                   (- (.length rope) (.end match))
                                   (- (.length rope) (.start match))
                                   (str (.subSequence reversed-cs (.start match) (.end match)))))

               search-state (-> search-state
                                (assoc :query query
                                       :match unreversed-match
                                       :direction :backward))
               
               editor (assoc editor ::search search-state)]
           (if match
             (let [;; calculate new cursor
                   ;; we're calculating to `.end` instead of `.start`
                   ;; because we searched in reverse.
                   s (-> (.subSequence rope 0 (- (.length rope) (.end match)))
                         .toString)
                   
                   {:keys [row column-byte]} (util/count-row-column-bytes s)
                   
                   cursor {:byte (alength (.getBytes s "utf-8"))
                           :char (.length s)
                           :point (util/num-points s)
                           :row row
                           :column-byte column-byte}]
               (assoc editor :cursor cursor))
             ;; else, no match
             editor))
         (if-let [query (:search/last-search editor)]
           (editor-isearch-backward editor query)
           editor)))
     ;; No current query, try to repeat last search.
     ;; this may seem like it's not doing anything,
     ;; but this can signal to the UI that a search is happening
     (assoc editor ::search {:initial-cursor (:cursor editor)
                             :initial-rope (:rope editor)
                             :direction :backward})))
  ([editor query]
   (let [search-state (or (::search editor)
                          {:initial-cursor (:cursor editor)
                           :initial-rope (:rope editor)
                           :direction :backward})
         
         search-cursor (or (:cursor search-state)
                           (:initial-cursor search-state))
         ^Rope
         rope (:rope editor)
         search-index (- (.length rope) (:char search-cursor))
         
         ;; There is no built in way to search backwards with regex (afaik)
         ;; Instead, search the reversed string and convert the indices.
         regexp (Pattern/compile (str/reverse query)
                                 (bit-or
                                  Pattern/CASE_INSENSITIVE
                                  Pattern/LITERAL))

         reversed-cs  (reverse-char-sequence rope)
         matcher (.matcher regexp reversed-cs)
         
         match (if (.find matcher search-index)
                 (.toMatchResult matcher)
                 (when (.find matcher 0)
                   (.toMatchResult matcher)))

         unreversed-match (when match
                            (->match-result 
                             (- (.length rope) (.end match))
                             (- (.length rope) (.start match))
                             (str (.subSequence reversed-cs (.start match) (.end match)))))
         
         search-state (-> search-state
                          (assoc :query query
                                 :match unreversed-match
                                 :direction :backward))
         
         editor (assoc editor ::search search-state)]
     (if match
       (let [;; calculate new cursor
             ;; we're calculating to `.end` instead of `.start`
             ;; because we searched in reverse.
             s (-> (.subSequence rope 0 (- (.length rope) (.end match)))
                   .toString)
             
             {:keys [row column-byte]} (util/count-row-column-bytes s)
             
             cursor {:byte (alength (.getBytes s "utf-8"))
                     :char (.length s)
                     :point (util/num-points s)
                     :row row
                     :column-byte column-byte}]
         (assoc editor :cursor cursor))
       ;; else, no match
       editor))))

(defn editor-cancel-search [editor]
  (let [initial-cursor (-> editor ::search :initial-cursor)
        editor (if initial-cursor
                 (assoc editor :cursor initial-cursor)
                 editor)]
    (dissoc editor ::search)))

(defn editor-finish-search-forward [editor]
  (let [last-search (-> editor ::search :query)
        editor (-> editor
                   (editor-push-mark (-> editor ::search :initial-cursor))
                   (dissoc ::search))
        editor (if (seq last-search)
                 (assoc editor :search/last-search last-search)
                 editor)]
    editor))

(defn editor-single-space [editor]
  (let [^Rope rope (:rope editor)
        
        cursor-char (-> editor :cursor :char)
        cursor-byte  (-> editor :cursor :byte) 

        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))

        ;; counting spaces and spaces are single bytes
        diff-bytes
        (loop [n 0
               char-index cursor-char]
          (let [prev-char (.preceding bi char-index)]
            (cond
              (= -1 prev-char) n
              (= \space (.charAt rope prev-char)) (recur (inc n) prev-char)
              :else n)))

        char-index (- cursor-char diff-bytes)
        byte-index (- cursor-byte diff-bytes)
        
        editor (editor-goto-byte editor byte-index)
        rope-length (.length rope)]
    (if (or (>= char-index rope-length)
            (not= \space (.charAt rope char-index)))
      (editor-self-insert-command editor " ")
      ;; else ensure exactly one space
      (let [start-snip-byte (inc byte-index)
            

            ;; this works because spaces are one byte
            diff-bytes 
            (loop [n 0
                   char-index (inc char-index)]
              (if (>= char-index rope-length)
                n
                (if (= \space (.charAt rope char-index))
                  (recur (inc n) (inc char-index))
                  n)))]
        (if (pos? diff-bytes)
          (-> editor
              (editor-snip start-snip-byte (+ start-snip-byte diff-bytes))
              (editor-forward-char))
          (editor-forward-char editor))))))



(defn editor-delete-horizontal-space [editor]
  (let [cursor-char (-> editor :cursor :char)
        
        ^Rope rope (:rope editor)
        rope-length (.length rope)
        ;; counting spaces forward, spaces are one byte each
        forward-diff-bytes
        (loop [n 0
               char-index cursor-char]
          (if (>= char-index rope-length)
            n
            (if (= \space (.charAt rope char-index))
              (recur (inc n) (inc char-index))
              n)))
        
        ;; count spaces backwards
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))
        backward-diff-bytes
        (loop [n 0
               char-index cursor-char]
          (let [prev-char (.preceding bi char-index)]
            (cond
              (= -1 prev-char) n
              (= \space (.charAt rope prev-char)) (recur (inc n) prev-char)
              :else n)))

        new-cursor-byte (- (-> editor :cursor :byte)
                           backward-diff-bytes)
        editor (if (pos? backward-diff-bytes)
                 (editor-goto-byte editor new-cursor-byte)
                 editor)

        editor (if (or (pos? backward-diff-bytes)
                       (pos? forward-diff-bytes))
                 (editor-snip editor
                              new-cursor-byte
                              (+ new-cursor-byte backward-diff-bytes forward-diff-bytes))
                 editor)]
    editor))

(defn editor-newline [editor]
  (editor-self-insert-command editor "\n"))

(defn editor-indent [editor]
  (editor-self-insert-command editor "    "))


(def key-bindings
  {"C-a" editor-move-beginning-of-line
   "C-d" editor-delete-char
   "C-e" editor-move-end-of-line
   "C-f" editor-forward-char
   "C-b" editor-backward-char
   "C-j" editor-newline
   "RET" editor-newline
   "C-k" editor-kill-line
   "C-l" editor-recenter-top-bottom
   "C-n" editor-next-line
   "C-o" editor-open-line
   "C-p" editor-previous-line
   "C-v" editor-scroll-down
   "C-x h" editor-mark-whole-buffer
   "C-x x" editor-exchange-point-and-mark
   "M-v" editor-scroll-up
   "M-<" editor-beginning-of-buffer
   "M->" editor-end-of-buffer
   "M-l" editor-downcase-word
   "M-u" editor-upcase-word   
   "M-c" editor-capitalize-word
   "M-{" editor-backward-paragraph
   "M-}" editor-forward-paragraph
   "M-b" editor-backward-word
   "M-f" editor-forward-word
   "M-d" editor-kill-word
   "M-DEL" editor-backward-kill-word
   "DEL" editor-delete-backward-char
   "<right>" editor-forward-char
   "<up>" editor-previous-line
   "<down>" editor-next-line
   "<left>" editor-backward-char
   "C-y" editor-yank
   "C-SPC" editor-set-mark
   "C-w" editor-kill-region
   "M-w" editor-save-region
   "M-SPC" editor-single-space
   "M-\\" editor-delete-horizontal-space
   "C-_" editor-undo
   "C-s" editor-isearch-forward
   "C-r" editor-isearch-backward
   "TAB" editor-indent
   ,})


(defn make-editor
  ([]
   {:cursor {:byte 0
             :char 0
             :point 0
             :row 0
             :column-byte 0}
    :rope Rope/EMPTY}))
