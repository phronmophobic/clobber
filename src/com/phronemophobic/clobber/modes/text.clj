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
                point-offset (util/count-points diff-string)
                new-char (+ (:char cursor)
                            (.length diff-string))
                new-point (+ (:point cursor)
                             (util/num-points diff-string))
                new-cursor-row (+ (:row cursor) (:row point-offset))
                new-cursor-column (if (pos? (:row point-offset))
                                    (:column point-offset)
                                    (+ (:column point-offset) (:column cursor)))]
            {:byte byte-index
             :char new-char
             :point new-point
             :row new-cursor-row
             :column new-cursor-column})

          ;; byte-index before cursor
          :else
          (let [diff-rope (.sliceBytes rope byte-index cursor-byte)
                diff-string (.toString diff-rope)
                new-char (- (:char cursor)
                            (.length diff-string))
                new-point (- (:point cursor)
                             (util/num-points diff-string))

                point-offset (util/count-points diff-string)
                new-cursor-row (- (:row cursor)
                              (:row point-offset))

                bi (doto (BreakIterator/getCharacterInstance)
                     (.setText rope))
                new-cursor-column
                (loop [char-index new-char
                       column 0]
                  (let [prev-char-index (.preceding bi char-index)]
                    (cond
                      (= -1 prev-char-index) column
                      (= \newline (.charAt rope prev-char-index)) column
                      :else (recur prev-char-index (inc column)))))]
            {:byte byte-index
             :char new-char
             :point new-point
             :row new-cursor-row
             :column new-cursor-column}))]
    (assoc editor
           :cursor cursor)))

(defn editor-insert
  "Inserts `s`.

  Inserts at current cursor if `byte-index` is not provided.

  If `row` and `column` are not provided. Calculate the row and column automatically"
  ([editor s]
   (let [cursor (:cursor editor)]
     (editor-insert editor s
                    (:byte cursor)
                    (:row cursor)
                    (:column cursor))))
  ([editor ^String s byte-index]
   (let [cursor (:cursor editor)
         ^Rope
         rope (:rope editor)
         {cursor-byte :byte} cursor

         {:keys [row column]} (if (= cursor-byte byte-index)
                                {:row (:row cursor)
                                 :column (:column cursor)}
                                (let [diff-rope (if (> byte-index cursor-byte)
                                                  (.sliceBytes rope cursor-byte byte-index)
                                                  (.sliceBytes rope byte-index cursor-byte))
                                      diff-string (.toString diff-rope)

                                      point-offset (util/count-points diff-string)

                                      new-cursor-row (+ (:row cursor) (:row point-offset))
                                      new-cursor-column (if (pos? (:row point-offset))
                                                          (:column point-offset)
                                                          (+ (:column point-offset) (:column cursor)))]
                                  {:row new-cursor-row
                                   :column new-cursor-column}))]
     (editor-insert editor s byte-index row column)))
  ([editor ^String s byte-index row column]
   (let [sbytes (.getBytes s "utf-8")
         point-offset (util/count-points s)
         delta-rows (:row point-offset)
         
         new-row (+ row delta-rows)
         new-column (if (pos? delta-rows)
                      (:column point-offset)
                      (+ (:column point-offset) column))

         tree (:tree editor)
         new-tree (when-let [^TSTree
                             tree tree]
                    (let [tree (.copy tree)]
                      (.edit tree (TSInputEdit. byte-index byte-index (+ byte-index (alength sbytes))
                                                (TSPoint. row column)
                                                (TSPoint. row column)
                                                (TSPoint. new-row new-column)))
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

         {:keys [row column]} (if (= cursor-byte start-byte)
                                {:row (:row cursor)
                                 :column (:column cursor)}
                                (let [diff-rope (if (> start-byte cursor-byte)
                                                  (.sliceBytes rope cursor-byte start-byte)
                                                  (.sliceBytes rope start-byte cursor-byte))

                                      point-offset (util/count-points diff-rope)

                                      new-cursor-row (+ (:row cursor) (:row point-offset))
                                      new-cursor-column (if (pos? (:row point-offset))
                                                          (:column point-offset)
                                                          (+ (:column point-offset) (:column cursor)))]
                                  {:row new-cursor-row
                                   :column new-cursor-column}))

         diff-rope (.sliceBytes rope start-byte end-byte)
         point-offset (util/count-points diff-rope)
         delta-rows (:row point-offset)
         
         end-row (+ row delta-rows)
         end-column (if (pos? delta-rows)
                      (:column point-offset)
                      (+ (:column point-offset) column))

         ^TSTree
         tree (:tree editor)
         new-tree (when-let [tree tree]
                    (let [tree (.copy tree)]
                      (.edit tree (TSInputEdit. start-byte end-byte start-byte
                                                (TSPoint. row column)
                                                (TSPoint. end-row end-column)
                                                (TSPoint. row column)))
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

(defn editor-set-string [editor s]
  (let [rope (Rope/from s)
        ^TSParser
         parser (:parser editor)
         tree (when parser
                    (let [reader (util/->RopeReader rope)]
                      (.parse parser (:buf editor) nil reader TSInputEncoding/TSInputEncodingUTF8)))]
    (assoc editor
      :rope rope
      :tree tree)))

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

(defn editor-forward-char
  ([editor]
   (editor-forward-char editor 1))
  ([editor n]
   (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
         
         {cursor-byte :byte
          cursor-char :char
          cursor-point :point
          cursor-row :row
          cursor-column :column} cursor
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

         rope-length (.length rope)
         [end-char new-cursor-row new-cursor-column]
         (loop [n n
                char-index cursor-char
                new-cursor-row cursor-row
                new-cursor-column cursor-column]
           (cond
             (or (>= char-index rope-length)
                 (<= n 0))
             [char-index new-cursor-row new-cursor-column]

             (= \newline (.charAt rope char-index))
             (recur (inc n)
                    (.following bi char-index)
                    (inc new-cursor-row)
                    0)

             :else
             (recur (dec n)
                    (.following bi char-index)
                    new-cursor-row
                    (inc new-cursor-column))))

         diff-string (-> (.subSequence rope cursor-char end-char)
                         .toString)]
     (assoc editor
            :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                     :char (+ cursor-char (.length diff-string))
                     :point (+ cursor-point (util/num-points diff-string))
                     :row new-cursor-row
                     :column new-cursor-column}))))

(defn editor-delete-backward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor]
    (if (<= (:byte cursor) 0)
      editor
      (let [{cursor-byte :byte
             cursor-char :char
             cursor-row :row
             cursor-point :point
             cursor-column :column} cursor
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
            new-cursor-column (if newline?
                                (loop [n 0]
                                  (let [start (.previous bi)]
                                    (if (or (= BreakIterator/DONE start)
                                            (= \newline (.charAt rope start)))
                                      n
                                      (recur (inc n)))))
                                (dec cursor-column))

            new-tree (when-let [^TSTree tree tree]
                       (let [tree (.copy tree)]
                         (.edit tree (TSInputEdit. cursor-byte cursor-byte prev-byte
                                                   (TSPoint. cursor-row cursor-column)
                                                   (TSPoint. cursor-row cursor-column)
                                                   (TSPoint. new-cursor-row new-cursor-column)))
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
                        :column new-cursor-column}
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
          cursor-column :column} cursor
         
         bi (doto (BreakIterator/getCharacterInstance)
              (.setText rope))

         [char-index old-end-row old-end-column]
         (loop [char-index cursor-char
                row cursor-row
                column cursor-column
                n n]
           (if (zero? n)
             [char-index row column]
             (cond
               (= -1 next) char-index
               (= \newline (.charAt rope char-index)) (recur (.following bi char-index) (inc row) 0 (dec n))
               :else (recur (.following bi char-index) row (inc column) (dec n)))))

         diff-string (-> (.subSequence rope cursor-char char-index)
                         .toString)
         diff-bytes (alength (.getBytes diff-string "utf-8"))

         end-byte (+ cursor-byte diff-bytes)
         new-tree (when-let [^TSTree tree tree]
                    (let [tree (.copy tree)]
                      (.edit tree (TSInputEdit. cursor-byte end-byte cursor-byte
                                                (TSPoint. cursor-row cursor-column)
                                                (TSPoint. old-end-row old-end-column)
                                                (TSPoint. cursor-row cursor-column)))
                      tree))

         new-rope (.concat (.sliceBytes rope 0 cursor-byte)
                           (.slice rope end-byte (.size rope)))
         
         reader (util/->RopeReader new-rope)
         new-tree (when parser
                    (.parse parser buf new-tree reader TSInputEncoding/TSInputEncodingUTF8 ))]
     (assoc editor
            :tree new-tree
            :paragraph nil
            :rope new-rope))))



(defn editor-backward-char [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        bi (doto (BreakIterator/getCharacterInstance)
             (.setText rope))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence rope prev-char cursor-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (dec cursor-row)
                             cursor-row)

            new-cursor-column (if newline?
                                (loop [column 0
                                       index (.previous bi)]
                                  (if (>= index 0)
                                    (let [c (.charAt rope index)]
                                      (if (= \newline c)
                                        column
                                        (recur (inc column)
                                               (.previous bi))))
                                    column))
                                (dec cursor-column))]
        (assoc editor
               :cursor {:byte (- cursor-byte (alength (.getBytes diff-string "utf-8")))
                        :char (- cursor-char (.length diff-string))
                        :point (- cursor-point (util/num-points diff-string))
                        :row new-cursor-row
                        :column new-cursor-column})))))

(defn editor-forward-word [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        bi (doto (BreakIterator/getWordInstance)
             (.setText rope))

        next-char (.following bi cursor-char)]
    (if (= -1 next-char)
      editor
      (let [diff-string (-> (.subSequence rope cursor-char next-char)
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (assoc editor
               :cursor {:byte (+ cursor-byte (alength (.getBytes diff-string "utf-8")))
                        :char (+ cursor-char (.length diff-string))
                        :point (+ cursor-point (util/num-points diff-string))
                        :row new-cursor-row
                        :column new-cursor-column})))))
(defn editor-backward-word [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
        bi (doto (BreakIterator/getWordInstance)
             (.setText rope))

        prev-char (.preceding bi cursor-char)]
    (if (= -1 prev-char)
      editor
      (let [diff-string (-> (.subSequence rope prev-char cursor-char )
                            .toString)
            newline? (= "\n" diff-string)
            new-cursor-row (if newline?
                             (inc cursor-row)
                             cursor-row)
            new-cursor-column (if newline?
                                0
                                (inc cursor-column))]
        (assoc editor
               :cursor {:byte (- cursor-byte (alength (.getBytes diff-string "utf-8")))
                        :char (- cursor-char (.length diff-string))
                        :point (- cursor-point (util/num-points diff-string))
                        :row new-cursor-row
                        :column new-cursor-column})))))
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
            new-cursor-column (- cursor-column num-bytes)]
        (assoc editor
               :cursor {:byte (- cursor-byte num-bytes)
                        :char (- cursor-char (.length diff-string))
                        :point (- cursor-point (util/num-points diff-string))
                        :row cursor-row
                        :column new-cursor-column}))))
  )
(defn editor-move-end-of-line [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor
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

         ;; keep going until target column
         [char-index column]
         (loop [char-index char-index
                column 0]
           (let [next-char (.following bi char-index)]
             (cond
               (= -1 next-char) [char-index column]
               (= \newline (.charAt rope char-index)) [char-index column]
               (= column target-column) [char-index column]
               :else (recur next-char (inc column)))))]
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
                         :target-column target-column
                         :column column}))))))


(defn editor-self-insert-command [editor ^String s]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        {cursor-byte :byte
         cursor-char :char
         cursor-point :point
         cursor-row :row
         cursor-column :column} cursor

        point-offset (util/count-points s)
        new-cursor-row (+ cursor-row (:row point-offset))
        new-cursor-column (if (pos? (:row point-offset))
                            (:column point-offset)
                            (+ (:column point-offset) cursor-column))]
    (-> editor
        (assoc :cursor {:byte (+ cursor-byte (util/num-bytes s))
                        :char (+ cursor-char (.length s))
                        :point (+ cursor-point (util/num-points s))
                        :row new-cursor-row
                        :column new-cursor-column})
        (editor-insert s cursor-byte cursor-row cursor-column))))

(defn editor-append-history [editor past]
  (update editor :history
    (fn [history]
      (let [log (or (:log history)
                    [])
            log (conj log past)]
        (assoc history :log log)))))

(defn editor-undo [editor]
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
  (let [rope (-> editor :clipboard :clips peek)]
    (if rope
      ;; todo: be smarter about this
      ;; converting from rope to string is unnecessary
      (editor-self-insert-command editor (.toString rope))
      editor)))

(defn editor-set-mark [editor]
  (if (:select-cursor editor)
    (dissoc editor :select-cursor)
    (assoc editor :select-cursor (:cursor editor))))

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
  editor)

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

         ;; keep going until target column
         [char-index column]
         (if (= cursor-char char-index)
           [char-index cursor-column]
           (loop [char-index char-index
                  column 0]
             (let [next-char (.following bi char-index)]
               (cond

                 (= -1 next-char) [char-index column]
                 (= \newline (.charAt rope char-index)) [char-index column]
                 (= column target-column) [char-index column]
                 :else (recur next-char (inc column))))))]
     (if (= char-index cursor-char)
       editor
       (let [diff-string (-> (.subSequence rope cursor-char char-index )
                             .toString)
             num-bytes (alength (.getBytes diff-string "utf-8"))
             new-cursor-column (+ cursor-column num-bytes)
             new-cursor-row (+ cursor-row lines)]
         (assoc editor
                :cursor {:byte (+ cursor-byte num-bytes)
                         :char (+ cursor-char (.length diff-string))
                         :point (+ cursor-point (util/num-points diff-string))
                         :row new-cursor-row
                         :target-column target-column
                         :column column}))))))

(defn editor-scroll-down [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-next-line editor to-scroll)))

(defn editor-scroll-up [editor]
  (let [to-scroll (quot (-> editor :viewport :num-lines)
                        2)]
    (editor-previous-line editor to-scroll)))


(defn editor-goto-line [editor n]
  editor)

(defn editor-end-of-buffer [editor]
  (let [{:keys [tree cursor paragraph ^Rope rope buf ^TSParser parser]} editor
        ;; could probably be more efficient
        {:keys [row column]} (util/count-points (.toString rope))]
    (assoc editor
           :cursor {:byte (.numBytes rope)
                    :char (-> rope .toCharSequence .length)
                    :point (-> rope .size)
                    :row row
                    :column column})))

(defn editor-beginning-of-buffer [editor]
  (assoc editor
         :cursor {:byte 0
                  :char 0
                  :point 0
                  :row 0
                  :column 0}))
(defn editor-isearch-forward [editor]
  editor)
(defn editor-kill-ring-save [editor]
  editor)

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

