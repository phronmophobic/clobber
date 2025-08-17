(ns com.phronemophobic.clobber.modes.text.search
  (:require [com.phronemophobic.clobber.util :as util]
            [com.phronemophobic.clobber.modes.text :as text-mode])
  (:import java.util.regex.Pattern
           io.lacuna.bifurcan.Rope))

(defn editor-search-forward [editor query]
  (let [search-state (or (::search editor)
                         {:initial-cursor (:cursor editor)
                          :initial-rope (:rope editor)})
        
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
                         (assoc :query query)
                         (assoc :match match))
        
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
      editor)))

(defn editor-repeat-search-forward [editor]
  (let [search-state (::search editor)
        _ (assert search-state)

        query (:query search-state)
        
        search-cursor (:cursor editor)
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
                         (assoc :query query)
                         (assoc :match match))
        
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
      editor)))


(comment
  
  (def my-editor
    (-> (text-mode/make-editor)
        (text-mode/editor-self-insert-command "hello there.")))

  (-> my-editor
      (editor-search-forward "l")
      (editor-repeat-search-forward)
      (editor-repeat-search-forward)
      (editor-repeat-search-forward))
  
  ,)