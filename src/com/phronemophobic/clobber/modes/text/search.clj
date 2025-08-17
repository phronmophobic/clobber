(ns com.phronemophobic.clobber.modes.text.search
  (:require [clojure.string :as str]
            [com.phronemophobic.clobber.util :as util]
            [com.phronemophobic.clobber.modes.text :as text-mode])
  (:import java.util.regex.Pattern
           io.lacuna.bifurcan.Rope))

(defrecord ReverseCharSequence [^CharSequence cs]
  CharSequence
  (length [_ ]
    (.length cs))
  (charAt [_ idx]
    (.charAt cs (dec (- (.length cs) idx))))
  (subSequence [_ start  end]
    (let [len (.length cs)]
      (ReverseCharSequence. 
       (.subSequence cs
                     (- len end)
                     (- len start)))))
  (toString [this]
    (.toString (StringBuilder. this))))

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



(defn editor-search-backward [editor query]
  (let [search-state (or (::search editor)
                         {:initial-cursor (:cursor editor)
                          :initial-rope (:rope editor)})
        
        search-cursor (or (:cursor search-state)
                          (:initial-cursor search-state))
        search-index (:char search-cursor)
        
        ;; There is no built in way to search backwards with regex (afaik)
        ;; Instead, search the reversed string and convert the indices.
        regexp (Pattern/compile (str/reverse query)
                                (bit-or
                                 Pattern/CASE_INSENSITIVE
                                 Pattern/LITERAL))
        ^Rope
        rope (:rope editor)
        reversed-cs  (ReverseCharSequence. rope)
        matcher (.matcher regexp reversed-cs)

        match (if (.find matcher search-index)
                (.toMatchResult matcher)
                (when (.find matcher 0)
                  (.toMatchResult matcher)))
        
        search-state (-> search-state
                         (assoc :query query)
                         (assoc :match match))
        
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
      editor)))



(comment
  
  (def my-editor
    (-> (text-mode/make-editor)
        (text-mode/editor-self-insert-command "a b c d a b c d")))

  (-> my-editor
      (editor-search-backward "b c")
      (editor-search-forward " b")
      )
  
  ,)

