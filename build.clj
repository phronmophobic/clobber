(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def lib 'com.phronemophobic/clobber)
(def version "0.1-SNAPSHOT")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile [_]
  (b/javac {:src-dirs ["src-java"]
            :class-dir class-dir
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))


(defn jar [opts]
  (compile opts)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy [opts]
  (jar opts)
  (try ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
        (merge {:installer :remote
                :artifact jar-file
                :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
               opts))
       (catch Exception e
         (if-not (str/includes? (ex-message e) "redeploying non-snapshots is not allowed")
           (throw e)
           (println "This release was already deployed."))))
  opts)


(def tree-sitter-libs
  {"tree-sitter-cpp" {:git/sha "5cb9b693cfd7bfacab1d9ff4acac1a4150700609"}
   "tree-sitter-json" {:git/sha "46aa487b3ade14b7b05ef92507fdaa3915a662a3"}
   "tree-sitter-javascript" {:git/sha "6fbef40512dcd9f0a61ce03a4c9ae7597b36ab5c"}
   "tree-sitter-bash" {:git/sha "56b54c61fb48bce0c63e3dfa2240b5d274384763"}
   "tree-sitter-html" {:git/sha "cbb91a0ff3621245e890d1c50cc811bffb77a26b"}
   "tree-sitter-java" {:git/sha "a7db5227ec40fcfe94489559d8c9bc7c8181e25a"}
   "tree-sitter-python" {:git/sha "710796b8b877a970297106e5bbc8e2afa47f86ec"}
   "tree-sitter-css" {:git/sha "6e327db434fec0ee90f006697782e43ec855adf5"}})



(def queries-build-dir (io/file "build" "queries"))
(def queries-class-dir (io/file queries-build-dir "classes"))
(def queries-jar-file (io/file queries-build-dir "queries.jar"))
(def queries-coord 'com.phronemophobic.clobber/tree-sitter-queries)

(defn clean-queries [_]
  (b/delete {:path "build/queries"}))

(defn jar-queries [_]
  (let [
        _ (.mkdirs queries-class-dir)
        
        basis (b/create-basis {:project
                               {:deps {}}})
        licenses (into [:licenses]
                       (map (fn [[repo-name {:keys [git/sha]}]]
                              [:license
                               [:name "The MIT License (MIT)"]
                               [:url (str "https://github.com/tree-sitter/" repo-name "/blob/" sha "/LICENSE")]]))
                       tree-sitter-libs)
        pom-data [licenses]]
    (b/write-pom {:class-dir (.getCanonicalPath queries-class-dir)
                  :pom-data pom-data
                  :lib queries-coord
                  :version "1.1"
                  :basis basis})
    (doseq [[repo-name {:keys [git/sha]}] tree-sitter-libs]
      (let [queries-url (io/as-url (str "https://raw.githubusercontent.com/tree-sitter/"repo-name "/" sha "/queries/highlights.scm"))
            target-dir (doto (io/file queries-class-dir "com" "phronemophobic" "clobber" "queries" repo-name)
                         (.mkdirs))
            target-file (io/file target-dir "highlights.scm")]
        (with-open [is (io/input-stream  queries-url)
                    os (io/output-stream target-file)]
          (io/copy is os))))

    (b/jar {:class-dir (.getCanonicalPath queries-class-dir)
            :jar-file (.getCanonicalPath queries-jar-file)})))


(defn deploy-queries [opts]
  (jar-queries opts)
  (try ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
        {:installer :remote
         :artifact queries-jar-file
         :pom-file (b/pom-path {:lib queries-coord :class-dir (.getCanonicalPath queries-class-dir)})})
       (catch Exception e
         (if-not (str/includes? (ex-message e) "redeploying non-snapshots is not allowed")
           (throw e)
           (println "This release was already deployed.")))))
