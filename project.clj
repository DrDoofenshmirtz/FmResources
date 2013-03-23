;; Leiningen project file for the FmResources clojure project.
;;
;; Additional dependencies:
;;   fm-core.jar

(def local-mvn-repo-path (-> (str (System/getProperty "user.home")
                                  "/maven-repository")
                             java.io.File.
                             .toURI
                             str))

(defproject fm/resources "1.0.0"
  :description "FmResources: Resource (Lifecycle) Management."
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [fm/fm-core "1.0.0"]]
  :repositories {"mvn-local" ~local-mvn-repo-path}
  :aot [fm.resources.types]    
  :jar-name "fm-resources.jar"
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/" 
                   #"(?:^|/).git/" 
                   #"(?:^|/)project.clj"])

