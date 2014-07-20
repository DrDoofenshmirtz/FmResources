;; Leiningen project file for the FmResources clojure project.

(defproject fm/fm-resources "1.1.0-SNAPSHOT"
  :description "FmResources: Resource (Lifecycle) Management."
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [fm/fm-core "1.0.0-SNAPSHOT"]]
  :aot [fm.resources.types]    
  :jar-name "fm-resources.jar"
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/" 
                   #"(?:^|/).git/" 
                   #"(?:^|/)project.clj"])

