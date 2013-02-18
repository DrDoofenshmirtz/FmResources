;; Leiningen project file for the FmResources clojure project.
;;
;; Additional dependencies (must be located the "lib" folder):
;; - fm-core.jar

(defproject fm/resources "1.0.0"
  :description "FmResources: Resource (Lifecycle) Management."
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :aot [fm.resources.types]
  ; don't sweep the "lib" folder when fetching the deps, because
  ; this would delete the additional deps.  
  :disable-deps-clean true    
  :jar-name "fm-resources.jar"
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/" 
                   #"(?:^|/).git/" 
                   #"(?:^|/)project.clj"])

