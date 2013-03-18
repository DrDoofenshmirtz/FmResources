(ns
  ^{:doc "Extensions for resource slots."
    :author "Frank Mosebach"}
  fm.resources.slot-extensions
  (:require
    [fm.resources.store :as sto]))

(defn- resource-expired-predicate [scope scopes]
  (if-let [scopes (seq (drop-while (partial not= scope) scopes))]
    (set scopes)
    (throw (IllegalArgumentException.
             (format "Scope %s is not in %s!" scope scopes)))))

(defn- scope-expired-slot [scope scopes]
  (let [resource-expired? (resource-expired-predicate scope scopes)]
    (fn [resource expired-scope]
      (if-not (resource-expired? expired-scope)
        resource))))

(defn with-scope [slots scope scopes]
  (assoc slots ::scope-expired (scope-expired-slot scope scopes)))

(defn scope-expired! [store scope]
  (sto/send! store ::scope-expired scope))
