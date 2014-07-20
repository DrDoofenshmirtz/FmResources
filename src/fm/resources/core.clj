(ns
  ^{:doc

  "Core operations for resource (lifecycle) management.

  Resources are stored in a data structure with the following layout:

   {:contents {key-1 {:resource resource :close! close! :slots slots}
               ...
               key-n {:resource resource :close! close! :slots slots}
    :paths    {slot-key-1 #{resource-keys}
               ...
               slot-key-n #{resource-keys}}}.

  'resource' is an arbitrary non-nil object.

  'close!' is a function (f [resource]) that will be invoked when a resource has
           expired and is supposed to release the resource.

  'slots' are a map {slot-key-1 slot-function-1
                     ...
                     slot-key-n slot-function-n},

          where each slot function is a function (f [resource & args]).
          When invoked, a slot function is supposed to update the passed
          resource and is expected to return the resulting resource state.

  The ':paths' map defines the paths from each existing slot to the related
  slot functions. It does so by mapping each slot key to a set, containing the
  keys of all resources for which a slot with the respective key exists. When
  a message is sent to a slot, it'll be dipatched to the related slot functions
  via the ':paths' map."

    :author "Frank Mosebach"}
  fm.resources.core
  (:refer-clojure :exclude [remove send])
  (:require
    [fm.resources.types :as types]))

(defn get-resource
  ([resources key]
    (get-resource resources key nil))
  ([resources key default]
    (get-in resources [:contents key :resource] default)))

(defn good
  ([]
    (good nil))
  ([rsc]
    (reify types/ResourceState
      (expired? [this]
        false)
      (resource [this]
        rsc))))

(defn expired
  ([]
    (expired nil))
  ([rsc]
    (reify types/ResourceState
      (expired? [this]
        true)
      (resource [this]
        rsc))))

(defn- resource-value [resource close! slots]
  (if (empty? slots)
    {:resource resource :close! close!}
    {:resource resource :close! close! :slots slots}))

(defn- remove-paths [paths key slot-keys]
  (reduce (fn [paths slot-key]
            (let [keys (disj (paths slot-key) key)]
              (if (empty? keys)
                (dissoc paths slot-key)
                (assoc paths slot-key keys))))
          paths
          slot-keys))

(defn- add-paths [paths key slot-keys]
  (reduce (fn [paths slot-key]
            (update-in paths [slot-key] #(conj (or % #{}) key)))
          paths
          slot-keys))

(defn- squeeze-resources [{:keys [contents paths] :as resources}]
  (let [resources (cond
                    (empty? contents) (dissoc resources :contents :paths)
                    (empty? paths)    (dissoc resources :paths)
                    :else             resources)]
    (if-not (empty? resources)
      resources)))

(defn- squeeze [resources removed]
  (let [resources (squeeze-resources resources)
        removed   (seq removed)]
    (cond
      removed   [resources removed]
      resources [resources]
      :else     nil)))

(defn store [{:keys [contents paths] :as resources} key resource
             {:keys [close! slots] :or {close! (constantly nil)}}]
  (assert (not (nil? resource)))
  (assert close!)
  (let [replaced  (get contents key)
        contents  (assoc contents key (resource-value resource close! slots))
        paths     (remove-paths paths key (keys (:slots replaced)))
        paths     (add-paths paths key (keys slots))
        resources (assoc resources :contents contents :paths paths)]
    (squeeze resources (if-not (nil? replaced) [replaced]))))

(defn- remove-resource [{:keys [contents paths] :as resources} key]
  (if-let [{slots :slots :as value} (get contents key)]
    [(assoc resources :contents (dissoc contents key)
                      :paths    (remove-paths paths key (keys slots))) value]
    [resources]))

(defn- remove-resources [resources removed keys]
  (if (seq keys)
    (let [[resources value] (remove-resource resources (first keys))]
      (recur resources
             (if value (conj removed value) removed)
             (rest keys)))
    (squeeze resources removed)))

(defn remove [{contents :contents :as resources} keys]
  (if (seq keys)
    (remove-resources resources [] keys)
    (squeeze (dissoc resources :contents) (vals contents))))

(defn- call-slot [[{contents :contents :as resources} removed]
                  key slot-key slot-args]
  (if-let [{:keys [resource slots] :as value} (get contents key)]
    (let [resource-state (apply (get slots slot-key) resource slot-args)
          expired?       (types/expired? resource-state)
          resource       (types/resource resource-state)]
      (if expired?
        (let [[resources] (remove-resource resources key)
              value       (if (nil? resource)
                            value
                            (assoc value :resource resource))]
          [resources (conj removed value)])
        [(assoc-in resources [:contents key :resource] resource) removed]))
    [resources removed]))

(defn- call-slots [resources keys slot-key slot-args]
  (apply squeeze
         (reduce #(call-slot %1 %2 slot-key slot-args) [resources []] keys)))

(defn send
  ([resources signal args]
    (send resources signal args nil))
  ([{paths :paths :as resources} signal args keys]
    (if-let [keys (if (seq keys)
                    (seq (filter #(contains? (get paths signal) %) keys))
                    (get paths signal))]
      (call-slots resources keys signal args)
      (squeeze resources nil))))

(defn close! [{:keys [resource close!]}]
  (io! (close! resource)))

(defn close-all! [resource-values]
  (doseq [value resource-values]
    (close! value)))
