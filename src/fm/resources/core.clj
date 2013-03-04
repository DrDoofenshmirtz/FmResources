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
          resource and is expected either return the resource or nil, if
          the resource has expired and is to be removed from the resources.

  The ':paths' map defines the paths from each existing slot to the related
  slot functions. It does so by mapping each slot key to a set, containing the
  keys of all resources for which a slot with the respective key exists. When
  a message is sent to a slot, it'll be dipatched to the related slot functions
  via the ':paths' map."

    :author "Frank Mosebach"}
  fm.resources.core
  (:refer-clojure :exclude [remove send]))

(defn- resource-value [resource close! slots]
  (if slots
    {:resource resource :close! close! :slots slots}
    {:resource resource :close! close!}))

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

(defn- call-slot [{:keys [resource slots]} slot-key slot-args]
  (if-let [slot (get slots slot-key)]
    (apply slot resource slot-args)
    resource))

(defn- call-slots [{contents :contents :as resources}
                   slot-key slot-args keys removed]
  (if (seq keys)
    (let [key      (first keys)
          keys     (rest keys)
          resource (call-slot (get contents key) slot-key slot-args)]
      (if (nil? resource)
        (let [[resources value] (remove-resource resources key)]
          (recur resources slot-key slot-args keys (conj removed value)))
        (recur (assoc-in resources [:contents key :resource] resource)
               slot-key
               slot-args
               keys
               removed)))
    (squeeze resources removed)))

(defn send [{paths :paths :as resources} slot-key slot-args]
  (if-let [keys (get paths slot-key)]
    (call-slots resources slot-key slot-args keys [])
    (squeeze resources nil)))

(defn close! [{:keys [resource close!]}]
  (io! (close! resource)))

(defn close-all! [resource-values]
  (doseq [value resource-values]
    (close! value)))
