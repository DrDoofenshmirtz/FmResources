(ns
  ^{:doc "Core operations for resource (lifecycle) management."
    :author "Frank Mosebach"}
  fm.resources.core
  (:refer-clojure :exclude [remove]))

(def ^{:private true
       :doc "Functions to be used as default values if the respective resource
            function is not given when a resource is submitted."}
     default-functions {:on-event (fn [id event resource] resource)
                        :expired? (constantly false)
                        :close!   (constantly nil)})

(defn with-default-functions [funcs]
  (merge default-functions funcs))

(defn store [{:keys [good expired] :or {expired []} :as resources}
              key resource & {:as funcs}]
  (assert resource)
  (let [funcs    (with-default-functions funcs)
        replaced (get good key)
        good     (assoc good key (assoc funcs :resource resource))
        expired  (if replaced (conj expired [key replaced]) expired)]
    (assoc resources :good good :expired expired)))

(defn- expired? [[key {:keys [resource expired?]}]]
  (expired? resource))

(defn- update-entries [{good :good :as resources} kees update]
  (if update
    (reduce
      (fn [{:keys [good expired] :as resources} key]
        (if-let [stored (get good key)]
          (if-let [updated (update [key stored])]
            (if (expired? updated)
              (assoc resources :good    (dissoc good key)
                               :expired (conj expired updated))
              (assoc resources :good (conj good updated)))
            resources)
          resources))
      resources
      (or (seq kees) (keys good)))
    resources))

(defn- update-resource [[key {resource :resource :as stored}] update args]
  (if update
    [key (assoc stored :resource (apply update resource args))]
    stored))

(defn update [{:keys [good expired] :or {expired []} :as resources} &
              {:keys [keys update args]}]
  (assert update)
  (update-entries resources keys #(update-resource % update args)))

(defn- process-event [[key {:keys [resource on-event] :as stored}] id event]
  [key (assoc stored :resource (on-event id event resource))])

(defn send-event [{:keys [good expired] :or {expired []} :as resources} &
                  {:keys [keys id event]}]
  (assert id)
  (assert event)
  (update-entries resources keys #(process-event % id event)))

(defn remove
  ([resources]
    (remove resources nil))
  ([{:keys [good expired] :or {expired []} :as resources} keys]
    (if (seq keys)
      (assoc resources :good    (apply dissoc good keys)
                       :expired (into expired (select-keys good keys)))
      (assoc resources :good (empty good) :expired (into expired good)))))

(defn clean-up! [{expired :expired :as resources}]
  (io!
    (doseq [[key {:keys [resource close!]}] expired]
      (close! resource))
    (dissoc resources :expired)))
