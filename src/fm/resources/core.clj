(ns
  ^{:doc "Core operations for resource (lifecycle) management."
    :author "Frank Mosebach"}
  fm.resources.core
  (:refer-clojure :exclude [remove]))

(def ^{:private true
       :doc "Functions to be used as default values if the respective resource
            function is not contained in a resource's context."}
     default-functions {:on-event (fn [id event stored] stored)
                        :expired? (constantly false)
                        :close!   (constantly nil)})

(defn with-default-functions [context]
  (merge default-functions context))

(defn store [{:keys [good expired] :or {expired []} :as resources}
              key resource & {:as context}]
  (assert resource)
  (let [context  (with-default-functions context)
        replaced (get good key)
        good     (assoc good key {:resource resource :context context})
        expired  (if replaced (conj expired [key replaced]) expired)]
    (assoc resources :good good :expired expired)))

(defn- expired? [{{expired? :expired?} :context :as stored}]
  (or (nil? stored)
      (and expired?
           (expired? stored))))

(defn- apply-update [{good :good :as resources} kees update]
  (reduce
    (fn [{:keys [good expired] :as resources} key]
      (if-let [stored (get good key)]
        (let [updated (update stored)]
          (if (expired? updated)
            (assoc resources :good    (dissoc good key)
                             :expired (if updated
                                        (conj expired updated)
                                        expired))
            (assoc resources :good (assoc good key updated))))
        resources))
    resources
    (or (seq kees) (keys good))))

(defn update [{:keys [good expired] :or {expired []} :as resources} &
              {:keys [keys update args]}]
  (assert update)
  (apply-update resources keys #(apply update % args)))

(defn- on-event [{{on-event :on-event} :context :as stored} id event]
  (if on-event
    (on-event id event stored)))

(defn send-event [{:keys [good expired] :or {expired []} :as resources} &
                  {:keys [keys id event]}]
  (assert id)
  (assert event)
  (apply-update resources keys #(on-event % id event)))

(defn remove
  ([resources]
    (remove resources nil))
  ([{:keys [good expired] :or {expired []} :as resources} keys]
    (if (seq keys)
      (assoc resources :good    (apply dissoc good keys)
                       :expired (into expired (select-keys good keys)))
      (assoc resources :good (empty good) :expired (into expired good)))))

(defn- close! [{{close! :close!} :context :as stored}]
  (if close!
    (close! stored))
  nil)

(defn clean-up! [{expired :expired :as resources}]
  (io!
    (doseq [stored (vals resources)]
      (close! stored))
    (dissoc resources :expired)))
