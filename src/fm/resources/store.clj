(ns
  ^{:doc "API for resource store access and manipulation."
    :author "Frank Mosebach"}
  fm.resources.store
  (:require
    [fm.resources.core :as core]
    [fm.resources.types :as types]))

(defn ref-store []
  (let [store (ref nil)]
    (reify types/ResourceStore
      (update! [this update]
        (dosync
          (let [{good :good :as resources} (update {:good @store})]
            (ref-set store good)
            resources)))
      (contents [this]
        @store))))

(defn partition-store [ref key]
  (reify types/ResourceStore
    (update! [this update]
      (dosync
        (let [{good :good :as resources} (update {:good (get @ref key)})]
          (if (empty? good)
            (alter ref dissoc key)
            (alter ref assoc key good))
          resources)))
    (contents [this]
      (get @ref key))))

(defn- update-and-clean-up [store update]
  (-> (types/update! store update) core/clean-up! :good))

(defn store! [store key resource & kwargs]
  (update-and-clean-up store #(apply core/store % key resource kwargs)))

(defn update! [store & kwargs]
  (update-and-clean-up store #(apply core/update % kwargs)))

(defn send! [store id event & keys]
  (update-and-clean-up store
                       #(core/send-event % :id id :event event :keys keys)))

(defn remove! [store key & keys]
  (update-and-clean-up store #(core/remove % (cons key keys))))

(defn sweep! [store]
  (update! store :update identity))

(defn clear! [store]
  (update-and-clean-up store #(core/remove %)))

(defn resource
  ([store key]
    (resource store key nil))
  ([store key default]
    (if-let [{resource :resource} (get (types/contents store) key)]
      resource
      default)))
