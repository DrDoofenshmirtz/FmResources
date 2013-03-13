(ns
  ^{:doc "API for resource store access and manipulation."
    :author "Frank Mosebach"}
  fm.resources.store
  (:require
    [fm.resources.core :as core]
    [fm.resources.types :as types]))

(defn ref-store
  ([]
    (ref-store nil))
  ([store]
    (let [store (or store (ref nil))]
      (reify types/ResourceStore
        (update! [this update]
          (dosync
            (let [[resources removed :as result] (update @store)]
              (ref-set store resources)
              result)))
        (contents [this]
          @store)))))

(defn partition-store [ref key]
  (reify types/ResourceStore
    (update! [this update]
      (dosync
        (let [[resources removed :as result] (update (get @ref key))]
          (if (empty? resources)
            (alter ref dissoc key)
            (alter ref assoc key resources))
          result)))
    (contents [this]
      (get @ref key))))

(defn update-and-clean-up! [store update]
  (assert store)
  (assert update)
  (let [[resources removed] (types/update! store update)]
    (core/close-all! removed)
    resources))

(defn store! [store key resource & kwargs]
  (update-and-clean-up! store #(core/store % key resource kwargs)))

(defn send! [store signal & args]
  (update-and-clean-up! store #(core/send % signal args)))

(defn send-to! [store keys signal & args]
  (update-and-clean-up! store #(core/send % signal args keys)))

(defn remove! [store & keys]
  (update-and-clean-up! store #(core/remove % keys)))

(defn clear! [store]
  (remove! store))

(defn get-resource
  ([store key]
    (get-resource store key nil))
  ([store key default]
    (if-let [resource (get-in (types/contents store) [:contents key :resource])]
      resource
      default)))
