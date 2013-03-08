(ns fm.resources.core-tests
  ^{:doc "Tests for namespace fm.resources.core."
    :author "Frank Mosebach"}
  (:require
    [clojure.test :as tst]
    [fm.resources.core :as core]
    [fm.resources.types :as types]))

(tst/deftest create-empty-good-state
  (let [good (core/good)]
    (tst/is (false? (types/expired? good)))
    (tst/is (nil? (types/resource good)))))

(tst/deftest create-good-state-with-resource
  (let [resource (Object.)
        good (core/good resource)]
    (tst/is (false? (types/expired? good)))
    (tst/is (identical? resource (types/resource good)))))

(tst/deftest create-empty-expired-state
  (let [expired (core/expired)]
    (tst/is (true? (types/expired? expired)))
    (tst/is (nil? (types/resource expired)))))

(tst/deftest create-expired-state-with-resource
  (let [resource (Object.)
        expired (core/expired resource)]
    (tst/is (true? (types/expired? expired)))
    (tst/is (identical? resource (types/resource expired)))))

(tst/deftest create-resource-value
  (let [resource (Object.)
        close! (constantly nil)]
    (tst/is (= {:resource resource :close! close!}
               (#'core/resource-value resource close! nil)))
    (tst/is (= {:resource resource :close! close!}
               (#'core/resource-value resource close! {})))
    (tst/is (= {:resource resource
                :close! close!
                :slots {:slot-1 1 :slot-2 2}}
               (#'core/resource-value resource close! {:slot-1 1 :slot-2 2})))))

(tst/deftest add-resource-without-slots
  (let [key      (Object.)
        resource (Object.)
        close!   (constantly nil)
        slots    {}
        kwargs   {:close! close! :slots  slots}
        [resources replaced :as result] (core/store nil key resource kwargs)]
    (tst/is (= [resources nil] [resources replaced]))
    (tst/is (= [resources] result))
    (tst/is (= {:contents {key {:resource resource :close! close!}}}
               resources))))

(tst/deftest add-resources-with-slots
  (let [key-1    (Object.)
        key-2    (Object.)
        resource (Object.)
        close!   (constantly nil)
        slots-1  {:slot-1 identity :slot-2 identity}
        slots-2  {:slot-2 identity}
        [resources replaced :as result] (-> nil
                                            (core/store key-1
                                                        resource
                                                        {:close! close!
                                                         :slots  slots-1})
                                            first
                                            (core/store key-2
                                                        resource
                                                        {:close! close!
                                                         :slots  slots-2}))]
    (tst/is (= [resources nil] [resources replaced]))
    (tst/is (= [resources] result))
    (tst/is (= {:contents {key-1 {:resource resource
                                  :close!   close!
                                  :slots    slots-1}
                           key-2 {:resource resource
                                  :close!   close!
                                  :slots    slots-2}}
              :paths      {:slot-1 #{key-1}
                           :slot-2 #{key-1 key-2}}}
               resources))))

(tst/deftest send-to-all
  (let [close!      (constantly nil)
        ++          (fn [resource]
                      (let [resource (inc resource)]
                        (if (< resource 3)
                          (core/good resource)
                          (core/expired resource))))
        --          (fn [resource]
                      (let [resource (dec resource)]
                        (if (pos? resource)
                          (core/good resource)
                          (core/expired resource))))
        [resources] (-> nil
                        (core/store :key-1
                                    1
                                    {:close! close!
                                     :slots  {:++ ++ :-- --}})
                        first
                        (core/store :key-2
                                    4
                                    {:close! close!
                                     :slots  {:-- --}}))]
    (tst/is (= {:contents {:key-1 {:resource 1
                                   :close!   close!
                                   :slots    {:++ ++ :-- --}}
                           :key-2 {:resource 4
                                   :close!   close!
                                   :slots    {:-- --}}}
                :paths    {:++ #{:key-1}
                           :-- #{:key-1 :key-2}}}
               resources))
    (let [[resources removed :as result] (core/send resources :-- [])]
      (tst/is (= [(#'core/resource-value 0 close! {:++ ++ :-- --})]
                 removed))
      (tst/is (= {:contents {:key-2 {:resource 3
                                     :close!   close!
                                     :slots    {:-- --}}}
                  :paths    {:-- #{:key-2}}}
                 resources)))))

(defn run-tests []
  (tst/run-tests 'fm.resources.core-tests))
