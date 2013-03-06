(ns fm.resources.core-tests
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

(defn run-tests []
  (tst/run-tests 'fm.resources.core-tests))
