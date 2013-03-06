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
               (#'core/resource-value resource close! nil)))))

(defn run-tests []
  (clojure.test/run-tests 'fm.resources.core-tests))
