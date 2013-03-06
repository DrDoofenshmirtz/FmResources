(ns
  ^{:doc "Types for resource (lifecycle) management."
    :author "Frank Mosebach"}
  fm.resources.types)

(defprotocol ResourceStore
  "Defines the contract for a place where resources can be stored."
  (update! [this update]
    "Updates the currently stored resources through application of
    the given update function.")
  (contents [this]
    "Returns (a snapshot of) the currenly stored contents."))

(defprotocol ResourceState
  "Defines the result type of resource slot functions."
  (expired? [this]
    "Determines if the resource has expired.")
  (resource [this]
    "Returns the resulting value of the resource."))

(extend-protocol ResourceState
  Object
    (expired? [this]
      false)
    (resource [this]
      this)
  nil
  (expired? [this]
    true)
  (resource [this]
    nil))
