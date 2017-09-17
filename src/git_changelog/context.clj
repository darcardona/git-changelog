(ns git-changelog.context
  (:gen-class))

(defmacro defm [name args body]
  `(def ~name (fn [context#]
                (letfn [(~'<< ([key#] (key# context#))
                         ([] context#))]
                  (fn ~args ~body)))))

(defmacro mrun
  ([context [funcm & args]]
   `((~funcm ~context) ~@args))

  ([body]
   `(mrun (~'<<) ~body)))

(defmacro letm
  [bindings body]
  (letfn [(as-map [statement]
            (cond (map? statement) statement
                  :else     {statement (keyword statement)}))
          (inject-context [bindings*]
            (interleave (map as-map bindings*)
                        (repeat `(~'<<))))]
    (let [mbindings (inject-context bindings)]
      `(let [~@mbindings] ~body))))

(defmacro with-context [& args] `(letm ~@args))
