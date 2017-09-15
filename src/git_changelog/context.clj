(ns git-changelog.context
  (:gen-class))

(defmacro def-m [name args body]
  `(def ~name (fn [~'context] (fn ~args ~body))))

(defmacro run-m
  [context [funcm & args]]
  `((~funcm ~context) ~@args))

(defmacro do-m
  [body]
  (list `run-m 'context body))

(defn- let-bindings
  [handler bindings]
  (cond
    (odd? (count bindings)) (throw (RuntimeException. "Even amount of elements expected"))
    (empty? bindings) (vec bindings)
    :else (let [var    (first bindings)
                action (second bindings)]
            (concat [var (handler action)] (let-bindings handler (drop 2 bindings))))))

(defn- let-statement
  [handler bindings body]
  (list 'let (vec (let-bindings handler bindings))
        body))

(defmacro with-context
  [bindings body]
  (letfn [(handler [mfunc] (list mfunc 'context))]
    (let-statement handler bindings body)))

(defmacro let-m
  [bindings body]
  (letfn [(handler [mfunc] (list `do-m mfunc))]
    (let-statement handler bindings body)))
