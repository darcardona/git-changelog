(ns git-changelog.cli
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [git-changelog.context :refer [run-m def-m with-context let-m do-m]]
            [git-changelog.template :as template]
            [git-changelog.system :as system]
            [git-changelog.core :as core])
  (:gen-class))

(def-m get-cli-options
  []
  (with-context
    [as-file             :as-file
     is-file?            :is-file?
     is-repository-path? :is-repository-path?]
    [["-t" "--template" "template file"
      :required "TEMPLATE"
      :parse-fn #(as-file %)
      :validate [#(is-file? %) "template file does not exist"]]
     ["-r" "--repository" "path to the git repository in the filesystem. Defaults to working directory."
      :required "PATH"
      :validate [(fn [path] (is-repository-path? path)) "path does not contain a git repository"]
      :default "."]
     ["-h" "--help"]]))

(def-m parse-args
  [args]
  (let-m
    [cli-options (get-cli-options)]
    (let
      [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
      (with-context
        [is-repository-path? :is-repository-path?
         is-valid-reference? :is-valid-reference?]
        (cond
          ;; help => exit OK with usage summary
          (:help options)
          {:action :help
           :summary summary}

          ;; errors => exit with description of error
          errors
          {:action :exit-with-errors
           :errors errors}

          ;; template is required
          (nil? (:template options))
          {:action :exit-with-errors
           :errors ["Missing required argument: -t"]}

          ;; path should be a valid git repository
          (not (is-repository-path? (:repository options)))
          {:action :exit-with-errors
           :errors [(str (:repository options) " is not a valid git repository")]}

          (not= 2 (count arguments))
          {:action :exit-with-errors
           :errors ["Invalid number of arguments"]}

          (nil? (is-valid-reference? (:repository options) (first arguments)))
          {:action :exit-with-errors
           :errors [(str (first arguments) " is not a valid commit reference")]}

          (nil? (is-valid-reference? (:repository options) (second arguments)))
          {:action :exit-with-errors
           :errors [(str (second arguments) " is not a valid commit reference")]}

          :else
          {:action :generate-release-notes
           :repository (:repository options)
           :template (:template options)
           :from (first arguments)
           :to (second arguments)})))))

(def-m help
  [args]
  (with-context [help! :help!]
    (help! (:summary args))))

(def-m exit-with-errors
  [args]
  (with-context [exit-with-errors! :exit-with-errors!]
    (exit-with-errors! (:errors args))))

(def-m generate-release-notes
  [args]
  (with-context
    [out!                    :out!
     generate-release-notes! :generate-release-notes!]
    (let [{:keys [template repository from to]} args]
      (out! (template/render (slurp template)
                             (generate-release-notes! repository from to))))))

(def-m app
  [args]
  (let [parsed-args (do-m (parse-args args))
        action      (-> parsed-args :action name symbol resolve)]
    (do-m (action parsed-args))))

(defn -main
  [& args]
  (let [context {:out!                    println
                 :as-file                 io/as-file
                 :is-file?                #(.isFile %)
                 :help!                   system/help!
                 :exit-with-errors!       system/exit-with-errors!
                 :is-repository-path?     core/is-repository-path?
                 :is-valid-reference?     core/is-valid-reference?
                 :generate-release-notes! core/generate-release-notes!}]
    (run-m context (app args))))
