(ns git-changelog.cli
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [git-changelog.context :refer [mrun defm letm with-context]]
            [git-changelog.template :as template]
            [git-changelog.system :as system]
            [git-changelog.core :as core])
  (:gen-class))

(defm get-cli-options
  []
  (letm
    [{as-file             :as-file
      is-file?            :is-file?
      is-repository-path? :is-repository-path?}]
    [["-t" "--template" "template file"
      :required "TEMPLATE"
      :parse-fn #(as-file %)
      :validate [#(is-file? %) "template file does not exist"]]
     ["-r" "--repository" "path to the git repository in the filesystem. Defaults to working directory."
      :required "PATH"
      :validate [(fn [path] (is-repository-path? path)) "path does not contain a git repository"]
      :default "."]
     ["-h" "--help"]]))

(defm parse-args
  [args]
  (let
      [cli-options (mrun (get-cli-options))
       {:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (letm
      [{:keys [is-repository-path? is-valid-reference?]}]
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
         :to (second arguments)}))))

(defm help
  [args]
  (with-context [help!]
    (help! (:summary args))))

(defm exit-with-errors
  [args]
  (with-context [exit-with-errors!]
    (exit-with-errors! (:errors args))))

(defm generate-release-notes
  [args]
  (with-context
    [out!
     read-file!
     generate-release-notes!]
    (let [{:keys [template repository from to]} args]
      (out! (template/render (read-file! template)
                             (generate-release-notes! repository from to))))))

(defm app
  [args]
  (let [parsed-args     (mrun (parse-args args))
        action-mappings {:help help
                         :exit-with-errors exit-with-errors
                         :generate-release-notes generate-release-notes}
        action          (-> parsed-args :action action-mappings)]
    (mrun (action parsed-args))))

(defn -main
  [& args]
  (let [context {:out!                    println
                 :read-file!              slurp
                 :as-file                 io/as-file
                 :is-file?                #(.isFile %)
                 :help!                   system/help!
                 :exit-with-errors!       system/exit-with-errors!
                 :is-repository-path?     core/is-repository-path?
                 :is-valid-reference?     core/is-valid-reference?
                 :generate-release-notes! core/generate-release-notes!}]
    (mrun context (app args))))
