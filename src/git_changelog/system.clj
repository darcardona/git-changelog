(ns git-changelog.system
  (:require [clojure.string :as string])
  (:gen-class))

(defn usage [options-summary]
  (->> ["Usage: release-notes [options] <from> <to>"
        ""
        "Options:"
        options-summary
        ""
        "Parameters:"
        "  from  commit, branch or tag of the previous release"
        "  to    commit, branch or tag of the target release"]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn -exit [status msg]
  (println msg)
  (System/exit status))

(defn exit-with-errors!
  [errors]
;  (exit 1 (error-msg errors)))
  (println (error-msg errors)))

(defn help!
  [summary]
;    (exit 0 (usage summary)))
  (println (usage summary)))
