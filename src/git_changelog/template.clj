(ns git-changelog.template
  (:require [clostache.parser :as clostache])
  (:gen-class))

(defn- format-data
  [summary]
  (letfn [(addresses?
            [issue-type commit]
            (-> commit (issue-type) count (> 0)))
          (add-see-also
            [summary type]
            (update summary type #(update-issues %)))
          (update-issues
            [issues]
            (map update-issue issues))
          (update-issue
            [issue]
            (update issue :commits #(map update-commit %)))
          (update-commit
            [commit]
            (assoc commit :see-also? (addresses? :see-also commit)))]
    (-> summary
        (merge {:implements? (addresses? :implements summary)
                :resolves?   (addresses? :resolves summary)
                :untracked?  (addresses? :untracked summary)})
        (add-see-also :implements)
        (add-see-also :resolves)
        (add-see-also :untracked))))

(defn render [template data]
  (clostache/render template (format-data data)))
