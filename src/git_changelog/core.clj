(ns git-changelog.core
  (:require [clojure.java.io :as io]
            [clj-jgit.porcelain :as g])
  (:gen-class))

(defmulti resolve-object-id (fn [_ ref] (type ref)))

(defmethod resolve-object-id
  String
  [repo rev]
  (try
    (let [ref (.findRef repo rev)]
      (if (nil? ref)
        (.resolve repo rev)
        (resolve-object-id repo ref)))
    (catch org.eclipse.jgit.errors.RevisionSyntaxException exception nil)))

(defmethod resolve-object-id
  org.eclipse.jgit.lib.Ref
  [repo ref]
  (let [peeled-ref (.peel repo ref)
        peeled-id  (.getPeeledObjectId peeled-ref)]
    (if (nil? peeled-id) (.getObjectId ref) peeled-id)))

(defmethod resolve-object-id
  org.eclipse.jgit.revwalk.RevCommit
  [repo commit]
  (.getId commit))

(defn- get-tags
  "Lists the tags in a repo, returning them as a seq of strings."
  [^org.eclipse.jgit.api.Git git]
  (->> git
       (.tagList)
       (.call)))

(defn- remove-trailing-slash
  "Remove the trailing '/' from a URI string, if it exists."
  [^String uri]
  (if (.endsWith uri "/")
    (.substring uri 0 (dec (.length uri)))
    uri))

(defn is-valid-reference?
  [path ref]
  (g/with-repo path (resolve-object-id (.getRepository repo) ref)))

(defn is-repository-path?
  [path]
  (let [git-path (io/as-file (str (remove-trailing-slash path) "/.git"))]
    (org.eclipse.jgit.lib.RepositoryCache$FileKey/isGitRepository git-path org.eclipse.jgit.util.FS/DETECTED)))

(defn- get-tag
  [repo tags commit]
  (cond
    (empty? tags) nil
    :else (let [[tag & tail] (seq tags)
                tag-id       (resolve-object-id repo tag)]
            (if (= commit tag-id) tag (get-tag repo tail commit)))))

(defn parse-commit
  [commit]
  {:subject    (.getShortMessage commit)
   :author     (.getEmailAddress (.getAuthorIdent commit))
   :hash       (.getName (.getId commit))
   :short-hash (.name (.abbreviate (.getId commit) 7))
   :committer  (.getEmailAddress (.getCommitterIdent commit))
   :implements (.getFooterLines commit "implements")
   :resolves   (.getFooterLines commit "resolves")
   :see-also   (.getFooterLines commit "see-also")})

(defn- log-between
  [git from to]
  (let [repo    (.getRepository git)
        from-id (resolve-object-id repo from)
        to-id   (resolve-object-id repo to)]
    (-> git
        (.log)
        (.setRevFilter org.eclipse.jgit.revwalk.filter.RevFilter/NO_MERGES)
        (.addRange from-id to-id)
        (.call))))

(def tag-name #(->> % .getName (re-matches #"refs/tags/(.*)") second))

(defn get-tags-names
  "Lists the tags in a repo, returning them as a seq of strings."
  [^org.eclipse.jgit.api.Git git-repo]
  (map tag-name (get-tags git-repo)))

(defn- when-nil [default expr] (if (nil? expr) default expr)) ;; TODO: turn into a macro

(defn- log-detailed*
  [git previous-tag all-tags commits]
  (letfn [(find-tag [commit]
            (when-nil previous-tag (get-tag (.getRepository git) all-tags commit)))]
    (if (empty? commits)
      ()
      (let [[commit & tail] commits
            current-tag     (find-tag commit)
            commit-data     (parse-commit commit)
            element         (merge (parse-commit commit)
                                   {:tag (tag-name current-tag)})]
        (cons element (log-detailed* git current-tag all-tags tail))))))

(defn- log-detailed
  [repo-path from to]
  (g/with-repo repo-path
    (log-detailed* repo nil (get-tags repo) (iterator-seq (.iterator (log-between repo from to))))))

(defn- log
  [repo-path from to]
  (g/with-repo repo-path
    (map parse-commit (log-between repo from to))))

(defn- all-addressed [issue-type log]
  (set (reduce concat (map #(get % issue-type) log))))

(defn- group-addressing [issue-type commits]
  (let [all-keys (all-addressed issue-type commits)
        addressing (fn [issue] #(.contains (get % issue-type) issue))
        reducer    (fn [acc issue]
                     (conj acc {:issue issue :commits (filter (addressing issue) commits)}))]
    (reduce reducer [] all-keys )))

(defn generate-release-notes!
  [repository from to]
  (let [commits (log repository from to)]
    {:implements (group-addressing :implements commits)
     :resolves   (group-addressing :resolves commits)
     :untracked  (let [untracked? #(and (empty? (:implements %))
                                        (empty? (:resolves %)))]
                   (filter untracked? commits))}))
