(ns adventofcode22.day7
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn lookup-at-path [tree path]
  (loop [path path
         place tree]
    (if (empty? path)
      place
      (let [v (get place (nth path 0) nil)]
        (if v
          (recur (rest path) v)
          nil)))))

(defn insert-at-path [tree path x]
  "Non-destructively insert an element in a tree consisting of nested maps."
  (let [old-branch-list
        (loop [remaining path
               acc '()
               subtree tree]
          (if (empty? remaining)
            acc
            (let [v (get subtree (nth remaining 0) {})]
              (recur (rest remaining)
                     (cons subtree acc)
                     v))))

        new-branch
        (loop [remaining-keys (reverse path)
               remaining-levels old-branch-list
               acc x]
          (if (empty? remaining-keys)
            acc
            (recur (rest remaining-keys)
                   (rest remaining-levels)
                   (assoc (first remaining-levels)
                          (first remaining-keys)
                          acc))))]
    new-branch))

(defn all-directories [tree]
  "Return a vector of paths (which are themselves vectors of strings) representing
 all the directories in the tree. Directories are returned in depth-first order,
 so a directory will always precede all its children in the list. This behavior
 is used by directory-sizes."
  (loop [saved-paths (map (fn [x] [x]) (keys tree))
         acc []]
    (if (empty? saved-paths)
      acc
      (let [[base-path & next] saved-paths
            sub (lookup-at-path tree base-path)]
        (if (map? sub)
          (let [new-saved-paths (into next
                                      (map #(conj base-path %))
                                      (keys sub))]
            (recur new-saved-paths
                   (conj acc base-path)))
          (recur next acc))))))

(defn directory-sizes [tree]
  (loop [paths (reverse (all-directories tree))
         acc {}]
    (if (empty? paths)
      acc
      (let [[p & next] paths
            subtree (lookup-at-path tree p)
            subpaths (map #(conj p %) (keys subtree))
            sizes (map (fn [x]
                         (let [v (lookup-at-path tree x)]
                           (if (map? v)
                             (get acc x)
                             v)))
                       subpaths)]
        (recur next (assoc acc p (reduce + sizes)))))))

(defn exec-cd [dir arg]
  (if (= arg "..")
    (pop dir)
    (conj dir arg)))

(defn maybe-insert-dir [tree parent name]
  (if (map? (lookup-at-path tree (conj parent name)))
    tree
    (insert-at-path tree (conj parent name) {})))

(defn exec-ls [lines dir tree]
  (let [[ls-lines rem-lines] (split-with #(not= (first %) \$) lines)
        new-tree
        (reduce (fn [acc in]
                  (let [[sz file] (clojure.string/split in #" " 2)]
                    (if (= sz "dir")
                      (maybe-insert-dir acc dir file)
                      (insert-at-path acc (conj dir file) (Integer/parseInt sz)))))
                tree
                ls-lines)]
    [rem-lines new-tree]))

(defn process-command [lines dir tree]
  "Returns [remaining-lines new-dir new-tree]. lines must be nonempty."
  (let [[_ cmd & args] (clojure.string/split (first lines) #" ")]
    (cond
      (= cmd "cd") [(rest lines)
                    (exec-cd dir (first args))
                    tree]
      (= cmd "ls") (let [[new-lines new-tree]
                         (exec-ls (rest lines) dir tree)]
                     [new-lines dir new-tree])
      :else [(rest lines) dir tree])))

(defn build-fs [lines]
  (loop [lines lines
         dir []
         tree {}]
    (let [[new-lines new-dir new-tree]
          (process-command lines dir tree)]
      (if (empty? new-lines)
        new-tree
        (recur new-lines new-dir new-tree)))))
(defn part1 [input]
  (let [dir-sizes
        (-> input file-lines build-fs directory-sizes)]
    (reduce (fn [acc in]
              (let [[k v] in]
                (if (<= v 100000)
                  (+ acc v)
                  acc)))
            0
            dir-sizes)))

(defn part2 [input]
  (let [dir-sizes (-> input file-lines build-fs directory-sizes)
        total-usage (get dir-sizes ["/"])
        amount-to-delete (- 30000000 (- 70000000 total-usage))]
    (reduce (fn [acc in]
              (let [[k v] in]
                (if (>= v amount-to-delete)
                  (min v acc)
                  acc)))
            total-usage
            dir-sizes)))
