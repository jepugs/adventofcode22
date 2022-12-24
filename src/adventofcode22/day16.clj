(ns adventofcode22.day16
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(set! *unchecked-math* true)

(defn parse-vertex [line]
  "Returns [vertex-name flow-rate neighbors] where vertex-name is a string,
flow-rate is an int, and neighbors is a seq of strings."
  (let [pairs-of-caps (re-seq #"[A-Z]{2}" line)
        flow-rate (-> (re-find #"rate=([0-9]+)" line)
                      (nth 1)
                      Integer/parseInt)]
    [(symbol (first pairs-of-caps))
     flow-rate
     (map symbol (rest pairs-of-caps))]))

(defrecord graph [start num-vertices edges vertex-weights
                  edge-weights labels->nums nums->labels])

(defn mk-edge-weight-fun [num-vertices edge-weights nums->labels]
  "Helper for graph-from-sets. Creates the edge weight map. This code was
written by a moron."
  (into {}
        (map (fn [l]
               [l (into {}
                        (keep (fn [r]
                                (let [w (edge-weights #{(nums->labels l)
                                                        (nums->labels r)})]
                                  (if w [r w] nil))))
                        (range (inc l) num-vertices))])
             (range num-vertices))))

(defn mk-edge-weight-fun [num-vertices edge-weights nums->labels]
  "Helper for graph-from-sets. Creates the edge weight map. This code was
written by a moron."
  (mapv (fn [l]
          (mapv (fn [r]
                  (if (= l r)
                    nil
                    (or (edge-weights #{(nums->labels l)
                                        (nums->labels r)})
                        nil)))
                (range num-vertices)))
        (range num-vertices)))

(defn graph-from-sets [start vertices edges vertex-weights edge-weights]
  (let [labels->nums (into {} (map-indexed (fn [i x] [x i]) vertices))
        nums->labels (into {} (map-indexed (fn [i x] [i x]) vertices))
        num (count vertices)
        edges (mapv #(let [l (first %)
                           r (first (disj % l))
                           l1 (labels->nums l)
                           r1 (labels->nums r)]
                       (if (< l1 r1)
                         [l1 r1]
                         [r1 l1]))
                    edges)
        edge-weight-fun (mk-edge-weight-fun num edge-weights nums->labels)]
    (new graph
         (labels->nums start)
         num
         edges
         (mapv #(vertex-weights (nums->labels %)) (range num))
         edge-weight-fun
         labels->nums
         nums->labels)))

;; (defn get-edge-weight [graph l r]
;;   (if (<= l r)
;;     (((:edge-weights graph) l) r)
;;     (((:edge-weights graph) r) l)))
(defn get-edge-weight [^graph graph l r]
  (nth (nth (:edge-weights graph) l) r))

(defn lookup-edge-weight [weight-vec l r]
  (nth (nth weight-vec l) r))

(defn get-vertex-weight [^graph graph i]
  (nth (:vertex-weights graph) i))

(defn build-graph [parsed-vertices]
  "Big assumption: this graph this builds is not directed."
  (loop [vertices #{}
         edges #{}
         vertex-weights {}
         edge-weights {}
         xs parsed-vertices]
    (if (empty? xs)
      (graph-from-sets 'AA
                       vertices
                       edges
                       vertex-weights
                       edge-weights)
      (let [[[v l us] & tl] xs
            new-edges (map (fn [u] #{v u}) us)]
        (recur (conj vertices v)
               (into edges new-edges)
               (assoc vertex-weights v l)
               (into edge-weights (map (fn [e] [e 1]) new-edges))
               tl)))))

(defn parse-graph [lines]
  (build-graph (map parse-vertex lines)))

(def test-lines (clojure.string/split-lines "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"))

(def test-graph
  (build-graph (map parse-vertex test-lines)))

(defn neighbors [graph u]
  (into []
        (comp (filter #(or (= (nth % 0) u)
                           (= (nth % 1) u)))
              (map #(first (filter (fn [x] (not= x u)) %))))
        (:edges graph)))

(defn spd [graph u v]
  "Shortest path distance, ignoring edge weights. If there's no path this does
an infinite loop lmao"
  (loop [ct 0
         current-nodes #{u}
         visited #{}]
    (if (contains? current-nodes v)
      ct
      (let [next-nodes (reduce into
                               #{}
                               (map #(neighbors graph %) current-nodes))]
        (recur (+ ct 1)
               next-nodes
               (into visited next-nodes))))))

(defn contract-graph [graph]
  "Starting from the graph we read in, get rid of all vertices having flow rate
zero (except the start). The resulting graph will be a complete weighted graph
with edge weights equal to the shortest path distance between each of the
vertices plus 1. (The plus one is to account for the time spent turning on the
valve. Since we construct a complete graph we might as well assume we turn on
each valve as soon as we visit it)."
  (let [{:keys [start num-vertices edges vertex-weights nums->labels labels->nums]} graph
        keep-vertices (as-> (range num-vertices) $
                        (filter #(not= (vertex-weights %) 0) $)
                        (conj $ start)
                        (map nums->labels $)
                        (set $))
        ;; not the most efficient way to compute edges but w/e
        edges (reduce into
                      #{}
                      (map (fn [v]
                             (keep #(if (not= % v)
                                      #{v %}
                                      nil)
                                  keep-vertices))
                           keep-vertices))
        edge-weights-map (into {}
                               (map (fn [e]
                                      [e (inc (spd graph
                                                   (labels->nums (first e))
                                                   (labels->nums (second e))))])
                                    edges))
        vertex-weights-map
        (into {}
              (map-indexed (fn [i v]
                             [v (get-vertex-weight graph (labels->nums v))])
                           keep-vertices))]
    (graph-from-sets (nums->labels start)
                     keep-vertices
                     edges
                     vertex-weights-map
                     edge-weights-map)))

(defn extensions-of-path [edge-weights num-vertices path-so-far len-so-far]
  "Get all the ways we could extend a path in a COMPLETE weighted graph by one
vertex. Returns list of vecs: containing respectively all extended paths,
their lengths (using edge weights), and the vertex set. Paths are assumed to be vecs."
  (let [end (peek path-so-far)
        end-weights (nth edge-weights end)
        vset (set path-so-far)
        new-vertices (filter #(not (vset %)) (range num-vertices))]
    (mapv (fn [v]
            [(conj path-so-far v)
             (+ len-so-far (nth end-weights v))])
          new-vertices)))

(defn paths-upto-length [graph max-len]
  "Get all maximal paths under length len, using edge weights to determine path length.
These are paths, not walks, so they contain no duplicates. Paths are maximal in
the sense that adding another vertex would either backtrack or exceed the given
length."
  (let [{:keys [num-vertices edge-weights]} graph
        acc (transient [])]
    (loop [active-paths [[[(:start graph)] 0]]]
      (if (empty? active-paths)
        (persistent! acc)
        (let [new-active
              (loop [remaining active-paths
                     new-active []]
                (if (empty? remaining)
                  new-active
                  (let [[[path len] & tl] remaining
                        exts (extensions-of-path edge-weights
                                                 num-vertices
                                                 path
                                                 len)
                        short-enough (filter #(<= (nth % 1) max-len) exts)]
                    (if (empty? short-enough)
                      (do
                        (conj! acc path)
                        (recur tl new-active))
                      (recur tl (into new-active short-enough))))))]
          (recur new-active))))))

(defn r-reduce-paths-upto-length [fun init ^graph graph max-len]
  "like reduce-paths-upto-length, but recursive"
  (let [{:keys [num-vertices edge-weights]} graph
        all-vertices (into [] (range num-vertices))]
    (letfn [(descend [acc path-so-far len-so-far vset]
              (let [end (peek path-so-far)
                    dist-left (- max-len len-so-far)
                    next-neighbors
                    (filter #(and (not (contains? vset %))
                                  (<= (lookup-edge-weight edge-weights end %)
                                      dist-left))
                            all-vertices)]
                (if (empty? next-neighbors)
                  (fun acc path-so-far)
                  (reduce #(descend %1
                                    (conj path-so-far %2)
                                    (+ len-so-far (lookup-edge-weight edge-weights end %2))
                                    (conj vset %2))
                          acc
                          next-neighbors))))]
      (descend init [(:start graph)] 0 #{(:start graph)}))))

(defn r-reduce-all-paths-upto-length [fun init ^graph graph max-len]
  "like reduce-paths-upto-length, but recursive"
  (let [{:keys [num-vertices]} graph
        all-vertices (into [] (range num-vertices))]
    (letfn [(descend [acc path-so-far len-so-far vset]
              (let [end (peek path-so-far)
                    dist-left (- max-len len-so-far)
                    next-neighbors (filter #(and (not (contains? vset %))
                                                 (<= (get-edge-weight graph end %)
                                                     dist-left))
                                           all-vertices)]
                (if (empty? next-neighbors)
                  (fun acc path-so-far)
                  (reduce #(descend %1
                                    (conj path-so-far %2)
                                    (+ len-so-far (get-edge-weight graph end %2))
                                    (conj vset %2))
                          (fun acc path-so-far)
                          next-neighbors))))]
      (descend init [(:start graph)] 0 #{(:start graph)}))))

(defn r-reduce-disj-paths-upto-length [fun init ^graph graph max-len starting-path]
  "like reduce-paths-upto-length, but recursive"
  (let [{:keys [num-vertices]} graph
        vset (conj (set starting-path) (:start graph))
        all-vertices (into [] (filter #(not (contains? vset %))
                                      (range num-vertices)))]
    (letfn [(descend [acc path-so-far len-so-far vset]
              (let [end (peek path-so-far)
                    dist-left (- max-len len-so-far)
                    next-neighbors (filter #(and (not (contains? vset %))
                                                 (<= (get-edge-weight graph end %)
                                                     dist-left))
                                           all-vertices)]
                (if (empty? next-neighbors)
                  (fun acc path-so-far)
                  (reduce #(descend %1
                                    (conj path-so-far %2)
                                    (+ len-so-far (get-edge-weight graph end %2))
                                    (conj vset %2))
                          acc
                          next-neighbors))))]
      (descend init [(:start graph)] 0 vset))))

(defn reduce-paths-upto-length [fun init ^graph graph max-len]
  "Reduce over all maximal paths under length len, using edge weights to
determine path length."
  (let [{:keys [num-vertices edge-weights]} graph]
    (loop [active-paths [[[(:start graph)] 0]]
           acc init]
      (if (empty? active-paths)
        acc
        (let [[new-active new-acc]
              (loop [remaining active-paths
                     new-active []
                     new-acc acc]
                (if (empty? remaining)
                  [new-active new-acc]
                  (let [[[path len] & tl] remaining
                        exts (extensions-of-path edge-weights
                                                 num-vertices
                                                 path
                                                 len)
                        short-enough (filter #(<= (nth % 1) max-len) exts)]
                    (if (empty? short-enough)
                      (recur tl new-active (fun new-acc path))
                      (recur tl (into new-active short-enough) acc)))))]
          (recur new-active new-acc))))))

(defn path-pressure-release [graph time vertices]
  "Causes an exception if vertices is empty. Assumes the first valve need not be
opened."
  (let [len (count vertices)]
    (loop [cur-time 0
           cur-rate 0
           acc 0
           n 1]
      (if (= n len)
        (+ acc (* (- time cur-time) cur-rate))
        (let [a (nth vertices (- n 1))
              b (nth vertices n)
              dist (get-edge-weight graph a b)]
          (recur (+ cur-time dist)
                 (+ cur-rate (get-vertex-weight graph b))
                 (+ acc (* dist cur-rate))
                 (+ n 1)))))))

(defn reduce-all-permutations [fun init n]
  "Perform a reduce operation over arrays all permuations of the first n
integers. This uses Even's version of the Steinhaus-Johnson-Trotter algorithm."
  (let [arr (into-array Integer/TYPE (range n))
        directions (into-array Integer/TYPE (take n (cons 0 (repeat -1))))
        do-swap (fn [i j]
                  (let [tmp (aget arr i)
                        tmp-dir (aget directions i)]
                    (aset arr i (aget arr j))
                    (aset directions i (aget directions j))
                    (aset arr j tmp)
                    (aset directions j tmp-dir)))
        ;; determine the index of the maximum element with nonzero direction
        determine-max (fn []
                        (loop [i 0
                               m -1
                               m-index -1]
                          (cond
                            (= i n) [m m-index]

                            (= (aget directions i) 0)
                            (recur (+ i 1) m m-index)

                            (> (aget arr i) m)
                            (recur (+ i 1) (aget arr i) i)

                            :else
                            (recur (+ i 1) m m-index))))
        ;; update directions besides the chosen index
        update-directions (fn [chosen-element chosen-index]
                            (loop [i 0]
                              (cond
                                (= i chosen-index) nil

                                (> (aget arr i) chosen-element)
                                (do (aset directions i 1)
                                    (recur (+ i 1)))

                                :else (recur (+ i 1))))
                            (loop [i (inc chosen-index)]
                              (cond
                                (>= i n) nil

                                (> (aget arr i) chosen-element)
                                (do (aset directions i -1)
                                    (recur (+ i 1)))

                                :else (recur (+ i 1)))))]
    (loop [acc (fun init arr)]
      (let [[m m-index] (determine-max)]
        (if (= m-index -1)
          acc
          (let [dir (aget directions m-index)
                j (+ m-index dir)]
            (do-swap m-index j)
            (update-directions m m-index)
            (if (or (= j 0)
                    (= j (dec n))
                    (> (aget arr (+ j dir)) m))
              (aset directions j 0))

            (recur (fun acc arr))))))))

(defn two-path-pressure-release [graph time path1 path2]
  (+ (path-pressure-release graph time path1)
     (path-pressure-release graph time path2)))

(defn compute-part1 [graph]
  (let [cgraph (contract-graph graph)]
    (r-reduce-paths-upto-length #(max %1 (path-pressure-release cgraph 30 %2))
                                0
                                cgraph
                                30)))

(defn max-pressure-flow-with-fixed-path [graph path]
  (r-reduce-disj-paths-upto-length
   #(max %1 (two-path-pressure-release graph 26 path %2))
   0
   graph
   26
   path))

(defn compute-part2 [graph]
  (let [cgraph (contract-graph graph)]
    (r-reduce-paths-upto-length
     #(max %1 (max-pressure-flow-with-fixed-path cgraph %2))
     0
     cgraph
     26)))

(defn part1 [input]
  (-> input file-lines parse-graph compute-part1 println))

(defn part2 [input]
  (println "Hey, this part takes like 20 minutes to actually run.")
  (println "The answer for my input is 2752")
  (-> input file-lines parse-graph compute-part2 println))

(def test-cgraph (contract-graph test-graph))
(set! *unchecked-math* false)
