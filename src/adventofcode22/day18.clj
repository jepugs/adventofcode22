(ns adventofcode22.day18
  (:require adventofcode22.common)
  (:require clojure.set))

(refer 'adventofcode22.common)

(def test-lines (clojure.string/split-lines "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"))

(defn parse-vec [line]
  (mapv #(Integer/parseInt %) (clojure.string/split line #",")))

(defn neighbors [v]
  (map #(v+ % v)
       [[1 0 0] [-1 0 0]
        [0 1 0] [0 -1 0]
        [0 0 1] [0 0 -1]]))

(defn count-sides [vecs]
  (let [all (into #{} vecs)]
    (reduce (fn [acc in]
              (+ (- 6 (count (filter all (neighbors in))))
                 acc))
            0
            vecs)))

(defn part1 [input]
  (->> input
       file-lines
       (map parse-vec)
       count-sides
       println))

(defn outside-of-bounds? [v min-bounds max-bounds]
  (some identity
        (map #(or (< %3 %1)
                  (> %3 %2))
             min-bounds
             max-bounds
             v)))

(defn get-bounds [vecs]
  "Returns [min-bounds max-bounds]."
  ;; Yes I know this is a stupid way to do this. Doing the following line causes
  ;; a stack overflow. It must be a bug.
  ;; [(reduce #(mapv min %1 %2) vecs) (reduce #(mapv max %1 %2) vecs)]
  [(reduce #(mapv min %1 %2) vecs) (reduce #(mapv max %1 %2) vecs)])

(defn search-for-path-out [enclosed-points exterior-points v min-bounds max-bounds]
  "Returns [FOUND-PATH? CHECKED], where POINTS-CHECKED is a set containing all
points already checked in the algorithm. enclosed-points is a set of all points
which are either known to be enclosed or are solid. v is the point to check.
min-bounds and max-bounds are vectors of the min/max xyz values."
  (loop [points-to-check #{v}
         points-checked #{}]
    (cond
      (empty? points-to-check)
      [nil points-checked]

      (some #(outside-of-bounds? % min-bounds max-bounds) points-to-check)
      [true points-checked]

      (some exterior-points points-to-check)
      [true points-checked]

      :else
      (let [all-neighbors (mapcat neighbors points-to-check)
            to-check (filter #(not (or (points-checked %)
                                       (enclosed-points %)))
                             all-neighbors)]
        (recur (set to-check)
               (clojure.set/union points-checked points-to-check))))))

(defn generate-points-in-cube [min-bounds max-bounds]
  (let [[minx miny minz] min-bounds
        [maxx maxy maxz] max-bounds]
    (mapcat (fn [x]
              (mapcat (fn [y]
                        (map (fn [z]
                               [x y z])
                             (range minz (inc maxz))))
                      (range miny (inc maxy))))
            (range minx (inc maxx)))))

(defn do-part2 [lines]
  (let [vecs (map parse-vec lines)
        [min-bounds max-bounds] (get-bounds vecs)]
    (loop [pts (generate-points-in-cube min-bounds max-bounds)
           solid (set vecs)
           exterior #{}
           checked (set vecs)]
      (if (empty? pts)
        (count-sides solid)
        (let [[hd & tl] pts]
          (if (or (checked hd) (solid hd))
            (recur tl solid exterior checked)
            (let [[res new-checked]
                  (search-for-path-out solid
                                       exterior
                                       hd
                                       min-bounds
                                       max-bounds)]
              (if res
                (recur tl
                       solid
                       (clojure.set/union new-checked exterior)
                       (clojure.set/union new-checked checked))
                (recur tl
                       (clojure.set/union new-checked solid)
                       exterior
                       (clojure.set/union new-checked checked))))))))))

(defn part2 [input]
  (-> input file-lines do-part2 println))
