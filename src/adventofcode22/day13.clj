(ns adventofcode22.day13
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(declare compare-packets)

(defn compare-list-packets [l r]
  (cond (empty? l) (if (empty? r) 0 -1)
        (empty? r) 1
        :else (case (compare-packets (first l) (first r))
                -1 -1
                1 1
                (compare-list-packets (rest l) (rest r)))))

(defn compare-packets [l r]
  "Returns true if l and r are in the right order, false otherwise."
  (cond (coll? l) (if (coll? r)
                    (compare-list-packets l r)
                    (compare-list-packets l [r]))
        (coll? r) (compare-list-packets [l] r)
        :else (compare l r)))

(defn parse-list [line]
  "lmao this is good enough"
  (read-string (clojure.string/replace line "," " ")))

(defn in-right-order? [l r]
  (<= (compare-packets l r) 0))

(defn correct-ordered-indices [lines]
  (let [xform (comp (filter #(not= "" %))
                    (map parse-list)
                    (partition-all 2)
                    (map-indexed (fn [i [l r]]
                                   (if (in-right-order? l r)
                                     (+ i 1)
                                     0)))
                    (filter #(not= 0 %)))]
    (into [] xform lines)))

(defn get-all-lists [lines]
  (into []
        (comp (filter #(not= "" %))
              (map parse-list))
        lines))

(def test-lines (clojure.string/split-lines "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"))

(defn part1 [input]
  (->> (file-lines input)
       correct-ordered-indices
       (reduce + 0)
       println))

(defn sorted-packets-with-dividers [lines]
    (->> lines
         get-all-lists
         (concat [[[2]] [[6]]])
         (sort compare-packets)))

(defn part2 [input]
  (let [sorted (sorted-packets-with-dividers (file-lines input))
        i1 (+ 1 (.indexOf sorted [[2]]))
        i2 (+ 1 (.indexOf sorted [[6]]))]
    (println (* i1 i2))))
