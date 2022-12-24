(ns adventofcode22.day20
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn mix-index [nums index-seq i len]
  (let [true-index (find-index #(= % i) index-seq)
        elt (nth nums true-index)
        j (mod (+ true-index elt) (- len 1))]
    (cond (= true-index j) [nums index-seq]
          (< true-index j)
          (let [[before at-and-after] (split-at true-index nums)
                after (rest at-and-after)
                [ibefore iat-and-after] (split-at true-index index-seq)
                iafter (rest iat-and-after)
                [front back] (split-at (- j true-index) after)
                [ifront iback] (split-at (- j true-index) iafter)]
            [(into [] (concat before front (cons elt back)))
             (into [] (concat ibefore ifront (cons i iback)))])
          (> true-index j)
          (let [[before at-and-after] (split-at true-index nums)
                after (rest at-and-after)
                [ibefore iat-and-after] (split-at true-index index-seq)
                iafter (rest iat-and-after)
                [front back] (split-at j before)
                [ifront iback] (split-at j ibefore)]
            [(into [] (concat front (cons elt back) after))
             (into [] (concat ifront (cons i iback) iafter))]))))

(defn mix-once [nums]
  (let [len (count nums)]
    (loop [nums nums
           index-seq (into [] (range len))
           i 0]
      (if (= i len)
        nums
        (let [[new-nums new-index-seq] (mix-index nums index-seq i len)]
          (recur new-nums new-index-seq (+ i 1)))))))

(defn mix-repeatedly [nums n]
  (let [len (count nums)
        total (* len n)]
    (loop [nums nums
           index-seq (into [] (range len))
           i 0
           ct 0]
      (if (= ct total)
        nums
        (let [[new-nums new-index-seq] (mix-index nums index-seq i len)]
          (recur new-nums
                 new-index-seq
                 (mod (+ i 1) len)
                 (+ ct 1)))))))

(defn find-coordinates [mixed-nums]
  (let [i (find-index #(= % 0) mixed-nums)
        cyc (cycle mixed-nums)]
    (map #(nth cyc (+ i %)) [1000 2000 3000])))

(def test-list '(1 2 -3 3 -2 0 4))

(defn do-part1 [nums]
  (->> (mix-once nums)
       find-coordinates
       (reduce + 0)))

(defn part1 [input]
  (->> (file-lines input)
       (map #(Integer/parseInt %))
       do-part1
       println))

(defn do-part2 [nums]
  (as-> nums $
    (map #(* 811589153 %) $)
    (mix-repeatedly $ 10)
    (find-coordinates $)
    (reduce + 0 $)))

(defn part2 [input]
  (->> (file-lines input)
       (map #(Integer/parseInt %))
       do-part2
       println))
