(ns adventofcode22.day8
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn transpose [m]
  (apply mapv
         (fn [& xs]
           (into [] xs))
         m))

(defn hflip [m]
  (mapv #(into [] (reverse %)) m))

(defn filter-visible-from-left [m]
  (mapv (fn [row]
          (loop [in row
                 acc []
                 highest -1]
            (if
              (empty? in) acc
              (let [[hd & tl] in]
                (if (> hd highest)
                  (recur tl (conj acc true) hd)
                  (recur tl (conj acc false) highest))))))
        m))

(defn offdiag-transpose [m]
  (-> m hflip transpose hflip))

(defn dim [m]
  [(count m)
   (if (empty? m)
     0
     (count (peek m)))])

(defn mref [m i j]
  (nth (nth m i) j))

(defn zip-matrices [zip-fun & mats]
  (apply mapv
         (fn [& rows]
           (apply mapv
                  (fn [& args] (apply zip-fun args))
                  rows))
         mats))

(defn filter-visible [m]
  (let [left (filter-visible-from-left m)
        top (-> m transpose filter-visible-from-left transpose)
        right (-> m hflip filter-visible-from-left hflip)
        bot (-> m offdiag-transpose filter-visible-from-left offdiag-transpose)]
    (zip-matrices (fn [& xs] (some identity xs)) left top right bot)))

(defn scenic-rank-right-at-point [m i j]
  "Get the number of spaces visible from a tree i,j on its right."
  (let [h (mref m i j)
        [_ cols] (dim m)]
    (loop [acc 0
           col (+ j 1)]
      (cond (>= col cols) acc
            (< (mref m i col) h) (recur (+ acc 1) (+ col 1))
            :else (+ acc 1)))))

(defn scenic-rank-right [m]
  (let [[rows cols] (dim m)]
    (mapv (fn [i]
            (mapv #(scenic-rank-right-at-point m i %)
                  (range 0 cols)))
          (range 0 rows))))

(defn scenic-ranks [m]
  (let [right (scenic-rank-right m)
        bot (-> m transpose scenic-rank-right transpose)
        left (-> m hflip scenic-rank-right hflip)
        top (-> m offdiag-transpose scenic-rank-right offdiag-transpose)]
    (zip-matrices * left top right bot)))

(defn read-map [lines]
  (mapv (fn [l]
          (mapv #(Integer/parseInt (str %)) l))
        lines))

(defn write-map [m]
  (reduce #(println (apply str %2)) nil m))

(defn part1 [input]
  (->> input
       file-lines
       read-map
       filter-visible
       (apply concat)
       (reduce #(if %2 (+ %1 1) %1) 0)
       println))

(def test-lines (clojure.string/split-lines
"30373
25512
65332
33549
35390"))
(def test-map (read-map test-lines))

(defn part2 [input]
  (->> input
       file-lines
       read-map
       scenic-ranks
       (apply concat)
       (apply max)
       println))
