(ns adventofcode22.day1
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn count-calories [lines]
  (loop [lines lines
         acc []
         cur-count 0]
    (if (empty? lines)
      (conj acc cur-count)
      (if (empty? (first lines))
        (recur (rest lines) (conj acc cur-count) 0)
        (recur (rest lines)
               acc
               (+ (Integer/parseInt (first lines)) cur-count))))))

(defn part1 [input]
  (->> (file-lines input)
       count-calories
       (reduce max 0)
       println))

(defn part2 [input]
  (->> (file-lines input)
       count-calories
       (sort >)
       (take 3)
       (apply +)
       println))
