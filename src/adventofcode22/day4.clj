(ns adventofcode22.day4
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn parse-line [line]
  (map #(Integer/parseInt %) (clojure.string/split line #"[,-]")))

(defn one-is-subset [line]
  (let [[a b c d] (parse-line line)]
    (or (and (<= a c) (>= b d))
        (and (<= c a) (>= d b)))))

(defn process-line [line]
  (let [[a b c d] (parse-line line)]
    (and (<= a d)
         (<= c b))))

(defn part1 [filename]
  (->> (file-lines filename)
      (filter one-is-subset)
      count
      println))

(defn part2 [filename]
  (->> (file-lines filename)
      (filter process-line)
      count
      println))

