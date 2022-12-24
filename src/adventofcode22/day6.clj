(ns adventofcode22.day6
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn starts-with-marker? [chs len]
  (= (count (set (take len chs))) len))

(defn find-first-marker [chs len]
    (loop [n len
           remaining chs]
      (if (or (starts-with-marker? remaining len)
              (empty? remaining))
        n
        (recur (+ n 1) (rest remaining)))))

(defn part1 [filename]
  (let [chs (with-open [in (clojure.java.io/reader filename)]
              (into '() (reverse (.readLine in))))]
    (println (find-first-marker chs 4))))

(defn part2 [filename]
  (let [chs (with-open [in (clojure.java.io/reader filename)]
              (into '() (reverse (.readLine in))))]
    (println (find-first-marker chs 14))))
