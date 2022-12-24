(ns adventofcode22.day3
  (:require adventofcode22.common))

(refer 'adventofcode22.common)


(defn find-repeated-item [line]
  (let [len (count line)
        [l r] (split-at (quot len 2) line)
        lset (set l)]
    (first (keep lset r))))

(defn ch->num [c]
  (let [i (int c)]
    (if (>= i (int \a))
      (inc (- i (int \a)))
      (+ 27 (- i (int \A))))))

(defn part1 [input]
  (->> (file-lines input)
       (map find-repeated-item)
       (map ch->num)
       (reduce + 0)
       println))

(defn do-part2 [lines]
  (loop [lines lines
         acc 0]
    (if (empty? lines)
      acc
      (let [[[a b c] tl] (split-at 3 lines)
            s (clojure.set/intersection (set b) (set c))
            priority (ch->num (first (keep s a)))]
        (recur tl (+ priority acc))))))

(defn part2 [input]
  (->> (file-lines input) do-part2 println))
