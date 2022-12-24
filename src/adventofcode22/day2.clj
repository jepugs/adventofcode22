(ns adventofcode22.day2
  (:require adventofcode22.common))

(refer 'adventofcode22.common)


(defn abc->num [ch]
  (- (int ch) (int \A)))
(defn xyz->num [ch]
  (- (int ch) (int \X)))

(defn compute-score [line]
  (let [elf (abc->num (nth line 0))
        you (xyz->num (nth line 2))]
    (cond
      (= you elf)                (+ 3 (inc you))
      (= you (mod (+ elf 1) 3))  (+ 6 (inc you))
      :else                      (inc you))))

(defn compute-score-part2 [line]
  (let [elf (abc->num (nth line 0))]
    (case (nth line 2)
      ;; lose
      \X (inc (mod (- elf 1) 3))
      ;; draw
      \Y (+ 3 (inc elf))
      ;; win
      \Z (+ 6 (inc (mod (+ elf 1) 3))))))

(defn part1 [input]
  (->> (file-lines input)
       (map compute-score)
       (reduce + 0)
       println))

(defn part2 [input]
  (->> (file-lines input)
       (map compute-score-part2)
       (reduce + 0)
       println))
