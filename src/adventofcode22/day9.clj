(ns adventofcode22.day9
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn sgn [x]
  (cond (> x 0) 1
        (= x 0) 0
        :else -1))

(defn move-head1 [head-pos dir]
  (let [[x y] head-pos]
    (case dir
      "L" [(- x 1) y]
      "D" [x (- y 1)]
      "U" [x (+ y 1)]
      "R" [(+ x 1) y])))

(defn move-succ1 [pre-pos succ-pos]
  "Move a successor one step to catch up with predecessor."
  (let [[px py] pre-pos
        [sx sy] succ-pos
        dx (- px sx)
        dy (- py sy)]
    (if (or (> (abs dx) 1)
            (> (abs dy) 1))
      [(+ sx (sgn dx)) (+ sy (sgn dy))]
      [sx sy])))

(defn move-head-record-tail [head-pos tail-pos record dir amt]
  (loop [head-pos head-pos
         tail-pos tail-pos
         record record
         n 0]
    (if (>= n amt)
      [head-pos tail-pos record]
      (let [new-hd (move-head1 head-pos dir)
            new-tl (move-succ1 new-hd tail-pos)]
        (recur new-hd new-tl (conj record new-tl) (+ n 1))))))

(defn apply-command [cmd head-pos tail-pos record]
  (let [[dir amt-str] (clojure.string/split cmd #" ")
        amt (Integer/parseInt amt-str)]
    (move-head-record-tail head-pos tail-pos record dir amt)))

(defn long-move1-record-tail [segments record dir]
  "Make one move of a rope consisting of a list of segments and record where the tail goes."
  (loop [acc [(move-head1 (first segments) dir)] ; start with just the head 
         old (rest segments)]
    (if (empty? old)
      [acc (conj record (peek acc))]
      (recur (conj acc (move-succ1 (peek acc) (first old)))
             (rest old)))))

(defn long-move-record-tail [segments record dir amt]
  (loop [segments segments
         record record
         n 0]
    (if (>= n amt)
      [segments record]
      (let [[news newr] (long-move1-record-tail segments record dir)]
        (recur news newr (+ n 1))))))

(defn apply-command-long [cmd segments record]
  (let [[dir amt-str] (clojure.string/split cmd #" ")
        amt (Integer/parseInt amt-str)]
    (long-move-record-tail segments record dir amt)))

(defn part1 [input]
  (as-> (file-lines input) $
    (reduce (fn [acc in]
              (let [[hd tl rec] acc]
                (apply-command in hd tl rec)))
            [[0 0] [0 0] #{[0 0]}]
            $)
    (nth $ 2)
    (count $)
    (println $)))

(defn part2 [input]
  (as-> (file-lines input) $
    (reduce (fn [acc in]
              (let [[segs rec] acc]
                (apply-command-long in segs rec)))
            [(repeat 10 [0 0]) #{[0 0]}]
            $)
    (nth $ 1)
    (count $)
    (println $)))
