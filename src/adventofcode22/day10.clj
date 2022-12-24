(ns adventofcode22.day10
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn apply-command [line x cycles]
  (let [[cmd & args] (clojure.string/split line #" ")]
    (if (= cmd "addx")
      (let [amt (Integer/parseInt (first args))]
        [(+ x amt) (+ cycles 2)])
      [x (+ cycles 1)])))

(defn apply-commands [lines x cycles]
  (reduce (fn [acc line]
            (let [[x cycles] acc]
              (apply-command line x cycles)))
          [x cycles]
          lines))

(defn signal-strength [x cycles]
  (* x cycles))


(defn do-part1 [lines]
  (loop [x 1
         cycles 1
         lines lines
         acc 0]
    (if (or (empty? lines) (> cycles 220))
      acc
      (let [[new-x new-cycles] (apply-command (first lines) x cycles)
            ;; decide if it's a cycle we need to record
            sig-cycles (filter #(and (<= cycles %)
                                     (< % new-cycles))
                               [20 60 100 140 180 220])]
        (if (not (empty? sig-cycles))
          (recur new-x
                 new-cycles
                 (rest lines)
                 (+ acc (signal-strength x (first sig-cycles))))
          (recur new-x new-cycles (rest lines) acc))))))

(defn part1 [input]
  (do-part1 (file-lines input)))

(defn draw-pixel [cycle sprite-pos]
  (if (<= (abs (- (mod (- cycle 1) 40) sprite-pos))
          1)
    \#
    \.))

(defn do-part2-without-newlines [lines]
  (loop [x 1
         cycles 1
         lines lines
         acc []]
    (cond
      (or (empty? lines)) acc

      :else
      (let [[new-x new-cycles] (apply-command (first lines) x cycles)]
        (if (= (- new-cycles cycles) 2)
            (recur new-x
                   new-cycles
                   (rest lines)
                   (conj (conj acc (draw-pixel cycles x))
                         (draw-pixel (+ cycles 1) x)))
            (recur new-x
                   new-cycles
                   (rest lines)
                   (conj acc (draw-pixel cycles x))))))))

(defn do-part2 [lines]
  (->> lines
       do-part2-without-newlines
       (partition 40)
       (reduce #(println (apply str %2)) nil)))

(defn part2 [input]
  (do-part2 (file-lines input)))


(def test-lines (clojure.string/split-lines "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"))
