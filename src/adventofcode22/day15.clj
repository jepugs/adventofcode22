(ns adventofcode22.day15
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn l1-dist [ax ay bx by]
  (+ (abs (- ax bx))
     (abs (- ay by))))


(defn just-numbers-plz [str]
  (into []
        (comp (filter #(not= % ""))
              (map #(Integer/parseInt %)))
        (clojure.string/split str #"[^\-0-9]+")))

(defrecord sensor-data [sx sy bx by dist])

(defn parse-sensor [line]
  (let [[sx sy bx by] (just-numbers-plz line)
        dist (l1-dist sx sy bx by)]
    (new sensor-data sx sy bx by dist)))

(defn sensor-excludes? [sensor x y]
  (let [{:keys [sx sy bx by dist]} sensor]
    (and (<= (l1-dist sx sy x y) dist)
         ; this second condition handles the case that a beacon is placed
         ; exactly at x, y. This does not occur in the actual input but I feel
         ; like I should handle it anyway.
         (or (not= x bx)
             (not= y by)))))

(defn cannot-have-beacon? [sensors x y]
  (some #(sensor-excludes? % x y) sensors))

(def test-lines (clojure.string/split-lines "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"))

(def test-sensors (map parse-sensor test-lines))

(defn determine-x-range [sensors]
  (let [xs (map :sx sensors)
        max-x (reduce max (map #(+ (:sx %) (:dist %)) sensors))
        min-x (reduce min (map #(- (:sx %) (:dist %)) sensors))]
    [min-x max-x]))

(defn xdist-to-sensor [sensor x y]
  "How many units in the positive x direction we'd have to move to be in range
of the sensor. Returns nil if no such movement is possible, or if the movement
would require moving backwards. (This behavior is because the map is scanned
left to right)."
  (let [{:keys [sx sy dist]} sensor]
    (if (> (abs (- y sy)) dist)
      nil
      (let [total-dist (l1-dist x y sx sy)]
        (if (and (> x sx) (>= total-dist dist))
          nil
          (max (- total-dist dist) 0))))))

(defn xdist-out-of-sensor [sensor x y]
  "How many units in the positive x direction we'd have to move to get out of
range of a sensor."
  (- (:dist sensor)
     (- x (:sx sensor))
     (abs (- y (:sy sensor)))
     -1))

(defn xdist-to-next-sensor [sensors x y]
  "Returns a vector [next-sensor x-dist]."
  (loop [sensors sensors
         min-sensor nil
         min-dist nil]
    (if (empty? sensors)
      [min-sensor min-dist]
      (let [[hd & tl] sensors
            dist (xdist-to-sensor hd x y)]
        (if (and dist
                 (or (not min-dist) (< dist min-dist)))
          (recur tl hd dist)
          (recur tl min-sensor min-dist))))))

(defn count-beacons-at-y [sensors y]
  (->> sensors
       (filter #(= (:by %) y))
       (map #(vector (:bx %) (:by %)))
       (into #{})
       count))

(defn do-part1 [lines y]
  (let [sensors (map parse-sensor lines)]
    (loop [x (first (determine-x-range sensors))
           acc 0]
      (let [[next-sensor dist] (xdist-to-next-sensor sensors x y)]
        (if next-sensor
          (let [dist-out (xdist-out-of-sensor next-sensor (+ x dist) y)]
            (recur (+ x dist dist-out)
                   (+ acc dist-out)))
          ;; subtract the beacons in this row
          (- acc 0
             (count-beacons-at-y sensors y)))))))

(defn part1 [input]
  (println (do-part1 (file-lines input) 2000000)))

(defn part2-scan-row [sensors y]
  (loop [x 0]
    (let [[next-sensor dist] (xdist-to-next-sensor sensors x y)]
      (cond
        (not next-sensor) nil
        (> dist 0) x
        :else (let [dist-out (xdist-out-of-sensor next-sensor x y)]
                (recur (+ x dist-out)))))))

(defn part2 [input]
  (let [sensors (->> input file-lines (map parse-sensor))]
    (loop [y 0]
      (let [res (part2-scan-row sensors y)]
        (if res
          (do (println [res y])
              (println (+ (* res 4000000) y)))
          (recur (+ y 1)))))))
