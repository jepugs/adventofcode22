(ns adventofcode22.day23
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn propose-dir [pos elf-set dir-order]
  (let [[nw n ne w e sw s se]
        (map #(elf-set (v+ % pos))
             [[-1  1] [0  1] [1  1]
              [-1  0] [1  0]
              [-1 -1] [0 -1] [1 -1]])]
    (if (or nw n ne w e sw s se)
      (loop [dirs (take 4 dir-order)]
        (case (first dirs)
          :north
          (if (not (or nw n ne))
            (v+ pos [0 1])
            (recur (rest dirs)))
          :south
          (if (not (or sw s se))
            (v+ pos [0 -1])
            (recur (rest dirs)))
          :west
          (if (not (or nw w sw))
            (v+ pos [-1 0])
            (recur (rest dirs)))
          :east
          (if (not (or ne e se))
            (v+ pos [1 0])
            (recur (rest dirs)))
          nil)))))

(defn elf-bounds [elf-vec]
  "Returns [min-bounds max-bounds]."
  [(reduce #(mapv min %1 %2) elf-vec)
   (reduce #(mapv max %1 %2) elf-vec)])

(defn print-elfs [elf-vec]
  (let [[[minx miny] [maxx maxy]] (elf-bounds elf-vec)
        elf-set (set elf-vec)]
    (loop [y maxy]
      (if (>= y miny)
        (do (loop [x minx]
              (if (<= x maxx)
                (do (if (elf-set [x y])
                      (print \#)
                      (print \.))
                    (recur (inc x)))))
            (println)
            (recur (dec y)))
        (println)
        ))))

(def init-dir-order (cycle [:north :south :west :east]))

(defn count-proposals [props]
  (reduce #(if (%1 %2)
             (assoc %1 %2 (+ (%1 %2) 1))
             (assoc %1 %2 1))
          {}
          props))

(defn step-elfs [elf-vec dirs]
  (let [elf-set (set elf-vec)
        proposals (mapv #(propose-dir % elf-set dirs) elf-vec)
        prop-map (count-proposals proposals)]
    (mapv #(if (= (prop-map %2) 1)
             %2
             %1)
          elf-vec
          proposals)))

(defn step-elfs-check-stopped [elf-vec dirs]
  (let [elf-set (set elf-vec)
        proposals (mapv #(propose-dir % elf-set dirs) elf-vec)
        prop-map (count-proposals proposals)
        new-pos (mapv #(if (= (prop-map %2) 1)
                         %2
                         %1)
                      elf-vec
                      proposals)]
    (if (= new-pos elf-vec)
      [new-pos true]
      [new-pos false])))

(defn simulate-elfs [elf-vec steps dirs]
  (loop [i 0
         dirs dirs
         elf-vec elf-vec]
    ;;(print-elfs elf-vec)
    (if (not= i steps)
      (recur (inc i)
             (drop 1 dirs)
             (step-elfs elf-vec dirs))
      (do ;;(print-elfs elf-vec)
          elf-vec))))

(defn simulate-elfs-to-stop [elf-vec dirs]
  (loop [i 0
         dirs dirs
         elf-vec elf-vec]
    ;;(print-elfs elf-vec)
    (let [[new-vec stopped] (step-elfs-check-stopped elf-vec dirs)]
      (if stopped
        (inc i)
        (recur (inc i)
               (drop 1 dirs)
               new-vec)))))

(def test-elfs-1 [[2 1] [3 1] [2 3] [2 4] [3 4]])

(def test-lines (clojure.string/split-lines ".......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......"))

(defn parse-elfs [lines]
  (loop [acc []
         y 0
         lines (reverse lines)]
    (if (empty? lines)
      acc
      (let [new-acc
            (loop [acc acc
                   x 0
                   text (first lines)]
              (if (empty? text)
                acc
                (recur (if (= (first text) \#)
                         (conj acc [x y])
                         acc)
                       (inc x)
                       (rest text))))]
        (recur new-acc (inc y) (rest lines))))))

(def test-elfs-2 (parse-elfs test-lines))

(defn count-empty-spaces [elf-vec]
  (let [[[minx miny] [maxx maxy]] (elf-bounds elf-vec)
        elf-set (set elf-vec)]
    (reduce (fn [acc x]
              (+ acc
                 (reduce (fn [acc y]
                           (if (elf-set [x y])
                             acc
                             (+ acc 1)))
                         0
                         (range miny (inc maxy)))))
            0
            (range minx (inc maxx)))))


(defn do-part1 [lines]
  (-> (parse-elfs lines)
      (simulate-elfs 10 init-dir-order)
      count-empty-spaces))

(defn part1 [input]
  (->  (file-lines input)
       do-part1
       println))

(defn do-part2 [lines]
  (-> (parse-elfs lines)
      (simulate-elfs-to-stop init-dir-order)))

(defn part2 [input]
  (-> (file-lines input)
      do-part2
      println))
