(ns adventofcode22.day12
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defrecord heightmap [array start end])

(defn char-to-height [c]
  (- (int c) (int \a)))

(defn read-heightmap-row [line rownum width]
  (loop [colnum 0
         acc []
         start nil
         end nil]
    (if (>= colnum width)
      [acc start end]
      (let [ch (nth line colnum)]
        (case ch
          \S (recur (+ colnum 1)
                    (conj acc 0)
                    [rownum colnum]
                    end)
          \E (recur (+ colnum 1)
                    (conj acc 25)
                    start
                    [rownum colnum])
          (recur (+ colnum 1)
                 (conj acc (char-to-height ch))
                 start
                 end))))))

(defn construct-heightmap [lines]
  (let [width (count (first lines))]
    (loop [rownum 0
           lines lines
           array []
           start nil
           end nil]
      (if (empty? lines)
        (new heightmap array start end)
        (let [[row new-start new-end]
              (read-heightmap-row (first lines) rownum width)]
          (recur (+ rownum 1)
                 (rest lines)
                 (conj array row)
                 (or start new-start)
                 (or end new-end)))))))

(def test-in (clojure.string/split-lines "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"))

(defn hmref [heightmap row col]
  (-> (:array heightmap)
      (nth row)
      (nth col)))

(def test-map (construct-heightmap test-in))

(defn traversable-neighbors [heightmap row col]
  (let [all-neighbors [[(+ row 1) col]
                       [(- row 1) col]
                       [row (+ col 1)]
                       [row (- col 1)]]
        {:keys [array]} heightmap]
    (filter (fn [[r c]]
              (and (>= r 0)
                   (>= c 0)
                   (< r (count array))
                   (< c (count (first array)))
                   (<= (hmref heightmap r c)
                       (+ (hmref heightmap row col) 1))))
            all-neighbors)))

(defn shortest-path [heightmap]
  (let [{:keys [array start end]} heightmap]
    (loop [n 0
           endpoints [start]
           visited #{start}]
      (if (some #(and (= (first %) (first end))
                      (= (second %) (second end)))
                endpoints)
        n
        (if (empty? endpoints)
          1000000000
          (let [neighbors-list (map #(traversable-neighbors heightmap
                                                            (first %)
                                                            (second %))
                                    endpoints)
                all-neighbors (distinct (apply concat neighbors-list))
                unvisited-neighbors
                (filter #(not (contains? visited [(first %) (second %)]))
                        all-neighbors)]
            (recur (+ n 1)
                   unvisited-neighbors
                   (into visited unvisited-neighbors))))))))

(defn part1 [input]
  (-> (construct-heightmap (file-lines input))
      shortest-path
      println))

(defn all-lowest-points [heightmap]
  (let [{:keys [array]} heightmap
        width (count (first array))
        height (count array)]
    (loop [row 0
           acc []]
      (if (= row height)
        acc
        (let [new-pts
              (loop [col 0
                     acc []]
                (if (< col width)
                  (recur (+ col 1)
                         (if (= (hmref heightmap row col) 0)
                           (conj acc [row col])
                           acc))
                  acc))]
          (recur (+ row 1)
                 (concat acc new-pts)))))))

(defn shortest-path-from [heightmap start]
  (shortest-path (assoc heightmap :start start)))

(defn shortest-path-part2 [heightmap]
  (reduce min
          (map #(shortest-path-from heightmap %)
               (all-lowest-points heightmap))))

(defn part2 [input]
  (-> (construct-heightmap (file-lines input))
      shortest-path-part2
      println))
