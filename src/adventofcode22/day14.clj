(ns adventofcode22.day14
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(def test-lines (clojure.string/split-lines "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"))

(defn extract-paths [lines]
  (let [inner (comp (map clojure.string/trim)
                    (map #(clojure.string/split % #","))
                    (map #(mapv (fn [x] (Integer/parseInt x)) %)))
        xform (comp (map #(clojure.string/split % #"->"))
                    (map #(into [] inner %)))]
    (into [] xform lines)))

(defn x-coords [paths]
  (flatten (mapv #(mapv first %) paths)))

(defn y-coords [paths]
  (flatten (mapv #(mapv second %) paths)))

(defrecord cavemap [array x-min x-max y-max])

(defn caveref [cavemap x y]
  (let [{:keys [array x-min]} cavemap]
    (nth (nth array y) (- x x-min))))

(defn add-line-points [set start stop]
  (let [[sx sy] start
        [tx ty] stop]
    (if (= sx tx)
      (reduce #(conj %1 [sx %2])
              set
              (range (min sy ty) (+ (max sy ty) 1)))
      ;; here sy must equal ty by constraints on the input
      (reduce #(conj %1 [%2 sy])
              set
              (range (min sx tx) (+ (max sx tx) 1))))))

(defn add-path-points [set path]
  (loop [prev (first path)
         remaining (rest path)
         acc (conj set (first path))]
    (if (empty? remaining)
      acc
      (recur (first remaining)
             (rest remaining)
             (add-line-points acc prev (first remaining))))))

(defn all-rocks [paths]
  (reduce add-path-points #{} paths))

(defn construct-cavemap [lines]
  (let [paths (extract-paths lines)
        xs (x-coords paths)
        x-min (reduce min (x-coords paths))
        x-max (reduce max (x-coords paths))
        y-max (reduce max (y-coords paths))
        rock-locs (all-rocks paths)
        array (mapv (fn [y]
                      (mapv (fn [x]
                              (if (contains? rock-locs [x y])
                                1
                                0))
                            (range x-min (+ x-max 1))))
                    (range 0 (+ y-max 1)))]
    (new cavemap array x-min x-max y-max)))

(defn construct-wide-cavemap [lines]
  "Widen a cavemap for part2, so that we will have enough room in the map to
model all the falling sand. This is a bit of a hack, but it should work fine.
The observation is that sand can move at most one square left or right per
square it falls."
  (let [paths (extract-paths lines)
        xs (x-coords paths)
        y-max (+ 1 (reduce max (y-coords paths)))
        x-min (- (reduce min xs) y-max)
        x-max (+ (reduce max xs) y-max)
        rock-locs (all-rocks paths)
        array (mapv (fn [y]
                      (mapv (fn [x]
                              (if (contains? rock-locs [x y])
                                1
                                0))
                            (range x-min (+ x-max 1))))
                    (range 0 (+ y-max 1)))]
    (new cavemap array x-min x-max y-max)))

(def test-map (construct-cavemap test-lines))
(def wide-test-map (construct-wide-cavemap test-lines))

(defn in-bounds? [cavemap x y]
  (let [{:keys [x-min x-max y-max]} cavemap]
    (and (<= x-min x)
         (>= x-max x)
         (<= 0 y)
         (>= y-max y))))

(defn sand-move [cavemap x y]
  "Determine where a piece of sand should move. Returns the same [x y] if the
 sand cannot move and returns nil if the sand would fall off the map"
  (let [incy (+ y 1)
        incx (+ x 1)
        decx (- x 1)]
    (cond
      (not (in-bounds? cavemap x incy)) nil
      (= (caveref cavemap x incy) 0) [x incy]
      (not (in-bounds? cavemap decx incy)) nil
      (= (caveref cavemap decx incy) 0) [decx incy]
      (not (in-bounds? cavemap incx incy)) nil
      (= (caveref cavemap incx incy) 0) [incx incy]
      :else [x y])))

(defn sand-move-part2 [cavemap x y]
  "Like sand-move, but stops the sand when it reaches the bottom"
  (let [incy (+ y 1)
        incx (+ x 1)
        decx (- x 1)
        {:keys [x-min x-max y-max]} cavemap]
    (cond
      (> incy y-max) [x y]
      (= (caveref cavemap x incy) 0) [x incy]

      (and (>= decx x-min)
           (= (caveref cavemap decx incy) 0))
      [decx incy]

      (and (<= incx x-max)
           (= (caveref cavemap incx incy) 0))
      [incx incy]

      :else [x y])))

(defn sand-simulate [cavemap]
  (loop [x 500
         y 0]
    (let [new-coords (sand-move cavemap x y)]
      (cond (nil? new-coords) nil
            (= new-coords [x y]) new-coords
            :else (recur (nth new-coords 0)
                         (nth new-coords 1))))))

(defn sand-simulate-part2 [cavemap]
  (loop [x 500
         y 0]
    (let [new-coords (sand-move-part2 cavemap x y)]
      (if (= new-coords [x y])
        new-coords
        (recur (nth new-coords 0)
               (nth new-coords 1))))))

(defn update-array [array x y new-item]
  "Update an element in a vec of vecs. This supposes that y is the row number
and x is the column number."
  (letfn [(update-item [rownum row]
            (if (= rownum y)
              (into [] (map-indexed (fn [colnum item]
                                      (if (= colnum x)
                                        new-item
                                        item)))
                    row)
              row))]
    (into [] (map-indexed update-item) array)))

(defn add-sand [cavemap x y]
  (assoc cavemap
         :array
         (update-array (:array cavemap) (- x (:x-min cavemap)) y 2)))

(defn count-sand [cavemap]
  (loop [ct 0
         updated-map cavemap]
    (let [new-sand (sand-simulate updated-map)]
      (if (nil? new-sand)
        ct
        (recur (+ ct 1)
               (add-sand updated-map (first new-sand) (second new-sand)))))))

(defn part1 [input]
  (-> (file-lines input)
      construct-cavemap
      count-sand
      println))

(defn count-sand-part2 [cavemap]
    (loop [ct 0
           updated-map cavemap]
      (let [[x y] (sand-simulate-part2 updated-map)]
        (if (and (= x 500) (= y 0))
          (+ ct 1)
          (recur (+ ct 1)
                 (add-sand updated-map x y))))))

(defn part2 [input]
  (-> (file-lines input)
      construct-wide-cavemap
      count-sand-part2
      println))
