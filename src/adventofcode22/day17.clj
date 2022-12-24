(ns adventofcode22.day17
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(set! *unchecked-math* true)

(def shapes
  [;; minus
   [[0 0] [1 0] [2 0] [3 0]]
   ;; plus
   [[1 0] [0 1] [1 1] [2 1] [1 2]]
   ;; right angle
   [[0 0] [1 0] [2 0] [2 1] [2 2]]
   ;; vertical line
   [[0 0] [0 1] [0 2] [0 3]]
   ;; square
   [[0 0] [1 0] [0 1] [1 1]]
   ])

;; dirs is an (infinite) lazy sequence of directions, dirs-consumed is the
;; number of directions used up by the simulation so far, and ct is the number
;; of rocks dropped.
(defrecord game [solid top num-dirs dirs dirs-consumed ct])

(defn highest-y [shape]
  (if (empty? shape)
    0
    (reduce max (map #(nth % 1) shape))))

(defn init-game [directions-unlooped]
  (new game
       #{}
       0
       (count directions-unlooped)
       (cycle directions-unlooped)
       0
       0))

(defn print-game [game falling]
  (let [{:keys [solid top]} game]
    (loop [n (max (highest-y falling) top)]
      (if (> n 0)
        (do
          (print \|)
          (->> (range 7)
               (map (fn [i]
                      (cond (solid [i n]) \#
                            (some #(= % [i n]) falling) \@
                            :else \.)))
              (apply str)
              print)
          (println \|)
          (recur (- n 1)))
        (println "+-------+")))))

(defn make-falling-rocks [shape top]
  "Create rock with the given shape to the top."
  (mapv #(v+ [2 (+ top 4)] %) shape))

(defn update-falling [falling by]
  "Move falling rocks by the specified amount. Does not check collisions.
Returns the new falling rocks so collisions can be checked."
  (mapv #(v+ by %) falling))

(defn solidify-falling [falling game]
  "Make the falling rock into solid spaces."
  (let [{:keys [solid top]} game]
    (assoc game
           :solid (into solid falling)
           :top (max top (highest-y falling)))))

(defn direction-vector [dir]
  (case dir
    \< [-1 0]
    \> [1 0]
    (println "dir is" dir)))

(defn out-of-bounds? [v]
  "Check if a point is out of bounds of the vector."
  (let [[x y] v]
    (or (<= y 0)
        (< x 0)
        (> x 6))))

(defn check-collision? [game new-pos]
  "Returns true if the new-pos collides with a wall or a solid rock."
  (let [solid (:solid game)]
    (some (fn [x]
            (or (solid x)
                (out-of-bounds? x)))
          new-pos)))

(def ^:dynamic *print-simulation* false)

(defn simulate-rock [game]
  (let [{:keys [dirs ct top]} game
        shape (nth shapes (mod ct (count shapes)))
        falling (make-falling-rocks shape top)]
   (if *print-simulation*
     (do (print-game game falling)
         (println)))
   (loop [falling falling
          consumed 0
          dirs dirs]
     (let [[hd & tl] dirs
           blown-rock (update-falling falling (direction-vector hd))
           updated-falling (if (check-collision? game blown-rock)
                             falling
                             blown-rock)
           dropped (update-falling updated-falling [0 -1])]
       (if *print-simulation*
         (do (print-game game updated-falling)
             (println)))
       (if (check-collision? game dropped)
         (let [updated-game
               (assoc game
                      :dirs tl
                      :dirs-consumed (+ consumed 1 (:dirs-consumed game))
                      :ct (+ 1 (:ct game)))]
           (solidify-falling updated-falling updated-game))
         (recur dropped (+ consumed 1) tl))))))

(def test-directions-unlooped (sequence ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))
(def test-directions (cycle test-directions-unlooped))

(defn simulate-n-rocks [game n]
  (loop [i 0
         acc 0
         game game]
    (if (= i n)
      game 
      (let [next-shape (nth shapes (mod (:ct game) (count shapes)))
            new-game (simulate-rock game)]
        (recur (+ i 1) 0 new-game)))))

(defn simulate-game-seq [directions-unlooped]
  (iterate simulate-rock (init-game directions-unlooped)))

(defn part1 [input]
  (with-bindings {(var *print-simulation*) nil}
    (let [directions (first (file-lines input))]
      (-> (simulate-n-rocks (init-game directions) 2022)
          :top
          println))))

(defn exhaust-directions [game-seq]
  "Iterate over a sequence of games and return the index of the one where all
directions have been used once."
  (let [num-dirs (:num-dirs (first game-seq))]
    (find-index #(>= (:dirs-consumed %) num-dirs) game-seq)))

(defn find-loop [game-seq start-from]
  "Finds two game states where both the shape being dropped and the index of the
direction are the same. Returns [START LEN] where START is index of the game at
the beginning of the loop and LEN is the length of the loop. start-from
specifies where to start the search."
  (let [game-seq (drop start-from game-seq)
        num-shapes (count shapes)
        num-dirs (:num-dirs (first game-seq))]
    (loop [i start-from
           game-seq game-seq
           moduli {}]
      (let [[hd & tl] game-seq
            shape-mod (mod (:ct hd) num-shapes)
            dir-mod (mod (:dirs-consumed hd) num-dirs)
            [old-shape-mod j] (moduli dir-mod)]
        (if (= old-shape-mod shape-mod)
          [j (- i j)]
          (recur (+ i 1)
                 tl
                 (assoc moduli dir-mod [shape-mod i])))))))

(defn top-after-n [n directions-unlooped]
  (-> (simulate-n-rocks (init-game directions-unlooped) n)
      :top))

(defn do-part2 [directions-unlooped]
  (with-bindings {(var *print-simulation*) nil}
    (let [game-seq (simulate-game-seq directions-unlooped)
          start-from (exhaust-directions game-seq)
          [loop-start loop-len] (find-loop game-seq start-from)
          loop-end (+ loop-start loop-len)

          q (quot (- 1000000000000 loop-start) loop-len)
          r (mod (- 1000000000000 loop-start) loop-len)

          initial (:top (nth game-seq loop-start))
          per-loop (- (:top (nth game-seq loop-end))
                      initial)
          extra (- (:top (nth game-seq (+ loop-end r)))
                   (:top (nth game-seq loop-end)))]
      (+ (* q per-loop) initial extra))))

(defn part2 [input]
  (-> (file-lines input)
      first
      do-part2
      println))

(set! *unchecked-math* false)

