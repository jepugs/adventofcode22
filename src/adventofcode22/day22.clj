(ns adventofcode22.day22
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(def test-input (clojure.string/split-lines
                 "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"))

(defrecord gamestate [arr pos dir instr instr-idx])

(defn str->gamestate-arr [lines]
  (mapv (fn [l]
          (mapv #(case %
                   \. 0
                   \# 1
                   -1)
                l))
        lines))

(defn str->instr [s]
  "Parse a string of instructions (directions to the passcode)."
  (loop [acc []
         remaining s]
    (let [[int-part [letter & more]] (split-with digit-char? remaining)
          num (read-string (apply str int-part))]
      (if letter
        (recur (into acc [num letter]) more)
        (conj acc num)))))

(defn turn [instr dir]
  "Turn the direction indicated by instr, starting out facing dir."
  (case instr
    \L (case dir
         :right :up
         :up    :left
         :left  :down
         :down  :right)
    \R (case dir
         :right :down
         :up    :right
         :left  :up
         :down  :left)))

(defn parse-init-gamestate [lines]
  "Convert input lines into and initial gamestate."
  (let [[drawing [_ instr-str]] (split-with #(not= "" %) lines)
        arr (str->gamestate-arr drawing)
        instr (str->instr instr-str)
        start-x (find-index #(= % 0) (nth arr 0))]
    (new gamestate arr [start-x 0] :right instr 0)))

(def test-gamestate (parse-init-gamestate test-input))

(defn board-ref [arr pos]
  "Get a tile on the board, returning -1 if the position is out of bounds."
  (let [[x y] pos]
    (get (get arr y nil) x -1)))

(defn dir->vec [dir]
  (case dir
    :right [1 0]
    :up    [0 -1]
    :left  [-1 0]
    :down  [0 1]))

(defn wrap-around [arr pos dir]
  "Wrap around the board, (unless there's a wall in the way)."
  (let [[x y] pos
        new-x
        (case dir
          :right (find-index #(not= % -1) (nth arr y))
          :left  (find-last-index #(not= % -1) (nth arr y))
          x)
        new-y
        (case dir
          :up (find-last-index #(not= (get % x -1) -1) arr)
          :down (find-index #(not= (get % x -1) -1) arr)
          y)
        new-pos [new-x new-y]]
    ;; check for walls
    (if (= (board-ref arr new-pos) 1)
      pos
      new-pos)))

(defn step-forward [arr pos dir]
  (let [dir-vec (dir->vec dir)
        first-try (v+ pos dir-vec)]
    (case (board-ref arr first-try)
      1  pos
      0  first-try
      -1 (wrap-around arr pos dir))))

(defn walk-forward [arr pos dir n]
  (nth (iterate #(step-forward arr % dir) pos) n))

(defn follow-instructions [gamestate]
  (let [{:keys [arr pos dir instr]} gamestate]
    (loop [pos pos
           dir dir
           instr instr]
      (let [[num letter & new-instr] instr
            new-pos (walk-forward arr pos dir num)]
        (if (nil? letter)
          (new gamestate arr new-pos dir instr (count instr))
          (let [new-dir (turn letter dir)]
            (recur new-pos new-dir new-instr)))))))

(defn gs-password [gamestate]
  (let [[x y] (:pos gamestate)
        dir-num (case (:dir gamestate)
                  :right 0
                  :down 1
                  :left 2
                  :up 3)]
    (+ (* (inc y) 1000)
       (* (inc x) 4)
       dir-num)))

(defn part1 [input]
  (-> (file-lines input)
      parse-init-gamestate
      follow-instructions
      gs-password
      println))

;; I started to work on a general solution, but it's not worth it lol
(defn identify-blocks [arr block-size]
  (let [width  (quot (reduce max 0 (mapv count arr)) block-size)
        height (quot (count arr) block-size)]
    (mapv (fn [row]
            (mapv #(if (not= -1 (board-ref arr [(* block-size %)
                                                (* block-size row)]))
                     1
                     0)
                  (range width)))
          (range height))))

(defn wrap-around-cube-coords [pos dir]
  "This is hardcoded to the shape of the cube in the input. It's the same shape
for everyone, so it should work on arbitrary inputs. Man fuck this one."
  (let [[x y] pos]
    (case dir
      :right
      (cond
        (< y 50)  [[99 (- 149 y)] :left]
        (< y 100) [[(+ 100 (mod y 50)) 49] :up]
        (< y 150) [[149 (- 49 (mod y 50))] :left]
        (< y 200) [[(+ 50 (mod y 50)) 149] :up])
      :left
      (cond
        (< y 50)  [[0 (- 149 (mod y 50))] :right]
        (< y 100) [[(mod y 50) 100] :down]
        (< y 150) [[50 (- 49 (mod y 50))] :right]
        (< y 200) [[(+ 50 (mod y 50)) 0] :down])
      :up
      (cond
        (< x 50)  [[50 (+ 50 x)] :right]
        (< x 100) [[0 (+ 150 (mod x 50))] :right]
        (< x 150) [[(mod x 50) 199] :up])
      :down
      (cond
        (< x 50)  [[(+ 100 x) 0] :down]
        (< x 100) [[49 (+ 150 (mod x 50))] :left]
        (< x 150) [[99 (+ 50 (mod x 50))] :left]))))

(defn wrap-around-part2 [arr pos dir]
  (let [[new-pos new-dir] (wrap-around-cube-coords pos dir)]
    (if (= (board-ref arr new-pos) 1)
      [pos dir]
      [new-pos new-dir])))

(defn step-forward-part2 [arr pos dir]
  (let [dir-vec (dir->vec dir)
        first-try (v+ pos dir-vec)]
    (case (board-ref arr first-try)
      1  [pos dir]
      0  [first-try dir]
      -1 (wrap-around-part2 arr pos dir))))

(defn walk-forward-part2 [arr pos dir n]
  (loop [i 0
         pos pos
         dir dir]
    (if (not= i n)
      (let [[new-pos new-dir] (step-forward-part2 arr pos dir)]
        (recur (+ i 1) new-pos new-dir))
      [pos dir])))

(defn follow-instructions-part2 [gamestate]
  (let [{:keys [arr pos dir instr]} gamestate]
    (loop [pos pos
           dir dir
           instr instr]
      (let [[num letter & new-instr] instr
            [new-pos new-dir] (walk-forward-part2 arr pos dir num)]
        (if (nil? letter)
          (new gamestate arr new-pos new-dir instr (count instr))
          (let [newer-dir (turn letter new-dir)]
            (recur new-pos newer-dir new-instr)))))))

(defn part2 [input]
  (-> (file-lines input)
      parse-init-gamestate
      follow-instructions-part2
      gs-password
      println))
