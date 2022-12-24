(ns adventofcode22.day24
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defrecord gamestate [width height
                      upblizz downblizz leftblizz rightblizz])

(defn parse-gamestate [lines]
  (let [width (- (count (first lines)) 2)
        height (- (count lines) 2)]
    (loop [y 0
           lines (rest lines)
           acc (new gamestate width height #{} #{} #{} #{})]
      (if (empty? lines)
        acc
        (let [new-acc
              (loop [x 0
                     line (rest (first lines))
                     acc acc]
                (if (or (empty? line)
                        (= (first line) \#))
                  acc
                  (let [new-acc
                        (case (first line)
                          \^ (assoc acc
                                    :upblizz
                                    (conj (:upblizz acc) [x y]))
                          \v (assoc acc
                                    :downblizz
                                    (conj (:downblizz acc) [x y]))
                          \< (assoc acc
                                    :leftblizz
                                    (conj (:leftblizz acc) [x y]))
                          \> (assoc acc
                                    :rightblizz
                                    (conj (:rightblizz acc) [x y]))
                          acc)]
                    (recur (inc x) (rest line) new-acc))))]
          (recur (inc y) (rest lines) new-acc))))))

(def test-input (clojure.string/split-lines "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"))

(def test-gamestate (parse-gamestate test-input))

(defn update-blizzards [gamestate]
  (let [{:keys [pos width height upblizz downblizz leftblizz rightblizz]}
        gamestate
        newup (set (map (fn [[x y]]
                          [x (mod (- y 1) height)])
                        upblizz))
        newdown (set (map (fn [[x y]]
                          [x (mod (+ y 1) height)])
                        downblizz))
        newleft (set (map (fn [[x y]]
                          [(mod (- x 1) width) y])
                        leftblizz))
        newright (set (map (fn [[x y]]
                          [(mod (+ x 1) width) y])
                        rightblizz))]
    (assoc gamestate
           :upblizz newup
           :downblizz newdown
           :leftblizz newleft
           :rightblizz newright)))

(defn movement-options [pos gamestate]
  (let [;; move blizzards first
        {:keys [width height upblizz downblizz leftblizz rightblizz]}
        gamestate
        opts-unpruned (map #(v+ pos %)
                           [[1 0] [0 1] [0 0] [-1 0] [0 -1]])
        opts (filter (fn [[x y :as v]]
                       (and (>= x 0)
                            (< x width)
                            (>= y 0)
                            (< y height)
                            (not (or (upblizz v)
                                     (downblizz v)
                                     (leftblizz v)
                                     (rightblizz v)))))
                     opts-unpruned)]
    (if (or (= (nth pos 1) -1)
            (= (nth pos 1) height))
      (conj opts pos)
      opts)))

(defn bfs [init-pos final-pos gamestate]
  (loop [i 0
         cur-positions #{init-pos}
         gamestate gamestate]
    (if (cur-positions final-pos)
      [i gamestate]
      (let [new-gamestate (update-blizzards gamestate)]
        (recur (inc i)
               (set (mapcat #(movement-options % new-gamestate) cur-positions))
               new-gamestate)))))

(defn do-part1 [lines]
  (let [gs (parse-gamestate lines)
        [t final-gs]
        (bfs [0 -1] [(dec (:width gs)) (dec (:height gs))] gs)]
    (inc t)))

(defn part1 [input]
  (-> (file-lines input) do-part1 println))

(defn do-part2 [lines]
  (let [init (parse-gamestate lines)
        width (:width init)
        height (:height init)
        [t1 almost-there] (bfs [0 -1] [(dec width) (dec height)] init)
        there (update-blizzards almost-there)
        [t2 almost-back] (bfs [(dec width) height] [0 0] there)
        back (update-blizzards almost-back)
        [t3 once-more] (bfs [0 -1] [(dec width) (dec height)] back)]
    (+ t1 t2 t3 3)))

(defn part2 [input]
  (-> (file-lines input) do-part2 println))
