(ns adventofcode22.day5
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn extract-entries [line]
  (let [stripped-seq (rest (into [] line))]
    (loop [acc []
           chs stripped-seq]
      (if (empty? chs)
        acc
        (recur (conj acc (str (first chs))) (nthrest chs 4))))))

(defn add-to-stack [world stack-id item]
  (conj world [stack-id (cons item (get world stack-id))]))

(defn create-world [lines-of-text]
  (let [rlines (reverse lines-of-text)
        keys (map #(Integer/parseInt %) (extract-entries (first rlines)))
        init (zipmap keys (repeat (list)))]
    (loop [world init
           next-lines (rest rlines)]
      (if (empty? next-lines)
        world
        (let [items (extract-entries (first next-lines))
              new-world (reduce (fn [acc in]
                                  (if (= (nth items in) " ")
                                    acc
                                    (add-to-stack acc (nth keys in) (nth items in))))
                                world
                                (range (count keys)))]
          (recur new-world (rest next-lines)))))))

(defn parse-move [line]
  (let [[_ n _ src _ dst] (clojure.string/split line #"[\s]+")]
    (map #(Integer/parseInt %) [n src dst])))

(defn perform-move-part1 [world n src dst]
  (let [cargo (take n (get world src))
        removed (conj world [src (nthrest (get world src) n)])]
    (conj removed [dst (into (get world dst) cargo)])))

(defn perform-move-part2 [world n src dst]
  (let [cargo (take n (get world src))
        removed (conj world [src (nthrest (get world src) n)])]
    (conj removed [dst (into (get world dst) (reverse cargo))])))

(defn part1 [input]
  (let [lines (file-lines input)
        [world-def [_ & moves]] (split-with #(not= "" %) lines)
        world-init (create-world world-def)
        world-end (reduce (fn [acc in]
                            (apply perform-move-part1 acc (parse-move in)))
                          world-init
                          moves)
        stack-ids (sort (keys world-end))]
    (println (apply str (map #(first (get world-end %)) stack-ids)))))

(defn part2 [input]
  (let [lines (file-lines input)
        [world-def [_ & moves]] (split-with #(not= "" %) lines)
        world-init (create-world world-def)
        world-end (reduce (fn [acc in]
                            (apply perform-move-part2 acc (parse-move in)))
                          world-init
                          moves)
        stack-ids (sort (keys world-end))]
    (println (apply str (map #(first (get world-end %)) stack-ids)))))
