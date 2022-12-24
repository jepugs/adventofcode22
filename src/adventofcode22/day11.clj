(ns adventofcode22.day11
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defrecord munki [id items op divis true-throw false-throw])

(defn parse-op [s]
  (let [[a op c] (clojure.string/split s #" ")]
    (fn [old]
      (let [l (if (= a "old") old (Integer/parseInt a))
            r (if (= c "old") old (Integer/parseInt c))
            fun (case op
                  "+" +
                  "*" *
                  :else +)]
        (fun l r)))))

(defn parse-munki [lines]
  (let [id (-> (nth lines 0)
               (clojure.string/split #"[ :]+")
               (nth 1)
               Integer/parseInt)
        items (as-> (nth lines 1) $
                (subs $ 18)
                (clojure.string/split $ #"[ ,]+")
                (map #(Integer/parseInt %) $))
        op (-> (nth lines 2)
               (subs 19)
               parse-op)
        divis (-> (nth lines 3)
                 (subs 21)
                 Integer/parseInt)
        true-throw (-> (nth lines 4)
                       (subs 29)
                       Integer/parseInt)
        false-throw (-> (nth lines 5)
                       (subs 30)
                       Integer/parseInt)]
    (new munki id items op divis true-throw false-throw)))

(defn process-munki [munki]
  "Runs through all the items owned by a munki and decides where to throw them.
 Generates two vecs: one vec of items thrown to the true munki, one vec of items
 thrown to the false munki."
  (let [{:keys [items op divis]} munki]
   (loop [true-out []
          false-out []
          items items]
     (if (empty? items)
       [true-out false-out]
       (let [worry (quot (op (first items)) 3)]
         (if (= (mod worry divis) 0)
           (recur (conj true-out worry)
                  false-out
                  (rest items))
           (recur true-out
                  (conj false-out worry)
                  (rest items))))))))

(defn process-munki-part2 [munki modulus]
  (let [{:keys [items op divis]} munki]
   (loop [true-out []
          false-out []
          items items]
     (if (empty? items)
       [true-out false-out]
       (let [worry (mod (op (first items)) modulus)]
         (if (= (mod worry divis) 0)
           (recur (conj true-out worry)
                  false-out
                  (rest items))
           (recur true-out
                  (conj false-out worry)
                  (rest items))))))))

(defn give-munki [to-id new-items]
  "Returns a transducer that gives the specified munki the specified items."
  (map (fn [m]
         (let [{:keys [id items]} m]
           (if (= id to-id)
             (assoc m :items (concat items new-items))
             m)))))

(defn clear-munki [id]
  (map (fn [m]
         (if (= (:id m) id)
           (assoc m :items [])
           m))))

(defn munki-by-id [munki-vec id]
  (first (filter #(= (:id %) id) munki-vec)))

(defn update-munki-step [munki-vec current-id]
  (let [munki (munki-by-id munki-vec current-id)
        [true-out false-out] (process-munki munki)
        {:keys [true-throw false-throw]} munki
        xform (comp (give-munki true-throw true-out)
                    (give-munki false-throw false-out)
                    (clear-munki current-id))]
    (into [] xform munki-vec)))

(defn update-munki-step-part2 [munki-vec current-id modulus]
  (let [munki (munki-by-id munki-vec current-id)
        [true-out false-out] (process-munki-part2 munki modulus)
        {:keys [true-throw false-throw]} munki
        xform (comp (give-munki true-throw true-out)
                    (give-munki false-throw false-out)
                    (clear-munki current-id))]
    (into [] xform munki-vec)))

(defn update-munki-round [munki-vec]
  (let [n (count munki-vec)]
    (loop [current-id 0
           munki-vec munki-vec
           inspection-vec []]
      (if (= current-id n)
        [munki-vec inspection-vec]
        (recur (+ current-id 1)
               (update-munki-step munki-vec current-id)
               (conj inspection-vec (count (:items (munki-by-id munki-vec current-id)))))))))

(defn update-munki-round-part2 [munki-vec modulus]
  (let [n (count munki-vec)]
    (loop [current-id 0
           munki-vec munki-vec
           inspection-vec []]
      (if (= current-id n)
        [munki-vec inspection-vec]
        (recur (+ current-id 1)
               (update-munki-step-part2 munki-vec current-id modulus)
               (conj inspection-vec
                     (count (:items (munki-by-id munki-vec current-id)))))))))

(defn init-munkis [lines]
  (let [xform (comp (filter #(not= % ""))
                    (partition-all 6)
                    (map parse-munki))]
    (into [] xform lines)))

(def test-in (clojure.string/split-lines
               "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"))

(defn count-posessions []
  (map #(count (:items %))))

(defn munki-business [inspection-vec]
  (reduce * (take 2 (sort > inspection-vec))))

(defn part1 [input]
  (loop [munki-vec (init-munkis (file-lines input))
         inspection-vec (into [] (repeat (count munki-vec) 0))
         round 0]
    (if (= round 20)
      (println (munki-business inspection-vec))
      (let [[updated inspected] (update-munki-round munki-vec)]
        (recur updated
               (mapv + inspection-vec inspected)
               (+ round 1))))))

(defn part2 [input]
  "For part 2, the worry numbers get very large. I reduce them by taking the
 modulus of the worry number by the product of the divisibility numbers for each
 munki. This guarantees that the modulus will not change relative to any of the
 divisibility numbers."
  (let [munki-vec (init-munkis (file-lines input))
        modulus (reduce (fn [acc in]
                          (* acc (:divis in)))
                        1
                        munki-vec)]
    (loop [munki-vec munki-vec
           inspection-vec (into [] (repeat (count munki-vec) 0))
           round 0]
      (if (= round 10000)
        (println (munki-business inspection-vec))
        (let [[updated inspected] (update-munki-round-part2 munki-vec modulus)]
          (recur updated
                 (mapv + inspection-vec inspected)
                 (+ round 1)))))))
