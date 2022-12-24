(ns adventofcode22.day21
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defrecord op-monke [name op left right])
(defrecord const-monke [name value])

(defn op-monke? [x]
  (= (type x) op-monke))

(defn const-monke? [x]
  (= (type x) const-monke))

(defn strs->op-monke [name-str left-str op-str right-str]
  (new op-monke
       (symbol name-str)
       (case op-str
         "+" +
         "-" -
         "*" *
         "/" /)
       (symbol left-str)
       (symbol right-str)))

(defn parse-monke [line]
  (let [[name-str desc] (clojure.string/split line #": ")
        x (read-string desc)]
    (if (number? x)
      (new const-monke (symbol name-str) x)
      (let [[left op right] (clojure.string/split desc #" ")]
        (strs->op-monke name-str left op right)))))

(def test-input (clojure.string/split-lines "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"))

;; a barrel of monkes is a map from names to monkes
(defn parse-barrel [lines]
  (into {}
        (map #(let [m (parse-monke %)]
                [(:name m) m]))
        lines))

(def test-barrel (parse-barrel test-input))

(defn monke-order [barrel]
  "Decide an order in which to compute the monkes."
  (let [first-monke (barrel 'root)
        total (count barrel)]
    (loop [acc ()
           deps [first-monke]]
      (if (empty? deps)
        acc
        (let [new-deps (mapcat #(if (op-monke? %)
                                  [(:left %) (:right %)]
                                  [])
                               deps)]
          (recur (into acc
                       deps)
                 (map barrel new-deps)))))))

(def test-order (monke-order test-barrel))

(defn compute-monke [computed-values monke]
  (if (op-monke? monke)
    ((:op monke)
     (computed-values (:left monke))
     (computed-values (:right monke)))
    (:value monke)))

(defn compute-monke-part2 [computed-values monke humn-val]
  (if (= (:name monke) 'humn)
    humn-val
    (compute-monke computed-values monke)))

(defn compute-monke-values [ordered-monkes]
  (reduce #(assoc %1 (:name %2) (compute-monke %1 %2))
          {}
          ordered-monkes))

(defn compute-monke-values-part2 [ordered-monkes humn-val]
  (reduce #(assoc %1 (:name %2) (compute-monke-part2 %1 %2 humn-val))
          {}
          ordered-monkes))

(defn do-part1 [lines]
  (as-> (parse-barrel lines) $
    (monke-order $)
    (compute-monke-values $)
    ($ 'root)))

(defn part1 [input]
  (-> (file-lines input)
      do-part1
      println))

(defn approx-derivative [order key1 key2 i]
  (let [values (compute-monke-values-part2 order i)
        values2 (compute-monke-values-part2 order (+ i 0.01))
        v1 (- (values key1) (values key2))
        v2 (- (values2 key1) (values2 key2))]
    (* 100 (- v2 v1))))

(defn do-part2 [lines]
  "This is the laziest way to do this. A smarter person would approximate a
derivative and at least use that. But my original code is fast enough to do this
lmao."
  (let [barrel (parse-barrel lines)
        order (monke-order barrel)
        root-monke (barrel 'root)
        check1 (:left root-monke)
        check2 (:right root-monke)]
    (loop [i (bigint 0)]
      (let [values (compute-monke-values-part2 order i)
            off-by (- (values check1) (values check2))]
        (if (= off-by (bigint 0))
          i
          (let [d (approx-derivative order check1 check2 i)
                next-i (bigint (- i (/ off-by d)))]
            (println "off by " off-by "; trying i =" next-i)
            (recur next-i)))))))

(defn part2 [input]
  (-> (file-lines input)
      do-part2
      println))
