(ns adventofcode22.day25
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defn base5-digits [num]
  (loop [acc ()
         q num]
    (if (= q 0)
      acc
      (recur (conj acc (mod q 5))
             (quot q 5)))))

(defn snafu-digits [num]
  (let [b5 (base5-digits num)]
    (loop [acc ()
           digits (reverse b5)]
      (if (empty? digits)
        acc
        (let [[hd & tl] digits]
          (cond
            (<= hd 2) (recur (conj acc hd) tl)
            (empty? tl) (conj (conj acc (- hd 5)) 1)
            :else (recur (conj acc (- hd 5))
                         (conj (rest tl) (inc (first tl))))))))))

(defn fmt-snafu [num]
  (let [digits (snafu-digits num)]
    (apply str (map #(case %
                       -1 \-
                       -2 \=
                       %)
                    digits))))

(defn parse-snafu [s]
  (reduce #(+ (* %1 5)
              (case %2
                \0 0
                \1 1
                \2 2
                \- -1
                \= -2))
          0
          s))

(def test-input (clojure.string/split-lines "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"))

(defn do-part1 [lines]
  (->> lines (map parse-snafu) (reduce + 0) fmt-snafu))

(defn part1 [input]
  (-> (file-lines input) do-part1 println))

(defn part2 [input]
  (println "No part 2 for this one!"))
