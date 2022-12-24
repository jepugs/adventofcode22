(ns adventofcode22.common)

(defn file-lines [filename]
  (with-open [in (clojure.java.io/reader filename)]
    (loop [acc []]
      (let [line (try (.readLine in))]
        (if line
          (recur (conj acc line))
          acc)))))

(defn find-by [pred coll]
  (loop [coll coll]
    (cond (empty? coll) nil
          (pred (first coll)) (first coll)
          :else (recur (rest coll)))))

(defn find-index [pred coll]
  (loop [i 0
         coll coll]
    (cond
      (empty? coll) nil
      (pred (first coll)) i
      :else (recur (+ i 1) (rest coll)))))

(defn find-last-index [pred coll]
  (loop [i (dec (count coll))]
    (cond
      (= i -1) nil
      (pred (nth coll i)) i
      :else (recur (dec i)))))

(defn gcd [a b]
  (loop [a a
         b b]
    (if (= a b)
      a
      (if (< a b)
        (recur (- b a) a)
        (recur (- a b) b)))))

(defn v+ [& args]
  (apply mapv + args))

(defn v- [& args]
  (apply mapv - args))

(defn digit-char? [c]
  (<= (int \0) (int c) (int \9)))

