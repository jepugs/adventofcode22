(ns adventofcode22.core
  (:gen-class))

(defmacro require-if-exists [name]
  `(try
     (require ~name)
     true
     (catch java.io.FileNotFoundException ~(gensym)
       false)))

(defmacro gen-solution-map [name]
  (let [ns-name #(str "adventofcode22.day" %)
        day-numbers (filter #(require-if-exists (symbol (ns-name %))) (range 1 26))]
    `(do ~@(map (fn [x] `(require '~(symbol (ns-name x)))) day-numbers)
         (def ~name
           ~(zipmap day-numbers
                    (map (fn [x]
                           (let [n (ns-name x)]
                             `{1 ~(symbol n "part1")
                               2 ~(symbol n "part2")}))
                         day-numbers))))))

(gen-solution-map solution-function-map)

(defn -main
  [& args]
  (cond
    (not= (count args) 3)
    (println "Please pass two numbers and a filename as arguments.")

    :else
    (let [day (try
                (Integer/parseInt (nth args 0))
                (catch NumberFormatException e nil))
          part (try
                 (Integer/parseInt (nth args 1))
                 (catch NumberFormatException e nil))
          day-map (get solution-function-map day {})
          func (get day-map part nil)]
      (if func
        (func (nth args 2))
        (println "Could not find the specified day/part combination.")))))
