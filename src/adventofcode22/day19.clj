(ns adventofcode22.day19
  (:require adventofcode22.common))

(refer 'adventofcode22.common)

(defrecord gamestate [t
                      ore-bot clay-bot obs-bot geo-bot
                      ore clay obs geo])

(defrecord blueprint [id
                      ore-ore
                      clay-ore
                      obs-ore obs-clay
                      geo-ore geo-obs])

(defn parse-blueprint [line]
  (let [[id ore-ore clay-ore obs-ore obs-clay geo-ore geo-obs]
        (map #(Integer/parseInt %) (re-seq #"[0-9]+" line))]
    (new blueprint id ore-ore clay-ore obs-ore obs-clay geo-ore geo-obs)))

(def init-gamestate (new gamestate 0 1 0 0 0 0 0 0 0))

(defn quot1 [m d]
  "Like quot, but rounds up if there's a remainder instead of rounding down."
  (let [q (quot m d)]
    (if (not= (mod m d) 0)
      (inc q)
      q)))

(defn time-to-build [gamestate blueprint bot-type]
  "Compute the time it would take to build the specified bot type."
  (let [{:keys [ore-bot clay-bot obs-bot ore clay obs]} gamestate
        {:keys [ore-ore clay-ore obs-ore obs-clay geo-ore geo-obs]} blueprint]
    (inc (case bot-type
           :ore-bot (quot1 (max (- ore-ore ore) 0) ore-bot)
           :clay-bot (quot1 (max (- clay-ore ore) 0) ore-bot)
           :obs-bot (if (= clay-bot 0)
                      nil
                      (max (quot1 (max (- obs-ore ore) 0) ore-bot)
                           (quot1 (max (- obs-clay clay) 0) clay-bot)))
           :geo-bot (if (= obs-bot 0)
                      nil
                      (max (quot1 (max (- geo-ore ore) 0) ore-bot)
                           (quot1 (max (- geo-obs obs) 0) obs-bot)))))))

(defn game-choices [gamestate blueprint time-left]
  "Game choices at the given state."
  (let [{:keys [ore-bot clay-bot obs-bot geo-bot ore clay obs]} gamestate
        {:keys [ore-ore clay-ore obs-ore obs-clay geo-ore geo-obs]} blueprint]
    (as-> [] $
      (if (and (< ore-bot (max clay-ore obs-ore geo-ore))
               (> time-left 3))
        (conj $ :ore-bot)
        $)
      (if (and (> time-left 4)
               (< clay-bot obs-clay))
        (conj $ :clay-bot)
        $)
      (if (and (> clay-bot 0)
               (< obs-bot geo-obs)
               (> time-left 3))
        (conj $ :obs-bot)
        $)
      (if (and (> obs-bot 0)
               (> time-left 2))
        (conj $ :geo-bot)
        $))))

(defn update-resources [gamestate delta-t]
  "Update resources (and time) over t minutes"
  (let [{:keys [t ore-bot clay-bot obs-bot geo-bot ore clay obs geo]}
        gamestate]
    (assoc gamestate
           :t (+ t delta-t)
           :ore (+ (* delta-t ore-bot) ore)
           :clay (+ (* delta-t clay-bot) clay)
           :obs (+ (* delta-t obs-bot) obs)
           :geo (+ (* delta-t geo-bot) geo))))

(defn wait-and-build [gamestate blueprint bot-type t]
  "Wait t minutes and build the specified bot type. This can result in negative
resources if not done correctly."
  (let [updated (update-resources gamestate t)
        {:keys [time ore-bot clay-bot obs-bot geo-bot ore clay obs]} updated
        {:keys [ore-ore clay-ore obs-ore obs-clay geo-ore geo-obs]} blueprint]
    (case bot-type
      :ore-bot (assoc updated :ore-bot (+ ore-bot 1) :ore (- ore ore-ore))
      :clay-bot (assoc updated :clay-bot (+ clay-bot 1) :ore (- ore clay-ore))
      :obs-bot (assoc updated
                      :obs-bot (+ obs-bot 1)
                      :ore (- ore obs-ore)
                      :clay (- clay obs-clay))
      :geo-bot (assoc updated
                      :geo-bot (+ geo-bot 1)
                      :ore (- ore geo-ore)
                      :obs (- obs geo-obs)))))

(def test-input (clojure.string/split-lines
                 "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."))

(defn prune-choices [choices]
  "This makes the following optimization: never build a clay robot when we can
build a geode robot faster. I don't think that this is correct, but it got the
job done. Over a long enough time, we could conceivably be in a situation where
we'd like to crank out a couple clay robots to get some more obsidian robots out 
faster. But for depth 32, it worked fine.

Notice that the actual code doesn't use this function, and it still runs pretty
fast."
  (let [ore-choice (find-by #(= (first %) :ore-bot) choices)
        clay-choice (find-by #(= (first %) :clay-bot) choices)
        obs-choice (find-by #(= (first %) :obs-bot) choices)
        geo-choice (find-by #(= (first %) :geo-bot) choices)]
    (filter identity
            [ore-choice
             (if (or (not geo-choice)
                     (and geo-choice clay-choice
                          (> (second geo-choice)
                             (second clay-choice))))
               clay-choice
               nil)
             obs-choice
             geo-choice])))

(defn seq-all-games [gamestate blueprint time-left]
  (letfn [(extend-seq [cur-state time-left]
            (let [choices (game-choices cur-state blueprint time-left)
                  good-choices (keep #(let [t (time-to-build cur-state
                                                             blueprint
                                                             %)]
                                        (if (< t time-left)
                                          [% t]
                                          nil))
                                     choices)]
              (if (empty? good-choices)
                (let [g (:geo (update-resources cur-state time-left))]
                  (if (= g 0)
                    []
                    [[g]]))
                (mapcat (fn [[x t]]
                          (let [updated (wait-and-build cur-state blueprint x t)]
                            (map #(cons x %)
                                 (extend-seq updated (- time-left t)))))
                        good-choices))))]
    (extend-seq gamestate time-left)))

(defn optimize-over-blueprint [blueprint total-time]
  (last (reduce #(max-key last %1 %2)
                [0]
                (seq-all-games init-gamestate blueprint total-time))))

(defn do-part1 [lines]
  (reduce +
          0
          (map #(let [opt (time
                           (optimize-over-blueprint % 24))]
                  (* (:id %1) opt))
               (map parse-blueprint lines))))

(defn part1 [input]
  (println (do-part1 (file-lines input))))

(defn do-part2 [lines]
  (reduce *
          1
          (map #(let [opt (time
                           (optimize-over-blueprint % 32))]
                  (println (:id %1) opt)
                  opt)
               (map parse-blueprint (take 3 lines)))))

(defn part2 [input]
  (println (do-part2 (file-lines input))))
