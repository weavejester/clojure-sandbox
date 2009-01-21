(ns clojure.sandbox.parser)

(defn match
  "Create a rule to match a regular expression."
  [re]
  (fn [src]
    (let [m (re-matcher re src)]
      (if (.lookingAt m)
        [(re-groups m)
         (.substring src (.end m))]))))

(defn observe
  "Creates a rule, but doesn't reduce the source if it matches."
  [re]
  (let [f (match re)]
    (fn [src]
      (let [[m _] (f src)] 
        [m src]))))

(defn attach
  "Attach a function to transform the result of a rule."
  [rule f]
  (fn [src]
    (if-let [[grps src] (rule src)]
      [(f grps) src])))

(defn series
  "Create a new rule out of a series of individual rules."
  [& rules]
  (fn [src]
    (reduce
      (fn [[xs s] rule]
        (if (seq s)
          (if-let [[x s] (rule s)]
            [(conj xs x) s])))
      [[] src]
      rules)))

(defn choice
  "Create a new rule by returning the first rule that matches."
  [& rules]
  (fn [src]
    (some
      (fn [rule] (rule src))
      rules)))

(defn many
  "Match zero or more for a rule."
  [rule]
  (fn [src]
    (loop [xs [], src src]
      (if-let [[x src] (rule src)]
        (recur (conj xs x) src)
        [xs src]))))
