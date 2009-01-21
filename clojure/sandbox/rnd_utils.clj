(ns clojure.sandbox.rnd-utils
  (:use clojure.sandbox.parser))

(defn rnd-choice
  [coll]
  (let [v (vec coll)]
    (v (rand-int (count v)))))

(defn take-fn
  [n f]
  (take n (repeatedly f)))

(defn rnd-seq
  [f min max]
  (take-fn (+ (rand-int (- max min)) min) f))

(defn parse-int
  [n]
  (Integer/parseInt n))

(defn char-range
  [from to]
  (map char
    (range (int (first from))
           (inc (int (first to))))))

(def literal
  (attach
     (match #"(\\.|[^{}.+*()\[\]^$])+")
     (fn [[m _]]
       (constantly (.replace m "\\" "")))))

(def any-char
  (attach
    (match #"\.")
    (fn [_] #(rnd-choice "abcdefghijklmnopqrstuvwxyz"))))

(defn sequence-of-chars
  [src]
  (let [f (match #"((\\.|[^\^\-\[\]\\])+)([^-]|$)")]
    (if-let [[[_ m _ s] src] (f src)]
      [(.replace m "\\" "")
       (str s src)])))

(def range-of-chars
  (attach
    (match #"(\\.|[^\^\-\[\]\\])-(\\.|[^\^\-\[\]\\])")
    (fn [[_ from to]] (char-range from to))))

(def char-class
  (attach
    (series
      (match #"\[")
      (many (choice sequence-of-chars range-of-chars))
      (match #"\]"))
    (fn [[_ chars _]]
      #(rnd-choice (apply concat chars)))))

(def single
  (choice literal
          any-char
          char-class))

(def zero-or-more
  (attach
    (series single (match #"\*"))
    (fn [[f _]] #(apply str (rnd-seq f 0 10)))))

(def one-or-more
  (attach
    (series single (match #"\+"))
    (fn [[f _]] #(apply str (rnd-seq f 1 10)))))

(def exactly-n
  (attach
    (series single (match #"\{(\d+)\}"))
    (fn [[f [_ n]]]
      #(apply str
         (take-fn (parse-int n) f)))))

(def between-n-and-m
  (attach
    (series single (match #"\{(\d+),\s*(\d+)\}"))
    (fn [[f [_ n m]]]
      #(apply str
         (rnd-seq f (parse-int n) (parse-int m))))))

(def regex
  (attach
    (many (choice zero-or-more
                  one-or-more
                  exactly-n
                  between-n-and-m
                  single))
    (fn [ms] (apply str (map apply ms)))))
