(ns rnd-utils
  (:use clojure.utils.parser))

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

(def literal
  (attach
     (match #"(\\.|[^{}.+*()\[\]^$])+")
     (fn [[m _]]
       (constantly (.replace m "\\" "")))))

(def any-char
  (attach
    (match #"\.")
    (fn [_] #(rnd-choice "abcdefghijklmnopqrstuvwxyz"))))

(def single
  (choice literal any-char))

(def char-sequence
  (match #"(\\.|[^\^\-\[\]])+"))

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
