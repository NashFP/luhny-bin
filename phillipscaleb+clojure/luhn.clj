(ns luhny.core
  (:require [clojure.string :as cs]))

(defn str->ints [s]
  (map #(Character/digit % 10) s))

(defn double-every-second [coll]
  (map #(%1 %2)
       (cycle  [identity #(* % 2)])
       (reverse coll)))

(defn ints->digits [coll]
  (mapcat #(str->ints (str %)) coll))

(def add (partial reduce +))

(defn luhny? [s]
  (-> s
      str->ints
      double-every-second
      ints->digits
      add
      (mod 10)
      zero?))

(defn cc? [s]
  (let [s (remove #{\- \space} s)]
    (and
     (<= 14 (count s) 16)
     (every? #(Character/isDigit %) s)
     (luhny? s))))

(defn partition-ignoring
  "Acts like the 3 argument version of partition in clojure.core, except that it 'ignores'
   the characters in the ignore set.  It includes them in the partiion,
   but does not count them when building the partition.
   The idea is to use this to divide a sequences of characters up into partitions
   of n characters, not counting spaces and hyphens, but preserving them so that
   we can spit them back out once we mask any cc numbers
   A lot of this is copied from the partition function, so there maybe a better
   way to reuse something existing."
  [n step ignore coll]
  (let [cnt-ignoring #(count (remove ignore %))
        full-partition? #(= n (cnt-ignoring %))
        take-ignoring #(loop [c % taken []]
                         (if (or (full-partition? taken) (empty? c))
                           taken
                           (recur (rest c) (conj taken (first c)))))]
    (lazy-seq
     (when-let [s (seq coll)]
       (let [p (take-ignoring s)]
         (when (full-partition? p)
           (cons p (partition-ignoring n step ignore (nthnext s step)))))))))

(defn find-cc-nums
  "Return a lazy seq of the credit card numbers in s.  The numbers will include
   all the allowed characters for a credit card number according to the spec:
   digits, spaces and hyphens."
  [s]
  (map #(apply str %)
       (filter cc?
               (partition-ignoring 16 1 #{\- \space} s))))

(defn mask-cc-num
  "Return the string s with cc replaced with its masked form"
  [s cc]
  (let [masked-cc (cs/replace cc #"\d" "X")]
    (cs/replace s cc masked-cc)))

(defn mask
  "Takes a string s and returns a string with the credit card numbers masked.

  This is a very simple version so far that does not cover many of the cases
  listed in the problem spec."
  [s]
  (loop [s s ccs (find-cc-nums s)]
    (if (empty? ccs)
      s
      (recur (mask-cc-num s (first ccs)) (rest ccs)))))