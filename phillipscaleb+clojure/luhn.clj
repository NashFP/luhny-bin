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
   way to reuse something existing.

   Is there a cleaner way to do this with regex?
  "
  ([n step ignore coll]
     (partition-ignoring n step ignore coll 0))
  ([n step ignore coll pos]
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
              (cons [p pos] (partition-ignoring
                             n step ignore (nthnext s step) (inc pos))))))))))

(defn find-cc-nums
  "Return a lazy seq of the credit card numbers in s.  The numbers will include
   all the allowed characters for a credit card number according to the spec:
   digits, spaces and hyphens."
  [s]
  (filter #(cc? (first %))
               (mapcat #(partition-ignoring % 1 #{\- \space} s)
                       [14 15 16])))

(defn mask-cc-num
  "Return the string s with cc replaced with its masked form"
  [s cc]
  (let [[num pos] cc
        masked-cc (cs/replace (apply str num) #"\d" "X")
        before (subs s 0 pos)
        after (subs s (+ pos (count masked-cc)))]
    (str before masked-cc after)))

(defn mask
  "Takes a string s and returns a string with the credit card numbers masked.

   Takes a pretty brute force approach.  Also does not read streams as the
   square program expects.
  "
  [text]
  (reduce (fn [s cc] (mask-cc-num s cc))
          text
          (find-cc-nums text)))