
(defn str->ints [s]
  (map #(Character/digit % 10) s))

(defn double-every-second [s]
  (reverse
   (map #(%1 %2)
        (cycle  [identity #(* % 2)])
        (reverse s))))

(defn ints->digits [s]
  (mapcat #(str->ints (str %)) s))

(def add (partial reduce +))

(defn luhny? [s]
  (-> s
      str->ints
      double-every-second
      ints->digits
      add
      (mod 10)
      zero?))
