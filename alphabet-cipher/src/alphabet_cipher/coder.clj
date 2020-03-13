(ns alphabet-cipher.coder)

(defn letter-int [x]
  (-> x
      int
      (bit-and 31)
      dec))

(defn int-letter [x]
  (-> x
      (mod 26)
      (+ (int \a))
      char))

(defn encode-1 [keyword message rest-keyword in-message subtract?]
  (if (zero? (count message))
    in-message
    (encode-1
      keyword
      (rest message)
      (if (<= (count rest-keyword) 1) keyword (rest rest-keyword))
      (cons (int-letter
              ((if subtract? #'- #'+)
               (letter-int (first message))
               (letter-int (first rest-keyword)))
              ) in-message)
      subtract?)))

(defn encode [keyword message]
  (->> (encode-1 keyword message keyword nil false)
       reverse
       (apply str)))

(defn decode [keyword message]
  (->> (encode-1 keyword message keyword nil true)
       reverse
       (apply str)))

(defn is-reps [str part]
  (if (<= (count str) (count part))
    (= str (subs part 0 (count str)))
    (and
      (= part (subs str 0 (count part)))
      (is-reps (subs str (count part)) part))
    ))

(defn minimal-repetition [str min-n]
  (let [str-start (subs str 0 min-n)]
    (if (is-reps str str-start)
      str-start
      (minimal-repetition str (inc min-n)))
    ))

(defn decipher [cipher message]
  (minimal-repetition
    (apply str (reverse (encode-1 "" cipher message nil true)))
    1))
