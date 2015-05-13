(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
   (boolean (first coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
   (if (singleton? coll)
     (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
        (first a-seq)
        (let [my-first (first a-seq)
              my-second (first (rest a-seq))
              my-others (rest (rest a-seq))
              my-bigger (max my-first my-second)
              one-shorter (cons my-bigger my-others)]
          (if (empty? my-others)
            my-bigger
            (max-element one-shorter))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (let [my-first (first a-seq)
            my-second (first (rest a-seq))
            my-others (rest (rest a-seq))
            my-bigger (seq-max my-first my-second)
            one-shorter (cons my-bigger my-others)]
        (if (empty? my-others)
          my-bigger
          (longest-sequence one-shorter))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      (my-take-while pred? ()))))
            

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))
    
    

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (zero? n)
    0
    (if (== n 1)
      1
    (+ (fib (dec n)) (fib (dec (dec  n)))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (cons (seq a-seq)
          (tails (rest a-seq)))))

      

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (let [rev-seq (reverse a-seq)
          rev-rest (rest rev-seq)]
      (cons (reverse rev-seq)
            (inits (reverse rev-rest))))))

(defn rotations [a-seq]
  (let [my-inits (inits a-seq)
        my-tails (tails a-seq)
        rev-inits (reverse my-inits)]
    (map concat (rest my-tails) (rest rev-inits))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
   (let [seq-first (first a-seq)
         new-freqs (assoc freqs seq-first (inc (get freqs seq-first)))]
     (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (let [set-seq (set a-seq)
        starting-freqs (apply merge (map (fn [x] {x 0}) set-seq))]
    (my-frequencies-helper starting-freqs a-seq)))

(defn un-frequencies [a-map]
  (let [my-first (first a-map)
        my-keys (keys a-map)
        repeat-elem (fn [x] (repeat (get a-map x) x ))]
    (apply concat (map repeat-elem my-keys))))

(defn my-take [n coll]
  (if
      (or (zero? n) (zero? (count coll)))
    ()
    (let [new-n (dec n)]
    (cons (first coll) (my-take new-n (rest coll))))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (if (zero? (count coll))
      ()
      (let [new-n (dec n)]
        (my-drop new-n (rest coll))))))

(defn halve [a-seq]
  (let [first-length (int (/ (count a-seq) 2))]
    [(my-take first-length a-seq) (my-drop first-length a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    []
    (if (empty? a-seq) b-seq
     (if (empty? b-seq) a-seq
      (let [first-a (first a-seq)
            first-b (first b-seq)]
        (if (< first-a first-b)
          (concat [first-a] (seq-merge (rest a-seq) b-seq))
          (concat [first-b] (seq-merge a-seq (rest b-seq)))))))))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    []
    (if (singleton? a-seq)
      a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) (merge-sort second-half))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

