(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (if (== up-to 1)
     (0)
     (cons (dec up-to) (my-range (dec (dec up-to)))))))



user> (set [4 4 5 0])
#{0 4 5}
user> (map inc #{4 5 0})
(1 5 6)
user> (def my-incs {1 (inc 1)
                    5 (inc 5)
                    8 (inc 8)})
#'user/my-incs
user> my-incs
{1 2, 5 6, 8 9}
user> (defn len-map-entry [a-seq]
          {a-seq (count a-seq)})
#'user/len-map-entry
user> (len-map-entry [2 4 7])
{[2 4 7] 3}
user> (map len-map-entry [[4 56 8] [4] [6 2 7 5]])
({[4 56 8] 3} {[4] 1} {[6 2 7 5] 4})
y


(defn halve [a-seq]
  (let [first-length (int (/ (count a-seq) 2))
        second-length (- (count a-seq) first-length)
        second-end (+ first-length second-length)
        first-range (range 0 first-length)
        second-range (range first-length second-end)
        grab-indices (fn [x] (get a-seq x))
        first-half (map grab-indices first-range)
        second-half (map grab-indices second-range)]
    (vec [first-half second-half])))
