(ns mhs.euler-26-50
  (:require [criterium.core :refer [bench with-progress-reporting quick-bench]]
            [mhs.euler-1-25 :refer [divisible-by? select-place primes-to digits-big div-pair is-whole?
                                    factorial exp all-factors]]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :refer [intersection]]))

(defn get-nine-and-zeros
  ([bits] (get-nine-and-zeros bits 0 1))
  ([bits nines-zeros place-multiplier]
   (cond
     (= 0 bits) nines-zeros
     ; Last bit is a one
     (bit-test bits 0) (recur
                        (bit-shift-right bits 1)
                        (+ nines-zeros (* 9 place-multiplier))
                        (* 10 place-multiplier))
     :else (recur (bit-shift-right bits 1)
                  nines-zeros
                  (* 10 place-multiplier)))))

(defn first-nines-zeros-divides-evenly
  [denom]
  (->> (drop 1 (range))
       (map get-nine-and-zeros)
       (filter (divisible-by? denom))
       first))

(defn decimals
  ([frac] (when-not (zero? frac)
            (let [ones (mod (quot frac 1) 10)]
              (cons ones (lazy-seq (decimals (* 10 (- frac ones)))))))))

(defn has? 
  [coll val]
  (some (partial = val) coll))

; Nines and zeroes didn't go anywhere, but the direct "find decimal place" approach
; works well
; Reuse select-place from "sum of digits" problems
; Notice that repeated decimals must be such that rational representation of
; the next decimal place must be the same as another one that was encountered
; Find and subtract out the next decimal place and remember where we found it
; Then check to see if we've been "here" before
; If so then we've cycled and we look to see how far it was since we cycled
; It's not length of the seen because of numbers like 1/6 that are like 0.16666...
; and the 1 is included in the seen
; Probably could improve by also tracking the index that a frac was found and putting
; the values into a map
; ---> Yep, makes it much faster
(defn repeating-decimal-length
  ([frac] (repeating-decimal-length frac {} 0))
  ([frac seen index]
   (cond
     (zero? frac) 0
     (get seen frac) (- (count seen) (get seen frac))
     :else (let [ones (select-place frac 1)]
             (recur (* 10 (- frac ones)) (assoc seen frac index) (inc index))))))

(defn euler-26 []
  (->> (range 1 1000)
       (map (partial / 1))
       (apply max-key repeating-decimal-length)))

(comment
  "Euler 26"
  ; Numbers that have all 9's in the denominator have repeating decimal representations
  (double (/ 145 9999))
  (double (/ 999999 7))
  ; Dividing by ten puts the repeating part later in the decimal
  (double (/ 6 90))
  (double (/ 6 9900))
  ; For 1/6, it's equal to 1/10 + (10/60 - 6/60) = 1/10 + 4/60 = 1/10 + 6/90
  ; If the denominator in 1/x evenly divides 9, 99, etc. then it will have a 
  ; repeating decimal representation. But also if it evenly divides 90, 900, 9900 etc.
  ; Probably want to instead have some calculation of the decimal and then see if we're
  ; at a spot we've been before
  ; One thing to note is that we will eventually get to a repeating section, so we can
  ; keep checking until we get there. No irrationals here
  ; For any of these, 9 divides the denominator evenly
  (/ (/ 145 9999) (/ 1 9))
  (/ (/ 1 6) (/ 1 9))
  (double (/ 15 90))
  (* 9 (/ 1 6))
  (* 99 (/ 1 6))
  (* 90 (/ 1 6))
  (* 99 (/ 1 7))
  (double (/ 1 9090)) ; Can also have zeroes in the middle of the 9s
  ; This is like 1/9000 + 1/90 so makes sense it's repeating
  (* 999999 (/ 1 7))
  (double (/ 1010 9090))
  ; The search can proceed as follows:
  ; Number of repeating decimals equals number of 9s in 9, 99, 90, 9900
  ; Things we can do: add a 9 to the end, add a zero to the end
  ; Stop when we denominator divides evenly
  ; Needs to be a breadth-first search between adding zeroes and adding nines
  ; To get number of repeating, it's length of first 9+zero sequence that the
  ; denom divides evenly(?), perhaps shortened by trailing zeros?
  ; Note that the nines correspond to the ones in the binary representation of
  ; an integer as we count up
  (/ 9 6)
  (/ 90 6)
  (/ 99 6)
  (/ 900 6)
  (/ 909 6)
  (/ 990 6)
  (/ 999 6)
  (/ 9 7)
  (/ 999999 7)
  (map get-nine-and-zeros (range 10))
  (map first-nines-zeros-divides-evenly (range 1 10))
  (quot (* 100 (/ 1 6)) 1)
  (contains? [])
  (repeating-decimal-length (/ 1 7))
  (take 10 (decimals (/ 1 6)))
  (euler-26)
  (double (/ 1 983))
  (count {:a :b}))

(defn quadratic
  [a b]
  (fn [n] (+ (* n n) (* a n) b)))

(def prime-pool-limit 2000000)

(def prime-pool
  (into #{} (primes-to prime-pool-limit)))

(defn is-prime?
  [n]
  (if (> n prime-pool-limit)
    (throw (Exception. (str "Number is too big: " n)))
    (contains? prime-pool n)))

(defn all-quadratics
  []
  (for [a (range -999 1000)
        b (range -1000 1001)]
    [a b]))

(defn length-of-primes
  [quadratic]
  (->> (range)
       (map quadratic)
       (take-while is-prime?)
       count))

(defn length-of-primes-ab
  [[a b]]
  (length-of-primes (quadratic a b)))

(defn euler-27 []
  (->> (all-quadratics)
       (apply max-key length-of-primes-ab)))

(comment
  "Euler 27"
  ((quadratic 1000 1000) 1000)
  "Reasonably assuming that the sequence is 1000 or less, we can calculate the
   primes up to about 2000000 and use that to check if our numbers are prime
   could also do a probabalistic primality test after that but let's see if
   we can get away with it"
  (is-prime? 17)
  (is-prime? 234982)
  (is-prime? 123456789991)
  (length-of-primes (quadratic 1 41))
  (take 3 (all-quadratics))
  (euler-27)
  (length-of-primes-ab [-61 971])
  (* -61 971))

(defn sum-of-corners-for-layer
  [n]
  (if (< n 1)
    (throw (Exception. "sum-of-corners-for-layer only valid for n >= 1"))
    (+ (* 16 n n) (* 4 n) 4)))

(defn sum-of-diagonals-at-layer
  [n]
  (->> (range 1 (inc n))
       (map sum-of-corners-for-layer)
       (reduce +)
       (+ 1)))

(defn sum-of-diagonals-at-layer-analytic
  [n]
  (bigint (+ (* 16/3 n n n) (* 10 n n) (* 26/3 n) 1)))

(defn layer-index-for-edge-length
  [n]
  (/ (- n 1) 2))

(defn euler-28 []
  (->> 1001
       layer-index-for-edge-length
       sum-of-diagonals-at-layer-analytic))

(comment
  "Euler 28"
  "Spiral diagonals:
   For layer (like an onion) number N > 0
   * Total number of entries in layer = 1, 9, 16, ... = (2N + 1)^2 - (2N - 1)^2 =
     4N^2 - 4N^2 + 4N + 4N + 1 - 1 = 8N
   * Total number in entire square = sum(layers_in(i)) from i = 0 to i = n
     (plus one for center) = 1 + 8(n(n+1)/2) = 1 + 4n(n+1) = 4n^2 + 4n + 1
   * Upper right number = Total in whole square
   * Upper left number = total in whole square - (edge length - 1)
     = T(n) - (2N + 1 - 1) = T(n) - 2N = 4n^2 + 2n + 1
   * Lower left = T(n) - 2(edge length - 1) = T(n) - 4n = 4n^2 + 1
   * Lower right = T(n) - 3(edge length -1) = T(n) = 6n = 4n^2 - 2n + 1
   * Sum of diagonoals for a layer = 4(4n^2) + 4n + 4 = 16n^2 + 4n + 4
   * Sum of all diagonals for layers = 1 + sum(16i^2 + 4i + 4) from i = 1 to n
     16sum(i^2) + 4sum(i) + 4sum(1) =
     1 + 16(n^3/3 + n^2/2 + n/6) + 4(n^2/2 + n/2) + 4n =
     16n^3/3 + 10n^2 + n(16/6 + 4/2 + 4) + 1 =
     (16/3)n^3 + 10n^2 + (26/3)n + 1
   Let's try it: n = 0 -> sum is 0, off to a good start
   n = 1 -> sum is 16/3 + 5 + 26/3 = 5 + 42 / 3 = 5 + 12 = 27, two off
   n = 2 -> sum is 16 * 8 / 3 + 20 + 52 / 3 = 128 / 3 + 52 / 3 + 20 = 180 / 3 + 20 = 80, 21 off"
  (+ 25 21 17 13 7 9 5 3 1)
  (+ 25 21 17 13)
  "Real sums of corners for n = 1, n = 2: [24 76]"
  (map sum-of-corners-for-layer (range 1 3))
  (= [24 76] (map sum-of-corners-for-layer (range 1 3))) ; true
  "Real sums of diagonals at layer for n = 0, n = 1, ... = [1 25 101]"
  (= [1 25 101] (map sum-of-diagonals-at-layer (range 3))) ; true
  (= [1 25 101] (map sum-of-diagonals-at-layer-analytic (range 3))) ; true
  (map layer-index-for-edge-length [1 3 5 7])
  (->> 5 layer-index-for-edge-length sum-of-diagonals-at-layer-analytic)
  (euler-28))

(defn euler-29 []
  (->> (for [a (range 2 101)
             b (range 2 101)]
         (Math/pow a b))
       (into #{})
       count))

(comment
  "Euler 29"
  (euler-29))

(def max-possible-fifth-power-digit-sum
  (int (* 5 (Math/pow 9 5))))

(defn is-sum-of-power-of-digits?
  [power]
  (fn [n]
    (->> (digits-big n)
         (map #(Math/pow % power))
         (map bigint)
         (reduce +)
         (= n))))

(defn euler-30 []
  (->> (range 2 max-possible-fifth-power-digit-sum)
       (filter (is-sum-of-power-of-digits? 5))
       (reduce +)))

(comment
  "Euler 30"
  (* 5 (Math/pow 9 5))
  ((is-sum-of-power-of-digits? 4) 1634)
  (euler-30))

(def uk-currency-in-p
  #{1 2 5 10 20 50 100 200})

; This actually gives all the permutations, not the combinations
(def ways-to-make-top-down
  (memoize
   (fn [denominations amount]
     (cond
       (zero? amount) 1 ; The last coin had us hit the mark exactly
       (< amount 0) 0 ; The last coin was too big
       ; Otherwise add up all ways to make after taking one of each kind
       :else (->> denominations
                  (map #(ways-to-make-top-down denominations (- amount %)))
                  (reduce +'))))))

; Similar structure as before for combinations (top-down, tree recursive)
; but we think of it as a series of choices for the largest coin type
; "Do I take coin A?" yes/no, and so on
; By going one coin type at a time each combination of coins will be
; counted just once, as there's only one representation of that
; combination in the sorted coins
; By going largest to smallest we improve performance and max stack
; depth
(def coin-combinations
  (memoize
   (fn [[max-denomination & rest :as denominations] amount]
     (cond
       (< amount 0) 0 ; The last coin overshot
       (zero? amount) 1 ; The last coin was right on the money (ha)
       (empty? denominations) 0 ; Ran out of coin types
       :else (+ (coin-combinations denominations (- amount max-denomination))
                (coin-combinations rest amount))))))

(defn coin-combinations-memo-clean
  [denominations amount]
  (let [iter-fn (memoize
                 (fn [self [max-denomination & rest :as denominations] amount]
                   (cond
                     (< amount 0) 0 ; The last coin overshot
                     (zero? amount) 1 ; The last coin was right on the money (ha)
                     (empty? denominations) 0 ; Ran out of coin types
                     :else (+ (self self denominations (- amount max-denomination))
                              (self self rest amount)))))]
    (iter-fn iter-fn denominations amount)))

(defn coin-combinations-no-memo
  [[max-denomination & rest :as denominations] amount]
    (cond
      (< amount 0) 0 ; The last coin overshot
      (zero? amount) 1 ; The last coin was right on the money (ha)
      (empty? denominations) 0 ; Ran out of coin types
      :else (+ (coin-combinations-no-memo denominations (- amount max-denomination))
               (coin-combinations-no-memo rest amount))))

(defn euler-31 []
  (-> (sort > uk-currency-in-p)
      (coin-combinations 200)))

;; Follow-up, understanding bottom-up recursive approach
;; Visualize fn dependencies as a matrix:
;; Amount left on one axis
;; Denomination (current) on the other axis
;; Number of ways to make are the values
;; For a given amount+denomination, fn dependencies go only
;; towards 0 amount and towards smallest denomination
;; Along the denomination axis, dependencies go by 1 (don't choose coin)
;; Along the amount axis, dependencies go by denomination amount
;; Then the thing to do is to calculate the entire array (skipping 
;; a few quantities at the beginning), which happens in linear time
;; for a fixed set of denominations
(defn coin-combinations-bottom-up
  [ordered-denominations amount]
  (loop [prev-amounts (vec (cons 1 (repeat amount 0))) ; "prev amounts" for first row are zero, save for the 0 amount
         d-amount (first ordered-denominations)
         remaining-amounts (rest ordered-denominations)]
    (if (nil? d-amount) ; Out of coins
      (get prev-amounts amount)
      (-> (loop [a d-amount ; Start at the coin denom so we don't need special-case checking
                 working-ways prev-amounts] ; Being very clever here - we need the ways for amounts [0, denom) to be the ways from the previous denomination, as this denom's coin won't fit but there's still potentially non-zero ways to make that amount
            (if (> a amount)
              working-ways ; Calculated all ways for this denom, including amount
              (recur (inc a)
                     (assoc working-ways a
                            (+ (get working-ways (- a d-amount)) ; Ways using this coin
                               (get prev-amounts a)))))) ; Ways not using this coin
          (recur (first remaining-amounts) (rest remaining-amounts))))))

(defn coin-combinations-bottom-up-transient
  [^longs ordered-denominations ^long amount]
  (loop [^longs prev-amounts (transient (vec (cons 1 (repeat amount 0)))) ; "prev amounts" for first row are zero, save for the 0 amount
         ^long d-amount (first ordered-denominations)
         ^longs remaining-amounts (rest ordered-denominations)]
    (if (nil? d-amount) ; Out of coins
      (get prev-amounts amount)
      (-> (loop [^long a d-amount ; Start at the coin denom so we don't need special-case checking
                 ^longs working-ways prev-amounts] ; Being very clever here - we need the ways for amounts [0, denom) to be the ways from the previous denomination, as this denom's coin won't fit but there's still potentially non-zero ways to make that amount
            (if (> a amount)
              working-ways ; Calculated all ways for this denom, including amount
              (recur (inc a)
                     (assoc! working-ways a
                             (+ (get working-ways (- a d-amount)) ; Ways using this coin
                                (get prev-amounts a)))))) ; Ways not using this coin, hasn't been overwritten yet
          (recur (first remaining-amounts) (rest remaining-amounts))))))

(def sorted-uk-currency-in-p
  (sort uk-currency-in-p))

(comment
  "Euler 31"
  "Trick is this:
   You have denominations d_1, d_2, etc.
   d_1 is an amount (say d_1 pence, like 2p)
   Number of ways to make amount A =
     (number of ways to make amount (A - d_1)) +
     (number of ways to make amount (A - d_2)) +
     ...
   Base case is when amount gets small so only one coin
   (or no coins) can fit
   With top-down dynamic programming approach it's important
   to memoize the calls, otherwise you do a lot of duplicate
   calculation (overlapping subproblems)
   Bottom-up dynamic programming approach is not so obvious...
   "
  (ways-to-make-top-down uk-currency-in-p 4)
  (ways-to-make-top-down uk-currency-in-p 200)
  "Ah, this seems to do all the permutations (1p + 2p) and (2p + 1p) counted separately
   Maybe we can get all the permutations grouped by number of coins,
   then divide each group by the number of permutations of that number
   of coins (deduping them) then sum all that up
   Maybe some way to build a map of {number_of_coins number_of_ways} up?"
  (cons 1 nil)
  (euler-31) ; 73682, correct
  (quick-bench (coin-combinations (sort > uk-currency-in-p) 200)) ; 4.75us (but because it's already memoized, uh)
  (quick-bench (coin-combinations (sort < uk-currency-in-p) 200)) ; 4.95us
  (quick-bench (coin-combinations-no-memo (sort > uk-currency-in-p) 200)) ; 300ms, dramatically slower
  (quick-bench (coin-combinations-memo-clean (sort > uk-currency-in-p) 200)) ; 1.1ms, more accurate
  (quick-bench (coin-combinations-memo-clean (sort < uk-currency-in-p) 200)) ; 4.1ms, significantly slower because of worse memoization
  (sort uk-currency-in-p)
  (coin-combinations-bottom-up [1 2] 4)
  (coin-combinations-bottom-up (sort uk-currency-in-p) 200) ; Correct (whew!)
  (quick-bench (coin-combinations-bottom-up (sort uk-currency-in-p) 200)) ; 493us - faster than 1.1ms above
  (coin-combinations-memo-clean sorted-uk-currency-in-p 10000) ; Also, this gives a stack overflow
  (coin-combinations-bottom-up sorted-uk-currency-in-p 10000) ; And this gives the answer lickety-split (1133873304647601, correct)
  (coin-combinations-bottom-up-transient sorted-uk-currency-in-p 10000)
  (quick-bench (coin-combinations-bottom-up sorted-uk-currency-in-p 200)) ; 323us
  (quick-bench (coin-combinations-bottom-up-transient sorted-uk-currency-in-p 200)) ; 199us, 182us, 219us -> transients and type hints saved us some time
  )

; There's a more efficient (early-returning) form
; using reduce, but this should suffice
(defn all-unique-elements?
  [coll]
  (= (count coll) (count (into #{} coll))))

(defn all-digits-unique?
  [num]
  (-> num
      digits-big
      all-unique-elements?))

(defn all-digits-together-unique?
  [nums]
  (->> nums
       (mapcat digits-big)
       all-unique-elements?))

; Adapted from euler-1-15/proper-divisors
(defn middle-divisor-pairs
  [n]
  (let [sqrt-n (Math/sqrt n)]
    (->> (range 2 (long (inc sqrt-n)))
         (map (div-pair n))
         (filter (partial (comp is-whole? second)))
         (into #{}))))

(defn has-exactly-1-9?
  [coll]
  (and (= 9 (count coll))
       (= #{1 2 3 4 5 6 7 8 9} (into #{} coll))))

(defn nums-1-9-pandigital-together?
  [nums]
  (->> nums
       (mapcat digits-big)
       has-exactly-1-9?))

; We just need to consider the proper divisors, without one,
; since a number and itself will have duplicates
(defn is-1-9-pandigital?
  [num]
  (->> num
       middle-divisor-pairs
       (map (partial cons num))
       (filter nums-1-9-pandigital-together?)
       first
       (not= nil)))

(defn euler-32 []
  (->> (range 1 100000)
       (filter is-1-9-pandigital?)
       (reduce +)))

(comment
  "Euler 32"
  "Max product is below 987654321 (actually smaller, but use as upper bound)"
  (all-unique-elements? [1 2 3])
  (all-digits-unique? 123)
  (all-digits-unique? 111)
  (middle-divisor-pairs 16)
  (all-digits-together-unique? [12 34 56])
  (all-digits-together-unique? [12 23 51])
  (= #{1 2 3} (into #{} [1 2 3]))
  (is-1-9-pandigital? 7254)
  (is-1-9-pandigital? 7255)
  "Okay, need a smaller bound. Largest pandigital...
   single digit x single digit = 2 or 1 digit
   3 digit x 3 digit = 5 or 6 digit
   2 digit x 4 digit = <6 digit
   Can limit search for products to 6 digits
   Still too slow, let's limit further
   5 digits? That finishes in a sane amount of time"
  (* 999 999)
  (* 99 9999)
  (euler-32))

(defn common-digits
  [n d]
  (intersection (into #{} (digits-big n))
                (into #{} (digits-big d))))

; Trying this in a LISP-y immediatly invoked anonymous fn
(defn assemble-decimal
  [digits]
  ((fn [num [digit & rest-digits]]
     (if-not digit
       num
       (recur (+' (*' 10 num) digit) rest-digits))) 0 digits))

(defn remove-digit
  [digit]
  (fn [num]
    (->> (digits-big num)
         (filter (partial not= digit))
         assemble-decimal)))

(defn remove-digit-equals-same?
  [n d]
  (fn [digit]
    (let [[n' d'] (map (remove-digit digit) [n d])]
      (when (pos? d')
        (= (/ n d) (/ n' d'))))))

(defn is-curious-fraction?
  [n d]
  (let [common (common-digits n d)]
    (when (pos? (count common))
      (some (remove-digit-equals-same? n d) common))))

(defn is-curious-nontrivial?
  [n d]
  (not-every? (divisible-by? 10) [n d]))

(defn euler-33 []
  (->> (for [d (range 11 100)
             n (range 10 d)]
         [n d])
       (filter (partial apply is-curious-fraction?))
       (filter (partial apply is-curious-nontrivial?))
       (map (partial apply /))
       (reduce *)
       denominator))

(comment
  "Euler 33"
  "Walk all n, d where n < d and n and d in [10, 100)
   Find common digit (if any)"
  (common-digits 12 12)
  (common-digits 11 22)
  (numerator 2/10)
  "Not so good to use the Rational datatype here, since
   it reduces the fraction for us. Normally nice, but
   we need to deal with the original numbers"
  (assemble-decimal [1 2 3])
  ((remove-digit-equals-same? 49 98) 9)
  ((remove-digit-equals-same? 12 26) 2)
  (is-curious-fraction? 49 98)
  (is-curious-fraction? 12 26)
  (filter (partial apply is-curious-fraction?) [[12 26] [49 98]])
  (is-curious-nontrivial? 10 20)
  (is-curious-nontrivial? 49 98)
  (* 16/64 26/65 19/95 49/98)
  (euler-33))

(defn is-curious-number?
  [num]
  (let [digits (digits-big num)]
    (->> digits
         (map factorial)
         (reduce +)
         (= num))))

(def max-candidate-curious-number
  (- (exp 10 7) 1))

(defn euler-34 []
  (->> (range 10 max-candidate-curious-number)
       (filter is-curious-number?)
       (reduce +)))

(comment
  "Euler 34"
  (* (factorial 9))
  (is-curious-number? 145)
  (is-curious-number? 144)
  "Now, what's the biggest possible curious number?"
  (factorial 9)
  "Adding 9s grows the digit sum by +9! but grows the
   original by *10, so eventually num will always be
   bigger than the factorial sum of its digits
   Say number of 9s is n
   decimal(n) = sum(9 * 10^i)) from i = 0 to n - 1
      = 9 * sum(10^i) = 9 * (1 - 10^n) / (1 - 10)
   Sum of digits = n * 9!
   Solve for n in n * 9! = 9 * (1 - 10^n) / -9
   n * 9! = - (1 - 10^n)
   n * 9! = 10^n - 1
   This involves some logs, looks like solutions are n = 0, n ~= 6.25"
  max-candidate-curious-number
  "Seems like only 145 and 40585?"
  "Yep, that's right. Neat"
  (euler-34))

(defn digits
  [num]
  (reverse (digits-big num)))

(defn slice
  [coll start count]
  (->> coll
       (drop start)
       (take count)))

(defn slice-of
  [coll len]
  (fn [start]
    (slice coll start len)))

(defn cycles
  [coll]
  (let [len (count coll)]
    (->> (range len)
         (map (slice-of (cycle coll) len)))))

(defn num-cycles
  [num]
  (->> (digits num)
       cycles
       (map assemble-decimal)))

(defn contains-all-cycles?
  [coll]
  (let [coll-set (into #{} coll)]
    (fn [num]
      (->> (num-cycles num)
           (every? coll-set)))))

(defn euler-35 []
  (let [prime-pool (primes-to 1000000)]
    (->> prime-pool
         (filter (contains-all-cycles? prime-pool))
         count)))

(comment
  "Euler 35"
  (cycles [1 2 3])
  (num-cycles 123)
  (digits 123)
  ((contains-all-cycles? [12 21]) 12)
  ((contains-all-cycles? [12 21]) 21)
  ((contains-all-cycles? [12 21]) 11)
  ((contains-all-cycles? (primes-to 1000000)) 999133)
  (num-cycles 999133)
  (euler-35) ; 55, right
  )

(defn digits-in-base
  [base]
  (fn [n] 
    (loop [n n
           digits nil]
      (if (zero? n)
        digits
        (recur (quot n base) (cons (int (mod n base)) digits))))))

(defn assemble-digits-in-base
  [base]
  (fn [digits]
    (reduce
     (fn [num digit]
       (+' (*' num base) digit))
     0
     digits)))

(defn reversed-num-in-base
  [base]
  (let [digits (digits-in-base base)
        assemble (assemble-digits-in-base base)]
    (fn [n]
      (-> n
          digits
          reverse
          assemble))))

(defn palindrome?-in-base
  [base]
  (let [reverse-num (reversed-num-in-base base)]
    (fn [n]
      (-> n 
          reverse-num
          (= n)))))

(defn euler-36 []
  (let [base-10-palindrome? (palindrome?-in-base 10)
        base-2-palindrome? (palindrome?-in-base 2)]
    (->> (range 1000000)
         (filter base-10-palindrome?)
         (filter base-2-palindrome?)
         (reduce +))))

(defn add-to-front-and-back
  [coll]
  (fn [item]
    (concat [item] coll [item])))

; Note, uses stack depth linear in n, should be good enough for now
; Can memoize inner function - uses a fair bit of heap, but saves a lot of duplicated work
; for the palindromes-in-base fn
(defn palindromes-of-length-for-coll
  [coll]
  (memoize
   (fn palindromes-of-length [length]
     (cond
       (zero? length) [nil]
       (= 1 length) (map vector coll)
       :else (mapcat (fn [palindrome]
                       (map (add-to-front-and-back palindrome)
                            coll))
                     (palindromes-of-length (- length 2)))))))

(defn palindromes-in-base
  [base]
  (let [digits-in-base (range base)
        palindromes-of-length (palindromes-of-length-for-coll digits-in-base)
        assemble-digits (assemble-digits-in-base base)]
    (->> (range)
         (drop 1)
         (mapcat palindromes-of-length)
         (map assemble-digits))))

(defn euler-36-2 []
  (let [base-2-palindrome? (palindrome?-in-base 2)]
    (->> (palindromes-in-base 10)
         (take-while (partial >= 1000000))
         (filter base-2-palindrome?)
         (reduce +))))

(comment
  "Euler 36"
  ((digits-in-base 2) 5)
  ((reversed-num-in-base 10) 1230)
  ((reversed-num-in-base 2) 10)
  ((palindrome?-in-base 10) 112)
  (euler-36) ; 872187, correct
  "From the guide, we can also notice that we can
   generate the palindromes rather than filtering/checking
   So do something like generate a sequence of all base 10
   palindromes, then filter it for only those that are base 2
   This is a lot less work than filtering all target numbers"
  ((palindromes-of-length-for-coll [1 2 3]) 2)
  (take 30 (palindromes-in-base 10))
  (time (count (take 1000000 (palindromes-in-base 10)))) ; Memoization only lasts for life of fn
  (euler-36-2) ; Correct, snappier
  (quick-bench (euler-36)) ; 1.7sec
  (quick-bench (euler-36-2)) ; 15msec, much faster
  )

(defn truncations-left
  [[_ & rest]]
  (when rest
    (conj (truncations-left rest) rest)))

(defn truncations-right
  [coll]
  (map reverse (truncations-left (reverse coll))))

(defn digit-truncations
  [n]
  (let [n-digits (digits n)]
    (conj (concat (truncations-left n-digits)
                  (truncations-right n-digits))
          n-digits)))

(defn truncations
  [n]
  (->> n 
       digit-truncations
       (map (assemble-digits-in-base 10))))

(defn contains-all?-in
  [test-set]
  (fn [coll]
    (every? test-set coll)))

(defn throw-if-not
  [pred message x]
  (if (pred x)
    x
    (throw (Exception. message))))

(defn has-length?-n
  [length]
  (fn [coll]
    (= length (count coll))))

; We know from the problem there's only eleven,
; so picked upper bounds larger and larger until this
; returned eleven values
(defn euler-37 []
  (let [prime-pool (into #{} (primes-to 1000000))
        all-prime? (contains-all?-in prime-pool)
        is-length-11? (has-length?-n 11)]
    (->> prime-pool
         (filter (partial < 9))
         (map truncations)
         (filter all-prime?)
         (map (partial apply max))
         (throw-if-not is-length-11? "Didn't find 11")
         (reduce +))))

(comment
  "Euler 37"
  (truncations-left '(1 2 3))
  (truncations-right [1 2 3])
  (conj nil 1)
  (truncations 123)
  (euler-37))

; I think this has a bug in it, doesn't count 1 and goes too high in n
(defn concatenated-product
  [x n]
  (->> (range 2 (+ 2 n))
       (map (partial * x))
       (mapcat (digits-in-base 10))
       ((assemble-digits-in-base 10))))

(def digits-1-9
  (into #{} (range 1 10)))

(defn has-digits-1-9?
  [num]
  (let [num-digits (into #{} (digits num))]
    (every? num-digits digits-1-9)))

; Notice that concatenated products strictly grow as
; n increases and they always increase without bound
; and quite quickly
(defn largest-concatenated-9-digit-pandigital
  [n]
  (->> (range)
       (drop 1)
       (map (partial concatenated-product n))
       (drop-while (partial > 100000000))
       (take-while (partial > 1000000000))
       (filter has-digits-1-9?)
       ; Numbers here are exactly of length 9 since we filtered
       ; out 10-digit numbers and above
       last))

(defn numbers-starting-with-9
  ([] (cons 9 (numbers-starting-with-9 1)))
  ([order] (concat (range (*' 9 (exp 10 order)) (exp 10 (inc order)))
                   (lazy-seq (numbers-starting-with-9 (inc order))))))

(defn euler-38 []
  (->> (range 1 1000000000)
       (map largest-concatenated-9-digit-pandigital)
       (filter identity) ; Some numbers won't be able to make one
       (apply max)))

(defn number-width
  [n]
  (if (zero? n)
    0
    (-> n
        Math/log10
        Math/ceil
        int)))

(defn shift-left-digits
  [digits number]
  (* number (exp 10 digits)))

; Trying another way that doesn't use vectors
(defn concatenated-product-2
  ([x n] (concatenated-product-2 x n 0))
  ([x n acc]
   (if (zero? n)
     acc
     (let [product (* x n)
           acc-width (number-width acc)]
       (recur x (dec n) (+ acc
                           (shift-left-digits acc-width product)))))))

(defn largest-concatenated-9-digit-pandigital-2
  [n]
  (->> (range)
       (drop 1)
       (map (partial concatenated-product-2 n))
       (take-while (partial > 1000000000))
       (filter #(-> % number-width (= 9)))
       (filter has-digits-1-9?)
       ; Numbers here are exactly of length 9 since we filtered
       ; out 10-digit numbers and above
       last
       (conj [n])))

(defn euler-38-2 []
  (->> (numbers-starting-with-9)
       
       (take-while (partial > 1000000000))
       (filter largest-concatenated-9-digit-pandigital)
       (take 5)
       #_#_#_
       (map largest-concatenated-9-digit-pandigital)
       (filter identity)
       (apply max)))

(defn euler-38-3 []
  (->> (numbers-starting-with-9)
       ; Since n must be >= 2, then we only need to check up to all 4-digit numbers
       ; as five-digit numbers will have pandigitals of 10 digits or more
       (take-while (partial > 100000))
       (map largest-concatenated-9-digit-pandigital-2)
       (filter second)
       (apply max-key second)))

(comment
  "Euler 38"
  (concatenated-product 192 3)
  (is-1-9-pandigital? 123456789)
  (largest-concatenated-9-digit-pandigital 192)
  (has-digits-1-9? 123)
  (has-digits-1-9? 123456789)
  ; Very slow, not done yet...
  (euler-38)
  "Optimization: Only check numbers where 1 * n starts with a 9,
   assuming that the maximum 1-9 pandigital will start with a 9"
  (take 20 (numbers-starting-with-9))
  (time (+ 1 2))
  (largest-concatenated-9-digit-pandigital 9013)
  (time (doall (euler-38-2))) ; 30s to take 5
  (Math/log10 940)
  (concatenated-product-2 192 3)
  (time (doall (euler-38-3))) ; 69ms to get five
  (largest-concatenated-9-digit-pandigital-2 9)
  (concatenated-product-2 9 5)
  (euler-38-3) ; Returns very fast, [9327 932718654]
  (concatenated-product-2 9327 2)
  )


(defn other-leg-for-perimiter
  [p a]
  (/ (- (* p p)
        (* 2 a p))
     (* 2
        (- p a))))

(defn is-valid-leg-solution?
  [b]
  (and (integer? b) (pos? b)))

(defn integer-right-trianges-for-perimiter
  [p]
  (let [get-other-leg (partial other-leg-for-perimiter p)
        small-leg-limit (Math/floor (/ p (+ 2 (Math/sqrt 2))))]
    (->> (range 1 (inc small-leg-limit))
         (map (fn [a] [a (get-other-leg a)]))
         (filter (comp is-valid-leg-solution? second)))))

(defn count-of-integer-right-triangles
  [p]
  (-> p
      integer-right-trianges-for-perimiter
      count))

(defn euler-39 []
  (->> (range 1 1001)
       (apply max-key count-of-integer-right-triangles)))

(comment
  "Euler 39"
  "For a given perimiter p, number of solutions is number
   of unique (a,b,c) where a^2 + b^2 = c^2
   and a + b + c = p
   We can eliminate one of a, b, or c - pick c
   c = p - a - b
   a^2 + b^2 = (p - a - b)^2
   a^2 + b^2 = (p - (a + b))^2
   a^2 + b^2 = p^2 - 2(a+b)p + (a+b)^2
   a^2 + b^2 = p^2 -2(a+b)p + a^2 + 2ab + b^2
   0 = p^2 - 2(a+b)p + 2ab
   0 = p^2 - 2ap - 2bp + 2ab
   p^2 = 2(ap + bp - ab)
   Then given an a you can solve for b, then check the sum
   p^2 = 2(ap + b(p - a))
   p^2 = 2ap + 2b(p - a)
   p^2 - 2ap = 2b(p - a)
   (p^2 - 2ap) / 2(p - a) = b
   ---> (p^2 - 2ap) = (p - 2a)p
   (p/2) (p - 2a) / (p - a)
   That seems easy-ish to check. To avoid duplicates, only run through
   as up to p / sqrt(2)"
   "Another way, c = sqrt(a^2 + b^2)
    a + b + sqrt(a^2 + b^2) = p
    a^2 + b^2 = (p - a - b)^2
    a^2 + b^2 = p^2 - ap - bp - ap + a^2 + ab - bp + ab + b^2
    0 = p^2 - 2ap - 2bp + 2ab
    0 = p^2 - 2ap - 2b(p - a)
    2b(p - a) = p^2 - 2ap
    b = (p^2 - 2ap) / 2 (p - a)"
   (other-leg-for-perimiter 3 12)
   (other-leg-for-perimiter 8 12)
   (Math/floor (/ 12 (Math/sqrt 2)))
   "Actually small leg limit is b s.t.
    2b + sqrt(2)b = p
    b (2 + sqrt(2)) = p
    b = p / (2 + sqrt(2))"
   (integer-right-trianges-for-perimiter 12)
   (integer-right-trianges-for-perimiter 120)
   (euler-39)
   (integer-right-trianges-for-perimiter 840) ; 840 is correct, so many!
   )

(defn concatenated-integers-decimal-digits
  ([] (concatenated-integers-decimal-digits 1))
  ([n] (concat (digits n)
               (lazy-seq (concatenated-integers-decimal-digits (inc n))))))

; Not very efficient, since it iterates over the first part of
; the coll multiple times for subsequent higher n
; However, fast enough for this problem
(defn get-nths
  [nths coll]
  (->> nths
       (map (fn [n] (nth coll n)))))

(defn euler-40 []
  (->> (concatenated-integers-decimal-digits)
       ; Have to decrement because the problem has the first digit starting at 1 instead of 0
       (get-nths (map dec [1 10 100 1000 10000 100000 1000000]))
       (reduce * 1)))

(comment
  "Euler 40"
  "We can build a tail-recursive function that gives the
   nth digits of the fraction
   Or we can build a lazy-seq that gives the digits in
   fixed stack space
   Lazy-seq is a little more expressive"
  (take 20 (concatenated-integers-decimal-digits))
  (reduce * 1 '(2 3 4))
  (euler-40))

(defn is-n-digit-pandigital?
  [n]
  (let [width (number-width n)
        needed-digits (range 1 (inc width))]
    (->> n
         digits
         (into #{})
         (= (into #{} needed-digits)))))

; Does big exponents in logarithmic time, but consumes log(n) stack depth
(defn exp-fast
  ([n pow] (if (zero? pow) 1 (exp-fast n n pow)))
  ([base acc pow]
   (cond
     (= 1 pow) base
     (even? pow) (exp (exp-fast base acc (/ pow 2)) 2)
     :else (*' base (exp-fast base acc (dec pow))))))

; Still top-down, so consumes log(n) stack depth - can handle beyond 1e15 though,
; so good enough for our purposes
(defn exp-mod
  [n pow modulo]
  (cond
    (zero? pow) 1
    (even? pow) (mod (exp (exp-mod n (/ pow 2) modulo) 2) modulo)
    :else (mod (*' n (exp-mod n (dec pow) modulo)) modulo)))

(defn divides?
  [n test-divisor]
  (zero? (rem n test-divisor)))

(defn none-divide?
  [test-divisors n]
  (->> test-divisors
       (filter (partial > (Math/sqrt n)))
       (not-any? (partial divides? n))))

(defn euler-41 []
  (let [max-prime-factor (int (Math/ceil (Math/sqrt 1e10)))
        prime-pool (primes-to max-prime-factor)]
    (->> (range (bigint 1e8) 0 -1)
         (filter is-n-digit-pandigital?)
         (filter (partial none-divide? prime-pool))
         first)))

(defn remove-first
  [x [head & tail]]
  (if tail
    (if (= head x)
      tail
      (cons head (lazy-seq (remove-first x tail))))
    (if (= head x)
      '()
      '(head))))

(defn permutations
  [coll]
  (if (empty? coll)
    (list (list))
    (mapcat (fn [x]
              (map (partial cons x)
                   (permutations (remove-first x coll))))
            coll)))

(defn pandigitals-of-width
  [width]
  (->> (range width 0 -1)
       permutations
       (map (assemble-digits-in-base 10))))

; Results in some duplicated permutations, but works well enough for our purposes
(defn pandigitals-high-to-low
  [start-width]
  (->> (range start-width 0 -1)
       (mapcat pandigitals-of-width)))

(defn euler-41-2 []
  (let [max-prime-factor (int (Math/ceil (Math/sqrt 1e10)))
        prime-pool (primes-to max-prime-factor)]
    (->> (pandigitals-high-to-low 9)
         (filter (partial none-divide? prime-pool))
         first)))

(comment
  "Euler 41"
  "Note that the largest possible pandigital prime is of 9 digits or fewer
   Start by generating all primes up to 9 digits
   Then reverse"
  (is-n-digit-pandigital? 1234)
  (is-n-digit-pandigital? 11)
  (primes-to 100)
  (even? 0)
  (exp 123 4)
  (exp-fast 123 4)
  (exp-fast 123 5)
  (*' 52389094428262881 52389094428262881)
  (exp-fast 123 12345678)
  (time (do (exp-fast 123 10000000) "done")) ; 15s, so no way we can do 9-digit powers
  "Hunch: Exponentiation with large BigNums is slow, when we do the modulo step alongside
   that will keep the numbers smaller so performance will be better"
  (exp-mod 123 10000000000000000 10000000000000001) ; Happens super fast now
  "Ah, notice that all prime factors of a number n are <= sqrt(n)
   So we can calculate all the primes up to sqrt(1e9) then start checking if things
   are pandigital and prime from the top down
   This should be reasonably quick as pandigital check is fast and we won't need to
   check that many numbers for primality"
  (Math/sqrt 1e10)
  (primes-to 10000)
  (is-n-digit-pandigital? 2143)
  (none-divide? (primes-to 100000) 2143)
  (none-divide? (primes-to 100000) 4312)
  (take 10 (primes-to 10000))
  (zero? (rem 4312 2))
  (none-divide? [2] 4312)
  (range (int 1e5) 0 -1)
  (contains? (into #{} (primes-to 5000)) 4231)
  (Math/sqrt 4231)
  (euler-41)
  1e10M
  "Increased the search range one power-of-ten at a time, found 7652413 and that's right
   Searching starting at 1e10 is just very slow... Must be a more clever way to do this
   Probably by generating the pandigital numbers directly rather than testing each one?"
  (time (euler-41))
  (remove-first 1 [1 1 2])
  (remove-first 1 [1])
  (cons 1 nil)
  (permutations [1 1 2])
  (permutations [1 2 3])
  (map (partial cons 1) nil)
  (range 9 0 -1)
  (sort > [1 2 3])
  (= (pandigitals-of-width 4) (sort > (pandigitals-of-width 4)))
  (take 10 (pandigitals-of-width 9))
  (take 10 (pandigitals-high-to-low 9))
  (= (sort > (pandigitals-high-to-low 4)) (pandigitals-high-to-low 4))
  (time (euler-41-2)) ; Nice, finds 7652413 in 4.2s
  )

(def char-to-pos
  (->> (range 97 123)
       (map-indexed (fn [index ascii-value] [(char ascii-value) (inc index)]))
       (into {})))

(defn word-to-num
  [word]
  (->> word
       (map char-to-pos)
       (reduce +)))

(defn calculate-triangle-number
  [pos]
  (* 1/2 pos (+ pos 1)))

(defn triangle-numbers
  []
  (->> (range)
       (drop 1)
       (map calculate-triangle-number)))

; Not nil-safe, but good enough for us
(defn ascending-coll-contains?
  [[head & tail] x]
  (when head
    (cond
      (= head x) true
      (< head x) (recur tail x)
      :else false)))

(defn euler-42 []
  (let [triangles (triangle-numbers) ; We use the same seq here so we don't recalculate
        is-triangle-number (fn [x] (ascending-coll-contains? triangles x))]
    (with-open [reader (io/reader "src/main/clojure/mhs/p042_words.txt")]
      (->> (csv/read-csv reader)
           first
           (map #(.toLowerCase %))
           (map word-to-num)
           (filter is-triangle-number)
           count))))

(comment
  "Euler 42"
  "Triangle numbers strictly increase, so we can stop searching if we pass it
   We can have one lazy sequence of the triangle numbers that slowly gets realized
   as we search farther and farther into the seq"
  char-to-pos
  (word-to-num "sky")
  (take 10 (triangle-numbers))
  (ascending-coll-contains? (triangle-numbers) 55)
  (euler-42))

(defn digit-windows
  [n]
  (->> n
       digits
       (partition 3 1)
       (drop 1)))

(defn window-numbers
  [n]
  (->> n
       digit-windows
       (map (assemble-digits-in-base 10))))

(def first-7-primes
  (primes-to 18))

(defn second-divisible-by-first?
  [[a b]]
  (= 0 (rem b a)))

(defn meets-window-prime-divisibility-condition?
  [n]
  (->> n 
       window-numbers
       (map vector first-7-primes)
       (every? second-divisible-by-first?)))

(defn pandigitals-0-9-of-width
  [width]
  (->> (range (dec width) -1 -1)
       permutations
       (filter (fn [digits] (not= 0 (first digits))))
       (map (assemble-digits-in-base 10))))

(defn euler-43 []
  (->> (pandigitals-0-9-of-width 10)
       (filter meets-window-prime-divisibility-condition?)
       (reduce +)))

(comment
  "Euler 43"
  "There are 10! 0-to-9 pandigitals (counting leading zero), which is about 3.6 million"
  (factorial 10)
  "If we generate them top-to-bottom then that's not too too many to check
   for our prime-window-divisibility condition"
  (digit-windows 123234345)
  (window-numbers 123456789)
  first-7-primes
  (vector 1 2)
  (rem 12 5)
  (meets-window-prime-divisibility-condition? 1406357289)
  (meets-window-prime-divisibility-condition? 1406357288)
  (take 10 (pandigitals-0-9-of-width 10))
  (= (sort > (pandigitals-0-9-of-width 5)) (pandigitals-0-9-of-width 5))
  (range 10 -1 -1)
  (time (euler-43)) ; 16695334890 correct, though takes more than a minute
  "Probably can improve by directly generating the appropriate numbers better, perhaps such as
   generating the windows directly. All 3-digits divisible by two, then for each one of those only
   need to test 10 numbers to find the next digit that makes the next 3-digit window divisible by 3, etc."
  17264606346
  ; Problem: Generating ones that start with zero, which are not 0-9 pandigital
  (->> '(4160357289 4130952867 4106357289 1460357289 1430952867 1406357289 419635728 149635728)
      (reduce +')))

(defn pentagonal-for-index
  [n]
  (* n (- (* 3 n) 1) 1/2))

(defn pentagonal-numbers
  []
  (->> (range)
       (drop 1)
       (map pentagonal-for-index)))

(defn pentagonal-numbers-descending
  [index]
  (->> (range index 0 -1)
       (map pentagonal-for-index)))

; Not sure how theoretically sound this is...
#_
(defn is-whole?
  [num]
  (== num (bigint num)))

(defn is-pentagonal? 
  [num]
  (is-whole? (/ (+ 1/2
                   (Math/sqrt (+ 1/4 (* 6 num))))
                3)))

(defn sum-and-difference-is-pentagonal?
  [p1 p2]
  (and (is-pentagonal? (+ p1 p2))
       (is-pentagonal? (- p2 p1))))

(defn smallest-pentagonal-triangle-iter
  [top-index smallest-diff]
  (let [p-top (pentagonal-for-index top-index)
        candidates (->> (range (dec top-index) 0 -1)
                        (map (fn [bottom-index] [bottom-index (pentagonal-for-index bottom-index)]))
                        (take-while (fn [[_ p-bottom]] (< (- p-top p-bottom) smallest-diff)))
                        (filter (fn [[_ p-bottom]] (sum-and-difference-is-pentagonal? p-bottom p-top)))
                        (map (fn [[bottom-index p-bottom]] [top-index bottom-index (- p-top p-bottom)])))]
    (when-not (empty? candidates)
      (apply min-key (fn [[_ _ diff]] diff) candidates))))

(defn smallest-pentagonal-triangle
  ([] (smallest-pentagonal-triangle 2 [nil nil ##Inf]))
  ([top-index [best-top best-bottom smallest-diff]]
   (when (zero? (rem top-index 10))
     (println top-index best-top best-bottom smallest-diff))
   (if (< smallest-diff
          (- (pentagonal-for-index top-index)
             (pentagonal-for-index (dec top-index))))
     [best-top best-bottom smallest-diff]
     (if-let [[better-top better-bottom better-diff]
              (smallest-pentagonal-triangle-iter top-index smallest-diff)]
       (recur (inc top-index) [better-top better-bottom better-diff])
       (recur (inc top-index) [best-top best-bottom smallest-diff])))))

(comment 
  "Euler 44"
  "When we're searching, we can start with the larger pentagonal
   number and then search downward with the other
   We can stop then downward phase when the difference gets larger
   than the minimum difference we've found so far
   We can also stop when the difference between the top pentagonal
   number and the next lowest one is larger than the minimal difference
   that we found, since it's impossible for D to get smaller"
  (* 4 1/2)
  (take 10 (pentagonal-numbers))
  "Could keep a big set of all pentagonal numbers to test if sum/difference
   is pentagonal, but let's see if we can have some kind of analytic solution
   P = n(3n - 1)/2 = (3n^2 - n)/2
   0 = (3/2)n^2 - (1/2)n - P
   Quadratic formula
   n = ((1/2) +- sqrt(1/4 + 4(3/2)P))/3
   ... sqrt(1/4 + 6P)
   Don't need to worry about the minus side"
  (= 6.0 6)
  (== 6.0 6)
  (== 6.0 6N)
  (bigint 6.2)
  (is-pentagonal? 5)
  (apply max (take 1000 (pentagonal-numbers)))
  (= (filter is-pentagonal? (range 1 1499501)) (take 1000 (pentagonal-numbers))) ; true, so works at least for first 1000 pentagonal numbers
  Double/POSITIVE_INFINITY
  ##Inf
  (smallest-pentagonal-triangle-iter 5 ##Inf)
  (smallest-pentagonal-triangle)
  )

(defn rest-if-next-element-equals
  [val seq]
  (if (= val (first seq))
    (rest seq)
    seq))

(defn matching-elements-in-ordered
  [& seqs]
  (when (every? seq seqs)
    (let [[next :as nexts] (map first seqs)]
      (if (apply = nexts)
        (cons next
              (lazy-seq (apply matching-elements-in-ordered (map rest seqs))))
        (let [min-next (apply min nexts)]
          (recur (map (partial rest-if-next-element-equals min-next) seqs)))))))

(defn hexagonal-numbers
  []
  (->> (range)
       (drop 1)
       (map (fn [n] (* n (- (* 2 n) 1))))))

(defn euler-45 []
  (->> (matching-elements-in-ordered (hexagonal-numbers)
                                     (pentagonal-numbers)
                                     (triangle-numbers))
       (take 3)))

(comment
  "Euler 45"
  "Since these numbers grow quickly, it's advantageous to
   not check every single number but rather keep skipping
   along the sequences until we find one that's equal in
   all"
  (doall (matching-elements-in-ordered [1 4 8] [2 4 9]))
  (take 1 (matching-elements-in-ordered [1 4 8] [2 4 9]))
  (cons 1 nil)
  (euler-45) ; (1 40755 1533776805) returns very quickly
  )

(defn half-of-remainder-is-square?
  [high low]
  (-> (- high low) (/ 2) (Math/sqrt) is-whole?))

(defn meets-goldbach-other-conjecture?
  [ordered-primes odd-composite]
  (->> ordered-primes
       (take-while (partial > odd-composite))
       (some (partial half-of-remainder-is-square? odd-composite))))

; Trial-and-error on upper prime limit should be sufficient
(defn euler-46 []
  (let [search-limit 10000
        prime-pool (primes-to search-limit)
        prime-set (into #{} prime-pool)]
    (->> (range 3 (inc search-limit) 2)
         (filter (complement prime-set))
         (filter (complement (partial meets-goldbach-other-conjecture? prime-pool)))
         first)))

(comment 
  "Euler 46"
  "Should be able to tell by checking each odd composite number and checking for
   all primes less than it if half the remainder is a perfect square"
  (half-of-remainder-is-square? 25 7)
  (meets-goldbach-other-conjecture? 25 (primes-to 25))
  (euler-46) ; Returns quickly, 5777
  )

; Only realizes up to size+1 elements of the lazy-seq
; Needs to potentially realize size+1 so we can tell if there's too many
(defn of-size?
  [size lazy-seq]
  (= size
     (->> lazy-seq (take (inc size)) count)))

(defn has-n-prime-factors?
  [prime-pool num-factors num]
  (->> prime-pool
       (take-while (partial > (int (Math/ceil (/ num 2)))))
       (filter (partial divides? num))
       (of-size? num-factors)))

(defn first-run-matches
  ([pred coll size] (first-run-matches pred coll size []))
  ([pred [head & tail] size run]
   (if (= size (count run))
     run
     (when tail
       (if (pred head)
         (recur pred tail size (conj run head))
         (recur pred tail size []))))))

; Again, trial-and-error for primes limit
(defn euler-47 []
  (let [prime-pool (primes-to 1000)]
    (first-run-matches (partial has-n-prime-factors? prime-pool 4)
                       (range)
                       4)))

(comment
  "Euler 47"
  "With all primes less than a number, we can avoid a full prime factorization of each number
   by checking all primes <= sqrt(n) to get the number of distinct primes"
  (take 10 [1 2])
  (has-n-prime-factors? (primes-to 100) 3 32)
  (first-run-matches (partial < 5) (range) 5)
  (first-run-matches (partial < 5) [1 2 3 4] 5)
  (primes-to 10)
  (euler-47) ; Takes a second, but 134043 is correct
  (->> (primes-to 1000)
       (take-while (partial > (int (Math/ceil (Math/sqrt 645))))))
  (all-factors 645)
  (all-factors 215)
  (all-factors 129))

(defn euler-48 []
  (->> (range 1 1000)
       (map (fn [n] (exp-mod n n 10000000000)))
       (reduce +')))

(comment
  "Euler 48"
  "Notice that no digits above the last 10 can influence the
   last 10 digits at any intermediate step in an exponentiation
   I think this means we can use the exp mod procedure to get the
   last 10 digits of each exponent
   Then we can do an addition (and mod) to get the last 10"
  (euler-48) ; Outputs 4629110846700N, last 10 digits is correct
  )

(defn arithmetic-sequences
  [b]
  (->> (range 1 (/ (- 10000 b) 2))
       (map (fn [a] [a b]))))

(defn sequence-terms
  [[a b]]
  (->> (range)
       (map (fn [n] (+ (* a n) b)))))

; Be lazy, reuse is-prime? above which uses a prime-pool way above
; what we need here
(defn all-are-prime?
  [coll]
  (every? is-prime? coll))

(defn are-all-permutations-of-each-other?
  [coll]
  (->> coll
       (map (digits-in-base 10))
       (map (fn [digits] (into #{} digits)))
       (apply =)))

(defn euler-49 []
  (let [prime-pool (primes-to 10000)
        candidate-bs (drop-while (partial > 1000) prime-pool)]
    (->> candidate-bs
         (mapcat arithmetic-sequences)
         (map sequence-terms)
         (map (partial take 3))
         (filter all-are-prime?)
         (filter are-all-permutations-of-each-other?))))

(comment
  "Euler 49"
  "Arithmetic sequences are formed by a * n + b
   Since we only care about 4-digit primes, we could limit our search to
   b in [1000, 9999ish] a in (1, (10000 - b) / 2) sort of bounds
   That wouldn't be so so bad, but we can do better by only considering
   starting bs that are themselves prime and four digits
   Since all three terms must be four digits, we can also limit our search
   in such a way to exlude numbers that cause subsequent terms to go over
   four digits, hence the (10000 - b) / 2 upper bound on the as"
  (are-all-permutations-of-each-other? [123 321 213])
  (are-all-permutations-of-each-other? [123 234 345])
  (arithmetic-sequences 1487)
  (euler-49) ; Returns ((1487 4817 8147) (2969 6299 9629)) in a few seconds, correct
  )

(defn prefixes
  [coll]
  (->> (range 1 (inc (count coll)))
       (map (fn [len] (take len coll)))))

(defn suffixes
  [coll]
  (->> (range (count coll))
       (map (fn [len] (drop len coll)))))

; Can't reuse is-prime? here because we need some sums above 200,000
(def sum-is-prime?
  (let [primes (into #{} (primes-to 1000000))
        is-prime? (fn [n] (contains? primes n))]
    (fn [coll]
      (->> coll (reduce +) is-prime?))))

(defn euler-50 []
  (let [prime-pool (primes-to 10000)]
    (->> prime-pool
         suffixes
         (mapcat prefixes)
         (filter sum-is-prime?)
         (map (fn [coll] [(first coll) (count coll) (reduce + coll)]))
         (apply max-key second))))

(comment
  "Euler 50"
  "Given an ascending list of primes less than 1000000, can start with
   the first one and add one prime, then add two, then three, and so on,
   until we exceed 1,000,000
   Basically, construct all subsequences starting at index n, then
   filter them based on if they sum to a prime and then sort the
   filtered ones by their length
   We can limit this a bit by having the upper limit be much smaller,
   because we know there will be one sequence of at least 21 terms, so
   we can limit our search to 1,000,000 / 21 ~= 50,000 (should be a little above this, but should be fine)
   This is approximately an n^2 operation, so 2.5 billion operations,
   that's a lot"
  (prefixes [1 2 3])
  (suffixes [1 2 3])
  (sum-is-prime? [2 3 5 7 11 13])
  (->> [1 2 3 4]
       suffixes
       (mapcat prefixes))
  (time (euler-50)) ; Caused an OOM or hangs when trying to do all the way to 50k
  ; Going to 10k takes 87secs, returns same result as using 4k though (which returns in 7.5 sec)
  )
