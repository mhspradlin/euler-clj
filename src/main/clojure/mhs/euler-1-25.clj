(ns mhs.euler-1-25
  (:require [criterium.core :refer [bench with-progress-reporting quick-bench]]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn divisible-by?
  [n]
  (fn [x]
    (= 0
       (rem x n))))

(defn euler-1
  []
  (->> (range)
       (filter (some-fn (divisible-by? 3) (divisible-by? 5)))
       (take-while (partial > 1000))
       (reduce +)))

(defn fib
  ([] (cons 1 (cons 2 (fib 1 2))))
  ([n0 n1] (let [next (+ n0 n1)]
             (lazy-seq (cons next (fib n1 next))))))

(defn euler-2
  []
  (->> (fib)
       (take-while (partial > 4000000))
       (filter even?)
       (reduce +)))

(defn divides?
  [n]
  (fn [x]
    (= 0
       (rem n x))))

(defn all-factors
  [n]
  (->> (range 1 (/ n 2))
       (filter (divides? n))))

(defn none-divide?
  [nums x]
  (every? (complement (divides? x)) nums))

(comment
  (none-divide? [2 3 4] 12)
  (none-divide? [2 3 4] 5))

(defn primes
  ([] (primes [] 2))
  ([ps next] (if (none-divide? ps next)
               (lazy-seq (cons next (primes (conj ps next) (inc next))))
               (recur ps (inc next)))))

(defn largest-divisor
  [n]
  (->> (range (long (Math/floor (/ n 2))) 0 -1)
       (filter (divides? n))
       (first)))

(def euler-3-num 600851475143)
#_(def euler-3-num 49)


(defn euler-3-2
  ([] (euler-3-2 2 euler-3-num))
  ([last-num remainder]
   (println last-num remainder)
   (if (= 1 remainder)
     last-num
     (if (= 0 (rem remainder last-num))
       (recur last-num (/ remainder last-num))
       (recur (inc last-num) remainder)))))

(defn euler-3
  []
  (let [ps (take-while (partial > (/ euler-3-num 2)) (primes))]
    (->> (range (long (Math/floor (/ euler-3-num 2))) 0 -1)
         (filter (divides? euler-3-num))
         (filter #(= 1 (largest-divisor %)))
         (first))))

(defn digits
  [n]
  (if (= n 0)
    []
    (conj (digits (unchecked-divide-int n 10)) (mod n 10))))

(defn is-palindrome?
  [n]
  (let [ds (digits n)]
    (= ds (reverse ds))))

(defn euler-4
  []
  (->> (for [n0 (range 100 1000)
             n1 (range 100 1000)]
         [n0 n1])
       (map (partial reduce *))
       (into (sorted-set-by >))
       (filter is-palindrome?)
       (first)))

; Just need something that shares all the prime factors
; of all the numbers between 1 and 20 I think
; Plus a few duplicates because of numbers like 4, needs two
; twos
(defn euler-5
  []
  (* 2 3 2 5 7 2 3 11 13 2 17 19))

(defn square
  [n]
  (Math/pow n 2))

(defn sum-squares
  [nums]
  (->> nums
       (map square)
       (reduce +)))

(defn square-sum
  [nums]
  (->> nums
       (reduce +)
       square))

(defn euler-6
  []
  (let [sum-sq (sum-squares (range 1 101))
        sq-sum (square-sum (range 1 101))]
    (println sum-sq)
    (println sq-sum)
    (- sum-sq sq-sum)))

(defn prime-sieve
  ([] (prime-sieve [] 2))
  ([primes next] (if (none-divide? primes next)
                   (lazy-seq (cons next (prime-sieve (conj primes next) (inc next))))
                   (recur primes (inc next)))))

(comment
  (take 2 (prime-sieve))
  (take 6 (prime-sieve))
  "Euler 7"
  (last (take 10001 (prime-sieve))))

(def euler-8-number
  (->> (str "73167176531330624919225119674426574742355349194934"
            "96983520312774506326239578318016984801869478851843"
            "85861560789112949495459501737958331952853208805511"
            "12540698747158523863050715693290963295227443043557"
            "66896648950445244523161731856403098711121722383113"
            "62229893423380308135336276614282806444486645238749"
            "30358907296290491560440772390713810515859307960866"
            "70172427121883998797908792274921901699720888093776"
            "65727333001053367881220235421809751254540594752243"
            "52584907711670556013604839586446706324415722155397"
            "53697817977846174064955149290862569321978468622482"
            "83972241375657056057490261407972968652414535100474"
            "82166370484403199890008895243450658541227588666881"
            "16427171479924442928230863465674813919123162824586"
            "17866458359124566529476545682848912883142607690042"
            "24219022671055626321111109370544217506941658960408"
            "07198403850962455444362981230987879927244284909188"
            "84580156166097919133875499200524063689912560717606"
            "05886116467109405077541002256983155200055935729725"
            "71636269561882670428252483600823257530420752963450")
       vec
       (map str)
       (map #(Integer/parseInt %))))

(defn largest-product
  ([window-size num-digits] (largest-product window-size [] 0 num-digits))
  ([window-size window-digits largest num-digits]
   (cond
     (< (count window-digits) window-size)
     (recur window-size (conj window-digits (first num-digits)) largest (rest num-digits))
     (empty? num-digits) largest
     :else (let [next-digit (first num-digits)
                 new-window (cons next-digit (butlast window-digits))
                 new-product (reduce * new-window)]
             (recur window-size new-window (max new-product largest) (rest num-digits))))))

(comment
  "Euler 8"
  (vec "123")
  (->> (vec "123")
       (map str)
       (map #(Integer/parseInt %)))
  (largest-product 2 [2 3 4 1])
  (count euler-8-number)
  (largest-product 2 euler-8-number)
  (largest-product 13 euler-8-number))

(defn is-whole?
  [n]
  (= 0.0 (mod n 1.0)))

(defn is-triple?
  [[_ _ c]]
  (is-whole? c))

(defn sums-to?
  [sum]
  (fn [ns]
    (= (int sum) (int (reduce + ns)))))

; Goes out in "wedge", starting at x axis, going up until x = y, then going to the next x
(defn pos-int-pairs
  ([] (pos-int-pairs 1 1))
  ([x y]
   (let [[x' y'] (cond
                   (= x y) [(inc x) 1]
                   (> x y) [x (inc y)])]
     (lazy-seq (cons [x y] (pos-int-pairs x' y'))))))

(defn create-triple
  [[a b]]
  [a b (Math/hypot a b)])

(defn pythagorean-triple-sum
  [sum]
  (->> (pos-int-pairs)
       (map create-triple)
       (filter is-triple?)
       (filter (sums-to? sum))
       (first)
       (reduce *)))

(comment
  "Euler 9"
  (Math/hypot 3 4)
  (mod 5.1 1.0)
  (mod 5.0 1.0)
  ((sums-to? 12) [3 4 5.0])
  (take 10 (pos-int-pairs))
  (is-triple? [3 4 (Math/hypot 3 4)])
  (pythagorean-triple-sum 1000))

; More optimizations possible:
; Stop when n crosses sqrt(bound), as these will not eliminate any more numbers 
;   less than the bound
; (not sure) Use a transient vector of boolean flags to assoc! true/false rather
;   than doing filter(v) to build up intermediate results
(defn bounded-prime-sieve
  ([bound] (bounded-prime-sieve [] (range 2 bound)))
  ([ps ns] (if-let [n (first ns)]
             (let [filtered-ns (filterv (complement (divisible-by? n)) (rest ns))]
               (recur (conj ps n) filtered-ns))
             ps)))

; Optimizations:
; Stops after reaching sqrt(n)
; Instead of filtering all numbers greater than n, starts with n^2
; Instead of filtering all numbers greater than n^2, skips up in increments of n
; Uses a single vector and updates it in-place (transient vec)
; Note: For getting really big primes in the future, will want to implement a
; primality test like fermat's test. In overview it suggested using a fast sieve to
; get primes up to something like 10^5 then using that list for probabalistic primality
; tests
(defn bounded-prime-sieve-2
  ([bound] (bounded-prime-sieve-2 (transient (into [] (range 2 bound))) 0 (int (Math/sqrt bound)) bound))
  ([nums i i-bound bound] (if (= i i-bound)
                            (filter (complement nil?) (persistent! nums))
                            (if (nth nums i)
                              (let [; Start at square, since smaller multiples are already eliminated
                                    n (+ i 2)
                                    filtered-nums (loop [nums' nums
                                                         j (* n n)]
                                                    (if (>= j bound)
                                                      nums'
                                                      (recur (assoc! nums' (- j 2) nil) (+ j n))))]
                                (recur filtered-nums (inc i) i-bound bound))
                              (recur nums (inc i) i-bound bound)))))

; From testing in the repl, appears to be about .5x speed of using transient version
(defn bounded-prime-sieve-persistent
  ([bound] (bounded-prime-sieve-persistent (into [] (range 2 bound)) 0 (int (Math/sqrt bound)) bound))
  ([nums i i-bound bound] (if (= i i-bound)
                            (filter (complement nil?) nums)
                            (if (nth nums i)
                              (let [; Start at square, since smaller multiples are already eliminated
                                    n (+ i 2)
                                    filtered-nums (loop [nums' nums
                                                         j (* n n)]
                                                    (if (>= j bound)
                                                      nums'
                                                      (recur (assoc nums' (- j 2) nil) (+ j n))))]
                                (recur filtered-nums (inc i) i-bound bound))
                              (recur nums (inc i) i-bound bound)))))

; See bounded-prime-sieve-2 - same thing but with type hints. The hints don't actually seem to
; provide any speedup
(defn primes-to
  ([^long bound] (primes-to (transient (into [] (range bound))) 2 (long (Math/sqrt bound)) bound))
  ([nums ^long i ^long i-bound ^long bound] (if (= i i-bound)
                                              (drop 2 (filter (complement nil?) (persistent! nums)))
                                              (if (nth nums i)
                                                (let [; Start at square, since smaller multiples are already eliminated
                                                      filtered-nums (loop [nums' nums
                                                                           j (bit-shift-left i 1)]
                                                                      (if (>= j bound)
                                                                        nums'
                                                                        (recur (assoc! nums' j nil) (+ j i))))]
                                                  (recur filtered-nums (inc i) i-bound bound))
                                                (recur nums (inc i) i-bound bound)))))

(defn euler-10
  []
  (->> (primes-to 2000000)
       (reduce +)))

(comment
  "Euler 10"
  (filter (complement (divisible-by? 10)) (range 30))
  (bounded-prime-sieve 20)
  (time (bounded-prime-sieve 1000000)) ; 
  (time (into [] (filter (complement (divisible-by? 123456)) (range 123456 1234567))))
  (take 20 (prime-sieve))
  (= (bounded-prime-sieve 72) (take 20 (prime-sieve)))
  (time (bounded-prime-sieve-2 2000000)) ; Wayy faster, takes 800ms, then 500ms
  (time (bounded-prime-sieve-persistent 2000000)); A bit slower than using transient vector, takes 1776ms, then 1143ms
  (time (primes-to 2000000))
  (= (bounded-prime-sieve-2 2000000) (primes-to 2000000))
  (with-progress-reporting (bench (primes-to 2000000) :verbose)) ; According to Criterion, this is about 20% faster than prime-sieve-2 (445ms vs 557ms)
  (with-progress-reporting (bench (bounded-prime-sieve-2 2000000) :verbose))
  (time (euler-10))); Takes 52 seconds with slow sieve, outputs 142913828922 (correct), takes only 1164ms with fast sieve (587ms on second run)

(def euler-11-rows
  [[8 2 22 97 38 15 0 40 0 75 4 5 7 78 52 12 50 77 91 8]
   [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 4 56 62 0]
   [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 3 49 13 36 65]
   [52 70 95 23 4 60 11 42 69 24 68 56 1 32 56 71 37 2 36 91]
   [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
   [24 47 32 60 99 3 45 2 44 75 33 53 78 36 84 20 35 17 12 50]
   [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
   [67 26 20 68 2 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21]
   [24 55 58 5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
   [21 36 23 9 75 0 76 44 20 45 35 14 0 61 33 97 34 31 33 95]
   [78 17 53 28 22 75 31 67 15 94 3 80 4 62 16 14 9 53 56 92]
   [16 39 5 42 96 35 31 47 55 58 88 24 0 17 54 24 36 29 85 57]
   [86 56 0 48 35 71 89 7 5 44 44 37 44 60 21 58 51 54 17 58]
   [19 80 81 68 5 94 47 69 28 73 92 13 86 52 17 77 4 89 55 40]
   [4 52 8 83 97 35 99 16 7 97 57 32 16 26 26 79 33 27 98 66]
   [88 36 68 87 57 62 20 72 3 46 33 67 46 55 12 32 63 93 53 69]
   [4 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36]
   [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 4 36 16]
   [20 73 35 29 78 31 90 1 74 31 49 71 48 86 81 16 23 57 5 54]
   [1 70 54 71 83 51 54 69 16 92 33 48 61 43 52 1 89 19 67 48]])

(defn columns
  [rows]
  (->> (range (count (first rows)))
       (map (fn [i] (map (fn [row] (nth row i)) rows)))))

(defn ne-sw-diagonals
  [rows]
  (let [skew-width (dec (count rows))]
    (->> rows
         (map-indexed (fn [i row] (concat (repeat i 0) row (repeat (- skew-width i) 0))))
         columns)))

(defn nw-se-diagonals
  [rows]
  (let [skew-width (dec (count rows))]
    (->> rows
         (map-indexed (fn [i row] (concat (repeat (- skew-width i) 0) row (repeat i 0))))
         columns)))

(defn euler-11-seq-max
  [seqs]
  (apply max (map (partial largest-product 4) seqs)))

(defn euler-11
  []
  (let [row-max (euler-11-seq-max euler-11-rows)
        col-max (euler-11-seq-max (columns euler-11-rows))
        ne-sw-max (euler-11-seq-max (ne-sw-diagonals euler-11-rows))
        nw-se-max (euler-11-seq-max (nw-se-diagonals euler-11-rows))]
    (max row-max col-max ne-sw-max nw-se-max)))

(comment
  "Euler 11"
  (map (partial largest-product 4) euler-11-rows)
  (columns euler-11-rows)
  (ne-sw-diagonals euler-11-rows)
  (nw-se-diagonals euler-11-rows)
  (euler-11-seq-max (ne-sw-diagonals euler-11-rows))
  (apply max (map (partial largest-product 4) (nw-se-diagonals euler-11-rows)))
  (euler-11))

(defn triangle-numbers
  ([] (triangle-numbers 1 2))
  ([tri nat] (cons tri (lazy-seq (triangle-numbers (+ tri nat) (inc nat))))))

; Check up to sqrt(n), multiply yeses by two
; Also add one if sqrt(n) is whole
(defn num-divisors
  [n]
  (->> (range 1 (long (Math/sqrt n)))
       (filter (divides? n))
       count
       (* 2)
       (+ (if (is-whole? (Math/sqrt n)) 1 0))))

(defn has-gt-n-divisors?
  [n]
  (fn [num]
    (> (num-divisors num) n)))

(defn euler-12
  []
  (->> (triangle-numbers)
       (filter (has-gt-n-divisors? 500))
       (first)))

(comment
  "Euler 12"
  (take 10 (triangle-numbers))
  ((divisible-by? 4) 28)
  (is-whole? (Math/sqrt 8))
  (num-divisors 16)
  (euler-12))

(def euler-13-nums
  [37107287533902102798797998220837590246510135740250
   46376937677490009712648124896970078050417018260538
   74324986199524741059474233309513058123726617309629
   91942213363574161572522430563301811072406154908250
   23067588207539346171171980310421047513778063246676
   89261670696623633820136378418383684178734361726757
   28112879812849979408065481931592621691275889832738
   44274228917432520321923589422876796487670272189318
   47451445736001306439091167216856844588711603153276
   70386486105843025439939619828917593665686757934951
   62176457141856560629502157223196586755079324193331
   64906352462741904929101432445813822663347944758178
   92575867718337217661963751590579239728245598838407
   58203565325359399008402633568948830189458628227828
   80181199384826282014278194139940567587151170094390
   35398664372827112653829987240784473053190104293586
   86515506006295864861532075273371959191420517255829
   71693888707715466499115593487603532921714970056938
   54370070576826684624621495650076471787294438377604
   53282654108756828443191190634694037855217779295145
   36123272525000296071075082563815656710885258350721
   45876576172410976447339110607218265236877223636045
   17423706905851860660448207621209813287860733969412
   81142660418086830619328460811191061556940512689692
   51934325451728388641918047049293215058642563049483
   62467221648435076201727918039944693004732956340691
   15732444386908125794514089057706229429197107928209
   55037687525678773091862540744969844508330393682126
   18336384825330154686196124348767681297534375946515
   80386287592878490201521685554828717201219257766954
   78182833757993103614740356856449095527097864797581
   16726320100436897842553539920931837441497806860984
   48403098129077791799088218795327364475675590848030
   87086987551392711854517078544161852424320693150332
   59959406895756536782107074926966537676326235447210
   69793950679652694742597709739166693763042633987085
   41052684708299085211399427365734116182760315001271
   65378607361501080857009149939512557028198746004375
   35829035317434717326932123578154982629742552737307
   94953759765105305946966067683156574377167401875275
   88902802571733229619176668713819931811048770190271
   25267680276078003013678680992525463401061632866526
   36270218540497705585629946580636237993140746255962
   24074486908231174977792365466257246923322810917141
   91430288197103288597806669760892938638285025333403
   34413065578016127815921815005561868836468420090470
   23053081172816430487623791969842487255036638784583
   11487696932154902810424020138335124462181441773470
   63783299490636259666498587618221225225512486764533
   67720186971698544312419572409913959008952310058822
   95548255300263520781532296796249481641953868218774
   76085327132285723110424803456124867697064507995236
   37774242535411291684276865538926205024910326572967
   23701913275725675285653248258265463092207058596522
   29798860272258331913126375147341994889534765745501
   18495701454879288984856827726077713721403798879715
   38298203783031473527721580348144513491373226651381
   34829543829199918180278916522431027392251122869539
   40957953066405232632538044100059654939159879593635
   29746152185502371307642255121183693803580388584903
   41698116222072977186158236678424689157993532961922
   62467957194401269043877107275048102390895523597457
   23189706772547915061505504953922979530901129967519
   86188088225875314529584099251203829009407770775672
   11306739708304724483816533873502340845647058077308
   82959174767140363198008187129011875491310547126581
   97623331044818386269515456334926366572897563400500
   42846280183517070527831839425882145521227251250327
   55121603546981200581762165212827652751691296897789
   32238195734329339946437501907836945765883352399886
   75506164965184775180738168837861091527357929701337
   62177842752192623401942399639168044983993173312731
   32924185707147349566916674687634660915035914677504
   99518671430235219628894890102423325116913619626622
   73267460800591547471830798392868535206946944540724
   76841822524674417161514036427982273348055556214818
   97142617910342598647204516893989422179826088076852
   87783646182799346313767754307809363333018982642090
   10848802521674670883215120185883543223812876952786
   71329612474782464538636993009049310363619763878039
   62184073572399794223406235393808339651327408011116
   66627891981488087797941876876144230030984490851411
   60661826293682836764744779239180335110989069790714
   85786944089552990653640447425576083659976645795096
   66024396409905389607120198219976047599490197230297
   64913982680032973156037120041377903785566085089252
   16730939319872750275468906903707539413042652315011
   94809377245048795150954100921645863754710598436791
   78639167021187492431995700641917969777599028300699
   15368713711936614952811305876380278410754449733078
   40789923115535562561142322423255033685442488917353
   44889911501440648020369068063960672322193204149535
   41503128880339536053299340368006977710650566631954
   81234880673210146739058568557934581403627822703280
   82616570773948327592232845941706525094512325230608
   22918802058777319719839450180888072429661980811197
   77158542502016545090413245809786882778948721859617
   72107838435069186155435662884062257473692284509516
   20849603980134001723930671666823555245252804609722
   53503534226472524250874054075591789781264330331690])

(comment
  "Euler 13"
  euler-13-nums
  (type (first euler-13-nums)) ; BigInt
  (reduce + euler-13-nums)) ; 5537376230390876637302048746832985971773659831892672N)

(defn next-collatz
  [n]
  (if (even? n)
    (/ n 2)
    (inc (* 3 n))))

(defn collatz
  ([n] (collatz [] n))
  ([cs n] (if (= 1 n)
            (conj cs n)
            (recur (conj cs n) (next-collatz n)))))

(defn collatz-length
  [n]
  (if (= 1 n)
    1
    (inc (collatz-length (next-collatz n)))))

(defn collatz-length-2
  ([n] (collatz-length-2 0 n))
  ([len n] (if (= 1 n)
             (inc len)
             (recur (inc len) (next-collatz n)))))

;; I don't think this will actually work, since the recur within collatz-length-2 
;; doesn't reference this
(def memo-collatz-length
  (memoize collatz-length-2))

(defn euler-14-1
  []
  (->> (range 1 1000000)
       (apply max-key collatz-length-2)))

(defn euler-14-2
  []
  (->> (range 1 1000000)
       (apply max-key memo-collatz-length)))

(comment
  "Euler 14"
  (next-collatz 13)
  (collatz 13)
  (collatz-length 13)
  (time (map collatz-length (range 1000))) ; StackOverflow
  (collatz-length-2 13)
  (collatz-length-2 0)
  (time (map collatz-length-2 (range 1 1000000)))
  (time (map memo-collatz-length (range 1 1000000)))
  (euler-14-1)
  (euler-14-2)
  (bench (euler-14-1) :verbose)
  (bench (euler-14-2) :verbose))

; Classic top-down dynamic programming approach
; Has max stack depth of x + y, but only x * y time complexity
(def lattice-paths
  (memoize
   (fn [x y]
     (cond
       (= 0 x y) 1
       (= 0 x) (lattice-paths x (dec y))
       (= 0 y) (lattice-paths (dec x) y)
       :else (+' (lattice-paths x (dec y))
                 (lattice-paths (dec x) y))))))

(defn sum-adjacents
  [known x y]
  (cond
    (= 0 x y) 1
    (= 0 x) (get known [x (dec y)])
    (= 0 y) (get known [(dec x) y])
    :else (+' (get known [(dec x) y])
              (get known [x (dec y)]))))

; First keep y constant and increase x (up along the x-axis)
; Then when hit x-bound, go back and keep x constant and increase along the y-axis
; Then go back to the start for the next "vee"
(defn next-xy
  [x y x-bound y-bound]
  (cond
    (= x-bound x) [y (inc y)]
    (= y-bound y) [(inc x) (inc x)]
    (> y x) [x (inc y)]
    :else [(inc x) y]))

; Tail-recursive bottom-up dynamic programming approach
; Goes out in 'vees', only calculating answers for points
; that we know can be calculated
; Has fixed stack consumption, x * y time complexity
(defn lattice-paths-2
  ([x y] (lattice-paths-2 {} 0 0 x y))
  ([known x y x-bound y-bound]
   (let [new-paths (sum-adjacents known x y)
         [next-x next-y] (next-xy x y x-bound y-bound)]
     (cond
       (and (= x x-bound) (= y y-bound)) new-paths
       :else (recur (assoc known [x y] new-paths) next-x next-y x-bound y-bound)))))

(defn lattice-paths-2-transient
  ([x y] (lattice-paths-2-transient (transient {}) 0 0 x y))
  ([known x y x-bound y-bound]
   (let [new-paths (sum-adjacents known x y)
         [next-x next-y] (next-xy x y x-bound y-bound)]
     (cond
       (and (= x x-bound) (= y y-bound)) new-paths
       :else (recur (assoc! known [x y] new-paths) next-x next-y x-bound y-bound)))))

(comment
  "Euler 15"
  (lattice-paths 2 2)
  (lattice-paths 20 20) ; 137846528820, correct
  (map (partial apply next-xy) [[1 1 2 2] [1 2 3 3] [2 1 3 3] [1 2 2 2]])
  (lattice-paths-2 20 20) ; Correct
  (quick-bench (lattice-paths 20 20)) ; mean 518ns before auto-promote, 574ns after auto-promote
  (quick-bench (lattice-paths-2 20 20)) ; mean 604 microseconds (much slower!)
  (get (transient {:a 1}) :a)
  (quick-bench (lattice-paths-2-transient 20 20)) ; mean 549 microsends (a little faster, still 1000x slower)
  (lattice-paths 1400 1400) ; Can calculate this (VERY large number)
  (lattice-paths 1500 1500) ; StackOverflow
  (lattice-paths-2 1400 1400) ; Can calculate this, noticably slower
  (lattice-paths-2-transient 1400 1400)
  (lattice-paths-2-transient 2000 2000) ; Can calculate this, no StackOverflow (but oh so slow)
  )

; Could use numeric-tower, but not totally in the spirit
; Uses the auto-promoting multiplication *'
(defn exp
  ([base pow] (exp 1 base pow))
  ([acc base pow] (if (= 0 pow)
                    acc
                    (recur (*' acc base) base (dec pow)))))

; Digits, but uses BigNum-safe quot and uses fixed stack space
(defn digits-big
  ([n] (digits-big [] n))
  ([digits n]
   (if (= 0 n)
     digits
     (recur (conj digits (int (mod n 10))) (quot n 10)))))

(defn euler-16
  []
  (->> (exp 2 1000)
       digits-big
       (reduce +')))

(comment
  "Euler 16"
  (exp 2 2)
  (digits 123)
  (exp 2 1000)
  ; mod works with BigNums
  (mod (exp 2 1000) 10)
  ; quot works with BigNums
  (quot (exp 2 1000) 10)
  (= 0 (quot 1 10))
  (digits-big (exp 2 1000))
  (euler-16))

(defn select-place
  [n place]
  (mod (quot n place) 10))

(defn add-thousand
  [[str n]]
  (if (= 1000 n)
    ["one thousand" n]
    [str n]))

(def ones-word
  {1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"})

(defn add-hundreds
  [[word n]]
  (let [hundreds (select-place n 100)]
    (if (= 0 hundreds)
      [word n]
      [(str word (get ones-word hundreds) " hundred") n])))

(def tens-word
   {2 "twenty"
    3 "thirty"
    4 "forty"
    5 "fifty"
    6 "sixty"
    7 "seventy"
    8 "eighty"
    9 "ninety"})

(def teens-word
  {10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"})

(defn add-tens-and-ones
  [[word n]]
  (let [tens (select-place n 10)
        ones (rem n 10)
        prefix (if (> n 100) " and " "")]
    (cond
      (= 0 tens ones) [word n]
      (= 0 tens) [(str word prefix (get ones-word ones)) 0]
      (= 1 tens) [(str word prefix (get teens-word (mod n 100))) 0]
      (= 0 ones) [(str word prefix (get tens-word tens)) 0]
      :else [(str word prefix (get tens-word tens) "-" (get ones-word ones)) 0])))

(defn make-word
  [n]
  (-> ["" n]
      add-thousand
      add-hundreds
      add-tens-and-ones
      first))

(defn character-count
  [word]
  (->> word
       (filter #(Character/isLetter %))
       (apply str)
       count))

(defn euler-17
  []
  (->> (range 1 1001)
       (map make-word)
       (map character-count)
       (reduce +)))

(comment
  "Euler 17"
  (quot 300 100)
  (add-hundreds ["" 300])
  (rem (quot 103 10) 10)
  (select-place 113 10)
  (select-place 1100 100)
  (make-word 502)
  (make-word 112)
  (make-word 342)
  (character-count (make-word 342))
  (->> (range 1 1001)
       (map make-word)
       reverse)
  (euler-17)
  (add-tens-and-ones ["" 103]))

(def euler-18-nums
  [[75]
   [95 64]
   [17 47 82]
   [18 35 87 10]
   [20 4 82 47 65]
   [19 1 23 75 3 34]
   [88 2 77 73 7 63 67]
   [99 65 4 28 6 16 70 92]
   [41 41 26 56 83 40 80 70 33]
   [41 48 72 33 47 32 37 16 94 29]
   [53 71 44 65 25 43 91 52 97 51 14]
   [70 11 33 28 77 73 17 78 39 68 17 57]
   [91 71 52 38 17 14 91 43 58 50 27 29 48]
   [63 66 4 68 89 53 67 30 73 16 69 87 40 31]
   [4 62 98 27 23 9 70 98 73 93 38 53 60 4 23]])

; Top-down dynamic programming approach
; Max recursion depth of height of pyramid
; Bottom-up approach with fixed stack space would go from the bottom
; of the pyramid up
(def maximum-total
  (memoize
   (fn
     ([pyramid] (maximum-total [0 0] pyramid))
     ([[row col] pyramid]
      (let [height (count pyramid)
            num (get-in pyramid [row col])]
        (cond
          (= (dec height) row) num
          :else (+ num
                   (max (maximum-total [(inc row) col] pyramid)
                        (maximum-total [(inc row) (inc col)] pyramid)))))))))

(comment
  "Euler 18"
  euler-18-nums
  (maximum-total [[3] [1 4]])
  (maximum-total euler-18-nums))

; Could write down all the rules for days in month, leap years, etc.
; But easier just to use Java's ZonedDateTime
(defn days-after
  [zoned-date-time]
  (cons zoned-date-time (lazy-seq (days-after (.plus zoned-date-time 1 java.time.temporal.ChronoUnit/DAYS)))))

(defn euler-19
  []
  (->> (days-after (java.time.ZonedDateTime/parse "1901-01-01T00:00:00Z"))
       (take-while #(.isAfter (java.time.ZonedDateTime/parse "2001-01-01T00:00:00Z") %))
       (filter #(= 1 (.getDayOfMonth %)))
       (map #(.getDayOfWeek %))
       (filter #(.equals java.time.DayOfWeek/SUNDAY %))
       count))

(comment
  "Euler 19"
  (take 10 (days-after (java.time.ZonedDateTime/parse "1901-01-01T00:00:00Z")))
  (euler-19))

; Constant stack space, auto-promoting multiply
(defn factorial
  ([n] (factorial 1 n))
  ([total n] (if (zero? n) total (recur (*' n total) (dec n)))))

(defn euler-20
  []
  (->> (factorial 100)
       digits-big
       (reduce +')))

(comment
  "Euler 20"
  (factorial 3)
  (factorial 100)
  (euler-20))

(defn div-pair
  [n]
  (fn [x] [x (/ n x)]))
              
(defn proper-divisors
  [n]
  (let [sqrt-n (Math/sqrt n)]
    (->> (range 2 (long (inc sqrt-n)))
         (map (div-pair n))
         (filter (partial (comp is-whole? second)))
         (apply concat)
         (cons 1)
         (into #{}))))

(defn is-amicable?
  [n]
  (let [divisor-sum (reduce + (proper-divisors n))]
    (and (not= n divisor-sum)
         (= n (reduce + (proper-divisors divisor-sum))))))

(defn euler-21
  []
  (->> (range 1 10000)
       (filter is-amicable?)
       (reduce +)))

(comment
  "Euler 21"
  ((comp is-whole? second) [0 1])
  (proper-divisors 15)
  (proper-divisors 284)
  (is-amicable? 220)
  (euler-21))

(defn euler-22-names
  []
  (with-open [reader (io/reader "src/main/clojure/mhs/p022_names.txt")]
    (->> (csv/read-csv reader)
         doall
         first)))

(def character-value
  {\a 1
   \b 2
   \c 3
   \d 4
   \e 5
   \f 6
   \g 7
   \h 8
   \i 9
   \j 10
   \k 11
   \l 12
   \m 13
   \n 14
   \o 15
   \p 16
   \q 17
   \r 18
   \s 19
   \t 20
   \u 21
   \v 22
   \w 23
   \x 24
   \y 25
   \z 26})

(defn alphabetical-value
  [word]
  (->> word
       string/lower-case
       (map character-value)
       (reduce +)))

(defn alphabetical-score
  [index item]
  (* (inc index) (alphabetical-value item)))

(defn euler-22
  []
  (->> (euler-22-names)
       sort
       (map-indexed alphabetical-score)
       (reduce +)))

(comment
  "Euler 22"
  (euler-22-names)
  (alphabetical-value "COLIN")
  (euler-22))

(def is-abundant?
  (memoize
   (fn [n]
     (< n (reduce + (proper-divisors n))))))

; Primitive way, check all numbers up to half of n to see if it matches
(defn not-sum-two-abundant?
  [n]
  (->> (range 12 (inc (long (/ n 2))))
       (filter is-abundant?)
       (map (partial - n))
       (filter is-abundant?)
       empty?))

(defn euler-23
  []
  (->> (range 1 28124)
       (filter not-sum-two-abundant?)
       (reduce +)))

(comment
  "Euler 23"
  (is-abundant? 12)
  (not-sum-two-abundant? 23)
  (not-sum-two-abundant? 24)
  (map not-sum-two-abundant? (range 21823 21900))
  (euler-23))

; Observation: ordered permutations of [n0, n1, ...] are equal to
; min(nums) : ordered_permutations(rest(nums))
; second_smalleset(nums) : ordered_permutations(rest(nums))
; ...
(defn lexicographic-permutations
  [nums]
  (if (not-empty nums)
    (->> (sort nums)
         (mapcat (fn [n] (map (partial cons n)
                              (lazy-seq (lexicographic-permutations
                                         (filter (partial not= n) nums)))))))
    '(())))

(defn euler-24
  []
  (nth (lexicographic-permutations (range 10)) 999999))

(comment
  "Euler 24"
  (sort [2 1 3])
  (filter (partial not= 1) [1 2])
  (map (partial cons 1) [[2]])
  (mapcat (fn [n] (map (partial cons n) [[2]])) [1 2])
  (lexicographic-permutations [2 1 3])
  (sort (range 10000))
  (nth (lexicographic-permutations (range 10)) 999999)
  (euler-24))

(defn fib-big
  ([] (cons 1 (cons 2 (fib-big 1 2))))
  ([n0 n1] (let [next (+' n0 n1)]
             (lazy-seq (cons next (fib-big n1 next))))))

(defn euler-25
  []
  (->> (fib-big)
       (map digits-big)
       (map-indexed (fn [index digits] [index (count digits)]))
       (filter (fn [[index count]] (= 1000 count)))
       first
       first
       (+ 2)))

(comment
  "Euler 25"
  (nth (fib-big) 1000000)
  (take 10 (fib-big))
  (euler-25))