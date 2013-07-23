(ns overtone-playground.markov
  (:use [clojure.math.numeric-tower]))

(defn nMatElem [index mat] (cond
  (seq (drop-last index)) (nth (nMatElem (drop-last index) mat) (last index))
  :else (nth mat (first index))))

(defn matElem [r c mat] (nMatElem [r c] mat))

(defn prob [n i]
  (fn [p] (- (/ (- (+ (expt p (+ (- n i) 1)) (expt p (+ i 2))) (* p (+ p 1))) (- p 1)) 1)))

(defn probDeriv [n i]
  (fn [p] (/ (+ (* (- p 1) (+ (* (+ (- n i) 1) (expt p (- n i))) (- (- (* (+ i 2) (expt p (+ i 1))) (* 2 p)) 1))) (- (- (* p (+ p 1)) (expt p (+ (- n i) 1))) (expt p (+ i 2)))) (expt (- p 1) 2))))

(defn make-improver [n i]
  (fn [p] (- p (/ ((prob n i) p) ((probDeriv n i) p)))))

(defn make-good-enough [n i tolerance]
  (fn [p] (< (abs ((prob n i) p)) tolerance)))

(defn iterative-improve [p improve good?]
  (if (good? p) p
    (iterative-improve (improve p) improve good?)))

(defn getProb [n i]
  (iterative-improve 0.5 (make-improver n i) (make-good-enough n i 0.00000000001)))

(defn markovRow
  [n i]
  (let [p (getProb n i)]
    (map (fn [k] (expt p (+ 1 (abs k)))) (range (- i) (- n i)))))

(defn markovMatrix
  "Builds an n by n probability matrix for a Markov chain."
  [n]
  (map (fn [i] (markovRow n i)) (range 0 n)))

(defn prob2 [n i]
  (fn [p] (+ (- (/ (- (+ (expt p (+ (- n i) 1)) (expt p (+ i 2))) (* 2 (expt p 2))) (- p 1)) 1) (expt p n))))

; UPDATE THIS
(defn probDeriv2 [n i]
  (fn [p] (+ (/ (- (* (- p 1) (- (+ (* (+ (- n i) 1) (expt p (- n i))) (* (+ i 2) (expt p (+ i 1))) (* 4 p))) (- (+ (expt p (+ (- n i) 1)) (expt p (+ i 2)))) (* 2 (expt p 2))) (expt (- p 1) 2)) (* n (expt p (- n 1)))) (* n (expt p (- n 1))))))

(defn make-improver2 [n i]
  (fn [p] (- p (/ ((prob2 n i) p) ((probDeriv2 n i) p)))))

(defn make-good-enough2 [n i tolerance]
  (fn [p] (< (abs ((prob2 n i) p)) tolerance)))

(defn getProb2 [n i]
  (iterative-improve 0.5 (make-improver2 n i) (make-good-enough2 n i 0.000000001)))

(defn markovRow2
  [n i]
  (let [p (getProb2 n i)]
    (map (fn [k] (expt p (cond 
                           (= k 0) n
                           :else (+ 1 (abs k))
                               ))) (range (- i) (- n i)))))

(defn markovMatrix2
  [n]
  (map (fn [i] (markovRow2 n i)) (range 0 n)))

(defn getNextState
  [probVector]
  (let [s (rand)
        sample (fn sample [total index vect]
          (cond
            (< s (+ (first vect) total)) index
            :else (sample (+ (first vect) total) (inc index) (rest vect))
          )
  )]
  (sample 0 0 probVector)))

(defn markovTransition
  [state probMatrix]
  (getNextState (nMatElem (list state) probMatrix)))

(defn markovList
  [n state probMatrix]
  (cond
    (<= n 0) ()
    :else (let [nextState (markovTransition state probMatrix)]
            (cons nextState (markovList (dec n) nextState probMatrix)))
  ))

(defn dot [a b]
  (do
    (assert (= (count a) (count b)))
    (map (fn [x] (* (nth a x) (nth b x))) (range (count a)))))

