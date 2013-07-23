(ns overtone-playground.note)

; List of all notes
(def notes [:C0 :C#0 :D0 :D#0 :E0 :F0 :F#0 :G0 :G#0 :A0 :A#0 :B0
            :C1 :C#1 :D1 :D#1 :E1 :F1 :F#1 :G1 :G#1 :A1 :A#1 :B1
            :C2 :C#2 :D2 :D#2 :E2 :F2 :F#2 :G2 :G#2 :A2 :A#2 :B2
            :C3 :C#3 :D3 :D#3 :E3 :F3 :F#3 :G3 :G#3 :A3 :A#3 :B3
            :C4 :C#4 :D4 :D#4 :E4 :F4 :F#4 :G4 :G#4 :A4 :A#4 :B4
            :C5 :C#5 :D5 :D#5 :E5 :F5 :F#5 :G5 :G#5 :A5 :A#5 :B5
            :C6 :C#6 :D6 :D#6 :E6 :F6 :F#6 :G6 :G#6 :A6 :A#6 :B6
            :C7 :C#7 :D7 :D#7 :E7 :F7 :F#7 :G7 :G#7 :A7 :A#7 :B7
            :C8 :C#8 :D8 :D#8 :E8 :F8 :F#8 :G8 :G#8 :A8 :A#8 :B8
            :C9 :C#9 :D9 :D#9 :E9 :F9 :F#9 :G9 :G#9 :A9 :A#9 :B9])

; Note tests
(defn C?  [n] (= (mod n 12) 0))
(defn C#? [n] (= (mod n 12) 1))
(defn D?  [n] (= (mod n 12) 2))
(defn D#? [n] (= (mod n 12) 3))
(defn E?  [n] (= (mod n 12) 4))
(defn F?  [n] (= (mod n 12) 5))
(defn F#? [n] (= (mod n 12) 6))
(defn G?  [n] (= (mod n 12) 7))
(defn G#? [n] (= (mod n 12) 8))
(defn A?  [n] (= (mod n 12) 9))
(defn A#? [n] (= (mod n 12) 10))
(defn B?  [n] (= (mod n 12) 11))

; Provided scale must be in format e.g [:C :D ... :B]
(defn makeInScale? [scale]
  (let [tests (map (fn [nSym] 
                     (cond
                       (= nSym :C ) C?
                       (= nSym :C#) C#?
                       (= nSym :D ) D?
                       (= nSym :D#) D#?
                       (= nSym :E ) E?
                       (= nSym :F ) F?
                       (= nSym :F#) F#?
                       (= nSym :G ) G?
                       (= nSym :G#) G#?
                       (= nSym :A ) A?
                       (= nSym :A#) A#?
                       (= nSym :B ) B?)) scale)]
    (fn [n] (reduce (fn [others isNote?] 
                      (or others (isNote? n))) 
                    false
                    tests))))

(def chromatic (list :C :C# :D :D# :E :F :F# :G :G# :A :A# :B))

; steps should be in the format e.g [1 2 2 1 1 ... ]
; startNote should be e.g. :C
(defn scaleTest [startNote steps]
  (makeInScale?
    (do
      (assert (= (reduce (fn [x y] (+ x y)) steps) 12))
      (cons startNote 
            (map (fn [n] (nth chromatic n)) 
                 (reductions + (butlast steps)))))))

(defn majorScaleTest [startNote]
  (scaleTest startNote [2 2 1 2 2 2 1]))

(defn weightProbScaleNotes [p scaleTest noteProbs]
  (let [totalScaleProb (reduce (fn [total i] (+ (cond 
                                               (scaleTest i) (nth noteProbs i)
                                               :else 0) total)) 0 (range (count noteProbs)))
        weight (/ p totalScaleProb)
        notWeight (/ (- 1 p) (- 1 totalScaleProb))]
    (map (fn [i] 
           (let [note (nth noteProbs i)]
             (cond
               (scaleTest i) (* note weight)
               :else (* note notWeight))))
         (range (count noteProbs)))))
