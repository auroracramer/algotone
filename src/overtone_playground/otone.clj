(ns overtone-playground.otone
  (:use [overtone.live]))



(defn play-pattern [nome beat-num pattern instr]
  (cond
    (empty? pattern) (do)
    :else (do
      (at (nome (+ beat-num)) (instr (midi->hz (first pattern))))
      (play-pattern nome (inc beat-num) (rest pattern) instr))))

(definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     vol))


