(ns flatpack.core
  (:use [overtone.live]
        [overtone.inst.piano]
        [overtone.inst.synth])
  (:require [flatpack.grid-display :as disp]
            [flatpack.launchpad :as lp]))

(defn launchpad-handler [{:keys [event key] :as m}]
  (if m
    (if (= :press event)
      (cond (vector? key) (let [[x y] key]
                            (disp/light-set LAUNCHPAD x y (rand-nth (range 1 4))))
            (= :session key) (launchpad-off)
            (= :user1 key) (launchpad-shiny)))
    (println m)))

(defonce LAUNCHPAD
  (lp/make-launchpad lp/default-palette))

(defn launchpad-off []
  (doseq [x (range 0 8)
          y (range 0 8)]
    (disp/light-set LAUNCHPAD x y 0)
    (Thread/sleep 10)))

(defn launchpad-shiny []
  (let [cells (for [y (range 0 8)
                    x (range 0 8)]
                {:x x :y y :colour (rand-nth (range 1 4))})]
    (doseq [cell (shuffle cells)]
      (let [{:keys [x y colour]} cell]
        (disp/light-set LAUNCHPAD x y colour)
        (Thread/sleep 20)))))

(defn launchpad-bounce []
  (launchpad-off)
  (Thread/sleep 1000)
  (launchpad-shiny))

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn beats 
  ([nome count] (take count (iterate inc (nome))))
  ([nome] (beats nome 8)))

(defn repeater [melody-fn inst nome cnt]
  (let [timings (beats nome cnt)]
    (map #(at (nome %1) (inst %2)) timings (flatten (repeatedly melody-fn)))))


(comment


  (repeater #(shuffle (scale :e3 :aeolian)) piano (metronome 2000) 20)
  (repeater #(shuffle (scale :e3 :aeolian)) piano (metronome 500) 20)
  (repeater #(shuffle (scale :e4 :aeolian)) piano (metronome 500) 20)
  (repeater #(shuffle (scale :e5 :aeolian)) piano (metronome 500) 20)
  (repeater #(shuffle (scale :e6 :aeolian)) piano (metronome 500) 20)
  (repeater #(shuffle (scale :e5 :dorian)) piano (metronome 500) 20)
  (repeater #(shuffle (scale :e5 :major-pentatonic)) piano (metronome 800) 200)
  (repeater #(shuffle (scale :e5 :bartok)) piano (metronome 500) 20)
  (repeater #(scale :e5 :bartok) piano (metronome 500) 20)
  (repeater #(scale :e5 :neapolitan-minor) piano (metronome 500) 20)
  (repeater #(scale :e5 :bartok) piano (metronome 500) 20)
  (repeater #(chord :e5 :dom7) piano (metronome 500) 20)
  (kill piano)
  (repeater #(chord :e5 :dom7) piano (metronome 500) 20)
  (repeater #(chord :e5 :dom7) piano (metronome 500) 16)
  (repeater #(chord :e2 :dom7) piano (metronome 500) 16)
  (repeater #(chord :a2 :dom7) piano (metronome 500) 16)
  (repeater #(chord :g2 :dom7) piano (metronome 500) 16)
  (repeater #(chord :b2 :dom7) piano (metronome 500) 16)
  (repeater #(chord :a2 :dom7) piano (metronome 500) 16)
  (repeater #(chord :e2 :dom7) piano (metronome 500) 16)
  (repeater #(chord :a2 :dom7) piano (metronome 500) 16)
  (repeater #(chord :b2 :dom7) piano (metronome 500) 16)
  )

