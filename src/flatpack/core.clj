(ns flatpack.core
  (:use [overtone.live]
        [overtone.inst.piano]
        [overtone.inst.sampled-piano]
        [overtone.inst.synth]))

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


