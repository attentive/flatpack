(ns flatpack.launchpad
  (:use [flatpack.grid-display]
        [overtone.midi]
        [clojure.set :only [map-invert]])
  (:require [clojure.stacktrace]
            [taoensso.timbre :as timbre :refer [debug spy]])
  (:import (javax.sound.midi ShortMessage)))

;;; Launchpad implementation of a Grid controller.
;;; currently only uses the 8x8 grid section, not the extra 16
;;; peripheral buttons.

;;; You may wish to consult the Launchpad Programmers Reference when
;;; reading this file:
;;; http://novationmusic.com/support/launchpad/

;;; -- this section to be pushed upstream to overtone.midi

(def cmd->java-cmd (map-invert midi-shortmessage-command))

(defn make-ShortMessage
  ([midi-msg]
   (apply make-ShortMessage ((juxt :command :note :velocity) midi-msg)))
  ([cmd byte1 byte2]
   {:pre [(contains? cmd->java-cmd cmd)]}
   (doto (ShortMessage.)
     (.setMessage (cmd->java-cmd cmd) byte1 byte2)))
  ([channel cmd byte1 byte2]
   {:pre [(contains? cmd->java-cmd cmd)]}
   (doto (ShortMessage.)
     (.setMessage (cmd->java-cmd cmd) channel byte1 byte2))))

(defn midi-send [sink msg]
  (.send (:receiver sink) msg -1))

;;; -- end section to be pushed upstream

(defn coords->midi-note [x y]
  (+ x (* 16 y)))

(def midi-note->coords
  (into {} (for [x (range 8)
                 y (range 8)]
             [(+ x (* y 16)) [x y]])))

;;; messages to control double-buffering.
;;;
;;; these messages display one buffer, copy that buffer's contents to
;;; the other buffer, and set the other buffer to update in the background.
(def display-buffer-0 (make-ShortMessage :control-change 0 (+ 32 16 4)))
(def display-buffer-1 (make-ShortMessage :control-change 0 (+ 32 16 1)))


;;; Summary of the colour data format sent to the launchpad:
;;;
;;; It's easiest to split the number into two-bit "crumbs":
;;;
;;; 4r300 <- LED green, single buffer
;;; 4r030 <- LED off, copy to both buffers
;;; 4r003 <- LED red, single buffer
;;;
;;; The first crumb is the green value; the last crumb is the red
;;; value. They separately control a green and a red LED
;;; respectively. Setting a value of 1 or 2 rather than 3 dims the
;;; LED. You can get other colours by mixing red and green; eg the
;;; launchpad programmer's reference suggests 4r333 (63) for amber and
;;; 4r332 (62) for yellow.
;;;
;;; The middle crumb contains two control bits with regard to double
;;; buffering. A value of 3 copies the sent colour to both buffers,
;;; while a value of 0 only sends it to the currently updating buffer
;;; (which is not the currently displayed one). Generally you want it
;;; to be 3 for setting a single LED at a time, and 0 while updating
;;; the whole field in double-buffering mode. For other values, see
;;; the programmer's reference.

(def colours
  {:red    4r003
   :green  4r300
   :yellow 4r302
   :off    4r000})

(defn both-buffers [colour]
  (bit-or colour 4r030))

(def metakeys->midi
  {:up      {:command :control-change :note 104}
   :down    {:command :control-change :note 105}
   :left    {:command :control-change :note 106}
   :right   {:command :control-change :note 107}
   :session {:command :control-change :note 108}
   :user1   {:command :control-change :note 109}
   :user2   {:command :control-change :note 110}
   :mixer   {:command :control-change :note 111}
   :vol     {:command :note-on        :note   8}
   :pan     {:command :note-on        :note  24}
   :snda    {:command :note-on        :note  40}
   :sndb    {:command :note-on        :note  56}
   :stop    {:command :note-on        :note  72}
   :trkon   {:command :note-on        :note  88}
   :solo    {:command :note-on        :note 104}
   :arm     {:command :note-on        :note 120}
   :vol-up     {:command :note-off        :note   8}
   :pan-up     {:command :note-off        :note  24}
   :snda-up    {:command :note-off        :note  40}
   :sndb-up    {:command :note-off        :note  56}
   :stop-up    {:command :note-off        :note  72}
   :trkon-up   {:command :note-off        :note  88}
   :solo-up    {:command :note-off        :note 104}
   :arm-up     {:command :note-off        :note 120}})

(def midi->metakeys
  (map-invert metakeys->midi))

(defn colour-single [colour palette]
  (both-buffers (colours (palette colour))))

(defn colour-msg [key colour palette]
  (make-ShortMessage
   (into (metakeys->midi key)
         {:velocity (colour-single colour palette)})))

(defn get-metakey
  "returns the metakey, or nil if it's not a metakey"
  [event]
  (midi->metakeys (select-keys event [:command :note])))

(defn key-coords [midi-event]
  (midi-note->coords (:note midi-event)))

(defn event-map [midi-event]
  (let [key-event (if (zero? (:velocity midi-event)) :release :press)
        key       (or (get-metakey midi-event)
                      (key-coords midi-event))]
    (when key
      {:event key-event
       :key key})))

(defn midi-handler [handler-atom]
  (fn [midi-event ts]
    (debug "In midi-handler " @handler-atom)
    (try
      (@handler-atom (event-map midi-event))
      (catch Exception e ;Don't let the midi thread die, it's messy
        (clojure.stacktrace/print-stack-trace e)))))


(defrecord Launchpad [launchpad-in launchpad-out palette]
  MetaKeys
  (meta-led-set [this key colour]
    (midi-send launchpad-out (colour-msg key colour palette)))
  (meta-list-keys [this] (keys metakeys->midi))

  GridDisplay
  (width [this] 8)
  (height [this] 8)
  (handle-events-with [this handler]
    (midi-handle-events launchpad-in (comp handler event-map)))
  (light-set [this x y colour]
    (midi-note-on launchpad-out (coords->midi-note x y) (colour-single colour palette)))

  (light-set-all [this colour]
    (light-frame this
                 (into {}
                       (for [y (range (height this))
                             x (range (width this))]
                         [[x y] colour]))))
  (light-frame [this leds]
    (midi-send launchpad-out display-buffer-0)
    (let [coords (for [y (range (height this))
                       x (range (width this))]
                   [x y])]
      (doseq [[coord-1 coord-2] (partition 2 coords)]
        (let [colour-1 (colours (palette (get leds coord-1 0)))
              colour-2 (colours (palette (get leds coord-2 0)))]
          (midi-send launchpad-out (make-ShortMessage 2 :note-on colour-1 colour-2)))))
    (midi-send launchpad-out display-buffer-1))) 

(defmethod print-method Launchpad [lp w]
  (.write w (format "#<Launchpad palette%s>" (:palette lp))))

(def default-palette
  [:off :red :green :yellow])

(defn make-launchpad
  "Creates an 8x8 Grid implementation backed by a launchpad."
  ([] (make-launchpad default-palette))
  ([palette]
   (if-let [launchpad-in (midi-in "Launchpad")]
     (if-let [launchpad-out (midi-out "Launchpad")]
       (->Launchpad launchpad-in launchpad-out palette)
       (throw (Exception. "Found launchpad for input but couldn't find it for output")))
     (throw (Exception. "Couldn't find launchpad")))))

