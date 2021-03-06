(ns flatpack.grid-display)

(defprotocol MetaKeys
  "A representation binding functionality to meta-keys, assuming they won't be part of the standard
   grid interface, an implementation will report its functionality"
  (meta-led-set [this key colour] "If supported, set the color of an led on the key")
  (meta-list-keys [this] "lists all the supported keys, informational"))

(defprotocol GridDisplay
  "A generic representation of a grid of lights."
  (width [this]
    "Returns the number of elements of the grid width-wise. This value
    is not expected to change.")
  (height [this]
    "Returns the number of elements of the grid height-wise. This
    value is not expected to change.")
  (handle-events-with [this handler]
    "Update the grid display so that it will handle MIDI input events
    using the provided handler.")
  (light-set [this x y colour]
    "Set the light at position [x y] to the given colour. Color 0 is
    off, > 0 represents a specific palette colour. Subject to
    change.")
  (light-set-all [this colour]
    "Sets all light to a single colour or off if 0.")
  (light-frame [this leds]
    "Update the entire field of lights. The rows arg is a map of coords
    to colour values, the same as in led-set. Any unspecified coords
    should default to 0.

    For example, if you have a 2x2 grid, you could update all of the
    lights like this:

    (light-frame grid {[0 0] 1 [1 0] 0
                       [0 1] 0 [1 1] 1})"))


