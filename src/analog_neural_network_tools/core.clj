(ns analog-neural-network-tools.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  {:color 0
   :angle 0})


(defn draw []
  (q/background 255)
  (q/line 0 0 (q/width) (q/height))
  (q/exit))
  

(q/defsketch analog-neural-network-tools
  :size [500 500]
  :renderer :pdf
  :output-file "test.pdf"
  :setup setup
  :draw draw)
