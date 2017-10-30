(ns analog-neural-network-tools.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/text-mode :shape)
  {:color 0
   :angle 0})

(defn update [method]
  (fn [state] {:method method}))

(defn draw [state]
  (q/background 255)
  (q/line 0 0 (q/width) (q/height))
  (when (= :print (:method state)) (q/exit)))
    


  ;; (q/exit))
  
(q/defsketch analog-neural-network-tools-dev
  :size [500 500]
  :renderer :java2d
  :setup setup
  :update (update :dev)
  :middleware [m/fun-mode]
  :draw draw)


(q/defsketch analog-neural-network-tools-print
  :size [500 500]
  :renderer :pdf
  :output-file "output.pdf"
  :setup setup
  :update (update :print)
  :middleware [m/fun-mode]
  :draw draw)
)
