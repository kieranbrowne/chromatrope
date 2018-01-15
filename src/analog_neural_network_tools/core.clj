(ns analog-neural-network-tools.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def cut [255 0 0])
(def engrave [0 055 0])

(defn function-block [fn start end step]
  (q/with-stroke engrave
    (q/begin-shape)
    (doseq [i (range start (+ end step) step)]
      (q/vertex 
        (q/map-range i start end 0 (q/width)) 
        (q/map-range
          (fn i) -1 1 0 (q/height))))
    (q/end-shape)))

;; (Math/PI)

(defn function-round [fn start end step max min radius]
  (q/with-stroke cut
    (q/ellipse 0 0 radius radius))
    
  (q/with-stroke engrave
    (doseq [i (range start end step)]
      (q/with-rotation 
        [(q/map-range 
           (fn i) 
           min max 0 (* 2 (Math/PI)))]
        (q/with-fill engrave
          (let [number (format "%.2f" (fn i))
                x (- (* (count number) 2)) 
                y (- (/ radius 2) 20)]

            (q/text-size 6)
            (when true 
              (q/text number (+ x)  y))))
            ; (when 
            ;   (mod i (quot (fn i)))


            ;   (q/text "text " 20 20))))

        (q/no-fill)

        (q/line 0 (/ radius 2) 0 (- (/ radius 2) 10))))))
        
      
      

(defn sigmoid [x]
  (Math/tanh x))
  
(defn log [x]
  (Math/log x))
  
(Math/log 10)

(defn setup []
  (q/text-mode :shape)
  (q/no-fill)
  {:color 0
   :angle 0})

(defn update [method]
  (fn [state] {:method method}))

(defn draw [state]
  (q/background 255)
  (q/with-fill [0 0 0]
    (q/text-size 18)
    (q/text "∑x" 10 18)
    (q/text-size 10)
    (q/text "i" 34 20))
  ;; (function-block sigmoid -4 4 0.2)
  (q/no-fill)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (q/with-stroke [0]
      (q/point 0 0))
    (function-round sigmoid -4 4 0.2 -1 1 300)
    (function-round log 1 10 0.1 0 2.302 380))
  (when (= :print (:method state)) (q/exit)))
  
(q/defsketch analog-neural-network-tools-dev
  :size [700 700]
  :renderer :java2d
  :setup setup
  :update (update :dev)
  :middleware [m/fun-mode]
  :draw draw)

(q/defsketch analog-neural-network-tools-print
  :size [700 700]
  :renderer :pdf
  :output-file "output.pdf"
  :setup setup
  :update (update :print)
  :middleware [m/fun-mode]
  :draw draw)
