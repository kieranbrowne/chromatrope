(ns chromatrope.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn centre [] [(* (q/width) 0.5) (* (q/height) 0.5)])

(defmacro do-around [n-times & body]
  `(dotimes [i# ~n-times]
     (q/with-rotation [(* i# (/ (Math/PI) 0.5 ~n-times))]
       ~@body)))
  
(defn star 
  ([n-points] (star n-points 0.5))
  ([n-points tuck]
   (let [full (* 0.45 (q/height))
          tuck (* 2 tuck)
          theta (q/radians (* 1/2 (/ 360 n-points))) 
          a (* 1/2 full)
          h (/ a (q/cos theta))
          o (* h (q/sin theta))]

      (do-around n-points
          (q/quad
            0 0 
            (* tuck o) (* tuck a)
            0 full
            (- (* tuck o)) (* tuck a))))))

(defn no-1 []
  (q/fill 200 0 255)
  ;; (q/fill (colour-phase 0.01) (colour-phase 0.02) (colour-phase 0.08))
  (q/stroke 0 255 0)
  (star 6))
  
(defn no-2 []
  ;; (q/fill (colour-phase 0.01) (colour-phase 0.2) (colour-phase 0.08))
  (q/fill 25 0 200)
  ;; (q/stroke (colour-phase 0.01) (colour-phase 0.2) (colour-phase 0.08))
  (q/stroke 255 0 255)
  (let [full (- (* 0.5 (q/height)) 40)

        middle (* 0.5 full)
        onethird (* 1/3 full)
        twothirds (* 2/3 full)
        thick (* 0.8 middle)
        thin (* 0.7 middle)
        num-parts 30]
        
    (dotimes [i num-parts]
      (q/with-rotation [(* i (/ (Math/PI) 0.5 num-parts))]
        (q/begin-shape)
        (q/vertex 0 0)
        (q/bezier-vertex thick onethird thick twothirds 0 full)
        (q/bezier-vertex thin twothirds thin onethird 0 0)
        ;; (q/bezier-vertex 40 (* 0.25 (q/height)) 40 (* 0.5 (q/height)) 0 0)

        ;; (q/bezier-vertex 0 0 400 400 0 0)
        (q/end-shape)))))

(defn colour-phase [rate]
  (q/map-range (q/sin (* (q/frame-count) rate)) -1 1 0 255))
  

(defn no-6 []
  ;; (q/fill (colour-phase 0.1) (colour-phase 0.22) 200)
  (q/fill 20 255 255)
  ;; (q/stroke (colour-phase 0.06))
  (q/stroke 255)
  (star 8 0.3)
  (do-around 8 (q/line 0 0 0 (* 1/2 (q/height)))))
  
        
(defn no-7 []
  ;; (q/fill (colour-phase 0.1) (colour-phase 0.22) 200)
  (q/fill 0 255 55)
  ;; (q/stroke (colour-phase 0.06))
  (q/stroke 255)
  (star 38 0.3)
  (do-around 28 (q/line 0 0 0 (* 1/2 (q/height)))))
  
          
        
    
  
(defn draw-slide [n slide-fn slides]
  (q/with-graphics (deref (nth slides n))
    (q/background 0)
    (q/no-stroke)
    (q/with-translation (centre)
      (slide-fn))))
  
(defn show-slide [n slides]
  (q/image (-> slides (nth n) deref) (* -0.5 (q/width)) (* -0.5 (q/height))))
  
  

(defn draw [state]
  (q/background 5)
  (draw-slide 0 no-1 (:slides state))
  (draw-slide 1 no-2 (:slides state))
  (draw-slide 5 no-6 (:slides state))
  (draw-slide 6 no-7 (:slides state))
      
  (q/translate (centre))
  (q/fill 0)
  (q/no-stroke)
  (do-around 46 (q/ellipse (* 1/2 (q/width)) (* 1/2 (q/width)) 169 169))
  (when (= (:method state) :dev) (q/blend-mode :exclusion))
  (q/with-rotation [(/ (q/frame-count) 380)]
    (show-slide 0 (:slides state)))
  (q/with-rotation [(/ (q/frame-count) 55)]
    (show-slide 6 (:slides state)))
  (q/with-rotation [(/ (q/frame-count) 155)]
    (show-slide 1 (:slides state)))
  (q/with-rotation [(/ (q/frame-count) -60)]
    (show-slide 5 (:slides state)))
  (when (= (:method state) :dev) (q/blend-mode :darkest))
  (q/with-rotation [(/ (q/frame-count) 820)]
    (show-slide 1 (:slides state)))
  (q/with-rotation [(/ (q/frame-count) -200)]
    (show-slide 6 (:slides state)))
  ;; (q/save "test.png")
  ;; (q/save-frame  "frame-####.png")
  (when-not (= (:method state) :dev) (q/exit)))


(q/defsketch chromatrope-dev
  :size [400 400]
  :renderer :p2d
  :setup (fn [] 
           ;; (q/color-mode :hsb)
           {:slides (repeatedly #(future (q/create-graphics 400 400 :p2d)))
            :method :dev
            :cut [0 0 0]
            :etch [255 0 0]
            :raster-etch [0 0 255]})
                 
  :update identity
  :draw draw
  :middleware [m/fun-mode])
  

; (q/defsketch analog-neural-network-tools-print
;   :size [595 842]
;   :renderer :pdf
;   :output-file "output.pdf"
;   :setup setup
;   :update (update :print)
;   :middleware [m/fun-mode]
;   :draw draw)

(def mm->300dpi (partial map (comp int #(Math/ceil %) (partial * 11.81))))

(q/defsketch chromatrope-lazer-cut
  :size (map (partial + 80) (mm->300dpi [80 80]))
  :renderer :pdf
  :output-file "output.pdf"
  :setup (fn [] 
           (q/background 255)
           (q/text-mode :shape) 
           (q/stroke-weight 0.1)
           (q/no-fill) {:bleed 80})
  ;:slides (repeatedly #(future (q/create-graphics 400 400 :pdf)))
                 ;:method :lazer-cut
                 ;:cut [0 0 0]
                 ;:etch [255 0 0]
                 ;:raster-etch [0 0 255]})
  :update #(assoc % :method :lazer-cut)
  :middleware [m/fun-mode]
  :draw (fn [state] 
          (q/with-translation (centre) 
            (q/stroke 255 0 0)
            (q/ellipse 0 0 (- (q/width) (:bleed state)) (- (q/height) (:bleed state)))
            (q/stroke 0 0 255)
            (no-2)) 
          (if (= (:method state) :lazer-cut) (q/exit))))
