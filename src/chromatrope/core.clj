(ns chromatrope.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def cut [0 0 0])
(def etch [200 200 200])
(def raster-etch [200 200 200])

(defn setup []
  ;; (q/text-mode :shape)
  {:color 0
   :angle 0
   :method :dev})

(defn update [method]
  (constantly {:method method}))


(defn centre [] [(* (q/width) 0.5) (* (q/height) 0.5)])
  
(defn no-2 []
  (q/fill 220 70 20)
  (let [full (* 0.5 (q/height))
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

(defmacro do-around [n-times & body]
  `(dotimes [i# ~n-times]
     (q/with-rotation [(* i# (/ (Math/PI) 0.5 ~n-times))]
       ~@body)))
       
  
(defn star 
  ([n-points] (star n-points 0.5))
  ([n-points tuck]
   (let [full (* 0.5 (q/height))
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
  ;; (q/fill 0 255 0)
  (q/fill 0 255 0)
  (q/stroke 0 20 25)
  (star 6))
  

(defn no-6 []
  (q/fill 205 0 205)
  (q/stroke 25)
  (star 8 0.3)
  (do-around 8 (q/line 0 0 0 (* 1/2 (q/height)))))
  
        
          
        
    
  
(defn draw-slide [n slide-fn slides]
  (q/with-graphics (deref (nth slides n))
    (q/background 255)
    (q/no-stroke)
    (q/with-translation (centre)
      (slide-fn))))
  
(defn show-slide [n slides]
  (q/image (-> slides (nth n) deref) (* -0.5 (q/width)) (* -0.5 (q/height))))
  
  

(defn draw [state]
  (q/background 255)
  (draw-slide 0 no-1 (:slides state))
  (draw-slide 1 no-2 (:slides state))
  (draw-slide 5 no-6 (:slides state))
      
  (q/translate (centre))
  (q/fill 255)
  (do-around 46 (q/ellipse (* 1/2 (q/width)) (* 1/2 (q/width)) 169 169))
  (q/blend-mode :subtract)
  (q/with-rotation [(/ (q/frame-count) -18)]
    (show-slide 0 (:slides state)))
  (q/with-rotation [(/ (q/frame-count) 40)]

    (show-slide 1 (:slides state)))
  (q/with-rotation [(/ (q/frame-count) -90)]
    (show-slide 5 (:slides state))))
  
    
    

    
  ;; (q/save "test.png")
  ;; (when-not (= (:method state) :dev) (q/exit)))

(nth (repeat 1) 4)

(q/defsketch analog-neural-network-tools-dev
  :size [400 400]
  :renderer :p2d
  :setup (fn [] {:slides (repeatedly #(future (q/create-graphics 400 400 :p2d)))})
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

; (def mm->300dpi (partial map (comp int #(Math/ceil %) (partial * 11.81))))

; (q/defsketch analog-neural-network-tools-lazer-cut
;   :size (mm->300dpi [300 150])
;   :renderer :pdf
;   :output-file "output.pdf"
;   :setup setup
;   :update (update :lazer-cut)
;   :middleware [m/fun-mode]
;   :draw draw)
