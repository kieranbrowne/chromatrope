(ns chromatrope.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def mm->300dpi (partial * 11.81))

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

(defn spiral []
  (q/begin-shape)
  (q/stroke-weight 5)
  (mapv
    (fn [n]
      (q/curve-vertex (* 1/4 n (q/sin (/ n 4))) (* 1/4 n (q/cos (/ n 4)))))
    (range (/ (q/width) 1/3)))
  (q/end-shape)
  (q/stroke-weight 1))



(defn no-1 []
  ;; (q/fill 1 1 1)
  ;; (q/fill (colour-phase 0.01) (colour-phase 0.02) (colour-phase 0.08))
  (q/stroke 0)
  (star 6))

(defn no-2 [num in-r out-r]
  ;; (q/fill (colour-phase 0.01) (colour-phase 0.2) (colour-phase 0.08))
  ;; (q/fill 0.2 1 2)
  ;; (q/stroke (colour-phase 0.01) (colour-phase 0.2) (colour-phase 0.08))
  ;; (q/stroke 0)
  (let [full out-r

        middle (q/map-range 0.3 0 1 in-r out-r)
        onethird (q/map-range 1/3 0 1 in-r out-r)
        twothirds (q/map-range 2/3 0 1 in-r out-r)
        thick (* 0.9 middle)
        thin (* 0.6 middle)]

    (dotimes [i num]
      (q/with-rotation [(* i (/ (Math/PI) 0.5 num))]
        (q/begin-shape)
        (q/vertex 0 in-r)
        (q/bezier-vertex thick onethird thick twothirds 0 full)
        (q/bezier-vertex thin twothirds thin onethird 0 in-r)
        ;; (q/bezier-vertex 40 (* 0.25 (q/height)) 40 (* 0.5 (q/height)) 0 0)

        ;; (q/bezier-vertex 0 0 400 400 0 0)
        (q/end-shape)))))

(defn colour-phase [rate]
  (q/map-range (q/sin (* (q/frame-count) rate)) -1 1 0 255))


(defn no-6 []
  ;; (q/fill (colour-phase 0.1) (colour-phase 0.22) 200)
  ;; (q/fill 1.0 1.0 1.0)
  ;; (q/stroke (colour-phase 0.06))
  (q/stroke 0)
  (star 8 0.3)
  (do-around 8 (q/line 0 0 0 (* 1/2 (q/height)))))


(defn no-7 []
  ;; (q/fill (colour-phase 0.1) (colour-phase 0.22) 200)
  ;; (q/fill 1 1 1)
  ;; (q/stroke (colour-phase 0.06))
  (q/stroke 255)
  (star 28 0.3)
  (do-around 28 (q/line 0 0 0 (* 1/2 (q/height)))))





(defn draw-slide [n slide-fn slides]
  (q/with-graphics (deref (nth slides n))
    ;; (q/background 0)
    (q/no-stroke)
    (q/with-translation (centre)
      (slide-fn))))

(defn show-slide [n slides]
  (q/image
    (deref (nth slides n))
    (* -0.5 (q/width))
    (* -0.5 (q/height))))


(defn ngon [n w]
  (q/begin-shape)

  (dotimes [i n]
    (q/vertex (* w (q/sin (q/map-range i 0 n 0 6.29))) (* w (q/cos (q/map-range i 0 n 0 6.29)))))
  (q/end-shape :close))
  


(defn draw [state]
  ;; white spotlight
  (q/stroke-cap :project)
  (q/background 1)

  (q/translate (centre))


  (q/fill 1)
  (q/ellipse 0 0 380 380)
  (q/with-rotation [(/ (q/frame-count) -40)]
    (q/blend-mode :blend)
    (q/stroke-weight 22)
    (q/stroke 0)
    (q/fill 1)
    (ngon 3 170)
    (ngon 3 120)
    (ngon 3 70)
    (q/rotate 3.145)
    (ngon 3 170)
    (ngon 3 120)
    (ngon 3 70))


  (q/blend-mode :lightest)
  (q/with-rotation [(/ (q/frame-count) 10)]

    (q/no-fill)
    (q/stroke 1)
    (q/stroke-weight 22)
    (q/begin-shape)
    (dotimes [i 20]
      (q/curve-vertex (* (* i 7) (q/sin (/ i 2))) (* (* i 7)  (q/cos (/ i 2)))))
    (q/end-shape)
    (q/begin-shape)
    (dotimes [i 20]
      (q/curve-vertex (* (* (- i) 7) (q/sin (/ i 2))) (* (* (- i) 7)  (q/cos (/ i 2)))))
 
    (q/end-shape))

    
  (q/stroke 255)
          
            

    
    
             

  ;; slot two
  

  ; (q/with-rotation [(/ (q/frame-count) -100)]
  ;   (do-around [] 10
  ;     (q/ellipse i 40 30 30)))
    
  (q/fill 0)

  (q/blend-mode :blend)
  ;; (q/blend-mode :normal)
  ;; (q/display-filter :invert)


  ;; (q/save "test.png")

  ; (if (and true;(>= (q/frame-count) 0)
  ;          (< (q/frame-count) 450))
  ;   (q/save-frame  "frame-####.png")
  ;   (q/exit))
  (when-not (= (:method state) :dev) (q/exit)))


(q/defsketch chromatrope-dev
  :size [400 400]
  :renderer :p2d
  :settings (fn [] (q/pixel-density 2) (q/smooth))
  :setup (fn []
           (q/color-mode :hsb 1.0)
           {:method :dev
            :cut [0 0 0]
            :etch [255 0 0]
            :raster-etch [0 0 255]
            })

  :update identity
  :draw draw
  :middleware [m/fun-mode])



(def mm->300dpi (partial * 11.81))

(q/defsketch chromatrope-lazer-cut
  :size (map (partial + 80) (map (comp int #(Math/ceil %) mm->300dpi) [80 80]))
  :renderer :pdf
  :output-file "output.pdf"
  :setup (fn []
           (q/background 255)
           (q/text-mode :shape)
           (q/stroke-weight (mm->300dpi 0.1))
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
            ;; (no-2)
            )
          (if (= (:method state) :lazer-cut) (q/exit))))
