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

(defn no-2 []
  ;; (q/fill (colour-phase 0.01) (colour-phase 0.2) (colour-phase 0.08))
  ;; (q/fill 0.2 1 2)
  ;; (q/stroke (colour-phase 0.01) (colour-phase 0.2) (colour-phase 0.08))
  ;; (q/stroke 0)
  (let [full (- (* 0.5 (q/height)) 40)

        middle (* 0.5 full)
        onethird (* 1/3 full)
        twothirds (* 2/3 full)
        thick (* 0.9 middle)
        thin (* 0.8 middle)
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







(defn draw [state]
  ;; white spotlight
  (q/background 0)

  (q/translate (centre))


  (q/fill 1)
  (q/ellipse 0 0 380 380)
  ;; (q/display-filter :dilate)
  (q/blend-mode :darkest)

  (q/no-stroke)
  ; (q/fill 0.0 1 1)
  ; (q/ellipse 0 0 20 20)

  ; (q/fill 0.2 1 1)
  ; (q/ellipse 10 0 20 20)

  ;; slot one
  ;; (q/fill 0 1 1)
  ;; (q/no-fill)
  (q/fill 1 1 1)
  (q/stroke 0)
  (q/no-stroke)
  (q/with-rotation [(/ (q/frame-count) 255)]
    (do-around 20
               (q/fill 0 1 1)
               (q/quad 20 20 50 80 32 82 20 28)
               (q/fill 0.5 1 1)
               (q/quad 00 00 20 20 32 22 00 0)
               (q/fill 0.9 1 0.9)
               (q/quad 80 80 120 80 92 92 90 90)
               (q/quad 70 70 110 70 82 82 80 80)
               )
    ;; (no-2)
    )



  ;; slot two
  (q/with-rotation [(/ (q/frame-count) -255)]
    (q/scale -1 1)
    (do-around 20
               (q/fill 0.5 1 1)
               (q/quad 20 20 50 80 32 82 20 28)
               (q/fill 0.0 1 1)
               (q/quad 00 00 20 20 32 22 00 0)
               (q/fill 0.4 1 0.7 0.1)
               (q/quad 80 80 120 80 92 92 90 90)
               (q/quad 70 70 110 70 82 82 80 80)
               )
    ;; (no-2)
    )

  (q/blend-mode :blend)
  ;; (q/blend-mode :normal)
  ;; (q/display-filter :invert)

  ;; (draw-slide 0 no-1 (:slides state))
  ; (draw-slide 1 no-2 (:slides state))
  ; (draw-slide 5 no-6 (:slides state))
  ; (draw-slide 6 no-7 (:slides state))
  ; (when (= 0 (mod (q/frame-count) 50))
  ;   (q/blend-mode (:shader state)))

  ; (q/fill 0)
  ; (q/no-stroke)
  ; (do-around 46 (q/ellipse (* 1/2 (q/width)) (* 1/2 (q/width)) 169 169))
  ;; (when (= (:method state) :dev) (q/blend-mode :exclusion))
  ; (q/with-rotation [(/ (q/frame-count) 55)]
  ;   (show-slide 0 (:slides state)))
  ; (q/with-rotation [(/ (q/frame-count) 380)]
  ;   (show-slide 1 (:slides state)))
  ;; (q/load-pixels)
  ;; (println 1)
  ; (let [pix (q/pixels)
  ;       a (q/pixels (deref (nth (:slides state) 0)))
  ;       b (q/pixels (deref (nth (:slides state) 1)))]
  ;   (dotimes [i (* 400 400)]
  ;     (aset pix i (+ (aget a i) (aget b i)))))
  ; (q/update-pixels)

  ; (q/with-rotation [(/ (q/frame-count) 155)]
  ;   (show-slide 1 (:slides state)))
  ; (q/with-rotation [(/ (q/frame-count) -60)]
  ;   (show-slide 5 (:slides state)))
  ; (when (= (:method state) :dev) (q/blend-mode :darkest))
  ; (q/with-rotation [(/ (q/frame-count) 820)]
  ;   (show-slide 1 (:slides state)))
  ; (q/with-rotation [(/ (q/frame-count) -200)]
  ;   (show-slide 6 (:slides state)))

  ;; (q/save "test.png")
  ;; (q/save-frame  "frame-####.png")
  (when-not (= (:method state) :dev) (q/exit)))


(q/defsketch chromatrope-dev
  :size [400 400]
  :renderer :p2d
  :settings (fn [] (q/pixel-density 1) (q/smooth))
  :setup (fn []
           (q/color-mode :hsb 1.0)
           {:slides (repeatedly #(future (q/create-graphics 400 400 :p2d)))
            :method :dev
            :cut [0 0 0]
            :etch [255 0 0]
            :raster-etch [0 0 255]
            :shader (doto (q/load-shader "hard-light.glsl")
                      (.set "destSize" 400 400)
                      (.set "destRect" 0 0 400 400)
                      (.set "srcSize" 400 400)
                      (.set "srcRect" 0 0 400 400))})

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
            (no-2))
          (if (= (:method state) :lazer-cut) (q/exit))))
