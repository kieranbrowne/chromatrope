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







(defn draw [state]
  ;; white spotlight
  (q/background 0)

  (q/translate (centre))


  (q/fill 1)
  (q/ellipse 0 0 380 380)
  ;; (q/display-filter :blur)
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
  (q/with-rotation [(/ (q/frame-count) 100)]
    (do-around 4
               (q/fill 0.0 1 1)
               (q/stroke-weight 4)
               (q/stroke 0)
               (q/stroke-join :bevel)
               ;; (q/stroke 0.0 1 1)
               (q/line 0 50  30 20)
               (q/line 0 60  40 20)
               ;; (q/stroke 0.65 1 1)
               (q/line 0 50 -20 30)
               (q/line 0 60 -30 30)
               ;; (q/stroke-weight 10)
               ;; (q/stroke 0.0 1 1)
               ;; (q/line 0 90 -100 80)
               ;; (q/line 0 120 -120 90)

               (q/no-fill)
               ;; (q/rect 0 2 20 20)
               ;; (q/stroke 0.65 1 1)
               (let [s 20]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0.65 1 1)
               (q/stroke-weight 2)
               ;; (q/stroke 0 1 1)
               (let [s 16]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0.65 1 1)
               (let [s 13]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0 1 1)
               (let [s 10]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0.65 1 1)
               (let [s 7]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0 1 1)
               (let [s 4]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )


               (q/stroke 0)
               (q/stroke-weight 6)
               (q/stroke-cap :square)
               ;; (q/stroke 0.0 1 1)
               (doall
                (for [i (range 10)]
                  (q/line 0 60
                          (* 120 (q/sin (/ i 6)))
                          (* 120 (q/cos (/ i 6))))))
               ;; (q/no-fill)
               ;; (q/ellipse  0 0 390 280)
               ;; (q/fill 0)
               ;; (q/quad 20 00 30 190 82 180 30 28)
               ;; (q/quad 20 00 70 190 22 179 30 28)
               ;; (q/fill 0.5 1 1)
               ;; (q/quad 00 00 20 20 32 22 00 0)
               ;; (q/fill 0.0 1 0.9)
               ;; (q/quad 80 80 120 80 92 92 90 90)
               ;; (q/quad 70 70 110 70 82 82 80 80)

               )
    ;; (do-around 8
    ;;            (q/stroke 0.25 1 1)
    ;;            (q/rotate 0.8)
    ;;            ;; (q/stroke-weight 8)
    ;;            (q/no-fill)
    ;;            (q/ellipse  0 0 390 280)
    ;;            )
    ;; (q/stroke 0.65 1 1)
    (q/stroke-weight 9)
    (do-around 40
               (q/bezier 0 120
                         90 140
                         -10 170
                         48 174))
    (q/fill 0)
    ;; (no-2 30 130 180)
    ;; (no-2 10 80 120)
    ;; (no-2 20 50 90)
    ;; (no-2 26 00 60)
    (q/no-stroke)
    ;; (q/fill 0.0 1 1)
    ;; (no-2 8 80 180)
    (do-around 40 (q/quad 0 180 11 200 18 200 6 180))
    )



  ;; slot two
  (q/with-rotation [(/ (q/frame-count) -100)]
    (q/scale -1 1)
    (do-around 4
               (q/stroke-weight 4)
               (q/stroke-join :bevel)
               (q/stroke 0)
               ;; (q/fill 0)

               ;; (q/stroke 0.65 1 1)
               (q/line 0 50  30 20)
               (q/line 0 60  40 20)
               ;; (q/stroke 0.0 1 1)
               (q/line 0 50 -20 30)
               (q/line 0 60 -30 30)

               (let [s 20]
                 (q/line 0 s s s)
                 (q/line s s s 0))

               (q/no-fill)
               ;; (q/rect 2 0 20 20)
               (q/stroke-weight 7)
               ;; (q/stroke 0.65 1 1)
               (doall
                (for [i (range 10)]
                  (q/line 0 60
                          (* 120 (q/sin (/ i 6)))
                          (* 120 (q/cos (/ i 6))))))
               (q/stroke-weight 2)
               ;; (q/stroke 0.65 1 1)
               (let [s 16]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0 1 1)
               (let [s 13]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0.65 1 1)
               (let [s 10]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0 1 1)
               (let [s 7]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )
               ;; (q/stroke 0.65 1 1)
               (let [s 4]
                 (q/line 0 s s s)
                 (q/line s s s 0)
                 )

               ;; (q/stroke 0.65 1 1)
               ;; (q/stroke-weight 10)
               ;; (q/line 0 90 -100 80)
               ;; (q/line 0 120 -120 90)
               ;; (q/no-fill)
               ;; (q/ellipse  0 0 390 280)
               ;; (q/quad 20 00 30 190 82 280 20 28)
               ;; (q/quad 20 00 40 190 22 122 30 28)
               ;; (q/fill 0.0 1 1)
               ;; (q/quad 00 00 20 20 32 22 00 0)
               ;; (q/fill 0.4 1 0.7 0.1)
               ;; (q/quad 80 80 120 80 92 92 90 90)
               ;; (q/quad 70 70 110 70 82 82 80 80)
               )
    ;; (do-around 9
    ;;            (q/no-stroke)
    ;;            (q/quad 40 40 42 50 90 50 42 40)
    ;;            )
    (q/stroke-weight 4)
    ;; (q/stroke 0.0 1 1)
    (q/stroke-weight 9)
    (do-around 40
               (q/bezier 0 120
                         90 140
                         -10 170
                         48 174))
    (q/no-stroke)
    (q/fill 0)
    ;; (q/fill 0.65 1 1)
    ;; (q/stroke 0.65 1 1)
    (q/stroke-weight 3)
    (do-around 40 (q/quad 0 180 11 200 18 200 6 180))
    ;; (no-2 22 90 100)
    ;; (no-2 20 50 90)
    )

  (q/blend-mode :blend)
  ;; (q/blend-mode :normal)
  ;; (q/display-filter :invert)


  ;; (q/save "test.png")
  (if (and (>= (q/frame-count) 10)
           (< (q/frame-count) 100)
           )
    (q/save-frame  "frame-####.png"))
  (when-not (= (:method state) :dev) (q/exit)))


(q/defsketch chromatrope-dev
  :size [400 400]
  :renderer :p2d
  :settings (fn [] (q/pixel-density 2) (q/smooth))
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
