(ns analog-neural-network-tools.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def cut [255 0 0])
; (def engrave [0 055 0])
;; (def cut [255 255 255 0])
(def engrave [0 0 0])

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

(defn puzzle-join
  [{:keys [from-d to-d rotation]}]
  (q/with-rotation [rotation]
    (q/with-stroke cut
      (q/stroke-weight 1)
      (q/begin-shape)
      (q/vertex 0 (* -1/2 from-d))

      (q/bezier-vertex
       0  (q/map-range 0.1 0 1 (* -1/2 to-d) (* -1/2 from-d))

       20 (q/map-range 1.2 0 1 (* -1/2 to-d) (* -1/2 from-d))

       ;; middle point
       20 (q/map-range 0.5 0 1 (* -1/2 to-d) (* -1/2 from-d))
       )

      (q/bezier-vertex
       20 (q/map-range -0.2 0 1 (* -1/2 to-d) (* -1/2 from-d))
       0  (q/map-range 0.9 0 1 (* -1/2 to-d) (* -1/2 from-d))
       0  (* -1/2 to-d))

      (q/end-shape)
      )
    )
  )

(defn function-round
  [{:keys [fn start end points max min radius mode numbers? tick-size text-fn] :or {mode :outer numbers? true tick-size 4 text-fn identity}}]
  ;; (q/with-stroke cut
  ;;   (q/ellipse 0 0 radius radius))

  (q/with-stroke engrave
    (doseq [i points]
      (q/with-rotation
        [(q/map-range
          (fn i)
          min max 0 (* 2 (Math/PI)))]
        (q/with-fill engrave
          (let [number (apply str (take (if (< (text-fn i) 0) 4 3)
                                        (str (text-fn i))))
                x (- (* (count number) (if (rational? i) 3 2)))
                y ((case mode :inner + :outer -) (* 1/2 radius)
                   (if (rational? i) 18 12))]

            (if (rational? i)
              (q/text-size 12)
              (q/text-size 8))
            (q/push-matrix)
            (q/translate (+ x) y)
            (when (= mode :inner)
              (q/text-align :right)
              (q/rotate Math/PI)
              )
            (when numbers?
              (q/text number 0 0))
            (q/text-align :left)
            (q/pop-matrix)
            ))

        (q/no-fill)
        (when (rational? i)
          (q/stroke-weight 2))
        (q/line 0 (/ radius 2)
                0 (- (/ radius 2)
                     ((case mode :inner - :outer +)
                      (if (rational? i) 10 tick-size))))
        (q/stroke-weight 1)))))




(defn sigmoid [x]
  (Math/tanh x))

(defn log [x]
  (Math/log x))


(defn setup []
  (q/text-mode :shape)
  (q/no-fill)
  {:color 0
   :angle 0})

(defn update [method]
  (fn [state] {:method method}))

;; (def mult-outer
;;    {:fn log :start 1 :end 10
;;     :points (range 1 10)
;;     :max 0 :min 2.302 :radius 700}
;;    {:fn log :start 1 :end 10
;;     :points (range 1 10 0.1)
;;     :max 0 :min 2.302 :radius 700 :numbers? false}
;;    {:fn log :start 1 :end 10
;;     :points (range 1 2 0.1)
;;     :max 0 :min 2.302 :radius 700}
;;    {:fn log :start 1 :end 10
;;     :points (range 2 3 0.2)
;;     :max 0 :min 2.302 :radius 700}
;;   )

(def mult-outer
  ((juxt
    identity
    #(assoc % :points (range 1 10 0.1) :numbers? false)
    #(assoc % :points (range 1 2 0.1))
    #(assoc % :points (range 2 3 0.2)))
   {:fn log :start 1 :end 10
    :points (range 1 10)
    :max 0 :min 2.302 :radius 700}))

(def add-outer
  ((juxt
    identity
    #(assoc % :points (range -9 9 0.1) :numbers? false)
    #(assoc % :points (range -9 9 0.05) :numbers? false :tick-size 2))
   {:fn identity :points (range -9 10 1) :max 10 :min -10 :radius 500}))


(defn draw [state]
  (q/background 255)
  ; (q/with-fill [0 0 0]
  ;   (q/text-size 18)
  ;   (q/text "∑x" 10 18)
  ;   (q/text-size 10)
  ;   (q/text "i" 34 20))
  ;; (function-block sigmoid -4 4 0.2)
  (q/no-fill)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]

    ; draw centre point
    (q/with-stroke [0]
      (q/ellipse 0 0 2 2))

    ; draw outer multiplier disc
    (q/ellipse 0 0 800 800)
    (mapv function-round
          (map #(assoc % :mode :inner)
               mult-outer))
    ; draw inner multiplier disc
    (q/ellipse 0 0 700 700)
    (mapv function-round mult-outer)

    ;; (puzzle-join
    ;;  {:from-d 700 :to-d 800 :rotation 1})
    ;; (puzzle-join
    ;;  {:from-d 100 :to-d 200 :rotation 0})

    ;; summer
    ; draw inner add disc
    (q/ellipse 0 0 591 591)
    (q/ellipse 0 0 500 500)
    (mapv function-round
          (map #(assoc % :radius 590 :numbers? false)
               add-outer))
    (mapv function-round
          (map #(assoc % :mode :inner)
               add-outer))
    ; draw outer add disc
    (q/ellipse 0 0 600 600)
    (mapv function-round
          (map #(assoc % :numbers? false) add-outer))
    ; draw sum disc
    (q/ellipse 0 0 410 410)
    (mapv function-round
          (map #(assoc % :radius 410 :mode :inner :fn - :text-fn (partial max 0))
               add-outer))

    ; draw sum indicator
    (q/with-fill engrave
      (q/triangle 0 -296 -2 -300 2 -300)
      (q/triangle 0 -204 -2 -200 2 -200)
      )
    (q/ellipse 0 0 400 400)
    ;; (function-round
    ;;  {:fn identity :start 1 :end 10 :points (range -1 1 0.1) :max -1 :min 1 :radius 600})
    ;; (function-round
    ;;  {:fn identity :start 1 :end 10 :points (range -1 1 0.1) :max -1 :min 1 :radius 800})
    )
  (when (= :print (:method state)) (q/exit)))

(max 1 2)

(log 0.01)

(mod 1.5 1)

(q/defsketch analog-neural-network-tools-dev
  :size [880 880]
  :renderer :java2d
  :setup setup
  :update (update :dev)
  :middleware [m/fun-mode]
  :draw draw)

(q/defsketch analog-neural-network-tools-print
  :size [880 880]
  :renderer :pdf
  :output-file "output.pdf"
  :setup setup
  :update (update :print)
  :middleware [m/fun-mode]
  :draw draw)
