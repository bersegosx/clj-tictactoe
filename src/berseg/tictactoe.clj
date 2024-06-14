(ns berseg.tictactoe
  (:gen-class))

(def state (atom {:board [[\_ \_ \_]
                          [\_ \_ \_]
                          [\_ \_ \_]]
                  :turns []}))
;; Tools
(defn in-range?
  [value [start end]]
  (and (>= value start)
       (<= value end)))

(defn equal?
  [mark]
  (partial
   (fn [mark elements]
     (apply = mark elements))
   mark))

(defn any-true?
  [col]
  (some true? col))

;; api
(defn get-next-mark
  [turns]
  (let [is-even (-> (count turns)
                    (rem 2)
                    (zero?))]
    (if is-even "X" "0")))

(defn number-to-index
  [pos]
  (let [offset (dec pos)
        row-index (quot offset 3)
        elem-index (mod offset 3)]
    [row-index elem-index]))

(defn create-next-board
  [board [row col] mark]
  (assoc-in board [row col] mark))

(defn place-move
  [board turns pos]
  (let [next-mark (get-next-mark turns)
        next-turns (conj turns next-mark)
        new-board (create-next-board
                   board
                   (number-to-index pos)
                   next-mark)]
    [new-board next-turns]))

(defn show-board
  [board]
  (doseq [row board]
    (println row)))

(defn is-valid-move?
  [pos board]
  (and (in-range? pos [1 9])
       (let [[row col] (number-to-index pos)
             mark (get-in board [row col])]
         (= \_ mark))))

(defn match?
  [board mark & numbers]
  (let [positions (map number-to-index numbers)
        row (map #(get-in board %) positions)
        =r= (equal? mark)]
    (=r= row)))

(defn has-winner-mark?
  [board mark]
  (or
    ;;  horizontal check
   (any-true?
    (map (equal? mark) board))

    ;;  vertical check
   (any-true?
    (apply map #(apply = mark %&) board))
   
   ;; diagonal checks
   (match? board mark 1 5 9)
   (match? board mark 3 5 7)))

(defn has-winner?
  [board]
  (or (has-winner-mark? board "X")
      (has-winner-mark? board "0")))

(defn game-loop
  [pos]
  (if (is-valid-move? pos (:board @state))
    (let [[next-board next-turns] (place-move
                                   (:board @state)
                                   (:turns @state)
                                   pos)]
      (reset! state
              (assoc @state :board next-board :turns next-turns))
      (if (has-winner? next-board)
        (println "has a winner!")
        (println "...no winner, continue")))
    (println "wrong move w/")))

(comment
  (println (:board @state))
  (get-next-mark [1 2])
  (number-to-index 4)
  (show-board
   (create-next-board
    (:board @state)
    (number-to-index 9)
    (get-next-mark (:turns @state))))
  (show-board
   (place-move (:board @state) (:turns @state) 5))
  (in-range? 4 [1 5])
  (is-valid-move? 3 (:board @state))
  (map game-loop [1 2 4 5 7])

  (some true?
        (apply map #(apply = 1 %&)
               [[1 0 0] [1 0 0] [1 0 0]]))
  )