(ns sudoku.core
  (:gen-class))

(use 'clojure.set)
(use 'clojure.pprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Testing Input;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def sudoku-board
    [[5 3 0 0 7 0 0 0 0]
    [6 0 0 1 9 5 0 0 0]
    [0 9 8 0 0 0 0 6 0]
    [8 0 0 0 6 0 0 0 3]
    [4 0 0 8 0 3 0 0 1]
    [7 0 0 0 2 0 0 0 6]
    [0 6 0 0 0 0 2 8 0]
    [0 0 0 4 1 9 0 0 5]
    [0 0 0 0 8 0 0 7 9]])

(def solved-board
    [[5 3 4 6 7 8 9 1 2]
    [6 7 2 1 9 5 3 4 8]
    [1 9 8 3 4 2 5 6 7]
    [8 5 9 7 6 1 4 2 3]
    [4 2 6 8 5 3 7 9 1]
    [7 1 3 9 2 4 8 5 6]
    [9 6 1 5 3 7 2 8 4]
    [2 8 7 4 1 9 6 3 5]
    [3 4 5 2 8 6 1 7 9]])

(def all-values #{1 2 3 4 5 6 7 8 9})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 1;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn value-at-coord [board coords]
  ;helper-func: will return value at coord from board
  (get-in board coords))

(defn row-values [board [row _]]
  ;get value of each number in row, then store as a set
  (set (map #(value-at-coord board [row %]) (range 9)))
  )

(defn column-values [board [_ col]]
  ;same as row-values
  (set (map #(value-at-coord board [% col]) (range 9)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 2;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pair-of-coords [coord]
  ;will return every permutatin of coordinates that I pass it
  (for [row coord col coord] [row col])
  )

(defn top-left [[row col]]
  ;will return the coordinates of the top left corner of current block
  [(- row (mod row 3)) (- col (mod col 3))]
  )

(defn block-values [board coord]
  ;will start at the top-left-coord and will return set of block coordinates
  (let [top-left-coord (top-left coord)
        block-coords (map #(map + top-left-coord %) (pair-of-coords [0 1 2]))]
    (set (map #(value-at-coord board %) block-coords))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 3;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-value? [board coord]
  ;returns true if non-zero value at coord in board
  (if (= (value-at-coord board coord) 0)
    false
    true))

(defn valid-values-for [board coord]
  ;find all the values already used in the block
  ;check if current coord already has a value (maybe make a helper-func)
  ;use set diff to get the possible values that the cell could take
  (if (has-value? board coord)
    #{} ;true cond
    ; false cond: take the union of all the row values of the coord,
    ; all the col values of the coord and the block values of the coord.
    ; Then take the difference from all possible values to return
    ; us the possible valid values.
    (clojure.set/difference all-values (clojure.set/union
                                (row-values board coord)
                                (column-values board coord)
                                (block-values board coord)
                                        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 4;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn filled? [board]
  ; make a list or set of all of the values in the board
  ; if there's a 0 in the list then the board is not filled.
  (let [all-board-values (for [row (range 9) col (range 9)]
                     (value-at-coord board [row col]))]
    (if (contains? (set all-board-values) 0)
      false  ;contains 0, not filled, false
      true   ;No 0, filled, true
      ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 5;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rows [board]
  ;run row-values on the whole board
  ;with variable rows and constant cols
  (map #(row-values board [% 0]) (range 9)))

(defn cols [board]
  ;run colums-values on the whole board
  ;with variable colums and constant rows
  (map #(column-values board [0 %]) (range 9)))

(defn blocks [board]
  ;for coords [0 0] [0 3] [0 9]
  ;           [3 0] [3 3] [3 9]
  ;           [9 0] [9 3] [9 9]
  ;range from 0-9 with intervals of 3
  ;then run block-values on those coords
  (for [row (range 0 9 3) col (range 0 9 3)] ;range from 0-9 with intervals of 3
    (block-values board [row col])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 6;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn valid-rows? [board]
  (every? #(empty? (clojure.set/difference all-values %)) (rows board)))

(defn valid-cols? [board]
  (every? #(empty? (clojure.set/difference all-values %)) (cols board)))

(defn valid-blocks? [board]
  (every? #(empty? (clojure.set/difference all-values %)) (blocks board)))

(defn valid-solution? [board]
  ; check if rows and cols and blocks are valid. Then true; else false
  (true? (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 7;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn change-coord-value [board coord new-value]
  (assoc-in board coord new-value))

(defn find-zero-cell [board]
  ;returns first cell it finds with a zero
  (first (filter #(not (has-value? board %))
                 (for [row (range 9) col(range 9)] [row col]))
         )
  )






(defn -main
  [& args]
  (println (str "\n****** EXERCISE 1 ******"))
  (println (str "row-values [0 2]: " (row-values sudoku-board [0 2])))
  (println (str "row-values [3 2]: " (row-values sudoku-board [3 2])))
  (println (str "column-values [0 2]: " (column-values sudoku-board [0 2])))
  (println (str "column-values [4 8]: " (column-values sudoku-board [4 8])))

  (println (str "\n****** EXERCISE 2 ******"))
  (println (str "block-values [0 2]: " (block-values sudoku-board [0 2])))
  (println (str "block-values [4 5]: " (block-values sudoku-board [4 5])))

  (println (str "\n****** EXERCISE 3 ******"))
  (println (str "has-value? [0 2]: " (has-value? sudoku-board [4 4])))
  (println (str "valid-values-for [0 0]: " (valid-values-for sudoku-board [0 0])))
  (println (str "valid-values-for [0 2]: " (valid-values-for sudoku-board [0 2])))

  (println (str "\n****** EXERCISE 4 ******"))
  (println (str "is sudoku-board filled? " (filled? sudoku-board)))
  (println (str "is solved-board filled? " (filled? solved-board)))

  (println (str "\n****** EXERCISE 5 ******"))
  (println (str "---Rows of sudoku-board: "))
  (pprint (rows sudoku-board))
  (println (str "---Rows of solved-board: "))
  (pprint (rows solved-board))
  (println (str "---Columns of sudoku-board: "))
  (pprint (cols sudoku-board))
  (println (str "---Colums of solved-board: "))
  (pprint (cols solved-board))
  (println (str "---Blocks of sudoku-board: "))
  (pprint (blocks sudoku-board))
  (println (str "---Blocks  of solved-board: "))
  (pprint (blocks solved-board))

  (println (str "\n****** EXERCISE 6 ******"))
  (println (str "valid rows for sudoku-board? ") (valid-rows? sudoku-board))
  (println (str "valid colums for sudoku-board? ") (valid-cols? sudoku-board))
  (println (str "valid blocks for sudoku-board? ") (valid-blocks? sudoku-board))
  (println (str "valid solution for sudoku-board? ") (valid-solution? sudoku-board))
  (println (str "valid rows for solved-board? ") (valid-rows? solved-board))
  (println (str "valid colums for solved-board? ") (valid-cols? solved-board))
  (println (str "valid blocks for solved-board? ") (valid-blocks? solved-board))
  (println (str "valid solution for solved-board? ") (valid-solution? solved-board))

  (println (str "\n****** EXERCISE 7 ******"))

  )
