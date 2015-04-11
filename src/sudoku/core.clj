(ns sudoku.core
  (:gen-class))

(:use ['clojure.set :as set])
(use 'clojure.pprint)

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
(defn value-at-coordinates [board coordinates]
  ;helper-func: will return value at coord from board
  (get-in board coordinates))

(defn row-values [board [row _]]
  ;get value of each number in row, then store as a set
  (set (map #(value-at-coordinates board [row %]) (range 9)))
  )

(defn col-values [board [_ col]]
  ;same as row-values
  (set (map #(value-at-coordinates board [% col]) (range 9)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 2;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pair-of-coordinates [coordinates]
  ;will return every permutatin of coordinates that I pass it
  (for [row coordinates col coordinates]
    [row col])
  )

(defn top-left [[row col]]
  ;will return the coordinates of the top left corner of current block
  [(- row (mod row 3)) (- col (mod col 3))]
  )

(defn block-values [board coord]
  ;will start at the top-left-coord and will return set of block coordinates
  (let [top-left-coord (top-left coord)
        block-coordinates (map #(map + top-left-coord %) (pair-of-coordinates [0 1 2]))]
    (set (map #(value-at-coordinates board %) block-coordinates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Excersice 3;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn valid-values-for [board coord]
  ;find all the values already used in the block
  ;check if current coord already has a value (mabe make a helper-func)
  ;use set diff to get the possible values that the cell could take
  )

(defn -main
  [& args]
  (pprint sudoku-board)
  (println (str "row-values [0 2]: " (row-values sudoku-board [0 2])))
  (println (str "row-values [3 2]: " (row-values sudoku-board [3 2])))
  (println (str "col-values [0 2]: " (col-values sudoku-board [0 2])))
  (println (str "col-values [4 8]: " (col-values sudoku-board [4 8])))
  (println (str "block-values [0 2]: " (block-values sudoku-board [0 2])))
  (println (str "block-values [4 5]: " (block-values sudoku-board [4 5])))
  )
