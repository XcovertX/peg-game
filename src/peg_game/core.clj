(ns peg-game.core
  (:require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn -main
  "Main entry point to the game."
  [& args]
  (println "Welcome to the Peg Game!!"))

(def board
  "board that game is played on"
  {1  {:pegged true, :connections {6 3, 4 2}},
   2  {:pegged true, :connections {9 5, 7 4}},
   3  {:pegged true, :connections {10 6, 8 5}},
   4  {:pegged true, :connections {13 8, 11 7, 6 5, 1 2}},
   5  {:pegged true, :connections {14 9, 12 8}},
   6  {:pegged true, :conenctions {15 10, 13 9, 4 5, 1 3}},
   7  {:pegged true, :connections {9 8, 2 4}},
   8  {:pegged true, :connections {10 9, 3 5}},
   9  {:pegged true, :connections {7 8, 2 5}},
   10 {:pegged true, :connections {8 9, 3 6}},
   11 {:pegged true, :connections {13 12, 4 7}},
   12 {:pegged true, :connections {14 13, 5 8}},
   13 {:pegged true, :connections {15 14, 11 12, 6 9, 4 8}},
   14 {:pegged true, :connections {12 13, 5 9}},
   15 {:pegged true, :connections {13 14, 6 10}},
   :rows 5})

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri
  "Calls tri* and binds it to tri"
  (tri*))

(defn triangular?
  "Is the number triangular?"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of the row"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to"
  [position]
  (inc (count (take-while #(> position %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos position neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[position destination] [destination position]])
    board))

(defn connect-right
  [board max-pos position]
  (let [neighbor (inc position)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? position))
      (connect board max-pos position neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos position]
  (let [row (row-num position)
        neighbor (+ row position)
        destination (+ 1 row neighbor)]
    (connect board max-pos position neighbor destination)))

(defn connect-down-right
  [board max-pos position]
  (let [row (row-num position)
        neighbor (+ 1 row position)
        destination (+ 2 row neighbor)]
    (connect board max-pos position neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos position]
  (let [pegged-board (assoc-in board [position :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos position))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

 (defn new-board
   "This creates a new board with the given number of rows"
   [rows]
   (let [initial-board {:rows rows}
         max-pos (row-tri rows)]
     (reduce (fn [board position] (add-pos board max-pos position))
             initial-board
             (range 1 (inc max-pos)))))


