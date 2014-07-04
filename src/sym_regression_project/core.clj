(ns sym-regression-project.core
(:require [clojure.zip :as zip])
(use [clojure repl pprint walk]))

;; This program will run a simple symbolic regression using data
;; supplied in interface.clj. 

;; Define the protected division function.
(defn pdiv [x y] (if (zero? y) 1 (/ x y)))

;; Define set of primitive operators.
(def functions
  [{:name '+ :arity 2}
   {:name '- :arity 2}
   {:name '* :arity 2}
   {:name 'pdiv :arity 2}])

;; Define function terminals (x and a random constant).
(def terminals
  [(constantly 'x)
   rand])

;; Recursive implementation of full tree generation.
(defn random-full-tree
  [functions terminals depth]
  (if (= depth 0)
    ((rand-nth terminals))
    (let [func (rand-nth functions)
          leaves (repeatedly (:arity func) #(random-full-tree functions terminals (- depth 1)))]
      (conj leaves (:name func)))))

;; Function that generates a data set from a given s-expression.
(defn functionalise [ex] (eval (list 'fn '[x] ex)))

;; Function that generates an initial population (size n) of s-expressions.
(defn make-initial-population
  [n max-depth]
  (repeatedly n #(random-full-tree functions terminals (+ 1 (rand-int (- max-depth 1))))))

;; Generate zipper constructor.
(defn expr-zip
  [expr]
  (zip/zipper
    (constantly true)
    (fn [node] (if (seq? node) (rest node) nil))
    (fn [node children] (with-meta (conj children (first node)) (meta node)))
    expr))

;; Define function that replaces part of a tree with another given tree.
(defn tree-replace
  [tree index new-tree]
  (let [subtree-z (nth (iterate zip/next (expr-zip tree)) index)
        new-zipper (zip/replace subtree-z new-tree)]
    (zip/root new-zipper)))

;; Define function that counts number of nodes.
(defn count-nodes
  [ex]
  (if (seq? ex)
  	(+ 1 (apply + (map count-nodes (rest ex))))
    1))

;; First parameter to be optimised.
(defn score-1
  "This function calculates how well a candidate expression fits a supplied data set. This is
   calculated using a modified chi squared test. A score of zero refers to an expression
   that perfectly describes the data set. A score much below zero refers to an expression
   that poorly descrives the data set."
  [data ex]
  (let [f (functionalise ex)]
    (* -1 (apply + (map #(Math/abs (- (f (first %)) (second %))) data)))))

;; Second parameter to be optimised.
(defn score-2
  "This function counts the number of nodes in an expression and outputs the negative of
  that number. This is the metric by which an the complexity of an expression is determined."
  [count-nodes ex]
  (* -1 (count-nodes ex)))

;; Define mutation operation.
(defn mutate-expr
  [expr new-tree-func]
  (let [size (count-nodes expr)
        target (rand-int size)]
    (tree-replace expr target (new-tree-func))))

;; Define crossover operation
(defn sub-tree
  [tree index]
  (zip/node (nth (iterate zip/next (expr-zip tree)) index)))

(defn crossover-expr
  [expr1 expr2]
  (let [size1 (count-nodes expr1)
        target1 (rand-int size1)
        size2 (count-nodes expr2)
        target2 (rand-int size2)
        subtree1 (sub-tree expr1 target1)
        subtree2 (sub-tree expr2 target2)]
    [(tree-replace expr1 target1 subtree2) (tree-replace expr2 target2 subtree1)]))

;; Map fitness values onto an expression.
(defn score-population
  [population score-1-func score-2-func]
  (map (fn [expr] {:expr expr 
                   :score-1 (score-1-func expr) 
                   :score-2 (score-2-func expr) 
                   :dominated false
                   :fitness 0})
       population))

;; Determine if one expression is dominated by another expression. 
(defn is-dominated
  "This function tests whether expression x is dominated by function y.
   An expression is dominated if both score-1 and score-2 of a comparison
   expression is better. "
  [x y]
  (if (or (and (<= (:score-1 x) (:score-1 y)) (< (:score-2 x) (:score-2 y))) 
          (and (< (:score-1 x) (:score-1 y)) (<= (:score-2 x) (:score-2 y))))
    (assoc x :dominated true) x))

;; Determine whether a function should go into the archive.
(defn should-be-archived
  "If expression x is dominated by expression y and empty list is
   outputted. If expression x is not dominated, x is outputted."
  [is-dominated x y]
  (if (:dominated (reduce is-dominated x y))
    () x))

(defn into-archive
  "This function maps _should-be-archived_ over every value in
   sequence x. The function outputs a sequence of all expression of x
   that should go into the archive."
  [should-be-archived is-dominated x y]
  (map #(should-be-archived is-dominated % y) x))

;; In the SPEA algorithm, fitness is determined by a strength function.
(defn strength-func
  "The strength of an individual in the archive is determined by the number
   of current population members it dominates divided by (1 + population size).
   An archive member's fitness is equal to its strength."
  [archive popn]
  (assoc archive :fitness 
                  (/ 
                   (count 
                    (filter #(and (>= (:score-1 archive) (:score-1 %)) (>= (:score-2 archive) (:score-2 %))) popn))
                    (+ 1 (count popn)))))

(defn sum-strengths
  "This function sums the strengths of all supplied archive members."
  [archive]
  (reduce + (map #(:fitness %) archive)))

(defn fitness-population
  "The fitness of a population individual is determined by 1 + the sum of strengths
   of all the individuals that cover it."
  [popn scored-archive]
  (assoc popn 
    :fitness (+ 1 
              (sum-strengths (filter #(and (<= (:score-1 popn) (:score-1 %)) (<= (:score-2 popn) (:score-2 %))) scored-archive)))))

;; Define tournament selection function. 
(defn tournament-selector
  "This function selects a number of individuals from a population determined by
   tournament-size. The tournament candidate with the minimum fitness value is
   outputted."
  [scored-popn tournament-size]
  (let [competitors (repeatedly tournament-size #(rand-nth scored-popn))]
    (:expr (apply min-key :fitness competitors))))

















