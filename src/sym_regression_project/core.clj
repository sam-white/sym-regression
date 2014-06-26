(ns sym-regression-project.core
(:require [clojure.zip :as zip])
(use [clojure repl pprint walk]))

;; This program will run a simple symbolic regression using data from a quadratic function. 


;; Generate a data set to fit to
(def data (doall (map (fn [x] [x (+ (* 3 x) (* 2 x x))]) (range 0 10 0.5)))) 

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

;; First parameter to be optimised. Currently this performs a modified chi-squared test on the data.
(defn score-1
  [data ex]
  (let [f (functionalise ex)]
    (* -1 (apply + (map #(Math/abs (- (f (first %)) (second %))) data)))))

;; Second parameter to be optimised. Currently this counts the number of nodes in an s-expression.
(defn score-2
  [ex]
  (if (seq? ex)
  	(+ 1 (apply + (map score-2 (rest ex))))
    1))

;; Define optimisation function using scalarisation technique. Alter   coefficients to tune optimisation. Coefficients must sum to 1.
(defn score-multi
  [data ex]
  (+ (* 1 (score-1 data ex)) (* 0 (score-2 ex))))

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
  	(+ 1 (apply + (map score-2 (rest ex))))
    1))

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

;; Implement tournament selection.
(defn score-population
  [population score-func score-1-func score-2-func]
  (map (fn [expr] {:expr expr :score (score-func expr) :score-1 (score-1-func expr) :score-2 (score-2-func expr)}) population))

(defn tournament-selector
  [scored-popn tournament-size]
  (let [competitors (repeatedly tournament-size #(rand-nth scored-popn))]
    (:expr (apply max-key :score competitors))))

;; Function that generates next generation.
(defn evolve
  [population config]
  (let [{:keys [score-func score-1-func score-2-func mutation-new-tree-func tournament-size clone-n mutate-n crossover-n]} config
        scored-popn (score-population population score-func score-1-func score-2-func)
        clones (repeatedly clone-n #(tournament-selector scored-popn tournament-size))
        mutations (repeatedly mutate-n 
                              #(mutate-expr 
                                (tournament-selector scored-popn tournament-size)
                                mutation-new-tree-func))
        crossovers (reduce into (repeatedly crossover-n
                               #(crossover-expr
                                 (tournament-selector scored-popn tournament-size)
                                 (tournament-selector scored-popn tournament-size))))]
    (into clones (into mutations crossovers))))















