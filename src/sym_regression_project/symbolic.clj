(ns sym-regression-project.symbolic)

;; Functions to create and unpack additions like (+ 1 2)
(defn make-add [a b] (list '+ a b))
(defn addition? [x] (and (=(count x) 3) (= (first x) '+)))
(defn add1   [x] (second x))
(defn add2   [x] (second (rest x)))

(defn make-sub [a b] (list '- a b))
(defn subtraction? [x] (and (= (count x) 3) (= (first x) '-)))
(defn sub1 [x] (second x))
(defn sub2 [x] (second (rest x)))


;; Similar for multiplications (* 1 2)
(defn make-mul [a b] (list '* a b))
(defn multiplication? [x] (and (=(count x) 3) (= (first x) '*)))
(defn mul1   [x] (second x))
(defn mul2   [x] (second (rest x)))


;; Functions to create and unpack sine and cosine eg (Math/sin x)
(defn make-sin [a] (list 'Math/sin a))
(defn make-cos [a] (list 'Math/cos a))
(defn sin? [x] (and (= (count x) 2) (= (first x) 'Math/sin)))
(defn cos? [x] (and = (count x) 2) (= (first x) 'Math/cos))
(defn trig-arg [x] (second x))


;; Differentiation. 
(defn deriv [exp var]
  (cond (number? exp) 0                                                              
        (symbol? exp) (if (= exp var) 1 0)                                          

        (addition? exp) (make-add (deriv (add1 exp) var) (deriv (add2 exp) var))
        (subtraction? exp) (make-sub (deriv (sub1 exp) var) (deriv (sub2 exp) var))      
        (multiplication? exp) (make-add (make-mul (deriv (mul1 exp) var) (mul2 exp))
                                        (make-mul (mul1 exp) (deriv (mul2 exp) var)))
        (sin? exp) (make-mul (deriv (trig-arg exp) var) (make-cos (trig-arg exp)))
        (cos? exp) (make-mul (deriv (trig-arg exp) var) (make-sub 0 (make-sin (trig-arg exp))))
        :else :error))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/deriv</span>","value":"#'user/deriv"}
;; <=

;; @@
 (deriv '(* x (Math/cos (* x x))) 'x)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>Math/cos</span>","value":"Math/cos"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* x x)"}],"value":"(Math/cos (* x x))"}],"value":"(* 1 (Math/cos (* x x)))"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* 1 x)"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(* x 1)"}],"value":"(+ (* 1 x) (* x 1))"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>Math/sin</span>","value":"Math/sin"},{"type":"list-like","open":"<span class='clj-list'>(<span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* x x)"}],"value":"(Math/sin (* x x))"}],"value":"(- 0 (Math/sin (* x x)))"}],"value":"(* (+ (* 1 x) (* x 1)) (- 0 (Math/sin (* x x))))"}],"value":"(* x (* (+ (* 1 x) (* x 1)) (- 0 (Math/sin (* x x)))))"}],"value":"(+ (* 1 (Math/cos (* x x))) (* x (* (+ (* 1 x) (* x 1)) (- 0 (Math/sin (* x x))))))"}
;; <=

;; @@

(defn poly->fnform [poly] (list 'fn '[x] poly))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/poly-&gt;fnform</span>","value":"#'user/poly->fnform"}
;; <=

;; @@
(def poly '(* x (Math/cos (* x x))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/poly</span>","value":"#'user/poly"}
;; <=

;; @@

;; @@
