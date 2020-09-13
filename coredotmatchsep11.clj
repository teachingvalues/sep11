(ns coredotmatch.core
  (:require [clojure.core.match :refer [match]]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Bye, World!"))

(foo 'bob)

(match [false true false true] 
[ _ false true true] :1 
[false true _ _] :2
[_ _ false true] :3 
[ _ true true true] :4) 



(let [x true]
  (if x :1 :2)) 


(let [x 3] 
  (case x
     1 :1
     2 :2
     3 :3
     :4)) 


(let [x 3] 
  (match x 
     1 :1
     2 :2
     3 :3 
     :else :4)) 

;;; ----- Nested vectors -----

(match [[1 2 3] [4 5 6]]
  [[_ _ 2] [4 _ _]] :1
  [[1 1 3] [_ 5 _]] :2
  [[_ 2 3] [4 5 _]] :3
  :else :4)


;;; ----- Sequences ----

(match '(1 2 3)
  ([1 3 _] :seq) :1
  ([1 _ 2] :seq) :2
  ([1 _ 3] :seq) :3
  :else :4)

;;; ----- Maps -----

(match {:a 1 :b 1}
{:a _ :b 2} :1
{:a 1 :b _} :2
{:c 3 :d _ :e 4} :3)

;;; ----- Binding ----

(match [3 2 3]
  [3 2 a] [:1 a]
  [a 2 4] [:2 a])

;;; ----- Or Patterns -----

(match [1 2 3]
  [1 (:or 3 4) 3] :1
  [1 (:or 2 5) 3] :2)

;;; ----- Guards -----

(match [1 2]
  [(_ :guard odd?) (_ :guard #(odd? %))] :1
  [(_ :guard (fn [n] (odd? n))) _] :2)



(defn fibonacci [n]
  (match n
    (_ :guard #(<= % 0)) 0
    1 1
    _ (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))


(fibonacci 7) 
(map fibonacci (range 15))

(defn greet [names] 
  ;(print-str names))
  (match 
   (into [] (re-seq #"\w+" names))
    [nickname] {:nickname nickname}
    [first-name last-name] {:first-name first-name :last-name last-name}
    [first-name middle-name last-name] {:middle-name middle-name}
    [title last-name "of" "House" house-name & more] { :title title :house-name house-name}
    [& all-names] (str (count all-names) " names is too damn high!")))



(re-seq #"\w+" "names")
(re-seq #"\w+" "bod and tom")
(into [] (re-seq #"\w+" "a ans b and c"))

(greet "Dany")
(greet "Dany")
(greet "Daenerys Targaryen")
(greet "Daenerys Stormborn Targaryen")
(greet "Queen Daenerys of House Targaryen, First of Her Name")
(greet "Daenerys Stormborn of the House Targaryen, the First of Her Name, 
the Unburnt, Queen of Meereen, Queen of the Andals and the Rhoynar 
and Mother of Dragons")