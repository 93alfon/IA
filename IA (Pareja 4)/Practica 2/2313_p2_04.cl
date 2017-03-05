;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Alfonso Bonilla Trueba y David Garcia Fernandez
;;;                 PAREJA 4 --- GRUPO 2313
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONTROL PUNTO PARA EL EDITOR DE ALEGRO

(setf *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))

(setf *white-holes*
  '((Avalon Mallory 2) (Avalon Proserpina 12)
    (Davion Proserpina 14) (Davion Sirtis 1)
    (Katril Davion 2) (Katril Mallory 6)
    (Kentares Avalon 3) (Kentares Katril 12)
    (Kentares Proserpina 10) (Mallory Katril 6)
    (Mallory Proserpina 17) (Proserpina Avalon 12)
    (Proserpina Davion 14) (Proserpina Mallory 17)
    (Proserpina Sirtis 10) (Sirtis Davion 1)
    (Sirtis Proserpina 10)))


(setf *worm-holes*
  '((Avalon Kentares 4) (Avalon Mallory 7)
    (Davion Katril 1) (Davion Sirtis 8)
    (Katril Davion 1) (Katril Mallory 5) (Katril Sirtis 10)
    (Kentares Avalon 4) (Kentares Proserpina 21)
    (Mallory Avalon 7) (Mallory Katril 5)
    (Mallory Proserpina 16) (Proserpina Kentares 21)
    (Proserpina Mallory 16) (Proserpina Sirtis 7)
    (Sirtis Davion 8) (Sirtis Katril 10)
    (Sirtis Proserpina 7)))


(setf *sensors*
 '((Avalon 5) (Davion 1)
   (Katril 3) (Kentares 4)
   (Mallory 7) (Proserpina 4)
   (Sirtis 0)))

(setf *planet-origin* 'Kentares)

(setf *planets-destination* '(Sirtis))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem definition
;;
(defstruct problem
 states ; List of states
 initial-state ; Initial state
 f-goal-test ; reference to a function that determines whether
 ; a state fulfills the goal
 f-h ; reference to a function that evaluates to the
 ; value of the heuristic of a state
 operators) ; list of operators (references to functions)
 ; to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Node in search tree
;;
(defstruct node
state ; state label
parent ; parent node
action ; action that generated the current node from its parent
 (depth 0) ; depth in the search tree
 (g 0) ; cost of the path from the initial state to this node
(h 0) ; value of the heuristic
 (f 0)) ; g + h
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Actions
;;
(defstruct action
name ; Name of the operator that generated the action
origin ; State on which the action is applied
final ; State that results from the application of the action
cost ) ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Search strategies
;;
(defstruct strategy
name ; Name of the search strategy
node-compare-p) ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun f-goal-test-galaxy (state planets-destination)
	(when (member state planets-destination)
			t))

;;; Casos de Prueba
;;; (f-goal-test-galaxy 'Sirtis *planets-destination*) ;-> T (o equivalente)
;;; (f-goal-test-galaxy 'Avalon *planets-destination*) ;-> NIL
;;; (f-goal-test-galaxy 'Urano *planets-destination*) ;-> NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun f-h-galaxy (state sensors)
  (when sensors
    (if (eq state (first (first sensors)))
      (second (first sensors))
      (f-h-galaxy state (rest sensors)))))

;;; Casos de Prueba
;;; (f-h-galaxy 'Sirtis *sensors*) ;-> 0
;;; (f-h-galaxy 'Avalon *sensors*) ;-> 5


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun navigate-worm-hole (state worm-holes)
(when worm-holes
    (if (eq state (first (first  worm-holes)))
      (cons (navigate-worm-hole state (rest worm-holes))
            (make-action
              :name 'navigate-worm-hole
              :origin state
              :final (second (first worm-holes))
              :cost (third (first worm-holes))))
      (navigate-worm-hole state (rest worm-holes)))))

; equivalente a
;
; (defun navigate-worm-hole (state worm-holes)
;   navigate-white-hole (state worm-holes))



(defun navigate-white-hole (state white-holes)
  (when while-holes
    (if (eq state (first (first  white-holes)))
      (cons (navigate-white-hole state (rest white-holes))
            (make-action
              :name 'navigate-white-hole
              :origin state
              :final (second (first white-holes))
              :cost (third (first white-holes))))
      (navigate-white-hole state (rest white-holes))))

;;; Casos de Prueba
;;; (navigate-worm-hole 'Katril *worm-holes*) ;->
;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL DAVION :COST 1)
; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL MALLORY :COST 5)
; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL SIRTIS :COST 10))
;;; (navigate-white-hole 'Urano *white-holes*) ;-> NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *A-star*
  (make-strategy 
    :name 'a-star 
    :node-compare-p 'node-star))

(defun node-star (node-1 node-2)

  )



(setf *uniform-cost*
  (make-strategy
    :name 'uniform-cost
    :node-compare-p 'node-g-<=))

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

