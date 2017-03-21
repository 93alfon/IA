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
    (let ((navigate (navigate-worm-hole state (rest worm-holes))))
      (if (eq state (first (first  worm-holes)))
        (append (list
                  (make-action
                    :name 'navigate-worm-hole
                    :origin state
                    :final (second (first worm-holes))
                    :cost (third (first worm-holes))))
                navigate)
        navigate))))

; equivalente a
;
 (defun navigate-white-hole (state worm-holes)
   (navigate-worm-hole state worm-holes))


; (defun navigate-white-hole (state white-holes)
;   (when while-holes
;     (if (eq state (first (first  white-holes)))
;       (cons (navigate-white-hole state (rest white-holes))
;             (make-action
;               :name 'navigate-white-hole
;               :origin state
;               :final (second (first white-holes))
;               :cost (third (first white-holes))))
;       (navigate-white-hole state (rest white-holes)))))


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
    (<= (node-f node-1)
        (node-f node-2)))

(setf *uniform-cost*
  (make-strategy
    :name 'uniform-cost
    :node-compare-p 'node-g-<=))

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf *galaxy-M35*
 (make-problem
   :states *planets*
   :initial-state *planet-origin*
   :f-goal-test #'(lambda (state)
                    (f-goal-test-galaxy state *planets-destination*))
   :f-h #'(lambda (state)
              (f-h-galaxy (node-state state) *sensors*))
   :operators (list 'navigate-worm-hole 'navigate-white-hole)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expand-node (node problem)
  (mapcar #'(lambda (accion)
                (let ((ge     (+ (node-g node) (action-cost accion)))
                      (hache  (problem-f-h node)))
                  (make-node
                    :state (action-final accion)    ; state label
                    :parent node                    ; parent node
                    :action accion                  ; action that generated the current node from its parent
                    :depth (+ (if (node-depth node)
                                    (node-depth node)
                                    0)
                              1)  ; depth in the search tree
                    :g ge                           ; cost of the path from the initial state to this node
                    :h hache                        ; value of the heuristic
                    :f (+ ge hache))                ; g + h
                    )
            )
    (append (navigate-worm-hole (node-state node) *worm-holes*)
            (navigate-white-hole (node-state node) *white-holes*))))


;;; Casos de Prueba
;;; (setf node-00
;;;   (make-node :state 'Proserpina :depth 12 :g 10 :f 20))
;;;
;;; (print
;;;   (setf lst-nodes-00
;;;     (expand-node node-00 *galaxy-M35*)))
;;;
;;;(#S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27)
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert a list of nodes into another list of nodes
;;;
;;;
(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (sort (append nodes lst-nodes) (strategy-node-compare-p strategy)))

;;; Casps de Prueba
;;;
;;; (setf node-00
;;;   (make-node :state 'Proserpina :depth 12 :g 10 :f 20))
;;;
;;; (setf node-01
;;;  (make-node :state 'Avalon :depth 0 :g 0 :f 0))
;;;
;;; (setf node-02
;;;  (make-node :state 'Kentares :depth 2 :g 50 :f 50))
;;;
;;; --------------------------------------------------------------
;;; (print(insert-nodes-strategy (list node-00 node-01 node-02)
;;;   lst-nodes-00
;;;   *uniform-cost*))
;;;
;;;(#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27)
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50))
;;;
;;; ---------------------------------------------------------------
;;; (print (insert-nodes-strategy (list node-00 node-01 node-02)
;;;  (sort (copy-list lst-nodes-00) #'<= :key #'node-g)
;;;  *uniform-cost*))
;;;
;;;(#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27)
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34)
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Realiza la búsqueda para el problema dado utilizando una estrategia
;;; Evalúa:
;;; Si no hay solución: NIL
;;; Si hay solución: un nodo que cumple el test objetivo

(defun graph-search (problem strategy)

)

;;; Casos de Prueba
;;; (graph-search *galaxy-M35* *A-star*)
;;;
;;; #S(NODE :STATE SIRTIS
;;; :PARENT
;;; #S(NODE :STATE ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-star-search (problem)
  (graph-search problem *a-star* ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Realiza la búsqueda A* para el problema dado
;;; Evalúa:
;;; Si no hay solución: NIL
;;; Si hay solución: el nodo correspondiente al estado-objetivo

;;; Casos de Prueba
;;; (a-star-search *galaxy-M35*);->
;
; #S(NODE :STATE SIRTIS
; :PARENT
; #S(NODE :STATE ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree-path (node)

)

;;; Casos de Prueba
;;; (tree-path nil) ;;;NIL
;;; (tree-path #S(NODE :STATE MALLORY ...)) ;;;(KENTARES PROSERPINA MALLORY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-sequence (node)
  (when (and node-1 node-2)
    (>
      (node-depth node-1)
      (node-depth node-2))))

;;; Casos de Prueba
;;; (action-sequence (a-star-search *galaxy-M35*))
;;; (#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)...)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *depth-first*
 (make-strategy
    :name 'depth-first
    :node-compare-p 'depth-first-node-compare-p))

(defun depth-first-node-compare-p (node-1 node-2)
  (when (and node-1 node-2)
    (<
      (node-depth node-1)
      (node-depth node-2))))

;;; Casos de Prueba
;;; (tree-path (graph-search *galaxy-M35* *depth-first*))
