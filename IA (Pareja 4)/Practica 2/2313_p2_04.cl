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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Actions
;;
(defstruct action
  name ; Name of the operator that generated the action
  origin ; State on which the action is applied
  final ; State that results from the application of the action
  cost ) ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Search strategies
;;
(defstruct strategy
  name ; Name of the search strategy
  node-compare-p) ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	f-goal-test-galaxy (state planets-destination)
;;;	Comprueba si se ha alcanzado el objetivo.
;;;
;;;	INPUT:
;;;		state: Estado que se quiere comprobar si es objetivo
;;;		planets-destination: Lista que contiene los planetas destino
;;;	OUTPUT:
;;;		T si el estado es el objetivo o NIL en caso contrario
;;;
(defun f-goal-test-galaxy (state planets-destination)
	(when (member state planets-destination)
			t))

;;; Casos de Prueba
;;; (f-goal-test-galaxy 'Sirtis *planets-destination*) ;-> T
;;; (f-goal-test-galaxy 'Avalon *planets-destination*) ;-> NIL
;;; (f-goal-test-galaxy 'Urano *planets-destination*) ;-> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	f-h-galaxy (state sensors)
;;;	Calcula la el valor de la heuristica en el estado que recibe.
;;;
;;;	INPUT:
;;;		state: Estado del que calcular el valor heuristico
;;;		sensors: Lista de pares planeta-valor heuristico
;;;	OUTPUT:
;;;		Valor de la heuristica o NIL si no se ha podido calcular
;;;
(defun f-h-galaxy (state sensors)
  (when sensors
    (if (eq state (first (first sensors)))
      (second (first sensors))
      (f-h-galaxy state (rest sensors)))))

;;; Casos de Prueba
;;; (f-h-galaxy 'Sirtis *sensors*) ;-> 0    ;Caso Tipico
;;; (f-h-galaxy 'Avalon *sensors*) ;-> 5    ;Caso Tipico
;;; (f-h-galaxy 'Tierra *sensors*) ;-> NIL	;Caso especial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	navigate-worm-hole (state worm-holes)
;;;	Busca las acciones posibles desde el estado actual a traves
;;; de agujeros de gusano.
;;;
;;;	INPUT:
;;;		state: Estado en el que se esta
;;;		white-holes: Lista que contiene los caminos entre agujeros
;;;                de gusano y el coste de los mismos
;;;	OUTPUT:
;;;		Acciones posibles desde el estado actual o NIL si no hay
;;;   acciones posibles
;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	navigate-white-hole (state white-holes)
;;;	Busca las acciones posibles desde el estado actual a traves de agujeros
;;; blancos
;;;
;;;	INPUT:
;;;		state: Estado en el que se esta
;;;		white-holes: Lista que contiene los caminos entre agujeros blancos y el
;;;                coste de los mismos
;;;	OUTPUT:
;;;		Acciones posibles desde el estado actual o NIL si no hay acciones
;;;   posibles
;;;
 (defun navigate-white-hole (state worm-holes)
   (navigate-worm-hole state worm-holes))


;;; Casos de Prueba
;;; (navigate-worm-hole 'Katril *worm-holes*) ;->
;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL DAVION :COST 1)
; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL MALLORY :COST 5)
; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL SIRTIS :COST 10))
;;; (navigate-white-hole 'Urano *white-holes*) ;-> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	(defun expand-node (node problem)
;;;	Expande el nodo segun el problema a tratar, devolviendo todos los nodos
;;; a los que se puede ir.
;;;
;;;	INPUT:
;;;		node: Nodo a expandir
;;;		problem: Problema bajo estudio segun el cual realizar la expansion
;;;	OUTPUT:
;;;		Nodos a los que se puede ir desde el nodo expandido o NIL
;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	defun insert-nodes-strategy (nodes lst-nodes strategy)
;;;	Inserta una lista de nodos en otra lista de acuerdo a una estrategia.
;;;
;;;	INPUT:
;;;		nodes: Lista de nodos a insertar
;;;		lst-nodes: Lista de nodos en la que insertar
;;;		strategy: Estrategia que seguir para la insercion
;;;	OUTPUT:
;;;		Lista resultado de la insercion
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	graph-search (problem strategy)
;;;	Realiza busqueda en grafo
;;;
;;;	INPUT:
;;;		problem: Problema sobre el que realizar la busqueda
;;;		strategy: Estrategia que seguir para la busqueda
;;;	OUTPUT:
;;;		Resulta de la busqueda realizada por la funcion graph-search-aux
;;;
(defun graph-search (problem strategy)

)
;;; Casos de Prueba
;;; (graph-search *galaxy-M35* *A-star*)
;;;
;;; #S(NODE :STATE SIRTIS
;;; :PARENT
;;; #S(NODE :STATE ...
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	a-star-search (problem)
;;;	Realiza la busqueda A* para el problema dado
;;;
;;;	INPUT:
;;;		problem: Problema sobre el que realizar la busqueda
;;;	OUTPUT:
;;;		Resultado de la busqueda o NIL si no se ha podido realizar
;;;
(defun a-star-search (problem)
  (graph-search problem *a-star* ))

;;; Casos de Prueba
;;; (a-star-search *galaxy-M35*);->
;
; #S(NODE :STATE SIRTIS
; :PARENT
; #S(NODE :STATE ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	defun tree-path (node)
;;;	Llama a la funcion que muestra el camino seguido para llegar a un nodo.
;;;
;;;	INPUT:
;;;		node: Nodo al que se ha llegado
;;;	OUTPUT:
;;;		Lista con los nombres de los planetas por los que se ha pasado para
;;;         llegar al nodo o NIL.
;;;
(defun tree-path (node)

)
;;; Casos de Prueba
;;;
;;; (tree-path nil) ; -> NIL
;;; (tree-path #S(NODE :STATE MALLORY ...)) ; -> (KENTARES PROSERPINA MALLORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	defun action-sequence (node)
;;;	Llama a la funcion que muestra la secuencia de acciones para llefar a un nodo.
;;;
;;;	INPUT:
;;;		node: Nodo al que se ha llegado
;;;	OUTPUT:
;;;		Lista con las acciones que se han realizado para llegar al  nodo o NIL.
;;;
(defun action-sequence (node)
)

;;; Casos de Prueba
;;;
;;; (action-sequence (a-star-search *galaxy-M35*))
;;; (#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *depth-first*
 (make-strategy
    :name 'depth-first
    :node-compare-p 'depth-first-node-compare-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	depth-first-node-compare-p (node-1 node-2)
;;;	Realiza una busqueda en profundidad
;;;
;;;	INPUT:
;;;		node-1: Nodo a comparar
;;;		node-2: Nodo a comparar
;;;	OUTPUT:
;;;		Lista con los nombres de los planetas de los nodos expandidos hasta
;;;         encontrar la solucion.
;;;
(defun depth-first-node-compare-p (node-1 node-2)
  (when node-1
    (when node-2
      (>
        (node-depth node-1)
        (node-depth node-2)))))

;;; Casos de Prueba
;;; (tree-path (graph-search *galaxy-M35* *depth-first*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p 'breadth-first-node-compare-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	breadth-first-node-compare-p (node-1 node-2)
;;;	Realiza una busqueda en anchura
;;;
;;;	INPUT:
;;;		node-1: Nodo a comparar
;;;		node-2: Nodo a comparar
;;;	OUTPUT:
;;;		Lista con los nombres de los planetas de los nodos que recorre hasta
;;;         encontrar la solucion.
;;;
(defun breadth-first-node-compare-p (node-1 node-2)
  (when node-1
    (when node-2
      (<
        (node-depth node-1)
        (node-depth node-2)))))

;;; Casos de Prueba
;;; (tree-path (graph-search *galaxy-M35* *breadth-first*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
