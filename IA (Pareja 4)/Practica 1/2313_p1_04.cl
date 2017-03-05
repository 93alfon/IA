;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Alfonso Bonilla Trueba y David Garcia Fernandez
;;;                 PAREJA 4 --- GRUPO 2313
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lp-rec (x p)
;;; Calcula la norma Lp de un vector de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; p: orden de la norma que se quiere calcular
;;;
;;; OUTPUT: norma Lp de x
;;;
(defun lp-rec (x p)
  (unless (< p 1) ;;; Comprobacion p >= 1
    (expt
      (sumatorio-norma x p)
      (float (/ 1 p)))))


;;; Funcion auxilar calcula el sumatorio
(defun sumatorio-norma (x p)
  (if (null x) 
    0 ;;; Comprobar si la lista esta vacia
    (+ 
      (expt (abs (first x) ) p)
      (sumatorio-norma (rest x) p) )))

;;; Casos de Prueba
;;; (setf milista '(6 2))
;;; (lp-rec milista 2)
;;; (lp-rec milista 0) ;;; Caso especial

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lp-mapcar (x p)
;;; Calcula la norma Lp de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; p: orden de la norma que se quiere calcular
;;;
;;; OUTPUT: norma Lp de x
;;;
(defun lp-mapcar (x p)
  (unless (< p 1) ;;; Comprobacion p >= 1
    (expt 
      (reduce '+ (mapcar #'(lambda (z) (expt (abs z) p)) x)) 
      (float (/ 1 p)))))

;;; Casos de Prueba
;;; (setf milista '(6 2))
;;; (lp-mapcar milista 2)
;;; (lp-mapcar milista 0) ;;; Caso especial


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;						EJERCICIO 1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l2-rec (x)
;;; Calcula la norma L2 de un vector de forma recursiva
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L2 de x
;;;
(defun l2-rec (x)
  (lp-rec x 2))

;;; Casos de Prueba
;;; (setf milista '(6 2))
;;; (l2-rec milista)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l2-mapcar (x)
;;; Calcula la norma L2 de un vector usando mapcar
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L2 de x
;;;
(defun l2-mapcar (x)
  (lp-mapcar x 2))

;;; Casos de Prueba
;;; (setf milista '(6 2))
;;; (l2-mapcar milista)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l1-rec (x)
;;; Calcula la norma L1 de un vector de forma recursiva
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L1 de x
;;;
(defun l1-rec (x)
  (lp-rec x 1))

;;; Casos de Prueba
;;; (setf milista '(6 2))
;;; (l1-rec milista)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l1-mapcar (x)
;;; Calcula la norma L1 de un vector usando mapcar
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L1 de x
;;;
(defun l1-mapcar (x)
  (lp-mapcar x 1))

;;; Casos de Prueba
;;; (setf milista '(6 2))
;;; (l1-mapcar milista)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nearest (lst-vectors vector fn-dist)
;;; Calcula de una lista de vectores el vector más cercano a uno dado,
;;; usando la función de distancia especificada
;;;
;;; INPUT: lst-vectors: lista de vectores para los que calcular la distancia
;;; vector: vector referencia, representado como una lista
;;; fn-dist: referencia a función para medir distancias
;;;
;;; OUTPUT: vector de entre los de lst-vectors más cercano al de referencia
;;;
(defun nearest (lst-vectors vector fn-dist)
  (if (rest lst-vectors)
    (let ((vector-actual (first lst-vectors))
          (vector-recursivo (nearest (rest lst-vectors) vector fn-dist)))
    (if (<
          (funcall fn-dist (mapcar #'- vector vector-actual))
          (funcall fn-dist (mapcar #'- vector vector-recursivo)))
      vector-actual ;Si secumple la condicion
      vector-recursivo)) ;Caso recursion
    (first lst-vectors))) 

;;; Casos de Prueba
;;; (setf vectors '((0.1 0.1 0.1) (0.2 -0.1 -0.1)))
;;; (nearest vectors '(1.0 -2.0 3.0) #'l2-mapcar)
;;; (nearest vectors '(1.0 -2.0 3.0) #'l1-rec)


;;; Tiempo
;;; (setf super-vectors '( (0.1 -0.1 0.1 4.5 6.0 7.0 8.7 9.1 1.2) (0.2 -0.1 0.1 0.1 4.5 6.0 7.0 8.7 2.2)))
;;; (time (nearest super-vectors '(0.2 -0.1 0.1 4.7 6.1 7.0 9.2 9.1 1.2) #'l2-mapcar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; secante (f tol-abs max-iter par-semillas)
;;; Estima el cero de una función mediante el método de la secante
;;;
;;; INPUT: f: función cuyo cero se desea encontrar
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; par-semillas: estimaciones iniciales del cero (x0 x1)
;;;
;;; OUTPUT: estimación del cero de f, o NIL si no converge
;;;
(defun secante (f tol-abs max-iter par-semillas)
  (let* ((xn (second par-semillas))     ;;;Macro para xn
         (xnm (first par-semillas))     ;;;Macro para xn-1
         (fxn (funcall f xn))           ;;;Macro para f(xn)
         (red-iter (- max-iter 1))
         
         (xnmas (- xn
                   (* 
                    (/ (- xn xnm)
                       (- fxn (funcall f xnm)))
                    fxn)
                   )))
    (if (tolerancia xnmas xn tol-abs)
        xnmas
        (unless (eq red-iter 0)
          (secante f tol-abs red-iter (list xn xnmas))))))

;;; Auxiliar Tolerancia
(defun tolerancia(a b tol-abs)
  (< 
    (abs (- a b)) 
    tol-abs))

;;; Casos de Prueba
;;; (setf tol 1e-6)
;;; (setf iters 50)
;;; (setf funcion (lambda (x) (+ (* x 3) (sin x) (- (exp x)))))
;;; (setf semillas1 '(0 1))
;;; (setf semillas2 '(3 5))
;;; (secante funcion tol iters semillas1)
;;; 0.3604217
;;; (secante funcion tol iters semillas2)
;;; 1.8900298


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; un-cero-secante (f tol-abs max-iter pares-semillas)
;;; Prueba con distintos pares de semillas iniciales hasta que
;;; la secante converge
;;;
;;; INPUT: f: función de la que se desea encontrar un cero
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; pares-semillas: pares de semillas con las que invocar a secante
;;;
;;; OUTPUT: el primer cero de f que se encuentre, o NIL si se diverge
;;; para todos los pares de semillas
;;;
(defun un-cero-secante (f tol-abs max-iter pares-semillas) 
  (unless (null pares-semillas)
    (let (( sec (secante f tol-abs max-iter (first pares-semillas)))) ;;;Macro para la secante
      (if (null sec)   
        (un-cero-secante f tol-abs max-iter (last pares-semillas))
        sec))))

;;; Casos de Prueba
;;; (un-cero-secante funcion tol iters (list semillas1 semillas2))
;;; 0.3604217
;;; (un-cero-secante funcion tol iters (list semillas2 semillas1))
;;; 1.8900298

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 2.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; todos-ceros-secante (f tol-abs max-iter pares-semillas)
;;; Prueba con distintas pares de semillas iniciales y devuelve
;;; las raíces encontradas por la secante para dichos pares
;;;
;;; INPUT: f: función de la que se desea encontrar un cero
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; pares-semillas: pares de semillas con las que invocar a secante
;;;
;;; OUTPUT: todas las raíces que se encuentren, o NIL si se diverge
;;; para todos los pares de semillas
;;;
(defun todos-ceros-secante (f tol-abs max-iter pares-semillas)
  (unless (null pares-semillas)
    (append 
      (list (secante f tol-abs max-iter (first pares-semillas))) 
      (todos-ceros-secante f tol-abs max-iter (rest pares-semillas)))))

;;; Casos de Prueba
;;; (todos-ceros-secante funcion tol iters (list semillas1 semillas2))
;;; (0.3604217 1.8900298)
;;; (todos-ceros-secante funcion tol iters (list semillas2 semillas1))
;;; (1.8900298 0.3604217)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 3.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst)
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elt: elemento a combinar
;;; lst: lista en la que combinar
;;;
;;; OUTPUT: lista resultado de combinar con elt
;;;
(defun combine-elt-lst (elt lst)
  (mapcar #'(lambda(z) (list elt z)) lst))

;;; Casos de Prueba
;;; (combine-elt-lst 'a nil)
;;; NIL
;;; (combine-elt-lst 'a '(1 2 3))
;;; ((A 1) (A 2) (A 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 3.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst (lst1 lst2)
;;; Producto cartesiano de dos listas
;;;
;;; INPUT: lst1: lista 1 para el producto
;;; lst2: lista 2 para el producto
;;;
;;; OUTPUT: lista resultado del prodcuto cartesiano
;;;
(defun combine-lst-lst (lst1 lst2)
  (unless (null lst1)
    (append 
      (combine-elt-lst (first lst1) lst2)
      (combine-lst-lst (rest lst1) lst2))))

;;; Casos de Prueba
;;; (combine-lst-lst nil nil)
;;; NIL
;;; (combine-lst-lst '(a b c) nil)
;;; NIL
;;; (combine-lst-lst NIL '(a b c))
;;; NIL
;;; (combine-lst-lst '(a b c) '(1 2))
;;; ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts (lstolsts)
;;; Calcula todas las posibles disposiciones de elementos pertenecientes a N
;;;
;;; INPUT: lstolsts lista de listas que se quieren comnbinar
;;;
;;; OUTPUT: todas las combinaciones posibles
;;;
(defun combine-list-of-lsts (lstolsts)
  (if (null (rest lstolsts))
    (mapcar #'list (first lstolsts)) 
    (mapcar #'(lambda (x) (cons (first x) (first (rest x)))) 
      (combine-lst-lst
        (first lstolsts)
        (combine-list-of-lsts (rest lstolsts))))))

;;; Casos de Prueba
;;; (combine-list-of-lsts '(() (+ -) (1 2 3 4)))
;;; NIL
;;; (combine-list-of-lsts '((a b c) () (1 2 3 4)))
;;; NIL
;;; (combine-list-of-lsts '((a b c) (1 2 3 4) ()))
;;; NIL
;;; (combine-list-of-lsts '((1 2 3 4)))
;;; ((1) (2) (3) (4))
;;; (combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;;; ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;;;  (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;;;  (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+ '=>)
(defconstant +and+ '^)
(defconstant +or+ 'v)
(defconstant +not+ '¬)

(defun truth-value-p (x)
  (or (eql x t)
      (eql x nil)))

(defun unary-connector-p (x) ;; toma 1 argumento
  (eql x +not+))

(defun binary-connector-p (x) ;; toma 2 argumentos
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x) ;; toma n argumentos
  (or (eql x +and+)
      (eql x +or+)))

(defun connector-p (x)
  (or (unary-connector-p x)
      (binary-connector-p x)
      (n-ary-connector-p x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proposicion-p (expr)
;;; Comprueba si una expresion esta en formato prefijo
;;;
;;; INPUT: expr Expresion a comporbar
;;;
;;; OUTPUT: todas las combinaciones posibles
;;;
(defun proposicion-p (expr)
  (unless (null expr) ;;; Comprobamos que la expresion no sea null
    (cond
      ((atom expr) ;;; Caso atomico 
          (not (connector-p expr)))
      ((binary-connector-p (first expr)) ;;; Operadores Binarios
          (and
            (not (null (second expr)))
            (not (null (third expr)))
            (null (fourth expr))))
      ((n-ary-connector-p (first expr)) ;;; Operadores N arios
        (unless
          (and (null (rest expr))       ;;; Al menos dos operandos
          (and (null rest(rest expr))))
            (comprueba-prop-aux (rest expr))))
      ((unary-connector-p (first expr)) ;;; Operadores unitarios
          (and
            (not (null (rest expr)))    ;;; Un operador, y despues algun conector
            (not (connector-p (first (rest expr))))
            (comprueba-prop-aux (rest expr))))
      (t nil)))) ;;; Caso por defecto

;;; Funcion auxiliar
(defun comprueba-prop-aux (expr)
  (if (null expr)
    t
    (if (atom (first expr))
      (unless (connector-p (first expr))
        (comprueba-prop-aux (rest expr)))
      (when (proposicion-p (first expr))
        (comprueba-prop-aux (rest expr))))))


;;; Casos de Prueba
;;; (proposicion-p 'A)  ;;; T
;;; (proposicion-p '(H <=> (¬ H))) ;;; NIL
;;; (proposicion-p '(<=> H (¬ H))) ;;; T


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; base-p (expr)
;;; Comprueba si una expresion es una base de conocimiento
;;;
;;; INPUT: expr Expresion a comporbar
;;;
;;; OUTPUT: T si es base de conocimiento, nil en caso contrario
;;;
(defun base-p (expr)
  (unless (not (listp expr))
    (if (and
          (atom expr) ;;; Los atomos son NIL siempre
          (not(connector-p expr))) ; que no sean un conector
      T
      (and (proposicion-p (first expr))
            (base-p (rest expr))))))

;;; Casos de Prueba
;;; (base-p 'A)
;;; NIL
;;; (base-p '(A))
;;; T
;;; (base-p '(<=> H (¬ H)))
;;; NIL
;;; (base-p '((<=> H (¬ H))))
;;; T
;;; (base-p '((H <=> (¬ H))))
;;; NIL
;;; (base-p '((<=> A (¬ H)) (<=> P (^ A H)) (<=> H P)))
;;; T


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extrae-simbolos (kb)
;;; Extrae sin repeticiones los simbolos de una base de conocimiento
;;;
;;; INPUT: kb Base de conocimiento de la que extraer simbolos
;;;
;;; OUTPUT: simbolos sin repeticiones
;;;
(defun extrae-simbolos (kb)
  (remove-duplicates 
    (remove-if #'connector-p
      (remove-if #'truth-value-p
        (elimina-parentesis kb)))))

;;; Funcion Auxiliar
(defun elimina-parentesis (expr)
  (unless (null expr)
    (if (atom (first expr))
      (cons
        (first expr)
        (elimina-parentesis (rest expr)))
      (append
        (elimina-parentesis (first expr))
        (elimina-parentesis (rest expr))))))

;;; Casos de Prueba
;;; (extrae-simbolos '(A))
;;; (A)
;;; (extrae-simbolos '((v (¬ A) A B (¬ B))))
;;; (A B)
;;; (extrae-simbolos '((=> A (¬ H)) (<=> P (^ A (¬ H))) (=> H P)))
;;; (A H P)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; genera-lista-interpretaciones (lst-simbolos)
;;; Elabora todas las posibles combinaciones valores de verdad
;;;
;;; INPUT: lst-simbolos sobre los que crear las interpretaciones
;;;         (Todas los casos de una tabla de verdad)
;;;
;;; OUTPUT: lista con todas las posibles interpretaciones
;;;
(defun genera-lista-interpretaciones (lst-simbolos)
  (unless (null lst-simbolos)   ;;; Caso de lista vacia
    (combine-list-of-lsts     ;;; combina las listas reslultantes del mapcar
     ;;; Para cada valor de la listagenera una lista con 2 sublistas
     (mapcar #'(lambda (x) (list (list x 'T ) (list x 'nil))) lst-simbolos))))

;;; Casos de Prueba
;;; (genera-lista-interpretaciones nil)
;;; NIL
;;; (genera-lista-interpretaciones '()) ;;;caso especial
;;; NIL
;;; (genera-lista-interpretaciones '(P)) 
;;; (((P T)) ((P NIL)))
;;; (genera-lista-interpretaciones '(P I L))
;;; (((P T) (I T) (L T)) ((P T) (I T) (L NIL)) ((P T) (I NIL) (L T))
;;; ((P T) (I NIL) (L NIL)) ((P NIL) (I T) (L T)) ((P NIL) (I T) (L NIL))
;;; ((P NIL) (I NIL) (L T)) ((P NIL) (I NIL) (L NIL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interpretacion-modelo-p (interp kb)
;;; Comprueba si una interpretacion es modelo de una base de conocimiento
;;;
;;; INPUT: interp interpretacion a comprobar
;;;        kb Base de Conocimiento sobre la que comprobar la interpretacion
;;;
;;; OUTPUT: T si es modelo, nil en caso contrario
;;;
(defun interpretacion-modelo-p (interp kb)
  (when (base-p kb) ;;Comprobar si la proposicion es correcta
      (eval
        (append 
          '(and)
          (mapcar #'(lambda (x) 
                      (if (listp x)
                        (cambia-valores interp (cambia-implicaciones-prop x))
                        (first (cambia-valores-verdad interp (list x)))))
          kb)))))

;;; Funcion auxiliar
(defun cambia-valores (interp x)
  (mapcar #'(lambda (y) 
      (cond 
        ((unary-connector-p y) 'not)          ;;; reemplazamos el operador ¬ por not
        ((eq +bicond+ y) 'eq)                 ;;; reemplazamos el operador <=> por eq
        ((eq +and+ y) 'and)                   ;;; reemplazamos el operador ^ por and
        ((eq +or+ y) 'or)                     ;;; reemplazamos el operador v por or
        ((listp y) (cambia-valores interp y)) ;;; Recursion
        (t y)))
    (cambia-valores-verdad interp x))) ;;; Actuamos sobre la lista ya cambiada segun la interpretacion
    
;;; Funcion auxiliar
(defun cambia-valores-verdad (interp kb)
  (if (null interp)
    kb
    (cambia-valores-verdad
      (rest interp)
      (substitute
        (second (first interp))
        (first(first interp))
        kb))))

;;; Funcion auxiliar para tratar las implicaciones
(defun cambia-implicaciones-prop (prop)
  (if (atom prop)
    prop
    (unless (null prop)
      (if (eq +cond+ (first prop) )
        (list 'or (list 'not 
                        (cambia-implicaciones-prop(second prop)))
                        (cambia-implicaciones-prop(third prop)))
        prop))))


;;; Casos de Prueba
;;; (interpretacion-modelo-p '((A nil) (P nil) (H t)) '((<=> A (¬ H)) (<=> P (^ A H)) (=> H P))) ;;; NIL
;;; (interpretacion-modelo-p '((A t) (P nil) (H nil)) '((<=> A (¬ H)) (<=> P (^ A H)) (=> H P))) ;;; T


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; encuentra-modelos (kb)
;;; Devuelve todas las interpretaciones que son modelo de la base de conocimiento
;;;
;;; INPUT: kb Base de Conocimiento de la que se quieren los modelos
;;;
;;; OUTPUT: Lista con las interpretaciones que son modelo
;;;
(defun encuentra-modelos (kb)
  (when (base-p kb)   
    (remove-if 
        #'(lambda(x)
            (not (interpretacion-modelo-p x kb)))
        (genera-lista-interpretaciones (extrae-simbolos kb)))))

;;; Casos de Prueba
;;; (encuentra-modelos '((=> A (¬ H)) (<=> P (^ A H)) (=> H P)))
;;; (((A T) (P NIL) (H NIL)) ((A NIL) (P NIL) (H NIL)))
;;; (encuentra-modelos '((=> (^ P I) L) (=> (¬ P) (¬ L)) (¬ P) L)) 
;;; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 4.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consecuencia-p (prop kb)
;;; Comprueba si una proposion es consecuencia de una base de conocimiento
;;;
;;; INPUT: kb Base de Conocimiento
;;;        prop Proposicon a comprobar si es consecuencia
;;;
;;; OUTPUT: T si es consecuencia, nil en caso contrario
;;;
(defun consecuencia-p (prop kb)
  (when
    (and
      (base-p kb)               ;;; kb es una base de conocimiento correcta
      (not (null prop)))        ;;; la proposion no es una lista vacia
    (probar-consecucias (encuentra-modelos kb) (list prop)))) ;;; Probamos si algun modelo de kb satisface prop

;;; Funcion Auxiliar
(defun probar-consecucias (modelos kprop)
  (if (null modelos) ;;; Comprobamos no haber terminado la lista
    T  ;;; Si ninguno ha fallado es que ha pasado todos los modelos
    (if 
      (not (interpretacion-modelo-p ;;; Si el modelo se satisface
        (first modelos)             ;;; podemos concluir que es consecuencia
        kprop))
    nil                             ;;; Si falla el modelo, no es consecuencia
    (probar-consecucias(rest modelos) kprop)))) ;;; Recursion para seguir probando el resto de modelos

;;; Casos de Prueba
;;; (consecuencia-p 'A '(A))
;;; T
;;; (consecuencia-p 'A '(¬ A))
;;; NIL
;;; (consecuencia-p '(¬ H) '((=> A (¬ H)) (<=> P (^ A H)) (=> H P)))
;;; T
;;;(consecuencia-p '(¬ P) '((=> A (¬ H)) (<=> P (^ A H)) (=> H P)))
;;; T
;;; (consecuencia-p '(^ (¬ H) (¬ P)) '((=> A (¬ H)) (<=> P (^ A H)) (=> H P)))
;;; T
;;; (consecuencia-p '(^ A (¬ H) (¬ P)) '((=> A (¬ H)) (<=> P (^ A H)) (=> H P)))
;;; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
  (if (null queue) nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end) (reverse path)
          (bfs end (append
                      (cdr queue)
                      (new-paths path node net))
                net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda(n)
        (cons n path))
              (cdr (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 5.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 5.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 5.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(shortest-path 'f 'c '((a c b d e) (b a d e f) (c a g) (d a g b h) (e a g b h) (f b h) (g c d e h) (h g d e f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   EJERCICIO 5.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bfs-improved (end queue net)
  (unless (null queue)  ;Caso base
    (let ((path (first queue))) ;;;Se vinculan los valores de path y de node
      (let ((node (first path)))
        (if (eql node end)    ;Caso base
          (reverse path)
          ;;;Recursion, se exploran los caminos con 
          ;;;origen en el nodo con el que estamos trabajando
          (bfs-improved end
            (append
              (rest queue)
              (new-paths-improved path node net))
            net))))))

;;; Funcion axuliar
(defun new-paths-improved (path node net)
  (unless (null (repetidos path))
    (mapcar #'(lambda(n)
                      (cons n path))
              (rest (assoc node net)))))

;;; Funcion axuliar
(defun repetidos (lst)
  (or (null lst)
      (and 
        (not (member (first lst)
        (rest lst)))
      (unique-p (rest lst)))))


(defun shortest-path-improved (end queue net)
  (bfs-improved end (list (list queue)) net))