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
