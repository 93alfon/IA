;;; Alfonso Bonilla Trueba y David Garcia Fernandez

;;; ------------------------------------------------------------------------------------------
;;; Implementacion del algoritmo negamax con poda alfa-beta
;;; ------------------------------------------------------------------------------------------

(defun negamax-a-b (estado profundidad-max f-eval)
  (let* ((oldverb *verb*)
       (*verb* (if *debug-nmx* *verb* nil))
       (estado2 (negamax-alfa-beta estado 0 t profundidad-max f-eval +min-val+ +max-val+))
       (*verb* oldverb))
  estado2))


(defun negamax-alfa-beta (estado profundidad devolver-movimiento profundidad-max f-eval alfa-valor alfa-sucesor)
   (cond ((>= profundidad profundidad-max)
          (unless devolver-movimiento  (funcall f-eval estado)))
         (t
          (let ((sucesores (generar-sucesores estado profundidad))
                (mejor-valor +min-val+)
                (mejor-sucesor nil))
            (cond ((null sucesores)
                   (unless devolver-movimiento  (funcall f-eval estado)))
                  (t
                   (loop for sucesor in sucesores do
                     (let* ((result-sucesor (- (negamax-alfa-beta sucesor (1+ profundidad)
                                         nil profundidad-max f-eval (- alfa-valor) alfa-sucesor))))
                       ;(format t "~% Nmx-1 Prof:~A result-suc ~3A de suc ~A, mejor=~A" profundidad result-sucesor (estado-tablero sucesor) mejor-valor)
                       (when (>= (- alfa-valor) (- result-sucesor))
                         (return
                           (if devolver-movimiento alfa-sucesor alfa-valor)))
                       (when (> result-sucesor mejor-valor)
                         (setq mejor-valor result-sucesor)
                         (setq mejor-sucesor  sucesor)
                         (setq alfa-valor result-sucesor)
                         (setq alfa-sucesor  sucesor))))
                   (if  devolver-movimiento mejor-sucesor mejor-valor)))))))
