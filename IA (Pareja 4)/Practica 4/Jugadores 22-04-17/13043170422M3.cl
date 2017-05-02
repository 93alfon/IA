; M39ZPTRPWD
; SexyBeast

(defun mi-f-ev (estado)
	(apply #'+ (heuristica-captura estado 5 '())))


(defun heuristica-captura (estado lim lista)
   (if (> lim 0)
  	(let ((fichas-jugador (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) lim))
   	     (fichas-oponente (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) lim)))	
		(cond
		  ((and
		     (> fichas-jugador 0)
		     (< fichas-oponente 1))
		   (heuristica-captura estado (- lim 1) (append lista (list (- (- fichas-jugador) 2))))
		  ((and
		     (< fichas-jugador 1)
		     (> fichas-oponente 0))
		  (heuristica-captura estado (- lim 1) (append lista (list (fichas-oponente)))))
		)))
  	 lista))

