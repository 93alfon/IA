; M39ZPTRPWD
; Toulousin

(defun mi-f-ev (estado)
	(-	(get-fichas (estado-tablero estado)(estado-lado-sgte-jugador estado) 6)
  		(get-fichas (estado-tablero estado)(lado-contrario (estado-lado-sgte-jugador estado)) 6)))