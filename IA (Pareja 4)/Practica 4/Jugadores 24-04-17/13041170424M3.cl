; M39ZPTRPWD
; Davalf

(defun mi-f-ev (estado)
  (if (juego-terminado-p estado)
      (if (> (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0) 18)
          10
        -10)

        (* 10 (- (get-fichas (estado-tablero estado)(estado-lado-sgte-jugador estado) *long-fila*)
                 (get-fichas (estado-tablero estado)(lado-contrario (estado-lado-sgte-jugador estado)) *long-fila*)))))
