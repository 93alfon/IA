; M39ZPTRPWD
; ElButanero

(defun mi-f-ev (estado)
  (- (+ (get-pts (estado-lado-sgte-jugador estado))
        (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
      (* (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4)4)
      (* (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5)5)
      (* (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 4)-2)
      (* (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 5)-4)
      (+ (get-pts (lado-contrario (estado-lado-sgte-jugador estado)))
         (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6))))
