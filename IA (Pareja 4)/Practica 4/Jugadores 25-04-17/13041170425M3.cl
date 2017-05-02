; M39ZPTRPWD
; DavAlfPRO

(defun evaluacion (estado)
  (- (+ (get-pts (estado-lado-sgte-jugador estado))
        (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6))
      (* (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4)4)
      (* (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5)5)
      (* (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 4)-2)
      (* (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 5)-4)
      (+ (get-pts (lado-contrario (estado-lado-sgte-jugador estado)))
         (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6))))
		 	 
(defun suma (estado lista)
  (mapcar #'(lambda(x)
              (- (evaluacion estado)
                 x))lista))		 
		 		 
(defun mi-f-ev (estado)
  (if (juego-terminado-p estado)
      -50                              ;; Condicion especial de juego terminado
    ;; Devuelve el maximo del numero de fichas del lado enemigo menos el numero de propias
    (max-list (suma estado
                    (mapcar #'(lambda(x)
                                (evaluacion x))
                      (generar-sucesores estado))))))		 
		 
		 
		 
		 
		 
		 