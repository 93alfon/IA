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