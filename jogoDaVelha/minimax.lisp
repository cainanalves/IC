;Este algoritmo, estruturado em árvore, verifica todas as possibilidades a cada jogada. Em cada nó visitado ele armazena sua utilidade, para que no final obtenha 
;uma jogada mais inteligente.

;Utilidades:
;	1 se for chance de vitória
;	-1 se for chance de derrota
;	0 se for chance de empate
; Buscar mesmo valor com chaves diferentes:  (setf T (make-hash-table :test 'equal))

;algorithm search (u, s, p)
;	if final(s) then	 
;		u[s] <- result(s) 
;	for each empty place i do
;		s' <- copy(s)
;		search (u,s', not p)
;		u[s] <- u[s] + u[s']

;search(u,[]*9, 1)

(defvar victory '( (or (and (ai 0) (ai 1) (ai 2)) (and (ai 3) (ai 4) (ai 5)) (and (ai 6) (ai 7) (ai 8))
				  (and (ai 0) (ai 3) (ai 6)) (and (ai 1) (ai 4) (ai 7)) (and (ai 2) (ai 5) (ai 8))
				  (and (ai 0) (ai 4) (ai 8)) (and (ai 2) (ai 4) (ai 6)) )

				  (or (and (h 0) (h 1) (h 2)) (and (h 3) (h 4) (h 5)) (and (h 6) (h 7) (h 8))
				  (and (h 0) (h 3) (h 6)) (and (h 1) (h 4) (h 7)) (and (h 2) (h 5) (h 8))
				  (and (h 0) (h 4) (h 8)) (and (h 2) (h 4) (h 6))) ) )

(defvar draw '((and (not_e 0) (not_e 1) (not_e 2) (not_e 3) (not_e 4) (not_e 5) (not_e 6) (not_e 7) (not_e 8) )))

(defvar board '(_ _ _ _ _ _ _ _ _))
(defvar current_state)
(defvar player 'x)
(defvar utility (make-hash-table :test 'equal))

(defun ai (pos)
	(eq (nth pos current_state) 'o)
)

(defun h (pos)
	(eq (nth pos current_state) 'x)
)

(defun not_e (pos)
	(not (eq (nth pos current_state) '_))
)

(defun toggle_player(player)
	(if (equalp player 'x) 'o 'x)
)

(defun final_sheet(state)
	(let (test)
		(setf test (test_win state))
		(if (or (equalp test 'o) (equalp test 'x) (equalp test 'dr)) t nil) )
)


(defun test_win(state)
	(let ()
		(setf current_state state)
		(if (eval (nth 0 victory))
			'o
			(if (eval (nth 1 victory))
				'x
				(if (eval (nth 0 draw))
					'dr
					'n
				) 
			) 
		)
	)
	 
)
(defun save_utility(state score)
	(setf (gethash state utility) score)
)

(defun result(state)
	(let (p)
		(setf p (test_win state))
		(write p)
		(cond ((equalp p 'x) -1) ((equalp p 'o) 1) ((equalp p 'dr) 0) )
	)
)

(defun searchh (utility state player)
	(let (copy)
		(loop for i from 0 to 8 do
			(if (final_sheet state) 
				(save_utility state (result state))
				(if (eq (nth i state) '_)
					(progn  
						(setf copy state)
						(setf (nth i copy) player)
						(print copy) 
						(write (gethash state utility))
						(searchh utility copy (toggle_player player))	
						(setf (gethash state utility) (+ (gethash state utility) (gethash copy utility)))
						(setf (nth i copy) '_)
					)
				)
			)
		)
	)
	
)

(searchh utility board player)