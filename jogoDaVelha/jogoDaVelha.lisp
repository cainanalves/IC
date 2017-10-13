(load "base_dados.lisp")

(defun busca-vit ()
	(let ((vitoriaPC '(or (and (ia 0 0) (ia 0 1) (ia 0 2)) (and (ia 1 0) (ia 1 1) (ia 1 2)) (and (ia 2 0) (ia 2 1) (ia 2 2))
						(and (ia 0 0) (ia 1 0) (ia 2 0)) (and (ia 0 1) (ia 1 1) (ia 2 1)) (and (ia 0 2) (ia 1 2) (ia 2 2))
						(and (ia 0 0) (ia 1 1) (ia 2 2)) (and (ia 0 2) (ia 1 1) (ia 2 0)) 	) ) 
		  (vitoriaHUM '(or (and (h 0 0) (h 0 1) (h 0 2)) (and (h 1 0) (h 1 1) (h 1 2)) (and (h 2 0) (h 2 1) (h 2 2))
						(and (h 0 0) (h 1 0) (h 2 0)) (and (h 0 1) (h 1 1) (h 2 1)) (and (h 0 2) (h 1 2) (h 2 2))
						(and (h 0 0) (h 1 1) (h 2 2)) (and (h 0 2) (h 1 1) (h 2 0)) 	) )
		  (vitoriaBOT '(or (and (bt 0 0) (bt 0 1) (bt 0 2)) (and (bt 1 0) (bt 1 1) (bt 1 2)) (and (bt 2 0) (bt 2 1) (bt 2 2))
						(and (bt 0 0) (bt 1 0) (bt 2 0)) (and (bt 0 1) (bt 1 1) (bt 2 1)) (and (bt 0 2) (bt 1 2) (bt 2 2))
						(and (bt 0 0) (bt 1 1) (bt 2 2)) (and (bt 0 2) (bt 1 1) (bt 2 0)) 	) ) )
		(cond ((eval vitoriaHUM) (write-line "Parabéns, HUMANO! Você é um Deus!!")) ((eval vitoriaPC) (write-line "Que pena! Acho que sou melhor!!"))
			((eval vitoriaBOT) (write-line "Algo errado! O BOT venceu!!")))
	)
)

(defun imprimir-tabuleiro ()
	(loop for i in tabuleiro do
		(loop for j in i do
			(if (equalp j nil)
				(write '[___])
				(case j 
					(1 (write '[_x_]))
					(2 (write '[_o_]))
					(3 (write '[_b_]))
				)
			)
			
		)
		(terpri)
	)
)

(defun jogar-ia-regra (lin col)
	(let ()
		(terpri)
		(write-line "Vez do Computador!")
		(setf (nth col (nth lin tabuleiro)) pc)
	)
	
)
(defun jogar-bot-regra (lin col)
	(let ()
		(terpri)
		(write-line "Vez do Bot!")
		(setf (nth col (nth lin tabuleiro)) bot)
	)
	
)

(defun jogar-aleatorio (jog)
	(let (lin col)
		(loop do
			(setf lin (random 3 (make-random-state t)))
			(setf col (random 3 (make-random-state t)))
			(if (equalp (nth col (nth lin tabuleiro)) nil)
				(progn
					(if (eq jog pc) 
						(jogar-ia-regra lin col) 
						(if (eq jog bot) 
							(jogar-bot-regra lin col)))
					(return)
				)
			)
		)
	)
)

(defun validar-jogada (lin col)
	(let ()
		(if (and (>= lin 0) (<= col 2))
			(if (equalp (nth col (nth lin tabuleiro)) nil)
				(setf (nth col (nth lin tabuleiro)) hum)
				(validar-jogada (read) (read))
			)
			(validar-jogada (read) (read))
		)
		
	)
)

(defun jogar-ia ()
	(loop for i in regras do
		(if (eval(nth 0 i))
			(let ((lin (nth 1 i)) (col (nth 2 i)) )
				(jogar-ia-regra lin col)
				(return-from jogar-ia)
			)
		)
	)
	(jogar-aleatorio pc)

) 

(defun jogar-hum ()
	(let ()
		(terpri)
		(write-line "Sua vez!")
		(validar-jogada (read) (read))
	)
	
)

(defun jogar-jog1 (jog1)
	(case jog1 
		(1 (jogar-hum))
		(2 (jogar-aleatorio bot))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun iniciar-jogo ()
	(let (jog1) 
		(format t "Escolha o primeiro jogador
                        1 - Humano
                        2 - Bot (Aleatório)")
		(terpri)
		(setq jog1 (read))
		(imprimir-tabuleiro)
		(loop for i from 0 to 8 do
			(if (equal (evenp i) T) 
				(jogar-jog1 jog1)
				(jogar-ia)
			)
			(terpri)
			(imprimir-tabuleiro)
			(when (busca-vit) (return-from iniciar-jogo))
			
		)
		(terpri)
		(write-line "Empate!!")
	)
	
)

(iniciar-jogo)

