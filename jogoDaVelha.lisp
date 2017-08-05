
(defvar tabuleiro '((nil nil nil) (nil nil nil) (nil nil nil)))
(defvar vitoria (- 1))
(defvar regras '( (0 0 0 1 0 2) 
					(0 0 0 2 0 1) 
					(0 1 0 2 0 0) 
					 (1 0 1 1 1 2) 
					 (1 1 1 2 1 0) 
					 (1 0 1 2 1 1) 
					 (2 0 2 1 2 2) 
					 (2 1 2 2 2 0) 
					 (2 0 2 2 2 1)
					 (0 0 1 0 2 0) 
					 (0 0 2 0 1 0) 
					 (1 0 2 0 0 0) 
					 (0 1 1 1 2 1) 
					 (1 1 2 1 0 1) 
					 (0 1 2 1 1 1) 
					 (0 2 1 2 2 2) 
					 (1 2 2 2 0 2) 
					 (0 2 2 2 1 2)
					 (0 0 1 1 2 2) 
					 (1 1 2 2 0 0) 
					 (0 0 2 2 1 1) 
					 (0 2 1 1 2 0) 
					 (1 1 2 0 0 2) 
					 (2 0 0 2 1 1) ) )

(defun imprimir-tabuleiro ()
	(loop for i in tabuleiro do 
		(loop for j in i do 
			(if (equal j nil)
				(write '_)
				(if (= j 1)
					(write 'x)
					(write 'o)
				)
				
			)
		)
		(terpri)
	)
) 

(defun busca-chance-vitoriaPC ()
	(loop for i in regras do 
		(if (and (and (equal (nth (nth 1 i) (nth (nth 0 i) tabuleiro)) 0) (equal (nth (nth 3 i) (nth (nth 2 i) tabuleiro)) 0)) (equal (nth (nth 5 i) (nth (nth 4 i) tabuleiro)) nil))
			(progn
				(setf vitoria 0)
				(setf (nth (nth 5 i) (nth (nth 4 i) tabuleiro)) 0)
				(return)
			)
		)
	)
)

(defun verificar-jogada-inteligente ()
	(let ((achouRegra 0))
		(busca-chance-vitoriaPC)

		(if (< vitoria 0)
			(progn
				(loop for i in regras do 
					(if (and (and (equal (nth (nth 1 i) (nth (nth 0 i) tabuleiro)) 1) (equal (nth (nth 3 i) (nth (nth 2 i) tabuleiro)) 1)) (equal (nth (nth 5 i) (nth (nth 4 i) tabuleiro)) nil))
						(progn
							(setf (nth (nth 5 i) (nth (nth 4 i) tabuleiro)) 0)
							(setf achouRegra 1)
							(return)
						)
					)
				)

				(if (= achouRegra 0)
					(let ((condicao 0) (linha (random 3 (make-random-state t))) (coluna (random 3 (make-random-state t))))
						(loop do
							(if (or (equal (nth coluna (nth linha tabuleiro)) 1) (equal (nth coluna (nth linha tabuleiro)) 0))
								(progn
									(setf linha (random 3 (make-random-state t)))
									(setf coluna (random 3 (make-random-state t)))
								)
								(return)
							)
						)
						(setf (nth coluna (nth linha tabuleiro)) 0)
					)
					
			)
			)
		)
		
	)
	
) 

(defun validar-jogada ()
	(let ((validador 0))
		(loop do
			(write-line "Jogada inválida!")
			(definir-jogada (read) (read))
			(return)
		)
	)
)

(defun definir-jogada (linha coluna)
	(let ()
		(if (and (and (< linha 4) (> linha 0)) (and (< coluna 4) (> coluna 0)))
			(if (equal (nth (- coluna 1) (nth (- linha 1) tabuleiro)) nil)
				(progn
					(setf (nth (- coluna 1) (nth (- linha 1) tabuleiro)) 1)
					;Busca chance de vitória
					(loop for i in regras do 
						(if (and (and (equal (nth (nth 1 i) (nth (nth 0 i) tabuleiro)) 1) (equal (nth (nth 3 i) (nth (nth 2 i) tabuleiro)) 1)) (equal (nth (nth 5 i) (nth (nth 4 i) tabuleiro)) 1))
							(progn
								(setf vitoria 1)
								(return)
							)
						)
					)
				)
				(validar-jogada)
			)
			(validar-jogada)
		)
	)	
)

(defun iniciar-jogo ()
	(loop for i from 0 to 8 do 
		(write-line "###############")
		(if (equal (evenp i) T)
			(progn
				(write-line "Sua vez.")
				(definir-jogada (read) (read))
			)
			(progn
				(write-line "Computador.")
				(verificar-jogada-inteligente)
			)
			
		)
		(terpri)
		(imprimir-tabuleiro)
		(terpri)
		(if (= vitoria 1)
			(progn
				(write-line "Parabéns! Você venceu!!")
				(return)
			)
			(if (= vitoria 0)
				(progn
					(write-line "Computador venceu! Tenha vergonha disso!!")
					(return)
				)
			)
		)
		
	)
)

(imprimir-tabuleiro)
(terpri)
(iniciar-jogo)
(terpri)
