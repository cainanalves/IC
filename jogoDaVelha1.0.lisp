
(defvar tabuleiro '((nil nil nil) (nil nil nil) (nil nil nil)))
(defvar vitoria (- 1))
(defvar regras '( (0 0 0 1 0 2) (0 0 0 2 0 1) (0 1 0 2 0 0) (1 0 1 1 1 2) (1 1 1 2 1 0) (1 0 1 2 1 1) (2 0 2 1 2 2) (2 1 2 2 2 0) (2 0 2 2 2 1) (0 0 1 0 2 0) 
	(0 0 2 0 1 0) (1 0 2 0 0 0) (0 1 1 1 2 1) (1 1 2 1 0 1) (0 1 2 1 1 1) (0 2 1 2 2 2) (1 2 2 2 0 2) (0 2 2 2 1 2) (0 0 1 1 2 2) (1 1 2 2 0 0) (0 0 2 2 1 1) 
	(0 2 1 1 2 0) (1 1 2 0 0 2) (2 0 0 2 1 1) ) )

;As jogadas são verificadas em ordem: CENTRO, CANTOS E MEIOS
(defvar melhores-jogadas '( (1 1) (0 0) (0 2) (2 0) (2 2) (0 1) (1 2) (2 1) (1 0)) )

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

(defun buscar-chance-vitoriaPC ()
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

(defun buscar-melhor-jogada ()
	(loop for i in melhores-jogadas do
		(if (equal (nth (nth 1 i) (nth (nth 0 i) tabuleiro)) nil)
			(progn 
				(setf (nth (nth 1 i) (nth (nth 0 i) tabuleiro)) 0)
				(return)
			)	
		)
	)
)

(defun verificar-jogada-inteligente ()
	(let ((achouRegra 0))
		(buscar-chance-vitoriaPC)
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
					(buscar-melhor-jogada)					
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
