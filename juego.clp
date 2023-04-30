(deftemplate jugador 
    (slot tipo (type INTEGER)) ;1 si humano, 2 si IA
    (slot color (type STRING)(default "none"))
)

(deftemplate movimiento
    (slot origen (type INTEGER))
    (slot destino (type INTEGER))
    (slot tipo (type INTEGER)) ;1 si normal, 2 si comer
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Cosas de tablero;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate tablero
    (slot id (type INTEGER))
    (slot idPadre (type INTEGER))
    (slot turno (type INTEGER))
    (multislot jugadores (type INSTANCE) (allowed-classes jugador)(cardinality 2 2))
    (multislot fichasN (type INTEGER)(cardinality 24 24)) 
    (multislot fichasB (type INTEGER)(cardinality 24 24)) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCIONES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction movimientosLegalesB (?dado1 ?dado2 $?fichas) ;COMPLETAR CUANDO TIENES FICHAS MUERTAS
    (bind ?movimientos (create$))

    (bind ?fichasN  (nth$ 1 $?fichas))
    (bind ?fichasB (nth$ 2 $?fichas))

    (loop-for-count (?i 24)
        ;if there is a fichaB in the i position
        (if (> (nth$ ?i ?fichasB) 0) then
            ;if the fichaB can move
            (bind ?nMovimiento  (create$ ?i (+ ?i ?dado1)) 1)) ;origen, destino, tipo

            (if (< (nth$ (+ ?i ?dado1) ?fichasN) 2) then;Si puedo mover la ficha i con el dado 1
                (if (eq (nth$ (+ ?i ?dado1) ?fichasN) 1) then ;Si hay una ficha negra en la posicion a la que me muevo
                    (bind ?nMovimiento (create$ ?i (+ ?i ?dado1)) 2)
                )
                
                (bind ?movimientos (create$ ?movimientos ?nMovimiento))
            )
            (if (< (nth$ (+ ?i ?dado2) ?fichasN) 2) then ;Si puedo mover la ficha i con el dado 2
                (bind ?nMovimiento (create$ ?i (+ ?i ?dado2)) 1)
                (if (eq (nth$ (+ ?i ?dado2) ?fichasN) 1) then ;Si hay una ficha negra en la posicion a la que me muevo
                    (bind ?nMovimiento (create$ ?i (+ ?i ?dado2)) 2)
                (bind ?movimientos (create$ ?movimientos ?nMovimiento))
            )
        )
    )

    (return ?movimientos)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;UTILIDADES


;REGLAS

(defrule inicializar
    (declare (salience 1))    

=>

    (printout t "¿Serás humano o IA? (1 para humano, 2 para IA):" ) 
    (bind ?tipo1 (read))
    (bind ?color1 "none")
    (printout t "¿Qué color quieres coger? (N o B): " )
    (bind ?color1 (read))
    (bind ?j1 (assert (jugador (tipo ?tipo1) (color ?color1))))

    

    (bind ?j2 ?j1)

    (printout t "¿Serás humano o IA? (1 para humano, 2 para IA):" )
    (bind ?tipo2 (read))
    (if (eq ?color1 "N") then
        (bind ?color2 "B")
    else
        (bind ?color2 "N")
    )
    (bind ?j2 (assert (jugador (tipo ?tipo2) (color ?color2))))

    
    

    (if (eq ?tipo1 1) then ;Si el jugador 1 y 2 son humanos o el jugador 1 es humano y el jugador 2 IA 
        (printout t "Jugador 1, ¿Cara o cruz? (1 o 2): " )
        (bind ?cCJ1 (read))

        (if (eq ?cCJ1 1) then
            (printout t "Jugador 1 ha escogido cara." crlf )
            (printout t "Jugador 2 asignado cruz." crlf )
            (bind ?cCJ2 2)

            
        else
            (bind ?cCJ1 2)
            (printout t "Jugador 1 ha escogido cruz." crlf )
            (printout t "Jugador 2 asignado cara." crlf )
            (bind ?cCJ2 1)
        )
    )

    (if (eq ?tipo1 2) then ;Si el jugador 1 es IA y el jugador 2 humano
        (printout t "Jugador 2, ¿Cara o cruz?: " )
        (bind ?cCJ2 (read))

        (if (eq ?cCJ2 1) then
            (printout t "Jugador 2 ha escogido cara." crlf )
            (printout t "Jugador 1 asignado cruz." crlf )
            (bind ?cCJ1 2)

            
        else
            (bind ?cCJ2 2)
            (printout t "Jugador 2 ha escogido cruz." crlf )
            (printout t "Jugador 1 asignado cara." crlf )
            (bind ?cCJ1 1)
        )
    )
    ;lanzamos una moneda
    (bind ?moneda (random 1 2))

    (if (eq ?moneda 1) then
        (printout t "Ha salido cara." crlf )
    else
        (printout t "Ha salido cruz." crlf )
    )

    (if (and (eq ?cCJ1 1) (eq ?moneda 1)) then
        (printout t "Jugador 1 empieza." crlf )
        (bind ?turno 1)

    else
        (if (and (eq ?cCJ1 2) (eq ?moneda 2)) then
            (printout t "Jugador 1 empieza." crlf )
            (bind ?turno 1)
        else
            (printout t "Jugador 2 empieza." crlf )
            (bind ?turno 2)
        )
    )

    ;crear tablero inicial
    (bind ?tablero (assert (tablero (id 1) (idPadre 0) (turno ?turno)
     (jugadores ?j1 ?j2) (fichasN 0 0 0 0 0 5 0 3 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 2) ; desde la posicion 1
                         (fichasB 2 0 0 0 0 0 0 0 0 0 0 5 0 0 0 0 3 0 5 0 0 0 0 0)))) ; desde la posicion 1

    (assert (imprimirTablero))

)

(defrule imprimirTablero
    (declare (salience 1))
    ?x <-(imprimirTablero)
    (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (fichasN $?fichasN) (fichasB $?fichasB))
=>

    (printout t crlf )
    (printout t crlf )
    (printout t crlf )


    (loop-for-count (?i 13 24)
        (printout t ?i " " )
    )
    (printout t "     Casa negras"crlf )

    (loop-for-count (?i 36)
        (printout t "-" )
    )

    (printout t crlf )

    (loop-for-count (?i 13 24)
        ;Print as many N as fichasN[i] has
        ;print as many B as fichasB[i] has

        (if (and (eq (nth$ ?i ?fichasN) 0) (eq (nth$ ?i ?fichasB) 0)) then
            (printout t "00" "|" )
        else
            (if (neq (nth$ ?i ?fichasN) 0) then
                (printout t (nth$ ?i ?fichasN)"N" "|" )
            )
            (if (neq (nth$ ?i ?fichasB) 0) then
                (printout t (nth$ ?i ?fichasB)"B" "|" )
            )
        )
        
        ;(printout t (nth$ ?i ?fichasN)"N" " " )

        ;(printout t (nth$ ?i ?fichasB)"B" " " )

    )

    (printout t crlf )
    (printout t crlf )
    (printout t crlf )

    (bind ?j 0)
    (loop-for-count (?i 12 23)

        (if (and (eq (nth$ (- ?i ?j) ?fichasN) 0) (eq (nth$ (- ?i ?j) ?fichasB) 0)) then
            (printout t "00" "|" )
        else
            (if (neq (nth$ (- ?i ?j) ?fichasN) 0) then
                (printout t (nth$ (- ?i ?j) ?fichasN)"N" "|" )
            )
            (if (neq (nth$ (- ?i ?j) ?fichasB) 0) then
                (printout t (nth$ (- ?i ?j) ?fichasB)"B" "|" )
            )

        )

        (bind ?j (+ ?j 2))
    )

    (printout t crlf )

    (loop-for-count (?i 36)
        (printout t "-" )
    )

    (printout t crlf )

    (bind ?j 0)
    (loop-for-count (?i 12 23)
        (if (< (- ?i ?j) 10) then
            (printout t "0" (- ?i ?j) " " )
        else
            (printout t (- ?i ?j) " " )
        )

        (bind ?j (+ ?j 2))

    )

    (printout t "     Casa blancas"crlf )
    (printout t crlf )
    (printout t crlf )

    (printout t "id: "?id crlf 
                "idPadre: "?idPadre crlf
                "turno: "?turno crlf
                "jugadores: "?jugadores crlf
                "fichasN: "?fichasN crlf
                "fichasB: "?fichasB crlf
    )

    (retract ?x)

    (if (eq ?turno 1) then
        (printout t "Turno de las fichas negras." crlf )
        (assert (turnoNegras))
    else
        (printout t "Turno de las fichas blancas." crlf )
        (assert (turnoBlancas))
    )

)


(defrule turnoNegras
    (declare (salience 1))
    ?x <-(turnoNegras)
    (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (fichasN $?fichasN) (fichasB $?fichasB))
=>
    (retract ?x) ;Quito el turno actual
    (bind ?dado1 (random 1 6))
    (bind ?dado2 (random 1 6))  ;tiro los dados

    (printout t "Dado 1: " ?dado1 crlf )
    (printout t "Dado 2: " ?dado2 crlf )

    (if (eq ?dado1 ?dado2) then

        (printout t "Dobles! " crlf )
        (assert (turnoNegras))

    )
    ;check if jugador negras is human
    (if (eq (nth$ 1 ?jugadores) 1) then ;CORREGIR
        (assert (moverFichaNegras ?dado1 ?dado2 ?fichasN ?fichasB)) ;mueve fichas negras humano
    else
        (assert (moverFichaNegrasPC ?dado1 ?dado2 ?fichasN ?fichasB))   ;mueve fichas negras IA
    )
)

(defrule turnoBlancas
    (declare (salience 1))
    ?x <-(turnoBlancas)
    (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (fichasN $?fichasN) (fichasB $?fichasB))
=>

    (retract ?x) ;Quito el turno actual
    (bind ?dado1 (random 1 6))
    (bind ?dado2 (random 1 6))  ;tiro los dados

    (printout t "Dado 1: " ?dado1 crlf )
    (printout t "Dado 2: " ?dado2 crlf )

    (if (eq ?dado1 ?dado2) then

        (printout t "Dobles! " crlf )
        (assert (turnoBlancas))

    )
    ;check if jugador blancas is human
    (if (eq (nth$ 2 ?jugadores) 1) then ;CORREGIR
        (assert (moverFichaBlancas ?dado1 ?dado2 (create$ ?fichasN ?fichasB))) ;mueve fichas blancas humano
    else
        (assert (moverFichaBlancasPC ?dado1 ?dado2 (create$ ?fichasN ?fichasB)))   ;mueve fichas blancas IA
    )
)

(defrule moverFichaNegras
    (declare (salience 1)) ; a lo mejor hay que cambiar la saliencia
    ;?x <-(moverFichaNegras ?dado1 ?dado2 ?fichasN ?fichasB)
    (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (fichasN $?fichasN) (fichasB $?fichasB))
=>

    ;(retract ?x) ;Quito el turno actual

    (printout t "Turno de las fichas negras." crlf )

    ;(bind ?fichasN (moverFichaN ?dado1 ?dado2 ?fichasN ?fichasB)) ;mueve fichas negras

    (assert (tablero (id ?id) (idPadre ?idPadre) (turno 2) (jugadores $?jugadores) (fichasN ?fichasN) (fichasB ?fichasB)))

)

(defrule moverFichaBlancas
    (declare (salience 1)) ; a lo mejor hay que cambiar la saliencia
    ?x <-(moverFichaBlancas ?dado1 ?dado2 ?fichas)
    (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (fichasN $?fichasN) (fichasB $?fichasB))
=>

    (bind ?movimientosDisponibles (movimientosLegalesB ?dado1 ?dado2 (create$ ?fichasN ?fichasB)))
    (printout t "Movimientos disponibles: " ?movimientosDisponibles crlf )
    (retract ?x) ;Quito el turno actual

)




