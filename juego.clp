(deftemplate jugador 
    (slot tipo (type INTEGER)) ;1 si humano, 2 si IA
    (slot color (type STRING)(default "none"))
)

(deftemplate tablero
    (slot id (type INTEGER))
    (slot idPadre (type INTEGER))
    (slot turno (type INTEGER))
    (multislot jugadores (type INSTANCE) (allowed-classes jugador)(cardinality 2 2))
    (multislot fichasN (type INTEGER)(cardinality 24 24)) 
    (multislot fichasB (type INTEGER)(cardinality 24 24)) 
)

;REGLAS

(defrule inicializar
    (declare (salience 1))    

=>

    (printout t "¿Serás humano o IA? (1 para humano, 2 para IA):" ) 
    (bind ?tipo1 (read))
    (bind ?color1 "none")
    (printout t "¿Qué color quieres coger?: " )
    (bind ?color1 (read))
    (bind ?j1 (assert (jugador (tipo ?tipo1) (color ?color1))))

    

    (bind ?j2 ?j1)

    (while (eq ?j2 ?j1) do
        (printout t "¿Serás humano o IA? (1 para humano, 2 para IA):" )
        (bind ?tipo2 (read))
        (printout t "¿Qué color quieres coger?: " )
        (bind ?color2 (read))
        (bind ?j2 (assert (jugador (tipo ?tipo2) (color ?color2))))

    )
    

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
    (assert (tablero (id 1) (idPadre 0) (turno ?turno)
     (jugadores ?j1 ?j2) (fichasN 0 0 0 0 0 5 0 3 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 2) ; desde la posicion 1
                         (fichasB 2 0 0 0 0 0 0 0 0 0 0 5 0 0 0 0 3 0 5 0 0 0 0 0))) ; desde la posicion 1
)


    