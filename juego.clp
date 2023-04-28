(deftemplate jugador 
    (slot tipo (type INTEGER)) ;1 si humano, 2 si IA
    (slot color (type STRING)(default "none"))
)

(deftemplate tablero
    (slot id (type INTEGER))
    (slot idPadre (type INTEGER))
    (slot turno (type INTEGER))
    (multislot jugadores (type INSTANCE) (allowed-classes jugador)(cardinality 2 2))
    (multislot fichasN (type INTEGER)(cardinality 24 24)(range -5 0)) 
    (multislot fichasB (type INTEGER)(cardinality 24 24)(range 0 5)) 
)

;REGLAS

(defrule inicializar
    (declare (salience 1))    

=>

    (printout t "¿Serás humano o IA? (1 para humano, 2 para IA):" ) 
    (bind ?tipo1 (read))
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
)
    