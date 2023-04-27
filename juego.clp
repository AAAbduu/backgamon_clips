(deftemplate jugador 
    (multislot fichas (type INTEGER)(cardinality 24 24)(range 0 5)) 
)

(deftemplate partida 
    (slot turno (type INTEGER))
    (multislot jugadores (type INSTANCE) (allowed-classes jugador)(cardinality 2 2))
)

(deftemplate calculatorClass
    (slot partida (type INSTANCE) (allowed-classes partida))
)