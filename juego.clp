(deftemplate jugador 
    (slot tipo (type INTEGER)) ;1 si humano, 2 si IA
    (slot color (type STRING)(default "none"))
)

(deftemplate movimiento
    (slot origen (type INTEGER))
    (slot destino (type INTEGER))
    (slot tipo (type INTEGER)) ;1 si normal, 2 si comer , 3 si casa
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Cosas de tablero;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate tablero
    (slot id (type INTEGER))
    (slot idPadre (type INTEGER))
    (slot turno (type INTEGER))
    (multislot jugadores (type INSTANCE) (allowed-classes jugador)(cardinality 2 2))
    (multislot juego (type INTEGER)(cardinality 24 24)) ;tendra el tablero, 24 posiciones, enteros negativo para las fichas negras y positivo para las blancas
    (slot fichasCapturadasB (type INTEGER)(default 0))
    (slot fichasCapturadasN (type INTEGER)(default 0)) 
    (slot casasB (type INTEGER)(default 0))
    (slot casasN (type INTEGER)(default 0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCIONES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction movimientosLegalesB (?dado1 ?dado2 ?fichasCapturadasB ?todasEnCasaB $?fichas) ;COMPLETAR CUANDO TIENES FICHAS CAPTURADAS
    (bind ?movimientos (create$))
    (bind ?nMovimiento (create$))

    ;;;;;;;;;;;;;;;;;;;;;;;;; MOVIMIENTOS FICHAS CAPTURADAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if(> ?fichasCapturadasB 0) then ;hay fichas blancas capturadas
        (if (and (neq ?dado1 0)(> (nth$ ?dado1 ?fichas) -2)) then ; si puedo mover la ficha capturada con el dado 1
            (bind ?nMovimiento  (create$ 0 ?dado1) 1) ;origen, destino, tipo
            (if (eq (nth$ ?dado1 ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                (bind ?nMovimiento (create$ 0 ?dado1) 2) ;origen, destino, tipo: como pieza
            )
                    
            (bind ?movimientos (create$ ?movimientos ?nMovimiento))
        )
        (if (and (neq ?dado2 0)(> (nth$ ?dado2 ?fichas) -2)) then ; si puedo mover la ficha capturada con el dado 2
            (bind ?nMovimiento  (create$ 0 ?dado2) 1) ;origen, destino, tipo
            (if (eq (nth$ ?dado2 ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                (bind ?nMovimiento (create$ 0 ?dado2) 2) ;origen, destino, tipo: como pieza
            )
                    
            (bind ?movimientos (create$ ?movimientos ?nMovimiento))
        )
        (bind ?sumaDados (+ ?dado1 ?dado2))
        (if (and (neq ?dado1 0) (neq ?dado2 0)) then
            (if (> (nth$ ?sumaDados ?fichas) -2) then ; si puedo mover la ficha capturada con la suma de los dados
                (bind ?nMovimiento  (create$ 0 ?sumaDados) 1) ;origen, destino, tipo
                (if (eq (nth$ ?sumaDados ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                    (bind ?nMovimiento (create$ 0 ?sumaDados) 2) ;origen, destino, tipo: como pieza
                )
                    
                (bind ?movimientos (create$ ?movimientos ?nMovimiento))
            )
        )
        (return ?movimientos)
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;; MOVIMIENTOS FICHAS EN EL TABLERO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (or (eq ?fichasCapturadasB 0)(eq (length$ ?movimientos) 0)) then ;O bien no hay fichas capturadas o las capturadas no se pueden mover
        (loop-for-count (?i 24)
            ;if there is a fichaB in the i position
            (if (> (nth$ ?i ?fichas) 0) then
                ;if the fichaB can move

                (if (and(and(< (+ ?i ?dado1) 25)(> (nth$ (+ ?i ?dado1) ?fichas) -2))(neq ?dado1 0)) then;Si puedo mover la ficha i con el dado 1
                    (bind ?nMovimiento  (create$ ?i (+ ?i ?dado1)) 1) ;origen, destino, tipo
                    (if (eq (nth$ (+ ?i ?dado1) ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                        (bind ?nMovimiento (create$ ?i (+ ?i ?dado1)) 2) ;origen, destino, tipo: como pieza
                    )
                    
                    (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                else
                    (if (and (eq (+ ?i ?dado1) 25)(eq ?todasEnCasaB 1)) then ;Si me muevo a la casa
                        (bind ?nMovimiento (create$ ?i 25) 3) ;origen, destino, tipo: como casa
                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    )
                )
                (if (and(and(and (< (+ ?i ?dado2) 25) (> (nth$ (+ ?i ?dado2) ?fichas) -2))(neq ?dado1 ?dado2))(neq ?dado2 0)) then ;Si puedo mover la ficha i con el dado 2
                    (bind ?nMovimiento (create$ ?i (+ ?i ?dado2)) 1)
                    (if (eq (nth$ (+ ?i ?dado2) ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                        (bind ?nMovimiento (create$ ?i (+ ?i ?dado2)) 2)
                    )

                    (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                else
                    (if (and(eq (+ ?i ?dado2) 25)(eq ?todasEnCasaB 1)) then ;Si me muevo a la casa
                        (bind ?nMovimiento (create$ ?i 25) 3) ;origen, destino, tipo: como casa
                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    )        
                )
                (bind ?sumaDadosi (+(+ ?i ?dado2)?dado1))
                (if (and (neq ?dado1 0) (neq ?dado2 0)) then
                    (if (and (< ?sumaDadosi 25) (> (nth$ ?sumaDadosi ?fichas) -2)) then ;Si puedo mover la ficha i con la suma de los dados
                        (bind ?nMovimiento (create$ ?i ?sumaDadosi) 1)
                        (if (eq (nth$ ?sumaDadosi ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                            (bind ?nMovimiento (create$ ?i ?sumaDadosi) 2)
                        )

                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    else
                        (if (and(eq ?sumaDadosi 25)(eq ?todasEnCasaB 1)) then ;Si me muevo a la casa
                            (bind ?nMovimiento (create$ ?i 25) 3) ;origen, destino, tipo: como casa
                            (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                        )        
                    )       
                )
            )
        )
    )    
    (return ?movimientos)
)

(deffunction movimientosLegalesN (?dado1 ?dado2 ?fichasCapturadasN ?todasEnCasaN $?fichas) ;COMPLETAR CUANDO TIENES FICHAS CAPTURADAS
    (bind ?movimientos (create$))
    (bind ?nMovimiento (create$))

    ;;;;;;;;;;;;;;;;;;;;;;;;;MOVIMIENTOS FICHAS CAPTURADAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (> ?fichasCapturadasN 0) then ;hay fichas negras capturadas
        (if (and (neq ?dado1 0)(< (nth$ (- 25 ?dado1) ?fichas) 2)) then ; si puedo mover la ficha capturada con el dado 1
            (bind ?nMovimiento  (create$ 25 (- 25 ?dado1)) 1) ;origen, destino, tipo
            (if (eq (nth$ (- 25 ?dado1) ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                (bind ?nMovimiento (create$ 25 (- 25 ?dado1)) 2) ;origen, destino, tipo: como pieza
            )
                    
            (bind ?movimientos (create$ ?movimientos ?nMovimiento))
        )
        (if (and(neq ?dado2 0)(and(< (nth$ (- 25 ?dado2) ?fichas) 2)(neq ?dado1 ?dado2))) then
            (bind ?nMovimiento  (create$ 25 (- 25 ?dado2)) 1) ;origen, destino, tipo
            (if (eq (nth$ (- 25 ?dado2) ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                (bind ?nMovimiento (create$ 25 (- 25 ?dado2)) 2) ;origen, destino, tipo: como pieza
            )
                    
            (bind ?movimientos (create$ ?movimientos ?nMovimiento))            
        )
        (bind ?sumaDados (- (- 25 ?dado2) ?dado1))
        (if (and (neq ?dado1 0) (neq ?dado2 0)) then
            (if (< (nth$ ?sumaDados ?fichas) 2) then
                    (bind ?nMovimiento (create$ 25 ?sumaDados) 1)
                    (if (eq (nth$ ?sumaDados ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                        (bind ?nMovimiento (create$ 25 ?sumaDados) 2)
                    )
                    (bind ?movimientos (create$ ?movimientos ?nMovimiento))            
            )
        )
        (return ?movimientos)
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;; MOVIMIENTOS FICHAS EN EL TABLERO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (or (eq ?fichasCapturadasN 0)(eq (length$ ?movimientos) 0)) then ;O bien no hay fichas capturadas o las capturadas no se pueden mover
        (bind ?i 24)
        (while (neq ?i 0)
        ;if there is a fichaN in the i position
            (if (< (nth$ ?i ?fichas) 0) then
                ;if the fichaN can move

                (if (and(and(> (- ?i ?dado1) 0)(< (nth$ (- ?i ?dado1) ?fichas) 2))(neq ?dado1 0)) then;Si puedo mover la ficha i con el dado 1
                    (bind ?nMovimiento  (create$ ?i (- ?i ?dado1)) 1) ;origen, destino, tipo
                    (if (eq (nth$ (- ?i ?dado1) ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                        (bind ?nMovimiento (create$ ?i (- ?i ?dado1)) 2) ;origen, destino, tipo: como pieza
                    )
                    
                    (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                else
                    (if (and (eq (- ?i ?dado1) 0)(eq ?todasEnCasaN 1)) then ;Si me muevo a la casa
                        (bind ?nMovimiento (create$ ?i 0) 3) ;origen, destino, tipo: como casa
                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    )
                )
                (if (and(and (and (> (- ?i ?dado2) 0) (< (nth$ (- ?i ?dado2) ?fichas) 2))(neq ?dado1 ?dado2))(neq ?dado2 0)) then ;Si puedo mover la ficha i con el dado 2
                    (bind ?nMovimiento (create$ ?i (- ?i ?dado2)) 1)
                    (if (eq (nth$ (- ?i ?dado2) ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                        (bind ?nMovimiento (create$ ?i (- ?i ?dado2)) 2)
                    )

                    (bind ?movimientos (create$ ?movimientos ?nMovimiento))

                else
                    (if (and (eq (- ?i ?dado2) 0)(eq ?todasEnCasaN 1)) then ;Si me muevo a la casa
                        (bind ?nMovimiento (create$ ?i 0) 3) ;origen, destino, tipo: como casa
                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    )
                )
                (bind ?sumaDadosi (-(- ?i ?dado2)?dado1))
                (if (and (neq ?dado1 0) (neq ?dado2 0)) then
                    (if (and (> ?sumaDadosi 0) (< (nth$ ?sumaDadosi ?fichas) 2)) then ;Si puedo mover la ficha i con la suma de los dados
                        (bind ?nMovimiento (create$ ?i ?sumaDadosi) 1)
                        (if (eq (nth$ ?sumaDadosi ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                            (bind ?nMovimiento (create$ ?i ?sumaDadosi) 2)
                        )
                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    else
                        (if (and (eq ?sumaDadosi 0)(eq ?todasEnCasaN 1)) then ;Si me muevo a la casa
                            (bind ?nMovimiento (create$ ?i 0) 3) ;origen, destino, tipo: como casa
                            (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                        )
                    )
                )
            )
            (bind ?i (- ?i 1))
        )
    )    
    (return ?movimientos)
)

(deffunction comprobarCasaB (?casasB ?fichas) ;Comprueba si las fichas blancas estan en casa
    (bind ?contador 0)
    (loop-for-count (?i 19 24)
        (bind ?contador (+ (nth$ ?i ?fichas) ?contador))
    )
    (if (eq (- 15 ?casasB) ?contador) then ;Si todas las fichas estan en casa
        (return 1) 
    )
    (return 0)
)

(deffunction comprobarCasaN (?casasN ?fichas) ;Comprueba si las fichas negras estan en casa
    (bind ?contador 0)
    (bind ?i 6)
    (while (neq ?i 0)
        (bind ?contador (+ (abs(nth$ ?i ?fichas)) ?contador))
        (bind ?i (- ?i 1))
    )
    (if (eq (- 15 ?casasN) ?contador) then ;Si todas las fichas estan en casa
        (return 1) 
    )
    (return 0)
)

(deffunction imprimirMovimientos (?cantMov ?movimientosDisponibles)
    (if (neq ?cantMov 0) then
        (bind ?i 1)
        (bind ?j 1)
        (while (<= ?i ?cantMov) do
            (printout t "Movimiento " ?j ": " )
            (printout t "Origen: " (nth$ ?i ?movimientosDisponibles) " " )
            (printout t "Destino: " (nth$ (+ ?i 1) ?movimientosDisponibles))
            (if (eq (nth$ (+ ?i 2) ?movimientosDisponibles) 2) then
                (printout t " (puedes comer ficha)")
            )
            (if (eq (nth$ (+ ?i 2) ?movimientosDisponibles) 3) then
                (printout t " (puedes meter ficha en casa)")
            )
            (printout t crlf )
            

            (bind ?j (+ ?j 1))
            (bind ?i (+ ?i 3))
        )
    )
)

(deffunction asignarMovFichasN (?color ?tipo ?tipo2 ?dado1 ?dado2 ?fichas)
    (if (eq ?color N)
    then
        (if (eq ?tipo 1) then
            (assert (moverFichaNegras ?dado1 ?dado2 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaNegrasPC ?dado1 ?dado2 ?fichas))   ;mueve fichas blancas IA
        )
    else
        (if (eq ?tipo2 1) then
            (assert (moverFichaNegras ?dado1 ?dado2 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaNegrasPC ?dado1 ?dado2 ?fichas))   ;mueve fichas blancas IA
        )
    )
)

(deffunction asignarMovFichasB (?color ?tipo ?tipo2 ?dado1 ?dado2 ?fichas)
    (if (eq ?color B)
    then
        (if (eq ?tipo 1) then
            (assert (moverFichaBlancas ?dado1 ?dado2 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaBlancasPC ?dado1 ?dado2 ?fichas))   ;mueve fichas blancas IA
        )
    else
        (if (eq ?tipo2 1) then
            (assert (moverFichaBlancas ?dado1 ?dado2 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaBlancasPC ?dado1 ?dado2 ?fichas))   ;mueve fichas blancas IA
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UTILIDADES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;REGLAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule inicializar
    (declare (salience 1))    

=>

    (printout t "¿Serás humano o IA? (1 para humano, 2 para IA):" ) 
    (bind ?tipo1 (read))
    (bind ?color1 "none")
    (printout t "¿Qué color quieres coger? (N o B): " )
    (bind ?color1 (read))
    (bind ?color1 (sym-cat ?color1))
    (bind ?j1 (assert (jugador (tipo ?tipo1) (color ?color1))))

    

    (bind ?j2 ?j1)

    (printout t "¿Serás humano o IA? (1 para humano, 2 para IA):" )
    (bind ?tipo2 (read))
    (if (eq ?color1 N) then
        (bind ?color2 B)
    else
        (bind ?color2 N)
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
        (bind ?turno ?color1)

    else
        (if (and (eq ?cCJ1 2) (eq ?moneda 2)) then
            (printout t "Jugador 1 empieza." crlf )
            (bind ?turno ?color1)
        else
            (printout t "Jugador 2 empieza." crlf )
            (bind ?turno ?color2)
        )
    )

    ;crear tablero inicial
    (bind ?tablero (assert (tablero (id 1) (idPadre 0) (turno ?turno)
     (jugadores ?j1 ?j2) (juego 2 0 0 0 0 -5 0 -3 0 0 0 5 -5 0 0 0 3 0 5 0 0 0 0 -2) ;table inicial bueno (juego 2 0 0 0 0 -5 0 -3 0 0 0 5 -5 0 0 0 3 0 5 0 0 0 0 -2)
     (fichasCapturadasB 0) (fichasCapturadasN 0) (casasB 0) (casasN 0)))) ; desde la posicion 1
    (assert (imprimirTablero))
    (if (eq ?turno N) then
        (assert (turnoNegras 0 0))
    else
        (assert (turnoBlancas 0 0))
    )

)

(defrule imprimirTablero
    (declare (salience 3))
    ?x <-(imprimirTablero)
    (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB)
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
=>

    (printout t crlf )
    (printout t crlf )
    (printout t crlf )


    (loop-for-count (?i 13 24)
        (printout t ?i " " )
    )
    (printout t "     Casa blancas"crlf )

    (loop-for-count (?i 36)
        (printout t "-" )
    )

    (printout t crlf )

    (loop-for-count (?i 13 24)
        ;Print as many N as fichasN[i] has
        ;print as many B as [i] has

        (if (eq (nth$ ?i ?fichas) 0) then
            (printout t "00" "|" )
        else
            (if (< (nth$ ?i ?fichas) 0) then
                (printout t (* (nth$ ?i ?fichas)-1)"N" "|" )
            )
            (if (> (nth$ ?i ?fichas) 0) then
                (printout t (nth$ ?i ?fichas)"B" "|" )
            )
        )

    )

    (printout t crlf )
    (printout t crlf )
    (printout t crlf )

    (loop-for-count (?i 1 12)

        (if (eq (nth$ (- 13 ?i) ?fichas) 0) then
            (printout t "00" "|" )
        else
            (if (< (nth$ (- 13 ?i) ?fichas) 0) then
                (printout t (*(nth$ (- 13 ?i) ?fichas)-1)"N" "|" )
            )
            (if (> (nth$ (- 13 ?i) ?fichas) 0) then
                (printout t (nth$ (- 13 ?i) ?fichas)"B" "|" )
            )

        )
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

    (printout t "     Casa negras"crlf )
    (printout t crlf )
    (printout t crlf )



    (retract ?x)

)

(defrule dobles
    (declare (salience 3))
    ?x <-(dobles ?turno ?dado1 ?dado2)
    (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
    ?j1 <- (jugador (tipo ?tipo) (color ?color))
    ?j2<-(jugador (tipo ?tipo2) (color ?color2))

    (test (neq ?j1 ?j2))



=>
    (if (and(eq ?dado1 ?dado2)(eq ?turno B)) then
        (asignarMovFichasB B ?tipo ?tipo2 ?dado1 ?dado2 ?fichas)
    else
        (if (and(eq ?dado1 ?dado2)(eq ?turno N)) then
        (asignarMovFichasN N ?tipo ?tipo2 ?dado1 ?dado2 ?fichas)
        )
    )

    (retract ?x)
)

(defrule turnoNegras
    (declare (salience 1))
    ?x <-(turnoNegras ?dado1 ?dado2)
    ?t <- (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB)
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
    (test (eq ?turno N))
    ;get player with fichas "N"
    ?j1 <- (jugador (tipo ?tipo) (color ?color))
    ?j2 <- (jugador (tipo ?tipo2) (color ?color2))

    (test (neq ?j1 ?j2))

=>
    (retract ?x)
    (if (and(eq ?dado1 0)(eq ?dado2 0)) then
        (bind ?dado1 (random 1 6))
        (bind ?dado2 (random 1 6))  ;tiro los dados

        (printout t "Dado 1: " ?dado1 crlf )
        (printout t "Dado 2: " ?dado2 crlf )

        (if (eq ?dado1 ?dado2) then

            (printout t "Dobles! " crlf )
            (assert (dobles N ?dado1 ?dado2))

        )   
    )

    (asignarMovFichasN ?color ?tipo ?tipo2 ?dado1 ?dado2 ?fichas)

    (bind ?turnoN B)    
    (retract ?t)
    (retract ?x)
    (assert(tablero (id ?id) (idPadre ?idPadre) (turno ?turnoN) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN)))
)

(defrule turnoBlancas
    (declare (salience 1))
    ?x <-(turnoBlancas ?dado1 ?dado2)
    ?t <-(tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB)
     (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
    (test (eq ?turno B))
    ?j1 <- (jugador (tipo ?tipo) (color ?color))
    ?j2 <- (jugador (tipo ?tipo2) (color ?color2))
    (test (neq ?j1 ?j2))


=>
    (retract ?x) ;quito el turno ya
    (if (and(eq ?dado1 0)(eq ?dado2 0)) then
        (bind ?dado1 (random 1 6))
        (bind ?dado2 (random 1 6))  ;tiro los dados

        (printout t "Dado 1: " ?dado1 crlf )
        (printout t "Dado 2: " ?dado2 crlf )

        (if (eq ?dado1 ?dado2) then
            (printout t "Dobles! " crlf )
            (assert (dobles B ?dado1 ?dado2))
    )
    )

    ;check if jugador blancas is human

    (if (eq ?color B)
    then
        (if (eq ?tipo 1) then
            (assert (moverFichaBlancas ?dado1 ?dado2 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaBlancasPC ?dado1 ?dado2 ?fichas))   ;mueve fichas blancas IA
        )
    else
        (if (eq ?tipo2 1) then
            (assert (moverFichaBlancas ?dado1 ?dado2 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaBlancasPC ?dado1 ?dado2 ?fichas))   ;mueve fichas blancas IA
        )
    )

    (bind ?turnoN N)
    (retract ?t)
    (retract ?x)
    (assert(tablero (id ?id) (idPadre ?idPadre) (turno ?turnoN) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN)))
    (assert (turnoNegras 0 0))
)

(defrule moverFichaNegras
    (declare (salience 2)) ; a lo mejor hay que cambiar la saliencia
    ?x <-(moverFichaNegras ?dado1 ?dado2 $?datos)
    ?t <- (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
=>


    (printout t "Turno de las fichas negras." crlf )
    (bind ?todasEnCasaN (comprobarCasaN ?casasN ?fichas))
    (bind ?movimientosDisponibles (movimientosLegalesN ?dado1 ?dado2 ?fichasCapturadasN ?todasEnCasaN ?fichas))
    (if (eq (length$ ?movimientosDisponibles) 0) then ; si no hay movimientos disponibles
        (printout t "No hay movimientos disponibles." crlf )
    )

    (bind ?cantMov (length$ ?movimientosDisponibles))
    (if (> ?cantMov 0) then
    
    
        (imprimirMovimientos ?cantMov ?movimientosDisponibles)
        (printout t "Escoge un movimiento a realizar: " )
        (bind ?movimiento (read))
        ;;;;Mover ficha aqui y actualizar el tablero

        (bind ?i (* ?movimiento 3))

        (bind ?origen (nth$ (- ?i 2) ?movimientosDisponibles))
        (bind ?destino (nth$ (- ?i 1) ?movimientosDisponibles))

    ;;;;;;;;;;;;;,mover ficha;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (if (neq ?origen 25) then ; si no es una ficha capturada
            (bind ?queHayO (nth$ ?origen ?fichas))
        )
        (if (neq ?destino 0) then ; si su destino no es casa
            (bind ?queHayD (nth$ ?destino ?fichas))
        else
            (bind ?queHayD 0)
        )

        (if (eq ?queHayD 1) then ; hay una ficha blanca en el destino
            (bind ?fichas (replace$ ?fichas ?destino ?destino 0))
            (bind ?fichasCapturadasB (+ ?fichasCapturadasB 1)) ; sumo uno a las fichas capturadas blancas
            (bind ?queHayD (nth$ ?destino ?fichas))

        )
        (if (neq ?destino 0) then ; si no se mueve a casa, entonces se mueve normal
            (bind ?fichas (replace$ ?fichas ?destino ?destino (- ?queHayD 1))); caso generico no como
        else ; si se mueve a casa
            (bind ?casasN (+ ?casasN 1)) ; sumo una ficha a las casas negras
        )

        (if (neq ?origen 25) then ; si no es una ficha capturada
            (bind ?fichas (replace$ ?fichas ?origen ?origen (+ ?queHayO 1))); caso generico no como
        else 
            (bind ?fichasCapturadasN (- ?fichasCapturadasN 1)) ; quito una ficha capturada negra
        )


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (retract ?t) ;elimino el tablero anterior
        (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero



        (if (and(eq (- ?origen ?destino) ?dado1)(neq ?dado2 0)) then
            (retract ?x) ;Quito el turno actual
            (printout t "Aún falta el movimiento con el dado 2!" crlf )
            (assert (moverFichaNegras 0 ?dado2 ?fichas))
        else
            (if (and(eq (- ?origen ?destino) ?dado2)(neq ?dado1 0)) then
                (retract ?x) ;Quito el turno actual
                (printout t "Aún falta el movimiento con el dado 1!" crlf )
                (assert (moverFichaNegras ?dado1 0 ?fichas))
            else
                (if (eq (- ?origen ?destino) (+ ?dado1 ?dado2))
                    then
                        (retract ?x) ;Quito el turno actual
                        (printout t "Movimiento realizado!" crlf )

                )
            )

        )
    )
    (assert (imprimirTablero))
    (retract ?x) ;Quito el turno actual
    (if (eq ?casasN 15) then ; Todas las fichas negras en casa
        (printout t "Ganan las fichas negras!" crlf )
        (halt)
    )
    (assert (turnoBlancas 0 0))

)

(defrule moverFichaBlancas
    (declare (salience 2)) ; a lo mejor hay que cambiar la saliencia
    ?x <-(moverFichaBlancas ?dado1 ?dado2 $?datos)
    (test (or(neq ?dado1 0)(neq ?dado2 0))) ; compruebo que no sean 0 los 2 dados
    ?t<-(tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
=>
    (printout t "Turno de las fichas blancas." crlf )
    (bind ?todasEnCasaB (comprobarCasaB ?casasB ?fichas))
    (bind ?movimientosDisponibles (movimientosLegalesB ?dado1 ?dado2 ?fichasCapturadasB ?todasEnCasaB ?fichas))
    (if (eq (length$ ?movimientosDisponibles) 0) then
        (printout t "No hay movimientos disponibles." crlf )
    )
    
    (bind ?cantMov (length$ ?movimientosDisponibles))
    (if (> ?cantMov 0) then
        (imprimirMovimientos ?cantMov ?movimientosDisponibles)
        (printout t "Escoge un movimiento a realizar: " )
        (bind ?movimiento (read))
        ;;;;Mover ficha aqui y actualizar el tablero

        (bind ?i (* ?movimiento 3))

        (bind ?origen (nth$ (- ?i 2) ?movimientosDisponibles))
        (bind ?destino (nth$ (- ?i 1) ?movimientosDisponibles))

    ;;;;;;;;;;;;;,mover ficha;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (if (neq ?origen 0) then ; si no es una ficha capturada
            (bind ?queHayO (nth$ ?origen ?fichas))
        )
        (if (neq ?destino 25) then ; si no se va a meter en casa
            (bind ?queHayD (nth$ ?destino ?fichas))
        else
            (bind ?queHayD 0)
        )

        (if (eq ?queHayD -1) then ; hay una ficha negra en el destino
            (bind ?fichas (replace$ ?fichas ?destino ?destino 0))
            (bind ?fichasCapturadasN (+ ?fichasCapturadasN 1)) ; sumo uno a las fichas capturadas negras
            (bind ?queHayD (nth$ ?destino ?fichas))

        )
        (if (neq ?destino 25) then ; si no se va a meter en casa, entonces se mueve normal
            (bind ?fichas (replace$ ?fichas ?destino ?destino (+ ?queHayD 1))); caso generico no como
        else ; el destino es 25 (casa)
            (bind ?casasB (+ ?casasB 1)) ; sumo una ficha en casa
        )
        (if (neq ?origen 0) then ; si no es una ficha capturada, entonces se mueve normal
            (bind ?fichas (replace$ ?fichas ?origen ?origen (- ?queHayO 1))); caso generico no como
        else ; el origen es 0 (capturada)
            (bind ?fichasCapturadasB (- ?fichasCapturadasB 1)) ; quito una ficha capturada blanca
        )
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (retract ?t) ;elimino el tablero anterior
        (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero

        (if (and(eq (- ?destino ?origen) ?dado1)(neq ?dado2 0)) then
            (retract ?x) ;Quito el turno actual
            (printout t "Aún falta el movimiento con el dado 2!" crlf )
            (assert (moverFichaBlancas 0 ?dado2 ?fichas))
        else
            (if (and(eq (- ?destino ?origen) ?dado2)(neq ?dado1 0)) then
                (retract ?x) ;Quito el turno actual
                (printout t "Aún falta el movimiento con el dado 1!" crlf )
                (assert (moverFichaBlancas ?dado1 0 ?fichas))
            else
                (if (eq (- ?destino ?origen) (+ ?dado1 ?dado2))
                    then
                        (retract ?x) ;Quito el turno actual
                        (printout t "Movimiento realizado!" crlf )

                )
            )

        )
    )
    (assert (imprimirTablero))
    (retract ?x) ;Quito el turno actual
    (if (eq ?casasB 15) then ; Todas las fichas blancas en casa
        (printout t "Ganan las fichas blancas!" crlf )
        (halt)
    )

)





