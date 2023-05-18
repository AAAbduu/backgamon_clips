(deftemplate jugador 
    (slot tipo (type INTEGER)) ;1 si humano, 2 si IA
   ; (slot color (type STRING)(default "none"))
    (slot color (default none))
    (slot tipoDado (type INTEGER)) ; 1 dados auto , 2 dados físicos
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

(deffunction movimientosLegalesB (?dado1 ?fichasCapturadasB ?todasEnCasaB $?fichas) ;COMPLETAR CUANDO TIENES FICHAS CAPTURADAS
    (bind ?movimientos (create$))
    (bind ?nMovimiento (create$))

    ;;;;;;;;;;;;;;;;;;;;;;;;; MOVIMIENTOS FICHAS CAPTURADAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if(> ?fichasCapturadasB 0) then ;hay fichas blancas capturadas
        (if (and (neq ?dado1 0)(> (nth$ ?dado1 ?fichas) -2)) then ; si puedo mover la ficha capturada con el dado 1
            (bind ?nMovimiento  (create$ 0 ?dado1) 1 ?dado1) ;origen, destino, tipo
            (if (eq (nth$ ?dado1 ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                (bind ?nMovimiento (create$ 0 ?dado1) 2 ?dado1) ;origen, destino, tipo: como pieza
            )
                    
            (bind ?movimientos (create$ ?movimientos ?nMovimiento))
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
                    (bind ?nMovimiento  (create$ ?i (+ ?i ?dado1)) 1 ?dado1) ;origen, destino, tipo
                    (if (eq (nth$ (+ ?i ?dado1) ?fichas) -1) then ;Si hay una ficha negra en la posicion a la que me muevo
                        (bind ?nMovimiento (create$ ?i (+ ?i ?dado1)) 2 ?dado1) ;origen, destino, tipo: como pieza
                    )
                    
                    (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                else
                    (if (and (>= (+ ?i ?dado1) 25)(eq ?todasEnCasaB 1)) then ;Si me muevo a la casa
                        (bind ?nMovimiento (create$ ?i 25) 3 ?dado1) ;origen, destino, tipo: como casa
                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    )
                )
            )
        )
    )    
    (return ?movimientos)
)

(deffunction movimientosLegalesN (?dado1 ?fichasCapturadasN ?todasEnCasaN $?fichas) ;COMPLETAR CUANDO TIENES FICHAS CAPTURADAS
    (bind ?movimientos (create$))
    (bind ?nMovimiento (create$))

    ;;;;;;;;;;;;;;;;;;;;;;;;;MOVIMIENTOS FICHAS CAPTURADAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (> ?fichasCapturadasN 0) then ;hay fichas negras capturadas
        (if (and (neq ?dado1 0)(< (nth$ (- 25 ?dado1) ?fichas) 2)) then ; si puedo mover la ficha capturada con el dado 1
            (bind ?nMovimiento  (create$ 25 (- 25 ?dado1)) 1 ?dado1) ;origen, destino, tipo
            (if (eq (nth$ (- 25 ?dado1) ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                (bind ?nMovimiento (create$ 25 (- 25 ?dado1)) 2 ?dado1) ;origen, destino, tipo: como pieza
            )
                    
            (bind ?movimientos (create$ ?movimientos ?nMovimiento))
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
                    (bind ?nMovimiento  (create$ ?i (- ?i ?dado1)) 1 ?dado1) ;origen, destino, tipo
                    (if (eq (nth$ (- ?i ?dado1) ?fichas) 1) then ;Si hay una ficha blanca en la posicion a la que me muevo
                        (bind ?nMovimiento (create$ ?i (- ?i ?dado1)) 2 ?dado1) ;origen, destino, tipo: como pieza
                    )
                    
                    (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                else
                    (if (and (<= (- ?i ?dado1) 0)(eq ?todasEnCasaN 1)) then ;Si me muevo a la casa
                        (bind ?nMovimiento (create$ ?i 0) 3 ?dado1) ;origen, destino, tipo: como casa
                        (bind ?movimientos (create$ ?movimientos ?nMovimiento))
                    )
                )
            )
            (bind ?i (- ?i 1))
        )
    )    
    (return ?movimientos)
)

(deffunction comprobarCasaB (?fichasCapturadasB ?fichas) ;Comprueba si las fichas blancas estan en casa
    (if (eq ?fichasCapturadasB 0) then ;Si no hay fichas capturadas
        (loop-for-count (?i 1 18)
            (if (> (nth$ ?i ?fichas) 0) then ;Si hay alguna ficha blanca en las posiciones 1-18 devuelvo 0
                (return 0)
            )
        )
        (return 1)
    )
    (return 0)
)

(deffunction comprobarCasaN (?fichasCapturadasN ?fichas) ;Comprueba si las fichas negras estan en casa
    (if (eq ?fichasCapturadasN 0) then ;Si no hay fichas capturadas
        (bind ?i 24)
        (while (> ?i 6)
            (if (< (nth$ ?i ?fichas) 0) then ;Si hay alguna ficha negra en las posiciones 19-24 devuelvo 0
                (return 0)
            )
            (bind ?i (- ?i 1))
        )
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
            (bind ?i (+ ?i 4))
        )
    )
)

(deffunction asignarMovFichasN (?color ?tipo ?dado1 ?dado2  ?dado11 ?dado22 ?fichas)
    (if (eq ?color N)
    then
        (if (eq ?tipo 1) then
            (assert (moverFichaNegras ?dado1 ?dado2 ?dado11 ?dado22 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaNegrasPC ?dado1 ?dado2 ?dado11 ?dado22 ?fichas))   ;mueve fichas blancas IA
        )
    )
)

(deffunction asignarMovFichasB (?color ?tipo ?dado1 ?dado2 ?dado11 ?dado22 ?fichas)
    (if (eq ?color B)
    then
        (if (eq ?tipo 1) then
            (assert (moverFichaBlancas ?dado1 ?dado2 ?dado11 ?dado22 ?fichas)) ;mueve fichas blancas humano
        else
            (assert (moverFichaBlancasPC ?dado1 ?dado2 ?dado11 ?dado22 ?fichas))   ;mueve fichas blancas IA
        )
    )
)


(deffunction funcionEvaluacionEstadoN (?tablero); funcion que dado un tablero devuelve la utilidad de ese tablero
    ;estado del tablero
    (bind ?utilidad 0)
    (bind ?turno (fact-slot-value ?tablero turno))
    (bind ?fichas (fact-slot-value ?tablero juego))
    (bind ?fichasCapturadasB (fact-slot-value ?tablero fichasCapturadasB))
    (bind ?fichasCapturadasN (fact-slot-value ?tablero fichasCapturadasN))
    (bind ?casasB (fact-slot-value ?tablero casasB))
    (bind ?casasN (fact-slot-value ?tablero casasN))

    ;Realiza el ajuste de la función de evaluación en funcion de diferentes factores

    ; Ajuste la utilidad por la cantidad de fichas en casa.
    (bind ?utilidad (- ?utilidad ?casasB))
    (bind ?utilidad (+ ?utilidad ?casasN))

    ; Ajuste la utilidad por la cantidad de fichas capturadas.
    (bind ?utilidad (+ ?utilidad (* 2 ?fichasCapturadasB)))
    (bind ?utilidad (- ?utilidad (* 2 ?fichasCapturadasN)))

    (bind ?bloqueosN 0)

    (bind ?unaFichaN 0)

    (loop-for-count (?i 1 24)

        (if (<= (nth$ ?i ?fichas) -2) then
            (bind ?bloqueosN (+ ?bloqueosN 1)))

        (if (eq (nth$ ?i ?fichas) -1) then
            (bind ?unaFichaN (+ ?unaFichaN 1)))
    )
    ; Ajuste la utilidad por la cantidad de bloqueos y fichas solas

    (bind ?utilidad (+ ?utilidad (* ?bloqueosN 2)))
    (bind ?utilidad (- ?utilidad (* ?unaFichaN 2)))

    
    (return ?utilidad)
)


(deffunction evaluarProximosEstadosN (?tablero ?movimientos) ;Funcion que evalua los proximos estados (tableros) 
                                                            ;a partir de los movimientos disponibles y devuelve el mejor movimiento


    (bind ?id (fact-slot-value ?tablero id))
    (bind ?idPadre (fact-slot-value ?tablero idPadre))
    (bind ?jugadores (fact-slot-value ?tablero jugadores))                                                     
    (bind ?turno (fact-slot-value ?tablero turno))
    (bind ?fichas (fact-slot-value ?tablero juego))
    (bind ?fichasCapturadasB (fact-slot-value ?tablero fichasCapturadasB))
    (bind ?fichasCapturadasN (fact-slot-value ?tablero fichasCapturadasN))
    (bind ?casasB (fact-slot-value ?tablero casasB))
    (bind ?casasN (fact-slot-value ?tablero casasN))

    (bind ?cantMov (length$ ?movimientos))
    (bind ?cantMov (/ ?cantMov 4))
    (bind ?cantMov (integer ?cantMov))

    (bind ?mejorEval -1000)
    (bind ?mejorMov 0)
    (bind ?i 1)
    (bind ?utilidad 0)

    (while (<= ?i ?cantMov) do

        ;;;;;;;;;;;;;;Ajuste de utilidad en base a los movimientos;;;;;;;;;;;;;;;;;;;;;;;;
        
        (bind ?posicion (* ?i 4))
        (bind ?tipo(nth$ (- ?posicion 1) ?movimientos)) ; tipo de movimiento
        (if (eq ?tipo 1) then
            (bind ?utilidad (+ ?utilidad 1)) ; si es un movimiento normal
        )
        (if (eq ?tipo 2) then
            (bind ?utilidad (+ ?utilidad 12)) ; si es un movimiento de comer
        )
        (if (eq ?tipo 3) then
            (bind ?utilidad (+ ?utilidad 20)) ; si es un movimiento de meter en casa
        )

        (bind ?dadoUsado (nth$ ?posicion ?movimientos)) ; dadoUsado
        (bind ?origen (nth$ (- ?posicion 3) ?movimientos)) 
        (bind ?destino (nth$ (- ?posicion 2) ?movimientos))

        (if (<= (nth$ ?destino ?fichas) -2) then ; si hay más de una ficha negra en el destino
            (bind ?utilidad (+ ?utilidad 10))
        )
        ;;;;;;;;;;;;;;Realizo el movimiento en el tablero;;;;;;;;;;;;;;;;;;;;;;;;

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
    ;;;;;;;;;;;;;;;;;;;;;;;;;; duplico el tablero para evaluarlo;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (bind ?tableroDuplicado (duplicate ?tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN)))


        (bind ?evaluacion (+ ?utilidad (funcionEvaluacionEstadoN ?tableroDuplicado)))
        (if (> ?evaluacion ?mejorEval) then
            (bind ?mejorEval ?evaluacion)
            (bind ?mejorMov ?i)
        )
        (retract ?tableroDuplicado)
        
        (bind ?i (+ ?i 1))
        (bind ?utilidad 0)
    )

    (return ?mejorMov)

)

(deffunction funcionEvaluacionEstadoB (?tablero); funcion que dado un tablero devuelve la utilidad de ese tablero
    ;estado del tablero
    (bind ?utilidad 0)
    (bind ?turno (fact-slot-value ?tablero turno))
    (bind ?fichas (fact-slot-value ?tablero juego))
    (bind ?fichasCapturadasB (fact-slot-value ?tablero fichasCapturadasB))
    (bind ?fichasCapturadasN (fact-slot-value ?tablero fichasCapturadasN))
    (bind ?casasB (fact-slot-value ?tablero casasB))
    (bind ?casasN (fact-slot-value ?tablero casasN))

    ; Ajuste la utilidad por la cantidad de fichas en casa.
    (bind ?utilidad (- ?utilidad ?casasB))
    (bind ?utilidad (- ?utilidad ?casasN))

    ; Ajuste la utilidad por la cantidad de fichas capturadas.
    (bind ?utilidad (+ ?utilidad (* 2 ?fichasCapturadasB)))
    (bind ?utilidad (+ ?utilidad (* 2 ?fichasCapturadasN)))

    (bind ?bloqueosN 0)
    (bind ?bloqueosB 0)
    (bind ?unaFichaN 0)
    (bind ?unaFichaB 0)
    (loop-for-count (?i 1 24)
        (if (>= (nth$ ?i ?fichas) 2) then
            (bind ?bloqueosB (+ ?bloqueosB 1)))

        (if (eq (nth$ ?i ?fichas) 1) then
            (bind ?unaFichaB (+ ?unaFichaB 1)))

    )
    ; Ajuste la utilidad por la cantidad de bloqueos y fichas solas
    (bind ?utilidad (+ ?utilidad ?bloqueosB))
    (bind ?utilidad (- ?utilidad (* ?unaFichaB 2)))

    (return ?utilidad)
)



(deffunction evaluarProximosEstadosB (?tablero ?movimientos) ;Funcion que evalua los proximos estados (tableros) 
                                                            ;a partir de los movimientos disponibles y devuelve el mejor movimiento


    (bind ?id (fact-slot-value ?tablero id))
    (bind ?idPadre (fact-slot-value ?tablero idPadre))
    (bind ?jugadores (fact-slot-value ?tablero jugadores))                                                     
    (bind ?turno (fact-slot-value ?tablero turno))
    (bind ?fichas (fact-slot-value ?tablero juego))
    (bind ?fichasCapturadasB (fact-slot-value ?tablero fichasCapturadasB))
    (bind ?fichasCapturadasN (fact-slot-value ?tablero fichasCapturadasN))
    (bind ?casasB (fact-slot-value ?tablero casasB))
    (bind ?casasN (fact-slot-value ?tablero casasN))

    (bind ?cantMov (length$ ?movimientos))
    (bind ?cantMov (/ ?cantMov 4))
    (bind ?cantMov (integer ?cantMov))

    (bind ?mejorEval -1000)
    (bind ?mejorMov 0)
    (bind ?i 1)
    (bind ?utilidad 0)

    (while (<= ?i ?cantMov) do
        ;;;;;;;;;;;;;;Ajuste de utilidad en base a los movimientos;;;;;;;;;;;;;;;;;;;;;;;;

        (bind ?posicion (* ?i 4))
        (bind ?tipo(nth$ (- ?posicion 1) ?movimientos)) ; tipo de movimiento
        (if (eq ?tipo 1) then
            (bind ?utilidad (+ ?utilidad 1)) ; si es un movimiento normal
        )
        (if (eq ?tipo 2) then
            (bind ?utilidad (+ ?utilidad 20)) ; si es un movimiento de comer
        )
        (if (eq ?tipo 3) then
            (bind ?utilidad (+ ?utilidad 30)) ; si es un movimiento de meter en casa
        )

        
        (bind ?dadoUsado (nth$ ?posicion ?movimientos)) ; dadoUsado
        (bind ?origen (nth$ (- ?posicion 3) ?movimientos)) 
        (bind ?destino (nth$ (- ?posicion 2) ?movimientos))
        
        (if (<= (nth$ ?destino ?fichas) -2) then ; si hay más de una ficha negra en el destino
            (bind ?utilidad (+ ?utilidad 10))
        )
        ;;;;;;;;;;;;;;Realizo el movimiento en el tablero;;;;;;;;;;;;;;;;;;;;;;;;
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
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; duplico el tablero para evaluarlo;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (bind ?tableroDuplicado (duplicate ?tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN)))


        (bind ?evaluacion (+ ?utilidad (funcionEvaluacionEstadoB ?tableroDuplicado)))
        (if (> ?evaluacion ?mejorEval) then
            (bind ?mejorEval ?evaluacion)
            (bind ?mejorMov ?i)
        )
        (retract ?tableroDuplicado)
        
        (bind ?i (+ ?i 1))
        (bind ?utilidad 0)
    )

    (return ?mejorMov)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UTILIDADES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction pause (?delay)
   (bind ?start (time))
   (while (< (time) (+ ?start ?delay)) do)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;REGLAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule inicializar
    (declare (salience 1))    

=>

    (printout t "¿Seras humano o IA? (1 para humano, 2 para IA):" ) 
    (bind ?tipo1 (read))
    ;(bind ?tipo1 2)
    (bind ?color1 "none")
    (printout t "¿Qué color quieres coger? (N o B): " )
    (bind ?color1 (read))
    ;(bind ?color1 N)
    (bind ?color1 (sym-cat ?color1))
    (printout t "¿Como quieres los tipos de dados (1 para automaticos, 2 para fisicos)")
    (bind ?tipoDado1 (read))
    ;(bind ?tipoDado1 1)
    (bind ?j1 (assert (jugador (tipo ?tipo1) (color ?color1) (tipoDado ?tipoDado1))))

    

    (bind ?j2 ?j1)

    (printout t "¿Seras humano o IA? (1 para humano, 2 para IA):" )
    (bind ?tipo2 (read))
    ;(bind ?tipo2 2)
    (if (eq ?color1 N) then
        (bind ?color2 B)
    else
        (bind ?color2 N)
    )
    (printout t "¿Como quieres los tipos de dados (1 para automaticos, 2 para fisicos)")
    (bind ?tipoDado2 (read))
    ;(bind ?tipoDado2 1)
    (bind ?j2 (assert (jugador (tipo ?tipo2) (color ?color2) (tipoDado ?tipoDado2))))

    
    

    (if (eq ?tipo1 1) then ;Si el jugador 1 y 2 son humanos o el jugador 1 es humano y el jugador 2 IA 
        (printout t "Jugador 1, ¿Cara o cruz? (1 o 2): " )
        (bind ?cCJ1 (read))
        ;(bind ?cCJ1 1)

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
        ;(bind ?cCJ2 1)

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
     (jugadores ?j1 ?j2) (juego 2 0 0 0 0 -5 0 -3 0 0 0 5 -5 0 0 0 3 0 5 0 0 0 0 -2) ;table inicial bueno (juego 2 0 0 0 0 -5 0 -3 0 0 0 5 -5 0 0 0 3 0 5 0 0 0 0 -2), tablero prueba (juego -2 -2 -2 -2 -2 -5 0 0 0 0 0 0 0 0 0 0 0 0 5 2 2 2 2 2)
     (fichasCapturadasB 0) (fichasCapturadasN 0) (casasB 0) (casasN 0)))) ; desde la posicion 1 ;;;;tablero con error ahora: (juego -5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
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

(defrule turnoNegras
    (declare (salience 1))
    ?x <-(turnoNegras ?dado1 ?dado2)
    ?t <- (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB)
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
    
    ;get player with fichas "N"
    ?j1 <- (jugador (tipo ?tipo) (color ?color) (tipoDado ?tipoDado1))
    ?j2 <- (jugador (tipo ?tipo2) (color ?color2) (tipoDado ?tipoDado2))
    (test (eq ?turno ?color))
    (test (neq ?j1 ?j2))
    

=>
    (bind ?dobles 0) ;por defecto no hay dobles
    (retract ?x)
    (if (and(eq ?dado1 0)(eq ?dado2 0)) then
        (if (eq ?tipoDado1 1) then
        
            (bind ?dado1 (random 1 6)) ;(random 1 6)
            (bind ?dado2 (random 1 6))  ;tiro los dados
        else
            (printout t "Valor para el dado 1: ")
            (bind ?dado1 (read))
            (printout t "Valor para el dado 2: ")
            (bind ?dado2 (read))
        )

        (printout t "Dado 1: " ?dado1 crlf )
        (printout t "Dado 2: " ?dado2 crlf )

        (if (eq ?dado1 ?dado2) then

            (printout t "Dobles! " crlf )
            ;(assert (dobles N ?dado1 ?dado2))
            (bind ?dobles 1); flag para saber si hay dobles
        )
           
    )
    (if (eq ?dobles 1) then
        (asignarMovFichasN ?color ?tipo ?dado1 (* 2 ?dado1) (* 3 ?dado1) (* 4 ?dado1) ?fichas)

    else
        (asignarMovFichasN ?color ?tipo ?dado1 ?dado2 (+ ?dado1 ?dado2) 0 ?fichas)
    )

    (bind ?turnoN B)    
    (retract ?t)
    (retract ?x)
    (assert(tablero (id ?id) (idPadre ?idPadre) (turno ?turnoN) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN)))
    (assert (turnoBlancas 0 0))

)

(defrule turnoBlancas
    (declare (salience 1))
    ?x <-(turnoBlancas ?dado1 ?dado2)
    ?t <-(tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB)
     (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
    ?j1 <- (jugador (tipo ?tipo) (color ?color) (tipoDado ?tipoDado1))
    ?j2 <- (jugador (tipo ?tipo2) (color ?color2) (tipoDado ?tipoDado2))
    (test (eq ?turno ?color))
    (test (neq ?j1 ?j2))
    
=>
    (bind ?dobles 0)
    (retract ?x) ;quito el turno ya
    (if (and(eq ?dado1 0)(eq ?dado2 0)) then
        (if (eq ?tipoDado1 1) then
            (bind ?dado1 (random 1 6))
            (bind ?dado2 (random 1 6)) 
        else
            (printout t "Valor para el dado 1: ")
            (bind ?dado1 (read))
            (printout t "Valor para el dado 2: ")
            (bind ?dado2 (read))
        )
        (printout t "Dado 1: " ?dado1 crlf )
        (printout t "Dado 2: " ?dado2 crlf )

        (if (eq ?dado1 ?dado2) then
            (printout t "Dobles! " crlf )
            (bind ?dobles 1); flag para saber si hay dobles
    )
    )

    ;check if jugador blancas is human
    (if (eq ?dobles 1) then
        (asignarMovFichasB ?color ?tipo ?dado1 (* 2 ?dado1) (* 3 ?dado1) (* 4 ?dado1) ?fichas)

    else
        (asignarMovFichasB ?color ?tipo ?dado1 ?dado2 (+ ?dado1 ?dado2) 0 ?fichas)
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
    ?x <-(moverFichaNegras ?dado1 ?dado2 ?dado11 ?dado22 $?datos)
    ?t <- (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
=>
    (bind ?dobles 0); por defecto no hay dobles
    (if (> ?dado22 0) then
        (bind ?dobles 1)    ;hay dobles
    )

    (bind ?dados (create$ ?dado1 ?dado2 ?dado11 ?dado22)); meto todos las combinaciones de movimientos

    (bind ?todasEnCasaN (comprobarCasaN ?fichasCapturadasN ?fichas)) ;compruebo si todas las fichas estan en casa

    (bind ?movimientos (create$))

    (loop-for-count (?i (length$ ?dados))
        (bind ?dado (nth$ ?i ?dados))
        (if (> ?dado 0) then
            (bind ?movimientosDisponibles (movimientosLegalesN ?dado ?fichasCapturadasN ?todasEnCasaN ?fichas))
            ;insert into movimientos
            (bind ?movimientos (create$ ?movimientos ?movimientosDisponibles))
        )

    )

    (if (eq (length$ ?movimientos) 0) then ; si no hay movimientos disponibles
        (printout t "No hay movimientos disponibles." crlf )
    )


    (bind ?cantMov (length$ ?movimientos))
    (if (> ?cantMov 0) then
    

        (printout t "Turno de las fichas negras." crlf )
    
        (imprimirMovimientos ?cantMov ?movimientos)
        (printout t "Escoge un movimiento a realizar: " )
        (bind ?movimiento (read))
        ;;;;Mover ficha aqui y actualizar el tablero

        (bind ?i (* ?movimiento 4))

        (bind ?tipo(nth$ (- ?i 1) ?movimientos)) ; tipo de movimiento
        (bind ?dadoUsado (nth$ ?i ?movimientos)) ; dadoUsado
        (bind ?origen (nth$ (- ?i 3) ?movimientos)) 
        (bind ?destino (nth$ (- ?i 2) ?movimientos))

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

       (if (eq ?tipo 3) then
            (loop-for-count(?j (length$ ?dados))
                (if (eq (nth$ ?j ?dados) ?dadoUsado) then
                    (bind ?dados (replace$ ?dados ?j ?j 0))
                )
            )
        )

        (if (eq ?dadoUsado ?dado11) then
            (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 3
            (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
            (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
            (if (<= (- ?dado22 ?dado11) 0) then
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4

            else
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado11 ))) ;resto al dado 4 el dado3
            )
        else (if (eq ?dadoUsado ?dado22) then
                (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
                (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4
            )
        )

        (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado1)) then;si hay dobles y uso dado1
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 2 2 (- ?dado2 ?dado1)))
            (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado1)))
            (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado1)))

        else (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 1 1 0))
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado2)))
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado2)))
                )
            )

        (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado1)) then
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 3 3 0))
        else (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 0))
                )
            )
        (retract ?t) ;elimino el tablero anterior
        (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    )

    (bind ?dado1 (nth$ 1 ?dados))
    (if (<= ?dado1 0)
        then
        (bind ?dado1 0)
    )
    (bind ?dado2 (nth$ 2 ?dados))
    (if (<= ?dado2 0)
        then
        (bind ?dado2 0)
    )
    (bind ?dado11 (nth$ 3 ?dados))
    (if (<= ?dado11 0)
        then
        (bind ?dado11 0)
    )
    (bind ?dado22 (nth$ 4 ?dados))
    (if (<= ?dado22 0)
        then
        (bind ?dado22 0)
    )

        (if (and (and (and (eq ?dado1 0)  (eq ?dado2 0)) (eq ?dado11 0))  (eq ?dado22 0)) then ; si no quedan dados
            (printout t "Turno realizado!" crlf )
        
            else

            (assert (moverFichaNegras ?dado1 ?dado2 ?dado11 ?dado22 ?fichas))

        )
        

    (retract ?t) ;elimino el tablero anterior
    (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    (assert (imprimirTablero))
    (retract ?x) ;Quito el turno actual
    (if (eq ?casasN 15) then ; Todas las fichas negras en casa
        (printout t "Ganan las fichas negras!" crlf )
        (halt)
    )

)

(defrule moverFichaBlancas
    (declare (salience 2)) ; a lo mejor hay que cambiar la saliencia
    ?x <-(moverFichaBlancas ?dado1 ?dado2 ?dado11 ?dado22 $?datos)
    ?t<-(tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
=>
    (bind ?dobles 0); por defecto no hay dobles
    (if (> ?dado22 0) then
        (bind ?dobles 1)    ;hay dobles
    )

    (bind ?dados (create$ ?dado1 ?dado2 ?dado11 ?dado22)); meto todos las combinaciones de movimientos

    (bind ?todasEnCasaB (comprobarCasaB ?fichasCapturadasB ?fichas)) ;compruebo si todas las fichas estan en casa

    (bind ?movimientos (create$))

    (loop-for-count (?i (length$ ?dados))
        (bind ?dado (nth$ ?i ?dados))
        (if (> ?dado 0) then
            (bind ?movimientosDisponibles (movimientosLegalesB ?dado ?fichasCapturadasB ?todasEnCasaB ?fichas))
            ;insert into movimientos
            (bind ?movimientos (create$ ?movimientos ?movimientosDisponibles))
        )

    )

    (if (eq (length$ ?movimientos) 0) then
        (printout t "No hay movimientos disponibles." crlf )
    )


    
    
    (bind ?cantMov (length$ ?movimientos))
    (if (> ?cantMov 0) then
        (printout t "Turno de las fichas blancas." crlf )

        (imprimirMovimientos ?cantMov ?movimientos)
        (printout t "Escoge un movimiento a realizar: " )
        (bind ?movimiento (read))
        ;;;;Mover ficha aqui y actualizar el tablero

        (bind ?i (* ?movimiento 4))
        (bind ?tipo(nth$ (- ?i 1) ?movimientos)) ; tipo de movimiento
        (bind ?dadoUsado (nth$ ?i ?movimientos)) ; dadoUsado
        (bind ?origen (nth$ (- ?i 3) ?movimientos))
        (bind ?destino (nth$ (- ?i 2) ?movimientos))

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
        (bind ?movRealizado (- ?destino ?origen)) ;cantidad de casillas que he movido

        (if (eq ?tipo 3) then
            (loop-for-count(?j (length$ ?dados))
                (if (eq (nth$ ?j ?dados) ?dadoUsado) then
                    (bind ?dados (replace$ ?dados ?j ?j 0)) ;elimino la posibilidad de dado 3
                )
            )
        )

        (if (eq ?dadoUsado ?dado11) then
            (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 3
            (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
            (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
            (if (<= (- ?dado22 ?dado11) 0) then
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4

            else
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado11 ))) ;resto al dado 4 el dado3
            )
        else (if (eq ?dadoUsado ?dado22) then
                (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
                (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4
            )
        )

        (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado1)) then;si hay dobles y uso dado1
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 2 2 (- ?dado2 ?dado1)))
            (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado1)))
            (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado1)))

        else (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 1 1 0))
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado2)))
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado2)))
                )
            )

        (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado1)) then
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 3 3 0))
        else (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 0))
                )
            )
        (retract ?t) ;elimino el tablero anterior
        (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    )

    (bind ?dado1 (nth$ 1 ?dados))
    (if (<= ?dado1 0)
        then
        (bind ?dado1 0)
    )
    (bind ?dado2 (nth$ 2 ?dados))
    (if (<= ?dado2 0)
        then
        (bind ?dado2 0)
    )
    (bind ?dado11 (nth$ 3 ?dados))
    (if (<= ?dado11 0)
        then
        (bind ?dado11 0)
    )
    (bind ?dado22 (nth$ 4 ?dados))
    (if (<= ?dado22 0)
        then
        (bind ?dado22 0)
    )

    (if (and (and (and (eq ?dado1 0)  (eq ?dado2 0)) (eq ?dado11 0))  (eq ?dado22 0)) then ; si no quedan dados
        (printout t "Turno realizado!" crlf )
        
        else

        (assert (moverFichaBlancas ?dado1 ?dado2 ?dado11 ?dado22 ?fichas))

    )

    (retract ?t) ;elimino el tablero anterior
    (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    (assert (imprimirTablero))
    (retract ?x) ;Quito el turno actual
    (if (eq ?casasB 15) then ; Todas las fichas blancas en casa
        (printout t "Ganan las fichas blancas!" crlf )
        (halt)
    )
    
)

(defrule moverFichaNegrasPC
    (declare (salience 2)) ; a lo mejor hay que cambiar la saliencia
    ?x <-(moverFichaNegrasPC ?dado1 ?dado2 ?dado11 ?dado22 $?datos)
    ?t <- (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
=>
    (bind ?dobles 0); por defecto no hay dobles
    (if (> ?dado22 0) then
        (bind ?dobles 1)    ;hay dobles
    )

    (bind ?dados (create$ ?dado1 ?dado2 ?dado11 ?dado22)); meto todos las combinaciones de movimientos

    (bind ?todasEnCasaN (comprobarCasaN ?fichasCapturadasN ?fichas)) ;compruebo si todas las fichas estan en casa

    (bind ?movimientos (create$))

    (loop-for-count (?i (length$ ?dados))
        (bind ?dado (nth$ ?i ?dados))
        (if (> ?dado 0) then
            (bind ?movimientosDisponibles (movimientosLegalesN ?dado ?fichasCapturadasN ?todasEnCasaN ?fichas))
            ;insert into movimientos
            (bind ?movimientos (create$ ?movimientos ?movimientosDisponibles))
        )
    )

    (open "pregunta.txt" pregunta "r+")
    (open "movimientosLegales.txt" mov_legales "r+")
    (printout mov_legales ?movimientos crlf)
    (printout pregunta ?fichas " " ?dados " " ?casasN " " ?casasB " " ?fichasCapturadasN " " ?fichasCapturadasB " " crlf)
    (close pregunta)
    (close mov_legales)

  
    (open "leerSemaforo.txt" semaforo "r")
    (bind ?semaforo (read semaforo))
    (close semaforo)
    (while (eq ?semaforo EOF) do
        (open "leerSemaforo.txt" semaforo "r")
        (bind ?semaforo (read semaforo))
        (close semaforo)
    )

    (system "rm leerSemaforo.txt")
    (open "leerSemaforo.txt" semaforo "w+")

    (open "respuesta.txt" respuesta "r")
    (bind ?i (read respuesta))
    (close respuesta)
    (close semaforo)
    (bind ?i (integer ?i))


    (if (eq (length$ ?movimientos) 0) then ; si no hay movimientos disponibles
        (printout t "No hay movimientos disponibles." crlf )
    )

    (bind ?cantMov (length$ ?movimientos))
    (bind ?cantMov (/ ?cantMov 4))

     (bind ?cantMov (integer ?cantMov))


    (if (and (neq (type ?i) INTEGER) (neq ?cantMov 0))
        then
        (bind ?i (random 1 ?cantMov))
        ;(bind ?i (evaluarProximosEstadosN ?t ?movimientos))
    )

    (if (and (eq ?i 0) (neq ?cantMov 0)) then
        (bind ?i (random 1 ?cantMov))
        ;(bind ?i (evaluarProximosEstadosN ?t ?movimientos))
    )

    (if (and(> ?i (length$ ?movimientos))(neq ?cantMov 0)) then
        (bind ?i (integer ?i))
        (bind ?i (mod ?i ?cantMov))
    )




    (printout t "Movimiento elegido: " ?i crlf)
    (bind ?i (* ?i 4))



    (system "rm respuesta.txt")


    (bind ?cantMov (length$ ?movimientos))
    (if (and(> ?cantMov 0)(neq ?i 0)) then
    

        (printout t "Turno de las fichas negras." crlf )
    
        (imprimirMovimientos ?cantMov ?movimientos)
        ;;;;Mover ficha aqui y actualizar el tablero

        (bind ?tipo(nth$ (- ?i 1) ?movimientos)) ; tipo de movimiento
        (bind ?dadoUsado (nth$ ?i ?movimientos)) ; dadoUsado
        (bind ?origen (nth$ (- ?i 3) ?movimientos)) 
        (bind ?destino (nth$ (- ?i 2) ?movimientos))

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

       (if (eq ?tipo 3) then
            (loop-for-count(?j (length$ ?dados))
                (if (eq (nth$ ?j ?dados) ?dadoUsado) then
                    (bind ?dados (replace$ ?dados ?j ?j 0))
                )
            )
        )

        (if (eq ?dadoUsado ?dado11) then
            (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 3
            (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
            (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
            (if (<= (- ?dado22 ?dado11) 0) then
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4

            else
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado11 ))) ;resto al dado 4 el dado3
            )
        else (if (eq ?dadoUsado ?dado22) then
                (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
                (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4
            )
        )

        (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado1)) then;si hay dobles y uso dado1
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 2 2 (- ?dado2 ?dado1)))
            (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado1)))
            (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado1)))

        else (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 1 1 0))
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado2)))
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado2)))
                )
            )

        (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado1)) then
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 3 3 0))
        else (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 0))
                )
            )
        (retract ?t) ;elimino el tablero anterior
        (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    )

    (bind ?dado1 (nth$ 1 ?dados))
    (if (<= ?dado1 0)
        then
        (bind ?dado1 0)
    )
    (bind ?dado2 (nth$ 2 ?dados))
    (if (<= ?dado2 0)
        then
        (bind ?dado2 0)
    )
    (bind ?dado11 (nth$ 3 ?dados))
    (if (<= ?dado11 0)
        then
        (bind ?dado11 0)
    )
    (bind ?dado22 (nth$ 4 ?dados))
    (if (<= ?dado22 0)
        then
        (bind ?dado22 0)
    )

        (if (and (and (and (eq ?dado1 0)  (eq ?dado2 0)) (eq ?dado11 0))  (eq ?dado22 0)) then ; si no quedan dados
            (printout t "Turno realizado!" crlf )
        
            else

            (assert (moverFichaNegrasPC ?dado1 ?dado2 ?dado11 ?dado22 ?fichas))

        )
        

    (retract ?t) ;elimino el tablero anterior
    (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    (assert (imprimirTablero))
    (retract ?x) ;Quito el turno actual
    (if (eq ?casasN 15) then ; Todas las fichas negras en casa
        (printout t "Ganan las fichas negras!" crlf )
        (open "pregunta.txt" pregunta "r+")
        (printout pregunta "1")
        (close pregunta)
        ;(pause 3)
        (reset)
       ;(printout t "Reiniciando... Pulse una tecla para reiniciar" crlf)
        ;(bind ?x (read))
        (run)
    )

    (retract ?x)


)

(defrule moverFichaBlancasPC
    (declare (salience 2)) ; a lo mejor hay que cambiar la saliencia
    ?x <-(moverFichaBlancasPC ?dado1 ?dado2 ?dado11 ?dado22 $?datos)
    ?t<-(tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))
=>
    (bind ?dobles 0); por defecto no hay dobles
    (if (> ?dado22 0) then
        (bind ?dobles 1)    ;hay dobles
    )

    (bind ?dados (create$ ?dado1 ?dado2 ?dado11 ?dado22)); meto todos las combinaciones de movimientos

    (bind ?todasEnCasaB (comprobarCasaB ?fichasCapturadasB ?fichas)) ;compruebo si todas las fichas estan en casa

    (bind ?movimientos (create$))

    (loop-for-count (?i (length$ ?dados))
        (bind ?dado (nth$ ?i ?dados))
        (if (> ?dado 0) then
            (bind ?movimientosDisponibles (movimientosLegalesB ?dado ?fichasCapturadasB ?todasEnCasaB ?fichas))
            ;insert into movimientos
            (bind ?movimientos (create$ ?movimientos ?movimientosDisponibles))
        )
    )

    (open "pregunta.txt" pregunta "r+")
    (open "movimientosLegales.txt" mov_legales "r+")
    (printout mov_legales ?movimientos crlf)
    (printout pregunta ?fichas " " ?dados " " ?casasN " " ?casasB " " ?fichasCapturadasN " " ?fichasCapturadasB " " crlf)
    (close pregunta)
    (close mov_legales)


    (open "leerSemaforo.txt" semaforo "r")
    (bind ?semaforo (read semaforo))
    (close semaforo)
    (while (eq ?semaforo EOF) do
        (open "leerSemaforo.txt" semaforo "r")
        (bind ?semaforo (read semaforo))
        (close semaforo)
    )


    (system "rm leerSemaforo.txt")
    (open "leerSemaforo.txt" semaforo "w+")

    (open "respuesta.txt" respuesta "r")
    (bind ?i (read respuesta))
    (close respuesta)
    (close semaforo)
    (bind ?i (integer ?i))


    (if (eq (length$ ?movimientos) 0) then ; si no hay movimientos disponibles
        (printout t "No hay movimientos disponibles." crlf )
    )

    (bind ?cantMov (length$ ?movimientos))
    (bind ?cantMov (/ ?cantMov 4))

    (bind ?cantMov (integer ?cantMov))


    (if (and (neq (type ?i) INTEGER) (neq ?cantMov 0))
        then
        (bind ?i (random 1 ?cantMov))
        ;(bind ?i (evaluarProximosEstadosB ?t ?movimientos))
    )

    (if (and (eq ?i 0) (neq ?cantMov 0)) then
        (bind ?i (random 1 ?cantMov))
        ;(bind ?i (evaluarProximosEstadosB ?t ?movimientos))
    )

    (if (and(> ?i (length$ ?movimientos))(neq ?cantMov 0)) then
        (bind ?i (integer ?i))
        (bind ?i (mod ?i ?cantMov))
    )


    (printout t "Movimiento elegido: " ?i crlf)
    (bind ?i (* ?i 4))
    (system "rm respuesta.txt")
    
    
    (bind ?cantMov (length$ ?movimientos))
    (if (and(> ?cantMov 0)(neq ?i 0)) then
        (printout t "Turno de las fichas blancas." crlf )

        (imprimirMovimientos ?cantMov ?movimientos)
        ;;;;Mover ficha aqui y actualizar el tablero

        (bind ?tipo(nth$ (- ?i 1) ?movimientos)) ; tipo de movimiento
        (bind ?dadoUsado (nth$ ?i ?movimientos)) ; dadoUsado
        (bind ?origen (nth$ (- ?i 3) ?movimientos))
        (bind ?destino (nth$ (- ?i 2) ?movimientos))

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
        (bind ?movRealizado (- ?destino ?origen)) ;cantidad de casillas que he movido

        (if (eq ?tipo 3) then
            (loop-for-count(?j (length$ ?dados))
                (if (eq (nth$ ?j ?dados) ?dadoUsado) then
                    (bind ?dados (replace$ ?dados ?j ?j 0)) ;elimino la posibilidad de dado 3
                )
            )
        )

        (if (eq ?dadoUsado ?dado11) then
            (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 3
            (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
            (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
            (if (<= (- ?dado22 ?dado11) 0) then
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4

            else
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado11 ))) ;resto al dado 4 el dado3
            )
        else (if (eq ?dadoUsado ?dado22) then
                (bind ?dados (replace$ ?dados 1 1 0)) ;elimino la posibilidad de dado 1
                (bind ?dados (replace$ ?dados 2 2 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 3 3 0)) ;elimino la posibilidad de dado 2
                (bind ?dados (replace$ ?dados 4 4 0)) ;elimino la posibilidad de dado 4
            )
        )

        (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado1)) then;si hay dobles y uso dado1
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 2 2 (- ?dado2 ?dado1)))
            (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado1)))
            (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado1)))

        else (if (and(eq ?dobles 1) (eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 1 1 0))
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 (- ?dado11 ?dado2)))
                (bind ?dados (replace$ ?dados 4 4 (- ?dado22 ?dado2)))
                )
            )

        (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado1)) then
            (bind ?dados (replace$ ?dados 1 1 0))
            (bind ?dados (replace$ ?dados 3 3 0))
        else (if (and(eq ?dobles 0)(eq ?dadoUsado ?dado2)) then
                (bind ?dados (replace$ ?dados 2 2 0))
                (bind ?dados (replace$ ?dados 3 3 0))
                )
            )
        (retract ?t) ;elimino el tablero anterior
        (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
        (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    )

    (bind ?dado1 (nth$ 1 ?dados))
    (if (<= ?dado1 0)
        then
        (bind ?dado1 0)
    )
    (bind ?dado2 (nth$ 2 ?dados))
    (if (<= ?dado2 0)
        then
        (bind ?dado2 0)
    )
    (bind ?dado11 (nth$ 3 ?dados))
    (if (<= ?dado11 0)
        then
        (bind ?dado11 0)
    )
    (bind ?dado22 (nth$ 4 ?dados))
    (if (<= ?dado22 0)
        then
        (bind ?dado22 0)
    )

    (if (and (and (and (eq ?dado1 0)  (eq ?dado2 0)) (eq ?dado11 0))  (eq ?dado22 0)) then ; si no quedan dados
        (printout t "Turno realizado!" crlf )
        
        else

        (assert (moverFichaBlancasPC ?dado1 ?dado2 ?dado11 ?dado22 ?fichas))

    )

    (retract ?t) ;elimino el tablero anterior
    (assert (tablero (id ?id) (idPadre ?idPadre) (turno ?turno) (jugadores $?jugadores) (juego $?fichas) (fichasCapturadasB ?fichasCapturadasB) 
    (fichasCapturadasN ?fichasCapturadasN) (casasB ?casasB) (casasN ?casasN))) ;actualizo el tablero
    (assert (imprimirTablero))
    (retract ?x) ;Quito el turno actual
    (if (eq ?casasB 15) then ; Todas las fichas blancas en casa
        (printout t "Ganan las fichas blancas!" crlf )
        (open "pregunta.txt" pregunta "r+")
        (printout pregunta "1")
        (close pregunta)
       ; (pause 3)
        (reset)
        ;(printout t "Reiniciando... Pulse una tecla para reiniciar" crlf)
        ;(bind ?x (read))
        (run)
    )
    
)




