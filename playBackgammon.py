import neat
import pickle
import random
import re
import numpy


config_path = "./configFile2"

# Cargar el checkpoint y restaurar el estado
with open('winner.txt', 'rb') as input:
    populationcargada = pickle.load(input)

# Acceder a la población y a otros elementos necesarios


# Supongamos que tienes una población llamada "population"

best_fitness = None
best_genome_key = None

config = neat.Config(neat.DefaultGenome, neat.DefaultReproduction,
                     neat.DefaultSpeciesSet, neat.DefaultStagnation,
                     config_path)

genomaGanador = populationcargada[0]

#crear red neuronal que va a jugar
net = neat.nn.FeedForwardNetwork.create(genome=genomaGanador, config=config)


juego_acabado = False
pregunta = open(
    "./pregunta.txt",
    "r+")

while not juego_acabado:

    movimientosL = open(
        "./movimientosLegales.txt",
        "r+")

    while True:
        datos = pregunta.readline()
        if len(datos) > 0:
            pregunta.truncate(0)
            break
    if datos == "1":
        juego_acabado = True
        pregunta.truncate(0)
        break

    patron = re.compile(r'-?\d+')
    # normalizar datos
    while True:
        movimientosLegales = movimientosL.read()
        if len(movimientosLegales) > 0 or juego_acabado:
            movimientosL.truncate(0)
            break

    if juego_acabado:
        break

    movimientosL = list(map(int, patron.findall(movimientosLegales)))
    # print(movimientosL)

    i = 3
    j = 0
    movimientosLD = {}
    while i < len(movimientosL):
        movimientosLD[(movimientosL[i - 3], movimientosL[i])] = (
            j, movimientosL[i - 2], movimientosL[i - 1])  # Mapero con llave (origen, dado) a valor destino
        i += 4
        j += 1

    datos = map(int, patron.findall(datos))
    datos = numpy.array(list(datos))
    datos = datos.reshape(32)

    salida = net.activate(datos)

    # print("Salida: ", salida, sum(salida))
    print(salida)

    salida = numpy.argmax(salida)
    print(salida)

    dado = salida // 25 + 24  # asi se que dado he usado

    dado = datos[dado]

    casilla = salida % 25  # asi se que casilla ha salido

    semaforo = open(
        "./leerSemaforo.txt",
        "r+")
    if (casilla, dado) in movimientosLD:
        respuesta = open(
            "./respuesta.txt",
            "w+")

        respuesta.write(str(movimientosLD[(casilla, dado)][0]))
        print("mov: ", movimientosLD[(casilla, dado)][0])
        semaforo.write("1")
        semaforo.close()

        respuesta.close()
    else:
        respuesta = open(
            "./respuesta.txt",
            "w+")
        movRandomEscribir = random.randint(1, 32)
        if len(movimientosLD):
            movRandomEscribir = movRandomEscribir % len(movimientosLD)
        else:
            movRandomEscribir = 1

        print("mov: ", movRandomEscribir)
        respuesta.write(str(movRandomEscribir))
        semaforo.write("1")
        semaforo.close()
        movimientosLD.clear()
        respuesta.close()

    pregunta.seek(0)
