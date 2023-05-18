import neat
import numpy
import re
import random
import pickle
import time

tiempoInicial = time.time()

# Definir la función de fitness para evaluar a los individuos de la población
def eval_genomes(genomes, config):
    genomes[-1][1].fitness = 0
    for i in range(0, len(genomes), 2):

        id_1, genome1 = genomes[i]
        id_2, genome2 = genomes[i+1]
        print("hola", id_1)

        net1 = neat.nn.FeedForwardNetwork.create(genome1, config)
        net2 = neat.nn.FeedForwardNetwork.create(genome2, config)

        pregunta = open(
            "/Users/abdu/Desktop/EHU/Tercero/SegundoCuatri/IA/proyectoClips/backgamon_clips/backgamon_clips/pregunta.txt",
            "r+")
        count = 0
        juego_acabado = False
        fichas_capturadas1 = 0
        fichas_capturadas2 = 0
        fichas_casa1 = 0
        fichas_casa2 = 0
        movRandom = 0
        movRandom2 = 0
        movimientosLegal2 = 0
        movTotales = 0
        movimientosLegal = 0

        while not juego_acabado:

            movimientosL = open(
                "/Users/abdu/Desktop/EHU/Tercero/SegundoCuatri/IA/proyectoClips/backgamon_clips/backgamon_clips/movimientosLegales.txt",
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
            #print(movimientosL)

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

            if count % 2 == 0:
                salida = net1.activate(datos) #juega el agente 1
            else:
                salida = net2.activate(datos) #juega el agente 2


            #print("Salida: ", salida, sum(salida))


            salida = numpy.argmax(salida)

            dado = salida // 25 + 24   #asi se que dado he usado

            dado = datos[dado]



            casilla = salida % 25  # asi se que casilla ha salido


            semaforo = open(
                "/Users/abdu/Desktop/EHU/Tercero/SegundoCuatri/IA/proyectoClips/backgamon_clips/backgamon_clips/leerSemaforo.txt",
                "r+")
            if (casilla, dado) in movimientosLD:
                respuesta = open(
                    "/Users/abdu/Desktop/EHU/Tercero/SegundoCuatri/IA/proyectoClips/backgamon_clips/backgamon_clips/respuesta.txt",
                    "w+")

                if movimientosLD[(casilla, dado)][2] == 2:
                    if count % 2 == 0:
                        fichas_capturadas1 += 1
                    else:
                        fichas_capturadas2 += 1

                elif movimientosLD[(casilla, dado)][2] == 3:
                    if count % 2 == 0:
                        fichas_casa1 += 1
                    else:
                        fichas_casa2 += 1

                if count % 2 == 0:
                    movimientosLegal += 1
                else:
                    movimientosLegal2 += 1


                respuesta.write(str(movimientosLD[(casilla, dado)][0]))
                semaforo.write("1")
                semaforo.close()

                respuesta.close()
            else:
                respuesta = open(
                    "/Users/abdu/Desktop/EHU/Tercero/SegundoCuatri/IA/proyectoClips/backgamon_clips/backgamon_clips/respuesta.txt",
                    "w+")
                movRandomEscribir = random.randint(1, len(movimientosLD) if len(movimientosLD) > 0 else 1)
                if len(movimientosLD):
                    movRandomEscribir = movRandomEscribir % len(movimientosLD)
                else:
                    movRandomEscribir = 1
                respuesta.write(str(movRandomEscribir))
                if count % 2 == 0:
                    movRandom += 1
                else:
                    movRandom2 += 1
                semaforo.write("1")
                semaforo.close()
                movimientosLD.clear()
                respuesta.close()

            pregunta.seek(0)
            movTotales += 1
            count += 1

        solution_fitness1 = 0

        solution_fitness2 = 0


        solution_fitness1 += movimientosLegal * 1000/ (movTotales/2)


        solution_fitness1 -= movRandom * 25


        solution_fitness2 += movimientosLegal2 * 1000 / (movTotales/2)

        solution_fitness2 -= movRandom2 * 25

        solution_fitness1 += fichas_capturadas1 * 100
        solution_fitness2 += fichas_capturadas2 * 100

        solution_fitness1 += fichas_casa1 * 100
        solution_fitness2 += fichas_casa2 * 100

        if count % 2 == 0:
            solution_fitness1 += 1000
        else:
            solution_fitness2 += 1000

        print("Fitness1 = ", solution_fitness1)
        print("Fitness2 = ", solution_fitness2)
        (print ("Movimientos legales1 = ", movimientosLegal))
        (print ("Movimientos legales2 = ", movimientosLegal2))
        (print ("Movimientos random1 = ", movRandom))
        (print ("Movimientos random2 = ", movRandom2))
        (print ("Fichas capturadas1 = ", fichas_capturadas1))
        (print ("Fichas capturadas2 = ", fichas_capturadas2))
        (print ("Fichas en casa1 = ", fichas_casa1))
        (print ("Fichas en casa2 = ", fichas_casa2))
        (print ("Movimientos totales = ", movTotales))



        # Simular un juego entre los dos agentes
        # Asignar la aptitud al individuo según su desempeño
        genome1.fitness = solution_fitness1
        genome2.fitness = solution_fitness2

# Configurar el entorno de NEAT
config_file = "./configFile2"
config = neat.Config(neat.DefaultGenome, neat.DefaultReproduction,
                     neat.DefaultSpeciesSet, neat.DefaultStagnation,
                     config_file)

# Crear la población inicial de agentes
pop = neat.Population(config)


# Añadir estadísticas de seguimiento
pop.add_reporter(neat.StdOutReporter(True))
stats = neat.StatisticsReporter()
pop.add_reporter(stats)

neat.Checkpointer(generation_interval=100, filename_prefix='neat-checkpoint-').save_checkpoint(config=config, population=pop.population, species_set=pop.species, generation=pop.generation)


# Entrenar los agentes
winner = pop.run(eval_genomes, n=150)

population = list(pop.population.values())

#guardar la poblacion final en un fichero externo
with open('winner.txt', 'wb') as output:
    pickle.dump(population, output, 1)

with open('winner.txt', 'rb') as input:
    populationcargada = pickle.load(input)



tiempoFinal = time.time()

print("Tiempo total: ", tiempoFinal - tiempoInicial)




# Evaluar el rendimiento del agente ganador
winner_net = neat.nn.FeedForwardNetwork.create(winner, config)
