import sys
from itertools import combinations

#Archivo donde voy a guardar todas las cosas para que quepan.
Archivo = open("ImpresionCompleta.txt", "w")

# Redirige la salida estándar a este archivo jeje
sys.stdout = Archivo

# Definir los individuos y sus tiempos de cruce
Lista_Individuos = {
    'A': 1,
    'B': 2,
    'C': 5,
    'D': 10
}

# Estado inicial: todos están en el lado izquierdo del puente
Estado_Inicial = {
    'Izquierda': set(Lista_Individuos.keys()),
    'Derecha': set(),
    'EstadoLampara': 'Izquierda'
}

# Imprimir el estado inicial
def Imprimir_Estado(Estado, time):
    Lado_Izquierdo = ', '.join(sorted(Estado['Izquierda']))
    Lado_Derecho = ', '.join(sorted(Estado['Derecha']))
    EstadoLampara_Actual = Estado['EstadoLampara']
    print(f"Tiempo: {time} minutos")
    print(f"Se encuentran de lado izquierdo: {Lado_Izquierdo}")
    print(f"Se encuentran de lado derecho: : {Lado_Derecho}")
    print(f"El que trae la lámpara está en la: {EstadoLampara_Actual}")


# Generar los movimientos posibles desde el estado actual
def Generar_Estado(Estado):
    Lista_cruces = []
    if Estado['EstadoLampara'] == 'Izquierda':
        # Generar combinaciones de 1 o 2 personas cruzando de izquierda a derecha
        for Combinacion_Actual in combinations(Estado['Izquierda'], 2):
            Lista_cruces.append(Combinacion_Actual)
        for Individuo in Estado['Izquierda']:
            Lista_cruces.append((Individuo,))
    else:
        # Generar combinaciones de 1 persona regresando de derecha a izquierda
        for Individuo in Estado['Derecha']:
            Lista_cruces.append((Individuo,))
    print("Posibles cruces en este Estado::")
    for Cruce in Lista_cruces:
        print(Cruce)   
    return Lista_cruces


def Calcular_Tiempo(Cruce):
    return max(Lista_Individuos[p] for p in Cruce)

def Encontrar_Soluciones(Estado, Tiempo_Actual, Tiempo_Limite, Ruta, Soluciones):
    Imprimir_Estado(Estado, Tiempo_Actual)
    if Tiempo_Actual > Tiempo_Limite:
        print()
        print("Se excedió el tiempo límite")
        print("Esta solución NO es viable.")
        print("No autorizo")
        print('-' * 40)

        return
    if len(Estado['Derecha']) == 4:
        print("Todos los individuos cruzaron exitosamente ")
        print("Esta solución sí es viable.")
        print('= ' * 40)
        Soluciones.append(Ruta)
        return

    Lista_cruces = Generar_Estado(Estado)
    for Cruce in Lista_cruces:
        Estado_nuevo = {
            'Izquierda': Estado['Izquierda'].copy(),
            'Derecha': Estado['Derecha'].copy(),
            'EstadoLampara': 'Derecha' if Estado['EstadoLampara'] == 'Izquierda' else 'Izquierda'
        }

        if Estado['EstadoLampara'] == 'Izquierda':
            Estado_nuevo['Izquierda'].difference_update(Cruce)
            Estado_nuevo['Derecha'].update(Cruce)
        else:
            Estado_nuevo['Derecha'].difference_update(Cruce)
            Estado_nuevo['Izquierda'].update(Cruce)

        Tiempo_Usado = Calcular_Tiempo(Cruce)
        Ruta_Nueva = Ruta + [(Cruce, Tiempo_Usado)]
        print(f"Regresa {Cruce} con la lámpara, lo que le toma {Tiempo_Usado} minutos.")
        Encontrar_Soluciones(Estado_nuevo, Tiempo_Actual + Tiempo_Usado, Tiempo_Limite, Ruta_Nueva, Soluciones)

# Probar la función de exploración de soluciones
Soluciones = []

print("== " * 30)
print("Hay 4 individuos que tienen que cruzar un puente.")
print("Cada uno tarda un tiempo diferente en cruzar.")
print("'A' tarda 1")
print("'B' tarda 2")
print("'C' tarda 5")
print("'D' tarda 10")
print()
print("Tienen que cruzar todos, pero siempre debe haber uno en el puente con una linterna.")
print("Sin embargo, puede haber un máximo de 2 personas en el puente al mismo tiempo.")
print()
print("Por lo que es conveniente que crucen 2, pero regrese sólo uno con la linterna.")
print("... Aunque nada impide que regresen dos.") 
print("... Ni que cruce sólo uno.")
print("Aunque estos movimientos son completamente inútiles, también forman parte de los operadores posibles")
print("Y todos los operadores posibles están dentro del espacio de soluciones.")
print()
print("El tiempo de cruzar de cada pareja es el tiempo del individuo que tarde más en cruzar.")
print()
print("El objetivo del programa es hayar la combinación correcta de cruces para que todos logren llegar al otro lado")
print("En un límite de 17 minutos.")
print("\n")
print("Las soluciones viables están al final, aunque también pueden encontrarse entre el espacio de solución impreso.")
print("Entre el montón de soluciones que no funcionan del espacio.")
print("A continuación, las impresiones de todas las soluciones posibles en el espacio de soluciones:")
print("== " * 30)

print("\n         Estado inicial")
Encontrar_Soluciones(Estado_Inicial, 0, 17, [], Soluciones)
print((" =  ") * 20)
print("Soluciones viables encontradas:")
for Solucion in Soluciones:
    print(Solucion)
sys.stdout = sys.__stdout__

print("Todas las soluciones fueron generadas en un archivo que se llama 'ImpresionCompleta' en este mismo directorio")
print("Porque si las ponía en consola no iban a caber.")
print("Son muchas soluciones.")


