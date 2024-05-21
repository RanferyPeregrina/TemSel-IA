# Este programa califica las cadenas de texto siguiendo reglas específicas
# Si hay un 1 en la cadena:             Se suma 1
# Si hay 2 1s seguidos en la cadena:    Se suma 1
# Si hay más 1s que 0s en la cadena:    Se suma 1
#Si hay un 0 en la cadena:              Se resta 1
#Si hay 2 0s seguidos en la cadena:     Se resta 1
#Si hay más 1s que 0s en la cadena:     Se resta 1

def PreguntarImpresion():
    print(" ================================ ")
    while True:
        print("Por motivos de depuración: ¿Quiere imprimir? ")
        print("1.- Sí")
        print("2.- No")
        Imprimir = int(input("Respuesta:  "))

        if Imprimir == 1:
            return True
        elif Imprimir == 2:
            return False
        
        print(" ================================ ")

def ValidarCadena(Cadena, ImpresionPermitida):
    if len(Cadena) != 8:
        if ImpresionPermitida == True:
            print(f"Los números ingresados no son 8. ")

        if len(Cadena) != 4:
            print("Ni son 4")
            print("Vuelva a intentarlo...")
            print("\n")
            return False
        elif len(Cadena) == 4:
            if ImpresionPermitida == True:
                print("Pero son 4.")
    
    for i in range(len(Cadena)):
        if Cadena[i] != "0" and Cadena[i] != "1":
            print("Los datos introducidos no son un string binario")
            print("Asegurate de que sólo sean '1' y '0' sin espacios")
            print(f"El caracter numero {i+1} es un: {Cadena[i]}")
            print("Vuelva a intentarlo")
            print("\n")
            return False
        else:
            print("\n")
            return True

def CalificarCadena(Cadena, ImpresionPermitida):
    Cuenta = 0
    Contador1s = 0
    Contador0s = 0
    for Bit in range (len(Cadena)):
  
        if ImpresionPermitida == True:
            print()
            print(f"En el espacio {Bit + 1} el bit es: {Cadena[Bit]}")

        if Cadena[Bit] == "1":
            Cuenta += 1
            Contador1s += 1
            if ImpresionPermitida == True:
                print("1 detectado: Se suma 1")
                print(f"Cuenta: {Cuenta}")

        if Cadena[Bit]== "0":
            Cuenta = Cuenta - 1
            Contador0s += 1
            if ImpresionPermitida == True:
                print("0 detectado: Se resta 1")
                print(f"Cuenta: {Cuenta}")

        if Bit != 0:
            if Cadena[Bit - 1] == Cadena[Bit]:
                if Cadena[Bit] == "1":
                    Cuenta += 1
                    if ImpresionPermitida == True:
                        print("1 doble: Se suma 1")
                        print(f"Cuenta: {Cuenta}")

                elif Cadena[Bit] == "0":
                    Cuenta -= 1
                    if ImpresionPermitida == True:
                        print("0 doble: Se resta 1")
                        print(f"Cuenta: {Cuenta}")

        if (Bit + 1) == len(Cadena):
            if Contador1s > Contador0s:
                if ImpresionPermitida == True:
                    print()
                    print("Y hay más 1s que 0s")
                    print("Se suma 1")
                Cuenta += 1

            elif Contador0s > Contador1s:
                if ImpresionPermitida == True:
                    print("Y hay más 0s que 1s")
                    print("Se resta 1")
                Cuenta -= 1
    if ImpresionPermitida == True:
        print("")
        print(f"Para la cadena {Cadena}: La calificación es: {Cuenta}")
        print("--------------------------------------------------------")
    return Cuenta

def PedirCadena(ImpresionPermitida):
    while(True):
        print("------------------------------------")
        Cadena = input("Ingrese su cadena de bits:  ")
        if(ValidarCadena(Cadena, ImpresionPermitida)) == True:
            return Cadena;

def main():
    ImpresionPermitida = PreguntarImpresion()

    Cantidad_Individuos = int(input("Ingrese la cantidad de individuos:  "))
    Individuos = []
    Calificacion_Individuos = []

    for i in range(Cantidad_Individuos):
        Individuos.append(PedirCadena(ImpresionPermitida))
        Calificacion_Individuos.append(CalificarCadena(Individuos[i], ImpresionPermitida))

    Imprimir = input("¿Imprimir calificaciones? Sí/No:  ")
    if Imprimir == "Si":
        for i in range(Cantidad_Individuos):
            print(f"Calificación del individuo {i + 1}: {Calificacion_Individuos[i]}")


ImpresionPermitida = PreguntarImpresion()
Cadena = PedirCadena(ImpresionPermitida)
Calificacion = CalificarCadena(Cadena, ImpresionPermitida)

print(f"Cadena {Cadena} calificada como: {Calificacion}")