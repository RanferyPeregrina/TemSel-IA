def numero_a_binario(numero, tamano_bits):
    # Convertir el número a su representación binaria y quitar el prefijo '0b'
    binario = bin(numero)[2:]
    # Rellenar con ceros a la izquierda para asegurar el tamaño deseado
    binario = binario.zfill(tamano_bits)
    return binario

def separar_numero(numero):
    # Convertir el número a una cadena para manipularlo fácilmente
    numero_str = str(numero)
    # Obtener el signo del número
    signo = numero_str[0] if numero_str[0] in ["-", "+"] else "+"
    # Obtener la parte entera y la parte decimal
    partes = numero_str.lstrip("+-").split(".")
    parte_entera = int(partes[0]) if partes[0] else 0
    parte_decimal = int(partes[1]) if len(partes) > 1 else 0
    return signo, parte_entera, parte_decimal

def convertir_a_binario(signo, parte_entera, parte_decimal):
    # Convertir cada parte a su representación binaria
    bin_signo = "1" if signo == "-" else "0"
    bin_parte_entera = numero_a_binario(parte_entera, 9)  # Se usan 9 bits para la parte entera
    bin_parte_decimal = numero_a_binario(parte_decimal, 11)  # Se usan 11 bits para la parte decimal
    return bin_signo, bin_parte_entera, bin_parte_decimal

def main():
    print("Estoy dedicando 1 bit al signo:")
    print("0 si el número recibido es positivo.")
    print("1 si el bit recibido es negativo.")

    print("\nEstoy dedicando 9 bits a la parte entera")
    print("Para poder representar desde 0, hasta 256")
    print("Y así poder cubrir los: 170")

    print("\nEstoy utilizando 11 bits a la parte decimal.")
    print("Para poder representar hasta 1024")
    print("Y así poder cubrir los posibles: 999 de posible decimal.")
    print("Usando un total de 21 caracteres para representar cada individuo\n")
    
    print("Y cada cromosoma tiene 2 individuos así que serían 42 caracteres por individuo")


    # Pedir al usuario que ingrese un número
    numero = input("Introduce un número: ")
    # Separar el número en sus partes
    signo, parte_entera, parte_decimal = separar_numero(numero)
    # Convertir cada parte a su representación binaria
    bin_signo, bin_parte_entera, bin_parte_decimal = convertir_a_binario(signo, parte_entera, parte_decimal)
    # Juntar todas las partes en una sola cadena
    binario_completo = bin_signo + bin_parte_entera + bin_parte_decimal
    # Mostrar el resultado
    print(f"El número {numero} en binario es: {binario_completo}")

while(True):
    if __name__ == "__main__":
        main()
