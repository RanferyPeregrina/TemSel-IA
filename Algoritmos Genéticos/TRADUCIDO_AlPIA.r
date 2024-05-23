# ===============================================================================
## ====================== Aquí se generan los individuos ========================
# ===============================================================================
#
# Paso 1: Definir el tamaño de la población y la longitud de las cadenas binarias
Tamaño_Poblacion <- 7
Tamaño_Individuo <- 8

# Paso 2: Crear una función para generar una cadena binaria aleatoria de longitud 8
Generar_IndividuosAleatorios <- function(length) {
  paste(sample(c(0, 1), length, replace = TRUE), collapse = "")
}

# Paso 3: Crear una población de 7 individuos utilizando la función anterior
Generar_Poblacion <- function(Tamaño_Poblacion, Tamaño_Individuo) {
  Poblacion <- character(Tamaño_Poblacion)
  for (i in 1:Tamaño_Poblacion) {
    Poblacion[i] <- Generar_IndividuosAleatorios(Tamaño_Individuo)
  }
  return(Poblacion)
}

# Generar la población inicial
Poblacion_Inicial <- Generar_Poblacion(Tamaño_Poblacion, Tamaño_Individuo)

# Imprimir la población inicial
print(Poblacion_Inicial)


# ===============================================================================
# ====================== Aquí se evalúan los individuos =========================
# ===============================================================================
#

# Función para evaluar una mitad de una cadena binaria
Evaluar_Mitad <- function(Mitad) {
    #Se inicia en 0 la cuenta de puntuacion, y la de 1s y 0s   
    Cuenta <- 0
    Cuenta_1s <- 0
    Cuenta_0s <- 0

  #Si se detecta 1, se suma 1 🚫
    for (i in 1:length(Mitad)) {
      bit <- Mitad[i]
      
      if (bit == "1") {
        Cuenta <- Cuenta + 1
        Cuenta_1s <- Cuenta_1s + 1
      }
  #Si se detecta 0 se resta 1 🚫
    if (bit == "0") {
      Cuenta <- Cuenta - 1
      Cuenta_0s <- Cuenta_0s + 1
    }
    
    # Si no estamos en el primer caracter
    # Ya podemos comparar con el anterior
    if (i > 1 && Mitad[i - 1] == bit) {
  # Si hay doble "1", se suma 1🚫
      if (bit == "1") {
        Cuenta <- Cuenta + 1
      }
  #Si hay doble "0", se resta 1🚫
      if (bit == "0") {
        Cuenta <- Cuenta - 1
      }
    }
    }


  #Y solo hasta que acabemos de contar a la mitad
  #Y no en cada nuevo bit contado...
  
  #Si hay más 1s que 0s, se suma 1🚫
    if (Cuenta_1s > Cuenta_0s) {
        Cuenta <- Cuenta + 1
  #Si hay más 0s que 1s, se resta 1🚫
    } else if (Cuenta_0s > Cuenta_1s) {
    Cuenta <- Cuenta - 1
    }
  
  return(Cuenta)
}

# Función para evaluar un individuo completo
Evaluar_Individuo <- function(individual) {
  # Convertir el string binario a un vector de enteros
  Vector_Binario <- as.integer(strsplit(individual, "")[[1]])
  
  # Separar en dos mitades--------------------------------------------------
  # La primer mitad es desde el caracter 1, hasta la mitad de la longitud del vector entero
  PrimerMitad <- Vector_Binario[1:(length(Vector_Binario) / 2)]
  # Las egunda mitad es desde el caracter [Mitad del vetor], hasta la longitud completa (El final)
  SegundaMitad <- Vector_Binario[(length(Vector_Binario) / 2 + 1):length(Vector_Binario)]
  
  #califica las dos mitades mitades-------------------------------------------
  Calificacion_PrimerMitad <- Evaluar_Mitad(PrimerMitad)
    print(paste("La primer mitad está calificada como:  ", Calificacion_PrimerMitad))
  Calificacion_SegundaMitad <- Evaluar_Mitad(SegundaMitad)
    print(paste("La segunda mitad está calificada como:  ", Calificacion_SegundaMitad))
  print("   -   -   -   -   -   -   -   -   -   -   -")
  
  #Obtiene la calificación del individuo entero --------------------------------
  PuntuacionTotal <- Calificacion_PrimerMitad + Calificacion_SegundaMitad #Sumando ambas mitades
  return(PuntuacionTotal)
}

# Evaluar toda la población
Evaluar_Poblacion <- function(Poblacion) {
  Putuaciones <- numeric(length(Poblacion))
  for (i in 1:length(Poblacion)) {
    Putuaciones[i] <- Evaluar_Individuo(Poblacion[i])
  }
  print(Putuaciones)
  return(Putuaciones)
}

# Calcular la puntuación total de la población
Calcular_PuntuacionTotal <- function(Putuaciones) {
  PuntuacionTotal <- sum(Putuaciones)
  return(PuntuacionTotal)
}

# Probar la evaluación de la población inicial
Putuaciones <- Evaluar_Poblacion(Poblacion_Inicial)
PuntuacionTotal <- Calcular_PuntuacionTotal(Putuaciones)
print(PuntuacionTotal)

# ===============================================================================
#= ====================== Aquí se cruzan los individuos =========================
# ===============================================================================
#
# Función para seleccionar los mejores individuos
Elegir_MejoresIndividuos <- function(Poblacion, Putuaciones, MejorIndividuo_Indice) {
  Individuos_Ordenados <- order(Putuaciones, decreasing = TRUE)
  MejoresIndividuos <- Poblacion[Individuos_Ordenados[1:MejorIndividuo_Indice]]
  
  # Imprimir los mejores individuos seleccionados
  print("Mejores individuos seleccionados:")
  print(MejoresIndividuos)
  
  return(MejoresIndividuos)
}

# Función para cruzar dos individuos
Cruzar_Poblacion <- function(ind1, ind2) {
  midpoint <- nchar(ind1) / 2
  child1 <- paste0(substr(ind1, 1, midpoint), substr(ind2, midpoint + 1, nchar(ind2)))
  child2 <- paste0(substr(ind2, 1, midpoint), substr(ind1, midpoint + 1, nchar(ind1)))
  
  # Imprimir los individuos y sus hijos después del cruce
  print(paste("Cruzando:", ind1, "y", ind2))
  print(paste("Resulta en hijos:", child1, "y", child2))
  
  return(c(child1, child2))
}

# Generar nueva población mediante cruces
Generar_Nueva_Poblacion <- function(Poblacion, Putuaciones, Tamaño_Poblacion) {
  MejoresIndividuos <- Elegir_MejoresIndividuos(Poblacion, Putuaciones, Tamaño_Poblacion)
  Nueva_Poblacion <- character()
  
  for (i in seq(1, length(MejoresIndividuos))) {
    for (j in seq(i + 1, length(MejoresIndividuos))) {
      if (length(Nueva_Poblacion) < Tamaño_Poblacion) {
        children <- Cruzar_Poblacion(MejoresIndividuos[i], MejoresIndividuos[j])
        Nueva_Poblacion <- c(Nueva_Poblacion, children[1])
        
        if (length(Nueva_Poblacion) < Tamaño_Poblacion) {
          Nueva_Poblacion <- c(Nueva_Poblacion, children[2])
        } else {
          break
        }
      }
    }
    if (length(Nueva_Poblacion) >= Tamaño_Poblacion) {
      break
    }
  }
  
  # Imprimir la nueva población generada
  print("Nueva población generada:")
  print(Nueva_Poblacion)
  
  return(Nueva_Poblacion)
}

Nueva_Poblacion <- Generar_Nueva_Poblacion(Poblacion_Inicial, Putuaciones, length(Poblacion_Inicial))



# ===============================================================================
#= ======================== Aquí se hace la mutación ============================
# ===============================================================================
#
# Función para invertir un bit en una posición específica
Mutar_Bit <- function(individual, position) {
  # Convertir la cadena binaria a una lista de caracteres
  bits <- unlist(strsplit(individual, ""))
  # Invertir el bit en la posición especificada
  if (bits[position] == "0") {
    bits[position] <- "1"
  } else {
    bits[position] <- "0"
  }
  # Unir los bits de nuevo en una cadena binaria
  Mutar_Individual <- paste(bits, collapse = "")
  return(Mutar_Individual)
}

# Función para mutar la población
Mutar_Poblacion <- function(Poblacion) {
  for (i in 1:length(Poblacion)) {
    Poblacion[i] <- Mutar_Bit(Poblacion[i], i)
    # Imprimir el individuo después de la mutación
    print(paste("Individuo después de la mutación en posición", i, ":", Poblacion[i]))
  }
  return(Poblacion)
}

# Aplicar mutación a la nueva población
Poblacion_Mutada <- Mutar_Poblacion(Nueva_Poblacion)

# Imprimir la población mutada
print("Población después de la mutación:")
print(Poblacion_Mutada)

# ===============================================================================
# ======================== Algoritmo Genético Completo =========================
# ===============================================================================

# Aquí toca repetir todo el rollo hasta que ya no haya mejoras significativas en la
# Calificación de la población.
# Que emocionante xd

Hacer_Todo <- function(Tamaño_Poblacion, Tamaño_Individuo, Limite_Iteraciones, Limite_Iteraciones_NoMejora) {
  Poblacion <- Generar_Poblacion(Tamaño_Poblacion, Tamaño_Individuo)
  Mejores_Calificaciones <- c()
  Cuenta_NoMejora <- 0
  Iteracion <- 1
  
  repeat {
    Putuaciones <- Evaluar_Poblacion(Poblacion)
    Mejor_Puntuacion <- max(Putuaciones)
    Mejores_Calificaciones <- c(Mejores_Calificaciones, Mejor_Puntuacion)
    
    cat("Iteración:", Iteracion, "Mejor puntuación:", Mejor_Puntuacion, "\n")
    
    if (length(Mejores_Calificaciones) > 1 && Mejores_Calificaciones[length(Mejores_Calificaciones)] == Mejores_Calificaciones[length(Mejores_Calificaciones) - 1]) {
      Cuenta_NoMejora <- Cuenta_NoMejora + 1
    } else {
      Cuenta_NoMejora <- 0
    }
    
    if (Cuenta_NoMejora >= Limite_Iteraciones_NoMejora || Iteracion >= Limite_Iteraciones) {
      break
    }
    
    Nueva_Poblacion <- Generar_Nueva_Poblacion(Poblacion, Putuaciones, Tamaño_Poblacion)
    Poblacion <- Mutar_Poblacion(Nueva_Poblacion)
    
    Iteracion <- Iteracion + 1
  }
  
  return(Poblacion)
}

# Parámetros del algoritmo
Tamaño_Poblacion <- 7
Tamaño_Individuo <- 8
Limite_Iteraciones <- 100
Limite_Iteraciones_NoMejora <- 10

# Ejecutar el algoritmo genético
Poblacion_Final <- Hacer_Todo(Tamaño_Poblacion, Tamaño_Individuo, Limite_Iteraciones, Limite_Iteraciones_NoMejora)

# Imprimir la población final
print(paste("Población final: ", Poblacion_Final))
