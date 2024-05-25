library(stringr)
library(ggplot2)


#Estos datos no se cambian.
Tamaño_Poblacion <- 8
Tamaño_Individuo <- 8

# Función para generar CADA individuo
Generar_IndividuosAleatorios <- function(length) {
  # Genera una cadena binaria aleatoria de longitud 'length'
  # La función sample toma muestras aleatorias de los valores 0 y 1 con reemplazo
  # Luego, paste une estos valores en una sola cadena de caracteres
  individuo <- paste(sample(c(0, 1), length, replace = TRUE), collapse = "")
  
  # Verifica si la longitud del individuo generado es mayor a 8 caracteres
  if (nchar(individuo) > 8) {
    # Si la longitud es mayor a 8, recorta la cadena a los primeros 8 caracteres
    individuo <- substr(individuo, 1, 8)
    
    # Imprime un mensaje indicando que la cadena se recortó a 8 caracteres
    print("Se indicó un tamaño de cadena de más de 8 caracteres. Pero eso no es posible")
    print("Se escribirán individuos sólo con los primeros 8 caracteres.")
  }
  return(individuo)
}
Generar_Poblacion <- function(Tamaño_Poblacion, Tamaño_Individuo, Poblacion_Predefinida = NULL) {
  # Si se proporciona una población predefinida, usarla en lugar de generar una nueva
  if (!is.null(Poblacion_Predefinida)) {
    Poblacion <- Poblacion_Predefinida
  } else {
    # Inicializar el contador de repeticiones y el vector de población
    CuentaRepeticion <- 0
    Poblacion <- character(Tamaño_Poblacion)
    Individuos_Generados <- character()
    max_intentos <- 10  # Número máximo de intentos para generar un nuevo individuo único

    # Verificar si el tamaño del individuo es menor que 8 y ajustarlo si es necesario
    if (Tamaño_Individuo < 8) {
      print("Se indicó un tamaño de cadena de menos de 8 caracteres. Pero eso no es posible")
      print("Se escribirán individuos de 8 caracteres.")
      Tamaño_Individuo <- 8
    }

    # Advertencia si el tamaño de la población es demasiado pequeño
    if (Tamaño_Poblacion <= 7) {
      print(" - - - - - - - -¡¡ADVERTENCIA!! - - - - - - -")
      print("La población introducida es tan pequeña que no habrá suficientes individuos para combinar.")
      print("El programa está hecho para funcionar con 8")
      cat("El programa no se detendrá, pero así no debería operar.", "\n\n")
    }

    # Generar la población
    for (i in 1:Tamaño_Poblacion) {
      intentos <- 0
      repeat {
        # Generar un nuevo individuo aleatorio
        nuevo_individuo <- Generar_IndividuosAleatorios(Tamaño_Individuo)
        # Verificar si el nuevo individuo ya existe en la población generada
        if (!(nuevo_individuo %in% Individuos_Generados)) {
          # Si el individuo es único, añadirlo a la población y al registro de generados
          intentos <- intentos - 1
          Poblacion[i] <- nuevo_individuo
          Individuos_Generados <- c(Individuos_Generados, nuevo_individuo)
          break
        } else {
          # Si el individuo ya existe, incrementar el contador de intentos
          intentos <- intentos + 1
          if (intentos >= max_intentos) {
            # Si se alcanzó el número máximo de intentos, permitir la repetición de individuos
            cat("No hay más combinaciones posibles sin repetir los números. Permitida la repetición.\n")
            Poblacion[i] <- nuevo_individuo
            break
          }
        }
      }
    }
  }
  return(Poblacion)
}



Evaluar_Mitad <- function(Mitad) {
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

Evaluar_Individuo <- function(individual) {
  # Convertir el string binario a un vector de enteros
  Vector_Binario <- as.integer(strsplit(individual, "")[[1]])

  # Separar el vector binario en dos mitades
  PrimerMitad <- Vector_Binario[1:(length(Vector_Binario) / 2)]
  SegundaMitad <- Vector_Binario[(length(Vector_Binario) / 2 + 1):length(Vector_Binario)]
  
  # Evaluar la primera mitad
  Calificacion_PrimerMitad <- Evaluar_Mitad(PrimerMitad)
  print(paste("La primer mitad está calificada como:", Calificacion_PrimerMitad))
  
  # Evaluar la segunda mitad
  Calificacion_SegundaMitad <- Evaluar_Mitad(SegundaMitad)
  print(paste("La segunda mitad está calificada como:", Calificacion_SegundaMitad))
  
  # Línea divisoria para claridad en la salida
  print("   -   -   -   -   -   -   -   -   -   -   -")
  
  # Sumar las calificaciones de ambas mitades para obtener la puntuación total del individuo
  PuntuacionTotal <- Calificacion_PrimerMitad + Calificacion_SegundaMitad
  
  return(PuntuacionTotal)
}

Evaluar_Poblacion <- function(Poblacion) {
  # Crear un vector numérico para almacenar las puntuaciones de cada individuo
  Putuaciones <- numeric(length(Poblacion))
    # Iterar sobre cada individuo en la población
  for (i in 1:length(Poblacion)) {
    # Evaluar el individuo y almacenar la puntuación en el vector de puntuaciones
    Putuaciones[i] <- Evaluar_Individuo(Poblacion[i])
  }
    # Imprimir las puntuaciones de todos los individuos
  print(Putuaciones)
    # Devolver el vector de puntuaciones
  return(Putuaciones)
}

Calcular_PuntuacionTotal <- function(Putuaciones) {
  PuntuacionTotal <- sum(Putuaciones)
  return(PuntuacionTotal)
}

Elegir_MejoresIndividuos <- function(Poblacion, Putuaciones, MejorIndividuo_Indice) {
    # Ordenar los índices de los individuos en función de sus puntuaciones, de mayor a menor
  Individuos_Ordenados <- order(Putuaciones, decreasing = TRUE)
    # Seleccionar los mejores individuos utilizando los índices ordenados
  MejoresIndividuos <- Poblacion[Individuos_Ordenados[1:MejorIndividuo_Indice]]
    # Imprimir los mejores individuos seleccionados
  print("Mejores individuos seleccionados:")
  print(MejoresIndividuos)
    # Devolver los mejores individuos
  return(MejoresIndividuos)
}


Cruzar_Poblacion <- function(ind1, ind2) {
    # Calcular el punto medio de los individuos
  midpoint <- nchar(ind1) / 2
    # Crear el primer hijo combinando la primera mitad del primer individuo y la segunda mitad del segundo individuo
  child1 <- paste0(substr(ind1, 1, midpoint), substr(ind2, midpoint + 1, nchar(ind2)))
    # Crear el segundo hijo combinando la primera mitad del segundo individuo y la segunda mitad del primer individuo
  child2 <- paste0(substr(ind2, 1, midpoint), substr(ind1, midpoint + 1, nchar(ind1)))
    # Imprimir los individuos y sus hijos después del cruce
  print(paste("Cruzando:", ind1, "y", ind2))
  print(paste("Resulta en hijos:", child1, "y", child2))
    # Devolver los hijos generados
  return(c(child1, child2))
}
Generar_Nueva_Poblacion <- function(Poblacion, Putuaciones, Tamaño_Poblacion) {
  # Seleccionar los mejores individuos de la población actual
  MejoresIndividuos <- Elegir_MejoresIndividuos(Poblacion, Putuaciones, Tamaño_Poblacion)
  Nueva_Poblacion <- character() # Inicializar la nueva población como un vector vacío
  
  # Bucle para cruzar los mejores individuos
  for (i in seq(1, length(MejoresIndividuos))) {
    for (j in seq(i + 1, length(MejoresIndividuos))) {
      if (length(Nueva_Poblacion) < Tamaño_Poblacion) {
        # Cruzar dos individuos para generar dos hijos
        children <- Cruzar_Poblacion(MejoresIndividuos[i], MejoresIndividuos[j])
        Nueva_Poblacion <- c(Nueva_Poblacion, children[1])
        
        # Añadir el segundo hijo si aún hay espacio en la nueva población
        if (length(Nueva_Poblacion) < Tamaño_Poblacion) {
          Nueva_Poblacion <- c(Nueva_Poblacion, children[2])
        } else {
          break # Romper el bucle si se ha alcanzado el tamaño de población deseado
        }
      }
    }
    if (length(Nueva_Poblacion) >= Tamaño_Poblacion) {
      break # Romper el bucle si se ha alcanzado el tamaño de población deseado
    }
  }
    # Imprimir la nueva población generada
  print("Nueva población generada:")
  print(Nueva_Poblacion)
  
  return(Nueva_Poblacion)
}


Mutar_Bit <- function(individual, position) {
  bits <- unlist(strsplit(individual, ""))
  position <- ((position - 1) %% length(bits)) + 1
  if (bits[position] == "0") {
    bits[position] <- "1"
  } else {
    bits[position] <- "0"
  }
  Mutar_Individual <- paste(bits, collapse = "")
  return(Mutar_Individual)
}

Mutar_Poblacion <- function(Poblacion) {
  for (i in 1:length(Poblacion)) {
    Poblacion[i] <- Mutar_Bit(Poblacion[i], i)

    print(paste("Individuo después de la mutación en posición", i, ":", Poblacion[i]))
  }
  return(Poblacion)
}
Hacer_Todo <- function(Tamaño_Poblacion, Tamaño_Individuo, Limite_Iteraciones, Limite_Iteraciones_NoMejora) {
  # Generar la población inicial
  Poblacion <- Generar_Poblacion(Tamaño_Poblacion, Tamaño_Individuo)
  Mejores_Calificaciones <- c()
  Puntuaciones_Iteraciones <- data.frame(Iteracion = integer(), Puntuacion = numeric())
  Cuenta_NoMejora <- 0
  Iteracion <- 1
  
  # Ciclo de iteraciones del algoritmo genético
  repeat {
    if(Iteracion == 1){
      cat("\n\n")
      print(" ================================================================== ")
      print(" POBLACION INICIAL:               ")
      print(Poblacion)
      # Evaluar la población inicial
      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)
      cat("Puntuación inicial: ", PuntuacionTotal)
      Mejores_Calificaciones <- c(Mejores_Calificaciones, PuntuacionTotal)
      Puntuaciones_Iteraciones <- rbind(Puntuaciones_Iteraciones, data.frame(Iteracion = Iteracion, Puntuacion = PuntuacionTotal))
      cat("\n")
            # Generar nueva población y mutar
      Nueva_Poblacion <- Generar_Nueva_Poblacion(Poblacion, Puntuaciones, Tamaño_Poblacion)
      Poblacion <- Nueva_Poblacion
      cat("\n")
      Poblacion <- Mutar_Poblacion(Poblacion)
            # Evaluar la población después de mutar
      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)
      cat("Puntuación después de mutar: ", PuntuacionTotal, "\n")
    } else {
      cat("\n")
      print("==========================================================================")
      cat("Iteración: ", (Iteracion), "\n")
      print(Poblacion)
            # Evaluar la población actual
      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)
      cat("Puntuación inicial: ", PuntuacionTotal)
      Mejores_Calificaciones <- c(Mejores_Calificaciones, PuntuacionTotal)
      Puntuaciones_Iteraciones <- rbind(Puntuaciones_Iteraciones, data.frame(Iteracion = Iteracion, Puntuacion = PuntuacionTotal))
      cat("\n")
            # Generar nueva población y mutar
      Nueva_Poblacion <- Generar_Nueva_Poblacion(Poblacion, Puntuaciones, Tamaño_Poblacion)
      Poblacion <- Nueva_Poblacion
      cat("\n")
      Poblacion <- Mutar_Poblacion(Poblacion)
            # Evaluar la población después de mutar
      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)
      cat("Puntuación después de mutar: ", PuntuacionTotal, "\n")
        # Comprobar si la puntuación ha mejorado
      if (length(Mejores_Calificaciones) > 1 && Mejores_Calificaciones[length(Mejores_Calificaciones)] == Mejores_Calificaciones[length(Mejores_Calificaciones) - 1]) {
        Cuenta_NoMejora <- Cuenta_NoMejora + 1
      } else {
        Cuenta_NoMejora <- 0
      }
      
      # Romper el bucle si se ha alcanzado el límite de iteraciones o el límite de iteraciones sin mejora
      if (Cuenta_NoMejora >= Limite_Iteraciones_NoMejora || Iteracion >= Limite_Iteraciones) {
        break
      }
    }
    Iteracion <- Iteracion + 1
  }
  # Crear gráfica y guardarla en un objeto
  grafica <- ggplot(Puntuaciones_Iteraciones, aes(x = Iteracion, y = Puntuacion)) +
    geom_line() +
    geom_point() +
    labs(title = "Evolución de la Puntuación en Cada Iteración",
         x = "Iteración",
         y = "Puntuación") +
    theme_minimal()
  # Imprimir la gráfica
  print(grafica)

  retu


#Estos datos no se cambian.
Tamaño_Poblacion <- Tamaño_Poblacion
Tamaño_Individuo <- Tamaño_Individuo

#Estos datos sí se pueden cambiar para hacer pruebas.
Limite_Iteraciones <- 2
Limite_Iteraciones_NoMejora <- 20

Poblacion_Final <- Hacer_Todo(Tamaño_Poblacion, Tamaño_Individuo, Limite_Iteraciones, Limite_Iteraciones_NoMejora)

# Imprimir la población final✨🎉
print(paste("Población final: ", Poblacion_Final))