library(stringr)
library(ggplot2)


#Estos datos no se cambian.
Tamaño_Poblacion <- 8
Tamaño_Individuo <- 8


Generar_IndividuosAleatorios <- function(length) {
  individuo <- paste(sample(c(0, 1), length, replace = TRUE), collapse = "")
  if (nchar(individuo) > 8) {
    individuo <- substr(individuo, 1, 8)
  } 

  return(individuo)
}

Generar_Poblacion <- function(Tamaño_Poblacion, Tamaño_Individuo, Poblacion_Predefinida = NULL) {
  if (!is.null(Poblacion_Predefinida)) {
    Poblacion <- Poblacion_Predefinida
  } else {
    CuentaRepeticion <- 0
    Poblacion <- character(Tamaño_Poblacion)
    Individuos_Generados <- character()
    max_intentos <- 10  

    if (Tamaño_Individuo < 8) {
      Tamaño_Individuo <- 8
    }

    if(Tamaño_Poblacion <= 7){
    }

    for (i in 1:Tamaño_Poblacion) {
      intentos <- 0
      repeat {
        nuevo_individuo <- Generar_IndividuosAleatorios(Tamaño_Individuo)
        if (!(nuevo_individuo %in% Individuos_Generados)) {
          intentos <- intentos - 1
          Poblacion[i] <- nuevo_individuo
          Individuos_Generados <- c(Individuos_Generados, nuevo_individuo)
          break
        } else {
          intentos <- intentos + 1
          if (intentos >= max_intentos) {
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
  Vector_Binario <- as.integer(strsplit(individual, "")[[1]])

  PrimerMitad <- Vector_Binario[1:(length(Vector_Binario) / 2)]
  SegundaMitad <- Vector_Binario[(length(Vector_Binario) / 2 + 1):length(Vector_Binario)]
  
  Calificacion_PrimerMitad <- Evaluar_Mitad(PrimerMitad)
  Calificacion_SegundaMitad <- Evaluar_Mitad(SegundaMitad)
  
  PuntuacionTotal <- Calificacion_PrimerMitad + Calificacion_SegundaMitad #Sumando ambas mitades
  return(PuntuacionTotal)
}

Evaluar_Poblacion <- function(Poblacion) {
  Putuaciones <- numeric(length(Poblacion))
  for (i in 1:length(Poblacion)) {
    Putuaciones[i] <- Evaluar_Individuo(Poblacion[i])
  }
  return(Putuaciones)
}

Calcular_PuntuacionTotal <- function(Putuaciones) {
  PuntuacionTotal <- sum(Putuaciones)
  return(PuntuacionTotal)
}

Elegir_MejoresIndividuos <- function(Poblacion, Putuaciones, MejorIndividuo_Indice) {
  Individuos_Ordenados <- order(Putuaciones, decreasing = TRUE)
  MejoresIndividuos <- Poblacion[Individuos_Ordenados[1:MejorIndividuo_Indice]]
  
  
  return(MejoresIndividuos)
}

Cruzar_Poblacion <- function(ind1, ind2) {
  midpoint <- nchar(ind1) / 2
  child1 <- paste0(substr(ind1, 1, midpoint), substr(ind2, midpoint + 1, nchar(ind2)))
  child2 <- paste0(substr(ind2, 1, midpoint), substr(ind1, midpoint + 1, nchar(ind1)))
   
  return(c(child1, child2))
}

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

  }
  return(Poblacion)
}

Hacer_Todo <- function(Tamaño_Poblacion, Tamaño_Individuo, Limite_Iteraciones, Limite_Iteraciones_NoMejora) {
  Poblacion <- Generar_Poblacion(Tamaño_Poblacion, Tamaño_Individuo)
  Mejores_Calificaciones <- c()
  Puntuaciones_Iteraciones <- data.frame(Iteracion = integer(), Puntuacion = numeric())
  Cuenta_NoMejora <- 0
  Iteracion <- 1
  
  repeat {
    if(Iteracion == 1){
      cat("\n\n")
      print(" ================================================================== ")
      print(" POBLACION INICIAL:               ")

      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)

      Mejores_Calificaciones <- c(Mejores_Calificaciones, PuntuacionTotal)
      Puntuaciones_Iteraciones <- rbind(Puntuaciones_Iteraciones, data.frame(Iteracion = Iteracion, Puntuacion = PuntuacionTotal))
      
      cat("\n")
      Nueva_Poblacion <- Generar_Nueva_Poblacion(Poblacion, Puntuaciones, Tamaño_Poblacion)
      Poblacion <- Nueva_Poblacion
      cat("\n")
      Poblacion <- Mutar_Poblacion(Poblacion)
      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)
      print(Poblacion)
      cat("Puntuación: ", PuntuacionTotal, "\n")
    } else {
      cat("\n")
      print("==========================================================================")
      cat("Iteración: ", (Iteracion), "\n")

      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)

      Mejores_Calificaciones <- c(Mejores_Calificaciones, PuntuacionTotal)
      Puntuaciones_Iteraciones <- rbind(Puntuaciones_Iteraciones, data.frame(Iteracion = Iteracion, Puntuacion = PuntuacionTotal))


      Nueva_Poblacion <- Generar_Nueva_Poblacion(Poblacion, Puntuaciones, Tamaño_Poblacion)
      Poblacion <- Nueva_Poblacion
      cat("\n")
      Poblacion <- Mutar_Poblacion(Poblacion)
      Puntuaciones <- Evaluar_Poblacion(Poblacion)
      PuntuacionTotal <- Calcular_PuntuacionTotal(Puntuaciones)
      print(Poblacion)
      cat("Puntuación: ", PuntuacionTotal, "\n")

      if (length(Mejores_Calificaciones) > 1 && Mejores_Calificaciones[length(Mejores_Calificaciones)] == Mejores_Calificaciones[length(Mejores_Calificaciones) - 1]) {
        Cuenta_NoMejora <- Cuenta_NoMejora + 1
      } else {
        Cuenta_NoMejora <- 0
      }
      
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

  return(Poblacion)
}

#Estos datos no se cambian.
Tamaño_Poblacion <- Tamaño_Poblacion
Tamaño_Individuo <- Tamaño_Individuo

#Estos datos sí se pueden cambiar para hacer pruebas.
Limite_Iteraciones <- 100
Limite_Iteraciones_NoMejora <- 10

Poblacion_Final <- Hacer_Todo(Tamaño_Poblacion, Tamaño_Individuo, Limite_Iteraciones, Limite_Iteraciones_NoMejora)

# Imprimir la población final✨🎉
print(paste("Población final: ", Poblacion_Final))