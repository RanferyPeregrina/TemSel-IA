# ===============================================================================
## ====================== Aquí se generan los individuos ========================
# ===============================================================================
#
# Paso 1: Definir el tamaño de la población y la longitud de las cadenas binarias
population_size <- 7
string_length <- 8

# Paso 2: Crear una función para generar una cadena binaria aleatoria de longitud 8
generate_random_individual <- function(length) {
  paste(sample(c(0, 1), length, replace = TRUE), collapse = "")
}

# Paso 3: Crear una población de 7 individuos utilizando la función anterior
generate_population <- function(pop_size, string_length) {
  population <- character(pop_size)
  for (i in 1:pop_size) {
    population[i] <- generate_random_individual(string_length)
  }
  return(population)
}

# Generar la población inicial
initial_population <- generate_population(population_size, string_length)

# Imprimir la población inicial
print(initial_population)


# ===============================================================================
# ====================== Aquí se evalúan los individuos =========================
# ===============================================================================
#

# Función para evaluar una mitad de una cadena binaria
evaluate_half <- function(half) {
 #Se inicia en 0 la cuenta de puntuacion, y la de 1s y 0s   
  count <- 0
  count_1s <- 0
  count_0s <- 0

  #Si se detecta 1, se suma 1 🚫
    for (i in 1:length(half)) {
      bit <- half[i]
      
      if (bit == "1") {
        count <- count + 1
        count_1s <- count_1s + 1
      }
  #Si se detecta 0 se resta 1 🚫
    if (bit == "0") {
      count <- count - 1
      count_0s <- count_0s + 1
    }
    
    # Si no estamos en el primer caracter
    # Ya podemos comparar con el anterior
    if (i > 1 && half[i - 1] == bit) {
  # Si hay doble "1", se suma 1🚫
      if (bit == "1") {
        count <- count + 1
      }
  #Si hay doble "0", se resta 1🚫
      if (bit == "0") {
        count <- count - 1
      }
    }
    }


  #Y solo hasta que acabemos de contar a la mitad
  #Y no en cada nuevo bit contado...
  
  #Si hay más 1s que 0s, se suma 1🚫
    if (count_1s > count_0s) {
    count <- count + 1
  #Si hay más 0s que 1s, se resta 1🚫
    } else if (count_0s > count_1s) {
    count <- count - 1
    }
  
  return(count)
}

# Función para evaluar un individuo completo
evaluate_individual <- function(individual) {
  # Convertir el string binario a un vector de enteros
  binary_vector <- as.integer(strsplit(individual, "")[[1]])
  
  # Separar en dos mitades--------------------------------------------------
  # La primer mitad es desde el caracter 1, hasta la mitad de la longitud del vector entero
  first_half <- binary_vector[1:(length(binary_vector) / 2)]
  # Lasegunda mitad es desde el caracter [Mitad del vetor], hasta la longitud completa (El final)
  second_half <- binary_vector[(length(binary_vector) / 2 + 1):length(binary_vector)]
  
  #califica las dos mitades mitades-------------------------------------------
  score_first_half <- evaluate_half(first_half)
    print(paste("La primer mitad está calificada como:  ", score_first_half))
  score_second_half <- evaluate_half(second_half)
    print(paste("La segunda mitad está calificada como:  ", score_second_half))
  print("   -   -   -   -   -   -   -   -   -   -   -")
  
  #Obtiene la calificación del individuo entero --------------------------------
  total_score <- score_first_half + score_second_half #Sumando ambas mitades
  return(total_score)
}

# Evaluar toda la población
evaluate_population <- function(population) {
  scores <- numeric(length(population))
  for (i in 1:length(population)) {
    scores[i] <- evaluate_individual(population[i])
  }
  print(scores)
  return(scores)
}

# Calcular la puntuación total de la población
calculate_total_score <- function(scores) {
  total_score <- sum(scores)
  return(total_score)
}

# Probar la evaluación de la población inicial
scores <- evaluate_population(initial_population)
total_score <- calculate_total_score(scores)
print(total_score)

# ===============================================================================
#= ====================== Aquí se cruzan los individuos =========================
# ===============================================================================
#
# Función para seleccionar los mejores individuos
select_best_individuals <- function(population, scores, num_best) {
  sorted_indices <- order(scores, decreasing = TRUE)
  best_individuals <- population[sorted_indices[1:num_best]]
  
  # Imprimir los mejores individuos seleccionados
  print("Mejores individuos seleccionados:")
  print(best_individuals)
  
  return(best_individuals)
}

# Función para cruzar dos individuos
crossover <- function(ind1, ind2) {
  midpoint <- nchar(ind1) / 2
  child1 <- paste0(substr(ind1, 1, midpoint), substr(ind2, midpoint + 1, nchar(ind2)))
  child2 <- paste0(substr(ind2, 1, midpoint), substr(ind1, midpoint + 1, nchar(ind1)))
  
  # Imprimir los individuos y sus hijos después del cruce
  print(paste("Cruzando:", ind1, "y", ind2))
  print(paste("Resulta en hijos:", child1, "y", child2))
  
  return(c(child1, child2))
}

# Generar nueva población mediante cruces
generate_new_population <- function(population, scores, population_size) {
  best_individuals <- select_best_individuals(population, scores, population_size)
  new_population <- character()
  
  for (i in seq(1, length(best_individuals))) {
    for (j in seq(i + 1, length(best_individuals))) {
      if (length(new_population) < population_size) {
        children <- crossover(best_individuals[i], best_individuals[j])
        new_population <- c(new_population, children[1])
        
        if (length(new_population) < population_size) {
          new_population <- c(new_population, children[2])
        } else {
          break
        }
      }
    }
    if (length(new_population) >= population_size) {
      break
    }
  }
  
  # Imprimir la nueva población generada
  print("Nueva población generada:")
  print(new_population)
  
  return(new_population)
}

new_population <- generate_new_population(initial_population, scores, length(initial_population))



# ===============================================================================
#= ======================== Aquí se hace la mutación ============================
# ===============================================================================
#
# Función para invertir un bit en una posición específica
mutate_bit <- function(individual, position) {
  # Convertir la cadena binaria a una lista de caracteres
  bits <- unlist(strsplit(individual, ""))
  # Invertir el bit en la posición especificada
  if (bits[position] == "0") {
    bits[position] <- "1"
  } else {
    bits[position] <- "0"
  }
  # Unir los bits de nuevo en una cadena binaria
  mutated_individual <- paste(bits, collapse = "")
  return(mutated_individual)
}

# Función para mutar la población
mutate_population <- function(population) {
  for (i in 1:length(population)) {
    population[i] <- mutate_bit(population[i], i)
    # Imprimir el individuo después de la mutación
    print(paste("Individuo después de la mutación en posición", i, ":", population[i]))
  }
  return(population)
}

# Aplicar mutación a la nueva población
mutated_population <- mutate_population(new_population)

# Imprimir la población mutada
print("Población después de la mutación:")
print(mutated_population)

# ===============================================================================
# ======================== Algoritmo Genético Completo =========================
# ===============================================================================

# Aquí toca repetir todo el rollo hasta que ya no haya mejoras significativas en la
# Calificación de la población.
# Que emocionante xd

run_genetic_algorithm <- function(population_size, string_length, max_iterations, no_improvement_threshold) {
  population <- generate_population(population_size, string_length)
  best_scores <- c()
  no_improvement_count <- 0
  iteration <- 1
  
  repeat {
    scores <- evaluate_population(population)
    best_score <- max(scores)
    best_scores <- c(best_scores, best_score)
    
    cat("Iteración:", iteration, "Mejor puntuación:", best_score, "\n")
    
    if (length(best_scores) > 1 && best_scores[length(best_scores)] == best_scores[length(best_scores) - 1]) {
      no_improvement_count <- no_improvement_count + 1
    } else {
      no_improvement_count <- 0
    }
    
    if (no_improvement_count >= no_improvement_threshold || iteration >= max_iterations) {
      break
    }
    
    new_population <- generate_new_population(population, scores, population_size)
    population <- mutate_population(new_population)
    
    iteration <- iteration + 1
  }
  
  return(population)
}

# Parámetros del algoritmo
population_size <- 7
string_length <- 8
max_iterations <- 100
no_improvement_threshold <- 10

# Ejecutar el algoritmo genético
final_population <- run_genetic_algorithm(population_size, string_length, max_iterations, no_improvement_threshold)

# Imprimir la población final
print("Población final:")
print(final_population)