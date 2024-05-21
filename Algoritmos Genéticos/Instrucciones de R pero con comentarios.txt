# Aquí importamos la librería GA
library(GA)

# Creamos una función matemática llamada Ackley -----------------------------
Ackley <- function(x1, x2)
{
 -20*exp(-0.2*(x1^2 +x2^2)^0.5) -
exp(0.5*(cos(2*3.14159*x1)+cos(2*3.14159*x2)))+exp(1)+20
}
# Genera una secuencia de números en el rango de -10 a 10 con un paso de 0.1
x1 <- x2 <- seq(-10, 10, by = 0.1)

# Calcula los valores de la función de Ackley para todas las combinaciones posibles de x1 y x2
f <- outer(x1, x2, Ackley)

# Crea una representación tridimensional de la función de Ackley
persp3D(x1, x2, f, theta = 50, phi = 20, col.palette = bl2gr.colors)

# Crea un gráfico de contorno relleno de la función de Ackley
filled.contour(x1, x2, f, color.palette = bl2gr.colors)



# Aquí empoioeza la parte del algoriutmo genético -----------------------------
# Ejecuta un algoritmo genético para optimizar la función de Ackley
GA <- ga(
  # Tipo de algoritmo genético: se utiliza para valores reales
  type = "real-valued", 
# Función de aptitud: se define como la negación de la función de Ackley para minimizarla
  fitness = function(x) -Ackley(x[1], x[2]), 
# Límites inferiores y superiores para las variables de decisión
  lower = c(-10, -10), upper = c(10, 10), 
# Tamaño de la población: 500 individuos
  popSize = 500, 
# Número máximo de iteraciones: 1000 generaciones
  maxiter = 1000, 
# Número de ejecuciones: 100 para mejorar la robustez del algoritmo
  run = 100,
# Indica que se realizará una optimización adicional después del algoritmo genético
  optim = TRUE, 
# Argumentos de optimización: se utiliza el método "Nelder-Mead" con una probabilidad de actualización de 0.05
  optimArgs = list(
    method = "Nelder-Mead", 
    poptim = 0.05,
# Parámetro de selección de presión: 0.5
    pressel = 0.5, 
# Control de argumentos para el método de optimización
    control = list(
# Escala de la función: se invierte para minimizar la función
      fnscale = -1, 
# Número máximo de iteraciones para el método de optimización interna
      maxit = 100
    )
  )
)

#Imprimir el resumen de todo este rollo:
summary(GA)

