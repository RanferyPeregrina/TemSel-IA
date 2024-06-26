library(GA)
Ackley <- function(x1, x2)
{
 -20*exp(-0.2*(x1^2 +x2^2)^0.5) -
exp(0.5*(cos(2*3.14159*x1)+cos(2*3.14159*x2)))+exp(1)+20
}
x1 <- x2 <- seq(-10, 10, by = 0.1)
f <- outer(x1, x2, Ackley)
persp3D(x1, x2, f, theta = 50, phi = 20, col.palette = bl2gr.colors)
filled.contour(x1, x2, f, color.palette = bl2gr.colors)


GA <- ga(type = "real-valued", 
 fitness = function(x) -Ackley(x[1], x[2]),
 lower = c(-10, -10), upper = c(10, 10), 
 popSize = 500, maxiter = 1000, run = 100,
optim = TRUE, optimArgs = list(method = "Nelder-Mead", poptim = 0.05,
pressel = 0.5, control = list(fnscale = -1, maxit = 100))
)

summary(GA)