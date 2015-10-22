#------------------------------------------------------------------------------
# Tarea IV
#------------------------------------------------------------------------------

# Definimos las varibles para las inversiones

# Primera inversión
x1 <- c( -1, 0,   1, 2, 3, 4,   5, 6)
m1 <- c(0.6, 0, 0.3, 0, 0, 0, 0.1, 0)

# Segunda inversión
x2 <- c( -1, 0,   1, 2,   3, 4, 5, 6)
m2 <- c(0.4, 0, 0.4, 0, 0.2, 0, 0, 0)

# Tercera inversión
# Segunda inversión
x3 <- c( -1,   0, 1, 2, 3, 4, 5,   6)
m3 <- c(0.2, 0.7, 0, 0, 0, 0, 0, 0.1)

# Representamos la función de masa de cada de las inversiones
par(mfrow=c(3,1)) 
barplot(names.arg = x1, m1, space = 1, xlab="Beneficio (millones de euros)", 
  ylab="Densidad", main="Función de masa para la primera inversión", col = 2)
abline(h=0)
barplot(names.arg = x2, m2, space = 1, xlab="Beneficio (millones de euros)", 
  ylab="Densidad", main="Función de masa para la segunda inversión", col = 3)
abline(h=0)
barplot(names.arg = x3, m3, space = 1, xlab="Beneficio (millones de euros)", 
  ylab="Densidad", main="Función de masa para la tercera inversión", col = 4)
abline(h=0)

# Calculamos la media y la varianza para cada inversión
x1.mean <- x1 %*% m1
x1.var <- sum(m1*(x1-x1.mean)^2)

x2.mean <- x2 %*% m2
x2.var <- sum(m2*(x2-x2.mean)^2)

x3.mean <- x3 %*% m3
x3.var <- sum(m3*(x3-x3.mean)^2)

# Dibujamos todos los posibles puntos en el plano
x <- c(-1, -1, -1,  0, 0, 0,  6, 6, 6)
y <- c(-1,  1,  3, -1, 1, 3, -1, 1, 3)
x2<-seq(-1,6)
y2<-seq(-1,6)
plot(x,y, xlim = c(-1,6), ylim = c(-1,6), xlab = "X_3", ylab="X_2")
par(new = TRUE)
plot(x2,y2, xlim = c(-1,6), ylim = c(-1,6), type = "l", xaxt='n', ann=FALSE)
polygon(c(-1,6,6), c(-1,-1,6), col=rgb(0, 191/255, 255/255,0.3), border=NA)
par(new = FALSE)

# Simulamos el coste diario de 1000 llamadas
samples <- rexp(1000, lambda)
cost <- (samples<=1)*(0.1 + 0.6*samples) + (samples>1)*(0.1+0.3*samples)
plot(samples, cost, xlab="Duración de la llamada", ylab = "Coste", 
  main = "Simulación del coste de 1000 llamadas")
mean(cost) # Media
sd(cost)   # Desviación típica
sum(cost)  # Coste total


#------------------------------------------------------------------------------
# Tarea VI
#------------------------------------------------------------------------------

# Definimos los parámetros de la distribución uniforme
a <- 25
b <- 45

# Representamos la función de densidad y la función de distribución
par(mfrow=c(1,2))
curve(dunif(x, a, b), from = a - 5, to = b + 5, main = "Función de densidad",
  ylab = "Densidad", col = 2, lwd=3)
curve(punif(x, a, b), from = a - 5, to = b + 5, main = "Función de distribución",
  ylab = "Probabilidad acumulada", col = 4, lwd=3)

#------------------------------------------------------------------------------
# Tarea VII
#------------------------------------------------------------------------------

# Definimos el parámetro de la distribución exponencial
lambda <- 1/8

# Representamos la función de densidad y de distribución
par(mfrow=c(1,2))
curve(dexp(x, lambda), from = 0, to = 25, main = "Función de densidad",
  ylab = "Densidad", col = 2, lwd = 3)
curve(pexp(x, lambda), from = 0, to = 25, main = "Función de distribución",
  ylab = "Probabilidad acumulada", col = 4, lwd = 3)

# Calculamos el intervalo de probabilidad al 90%
lb <- qexp(0, lambda)
ub <- qexp(0.9, lambda)

# Representamos el intervalo de probabilidad sobre la función de densidad
par(mfrow=c(1,1))
curve(dexp(x, lambda), from = 0, to = 25, main = "Función de densidad e 
  intervalo de probabilidad al 90%",
  ylab = "Densidad", col = 2, lwd = 3)
cords.x <- c(lb, seq(lb, ub, 0.01), ub)
cords.y <- c(0, dexp(seq(lb, ub, 0.01), lambda), 0)
polygon(cords.x, cords.y, col='skyblue', border=NA, add = TRUE)





  
  
  