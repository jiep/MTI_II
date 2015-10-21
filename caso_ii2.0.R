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
barplot(names.arg = x1, m1, space = 1, xlab="Beneficio (millones de euros)", ylab="Densidad", main="Función de masa para la primera inversión", col = 2)
abline(h=0)
barplot(names.arg = x2, m2, space = 1, xlab="Beneficio (millones de euros)", ylab="Densidad", main="Función de masa para la segunda inversión", col = 3)
abline(h=0)
barplot(names.arg = x3, m3, space = 1, xlab="Beneficio (millones de euros)", ylab="Densidad", main="Función de masa para la tercera inversión", col = 4)
abline(h=0)

# Calculamos la media y la varianza para cada inversión
x1.mean <- x1 %*% m1
x1.var <- sum(m1*(x1-x1.mean)^2)

x2.mean <- x2 %*% m2
x2.var <- sum(m2*(x2-x2.mean)^2)

x3.mean <- x3 %*% m3
x3.var <- sum(m3*(x3-x3.mean)^2)
