# Seleccionamos el archivo
filename <- file.choose()

# Cargamos el archivo como CSV
data <- read.csv(filename,sep=" ", header=TRUE)

# Seleccionamos la variable x
x <- data$x

#------------------------------------------------------------------------------
# Tarea I
#------------------------------------------------------------------------------

# Definimos un array con las categorías
categories <- c("EXCELENTE", "BUENA", "MALA")

# Contamos el número de datos
n <- length(x)

# Definimos un vector para contar el número de datos de cada categoria
categories_count <- c(0,0,0)

# Recorremos el array
for(i in 1:n) {
  if(x[i] <= 2){ # Si la categia es EXCELENTE, aumentamos en 1 la cuenta
    categories_count[1] <- categories_count[1] + 1
  }else if(x[i] == 3 | x[i] == 4){ # Si la categia es BUENA, aumentamos en 1 la cuenta
    categories_count[2] <- categories_count[2] + 1
  }else if(x[i] >= 5){ # Si la categia es MALA, aumentamos en 1 la cuenta
    categories_count[3] <- categories_count[3] + 1
  }
}

# Calculamos los porcentajes de cada categoría
categories_percentage <- categories_count/n*100

# Contamos la frecuencia de cada valor
frequencies <- table(x)

# Calculamos la probabilidad de un cliente más de 7 intentos para conectarse 
# a la web P(#intentos > 7) = 1 - P(#intentos <= 6) = 1 - [ P(#intentos = 1) 
# + P(#intentos = 2) + P(#intentos = 3) + P(#intentos = 4) + P(#intentos = 5) 
# + P(#intentos = 6) ]
1 - sum(table(x)/n)

#------------------------------------------------------------------------------
# Tarea II
#------------------------------------------------------------------------------

# Definimos la distribución normal con paramétros media = 250 y desviación 
# típica 60

mean <- 250
sd <- 60

# Calculamos los los percentiles 100/3 y 200/3
p1 <- qnorm(1/3, 250, 60)
p2 <- qnorm(2/3, 250, 60)

# Dibujamos la distribución
curve(xlim=c(mean-6*sd, mean+6*sd), dnorm(x,mean,sd), lwd=0, main="Función de 
    densidad la variable W = \"número de KB transmitidos \" ~ N(250, 60)", xlab="Tráfico 
    transmitido (KB)", ylab="Probabilidad")

# Dibujamos las regiones "Tráfico bajo", "Tráfico medio" y "Tráfico alto"
cords1.x <- c(mean-6*sd, seq(mean-6*sd, p1, 0.01), p1)
cords1.y <- c(0, dnorm(seq(mean-6*sd, p1, 0.01), mean, sd), 0)
polygon(cords1.x,cords1.y,col='green', border=NA)

cords2.x <- c(p1, seq(p1, p2, 0.01), p2)
cords2.y <- c(0, dnorm(seq(p1, p2, 0.01), mean, sd), 0)
polygon(cords2.x,cords2.y,col='orange', border=NA)

cords3.x <- c(p2, seq(p2, mean+6*sd, 0.01), mean+6*sd)
cords3.y <- c(0, dnorm(seq(p2, mean+6*sd, 0.01), mean, sd), 0)
polygon(cords3.x,cords3.y,col='red', border=NA)

# Comprobamos si efectivamente las zonas tienen un tercio de probabilidad cada
# una. Para ello, simulamos 100000 muestras de nuestra distribución y contamos
# cuántas están cada de las zonas, y calculamos el porcentaje de cada ellas.

n_samples = 100000
samples <- rnorm(n_samples, mean, sd)
samples_BAJO = sum(samples <= p1)
samples_MEDIO = sum(samples > p1 & samples < p2)
samples_ALTO = sum(samples >= p2)

samples_percentages <- c(samples_BAJO, samples_MEDIO, samples_ALTO)/n_samples*100

# Probabilidad de un cliente tenga una conexión MALA y haya generado un tráfico
# BAJO

# P(MALA ∩ BAJO) = P(MALA) * P(BAJO) puesto que dice el enunciado que son 
# independientes

# P(MALA ∩ BAJO) = P(MALA) * P(BAJO) = 0.04* P(W <= p1 = 224.15)

(categories_percentage[3]/100) * pnorm(p1, mean, sd)

# Probabilidad de que, de 10 clientes seleccionados al azar, al menos 9 de ellos 
# tengan conexión BUENA o EXCELENTE

# Primero calculamos la probabilidad de que se tenga una conexión BUENA o EXCELENTE.
# Es decir, P(BUENA ∪ EXCELENTE) = P(BUENA) + P(EXCELENTE) - P(BUENA ∩ EXCELENTE) 
# = P(BUENA) + P(EXCELENTE), puesto que tener conexión BUENA y tener conexión 
# EXCELENTE son sucesos disjuntos.

# P(BUENA ∪ EXCELENTE) = P(BUENA) + P(EXCELENTE)
p <- (categories_percentage[2]/100) + (categories_percentage[1]/100)

# Una vez que hemos calculado esta probabilidad, calculamos la probabilidad de que al 
# menos 9 de 10 clientes elegidos al azar tengan una conexión BUENA o EXCELENTE

# En este caso tenemos una distribución binomial con n = 10 y p = 0.96, calculada
# anteriormente

# P(número de clientes con conexión BUENA o EXCELENTE >= 9) 
# = 1 - P(número de clientes con conexión BUENA o EXCELENTE < 9) = 
# = 1 - P(número de clientes con conexión BUENA o EXCELENTE <= 8)

1 - pbinom(8, 10, p)

# Probabilidad de que, de 10 clientes seleccionados al azar, no haya ninguno 
# que tenga conexión EXCELENTE y haya generado un tráfico BAJO.

# Calculamos la probabilidad de que haya un cliente que tenga conexión EXCELENTE
# y haya generado un tráfico BAJO.

# P(EXCELENTE ∩ BAJO) = P(EXCELENTE) * P(BAJO), puesto que, según el enunciado, 
# los sucesos son independientes.

# P(EXCELENTE ∩ BAJO) = P(EXCELENTE) * P(BAJO) = 0.7 * P(W <= p1 = 224.15)

q <- (categories_percentage[1]/100) * pnorm(p1, mean, sd)

# Falta calcular que de 10 clientes elegidos al azar, no haya ninguno que tenga
# conexión EXCELENTE y haya generado un tráfico BAJO

# De nuevo, tenemos una distribución binomial con n = 10, p = 0.2333333, 
# calculada anteriormente. Hay que calcular
# P(número de clientes tenga conexión EXCELENTE y haya generado tráfico BAJO = 0) 

dbinom(0, 10, q)


#------------------------------------------------------------------------------
# Tarea III
#------------------------------------------------------------------------------

# Definimos las variables con las probabilidades de los cuestionario y los
# que no han sido contestados

C1 = 0.4
C2 = 0.3
C3 = 0.2
C4 = 0.1
C1_NC = 0.01
C2_NC = 0.02
C3_NC = 0.07
C4_NC = 0.04

# Probabilidad de que se haya contestado al cuestionario, si se elige un
# cliente al azar

# Aplicando el teorema de la probabilidad total, la probabilidad pedida es

C1 * (1 - C1_NC) + C2 * (1 - C2_NC) + C3 * (1 - C3_NC) + C4 * (1 - C4_NC)

# Si un cliente no ha contestado a su cuestionario, ¿qué cuestionario es más 
# probable que hubiese recibido?

# Calculamos la probabilidad de que no contestar a un cuestionario

q2 <- C1 * C1_NC + C2 * C2_NC + C3 * C3_NC + C4 * C4_NC

# Aplicamos el teorema de Bayes para cada uno de los cuestionarios.
# P(C_i | NC) = P(NC | C_i) * P(C_i) / P(NC)

c <- c(C1, C2, C3, C4)
c_NC <- c(C1_NC, C2_NC, C3_NC, C4_NC)

bayes <- c*c_NC/q2

# Probabilidad de que al menos haya #un cuestionario sin contestar, si tomamos
# 4 clientes al azar
  
# La probabilidad de no contestar un cuestionario es:

q2 <- C1 * C1_NC + C2 * C2_NC + C3 * C3_NC + C4 * C4_NC

# Tenemos de nuevo una distribución binomial con n = 4, p = 0.028

# P(Cuestionarios sin contestar >= 1) = 1 - P(Cuestionarios sin contestar < 1) = 
# P(Cuestionarios sin contestar = 0)

1 - dbinom(0, 4, q2)

# Si cambiamos 4 por 200, la probabilidad es

1 - dbinom(0, 200, q2)