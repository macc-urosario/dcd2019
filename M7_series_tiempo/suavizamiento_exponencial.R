library(fTrading)
library(zoo)
library(forecast)
library(tseries)
library(ggplot2)


x <- 1:30
t <- 2*x
s <- 12*sin(2*x/pi)
e <- 4*rnorm(30)
y <- t + s + e

datos <- data.frame(fenomeno = y,
                    tendencia = t,
                    estacionalidad = s,
                    error = e,
                    tiempo = x)


alpha = 0.6
beta = 0.6

n2 <- y[2]
t2 <- y[2]-y[1]

n3 <- alpha*y[3]+(1-alpha)*(n2+t2)
t3 <- beta*(n3-n2) + (1-beta)*t2
y3_e <- n3 + t3
error = y3_e - y[3]


predicciones <- array(y[1], y[2])
nia <- y[2]
tia <- y[2]-y[1]
for(i in 1:length(y)){
  ni <- alpha*y[i]+(1-alpha)*(nia+tia)
  ti <- beta*(ni-nia) + (1-beta)*t2
  yi_e <- ni + ti
  print(yi_e - y[i])
  nia <- ni
  tia <- ti
  predicciones[i] <- yi_e
}

plot(y, type = "l")
lines(predicciones, col = "blue")  


# Algortimo basado en el nivel  ----------------------------------------


activos <- MSFT #Activos de fondos suizos, Precio y volumen diaro de microsoft y tazas de cambio
activos_train <- activos[1:(nrow(activos)-20), "Close"] # PRecios de cierre
activos_test <- activos[(nrow(activos)-20+1):nrow(activos), "Close"] # PRecios de cierre

activos_ajustados <-  emaTA(activos_train, lambda = 0.189)

seriesPlot(activos_train)
lines(activos_ajustados, col="red")

predicciones <- predict(activos_ajustados, 20)

sum((predicciones$mean - activos_test)^2)

plot(predicciones)


# Algoritmo basado en nivel y tendencia -----------------------------------


serie <- co2
plot(serie)
fit <- stl(serie, s.window="period")
plot(fit)
monthplot(serie)
seasonplot(serie)


# Ajuste de los modelos de prediccion
# Ajustando sólamente el nivel de la serie
m <- HoltWinters(serie, beta = FALSE, gamma = FALSE)
plot(m)
# Ajustando el nivel y la tendencia
m1 <- HoltWinters(serie, gamma = FALSE)
plot(m1)
# Ajustando el nivel, la tendencia y la estacionalidad
m2 <- HoltWinters(serie)
plot(m2)

# Predecir valores futuros
pred <- forecast(m, 12)
plot(pred)
accuracy(pred)

