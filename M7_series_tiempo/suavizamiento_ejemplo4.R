# Suavizamiento exponencial
x <- MSFT #Activos de fondos suizos, Precio y volumen diaro de microsoft y tazas de cambio
xe <- x[1:(nrow(x)-20), "Close"] # PRecios de cierre
xp <- x[(nrow(x)-20+1):nrow(x), "Close"] # PRecios de cierre

alpha <- seq(from = 0 , to = 1, length.out = 100)

error <- array()
for(i in 1:length(alpha)){
  y <-  emaTA(xe, lambda = alpha[i])
  predicciones <- predict(y, 20)
  error[i] <- sum((predicciones$mean - xp)^2 )
}

which.min(error)
alpha[20]

y_fin <- emaTA(xe, lambda = alpha[20])
seriesPlot(xe)
lines(y_fin, col="red")
predicciones <- predict(y_fin, 20)
plot(predicciones)
