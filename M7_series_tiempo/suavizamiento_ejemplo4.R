# Suavizamiento exponencial
x <- MSFT #Activos de fondos suizos, Precio y volumen diaro de microsoft y tazas de cambio
x <- x[, "Close"] # PRecios de cierre
y <-  emaTA(x, lambda = 0.189)
seriesPlot(x)
lines(y, col="red")

predicciones <- predict(y, 12)
plot(predicciones)