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

