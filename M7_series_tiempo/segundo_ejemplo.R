serieSales  <-  read.table("Trabajo/educon/M7/rsales.dat",header=T)
attach(serieSales) # utiliza el nombre de las columnas como variables

# La variable RTRR del archivo tiene datos faltantes NA
y = na.omit(RTRR)

# Converir los datos en un objeto tipo ts
y = ts(y,frequency=12,start=c(1955,01))
fechas = seq(as.Date("1955/1/1"), length.out = length(y), by = "months")
ts.plot(y,main="Ventas al menudeo en UDS")

np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.ano = seq(fechas[1],fechas[np],"years")
plot(fechas,y, xaxt="n", panel.first = grid(),type="l",
     ylab="ventas.mes", lwd = 2)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.ano, labels = FALSE, tcl = 0.2)

layout(1:2)
plot(y,main = "Ventas al menudeo en UDS")
plot(log(y),main = "Log(Ventas)")

# Generar datos para los dos períodos

N = length(y)
yi = y[1:(N-12)]
yf = y[(N-12+1):N]

# Ajustar 5 modelos: lineal, cuadrático, cúbico, log-lineal,
# exponencial

t = seq(1:(N-12))
t2 = t^2
t3 = t^3
lyi = log(yi)

mod.lin = lm(yi~t)
mod.cuad = lm(yi~t+t2)
mod.cub = lm(yi~t+t2+t3)
mod.log.lin = lm(lyi~t)


summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)
summary(mod.log.lin)

# El modelo exponencial es no lineal

Ds = data.frame(yi,t)
beta0 = mod.log.lin$coefficient[1]
beta1 = mod.log.lin$coefficient[2]
mod.exp = nls(yi~exp(beta0+beta1*t),data=Ds,
              start=list(beta0=beta0, beta1=beta1)) # Minimos cuadrados NO lineales

summary(mod.exp)

# Calcular AIC y BIC

AIC.tot = c(AIC(mod.lin), AIC(mod.cuad), AIC(mod.cub),
            AIC(mod.log.lin), AIC(mod.exp))
BIC.tot = c(BIC(mod.lin), BIC(mod.cuad), BIC(mod.cub),
            BIC(mod.log.lin), BIC(mod.exp))

summary(mod.cuad)

yest = mod.cuad$fitted.values

plot(t,yi,type='b')
lines(t,yest,type='l',col='blue')

# Nomalidad residuos

r2 = mod.cuad$residuals

layout(1:2)
qqnorm(r2)
qqline(r2,col="red")
hist(r2,40)
shapiro.test(r2)

# Pronósticos

tt=seq((N-12+1),N,1)
tt2 = tt*tt

pr2 = predict(mod.cuad,data.frame(t=tt,t2=tt2))

plot(tt,yf,type='b',xlab='t')
lines(tt,pr2,col='red')
acf(r2,ci.type="ma",60)
