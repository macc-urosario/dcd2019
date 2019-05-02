licencias <- read.delim("Trabajo/educon/M7/licencias.txt")
attach(licencias)
yw <-  loess(y ~ time(y))
np <-  length(y)
fecha  <-  seq(as.Date("1986/01/01"), as.Date("2003/06/03"),
            by="months")
fechas  <-  strptime(as.character(fecha), "%Y-%m-%d")
plot(fechas,y, xaxt="n",panel.first = grid(),type="l",ylab="")
axis.POSIXct(1, at=seq(as.Date(fechas[1]),as.Date(fechas[np]),
                       "months"), format="%m/%y")
axis.POSIXct(1, at=seq(as.Date(fechas[1]),as.Date(fechas[np]),
                       "years"), labels = FALSE, tcl = -0.2)

lines(fechas,yw$fitted, xaxt="n", panel.first = grid(),
      type="l",col="blue",lwd=2)

