library(ggpubr)
library(ggplot2)
library(MVN)
library(HH)
library(MASS)
library(dplyr)
library(reshape)
library(klaR)

# Graficando los histogramas de todas las variables -----------------------

p1 <- ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_density(aes(colour = Species)) + theme_bw()
p2 <- ggplot(data = iris, aes(x = Sepal.Width)) +
  geom_density(aes(colour = Species)) + theme_bw()
p3 <- ggplot(data = iris, aes(x = Petal.Length)) +
  geom_density(aes(colour = Species)) + theme_bw()
p4 <- ggplot(data = iris, aes(x = Petal.Width)) +
  geom_density(aes(colour = Species)) + theme_bw()
ggarrange(p1, p2, p3, p4, common.legend = TRUE, legend = "bottom")


# Graficando diagramas de puntos para todas las variables -----------------


pairs(x = iris[, -5], col = c("firebrick", "green3", "blue")[iris$Species],
      pch = 20)


# ¿Qué tanto se parece la normal a cada variable? -------------------------

par(mfcol = c(3, 4))
for (k in 1:4) {
  j0 <- names(iris)[k]
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("especie", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}


# Grafico de Normalidad para las variables --------------------------------

for (k in 1:4) {
  j0 <- names(iris)[k]
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1) 
    qqline(x)
  }
}


# Probando normalidad para cada una de las variables ----------------------

datos_tidy <- melt(iris, value.name = "valor")
datos_tidy %>% group_by(Species, variable) %>% summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5))


# Probando normalidad Multivariante ---------------------------------------


outliers <- mvn(data = iris[,-5], mvnTest = "hz", multivariateOutlierMethod = "quan")

royston_test <- mvn(data = iris[,-5], mvnTest = "royston", multivariatePlot = "qq")


# Probando Homogeneidad de las varianzas ----------------------------------

bartlett.test(iris$Sepal.Length ~ iris$Species)
hov(iris$Sepal.Length ~ iris$Species)


# Modelo ------------------------------------------------------------------

modelo_lda <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length +
                    Petal.Width, data = iris)
modelo_lda

predicciones <- predict(object = modelo_lda, newdata = iris[, -5])
table(iris$Species, predicciones$class, dnn = c("Clase real", "Clase predicha"))
trainig_error <- mean(iris$Species != predicciones$class) * 100
paste("trainig_error =", trainig_error, "%")


partimat(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width,
         data = iris, method = "qda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick")

