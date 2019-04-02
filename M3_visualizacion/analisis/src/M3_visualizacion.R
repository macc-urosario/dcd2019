install.packages("reshape")
library(reshape)
library(dplyr)
library(ggplot2)
library(carData)
library(car)
setwd("../Documents/Trabajo/educon/M3/analisis/src/")

# Continuacion ----------------------------------------------------
# Terminando el ejercicio anterior

hurtos <- read.csv("../input/Hurto_a_personas_2019.csv", encoding = "UTF-8")
poblacion <- read.csv("../input/departamentos.csv", sep = ";")
poblacion_genero <- read.delim("../input/poblacion_genero.txt", encoding = "UTF-8")

poblacion$Departamento <- toupper(poblacion$Departamento)
poblacion_genero$Departamento <- toupper(poblacion_genero$Departamento)

hurtos_depto <- count(hurtos, Departamento, Sexo) %>% filter(., Sexo %in% c("MASCULINO", "FEMENINO"))

hurtos_poblacion <- left_join(hurtos_depto, poblacion, by = "Departamento")


poblacion_genero <- melt(poblacion_genero, id = "Departamento")
levels(poblacion_genero$variable) <- c("TOTAL", "MASCULINO", "FEMENINO")

poblacion_genero <- left_join(hurtos_depto, poblacion_genero, by = c("Departamento", "Sexo" = "variable"))

poblacion_genero[, "Tasa"] <- poblacion_genero$n/poblacion_genero$value * 10000 # Por cada diezmil habitantes

# Ejercicio 1: Hacer un gráfico que permita comparar las tasas de hombres y mujeres


# Visualizaciones ---------------------------------------------------------

p <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + 
  geom_point()

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + 
  geom_point() + geom_smooth()

# Guardando los gráficos desde la consola
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + geom_point()
ggsave("../output/mi_grafico.png")

# Facets
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point() + geom_smooth() + 
  facet_grid(~ Species)

p <- p + ggtitle("Petal length and width") + 
  labs(x = "Petal length", 
       y = "Petal width", 
       colour = "Species")

p


# Temas -------------------------------------------------------------------

p + theme_bw() # Tema clasico

p + theme_bw() + 
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.grid.minor = element_line(linetype = "dotted")
  )


tmp <- melt(iris)
ggplot(tmp, aes(x = Species, y = value)) + geom_boxplot() + facet_wrap(~ variable)


p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

p2 <- ggplot(mtcars, aes(x=wt, y=mpg, color=qsec)) + geom_point()

p2+scale_color_gradient(low="blue", high="red")

medio <- mean(mtcars$qsec)
p2+scale_color_gradient2(midpoint = medio, low="blue", mid="white",
                         high="red", space ="Lab" )

# Ejemplo simulado

set.seed(1234)
x <- rnorm(200)
p3 <- qplot(x =x, fill=..count.., geom="histogram") 
p3
p3 + scale_fill_gradient(low="blue", high="red")


# Usando los datos de las naciones unidas ---------------------------------


aggregate(lifeExpF ~ group, data = UN, FUN = median)
aggregate(lifeExpF ~ group, data = UN, FUN = sd)

ggplot(data = UN, mapping = aes(x = group, y = lifeExpF, colour = group)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = UN, mapping = aes(x = lifeExpF, colour = group)) + geom_histogram() + 
  theme_bw() + facet_grid(. ~ group) + theme(legend.position = "none")



# Intervalos y pruebas de hipotesis ---------------------------------------

setosa <- filter(iris, Species == "setosa")
versicolor <- filter(iris, Species == "versicolor")

qqnorm(setosa$Sepal.Length)
qqline(setosa$Sepal.Length)

qqnorm(versicolor$Sepal.Length)
qqline(versicolor$Sepal.Length)

shapiro.test(setosa$Sepal.Length)
shapiro.test(versicolor$Sepal.Length)

t.test(iris$Sepal.Length)
t.test(setosa$Sepal.Length, versicolor$Sepal.Length)


leveneTest(Petal.Length ~ Species, data = iris, center = "median")
