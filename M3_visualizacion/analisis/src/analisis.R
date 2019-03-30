# Cargando las librerias --------------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot2")
install.packages("tidyr")
library(dplyr)
library(ggplot2)
setwd("../Documents/Trabajo/educon/M3/analisis/src/")

# Lectura de los datos ----------------------------------------------------

hurtos <- read.csv("../input/Hurto_a_personas_2019.csv", encoding = "UTF-8")
poblacion <- read.delim("../input/poblacion.txt", encoding = "UTF-8")

names(hurtos)
names(poblacion)


# Otras funciones ---------------------------------------------------------

select(iris, starts_with("Sepal"))
select(iris, ends_with("Length"))
select(iris, matches("Leng"))

especies <- iris %>% group_by(Species)

# Resumir varias columnas al mismo tiempo

especies %>% summarise_all(n_distinct)
especies %>% summarise_all(mean)

especies %>% summarise_if(is.numeric, mean)
especies %>% summarise_at(vars(Petal.Width), mean)
especies %>% summarise_at(vars(matches("Width")), mean)

especies %>% summarise_at(c("Sepal.Width", "Petal.Width"), mean)
especies %>% summarise_at(c(1, 3), mean)

especies %>% summarise_at(vars(Petal.Width, Sepal.Width), funs(min, max))
especies %>% summarise_at(vars(matches("Width")), funs(min, max))

especies %>% summarise_all(funs(min, max)) %>% View()

especies %>% mutate_all(funs("in" = . / 2.54))

especies %>% summarise_all(funs(med = median))
especies %>% summarise_all(funs(Q3 = quantile), probs = 0.75)
especies %>% summarise_all(c("min", "max"))


# Creación de nuevas variables --------------------------------------------

mutate(iris, inv = 1/Sepal.Length) # Crear una nueva variable
transmute(iris, inv = 1/Sepal.Length) # Sacar una variable nueva creada
mutate(iris, Species = NULL) # Borrar una variable


# Integracion de bases de datos -------------------------------------------


# Volviendo a la base de datos de hurtos ----------------------------------




# Base de datos de poblacion por departamento (Datos de 2018)

poblacion$Departamento <- toupper(poblacion$Departamento)

hurtos_depto <- count(hurtos, Departamento)
hurtos_poblacion <- left_join(hurtos_depto, poblacion, by = "Departamento")
