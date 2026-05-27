
install.packages("agrcolae")
library(agricolade)

# Tratamientos
sustrato <- c("Gallinaza", "Humus", "Compost", "Fibra")

# Número de repeticiones
rep <- 5

# Diseño completamente al azar
diseño <- design.crd(trt = sustrato, r = rep, seed = 123)

diseño$book

#Datos de altura
altura <- c(25,27,26,28,29,   # Gallinaza
            30,32,31,29,33,   # Humus
            22,23,21,24,22,   # Compost
            26,25,27,28,26)   # Fibra

datos <- data.frame(
  sustrato = rep(sustrato, each = 5),
  altura = altura
)

datos
#Analisis de varianza ANOVA
modelo <- aov(altura ~ sustrato, data = datos)
summary(modelo)

#Prueba de comparacion de medias 
tukey <- HSD.test(modelo, "sustrato", group = TRUE)
tukey$groups

#Grafico de medias

bar.group(tukey$groups, ylim = c(0,35),
          ylab = "Altura (cm)",
          xlab = "Sustratos",
          col = "green")
