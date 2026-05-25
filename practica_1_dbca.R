# ============================================================================
# ANÁLISIS DE DBCA CON EL PAQUETE AGRICOLAE
# Especie: Cedrelinga cateniformis (Tornillo)
# ============================================================================

# 1. CARGA DE LIBRERÍAS
if (!require("agricolae")) install.packages("agricolae")
library(agricolae)


# Definir tratamientos (niveles del factor "sustrato")
tratamientos <- c("Gallinaza", "Humus_lombriz", "Compost", "Fibra_coco")

# Número de bloques (repeticiones)
bloques <- 4

# Crear el diseño en bloques completamente aleatorizado
diseño <- design.rcbd(trt = tratamientos, r = bloques, seed = 123)

# Ver el diseño de campo
print(diseño$sketch)

# Ver el libro de campo (asignación completa)
book <- diseño$book
print(book)

# Guardar en archivo CSV
write.csv(book, "DBCA_tornillo.csv", row.names = FALSE)

# Convertir a factores
book$block <- as.factor(book$block)
book$tratamientos <- as.factor(book$tratamientos)

# Grafica del esperimento 
plot(book$block, book$trt,
     xlab = "Bloques",
     ylab = "Tratamientos",
     main = "Distribución del DBCA",
     col = "blue",
     pch = 19)


