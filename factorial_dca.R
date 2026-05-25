
library(agricolae)

# ============================================================
#   DISEÑO FACTORIAL 3x3 EN DCA
#   Factor A: Fertilizante nitrogenado (100, 200, 300 kg/ha)
#   Factor B: Especie forestal (Lupuna, Cedro, Caoba)
#   Arreglo: DCA | Réplicas: 6
# ============================================================

# --- 1. Definir factores ---
niveles_A <- c("100 kg/ha", "200 kg/ha", "300 kg/ha")
niveles_B <- c("Lupuna", "Cedro", "Caoba")

# --- 2. Generar diseño ---
diseno_factorial <- design.ab(
  trt    = c(3, 3),   # número de niveles de cada factor
  r      = 6,
  design = "crd",
  seed   = 4721
)

# --- 3. Construir libro de campo ---
libro_factorial <- diseno_factorial$book

libro_factorial$A <- factor(libro_factorial$A,
                            levels = 1:3,
                            labels = niveles_A)

libro_factorial$B <- factor(libro_factorial$B,
                            levels = 1:3,
                            labels = niveles_B)

colnames(libro_factorial) <- c("Parcela", "Replica",
                               "Fertilizante_N_kgha",
                               "Especie_Forestal")

# --- 4. Resultados ---
cat("==============================================\n")
cat("     DISEÑO FACTORIAL 3x3 EN DCA\n")
cat("==============================================\n")
cat("Factor A - Fertilizante N :", paste(niveles_A, collapse = ", "), "\n")
cat("Factor B - Especie forestal:", paste(niveles_B, collapse = ", "), "\n")
cat("Combinaciones (tratamientos):", 3 * 3, "\n")
cat("Réplicas por tratamiento   :", 6, "\n")
cat("Total unidades experimentales:", 3 * 3 * 6, "\n")
cat("----------------------------------------------\n\n")

cat("--- Libro de campo ---\n")
print(libro_factorial)

cat("\n--- Verificación de balance (réplicas por celda) ---\n")
print(table(libro_factorial$Fertilizante_N_kgha,
            libro_factorial$Especie_Forestal))



library(ggplot2)

# Ordenar por parcela y crear posición en grilla (9 columnas x 6 filas)
libro_factorial$orden <- 1:nrow(libro_factorial)
libro_factorial$fila  <- ceiling(libro_factorial$orden / 9)
libro_factorial$col   <- ((libro_factorial$orden - 1) %% 9) + 1

# Etiqueta combinada para cada celda
libro_factorial$tratamiento <- paste0(
  gsub(" kg/ha", "", libro_factorial$Fertilizante_N_kgha), "\n",
  libro_factorial$Especie_Forestal
)

ggplot(libro_factorial, aes(x = col, y = fila,
                            fill = Especie_Forestal,
                            alpha = Fertilizante_N_kgha)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = tratamiento), size = 2.6, lineheight = 0.9) +
  scale_y_reverse(breaks = 1:6, labels = paste("Fila", 1:6)) +
  scale_x_continuous(breaks = 1:9, labels = paste("Col", 1:9)) +
  scale_fill_manual(
    name = "Especie forestal",
    values = c("Lupuna" = "#4CAF50", "Cedro" = "#FF9800", "Caoba" = "#9C27B0")
  ) +
  scale_alpha_manual(
    name = "Fertilizante N",
    values = c("100 kg/ha" = 0.45, "200 kg/ha" = 0.70, "300 kg/ha" = 0.95)
  ) +
  labs(
    title    = "Diagrama de distribución — Diseño Factorial 3×3 en DCA",
    subtitle = "Factor A: Fertilizante N (kg/ha)  |  Factor B: Especie forestal  |  54 UE — 6 réplicas",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    panel.grid    = element_blank(),
    axis.text     = element_text(size = 8, color = "gray50"),
    legend.position = "right"
  )

