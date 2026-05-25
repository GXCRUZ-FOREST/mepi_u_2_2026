
library(agricolae)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# ============================================================
#   DISEÑO FACTORIAL 4×3 EN DCL (Cuadrado Latino)
#   Factor A: Temperatura     (12, 15, 17, 19 °C)
#   Factor B: Sustrato        (humus, compost, fibra_coco)
#   Combinaciones             : 4 × 3 = 12 tratamientos
#   Cuadrado Latino           : 12 × 12 (144 UE)
# ============================================================


# ── 1. Definir factores ─────────────────────────────────────
niveles_A <- c(12, 15, 17, 19)                       # Factor A: Temperatura
niveles_B <- c("humus", "compost", "fibra_coco")     # Factor B: Sustrato


# ── 2. Combinaciones de tratamientos (factorial cruzado) ────
tratamientos <- as.vector(
  outer(paste0("T", niveles_A), paste0("_", niveles_B), paste0)
)

cat("==============================================\n")
cat("    DISEÑO FACTORIAL 4×3 EN CUADRADO LATINO\n")
cat("==============================================\n")
cat("Factor A - Temperatura  :", paste(niveles_A, collapse = ", "), "°C\n")
cat("Factor B - Sustrato     :", paste(niveles_B, collapse = ", "), "\n")
cat("Combinaciones           :", length(tratamientos), "tratamientos\n")
cat("Dimensión del CL        : 12 × 12\n")
cat("Total unidades exp.     :", 12 * 12, "\n")
cat("----------------------------------------------\n\n")


# ── 3. Generar el Cuadrado Latino ───────────────────────────
set.seed(7641)
diseno_lsd <- design.lsd(trt = tratamientos, serie = 2, seed = 7641)


# ── 4. Construir libro de campo ─────────────────────────────
campo <- diseno_lsd$book

# Renombrar columnas
colnames(campo) <- c("Parcela", "Fila", "Columna", "Tratamiento")

# Extraer factores A y B como columnas separadas
campo$Temperatura <- factor(
  as.numeric(str_extract(campo$Tratamiento, "\\d+")),
  levels = niveles_A
)
campo$Sustrato <- factor(
  str_extract(campo$Tratamiento, "(?<=_).*"),
  levels = niveles_B
)

cat("--- Primeras filas del libro de campo ---\n")
print(head(campo, 24))


# ── 5. Distribución en el cuadrado latino (matriz visual) ───
cat("\n--- Cuadrado Latino: Filas 1-6 × Columnas 1-12 ---\n")
campo |>
  select(Fila, Columna, Tratamiento) |>
  pivot_wider(names_from = Columna,
              values_from = Tratamiento,
              names_prefix = "C") |>
  rename(F = Fila) |>
  print(n = 6, width = 120)


# ── 6. Ingresar datos de respuesta ──────────────────────────
# REEMPLAZAR los datos simulados con tus observaciones reales.
# El orden debe coincidir con el libro de campo (campo$Parcela).
#
# Ejemplo:
#   campo$Respuesta <- c(23.1, 18.4, 25.0, ...)  # 144 valores

set.seed(7641)
campo$Respuesta <- 20 +
  case_when(campo$Temperatura == "12" ~ -4,
            campo$Temperatura == "15" ~  1,
            campo$Temperatura == "17" ~  3,
            campo$Temperatura == "19" ~  2) +
  case_when(campo$Sustrato == "humus"      ~  2,
            campo$Sustrato == "compost"    ~  0,
            campo$Sustrato == "fibra_coco" ~ -1) +
  rnorm(nrow(campo), mean = 0, sd = 1.5)


# ── 7. Modelo ANOVA para factorial en DCL ───────────────────
#
#  Yijk = μ + Fila_i + Col_j + A_k + B_l + (AB)_kl + ε_ijkl
#
modelo <- aov(
  Respuesta ~ factor(Fila) + factor(Columna) + Temperatura * Sustrato,
  data = campo
)

cat("\n==============================================\n")
cat("   ANÁLISIS DE VARIANZA — Factorial 4×3 DCL\n")
cat("==============================================\n")
print(summary(modelo))


# ── 8. Supuestos del modelo ──────────────────────────────────

# Normalidad de residuos (Shapiro-Wilk; muestra si n > 5000)
cat("\n--- Prueba de normalidad (Shapiro-Wilk) ---\n")
print(shapiro.test(residuals(modelo)))

# Homogeneidad de varianzas (Bartlett por factor)
cat("\n--- Homogeneidad de varianzas — Factor A (Temperatura) ---\n")
print(bartlett.test(Respuesta ~ Temperatura, data = campo))

cat("\n--- Homogeneidad de varianzas — Factor B (Sustrato) ---\n")
print(bartlett.test(Respuesta ~ Sustrato, data = campo))


# ── 9. Comparación de medias (Tukey) ────────────────────────

# Solo si el efecto es significativo en el ANOVA
cat("\n--- Tukey: Factor A (Temperatura) ---\n")
print(TukeyHSD(modelo, "Temperatura"))

cat("\n--- Tukey: Factor B (Sustrato) ---\n")
print(TukeyHSD(modelo, "Sustrato"))


# ── 10. Gráfico de interacción A × B ────────────────────────
medias <- campo |>
  group_by(Temperatura, Sustrato) |>
  summarise(Media = mean(Respuesta),
            SE    = sd(Respuesta) / sqrt(n()),
            .groups = "drop")

ggplot(medias, aes(x = Temperatura, y = Media,
                   color = Sustrato, group = Sustrato)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = 0.15, linewidth = 0.7) +
  labs(
    title    = "Factorial 4×3 en Cuadrado Latino",
    subtitle = "Medias (± EE) por Temperatura × Sustrato",
    x        = "Temperatura (°C)",
    y        = "Respuesta media",
    color    = "Sustrato"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )


# ── 11. Diagrama de cajas por tratamiento ───────────────────
ggplot(campo, aes(x = Temperatura, y = Respuesta, fill = Sustrato)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.8) +
  labs(
    title = "Distribución de respuesta por tratamiento",
    x     = "Temperatura (°C)",
    y     = "Respuesta",
    fill  = "Sustrato"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
