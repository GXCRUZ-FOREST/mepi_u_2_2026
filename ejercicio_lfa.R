library(tidyverse)
library(agricolae)

# Asumiendo que tu base de datos ya está cargada en el objeto 'fb'
# 1. Asegurar que las variables de diseño sean factores (CRÍTICO)
fb <- fb |> 
  mutate(geno = as.factor(geno), 
         riego = as.factor(riego), 
         bloque = as.factor(bloque))

# 2. Resumen Estadístico: Calcular Medias y Error Estándar (EE)
resumen <- fb |>
  group_by(geno, riego) |>
  summarise(
    media_lfa = mean(lfa, na.rm = TRUE),
    sd_lfa = sd(lfa, na.rm = TRUE),
    n = sum(!is.na(lfa)),
    se_lfa = sd_lfa / sqrt(n), # Cálculo del Error Estándar
    .groups = "drop"
  )

# 3. Modelo ANOVA y Prueba de Tukey para la Interacción
modelo_anova <- aov(lfa ~ bloque + riego * geno, data = fb)
tukey_int <- HSD.test(modelo_anova, c("riego", "geno"), group = TRUE)

# 4. Extracción y Unión de las Letras de Significancia
# agricolae genera los nombres pegados por dos puntos (ej. "irrigado:G01")
letras <- data.frame(
  tratamiento = rownames(tukey_int$groups),
  letra = str_trim(tukey_int$groups$groups)
) |>
  # Separar la columna "tratamiento" en "riego" y "geno" para poder unir los datos
  separate(tratamiento, into = c("riego", "geno"), sep = ":")

# Unir el resumen estadístico con las letras correspondientes
data_grafico <- resumen |>
  left_join(letras, by = c("riego", "geno"))

# 5. Ordenamiento Riguroso (Por el vigor intrínseco en condición irrigada)
orden <- data_grafico |> 
  filter(riego == "irrigado") |> 
  arrange(media_lfa) |> 
  pull(geno)

data_grafico <- data_grafico |> 
  mutate(geno = factor(geno, levels = orden))

# 6. Gráfico de Barras con Error Estándar y Letras de Tukey
ggplot(data_grafico, aes(x = geno, y = media_lfa, fill = riego)) +
  # Barras de media
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black", alpha = 0.85) +
  # Barras de Error Estándar
  geom_errorbar(aes(ymin = media_lfa - se_lfa, ymax = media_lfa + se_lfa),
                position = position_dodge(0.8), width = 0.25, color = "gray20") +
  # Letras de significancia posicionadas sobre la barra de error (+ un margen de 400 para que no choque)
  geom_text(aes(y = media_lfa + se_lfa + 400, label = letra), 
            position = position_dodge(0.8), size = 3.5, fontface = "bold", vjust = 0) +
  scale_fill_manual(values = c("irrigado" = "#4C72B0", "sequia" = "#C44E52")) +
  # Etiquetas formales
  labs(
    x = "Genotipo de Solanum tuberosum",
    y = expression("Área Foliar Media (" * cm^2 * ") ± EE"),
    fill = "Tratamiento Hídrico",
    title = "Efecto del Estrés Hídrico en el Área Foliar por Genotipo",
    subtitle = "Comparación de medias. Las barras de error representan el Error Estándar (EE). \nLetras distintas indican diferencias significativas (Tukey HSD, p < 0.05)."
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )