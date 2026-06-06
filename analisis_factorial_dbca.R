# ============================================================
#   ANÁLISIS DE VARIANZA - DBCA
#   Variables: altp_13s, diam_13s, p_seco_tot, i_Dickson
#   Diseño: Bloques Completos al Azar (DBCA)
#   9 Tratamientos (sustratos) x 4 Bloques
# ============================================================

library(readxl)
library(tidyverse)
library(agricolae)
library(broom)

# ── Carga de datos ─────────────────────────────────────────
df <- read_excel("2026-05-01_BaseDatos_Germinacion.xlsx",
                 sheet = "fb_germinación") |>
  mutate(
    bloq = factor(bloq),
    trat = factor(trat)
  )

# Tabla de sustratos (para unir nombres completos)
sustratos <- tibble(
  Tratamiento = paste0("T", 1:9),
  Sustrato = c(
    "Suelo (Testigo)",      "Suelo+Humus 2:1",
    "Suelo+Humus 3:1",      "Suelo+Arena 2:1",
    "Suelo+Arena 3:1",      "Suelo+Cascarilla 2:1",
    "Suelo+Cascarilla 3:1", "Suelo+Estiercol 2:1",
    "Suelo+Estiercol 3:1"
  )
)

# Etiquetas cortas para gráficos
etiquetas_trat <- c(
  T1 = "Suelo\n(Testigo)", T2 = "S+Humus\n2:1",
  T3 = "S+Humus\n3:1",     T4 = "S+Arena\n2:1",
  T5 = "S+Arena\n3:1",     T6 = "S+Casc.\n2:1",
  T7 = "S+Casc.\n3:1",     T8 = "S+Estierc.\n2:1",
  T9 = "S+Estierc.\n3:1"
)

# ── Función principal de análisis ──────────────────────────
analisis_dbca <- function(variable, titulo_var, unidad) {

  cat("\n")
  cat("══════════════════════════════════════════════════════\n")
  cat("  VARIABLE:", titulo_var, "\n")
  cat("══════════════════════════════════════════════════════\n\n")

  # ── 1. Modelo estadístico ────────────────────────────────
  cat("── 1. MODELO ESTADÍSTICO ──────────────────────────\n")
  cat("  Yij = μ + βi + τj + εij\n")
  cat("  Donde:\n")
  cat("    Yij  = observación del bloque i, tratamiento j\n")
  cat("    μ    = media general\n")
  cat("    βi   = efecto del bloque i   (i = 1, 2, 3, 4)\n")
  cat("    τj   = efecto del tratamiento j (j = 1,...,9)\n")
  cat("    εij  = error experimental ~ N(0, σ²)\n\n")

  formula_modelo <- as.formula(paste(variable, "~ bloq + trat"))
  modelo <- aov(formula_modelo, data = df)

  # ── 2. ANOVA ─────────────────────────────────────────────
  cat("── 2. ANÁLISIS DE VARIANZA ────────────────────────\n")
  anova_tabla <- tidy(modelo) |>
    rename(
      `Fuente de Variación` = term,
      GL          = df,
      SC          = sumsq,
      CM          = meansq,
      `F calc.`   = statistic,
      `P-valor`   = p.value
    ) |>
    mutate(
      `Fuente de Variación` = case_when(
        `Fuente de Variación` == "bloq" ~ "Bloques",
        `Fuente de Variación` == "trat" ~ "Tratamientos",
        TRUE                            ~ "Error"
      ),
      across(where(is.numeric), \(x) round(x, 4)),
      `Sig.` = case_when(
        is.na(`P-valor`)  ~ "",
        `P-valor` < 0.001 ~ "***",
        `P-valor` < 0.01  ~ "**",
        `P-valor` < 0.05  ~ "*",
        TRUE              ~ "ns"
      )
    )

  print(anova_tabla)
  cat("\n  Signif.: *** p<0.001  ** p<0.01  * p<0.05  ns = no significativo\n\n")

  # ── 3. Comparación de medias (Tukey HSD) ─────────────────
  cat("── 3. COMPARACIÓN DE MEDIAS — Prueba de Tukey (HSD)\n\n")
  tukey <- HSD.test(modelo, "trat", group = TRUE, console = FALSE)

  # Medias
  medias_df <- tukey$means |>
    rownames_to_column("Tratamiento") |>
    as_tibble() |>
    mutate(Media = .data[[variable]], DE = std) |>
    dplyr::select(Tratamiento, Media, DE, Min, Max)

  # Grupos (letras)
  grupos_df <- tukey$groups |>
    rownames_to_column("Tratamiento") |>
    dplyr::select(Tratamiento, Grupo = groups)

  tabla_medias <- medias_df |>
    left_join(grupos_df,   by = "Tratamiento") |>
    left_join(sustratos,   by = "Tratamiento") |>
    arrange(desc(Media)) |>
    mutate(across(where(is.numeric), \(x) round(x, 4))) |>
    dplyr::select(Tratamiento, Sustrato, Media, DE, Min, Max, Grupo) |>
    as_tibble()

  print(tabla_medias, n = Inf)
  cat("\n")

  # ── 4. Gráfico ───────────────────────────────────────────
  datos_graf <- tabla_medias |>
    mutate(
      trat_factor = factor(Tratamiento,
                           levels = tabla_medias$Tratamiento)
    )

  letra_y <- max(datos_graf$Media + datos_graf$DE) * 1.07

  p <- ggplot(datos_graf, aes(x = trat_factor, y = Media)) +
    geom_col(fill = "#3E7CB1", width = 0.65) +
    geom_errorbar(
      aes(ymin = Media - DE, ymax = Media + DE),
      width = 0.25, linewidth = 0.7, color = "gray30"
    ) +
    geom_text(
      aes(y = Media + DE + (letra_y - max(Media + DE)) * 0.35,
          label = Grupo),
      size = 4.5, fontface = "bold", color = "gray20"
    ) +
    scale_x_discrete(labels = etiquetas_trat) +
    labs(
      title    = paste("Comparación de medias —", titulo_var),
      subtitle = paste(
        "Prueba de Tukey (HSD) | DBCA",
        "| Letras distintas = diferencias significativas (α = 0.05)"
      ),
      x = "Tratamiento (Sustrato)",
      y = paste0(titulo_var, " (", unidad, ")")
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, color = "gray40"),
      axis.text.x   = element_text(size = 8, lineheight = 0.85)
    )

  print(p)

  invisible(list(modelo    = modelo,
                 anova     = anova_tabla,
                 medias    = tabla_medias,
                 grafico   = p))
}

# ══════════════════════════════════════════════════════════
#   EJECUCIÓN POR VARIABLE
# ══════════════════════════════════════════════════════════

res_altp    <- analisis_dbca("altp_13s",   "Altura a las 13 semanas",      "cm")
res_diam    <- analisis_dbca("diam_13s",   "Diámetro a las 13 semanas",    "mm")
res_pseco   <- analisis_dbca("p_seco_tot", "Peso seco total",               "g")
res_dickson <- analisis_dbca("i_Dickson",  "Índice de Calidad de Dickson", "adim.")
