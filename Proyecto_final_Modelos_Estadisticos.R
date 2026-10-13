# Proyecto Final - Modelos Estadísticos
# Autores:
# 1. Alexis Alberto Zúñiga Alonso
# 2. Baruc Azael Ramírez Romo
# Profesor: Axel Torres
# Institución: EdgeHub | School of Innovation
# Fecha: Lunes 12 de octubre de 2026
#
#########################################################################
# Descripción:
# Considerando la base de datos `Wine.csv` realizar los siguientes
# grandes apartados:
# 1. Limpieza de datos:
#    a. Comprensión y descripción con sus palabras de los tipos
#       de variables y susregistros en las bases de datos.
#    b. Enfoque del análisis estadístico a realizar (exploratorio o predictivo).
#    c. Detección de variables con registros vacíos (NAs) o con duplicados.
#    d. Procedimiento para tratar estos casos en el caso que se presenten.
# 2. Análisis Exploratorio de Datos
#    a. Medidas de tendencia central, dispersión, outliers, distribución
#       para aquellas variables que consideren
#       pertinentes para lo que vayan a hacer en el punto 3.
# 3. Inferencia estadística y/o prueba de desempeño de al menos un modelo
#    estadístico visto en el curso.
#    a. Descripción de las pruebas y/o los modelos a utilizar con sus datos.
#    b. Principales resultados obtenidos con las pruebas y/o modelos empleados.
#    c. Agregar en el script el comando set.seed() para que yo pueda reproducir
#       sus resultados en cualquier momento que consulte el archivo.
#    d. Interpretación de los mismos en el contexto inicial.
#
#########################################################################
# Entregables:
# 1. Script de R con el cual ustedes hicieron el análisis de las bases de datos
#    (limpieza, análisis exploratorio, inferencias y modelos empleados)
# 2. Documento en formato PDF, donde me expliquen todo lo que les solicito
#    de información.
#
#########################################################################
# 0. Preparación del entorno

# Instala (si faltan) y carga los paquetes; MASS y nnet se llaman con `::` para no enmascarar dplyr::select
libs <- c("tidyverse", "car", "rstatix", "ggpubr", "caret", "patchwork")
faltantes <- libs[!libs %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(c(faltantes, "MASS", "nnet"))
invisible(lapply(libs, library, character.only = TRUE))

# Fija el directorio de trabajo en la carpeta del script (RStudio/Positron); si no, usar la raíz del proyecto
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Carpeta de salida para figuras y función auxiliar para guardarlas en PNG
dir.create("plots", showWarnings = FALSE)
guardar_plot <- function(p, nombre, w = 8, h = 5) {
  ggsave(file.path("plots", nombre), p, width = w, height = h, dpi = 300, bg = "white")
}

# Colores fijos por pueblo (paleta apta para daltonismo) y tema base de las gráficas
colores_pueblo <- c("Pueblo 1" = "#2a78d6", "Pueblo 2" = "#eb6834", "Pueblo 3" = "#1baf7a")
theme_set(
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
          plot.title.position = "plot", legend.position = "top")
)

#########################################################################
# 1. Limpieza de datos

# ---- 1.a Comprensión de las variables y sus registros ----

# Carga de datos crudos y vistazo a la estructura (178 muestras x 14 variables)
wine_raw <- read_csv("data/Wine.csv", show_col_types = FALSE)
glimpse(wine_raw)

# Diccionario de datos: significado, unidad (propuesta) y tipo estadístico de cada variable
diccionario <- tribble(
  ~Variable,              ~Descripcion,                                            ~Unidad,          ~Tipo,
  "Alcohol",              "Contenido de alcohol",                                  "% vol",          "Cuantitativa continua",
  "Malic_Acid",           "Ácido málico (acidez, notas a manzana verde)",          "g/L",            "Cuantitativa continua",
  "Ash",                  "Cenizas: residuo mineral tras la combustión",           "g/L",            "Cuantitativa continua",
  "Ash_Alcanity",         "Alcalinidad de las cenizas",                            "meq/L",          "Cuantitativa continua",
  "Magnesium",            "Magnesio",                                              "mg/L",           "Cuantitativa discreta",
  "Total_Phenols",        "Fenoles totales (compuestos antioxidantes)",            "g/L",            "Cuantitativa continua",
  "Flavanoids",           "Flavonoides (fenoles de sabor y amargor)",              "g/L",            "Cuantitativa continua",
  "Nonflavanoid_Phenols", "Fenoles no flavonoides",                                "g/L",            "Cuantitativa continua",
  "Proanthocyanins",      "Proantocianidinas (taninos, astringencia)",             "g/L",            "Cuantitativa continua",
  "Color_Intensity",      "Intensidad de color",                                   "absorbancia",    "Cuantitativa continua",
  "Hue",                  "Tonalidad del color",                                   "razón",          "Cuantitativa continua",
  "OD280",                "Razón OD280/OD315 (contenido de proteínas/fenoles)",    "razón",          "Cuantitativa continua",
  "Proline",              "Concentración de prolina (aminoácido)",                 "mg/L",           "Cuantitativa discreta",
  "Customer_Segment",     "Pueblo de origen del vino (1, 2 o 3)",                  "—",              "Cualitativa nominal"
)
print(diccionario, n = Inf)

# Clase con la que R leyó cada variable y número de valores distintos (Customer_Segment solo tiene 3)
tibble(Variable = names(wine_raw),
       Clase_R  = map_chr(wine_raw, class),
       Valores_distintos = map_int(wine_raw, n_distinct))

# Número de muestras por pueblo (clases moderadamente desbalanceadas: 59, 71 y 48)
count(wine_raw, Customer_Segment)

p_muestras <- wine_raw %>%
  count(Pueblo = paste("Pueblo", Customer_Segment)) %>%
  ggplot(aes(Pueblo, n, fill = Pueblo)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = colores_pueblo) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Número de muestras de vino por pueblo", x = NULL, y = "Muestras")
p_muestras
guardar_plot(p_muestras, "01_muestras_por_pueblo.png", w = 6, h = 4)

# ---- 1.b Enfoque del análisis ----
# Enfoque mixto: exploratorio (sección 2) para describir las variables, e inferencial/predictivo
# (sección 3) para comparar pueblos, modelar la prolina y clasificar el pueblo de origen.

# ---- 1.c Detección de NAs, duplicados y valores anómalos ----

# Conteo de NAs por variable
colSums(is.na(wine_raw))

# Filas completamente duplicadas
sum(duplicated(wine_raw))

# Valores negativos (imposibles en concentraciones químicas)
colSums(wine_raw < 0, na.rm = TRUE)

# Valores con más de 3 decimales: artefactos de formato (p. ej. Color_Intensity = 9.899999)
wine_raw %>%
  mutate(fila = row_number()) %>%
  pivot_longer(-fila, names_to = "Variable", values_to = "Valor") %>%
  filter(abs(Valor * 1000 - round(Valor * 1000)) > 1e-6)

# ---- 1.d Tratamiento de los casos detectados ----

# Pipeline de limpieza: aplica cada paso solo si el problema existe, así sirve para futuras versiones del CSV
wine <- wine_raw %>%
  mutate(across(everything(), ~ round(.x, 3))) %>%                         # elimina artefactos de redondeo
  filter(!is.na(Customer_Segment)) %>%                                     # sin pueblo no se puede imputar
  mutate(Pueblo = factor(Customer_Segment, levels = 1:3, labels = names(colores_pueblo))) %>%
  select(-Customer_Segment) %>%
  group_by(Pueblo) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, median(.x, na.rm = TRUE)))) %>%  # NA -> mediana del pueblo
  ungroup() %>%
  mutate(across(c(Magnesium, Proline), as.integer)) %>%                    # variables de conteo a entero
  distinct()                                                               # elimina filas duplicadas

# Verificación: sin NAs ni duplicados y mismas dimensiones que el original
stopifnot(sum(is.na(wine)) == 0, !any(duplicated(wine)))
dim(wine)

# Exporta la base limpia para el análisis exploratorio y el documento
write_csv(wine, "data/Wine_clean.csv")

#########################################################################
# 2. Análisis Exploratorio de Datos

# El análisis se realiza sobre las 13 variables químicas de la base limpia.
# Se estudian medidas de tendencia central, dispersión, valores atípicos y
# distribución. Además, se presentan resultados por pueblo para relacionar
# esta sección con las pruebas y modelos de la sección 3.

# Carpeta para guardar las tablas obtenidas
dir.create("resultados", showWarnings = FALSE)

# Convertir las variables numéricas a formato largo facilita resumirlas
# y representarlas con el mismo código.
datos_largos <- wine %>%
  pivot_longer(
    cols = -Pueblo,
    names_to = "Variable",
    values_to = "Valor"
  )

# ---- 2.1 Medidas de tendencia central y dispersión ----

# La media y la mediana representan la tendencia central.
# La desviación estándar, varianza, IQR y rango representan la dispersión.
tabla_descriptiva <- datos_largos %>%
  group_by(Variable) %>%
  summarise(
    N = n(),
    Media = mean(Valor),
    Mediana = median(Valor),
    Desviacion_estandar = sd(Valor),
    Varianza = var(Valor),
    Minimo = min(Valor),
    Primer_cuartil = quantile(Valor, 0.25),
    Tercer_cuartil = quantile(Valor, 0.75),
    Maximo = max(Valor),
    IQR = IQR(Valor),
    Rango = max(Valor) - min(Valor),
    Coeficiente_variacion = 100 * sd(Valor) / mean(Valor),
    .groups = "drop"
  ) %>%
  arrange(Variable)

# Tabla redondeada para mostrar en consola y utilizar en el reporte
tabla_descriptiva_reporte <- tabla_descriptiva %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(tabla_descriptiva_reporte, n = Inf)
write_csv(tabla_descriptiva_reporte,
          "resultados/tabla_descriptiva_general.csv")

# Resumen de todas las variables por pueblo. Permite observar diferencias
# iniciales antes de aplicar ANOVA, Kruskal-Wallis y los modelos predictivos.
tabla_por_pueblo <- datos_largos %>%
  group_by(Pueblo, Variable) %>%
  summarise(
    N = n(),
    Media = mean(Valor),
    Mediana = median(Valor),
    Desviacion_estandar = sd(Valor),
    IQR = IQR(Valor),
    Minimo = min(Valor),
    Maximo = max(Valor),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(tabla_por_pueblo, n = Inf)
write_csv(tabla_por_pueblo,
          "resultados/tabla_descriptiva_por_pueblo.csv")

# ---- 2.2 Detección de valores atípicos ----

# Se usa el criterio de Tukey: un valor es atípico si se encuentra por debajo
# de Q1 - 1.5*IQR o por encima de Q3 + 1.5*IQR.
tabla_outliers <- datos_largos %>%
  group_by(Variable) %>%
  summarise(
    Q1 = quantile(Valor, 0.25),
    Q3 = quantile(Valor, 0.75),
    IQR = IQR(Valor),
    Limite_inferior = Q1 - 1.5 * IQR,
    Limite_superior = Q3 + 1.5 * IQR,
    Cantidad_outliers = sum(
      Valor < Limite_inferior | Valor > Limite_superior
    ),
    Porcentaje_outliers = 100 * Cantidad_outliers / n(),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(Cantidad_outliers))

print(tabla_outliers, n = Inf)
write_csv(tabla_outliers,
          "resultados/tabla_valores_atipicos.csv")

# Se identifican también las observaciones específicas consideradas atípicas.
detalle_outliers <- datos_largos %>%
  group_by(Variable) %>%
  mutate(
    Q1 = quantile(Valor, 0.25),
    Q3 = quantile(Valor, 0.75),
    RIC = IQR(Valor),
    Limite_inferior = Q1 - 1.5 * RIC,
    Limite_superior = Q3 + 1.5 * RIC
  ) %>%
  filter(Valor < Limite_inferior | Valor > Limite_superior) %>%
  select(Pueblo, Variable, Valor, Limite_inferior, Limite_superior) %>%
  ungroup()

print(detalle_outliers, n = Inf)
write_csv(detalle_outliers,
          "resultados/detalle_valores_atipicos.csv")

# Los valores atípicos no se eliminan automáticamente: son mediciones posibles
# del perfil químico y podrían ayudar a distinguir el pueblo de origen. Solo se
# eliminarían si se comprobara que provienen de un error de captura o medición.

# ---- 2.3 Distribución de las variables ----

# Shapiro-Wilk complementa la inspección gráfica. H0: la variable presenta
# una distribución normal. Un p-valor menor a 0.05 sugiere no normalidad.
tabla_distribucion <- datos_largos %>%
  group_by(Variable) %>%
  summarise(
    Asimetria = mean(
      ((Valor - mean(Valor)) / sd(Valor))^3
    ),
    Shapiro_W = unname(shapiro.test(Valor)$statistic),
    Shapiro_p = shapiro.test(Valor)$p.value,
    Conclusion = if_else(
      Shapiro_p < 0.05,
      "No presenta normalidad",
      "Compatible con normalidad"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Asimetria = round(Asimetria, 3),
    Shapiro_W = round(Shapiro_W, 3),
    Shapiro_p = signif(Shapiro_p, 4)
  )

print(tabla_distribucion, n = Inf)
write_csv(tabla_distribucion,
          "resultados/tabla_distribuciones.csv")

# Histogramas y curvas de densidad de las 13 variables químicas.
p_distribuciones <- ggplot(datos_largos, aes(Valor)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 18,
    fill = "#8fc2e8",
    color = "white",
    linewidth = 0.25
  ) +
  geom_density(color = "#174f86", linewidth = 0.8) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  labs(
    title = "Distribución de las variables químicas del vino",
    subtitle = "Histogramas y curvas de densidad",
    x = "Valor observado",
    y = "Densidad"
  ) +
  theme(
    axis.text = element_text(size = 7),
    strip.text = element_text(size = 9, face = "bold")
  )

p_distribuciones
guardar_plot(
  p_distribuciones,
  "09_distribuciones_variables.png",
  w = 12,
  h = 10
)

# ---- 2.4 Comparación de dispersión y outliers ----

# Como las variables tienen unidades diferentes, se estandarizan para poder
# compararlas en una misma gráfica de cajas.
datos_estandarizados <- wine %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.x)))) %>%
  pivot_longer(
    cols = -Pueblo,
    names_to = "Variable",
    values_to = "Valor_estandarizado"
  )

p_boxplots <- ggplot(
  datos_estandarizados,
  aes(x = reorder(Variable, Valor_estandarizado, FUN = median),
      y = Valor_estandarizado)
) +
  geom_boxplot(
    fill = "#a9d6f5",
    color = "#174f86",
    outlier.color = "#d73027",
    outlier.size = 1.8
  ) +
  coord_flip() +
  labs(
    title = "Dispersión y valores atípicos de las variables",
    subtitle = "Valores estandarizados; los puntos rojos son posibles outliers",
    x = NULL,
    y = "Valor estandarizado"
  )

p_boxplots
guardar_plot(
  p_boxplots,
  "10_boxplots_variables.png",
  w = 8,
  h = 7
)

# ---- 2.5 Correlaciones entre variables ----

# La correlación permite explorar asociaciones lineales y anticipar posibles
# problemas de multicolinealidad en la regresión del punto 3.
matriz_correlaciones <- wine %>%
  select(-Pueblo) %>%
  cor(use = "complete.obs")

print(round(matriz_correlaciones, 3))
write_csv(
  as.data.frame(matriz_correlaciones) %>% rownames_to_column("Variable"),
  "resultados/matriz_correlaciones.csv"
)

correlaciones_largas <- as.data.frame(
  as.table(matriz_correlaciones)
) %>%
  rename(
    Variable_1 = Var1,
    Variable_2 = Var2,
    Correlacion = Freq
  )

p_correlaciones <- ggplot(
  correlaciones_largas,
  aes(Variable_1, Variable_2, fill = Correlacion)
) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "#b2182b",
    mid = "white",
    high = "#2166ac",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlación"
  ) +
  coord_equal() +
  labs(
    title = "Matriz de correlaciones del perfil químico",
    subtitle = "Correlación de Pearson entre las variables numéricas",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

p_correlaciones
guardar_plot(
  p_correlaciones,
  "11_matriz_correlaciones.png",
  w = 9,
  h = 8
)

# ---- 2.6 Variables principales para los análisis del punto 3 ----

# Se destacan Proline y cinco variables relacionadas con la diferenciación
# y predicción de los pueblos: Alcohol, Flavanoids, Color_Intensity, OD280
# y Total_Phenols.
variables_clave <- c(
  "Proline",
  "Alcohol",
  "Flavanoids",
  "Color_Intensity",
  "OD280",
  "Total_Phenols"
)

p_variables_clave <- datos_largos %>%
  filter(Variable %in% variables_clave) %>%
  ggplot(aes(Pueblo, Valor, fill = Pueblo)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = colores_pueblo, guide = "none") +
  labs(
    title = "Variables químicas principales por pueblo",
    subtitle = "Comparación exploratoria previa a las pruebas y modelos",
    x = NULL,
    y = "Valor observado"
  )

p_variables_clave
guardar_plot(
  p_variables_clave,
  "12_variables_clave_por_pueblo.png",
  w = 10,
  h = 7
)

# Interpretación exploratoria:
# - La media y la mediana permiten reconocer el valor central de cada compuesto.
# - La desviación estándar y el IQR muestran diferencias importantes de escala
#   y variabilidad, especialmente en Magnesium y Proline.
# - El criterio de Tukey identifica posibles valores atípicos en algunas
#   variables, pero se conservan porque pueden ser observaciones válidas.
# - Varias distribuciones no son normales, lo cual justifica complementar el
#   ANOVA con pruebas robustas y no paramétricas en la sección 3.
# - Proline, Alcohol, Flavanoids, Color_Intensity, OD280 y Total_Phenols muestran
#   diferencias visibles entre pueblos y son pertinentes para la inferencia,
#   la regresión y la clasificación realizadas posteriormente.

#########################################################################
# 3. Inferencia estadística y modelos

# Semilla para reproducir particiones, validación cruzada y modelos
set.seed(2026)

# ---- 3.a Descripción de las pruebas y modelos ----
# 3.1 ANOVA de una vía (y alternativas robustas) para comparar la prolina media entre pueblos.
# 3.2 Kruskal-Wallis por variable para identificar qué compuestos distinguen a los pueblos.
# 3.3 Regresión lineal múltiple para predecir la prolina a partir del resto de compuestos químicos.
# 3.4 Regresión logística multinomial para clasificar el pueblo de origen de una muestra.

# ---- 3.1 Prolina por pueblo: ANOVA y verificación de supuestos ----

# Media y desviación estándar de prolina por pueblo
wine %>% group_by(Pueblo) %>% get_summary_stats(Proline, type = "mean_sd")

# ANOVA clásico; H0: la prolina media es igual en los tres pueblos
anova_prolina <- aov(Proline ~ Pueblo, data = wine)
summary(anova_prolina)

# Supuestos: normalidad de residuos (Shapiro-Wilk) y homogeneidad de varianzas (Levene)
shapiro.test(residuals(anova_prolina))
leveneTest(Proline ~ Pueblo, data = wine)

# Diagnóstico visual de normalidad de los residuos del ANOVA
p_qq_anova <- tibble(residuo = residuals(anova_prolina)) %>%
  ggplot(aes(sample = residuo)) +
  stat_qq(color = "#2a78d6", alpha = 0.7, size = 2) +
  stat_qq_line(linewidth = 0.8) +
  labs(title = "Q-Q de residuos del ANOVA (prolina ~ pueblo)",
       x = "Cuantiles teóricos", y = "Cuantiles de los residuos")
p_qq_anova
guardar_plot(p_qq_anova, "02_anova_qq_residuos.png", w = 6, h = 5)

# Alternativas robustas ante violación de supuestos: ANOVA de Welch y Kruskal-Wallis (+ tamaño de efecto)
welch_anova_test(wine, Proline ~ Pueblo)
kruskal_test(wine, Proline ~ Pueblo)
kruskal_effsize(wine, Proline ~ Pueblo)

# Comparaciones por pares con Games-Howell (no asume varianzas iguales)
gh_prolina <- games_howell_test(wine, Proline ~ Pueblo) %>% mutate(p_lab = scales::pvalue(p.adj, add_p = TRUE))
gh_prolina

# Boxplot de prolina por pueblo con los p-valores ajustados de Games-Howell
p_prolina <- ggplot(wine, aes(Pueblo, Proline, color = Pueblo)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, linewidth = 0.7) +
  geom_jitter(position = position_jitter(width = 0.12, seed = 2026), alpha = 0.5, size = 1.8) +
  stat_pvalue_manual(gh_prolina, label = "p_lab", y.position = c(1750, 1850, 1950), size = 3.5) +
  scale_color_manual(values = colores_pueblo, guide = "none") +
  labs(title = "Concentración de prolina por pueblo",
       subtitle = "Comparaciones por pares: prueba de Games-Howell",
       x = NULL, y = "Prolina (mg/L)")
p_prolina
guardar_plot(p_prolina, "03_prolina_por_pueblo.png", w = 7, h = 5)

# ---- 3.2 ¿Qué compuestos distinguen a los pueblos? Kruskal-Wallis por variable ----

# Kruskal-Wallis y eta² (η²[H]) para cada variable química, con p-valores ajustados por Bonferroni
kw_variables <- wine %>%
  pivot_longer(-Pueblo, names_to = "Variable", values_to = "Valor") %>%
  group_by(Variable) %>%
  summarise(H = kruskal.test(Valor ~ Pueblo)$statistic,
            p = kruskal.test(Valor ~ Pueblo)$p.value,
            eta2_H = kruskal_effsize(pick(everything()), Valor ~ Pueblo)$effsize) %>%
  mutate(p_ajustado = p.adjust(p, method = "bonferroni")) %>%
  arrange(desc(eta2_H))
kw_variables

# Tamaño de efecto por variable: mientras mayor η²[H], más diferencia entre pueblos
p_kw <- ggplot(kw_variables, aes(eta2_H, fct_reorder(Variable, eta2_H))) +
  geom_segment(aes(x = 0, xend = eta2_H, yend = Variable), color = "#2a78d6", linewidth = 1) +
  geom_point(color = "#2a78d6", size = 3) +
  geom_vline(xintercept = 0.14, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.15, y = 1.5, label = "Efecto grande (≥ 0.14)", hjust = 0, size = 3.5, color = "grey30") +
  scale_x_continuous(limits = c(0, 0.85), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Diferencia entre pueblos por compuesto químico",
       subtitle = "Tamaño de efecto η²[H] de la prueba de Kruskal-Wallis",
       x = "η²[H]", y = NULL)
p_kw
guardar_plot(p_kw, "04_kruskal_efecto_por_variable.png", w = 7, h = 5)

# ---- 3.3 Regresión lineal múltiple: predicción de la prolina ----

# Partición estratificada 70/30 por pueblo (la misma se reutiliza en 3.4)
idx_train <- createDataPartition(wine$Pueblo, p = 0.7, list = FALSE)
train <- wine[idx_train, ]
test  <- wine[-idx_train, ]

# Modelo completo con los 12 compuestos (sin el pueblo) y selección de variables por AIC en ambos sentidos
lm_completo <- lm(Proline ~ . - Pueblo, data = train)
lm_prolina  <- MASS::stepAIC(lm_completo, direction = "both", trace = FALSE)
summary(lm_prolina)

# Supuestos: multicolinealidad (VIF > 5 preocupante), heterocedasticidad (Breusch-Pagan) y normalidad de residuos
vif(lm_prolina)
ncvTest(lm_prolina)
shapiro.test(residuals(lm_prolina))

# Diagnóstico de residuos: residuos vs ajustados y Q-Q
diag_lm <- tibble(ajustado = fitted(lm_prolina), residuo = residuals(lm_prolina))
p_diag_lm <- (
  ggplot(diag_lm, aes(ajustado, residuo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(color = "#2a78d6", alpha = 0.7, size = 2) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "grey20", linewidth = 0.8) +
    labs(title = "Residuos vs ajustados", x = "Prolina ajustada (mg/L)", y = "Residuo")
) + (
  ggplot(diag_lm, aes(sample = residuo)) +
    stat_qq(color = "#2a78d6", alpha = 0.7, size = 2) +
    stat_qq_line(linewidth = 0.8) +
    labs(title = "Q-Q de residuos", x = "Cuantiles teóricos", y = "Cuantiles de los residuos")
) + plot_annotation(title = "Diagnóstico de la regresión lineal múltiple (prolina)",
                    theme = theme(plot.title = element_text(face = "bold")))
p_diag_lm
guardar_plot(p_diag_lm, "05_regresion_diagnostico.png", w = 10, h = 4.5)

# Desempeño en el conjunto de prueba: RMSE, R² y MAE
pred_prolina <- predict(lm_prolina, newdata = test)
postResample(pred = pred_prolina, obs = test$Proline)

# Valores observados vs predichos en prueba (la diagonal es la predicción perfecta)
p_pred_lm <- test %>%
  mutate(Prediccion = pred_prolina) %>%
  ggplot(aes(Proline, Prediccion, color = Pueblo)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_manual(values = colores_pueblo, name = NULL) +
  coord_equal(xlim = c(200, 1750), ylim = c(200, 1750)) +
  labs(title = "Prolina observada vs predicha (conjunto de prueba)",
       x = "Prolina observada (mg/L)", y = "Prolina predicha (mg/L)")
p_pred_lm
guardar_plot(p_pred_lm, "06_regresion_observado_vs_predicho.png", w = 6.5, h = 6.5)

# ---- 3.4 Regresión logística multinomial: clasificación del pueblo de origen ----

# Validación cruzada 10-fold sobre entrenamiento; estandariza predictores y ajusta la penalización (decay)
ctrl <- trainControl(method = "cv", number = 10)
multi_pueblo <- train(Pueblo ~ ., data = train, method = "multinom",
                      preProcess = c("center", "scale"), trControl = ctrl,
                      tuneGrid = expand.grid(decay = c(0, 0.001, 0.01, 0.1, 0.5, 1)), trace = FALSE)
multi_pueblo

# Razones de momios por cada desviación estándar de aumento (referencia: Pueblo 1)
exp(coef(multi_pueblo$finalModel))

# Desempeño en prueba: matriz de confusión, exactitud, kappa y métricas por clase
pred_pueblo <- predict(multi_pueblo, newdata = test)
cm_pueblo <- confusionMatrix(pred_pueblo, test$Pueblo)
cm_pueblo

# Matriz de confusión como mapa de calor
p_cm <- as_tibble(cm_pueblo$table) %>%
  ggplot(aes(Reference, Prediction, fill = n)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = n, color = n > max(n) / 2), size = 6, show.legend = FALSE) +
  scale_fill_gradient(low = "#eef4fc", high = "#1c5aa6", guide = "none") +
  scale_color_manual(values = c("FALSE" = "grey15", "TRUE" = "white")) +
  scale_y_discrete(limits = rev) +
  labs(title = "Matriz de confusión: regresión logística multinomial",
       subtitle = sprintf("Conjunto de prueba (n = %d) · Exactitud = %.1f%%",
                         nrow(test), 100 * cm_pueblo$overall["Accuracy"]),
       x = "Pueblo real", y = "Pueblo predicho")
p_cm
guardar_plot(p_cm, "07_multinomial_matriz_confusion.png", w = 6.5, h = 5)

# Importancia de variables (magnitud de los coeficientes estandarizados, escala 0-100)
imp_pueblo <- varImp(multi_pueblo)$importance %>%
  rownames_to_column("Variable") %>%
  as_tibble() %>%
  rename(Importancia = Overall) %>%
  arrange(desc(Importancia))
imp_pueblo

p_imp <- ggplot(imp_pueblo, aes(Importancia, fct_reorder(Variable, Importancia))) +
  geom_col(fill = "#2a78d6", width = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Importancia de variables en la clasificación del pueblo",
       subtitle = "Regresión logística multinomial (coeficientes estandarizados)",
       x = "Importancia relativa (0-100)", y = NULL)
p_imp
guardar_plot(p_imp, "08_multinomial_importancia.png", w = 7, h = 5)

# ---- 3.d Interpretación ----
# 3.1 La prolina difiere entre pueblos (ANOVA, Welch y Kruskal-Wallis con p < 0.001; η²[H] = 0.67, efecto grande):
#     Pueblo 1 (~1116 mg/L) > Pueblo 3 (~630 mg/L) > Pueblo 2 (~520 mg/L), todas significativas por Games-Howell.
# 3.2 Los 13 compuestos difieren entre pueblos (p ajustado < 0.001); Flavanoids, Proline, Color_Intensity,
#     Alcohol y OD280 son los que más separan el origen, mientras que Ash es el que menos lo hace.
# 3.3 Ocho compuestos explican ~64% de la variación de la prolina (R² ajustado = 0.61; en prueba R² = 0.61 y RMSE ≈ 211 mg/L),
#     con supuestos cumplidos (VIF < 3, Breusch-Pagan p = 0.09, Shapiro p = 0.68); más alcohol, color y tonalidad, más prolina.
# 3.4 El perfil químico identifica el pueblo de origen con 98% de exactitud en prueba (51 de 52 muestras; kappa = 0.97).
#     Ash_Alcanity, Proline, Color_Intensity y Flavanoids son los rasgos que más distinguen a cada pueblo.
