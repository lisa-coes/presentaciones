---
lang: es
knitr:
  opts_chunk:
    collapse: true
    comment: "#>"
    R.options:
      
      knitr.graphics.auto_pdf: true
format:
  revealjs:
    slide-number: true
    theme: "libs/quarto-press.scss"
    auto-stretch: false
    title-slide-attributes:
      visibility: false
    transition: fade
    transition-speed: slow
    auto-play-media: true
    mathjax: "default"
editor_options: 
  chunk_output_type: console
---

```{r setup2, include=FALSE}
 knitr::opts_chunk$set(echo=FALSE, warning = FALSE,message = FALSE, cache = TRUE,out.width = '85%',fig.pos= "H"
                       # , fig.align = 'center'
                       )
 # knitr::opts_knit$set(base.url = "../") #relative path for .html output file
 # knitr::opts_knit$set(root.dir = "../") #relative path for chunks within .rmd files
 options(scipen=999)
 options(kableExtra.auto_format = FALSE)
 options(knitr.kable.NA = '')
 options(knitr.graphics.error = FALSE)
 Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
 
 table_format = if (knitr::is_html_output()) {
   #conditional instructions for kable
   "html" #if html set "html" in format
 } else if (knitr::is_latex_output()) {
   "latex"#if latex set "latex" in format
 }
 
 library(conflicted)

conflicts_prefer(dplyr::filter)
```

```{r}
#| label: packages
#| include: false

if (! require("pacman")) install.packages("pacman")

pacman::p_load(here,
               kableExtra)

options(scipen=999)
rm(list = ls())

```


```{r}
#| label: data
#| include: false

# Cargar datos
load(here("input/data/proc/elsoc_final_2.RData"))

if (!require("pacman")) install.packages("pacman")

library(here)
library(dplyr) 
library(ggplot2)
library(kableExtra)
library(tidyr)
library(glmmTMB)
library(texreg)
library(ggeffects)
library(patchwork)
library(broom.mixed)


```

```{r}
#| label: process-data
#| include: false
#| cache: true

# Procesar datos
elsoc_final_2 <- elsoc_final_2 %>%
  mutate(
    educ_cat_unordered = factor(educ_cat_factor, ordered = FALSE),
    # Índice consistente temporal
    justif_violencia_protesta_consistente = rowMeans(
      dplyr::select(., violencia_trabajadores, violencia_estudiantes), 
      na.rm = TRUE
    )
  )

# Redefinir labels
elsoc_final_2 <- elsoc_final_2 %>%
  mutate(
    educ_cat_unordered = factor(
      educ_cat_unordered,
      labels = c(
        "Media completa o menos",
        "Téc. sup.incompleta",
        "Téc. sup.completa",
        "Univ. incompleta",
        "Univ. completa"
      )
    )
  )
```

::: columns


::: {.column width="15%"}

![](images/logo_isuc.png){width=100%}

:::

::: {.column .column-right width="85%"}

<div style="font-size: 110%;">

## **Actitudes hacia la violencia política: Educación y Clase Social en el ciclo contencioso chileno (2016-2023)**

**René Canales Sellés^1^ ^2^** 

<div style="font-size: 80%; line-height: 1.4;">

**^1^Instituto de Sociología, Pontificia Universidad Católica de Chile**

**^2^Fondecyt Actitudes y comportamiento político juvenil: desigualdad, estilos de socialización escolar e influencias intergeneracionales**

Defensa de título para optar al grado de Magíster en Sociología

Comisión: Prof. Nicolás Somma, Prof. Matías Bargsted y Prof. Andrés Biehl

19 de enero 2026, Chile

</div>

</div>

:::
:::




# Problema de investigación {.xlarge data-background-color="#e4a149"}


## **Contexto: Estallido Social (El 18 O)**

::: {.box-inv-4 .sp-after .fragment style="font-size: 100%;"}

Lo que comenzó como una protesta estudiantil esporádica se transformó en el ciclo de movilización más intenso desde el retorno a la democracia (Somma et al., 2021)

:::

::: {.box-inv-4 .sp-after .fragment style="font-size: 100%;"}

Protestas multitudinarias con repertorios diversos, niveles de confrontación pocas veces vistos y un ciclo de violencia política altamente impactante (Cox et al., 2024; Somma, 2021; Pleyers, 2023)

:::

::: {.box-inv-4 .sp-after .fragment style="font-size: 100%;"}

Esto instaló una pregunta central en el debate académico y público: ¿cuando y para quién la violencia política resulta legítima?

:::

::: {.box-inv-4 .sp-after-half .fragment style="font-size: 110%;"}

Evidencia reciente (González y Le Foulon, 2020) sugiere que la justificación no fue homogénea.

:::


## **El problema teórico**

::: {.box-inv-4 .sp-after .fragment style="font-size: 110%;"}

Este patrón contradice la literatura clásica que asocia educación con mayor apego a normas democráticas y rechazo a la violencia política (Lipset, 1959; Nie et al., 1996)

:::


::: {.box-inv-4 .sp-after .fragment style="font-size: 110%;"}

Asume que los efectos de la educación y la clase social son **lineales**, **estables** y **universales**

:::

::: {.box-inv-4 .sp-after .fragment style="font-size: 110%;"}

En otras palabras, no sonsidera que estos efectos pueden ser **condicionales al contexto**

:::

## **El propósito de este trabajo...**

::::: columns
::: {.incremental .highlight-last style="font-size: 110%;"}

- Argumento que la participación en protestas no sólo refleja predisposiciones previas, **sino que puede transformarlas**
- En contextos de movilización intensa y represión, factores como la educación y la clase social **interactúan** con la experiencia de protesta
- Esto deviene en marcos morales e interpretativos sobre la legitimidad de la violencia política

:::

::: {.box-inv-4 .sp-after .fragment style="font-size: 110%;"}

### La pregunta de investigación...

**¿Cómo interactúan la educación, la clase social y la participación en protestas para moldear las actitudes hacia la violencia política en el contexto contencioso chileno?**

:::
:::::

# Antecedentes teóricos y empíricos {.xlarge data-background-color="#e4a149"}

## **El efecto de la educación**

::::: columns
::: {.column width="50%" .incremental .highlight-last style="font-size: 110%;"}

### El efecto civilizatorio

- La educación tiene un **efecto civilizatorio** (Lipset, 1959; Almond y Verba, 1963)
- Más educación → mayor apego a normas democráticas (Converse, 1972; Nie et al., 1996)
- Más educación → rechazo de la violencia (Dyrstad y Hillesund, 2020)

:::

::: {.column width="50%" .incremental .highlight-last style="font-size: 110%;"}

### La paradoja de la sofisticación

- La educación fomenta **sofisticación cognitiva** (Sabucedo et al., 2018; Nie et al., 1996)
- Permite **excepciones situacionales** (Varlik et al., 2025)
- Relación no lineal entre educación y violencia política


:::
:::::

## **De la moderación a legitimación táctica**

::::: columns

La sofisticación cognitiva no conduce necesariamente a la moderación, sino a **justificaciones más elaboradas y contextualizadas**

::: {.incremental .highlight-last style="font-size: 110%;"}
**En este marco:**

-   La participación en protestas no activa un efecto latente de la educación
-   Crea las condiciones donde esa sofisticación se emplea para **legitimar tácticamente la violencia**
-   Como una excepción moralmente aceptable

:::
:::::

## **La Clase Social y la experiencia de conflicto**

::::: columns
::: {.column width="50%" .incremental .highlight-last style="font-size: 110%;"}

### La Clase trabajadora

- Más expuestos a violencia estructural (Varlik et al., 2025)
- Mayor tolerancia basal a transgresión normativa (Bottero, 2004)
- Disposiciones relativamente estables
- La participación **refuerza** pero no transforma

:::

::: {.column width="50%" .incremental .highlight-last style="font-size: 110%;"}

### Clases medias y profesionales

- Parten desde umbrales más bajos
- La participación es una **experiencia disruptiva** (Varlik et al., 2025)
- Cambios actitudinales más pronunciados
- La clase define **punto de partida** y **margen de transformación**


:::

::: {.box-inv-4 .sp-after .fragment style="font-size: 110%;"}

Esto se conceptualiza como **el efecto techo** (Elsässer & Schäfer, 2023)

:::

:::::

## **El efecto de la participación: selección y transformación**

::::: columns
::: {.column width="50%" .incremental .highlight-last style="font-size: 110%;"}

### Modelo de Recursos

- Las actitudes son previas y estables
- La participación es un reflejo de ellas
- Perspectiva estática

:::

::: {.column width="50%" .incremental .highlight-last style="font-size: 110%;"}

### Consecuencias biográficas del activismo

- La participación es una **experiencia transformadora**
- Especialmente en contextos de alto riesgo
- Reconfigura identidades y marcos morales
- **Efectos heterogéneos y socialmente estratificados**


:::
:::::

## **Entonces...**

::: {.incremental .highlight-last style="font-size: 120%;"}

- Retomando la pregunta: **¿Cómo interactúan la educación, la clase social y la participación en protestas para moldear las actitudes hacia la violencia política en el contexto contencioso chileno?**

- Los efectos de la educación y la clase social sobre la justificación de la violencia **no son directos ni universales**, sino **condicionales a la experiencia de participación**

:::

## **Hipótesis 1: El efecto civilizatorio**

::: {.incremental .highlight-last style="font-size: 120%;"}

- _H1: La participación en protestas crea las condiciones específicas para la sofisticación cognitiva que lleva a legitimar la violencia_

- $H_{1a}$: Entre individuos que **no participan** en protestas, mayor nivel educativo está asociado con **menor justificación** de violencia en manifestaciones

- $H_{1b}$: Entre individuos que **sí participan** en protestas, el efecto se invierte: universitarios muestran **mayor justificación** de violencia comparado con individuos de menor educación

:::

## **Hipótesis 2: El efecto techo**

::: {.incremental .highlight-last style="font-size: 120%;"}

- _H2: La clase opera como un moderador estructural del vínculo entre participación y actitudes hacia la violencia política_

- $H_{2a}$: La clase trabajadora presenta una **mayor justificación basal** de la violencia política que la clase de servicio, independientemente de su nivel de participación

- $H_{2b}$: El incremento en la justificación de la violencia asociado a la participación es **menor para la clase trabajadora** que para la clase de servicio

:::


## **Hipótesis 3: La reconfiguración normativa diferenciada**

::: {.incremental .highlight-last style="font-size: 90%;"}

- _H3: La participación constituye un efecto transformador sobre la capacidad cognitiva de elaborar justificaciones sofisticadas, contribuyendo a la condiciones para interpretar la violencia política_

**Para Universitarios**

- $H_{3a}$: Universitarios participantes muestran **mayor justificación de violencia por manifestantes** vs universitarios no participantes

- $H_{3b}$: Los mismos universitarios participantes muestran **menor justificación de violencia estatal** vs universitarios no participantes

**Para la Clase de Servicios**

- $H_{3c}$: Clase de servicio participante muestra **mayor justificación de violencia por manifestantes** vs no participantes de la misma clase

- $H_{3d}$: La misma clase de servicio participante muestra **menor justificación de violencia estatal** vs no participantes
:::

## **Lógica de las hipótesis**

::: {.callout-note icon=false}
## **Marco moral dual**
Las hipótesis H3 capturan una **reconfiguración normativa bidireccional**: 

- ↑ Legitimación de violencia táctica (manifestantes)
- ↓ Legitimación de violencia represiva (Estado)

**Solo** entre sectores educados/privilegiados que participan
:::

# Datos y metodología {.xlarge data-background-color="#e99a24"}

## **Datos: Encuesta Longitudinal Social de Chile (ELSOC)**

::: {.incremental .highlight-last style="font-size: 120%;"}

- Encuesta panel representativa de población adulta urbana
- **Ventaja clave:** Sigue a los mismos individuos a lo largo del tiempo
- Permite observar cambios actitudinales antes, durante y después del estallido

**Muestra analítica:**

- Individuos que participaron en **al menos 3 olas**
- **Más de 20,000 observaciones** anidadas en ~3,700 personas
- Panorama completo en el tiempo de un grupo de personas
:::


## **Variable dependiente: Justificación de la Violencia Política**

::: {.incremental style="font-size: 120%;"}
- Dos índices diferenciados según tipo de violencia:
:::

::: {.box-inv-4 .sp-after .fragment style="font-size: 110%;"}
Escala continua 1 (Nunca se Justifica) a 5 (Siempre se justifica)
:::

::: columns
::: {.incremental .highlight .column width="50%"}
**Violencia en Protestas**

Promedio de 2 ítems:

- Trabajadores en huelga bloqueen la calle con barricadas
- Estudiantes lancen piedras a carabineros en marcha
:::

::: {.incremental .highlight .column width="50%"}
**Violencia Estatal**

Promedio de 2 ítems:

- Carabineros use violencia para reprimir manifestación pacífica
- Carabineros desaloje a la fuerza tomas de edificios/universidades
:::
:::


## **Variables independientes principales**

::: {.box-inv-4 .sp-after style="font-size: 95%;"}
**Educación (Categórica):** Media o menos (referencia); Técnica superior incompleta/completa; Universitaria incompleta/completa
:::

::: {.box-inv-4 .sp-after style="font-size: 95%;"}
**Clase Social (EGP):** Working class (V–VII, trabajadores manuales); Intermediate class (III–IV, empleados no manuales); Service class (I–II, profesionales y gerentes, referencia)
:::

::: {.box-inv-4 .sp-after style="font-size: 95%;"}
**Participación en protestas:** Variable dicotómica (Participó de alguna protesta en los últimos 12 meses): 1 = Sí | 0 = No
:::

::: {.box-inv-4 .sp-after style="font-size: 95%;"}
**Controles:** Edad (centrada en media), Género (1=Mujer), Ideología política (0-10, estandarizada), Tiempo (ola 2016-2023)
:::

## **Estrategia analítica**

::: {.incremental .highlight-last style="font-size: 110%;"}

**Modelos multinivel de efectos mixtos:**

- Observaciones (nivel 1) anidadas en individuos (nivel 2)
- Controlan heterogeneidad no observada entre individuos
- Modelan dependencia temporal

**Especificaciones:**

1. **Modelos de efectos principales:** Educación/Clase + Participación + Controles
2. **Modelos de interacción:** Educación × Participación y Clase × Participación
3. **Modelos separados** para violencia en protestas vs violencia estatal

:::

## **Estrategia analítica**

::: {.incremental .highlight-last style="font-size: 110%;"}

- En específico:

:::


::: {.fragment style="font-size: 120%;"}
$$
y_{tj} = \beta_{0j} + \beta_{1j} X_{tj} + \varepsilon_{tj}
$$ {#eq-final}

:::

::: {.fragment style="font-size: 120%;"}
$$
y_{tj} = \gamma_{00} + \gamma_{01} Z_j + \gamma_{10} X_{tj} + \gamma_{20} t + u_{0j} + \varepsilon_{tj}
$$ {#eq-final-int} 

:::


# Resultados {.xlarge data-background-color="#e99a24"}

```{r}
#| label: estimate-models
#| include: false
#| cache: true

# ============================================
# MODELOS DE EDUCACIÓN
# ============================================

# Modelo 1a: Educación - Efectos principales para Violencia en Protestas
mod_educ_protesta_main <- glmmTMB(
  justif_violencia_protesta_consistente ~ educ_cat_unordered + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 1b: Educación × Protesta para Violencia en Protestas (con interacción)
mod_educ_protesta_int <- glmmTMB(
  justif_violencia_protesta_consistente ~ educ_cat_unordered * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 2a: Educación - Efectos principales para Violencia Estatal
mod_educ_estatal_main <- glmmTMB(
  justif_violencia_estatal ~ educ_cat_unordered + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 2b: Educación × Protesta para Violencia Estatal (con interacción)
mod_educ_estatal_int <- glmmTMB(
  justif_violencia_estatal ~ educ_cat_unordered * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# ============================================
# MODELOS DE CLASE SOCIAL
# ============================================

# Modelo 3a: Clase - Efectos principales para Violencia en Protestas
mod_clase_protesta_main <- glmmTMB(
  justif_violencia_protesta_consistente ~ egp3 + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 3b: Clase × Protesta para Violencia en Protestas (con interacción)
mod_clase_protesta_int <- glmmTMB(
  justif_violencia_protesta_consistente ~ egp3 * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 4a: Clase - Efectos principales para Violencia Estatal
mod_clase_estatal_main <- glmmTMB(
  justif_violencia_estatal ~ egp3 + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)

# Modelo 4b: Clase × Protesta para Violencia Estatal (con interacción)
mod_clase_estatal_int <- glmmTMB(
  justif_violencia_estatal ~ egp3 * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data = elsoc_final_2,
  family = gaussian()
)
```

```{r}
#| echo: false
#| warning: false

# Panel: Comparación directa Protestas vs Estatal por año
comparacion_data <- elsoc_final_2 %>%
  group_by(year) %>%
  summarise(
    `Viol. en Protestas` = mean(justif_violencia_protesta_consistente, na.rm = TRUE),
    `Viol. Estatal` = mean(justif_violencia_estatal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("Viol"), 
                      names_to = "Tipo", 
                      values_to = "Justificacion")

diverg_estallido <- ggplot(comparacion_data, aes(x = year, y = Justificacion, color = Tipo, group = Tipo)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.6) +
  scale_color_manual(values = c("Viol. en Protestas" = "#EFC000FF", 
                                  "Viol. Estatal" = "#0073C2FF"),
                     name = "") +
  labs(
    title = "Divergencia Post-Estallido: Inversión de Jerarquías Morales",
    x = "Año",
    y = "Justificación (escala 1-5)",
    caption = "Línea roja: estallido 2019. Elaboración propia con ELSOC 2016-2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  ylim(1.4, 2.1)

```


```{r}
#| echo: false
#| warning: false

# Crear tablas descriptivas compactas
tabla_educ <- elsoc_final_2 %>%
  filter(!is.na(educ_cat_unordered) & !is.na(protesta_dummy)) %>%
  mutate(
    periodo = case_when(
      year %in% 2016:2018 ~ "Pre",
      year %in% 2022:2023 ~ "Post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(periodo)) %>%
  group_by(periodo, educ_cat_unordered, protesta_dummy) %>%
  summarise(
    `Viol. Protestas` = round(mean(justif_violencia_protesta_consistente, na.rm = TRUE), 2),
    `Viol. Estatal` = round(mean(justif_violencia_estatal, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(participacion = ifelse(protesta_dummy == 1, "Sí", "No"))

tabla_clase <- elsoc_final_2 %>%
  filter(!is.na(egp3) & !is.na(protesta_dummy)) %>%
  mutate(
    periodo = case_when(
      year %in% 2016:2018 ~ "Pre",
      year %in% 2022:2023 ~ "Post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(periodo)) %>%
  group_by(periodo, egp3, protesta_dummy) %>%
  summarise(
    `Viol. Protestas` = round(mean(justif_violencia_protesta_consistente, na.rm = TRUE), 2),
    `Viol. Estatal` = round(mean(justif_violencia_estatal, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(participacion = ifelse(protesta_dummy == 1, "Sí", "No"))

```

## **Divergencia temporal de la justificación de la violencia (2016–2023)**

:::{.fragment style="font-size: 110%;"}
```{r}
#| label: fig-part-estratificada
#| fig-cap: "Divergencia entre tipos de violencia (2016-2023)"
#| fig-width: 10
#| fig-height: 5.5

diverg_estallido

```

:::

## **Descriptivos: Educación y Participación (Pre vs Post estallido)**
```{r}
#| label: tbl-educ-descriptivos
#| tbl-width: 15
#| tbl-height: 7

tabla_educ_wide <- tabla_educ %>%
  unite("grupo", participacion, periodo, sep = "-") %>%
  select(-protesta_dummy) %>%
  pivot_wider(
    names_from = grupo,
    values_from = c(`Viol. Protestas`, `Viol. Estatal`)
  ) %>%
  select(educ_cat_unordered, 
         `Viol. Protestas_No-Pre`, `Viol. Protestas_No-Post`,
         `Viol. Protestas_Sí-Pre`, `Viol. Protestas_Sí-Post`,
         `Viol. Estatal_No-Pre`, `Viol. Estatal_No-Post`,
         `Viol. Estatal_Sí-Pre`, `Viol. Estatal_Sí-Post`)

kable(tabla_educ_wide, 
      col.names = c("Educación", 
                    "Pre-2019", "Post-2019", "Pre-2019", "Post-2019",
                    "Pre-2019", "Post-2019", "Pre-2019", "Post-2019"),
      align = "lcccccccc",
      format = "html") %>%
  kable_styling(font_size = 16, full_width = TRUE) %>%
  add_header_above(c(" " = 1, "No Participa" = 2, "Participa" = 2, "No Participa" = 2, "Participa" = 2)) %>%
  add_header_above(c(" " = 1, "Violencia Protestas" = 4, "Violencia Estatal" = 4))
```


## **Descriptivos: Clase Social y Participación (Pre vs Post estallido)**

```{r}
#| label: tbl-clase-descriptivos

tabla_clase_wide <- tabla_clase %>%
  unite("grupo", participacion, periodo, sep = "-") %>%
  select(-protesta_dummy) %>%
  pivot_wider(
    names_from = grupo,
    values_from = c(`Viol. Protestas`, `Viol. Estatal`)
  ) %>%
  select(egp3, 
         `Viol. Protestas_No-Pre`, `Viol. Protestas_No-Post`,
         `Viol. Protestas_Sí-Pre`, `Viol. Protestas_Sí-Post`,
         `Viol. Estatal_No-Pre`, `Viol. Estatal_No-Post`,
         `Viol. Estatal_Sí-Pre`, `Viol. Estatal_Sí-Post`)

kable(tabla_clase_wide, 
      col.names = c("Clase Social", 
                    "Pre-2019", "Post-2019", "Pre-2019", "Post-2019",
                    "Pre-2019", "Post-2019", "Pre-2019", "Post-2019"),
      align = "lcccccccc",
      format = "html") %>%
  kable_styling(font_size = 16, full_width = TRUE) %>%
  add_header_above(c(" " = 1, "No Participa" = 2, "Participa" = 2, "No Participa" = 2, "Participa" = 2)) %>%
  add_header_above(c(" " = 1, "Violencia Protestas" = 4, "Violencia Estatal" = 4))
```

## **Modelos de efectos principales**

::: {.fragment style="font-size: 110%;"}

```{r}
#| label: fig-modelos-principales
#| fig-cap: "Coeficientes de regresión (efectos principales)"
#| fig-width: 14
#| fig-height: 9
#| cache: true

#-------------------------------
# Función para extraer coeficientes
#-------------------------------
extract_coefs <- function(model, model_name) {
  broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed") %>%
    dplyr::filter(
      !grepl("Intercept|factor\\(year\\)|edad|mujer|ideologia", term)
    ) %>%
    dplyr::mutate(model = model_name)
}

#-------------------------------
# Unir modelos y clasificar tipo
#-------------------------------
coef_data <- dplyr::bind_rows(
  extract_coefs(mod_educ_protesta_main, "M1: Educ-Protesta"),
  extract_coefs(mod_educ_estatal_main, "M2: Educ-Estatal"),
  extract_coefs(mod_clase_protesta_main, "M3: Clase-Protesta"),
  extract_coefs(mod_clase_estatal_main, "M4: Clase-Estatal")
) %>%
  dplyr::mutate(
    tipo_modelo = dplyr::case_when(
      grepl("^M1|^M2", model) ~ "educacion",
      grepl("^M3|^M4", model) ~ "clase"
    )
  )

#-------------------------------
# Renombrar términos y filtrar cruzados
#-------------------------------
coef_data <- coef_data %>%
  dplyr::mutate(
    term = dplyr::case_when(
      grepl("Téc.*incompleta", term) ~ "Téc. incompleta",
      grepl("Téc.*completa", term) ~ "Téc. completa",
      grepl("Univ.*incompleta", term) ~ "Univ. incompleta",
      grepl("Univ.*completa", term) ~ "Univ. completa",
      grepl("Intermediate", term) ~ "Clase Media",
      grepl("Working", term) ~ "Clase Trabajadora",
      grepl("protesta", term) ~ "Participación",
      TRUE ~ term
    ),
    significativo = p.value < 0.05,
    asterisco = dplyr::case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    label_texto = paste0(round(estimate, 3), asterisco)
  ) %>%
  # Filtrar variables no pertinentes según tipo de modelo
  dplyr::filter(
    !(tipo_modelo == "educacion" & term %in% c("Clase Media", "Clase Trabajadora")) &
    !(tipo_modelo == "clase" & grepl("Téc\\.|Univ\\.", term))
  ) %>%
  # Ordenar términos para consistencia visual
  dplyr::mutate(
    term = forcats::fct_relevel(
      term,
      "Téc. incompleta", "Téc. completa",
      "Univ. incompleta", "Univ. completa",
      "Clase Trabajadora", "Clase Media",
      "Participación"
    )
  )

#-------------------------------
# Gráfico
#-------------------------------
ggplot(coef_data, aes(x = estimate, y = term, color = significativo)) +
  geom_vline(
    xintercept = 0, linetype = "dashed",
    color = "gray40", linewidth = 0.8
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.3, linewidth = 1, alpha = 0.7
  ) +
  geom_point(size = 3.5) +
  geom_text(
    aes(label = label_texto),
    hjust = -0.2, vjust = -0.7,
    size = 3.5, color = "black", fontface = "bold"
  ) +
  facet_wrap(~ model, ncol = 2, scales = "free_x") +
  scale_color_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "#95A5A6"),
    guide = "none"
  ) +
  labs(
    x = "Coeficiente (IC 95%)",
    y = "",
    caption = "* p<0.05; ** p<0.01; *** p<0.001. Referencias: Educación media o menos; Service class."
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.y = element_text(size = 13)
  )
```

:::

```{r}
#| fig-width: 14
#| fig-height: 10
#| echo: false
#| warning: false
#| include: false

# ===================================================================
# PANEL A: Efecto marginal de participación según EDUCACIÓN 
# para Violencia en PROTESTAS
# ===================================================================

# Calcular efectos marginales manualmente
# Efecto marginal = β_protesta + β_interacción * nivel_educación

# Extraer coeficientes del modelo
coef_educ_prot <- fixef(mod_educ_protesta_int)$cond

# Efecto marginal base (educación media = categoría de referencia)
me_base <- coef_educ_prot["protesta_dummy"]

# Efectos marginales para cada nivel educativo
me_data_educ_prot <- data.frame(
  educacion = c("Media o menos", "Téc. sup.\nincompleta", "Téc. sup.\ncompleta", 
                "Univ.\nincompleta", "Univ.\ncompleta"),
  me = c(
    me_base,  # Media (referencia)
    me_base + coef_educ_prot["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"],  # Téc inc
    me_base + coef_educ_prot["educ_cat_unorderedTéc. sup.completa:protesta_dummy"],     # Téc comp
    me_base + coef_educ_prot["educ_cat_unorderedUniv. incompleta:protesta_dummy"],      # Univ inc
    me_base + coef_educ_prot["educ_cat_unorderedUniv. completa:protesta_dummy"]         # Univ comp
  )
)

# Calcular errores estándar aproximados usando vcov
vcov_mat <- vcov(mod_educ_protesta_int)$cond

# SE para categoría de referencia (solo varianza de protesta_dummy)
se_base <- sqrt(vcov_mat["protesta_dummy", "protesta_dummy"])

# Para interacciones: SE = sqrt(Var(β1) + Var(β2) + 2*Cov(β1,β2))
se_tec_inc <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy", 
           "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"]
)

se_tec_comp <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedTéc. sup.completa:protesta_dummy", 
           "educ_cat_unorderedTéc. sup.completa:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedTéc. sup.completa:protesta_dummy"]
)

se_univ_inc <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedUniv. incompleta:protesta_dummy", 
           "educ_cat_unorderedUniv. incompleta:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedUniv. incompleta:protesta_dummy"]
)

se_univ_comp <- sqrt(
  vcov_mat["protesta_dummy", "protesta_dummy"] +
  vcov_mat["educ_cat_unorderedUniv. completa:protesta_dummy", 
           "educ_cat_unorderedUniv. completa:protesta_dummy"] +
  2 * vcov_mat["protesta_dummy", "educ_cat_unorderedUniv. completa:protesta_dummy"]
)

me_data_educ_prot$se <- c(se_base, se_tec_inc, se_tec_comp, se_univ_inc, se_univ_comp)
me_data_educ_prot$ci_low <- me_data_educ_prot$me - 1.96 * me_data_educ_prot$se
me_data_educ_prot$ci_high <- me_data_educ_prot$me + 1.96 * me_data_educ_prot$se
me_data_educ_prot$significativo <- !(me_data_educ_prot$ci_low < 0 & me_data_educ_prot$ci_high > 0)

# Reordenar niveles
me_data_educ_prot$educacion <- factor(me_data_educ_prot$educacion, 
                                       levels = c("Media o menos", "Téc. sup.\nincompleta", 
                                                  "Téc. sup.\ncompleta", "Univ.\nincompleta", 
                                                  "Univ.\ncompleta"))

# Gráfico Panel A
panel_a <- ggplot(me_data_educ_prot, aes(x = educacion, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1.2) +
  geom_point(aes(color = significativo), size = 5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo),
                width = 0.2, linewidth = 1.5) +
  geom_text(aes(label = round(me, 3)), vjust = -1.6, size = 5, fontface = "bold") +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#00BFC4"),
                     guide = "none") +
  labs(
    title = "A) Violencia en Protestas: Efecto Marginal de Participación\npor Educación",
    x = "Nivel Educativo",
    y = "Efecto Marginal de Participación"
  ) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank())

# ===================================================================
# PANEL B: Efecto marginal de participación según EDUCACIÓN 
# para Violencia ESTATAL
# ===================================================================

coef_educ_est <- fixef(mod_educ_estatal_int)$cond
me_base_est <- coef_educ_est["protesta_dummy"]

me_data_educ_est <- data.frame(
  educacion = c("Media o menos", "Téc. sup.\nincompleta", "Téc. sup.\ncompleta", 
                "Univ.\nincompleta", "Univ.\ncompleta"),
  me = c(
    me_base_est,
    me_base_est + coef_educ_est["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"],
    me_base_est + coef_educ_est["educ_cat_unorderedTéc. sup.completa:protesta_dummy"],
    me_base_est + coef_educ_est["educ_cat_unorderedUniv. incompleta:protesta_dummy"],
    me_base_est + coef_educ_est["educ_cat_unorderedUniv. completa:protesta_dummy"]
  )
)

vcov_mat_est <- vcov(mod_educ_estatal_int)$cond
se_base_est <- sqrt(vcov_mat_est["protesta_dummy", "protesta_dummy"])

se_tec_inc_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedTéc. sup.incompleta:protesta_dummy", 
               "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedTéc. sup.incompleta:protesta_dummy"]
)

se_tec_comp_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedTéc. sup.completa:protesta_dummy", 
               "educ_cat_unorderedTéc. sup.completa:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedTéc. sup.completa:protesta_dummy"]
)

se_univ_inc_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedUniv. incompleta:protesta_dummy", 
               "educ_cat_unorderedUniv. incompleta:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedUniv. incompleta:protesta_dummy"]
)

se_univ_comp_est <- sqrt(
  vcov_mat_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_est["educ_cat_unorderedUniv. completa:protesta_dummy", 
               "educ_cat_unorderedUniv. completa:protesta_dummy"] +
  2 * vcov_mat_est["protesta_dummy", "educ_cat_unorderedUniv. completa:protesta_dummy"]
)

me_data_educ_est$se <- c(se_base_est, se_tec_inc_est, se_tec_comp_est, se_univ_inc_est, se_univ_comp_est)
me_data_educ_est$ci_low <- me_data_educ_est$me - 1.96 * me_data_educ_est$se
me_data_educ_est$ci_high <- me_data_educ_est$me + 1.96 * me_data_educ_est$se
me_data_educ_est$significativo <- !(me_data_educ_est$ci_low < 0 & me_data_educ_est$ci_high > 0)

me_data_educ_est$educacion <- factor(me_data_educ_est$educacion, 
                                      levels = c("Media o menos", "Téc. sup.\nincompleta", 
                                                 "Téc. sup.\ncompleta", "Univ.\nincompleta", 
                                                 "Univ.\ncompleta"))

panel_b <- ggplot(me_data_educ_est, aes(x = educacion, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1.2) +
  geom_point(aes(color = significativo), size = 5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo),
                width = 0.2, linewidth = 1.5) +
  geom_text(aes(label = round(me, 3)), vjust = -1.6, size = 5, fontface = "bold") +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#619CFF"),
                     guide = "none") +
  labs(
    title = "B) Violencia Estatal: Efecto Marginal de Participación\npor Educación",
    x = "Nivel Educativo",
    y = "Efecto Marginal de Participación"
  ) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank())

# ===================================================================
# PANEL C: Efecto marginal de participación según CLASE SOCIAL
# para Violencia en PROTESTAS
# ===================================================================

coef_clase_prot <- fixef(mod_clase_protesta_int)$cond
me_base_clase_prot <- coef_clase_prot["protesta_dummy"]

me_data_clase_prot <- data.frame(
  clase = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", "Working\nclass (V+VI+VII)"),
  me = c(
    me_base_clase_prot,
    me_base_clase_prot + coef_clase_prot["egp3Intermediate class (III+IV):protesta_dummy"],
    me_base_clase_prot + coef_clase_prot["egp3Working class (V+VI+VII):protesta_dummy"]
  )
)

vcov_mat_clase_prot <- vcov(mod_clase_protesta_int)$cond
se_base_clase_prot <- sqrt(vcov_mat_clase_prot["protesta_dummy", "protesta_dummy"])

se_int_clase <- sqrt(
  vcov_mat_clase_prot["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_prot["egp3Intermediate class (III+IV):protesta_dummy", 
                      "egp3Intermediate class (III+IV):protesta_dummy"] +
  2 * vcov_mat_clase_prot["protesta_dummy", "egp3Intermediate class (III+IV):protesta_dummy"]
)

se_work_clase <- sqrt(
  vcov_mat_clase_prot["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_prot["egp3Working class (V+VI+VII):protesta_dummy", 
                      "egp3Working class (V+VI+VII):protesta_dummy"] +
  2 * vcov_mat_clase_prot["protesta_dummy", "egp3Working class (V+VI+VII):protesta_dummy"]
)

me_data_clase_prot$se <- c(se_base_clase_prot, se_int_clase, se_work_clase)
me_data_clase_prot$ci_low <- me_data_clase_prot$me - 1.96 * me_data_clase_prot$se
me_data_clase_prot$ci_high <- me_data_clase_prot$me + 1.96 * me_data_clase_prot$se
me_data_clase_prot$significativo <- !(me_data_clase_prot$ci_low < 0 & me_data_clase_prot$ci_high > 0)

me_data_clase_prot$clase <- factor(me_data_clase_prot$clase, 
                                    levels = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", 
                                               "Working\nclass (V+VI+VII)"))

panel_c <- ggplot(me_data_clase_prot, aes(x = clase, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1.2) +
  geom_point(aes(color = significativo), size = 5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo),
                width = 0.2, linewidth = 1.5) +
  geom_text(aes(label = round(me, 3)), vjust = -1.6, size = 5, fontface = "bold") +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#00BFC4"),
                     guide = "none") +
  labs(
    title = "C) Violencia en Protestas: Efecto Marginal de Participación\npor Clase Social",
    x = "Clase Social (EGP)",
    y = "Efecto Marginal de Participación"
  ) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank())

# ===================================================================
# PANEL D: Efecto marginal de participación según CLASE SOCIAL
# para Violencia ESTATAL
# ===================================================================

coef_clase_est <- fixef(mod_clase_estatal_int)$cond
me_base_clase_est <- coef_clase_est["protesta_dummy"]

me_data_clase_est <- data.frame(
  clase = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", "Working\nclass (V+VI+VII)"),
  me = c(
    me_base_clase_est,
    me_base_clase_est + coef_clase_est["egp3Intermediate class (III+IV):protesta_dummy"],
    me_base_clase_est + coef_clase_est["egp3Working class (V+VI+VII):protesta_dummy"]
  )
)

vcov_mat_clase_est <- vcov(mod_clase_estatal_int)$cond
se_base_clase_est <- sqrt(vcov_mat_clase_est["protesta_dummy", "protesta_dummy"])

se_int_clase_est <- sqrt(
  vcov_mat_clase_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_est["egp3Intermediate class (III+IV):protesta_dummy", 
                     "egp3Intermediate class (III+IV):protesta_dummy"] +
  2 * vcov_mat_clase_est["protesta_dummy", "egp3Intermediate class (III+IV):protesta_dummy"]
)

se_work_clase_est <- sqrt(
  vcov_mat_clase_est["protesta_dummy", "protesta_dummy"] +
  vcov_mat_clase_est["egp3Working class (V+VI+VII):protesta_dummy", 
                     "egp3Working class (V+VI+VII):protesta_dummy"] +
  2 * vcov_mat_clase_est["protesta_dummy", "egp3Working class (V+VI+VII):protesta_dummy"]
)

me_data_clase_est$se <- c(se_base_clase_est, se_int_clase_est, se_work_clase_est)
me_data_clase_est$ci_low <- me_data_clase_est$me - 1.96 * me_data_clase_est$se
me_data_clase_est$ci_high <- me_data_clase_est$me + 1.96 * me_data_clase_est$se
me_data_clase_est$significativo <- !(me_data_clase_est$ci_low < 0 & me_data_clase_est$ci_high > 0)

me_data_clase_est$clase <- factor(me_data_clase_est$clase, 
                                   levels = c("Service class\n(I+II)", "Intermediate\nclass (III+IV)", 
                                              "Working\nclass (V+VI+VII)"))

panel_d <- ggplot(me_data_clase_est, aes(x = clase, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1.2) +
  geom_point(aes(color = significativo), size = 5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significativo),
                width = 0.2, linewidth = 1.5) +
  geom_text(aes(label = round(me, 3)), vjust = -1.6, size = 5, fontface = "bold") +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#619CFF"),
                     guide = "none") +
  labs(
    title = "D) Violencia Estatal: Efecto Marginal de Participación\npor Clase Social",
    x = "Clase Social (EGP)",
    y = "Efecto Marginal de Participación"
  ) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank())

# Combinar paneles
combined_marginal_effects_educ <- (panel_a | panel_b) +
  plot_annotation(
    caption = "Efectos marginales con IC 95%. Línea roja indica efecto nulo. Puntos grises: no significativos. \nEfecto marginal = cambio en justificación al participar vs no participar, condicionado por educación. Elaboración propia con ELSOC.",
    theme = theme(plot.caption = element_text(size = 9, hjust = 0))
  )

combined_marginal_effects_clase <- (panel_c | panel_d) +
  plot_annotation(
    caption = "Efectos marginales con IC 95%. Línea roja indica efecto nulo. Puntos grises: no significativos. \nEfecto marginal = cambio en justificación al participar vs no participar, condicionado por clase. Elaboración propia con ELSOC.",
    theme = theme(plot.caption = element_text(size = 9, hjust = 0))
  )
```

## **Efectos marginales interacción Educación x Participación**

::: {.fragment style="font-size: 110%;"}
```{r}
#| label: fig-efectos-marginales-interaccion-educ
#| fig-cap: "Efectos marginales de la participación en protestas sobre justificación de violencia según educación"
#| fig-width: 16
#| fig-height: 9
#| echo: false
#| warning: false

combined_marginal_effects_educ
```
:::

## **Efectos marginales interacción Clase x Participación**

::: {.fragment style="font-size: 110%;"}
```{r}
#| label: fig-efectos-marginales-interaccion-clase
#| fig-cap: "Efectos marginales de la participación en protestas sobre justificación de violencia según clase social"
#| fig-width: 16
#| fig-height: 9
#| echo: false
#| warning: false

combined_marginal_effects_clase
```

:::

## **Reconfiguración Bidireccional Educación**

:::{.fragment style="font-size: 110%"}

```{r}
#| label: fig-bidireccional
#| fig-cap: "Marco moral dual: universitarios"
#| fig-width: 14
#| fig-height: 9
#| cache: true

# Predicciones para universitarios completos
pred_univ_prot <- ggpredict(mod_educ_protesta_int,
                            terms = c("protesta_dummy", 
                                     "educ_cat_unordered [Univ. completa]"))

pred_univ_est <- ggpredict(mod_educ_estatal_int,
                           terms = c("protesta_dummy",
                                    "educ_cat_unordered [Univ. completa]"))

# Combinar datos
df_prot <- as.data.frame(pred_univ_prot)
df_prot$tipo <- "Violencia en Protestas"

df_est <- as.data.frame(pred_univ_est)
df_est$tipo <- "Violencia Estatal"

df_combined <- rbind(df_prot, df_est)
df_combined$participacion <- factor(df_combined$x, labels = c("No participó", "Participó"))

ggplot(df_combined, aes(x = participacion, y = predicted, 
                        color = tipo, group = tipo)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.1, linewidth = 1.2) +
  scale_color_manual(values = c("Violencia en Protestas" = "#EFC000FF",
                                 "Violencia Estatal" = "#0073C2FF")) +
  labs(
    title = "Marco Moral Dual: Universitarios",
    subtitle = "Legitiman protestas, rechazan represión",
    x = "",
    y = "Justificación Predicha",
    color = ""
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )
```

:::

## **Reconfiguración Bidireccional Clase Social**

:::{.fragment style="font-size: 110%"}

```{r}
#| label: fig-bidireccional-clase
#| fig-cap: "Marco moral dual: clase de servicio"
#| fig-width: 14
#| fig-height: 9
#| cache: true

# Predicciones para clase de servicio
pred_service_prot <- ggpredict(mod_clase_protesta_int,
                               terms = c("protesta_dummy", 
                                        "egp3 [Service class (I+II)]"))

pred_service_est <- ggpredict(mod_clase_estatal_int,
                              terms = c("protesta_dummy",
                                       "egp3 [Service class (I+II)]"))

# Combinar datos
df_prot_clase <- as.data.frame(pred_service_prot)
df_prot_clase$tipo <- "Violencia en Protestas"

df_est_clase <- as.data.frame(pred_service_est)
df_est_clase$tipo <- "Violencia Estatal"

df_combined_clase <- rbind(df_prot_clase, df_est_clase)
df_combined_clase$participacion <- factor(df_combined_clase$x, labels = c("No participó", "Participó"))

ggplot(df_combined_clase, aes(x = participacion, y = predicted, 
                              color = tipo, group = tipo)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.1, linewidth = 1.2) +
  scale_color_manual(values = c("Violencia en Protestas" = "#EFC000FF",
                                 "Violencia Estatal" = "#0073C2FF")) +
  labs(
    title = "Marco Moral Dual: Clase de Servicio",
    subtitle = "Legitiman protestas, rechazan represión",
    x = "",
    y = "Justificación Predicha",
    color = ""
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )
```

:::

# Discusión {.xlarge data-background-color="#e4a149"}

## **Discusión: Educación, participación y sofisticación cognitiva**

:::{.fragment style="font-size: 110%"}

- Los resultados matizan el efecto civilizatorio (Lipset, 1959; Nie et al., 1996): a mayor educación, menor grado de justificación de la violencia (H1a​).

- Pero este efecto se debilita cuando la  persona participa de protestas. Neutraliza el efecto inhibidor de la educación (H1b se aprueba de manera parcial) 

- La educación actúa como un recurso para re-moralizar el conflicto situacional, desafiando las concepciones lineales clásicas de la sociología política (Lipset, 2959; Nie et al., 1996).

- La protesta sería una especie de *laboratorio moral* que transforma las disposiciones previas del individuo (Passy & Monsch, 2019).

:::

## **Discusión: Clase social y el efecto techo**

:::{.fragment style="font-size: 110%"}

- Clase trabajadora presenta justificación basal mayor de la violencia política, consistente con procesos de habituación derivados de la exposición cotidiana a la violencia estructural (Beeghley, 1986; Bottero, 2004; Olin Wright, 2005). Consistente con H2a.

- La interacción entre clase social y participación revela que la desigualdad distribuye márgenes de transformación actitudinal. Este patrón de habituación estructural permite confirmar la Hipótesis H2b​.

- El hallazgo más llamativo la convergencia de clase el dentro de la movilización: la participación actúa como un ecualizador entre clases frente a la justificación de la violencia de diferente tipo (H3c y H3d)

:::

## **Discusión: Participación y reconfiguración bidireccional**

:::{.fragment style="font-size: 110%"}

- Sucede una deslegitimación selectiva donde la violencia propia se justifica como respuesta táctica ante una injusticia procedimental percibida (Gerber et al., 2023).

- Este hallazgo permite validar simultáneamente las hipótesis H3a​, H3b​, H3c​ y H3d​.

- La violencia en 2019 no fue un síntoma de "marginalidad", sino un producto de la politización del malestar entre los sectores más educados del país.

:::

## Conclusiones {.xlarge data-background-color="#e4a149"}

:::{.fragment style="font-size: 110%"}

### **Agenda de investigación**

- Repensar la relación entre desigualdad, socialización política y legitimidad de la violencia más allá de los enfoques normativos

### **¿Qué pasa con la educación, la clase y la participación?**

- Los resultados sugieren que ni la educación ni la clase social operan como determinantes unidireccionales de las actitudes hacia la violencia política.

:::

## Conclusiones {.xlarge data-background-color="#e4a149"}

:::{.fragment style="font-size: 110%"}

### **Contribuciones**

Esta tesis realiza tres contribuciones principales:

1. Evidencia longitudinal: Fortalece la plausibilidad causal de la participación como motor de transformación moral en contextos de alta conflictividad.

2. Exploración del efecto techo en contextos contenciosos: Desigualdad estructural también repercute en las actitudes políticas.

3. Distribución selectiva de la legitimidad de la violencia: Desafía la interpretación de que hay aceptación o rechazo indiferenciado de la violencia política

:::

## Limitaciones y proyecciones {.xlarge data-background-color="#e4a149"}

:::{.fragment style="font-size: 110%"}

- El uso de medidas autoreportadas puede implicar sesgos de deseabildiad social. Profundidad cualitativa puede ser útil para este tipo de estudios.

- No captura la dimensión situacional inmediata de la interacción con la protesta. Profundizar en diseños causales (Disi Pavlic et al., 2025)

- La operacionalización de la educación no permite distinguir entre trayectorias educativas diferenciadas: La socialización política difiere.

:::

# Gracias por su atención

:::{style="font-size: 90%"}

Página del proyecto: <https://github.com/renejcanales/protest_effects>

:::


## **Referencias**

:::{style="font-size: 50%"}

Almond, G. A., y Verba, S. (1963). The civic culture: Political attitudes and democracy in five nations. Princeton University Press.

Beeghley, L. (1986). Social stratification in America: A critical analysis of institutional inequality. State University of New York Press.

Bottero, W. (2004). Stratification: Social division and inequality. Routledge.

Disi Pavlic, R., et al. (2025). Long-term attitudinal consequences of the 2019 Chilean outburst: A panel data analysis. (Working Paper/Forthcoming).

Donoso, S., & Somma, N. M. (2019). Student movements in Chile: Between the street and the parliament. Oxford University Press.

Gerber, M. M., et al. (2023). Procedural justice and police legitimacy during the Chilean social uprising. (Journal article reference).

Gonzalez, R., & Morán, C. L. F. (2020). The 2019–2020 Chilean protests: A first look at their causes and participants. International Journal of Sociology, 50(3), 227–235. https://doi.org/10.1080/00207659.2020.1752499

Jost, J. T., Glaser, J., Kruglanski, A. W., & Sulloway, F. J. (2003). Political conservatism as motivated social cognition. Psychological Bulletin, 129(3), 339–375.

Lindh, A., & McCall, L. (2020). Class position and political attitudes: A ceiling effect? European Sociological Review.

Lipset, S. M. (1959). Political man: The social bases of politics. Doubleday.

McAdam, D. (1989). The biographical consequences of activism. American Sociological Review, 54(5), 744–760.

Nie, N. H., Junn, J., & Stehlik-Barry, K. (1996). Education and democratic citizenship in America. University of Chicago Press.

Olin Wright, E. (2005). Approaches to class analysis. Cambridge University Press.

Passy, F., & Monsch, G.-A. (2019). Contentious politics and social change. Palgrave Macmillan.

Snow, D. A., & Benford, R. D. (1988). Ideology, resonance, and participant mobilization. International Social Movement Research, 1(1), 197–217.

Tetlock, P. E. (1986). A value pluralism model of ideological reasoning. Journal of Personality and Social Psychology, 50(4), 819–827.

Tyler, T. R. (2006). Why people obey the law. Princeton University Press.

Varlik, S., et al. (2025). Structural violence and normative orientations in unequal societies.

:::