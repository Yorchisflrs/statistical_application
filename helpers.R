# helpers.R

# Carga de librerías
load_libs <- function() {
  libs <- c("shiny", "shinythemes", "readr", "readxl", "ggplot2", "DT", "report", "stats", "effectsize", "rstatix", "DescTools", "car")
  invisible(lapply(libs, library, character.only = TRUE))
}

# Función para generar selectInput dinámico
gen_select <- function(id, label, vars, multiple=FALSE) selectInput(id, label, choices=vars, multiple=multiple)

# Función para obtener tipo de variable
get_types <- function(df) sapply(df, function(x) if(is.numeric(x)) "Cuantitativa" else "Cualitativa")

# Función para resumen descriptivo
resumen <- function(var) list(Media=mean(var,na.rm=T), Mediana=median(var,na.rm=T), Moda=unique(var)[which.max(tabulate(match(var,unique(var))))], Mínimo=min(var,na.rm=T), Máximo=max(var,na.rm=T), Rango=diff(range(var,na.rm=T)), Desv_Est=sd(var,na.rm=T), Coef_Var=sd(var,na.rm=T)/mean(var,na.rm=T)*100)

# Teoría centralizada
theory <- list(
  cuant = list(
    "t-test" = "Compara medias de dos grupos independientes.",
    "ANOVA" = "Compara medias de tres o más grupos.",
    "Wilcoxon" = "Versión no paramétrica de t-test.",
    "Pearson" = "Correlación lineal para datos normales.",
    "Spearman" = "Correlación no paramétrica.",
    "Shapiro-Wilk" = "Prueba de normalidad para muestras pequeñas.",
    "Kolmogorov-Smirnov" = "Prueba de normalidad alternativa.",
    "Kruskal-Wallis" = "ANOVA no paramétrico.",
    "Friedman" = "Para medidas repetidas/múltiples."
  ),
  cual = list(
    "Chi-cuadrado" = "Asociación entre variables categóricas.",
    "Fisher" = "Exacta para muestras pequeñas (2x2).",
    "Binomial" = "Proporciones observadas vs esperadas.",
    "Coef.Contingencia" = "Medida de asociación.",
    "G-test" = "Prueba de razón de verosimilitud.",
    "McNemar" = "Datos categóricos pareados.",
    "Cochran-Q" = "Extensión de McNemar para múltiples grupos."
  )
)

# Función para mostrar teoría
teoria_ui <- function(tipo, test) {
  txt <- theory[[tipo]][[test]]
  if (is.null(txt)) txt <- "Seleccione una prueba para ver su teoría."
  HTML(paste("<div class='alert alert-info'>", txt, "</div>"))
}

# Validación de variables
default_validate <- function(var1, var2 = NULL, type1 = NULL, type2 = NULL, data) {
  if (!is.null(var2) && var1 == var2) stop("No selecciones la misma variable como dependiente e independiente.")
  if (!is.null(type1) && !inherits(data[[var1]], type1)) stop(paste("La variable", var1, "debe ser de tipo", type1))
  if (!is.null(var2) && !is.null(type2) && !inherits(data[[var2]], type2)) stop(paste("La variable", var2, "debe ser de tipo", type2))
}

# Fórmula ANOVA
anova_formula <- function(dep, factors, interacciones = FALSE) {
  rhs <- if (interacciones && length(factors) > 1) paste(factors, collapse = "*") else paste(factors, collapse = "+")
  as.formula(paste(dep, "~", rhs))
}
