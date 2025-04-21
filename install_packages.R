# install_packages.R
cran_packages <- c(
  "shiny", "shinythemes", "readr", "readxl", "ggplot2", "DT",
  "stats", "effectsize", "rstatix", "DescTools", "car"
)
github_packages <- c("easystats/report")

# Instala paquetes CRAN que falten
to_install <- setdiff(cran_packages, rownames(installed.packages()))
if(length(to_install) > 0) install.packages(to_install)

# Instala 'remotes' si falta
if(!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Instala paquetes de GitHub que falten
if(!requireNamespace("report", quietly = TRUE)) remotes::install_github("easystats/report")
