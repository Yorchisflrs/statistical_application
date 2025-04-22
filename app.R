# ---------------------------
# LIBRERÍAS REQUERIDAS
# ---------------------------
source("helpers.R")
load_libs()

# ---------------------------
# INTERFAZ DE USUARIO (UI)
# ---------------------------
ui <- navbarPage(
  title = "Analizador Estadístico Pro",
  theme = shinytheme("flatly"),
  
  # Pestaña 1: Carga de Datos
  tabPanel("Cargar Datos",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Sube tu archivo", accept = c(".csv", ".xlsx", ".txt")),
        radioButtons("sep", "Separador (CSV/TXT):", choices = c(Coma=",", PuntoComa=";", Tabulador="\t"), selected=","),
        radioButtons("dec", "Decimal:", choices = c(Punto=".", Coma=","), selected="."),
        actionButton("load_sample", "Cargar Datos de Ejemplo", class="btn-info")
      ),
      mainPanel(DTOutput("data_preview"), uiOutput("data_summary"))
    )
  ),
  
  # Pestaña 2: Análisis Cuantitativo
  tabPanel("Cuantitativas",
    sidebarLayout(
      sidebarPanel(
        uiOutput("quant_var_selector"),
        selectInput("quant_test", "Prueba Estadística:", choices = c("Seleccione..."="", "t-test", "ANOVA", "Wilcoxon", "Pearson", "Spearman", "Shapiro-Wilk", "Kolmogorov-Smirnov", "Kruskal-Wallis", "Friedman")),
        conditionalPanel("input.quant_test=='t-test'|input.quant_test=='ANOVA'|input.quant_test=='Wilcoxon'|input.quant_test=='Kruskal-Wallis'", uiOutput("group_var_selector")),
        conditionalPanel("input.quant_test=='Pearson'|input.quant_test=='Spearman'", uiOutput("quant_var2_selector")),
        actionButton("run_quant", "Ejecutar Análisis", class="btn-primary")
      ),
      mainPanel(
        uiOutput("quant_theory"), h3("Medidas Descriptivas"), tableOutput("descriptive_stats"),
        h3("Resultado de la Prueba"), verbatimTextOutput("quant_results"),
        h3("Interpretación Estadística"), uiOutput("quant_interpretation"),
        h3("Gráfico Inferencial"), plotOutput("quant_plot"),
        h3("Validación de Supuestos"), uiOutput("test_assumptions"),
        h3("Efectos Principales e Interacciones"), tableOutput("anova_effects"),
        h3("Post-hoc"), tableOutput("anova_posthoc")
      )
    )
  ),
  
  # Pestaña 3: Análisis Cualitativo
  tabPanel("Cualitativas",
    sidebarLayout(
      sidebarPanel(
        uiOutput("qual_var_selector"),
        selectInput("qual_test", "Prueba Estadística:", choices = c("Seleccione..."="", "Chi-cuadrado", "Fisher", "Binomial", "Coef.Contingencia", "G-test", "McNemar", "Cochran-Q")),
        conditionalPanel("input.qual_test!='Binomial'", uiOutput("qual_var2_selector")),
        actionButton("run_qual", "Ejecutar Análisis", class="btn-primary")
      ),
      mainPanel(
        uiOutput("qual_theory"), h3("Distribución de Frecuencias"), tableOutput("freq_table"),
        h3("Resultado de la Prueba"), verbatimTextOutput("qual_results"),
        h3("Interpretación Estadística"), uiOutput("qual_interpretation"),
        h3("Visualización"), plotOutput("qual_plot"),
        h3("Validación de Supuestos"), uiOutput("qual_assumptions")
      )
    )
  ),
  
  # Pestaña 4: Teorema Límite Central
  tabPanel("TLC",
    sidebarLayout(
      sidebarPanel(
        numericInput("n_samples", "Número de muestras:", 100, min=30),
        numericInput("sample_size", "Tamaño de muestra:", 30, min=10),
        selectInput("dist_type", "Distribución:", choices=c("Normal", "Exponencial", "Uniforme")),
        actionButton("run_clt", "Simular", class="btn-success")
      ),
      mainPanel(plotOutput("clt_plot"), htmlOutput("clt_explanation"), h3("Estadísticos Descriptivos"), verbatimTextOutput("clt_stats"))
    )
  ),
  
  # Pestaña 5: Documentación
  tabPanel("Guía", includeMarkdown("www/instructions.md"))
)

# ---------------------------
# LÓGICA DEL SERVIDOR (SERVER)
# ---------------------------
server <- function(input, output, session) {
  
  # --- FUNCIONES AUXILIARES MODULARES ---
  validate_vars <- default_validate
  anova_formula <- anova_formula
  addTooltip <- function(id, text) {
    tags$script(HTML(sprintf('$("#%s").attr("title", "%s");', id, text)))
  }
  
  # Carga de datos reactiva
  data <- reactive({
    if (input$load_sample > 0) {
      data.frame(
        Grupo = rep(c("Control", "Tratamiento"), each = 20),
        Puntuacion = c(rnorm(20, mean = 50, sd = 5), rnorm(20, mean = 55, sd = 5)),
        Edad = sample(18:65, 40, replace = TRUE),
        Satisfaccion = factor(sample(c("Baja", "Media", "Alta"), 40, replace = TRUE)),
        stringsAsFactors = TRUE
      )
    } else {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      tryCatch({
        switch(ext,
               "csv" = read_delim(input$file$datapath, delim = input$sep, 
                                  locale = locale(decimal_mark = input$dec)),
               "xlsx" = read_excel(input$file$datapath),
               "txt" = read_delim(input$file$datapath, delim = input$sep))
      }, error = function(e) {
        showNotification("Error al leer el archivo. Verifica el formato.", type = "error")
        return(NULL)
      })
    }
  })
  
  var_types <- reactive({
    req(data())
    get_types(data())
  })
  
  # ---------------------------
  # MÓDULO: CARGA DE DATOS
  # ---------------------------
  output$data_preview <- renderDT({
    req(data())
    datatable(head(data(), 10), 
              options = list(scrollX = TRUE, dom = 't'))
  })
  
  output$data_summary <- renderUI({
    req(data())
    tagList(
      h4("Resumen de Variables:"),
      p(strong("Cuantitativas:"), paste(names(data())[var_types() == "Cuantitativa"], collapse = ", ")),
      p(strong("Cualitativas:"), paste(names(data())[var_types() == "Cualitativa"], collapse = ", "))
    )
  })
  
  # ---------------------------
  # MÓDULO: ANÁLISIS CUANTITATIVO
  # ---------------------------
  output$quant_var_selector <- renderUI({
    req(data())
    quant_vars <- names(data())[var_types() == "Cuantitativa"]
    tagList(
      gen_select("quant_var", "Variable numérica:", quant_vars),
      addTooltip("quant_var", "Selecciona la variable dependiente numérica.")
    )
  })
  
  output$quant_var2_selector <- renderUI({
    req(data())
    quant_vars <- names(data())[var_types() == "Cuantitativa"]
    gen_select("quant_var2", "Segunda variable:", setdiff(quant_vars, input$quant_var))
  })
  
  output$group_var_selector <- renderUI({
    req(data())
    qual_vars <- names(data())[var_types() == "Cualitativa"]
    if (input$quant_test == "ANOVA") {
      tagList(
        gen_select("group_var", "Variables de agrupación (factores):", qual_vars, multiple = TRUE),
        checkboxInput("anova_inter", "Incluir interacciones (A*B)", value = FALSE),
        addTooltip("group_var", "Selecciona uno o más factores (variables cualitativas)")
      )
    } else {
      gen_select("group_var", "Variable de agrupación:", qual_vars)
    }
  })
  
  descriptive_stats <- reactive({
    req(data(), input$quant_var)
    df <- data()
    var <- df[[input$quant_var]]
    resumen(var)
  })
  
  output$descriptive_stats <- renderTable({
    stats <- descriptive_stats()
    data.frame(
      Medida = names(stats),
      Valor = format(unlist(stats), digits = 4),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, align = 'c', width = '100%')
  
  quant_test_result <- eventReactive(input$run_quant, {
    req(input$quant_test, input$quant_var)
    tryCatch({
      if(input$quant_test == "t-test") {
        req(input$group_var)
        validate_vars(input$quant_var, input$group_var, "numeric", "factor", data())
        t.test(data()[[input$quant_var]] ~ data()[[input$group_var]], na.action = na.omit)
      } else if(input$quant_test == "ANOVA") {
        req(input$group_var)
        validate_vars(input$quant_var, NULL, "numeric", NULL, data())
        formula <- anova_formula(input$quant_var, input$group_var, interacciones = input$anova_inter)
        aov(formula, data = data(), na.action = na.omit)
      } else if(input$quant_test == "Wilcoxon") {
        req(input$group_var)
        validate_vars(input$quant_var, input$group_var, "numeric", "factor", data())
        wilcox.test(data()[[input$quant_var]] ~ data()[[input$group_var]], na.action = na.omit)
      } else if(input$quant_test == "Pearson") {
        req(input$quant_var2)
        validate_vars(input$quant_var, input$quant_var2, "numeric", "numeric", data())
        cor.test(data()[[input$quant_var]], data()[[input$quant_var2]], method = "pearson", use = "complete.obs")
      } else if(input$quant_test == "Spearman") {
        req(input$quant_var2)
        validate_vars(input$quant_var, input$quant_var2, "numeric", "numeric", data())
        cor.test(data()[[input$quant_var]], data()[[input$quant_var2]], method = "spearman", use = "complete.obs")
      } else if(input$quant_test == "Shapiro-Wilk") {
        shapiro.test(na.omit(data()[[input$quant_var]]))
      } else if(input$quant_test == "Kolmogorov-Smirnov") {
        ks.test(na.omit(data()[[input$quant_var]]), "pnorm")
      } else if(input$quant_test == "Kruskal-Wallis") {
        req(input$group_var)
        validate_vars(input$quant_var, input$group_var, "numeric", "factor", data())
        kruskal.test(data()[[input$quant_var]] ~ data()[[input$group_var]], na.action = na.omit)
      } else if(input$quant_test == "Friedman") {
        req(input$group_var)
        friedman.test(data()[[input$quant_var]], data()[[input$group_var]])
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$quant_results <- renderPrint({
    req(quant_test_result())
    print(quant_test_result())
  })
  
  output$anova_effects <- renderTable({
    req(input$quant_test == "ANOVA", quant_test_result())
    summary(quant_test_result())[[1]]
  }, rownames = TRUE)
  
  output$anova_posthoc <- renderTable({
    req(input$quant_test == "ANOVA", quant_test_result())
    if (any(summary(quant_test_result())[[1]][["Pr(>F)"]] < 0.05, na.rm = TRUE)) {
      as.data.frame(TukeyHSD(quant_test_result()))
    }
  }, rownames = TRUE)
  
  output$quant_interpretation <- renderUI({
    req(quant_test_result())
    res <- quant_test_result()
    pval <- NULL
    if (!is.null(res$p.value)) {
      pval <- res$p.value
    } else if (!is.null(res[["Pr(>F)"]])) {
      pval <- res[["Pr(>F)"]][1]
    }
    effect <- tryCatch({
      if (inherits(res, "htest")) effectsize::interpret_r(res$estimate) else NULL
    }, error = function(e) NULL)
    interp <- tryCatch({
      paste(capture.output(report(res)), collapse = "<br>")
    }, error = function(e) {
      "<i>No se pudo generar interpretación automática. Consulte el resultado numérico.</i>"
    })
    html <- paste0(
      if (!is.null(pval)) paste0("<b>p-value:</b> ", format(pval, digits=4), "<br>") else "",
      if (!is.null(effect)) paste0("<b>Tamaño del efecto:</b> ", effect, "<br>") else "",
      interp
    )
    HTML(paste("<div class='well'>", html, "</div>"))
  })
  
  output$quant_plot <- renderPlot({
    req(input$quant_test, input$quant_var, data())
    if(input$quant_test %in% c("t-test", "ANOVA", "Wilcoxon", "Kruskal-Wallis")) {
      req(input$group_var)
      ggplot(data(), aes_string(x = input$group_var, y = input$quant_var)) + 
        geom_boxplot(fill = "#0055A4", alpha = 0.7) + 
        stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#FFDD00") +
        labs(title = paste("Comparación de grupos -", input$quant_var),
             subtitle = "Cuadro amarillo = media, Caja = IQR",
             x = "Grupo", y = input$quant_var) +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(face = "bold"))
    } else if(input$quant_test %in% c("Pearson", "Spearman")) {
      req(input$quant_var2)
      ggplot(data(), aes_string(x = input$quant_var, y = input$quant_var2)) + 
        geom_point(color = "#0055A4", alpha = 0.7) + 
        geom_smooth(method = "lm", color = "#FFDD00", se = TRUE) +
        labs(title = paste("Correlación", input$quant_test),
             subtitle = paste("Entre", input$quant_var, "y", input$quant_var2),
             x = input$quant_var, y = input$quant_var2) +
        theme_minimal(base_size = 15)
    } else if(input$quant_test == "Shapiro-Wilk") {
      ggplot(data(), aes_string(sample = input$quant_var)) + 
        stat_qq(color = "#0055A4") + 
        stat_qq_line(color = "#FFDD00") +
        labs(title = "Gráfico Q-Q para normalidad",
             subtitle = paste("Variable:", input$quant_var),
             x = "Cuantiles teóricos", y = "Cuantiles muestrales") +
        theme_minimal(base_size = 15)
    }
  })
  
  # ---------------------------
  # MÓDULO: ANÁLISIS CUALITATIVO
  # ---------------------------
  output$qual_var_selector <- renderUI({
    req(data())
    qual_vars <- names(data())[var_types() == "Cualitativa"]
    tagList(
      gen_select("qual_var1", "Variable 1:", qual_vars),
      if(input$qual_test != "Binomial") {
        gen_select("qual_var2", "Variable 2:", qual_vars)
      }
    )
  })
  
  output$freq_table <- renderTable({
    req(input$qual_var1, data())
    
    if(input$qual_test == "Binomial") {
      tab <- table(data()[[input$qual_var1]])
      prop <- prop.table(tab)
      cbind(Frecuencia = tab, Proporcion = round(prop, 4))
    } else {
      req(input$qual_var2)
      tab <- table(data()[[input$qual_var1]], data()[[input$qual_var2]])
      addmargins(tab)
    }
  }, rownames = TRUE)
  
  qual_test_result <- eventReactive(input$run_qual, {
    req(input$qual_test, input$qual_var1)
    
    tryCatch({
      if(input$qual_test == "Chi-cuadrado") {
        req(input$qual_var2)
        chisq.test(table(data()[[input$qual_var1]], data()[[input$qual_var2]]))
      } else if(input$qual_test == "Fisher") {
        req(input$qual_var2)
        fisher.test(table(data()[[input$qual_var1]], data()[[input$qual_var2]]))
      } else if(input$qual_test == "Binomial") {
        binom.test(table(data()[[input$qual_var1]]))
      } else if(input$qual_test == "Coef.Contingencia") {
        req(input$qual_var2)
        ct <- table(data()[[input$qual_var1]], data()[[input$qual_var2]])
        list(
          Coefficient = ContCoef(ct),
          ChiSquared = chisq.test(ct)
        )
      } else if(input$qual_test == "G-test") {
        req(input$qual_var2)
        DescTools::GTest(table(data()[[input$qual_var1]], data()[[input$qual_var2]]))
      } else if(input$qual_test == "McNemar") {
        req(input$qual_var2)
        mcnemar.test(table(data()[[input$qual_var1]], data()[[input$qual_var2]]))
      } else if(input$qual_test == "Cochran-Q") {
        req(input$qual_var2)
        cochran.qtest(data()[[input$qual_var1]], data()[[input$qual_var2]])
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$qual_results <- renderPrint({
    req(qual_test_result())
    print(qual_test_result())
  })
  
  output$qual_interpretation <- renderUI({
    req(qual_test_result())
    HTML(paste("<div class='well'>", 
               paste(capture.output(report(qual_test_result())), collapse = "<br>"), 
               "</div>"))
  })
  
  output$qual_plot <- renderPlot({
    req(input$qual_test, input$qual_var1, data())
    
    if(input$qual_test == "Binomial") {
      ggplot(data(), aes_string(x = input$qual_var1)) + 
        geom_bar(fill = "steelblue", alpha = 0.7) + 
        labs(title = paste("Distribución de", input$qual_var1),
             x = input$qual_var1, y = "Frecuencia") +
        theme_minimal()
    } else {
      req(input$qual_var2)
      ggplot(data(), aes_string(x = input$qual_var1, fill = input$qual_var2)) + 
        geom_bar(position = "dodge") + 
        labs(title = paste("Distribución conjunta de", input$qual_var1, "y", input$qual_var2),
             x = input$qual_var1, y = "Frecuencia", fill = input$qual_var2) +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal()
    }
  })
  
  output$qual_assumptions <- renderUI({
    req(input$qual_test, input$qual_var1)
    
    assumptions <- switch(input$qual_test,
                          "Chi-cuadrado" = {
                            req(input$qual_var2)
                            tab <- table(data()[[input$qual_var1]], data()[[input$qual_var2]])
                            exp <- chisq.test(tab)$expected
                            viol <- sum(exp < 5)/length(exp)*100
                            
                            tagList(
                              h4("Supuestos para Chi-cuadrado:"),
                              p(strong("1. Frecuencias esperadas:")),
                              p(paste(round(viol, 1), "% de celdas con frecuencias esperadas < 5")),
                              if(viol > 20) {
                                p("❌ Demasiadas celdas con frecuencias bajas (considerar prueba exacta de Fisher)", 
                                  style = "color:red;")
                              } else if(viol > 0) {
                                p("⚠️ Algunas celdas con frecuencias bajas (podría afectar resultados)", 
                                  style = "color:orange;")
                              } else {
                                p("✅ Todas las celdas tienen frecuencias adecuadas", 
                                  style = "color:green;")
                              },
                              
                              p(strong("2. Independencia de observaciones:")),
                              p("✅ Las observaciones deben ser independientes")
                            )
                          },
                          "Fisher" = {
                            tagList(
                              h4("Supuestos para prueba exacta de Fisher:"),
                              p("1. ✅ Datos categóricos"),
                              p("2. ✅ Muestras pequeñas o frecuencias esperadas < 5"),
                              p("3. ✅ Tablas 2x2 o pequeñas")
                            )
                          },
                          "Binomial" = {
                            tagList(
                              h4("Supuestos para prueba binomial:"),
                              p("1. ✅ Dos categorías mutuamente excluyentes"),
                              p("2. ✅ Ensayos independientes"),
                              p("3. ✅ Probabilidad constante en cada ensayo")
                            )
                          }
    )
    
    return(assumptions)
  })
  
  # ---------------------------
  # MÓDULO: TEOREMA LÍMITE CENTRAL
  # ---------------------------
  clt_simulation <- eventReactive(input$run_clt, {
    req(input$n_samples, input$sample_size, input$dist_type)
    
    means <- replicate(input$n_samples, {
      sample <- switch(input$dist_type,
                       "Normal" = rnorm(input$sample_size),
                       "Exponencial" = rexp(input$sample_size),
                       "Uniforme" = runif(input$sample_size, min = 0, max = 1))
      mean(sample)
    })
    
    return(means)
  })
  
  output$clt_plot <- renderPlot({
    means <- clt_simulation()
    
    ggplot(data.frame(means), aes(x = means)) + 
      geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "steelblue", alpha = 0.7) + 
      stat_function(fun = dnorm, 
                    args = list(mean = mean(means), sd = sd(means)),
                    color = "red", size = 1) +
      labs(title = "Distribución de Medias Muestrales (TLC)",
           subtitle = paste("Distribución original:", input$dist_type),
           x = "Media Muestral", y = "Densidad") +
      theme_minimal()
  })
  
  output$clt_stats <- renderPrint({
    means <- clt_simulation()
    cat("Media de medias:", mean(means), "\n")
    cat("Desviación estándar de medias:", sd(means), "\n")
    cat("Error estándar teórico:", sd(means)/sqrt(input$sample_size), "\n")
    cat("\nPrueba de normalidad (Shapiro-Wilk):\n")
    print(shapiro.test(means))
  })
  
  output$clt_explanation <- renderUI({
    HTML(paste(
      "<h4>Teorema del Límite Central</h4>",
      "<p>Independientemente de la distribución original, la distribución de las medias muestrales",
      "se aproxima a una distribución normal cuando el tamaño de muestra es grande (n ≥ 30).</p>",
      "<p><strong>Parámetros actuales:</strong></p>",
      "<ul>",
      "<li>Número de muestras: ", input$n_samples, "</li>",
      "<li>Tamaño de muestra: ", input$sample_size, "</li>",
      "<li>Distribución original: ", input$dist_type, "</li>",
      "</ul>"
    ))
  })
  
  # ---------------------------
  # BASE DE DATOS DE TEORÍA
  # ---------------------------
  output$quant_theory <- renderUI({
    req(input$quant_test)
    teoria_ui("cuant", input$quant_test)
  })
  
  output$qual_theory <- renderUI({
    req(input$qual_test)
    teoria_ui("cual", input$qual_test)
  })
}

# EJECUTAR LA APLICACIÓN
shinyApp(ui, server)
