# ---------------------------
# LIBRERÍAS REQUERIDAS
# ---------------------------
library(shiny)
library(shinythemes)
library(readr)
library(readxl)
library(ggplot2)
library(DT)
library(report)
library(stats)
library(effectsize)  # Para pruebas adicionales
library(rstatix)     # Para G-test y otras pruebas
library(DescTools)   # Para Coeficiente de contingencia

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
               fileInput("file", "Sube tu archivo", 
                         accept = c(".csv", ".xlsx", ".txt")),
               radioButtons("sep", "Separador (CSV/TXT):", 
                            choices = c(Coma = ",", PuntoComa = ";", Tabulador = "\t"),
                            selected = ","),
               radioButtons("dec", "Decimal:", 
                            choices = c(Punto = ".", Coma = ","),
                            selected = ".")
             ),
             mainPanel(
               DTOutput("data_preview"),
               uiOutput("data_summary")
             )
           )),
  
  # Pestaña 2: Análisis Cuantitativo
  tabPanel("Cuantitativas",
           sidebarLayout(
             sidebarPanel(
               uiOutput("quant_var_selector"),
               selectInput("quant_test", "Prueba Estadística:", 
                           choices = c("Seleccione..." = "", 
                                       "t-test", 
                                       "ANOVA", 
                                       "Wilcoxon",
                                       "Pearson",
                                       "Spearman",
                                       "Shapiro-Wilk", 
                                       "Kolmogorov-Smirnov",
                                       "Kruskal-Wallis",
                                       "Friedman")),
               conditionalPanel(
                 condition = "input.quant_test == 't-test' | input.quant_test == 'ANOVA' | input.quant_test == 'Wilcoxon' | input.quant_test == 'Kruskal-Wallis'",
                 uiOutput("group_var_selector")
               ),
               conditionalPanel(
                 condition = "input.quant_test == 'Pearson' | input.quant_test == 'Spearman'",
                 uiOutput("quant_var2_selector")
               ),
               actionButton("run_quant", "Ejecutar", class = "btn-primary")
             ),
             mainPanel(
               uiOutput("quant_theory"),
               plotOutput("quant_plot"),
               verbatimTextOutput("quant_results"),
               uiOutput("quant_interpretation")
             )
           )),
  
  # Pestaña 3: Análisis Cualitativo
  tabPanel("Cualitativas",
           sidebarLayout(
             sidebarPanel(
               uiOutput("qual_var_selector"),
               selectInput("qual_test", "Prueba Estadística:", 
                           choices = c("Seleccione..." = "", 
                                       "Chi-cuadrado", 
                                       "Fisher", 
                                       "Binomial",
                                       "Coef.Contingencia",
                                       "G-test",
                                       "McNemar",
                                       "Cochran-Q")),
               conditionalPanel(
                 condition = "input.qual_test != 'Binomial'",
                 uiOutput("qual_var2_selector")
               ),
               actionButton("run_qual", "Ejecutar", class = "btn-primary")
             ),
             mainPanel(
               uiOutput("qual_theory"),
               plotOutput("qual_plot"),
               verbatimTextOutput("qual_results"),
               uiOutput("qual_interpretation")
             )
           )),
  
  # Pestaña 4: Teorema Límite Central
  tabPanel("TLC",
           sidebarLayout(
             sidebarPanel(
               numericInput("n_samples", "Número de muestras:", 100, min = 30),
               numericInput("sample_size", "Tamaño de muestra:", 30, min = 10),
               selectInput("dist_type", "Distribución:", 
                           choices = c("Normal", "Exponencial", "Uniforme"))
             ),
             mainPanel(
               plotOutput("clt_plot"),
               htmlOutput("clt_explanation")
             )
           )),
  
  # Pestaña 5: Documentación
  tabPanel("Guía", includeMarkdown("www/instructions.md"))
)

# ---------------------------
# LÓGICA DEL SERVIDOR (SERVER)
# ---------------------------
server <- function(input, output, session) {
  
  # Reactive: Carga de datos
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      switch(ext,
             "csv" = read_delim(input$file$datapath, delim = input$sep, locale = locale(decimal_mark = input$dec)),
             "xlsx" = read_excel(input$file$datapath),
             "txt" = read_delim(input$file$datapath, delim = input$sep))
    }, error = function(e) {
      showNotification("Error al leer el archivo. Verifica el formato.", type = "error")
      return(NULL)
    })
  })
  
  # Detección de tipos de variables
  var_types <- reactive({
    req(data())
    sapply(data(), function(x) {
      if (is.numeric(x)) "Cuantitativa" else "Cualitativa"
    })
  })
  
  # ---------------------------
  # MÓDULO: CARGA DE DATOS
  # ---------------------------
  output$data_preview <- renderDT({
    req(data())
    datatable(head(data(), 10), options = list(scrollX = TRUE))
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
    selectInput("quant_var", "Variable numérica:", choices = quant_vars)
  })
  
  output$quant_var2_selector <- renderUI({
    req(data())
    quant_vars <- names(data())[var_types() == "Cuantitativa"]
    selectInput("quant_var2", "Segunda variable:", choices = quant_vars)
  })
  
  output$group_var_selector <- renderUI({
    req(data())
    qual_vars <- names(data())[var_types() == "Cualitativa"]
    selectInput("group_var", "Variable de agrupación:", choices = qual_vars)
  })
  
  quant_test_result <- eventReactive(input$run_quant, {
    req(input$quant_test, input$quant_var)
    
    tryCatch({
      if(input$quant_test == "t-test") {
        req(input$group_var)
        t.test(data()[[input$quant_var]] ~ data()[[input$group_var]])
      } else if(input$quant_test == "ANOVA") {
        req(input$group_var)
        aov(data()[[input$quant_var]] ~ data()[[input$group_var]])
      } else if(input$quant_test == "Wilcoxon") {
        req(input$group_var)
        wilcox.test(data()[[input$quant_var]] ~ data()[[input$group_var]])
      } else if(input$quant_test == "Pearson") {
        req(input$quant_var2)
        cor.test(data()[[input$quant_var]], data()[[input$quant_var2]], method = "pearson")
      } else if(input$quant_test == "Spearman") {
        req(input$quant_var2)
        cor.test(data()[[input$quant_var]], data()[[input$quant_var2]], method = "spearman")
      } else if(input$quant_test == "Shapiro-Wilk") {
        shapiro.test(data()[[input$quant_var]])
      } else if(input$quant_test == "Kolmogorov-Smirnov") {
        ks.test(data()[[input$quant_var]], "pnorm")
      } else if(input$quant_test == "Kruskal-Wallis") {
        req(input$group_var)
        kruskal.test(data()[[input$quant_var]] ~ data()[[input$group_var]])
      } else if(input$quant_test == "Friedman") {
        req(input$group_var)
        friedman.test(data()[[input$quant_var]], data()[[input$group_var]])
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$quant_plot <- renderPlot({
    req(input$quant_test, input$quant_var, data())
    
    if(input$quant_test %in% c("t-test", "ANOVA", "Wilcoxon", "Kruskal-Wallis")) {
      req(input$group_var)
      ggplot(data(), aes_string(x = input$group_var, y = input$quant_var)) + 
        geom_boxplot() + 
        labs(title = paste("Diagrama de Cajas -", input$quant_test)) +
        theme_minimal()
    } else if(input$quant_test %in% c("Pearson", "Spearman")) {
      req(input$quant_var2)
      ggplot(data(), aes_string(x = input$quant_var, y = input$quant_var2)) + 
        geom_point() + 
        geom_smooth(method = "lm") +
        labs(title = paste("Correlación -", input$quant_test)) +
        theme_minimal()
    } else if(input$quant_test == "Shapiro-Wilk") {
      ggplot(data(), aes_string(sample = input$quant_var)) + 
        stat_qq() + 
        stat_qq_line() +
        labs(title = "Q-Q Plot para Normalidad") +
        theme_minimal()
    }
  })
  
  output$quant_results <- renderPrint({
    req(quant_test_result())
    print(quant_test_result())
  })
  
  output$quant_interpretation <- renderUI({
    req(quant_test_result())
    HTML(paste("<div class='alert alert-info'>", 
               paste(capture.output(report(quant_test_result())), collapse = "<br>"), 
               "</div>"))
  })
  
  # ---------------------------
  # MÓDULO: ANÁLISIS CUALITATIVO
  # ---------------------------
  output$qual_var_selector <- renderUI({
    req(data())
    qual_vars <- names(data())[var_types() == "Cualitativa"]
    tagList(
      selectInput("qual_var1", "Variable 1:", choices = qual_vars),
      if(input$qual_test != "Binomial") {
        selectInput("qual_var2", "Variable 2:", choices = qual_vars)
      }
    )
  })
  
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
        ContCoef(ct)
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
  
  output$qual_plot <- renderPlot({
    req(input$qual_test, input$qual_var1, data())
    
    if(input$qual_test != "Binomial") {
      req(input$qual_var2)
      ggplot(data(), aes_string(x = input$qual_var1, fill = input$qual_var2)) + 
        geom_bar(position = "dodge") + 
        labs(title = paste("Distribución Conjunta -", input$qual_test)) +
        theme_minimal()
    } else {
      ggplot(data(), aes_string(x = input$qual_var1)) + 
        geom_bar() + 
        labs(title = paste("Distribución de Frecuencias -", input$qual_test)) +
        theme_minimal()
    }
  })
  
  output$qual_results <- renderPrint({
    req(qual_test_result())
    print(qual_test_result())
  })
  
  output$qual_interpretation <- renderUI({
    req(qual_test_result())
    HTML(paste("<div class='alert alert-info'>", 
               paste(capture.output(report(qual_test_result())), collapse = "<br>"), 
               "</div>"))
  })
  
  # ---------------------------
  # MÓDULO: TEOREMA LÍMITE CENTRAL
  # ---------------------------
  output$clt_plot <- renderPlot({
    means <- replicate(input$n_samples, {
      sample <- switch(input$dist_type,
                       "Normal" = rnorm(input$sample_size),
                       "Exponencial" = rexp(input$sample_size),
                       "Uniforme" = runif(input$sample_size))
      mean(sample)
    })
    
    ggplot(data.frame(means), aes(x = means)) + 
      geom_histogram(binwidth = 0.1, fill = "#3498db", color = "white") + 
      labs(title = "Distribución de Medias Muestrales (TLC)",
           x = "Media Muestral", y = "Frecuencia") +
      theme_minimal()
  })
  
  output$clt_explanation <- renderUI({
    HTML(paste(
      "<h4>Teorema del Límite Central</h4>",
      "<p>Independientemente de la distribución original, la distribución de las medias muestrales",
      "se aproxima a una distribución normal cuando el tamaño de muestra es grande (n ≥ 30).</p>",
      "<p><strong>Ejemplo:</strong> Muestra cómo la media de", input$n_samples, "muestras de tamaño", 
      input$sample_size, "de una distribución", input$dist_type, "converge a la normalidad.</p>"
    ))
  })
  
  # ---------------------------
  # BASE DE DATOS DE TEORÍA
  # ---------------------------
  output$quant_theory <- renderUI({
    req(input$quant_test)
    
    theories <- list(
      "t-test" = list(
        title = "Prueba t de Student",
        content = HTML(paste(
          "<h4>Compara las medias de dos grupos independientes.</h4>",
          "<p><strong>Supuestos:</strong></p>",
          "<ul>",
          "<li>Normalidad de los datos</li>",
          "<li>Homogeneidad de varianzas</li>",
          "<li>Observaciones independientes</li>",
          "</ul>"
        ))
      ),
      "ANOVA" = list(
        title = "Análisis de Varianza",
        content = HTML(paste(
          "<h4>Compara las medias de tres o más grupos.</h4>",
          "<p><strong>Supuestos:</strong></p>",
          "<ul>",
          "<li>Normalidad de los datos</li>",
          "<li>Homogeneidad de varianzas</li>",
          "<li>Independencia de observaciones</li>",
          "</ul>"
        ))
      ),
      "Wilcoxon" = list(
        title = "Prueba de Wilcoxon",
        content = HTML(paste(
          "<h4>Versión no paramétrica de la prueba t.</h4>",
          "<p><strong>Usar cuando:</strong></p>",
          "<ul>",
          "<li>Los datos no son normales</li>",
          "<li>Muestras pequeñas</li>",
          "</ul>"
        ))
      ),
      "Pearson" = list(
        title = "Correlación de Pearson",
        content = HTML(paste(
          "<h4>Mide la relación lineal entre dos variables.</h4>",
          "<p><strong>Rango:</strong> -1 (negativa perfecta) a 1 (positiva perfecta)</p>"
        ))
      ),
      "Spearman" = list(
        title = "Correlación de Spearman",
        content = HTML(paste(
          "<h4>Versión no paramétrica de Pearson.</h4>",
          "<p>Usa rangos en lugar de valores originales.</p>"
        ))
      ),
      "Shapiro-Wilk" = list(
        title = "Prueba de Normalidad",
        content = HTML(paste(
          "<h4>Evalúa si los datos siguen una distribución normal.</h4>",
          "<p>H0: Los datos son normales</p>"
        ))
      )
    )
    
    if(input$quant_test %in% names(theories)) {
      theories[[input$quant_test]]$content
    } else {
      HTML("<p>Seleccione una prueba para ver su teoría.</p>")
    }
  })
  
  output$qual_theory <- renderUI({
    req(input$qual_test)
    
    theories <- list(
      "Chi-cuadrado" = list(
        title = "Prueba Chi-cuadrado",
        content = HTML(paste(
          "<h4>Prueba de independencia entre variables categóricas.</h4>",
          "<p><strong>Supuestos:</strong></p>",
          "<ul>",
          "<li>Frecuencias esperadas ≥ 5</li>",
          "<li>Observaciones independientes</li>",
          "</ul>"
        ))
      ),
      "Fisher" = list(
        title = "Prueba exacta de Fisher",
        content = HTML(paste(
          "<h4>Alternativa a Chi-cuadrado para muestras pequeñas.</h4>",
          "<p><strong>Usar cuando:</strong></p>",
          "<ul>",
          "<li>Frecuencias esperadas < 5</li>",
          "<li>Tablas 2x2</li>",
          "</ul>"
        ))
      ),
      "Binomial" = list(
        title = "Prueba Binomial",
        content = HTML(paste(
          "<h4>Compara proporciones observadas con las esperadas.</h4>",
          "<p>Útil para pruebas de proporciones simples.</p>"
        ))
      ),
      "Coef.Contingencia" = list(
        title = "Coeficiente de Contingencia",
        content = HTML(paste(
          "<h4>Mide la asociación entre variables categóricas.</h4>",
          "<p>Rango: 0 (no asociación) a 1 (asociación perfecta)</p>"
        ))
      )
    )
    
    if(input$qual_test %in% names(theories)) {
      theories[[input$qual_test]]$content
    } else {
      HTML("<p>Seleccione una prueba para ver su teoría.</p>")
    }
  })
  
  # ---------------------------
  # MANUAL DE USUARIO
  # ---------------------------
  output$manual_html <- renderUI({
    HTML("
      <h2>Guía de Usuario</h2>
      <h3>Carga de Datos</h3>
      <p>Formatos soportados: CSV, XLSX, TXT</p>
      <p>Especifique el separador y tipo de decimal según su archivo</p>
      
      <h3>Análisis Cuantitativo</h3>
      <ul>
        <li><strong>t-test:</strong> Comparación de medias entre 2 grupos</li>
        <li><strong>ANOVA:</strong> Comparación de medias entre 3+ grupos</li>
        <li><strong>Wilcoxon:</strong> Versión no paramétrica del t-test</li>
        <li><strong>Pearson:</strong> Correlación lineal</li>
        <li><strong>Spearman:</strong> Correlación no paramétrica</li>
        <li><strong>Shapiro-Wilk:</strong> Prueba de normalidad</li>
        <li><strong>Kolmogorov-Smirnov:</strong> Otra prueba de normalidad</li>
        <li><strong>Kruskal-Wallis:</strong> ANOVA no paramétrico</li>
        <li><strong>Friedman:</strong> ANOVA para medidas repetidas</li>
      </ul>
      
      <h3>Análisis Cualitativo</h3>
      <ul>
        <li><strong>Chi-cuadrado:</strong> Asociación entre variables categóricas</li>
        <li><strong>Fisher:</strong> Para muestras pequeñas</li>
        <li><strong>Binomial:</strong> Prueba de proporciones</li>
        <li><strong>Coef.Contingencia:</strong> Medida de asociación</li>
        <li><strong>G-test:</strong> Prueba de razón de verosimilitud</li>
        <li><strong>McNemar:</strong> Para datos pareados</li>
        <li><strong>Cochran-Q:</strong> Para múltiples muestras relacionadas</li>
      </ul>
      
      <h3>Interpretaciones</h3>
      <p>Todos los resultados incluyen interpretaciones automáticas usando el paquete <code>report</code>.</p>
    ")
  })
}

# ---------------------------
# EJECUTAR LA APLICACIÓN
# ---------------------------
shinyApp(ui, server)