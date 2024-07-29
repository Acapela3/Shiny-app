library(shiny)
library(stringr)
library(dplyr)
library(zoo)
library(ggplot2)
library(shinyFiles)
library(png)


source("funciones.R")
# Define UI for application that has multiple tabs
ui <- fluidPage(
  # Application title
  titlePanel("Levaduras y Campos"),
  
  # Tabs
  tabsetPanel(
    tabPanel("Datos",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Directorios"),
                 tags$hr(),
                 shinyDirButton("directorio_crudos", "Datos a curar", "Carpeta de datos a curar"),
                 shinyDirButton("directorio_curados", "Datos curados", "Carpeta de datos curados"),
                 tags$hr(),
                 shinyDirButton("directorio_promedios", "Promedios", "Selecciona un directorio"),
                 tags$hr(),
                 shinySaveButton("save_pendientes", "Guardar pendientes como...", "Seleccionar archivo RDS", filetype = list(rds = "rds")),
                 # Dentro de tabPanel "Datos"
                 actionButton("load_rds_btn", "Cargar archivo RDS"),
                 tags$hr(),
                 fileInput("parametros", "Obtener Parametros de un archivo CSV:"),
                 actionButton("load_btn", "Cargar archivo"),
                 tags$hr(),
                 actionButton("mostrar_directorios", "Mostrar Directorios"),
                 checkboxGroupInput(
                   inputId = "columnas",
                   label = "Seleccione nombres a ver:",
                   choices = NULL  # Inicialmente vacío, se llenará dinámicamente
                 )
               ),
               mainPanel(
                 actionButton("curar_datos", "Curar Datos"),
                 actionButton("cargar_curados", "Cargar datos Curados"),
                 actionButton("calculo_pendientes", "Calcular pendientes sin log"),
                 actionButton("calculo_pendientes_con_log", "Calcular pendientes con log"),
                 textOutput("texto_mostrado"),
                 br(),
                 tableOutput("tabla")
               )
             )),
    
    tabPanel("Graficas de pendientes",
             sidebarLayout(
               sidebarPanel(
                 textInput("sesiones", "Sesiones"),
                 actionButton("graficar_pendientes", "Graficar por sesiones"),
                 textInput("experimentos", "Experimentos"),
                 actionButton("graficar_pendientes_por_experimento", "Graficar por experimentos"),
                 tags$hr(),
                 shinySaveButton("save_pendientes_grafica", "Guardar gráfico", "Guardar gráfico como...", filetype = list(png = "png"))
               ),
               mainPanel(
                 plotOutput("grafica_output_pendientes")
               )
             )),
    
    tabPanel("Graficas temporales",
             sidebarLayout(
               sidebarPanel(
                 textInput("sesion", "Sesion"),
                 textInput("experimento", "Experimento"),
                 selectInput("selector_tipo_de_grafica", "Selecciona una opción:",
                             choices = c("CO2", "Log(CO2)","Derivada")),
                 actionButton("graficar_sesion_y_experimento", "Graficar"),
                 textInput("ventana_derivada", "Ventana de derivada"),
                 shinySaveButton("save_temporal", "Guardar gráfico", "Guardar gráfico como...", filetype = list(png = "png"))
                 # No hay controles en el panel lateral por ahora
               ),
               mainPanel(
                 plotOutput("grafica_output_temporales")
                 # No hay salida en el panel principal por ahora
               )
             ))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Variables globales para almacenar directorios
  directorio_pendientes <- reactiveVal(NULL)
  pendientes <- reactiveVal(NULL)
  tiempo_i <- reactiveVal(NULL)
  tiempo_f <- reactiveVal(NULL)
  global <- reactiveVal(NULL)
  sesiones <- reactiveValues(valor = NULL)
  experimentos <- reactiveValues(valor = NULL)
  experimento <- reactiveVal(NULL)
  sesion <- reactiveVal(NULL)
  grafica <- reactiveVal(NULL)
  parametros <- reactiveVal(NULL)
  ventana_derivada <- reactiveVal(NULL)
  
  
  # Directorios y funciones para obtenerlos
  roots <- c("C" = "C:/", "D" = "D:/")
  

  
  # Cargar el archivo RDS
  observeEvent(input$load_rds_btn, {
    showModal(modalDialog(
      title = "Seleccionar archivo RDS",
      fileInput("rds_file", "Selecciona un archivo RDS", accept = ".rds"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_load_rds", "Cargar")
      )
    ))
  })
  
  observeEvent(input$confirm_load_rds, {
    req(input$rds_file)
    file_path <- input$rds_file$datapath
    tryCatch({
      df <- readRDS(file_path)
      pendientes(df)  # Guarda el dataframe en la variable reactiva
      removeModal()
      showNotification("El archivo RDS ha sido cargado exitosamente.", type = "default")
    }, error = function(e) {
      showNotification("Error al cargar el archivo RDS. Verifique el archivo y vuelva a intentar.", type = "error")
    })
  })
  
  
  shinyFileSave(input, "save_pendientes", roots = roots, session = session)
  
  
  observeEvent(input$save_pendientes, {
    file_info <- parseSavePath(roots = roots, input$save_pendientes)
    if (nrow(file_info) > 0 && !is.null(file_info$datapath) && file_info$datapath != "") {
      file_path <- as.character(file_info$datapath)
      saveRDS(pendientes(), file = file_path)
      showNotification("El archivo de pendientes ha sido guardado exitosamente.", type = "default")
      output$file_path <- renderPrint({ file_path })
    } else {
      showNotification("El guardado del archivo RDS ha sido cancelado o no se ha seleccionado una ruta válida.", type = "error")
    }
  })
  
  
  
  
  # Configurar shinyFiles para acceder al sistema de archivos
  shinyFileSave(input, "save_pendientes_grafica", roots = roots, session = session)
  
  # Guardar el archivo en el directorio y con el nombre seleccionados por el usuario
  observeEvent(input$save_pendientes_grafica, {
    file_info <- parseSavePath(roots = roots, input$save_pendientes_grafica)
    if (nrow(file_info) > 0 && !is.null(file_info$datapath) && file_info$datapath != "") {
      file_path <- as.character(file_info$datapath)
      ggsave(file_path, plot = last_plot(), device = "png", width = 8, height = 6)
    } else {
      showNotification("El guardado ha sido cancelado o no se ha seleccionado una ruta válida.", type = "error")
    }
  })
  
  
  
  
  # Configurar shinyFiles para acceder al sistema de archivos
  shinyFileSave(input, "save_temporal", roots = roots, session = session)
  
  # Guardar el archivo en el directorio y con el nombre seleccionados por el usuario
  observeEvent(input$save_temporal, {
    file_info <- parseSavePath(roots = roots, input$save_temporal)
    if (nrow(file_info) > 0 && !is.null(file_info$datapath) && file_info$datapath != "") {
      file_path <- as.character(file_info$datapath)
      ggsave(file_path, plot = last_plot(), device = "png", width = 8, height = 6)
    } else {
      showNotification("El guardado ha sido cancelado o no se ha seleccionado una ruta válida.", type = "error")
    }
  })
  
  
  # Mostrar el data frame filtrado
  output$tabla <- renderTable({
    nombres_filtrados()
  })
  
  # Actualizar las opciones del checkboxGroupInput con los nombres de las columnas del data frame
  observe({
    updateCheckboxGroupInput(
      session,
      inputId = "columnas",
      choices = colnames(parametros()),
      selected = colnames(parametros())[1]  # Selecciona la primera columna por defecto
    )
  })
  
  # Filtrar el data frame basado en las selecciones del usuario
  nombres_filtrados <- reactive({
    req(input$columnas)  # Asegúrate de que haya selecciones
    parametros()[, input$columnas, drop = FALSE]
  })
  
  # Configurar y manejar directorios
  shinyDirChoose(input, "directorio_crudos", roots = roots, filetypes = c('', 'txt', 'csv'))
  shinyDirChoose(input, "directorio_curados", roots = roots, filetypes = c('', 'txt', 'csv'))
  shinyDirChoose(input, "directorio_graficas", roots = roots, filetypes = c('', 'txt', 'csv'))
  shinyDirChoose(input, "directorio_promedios", roots = roots, filetypes = c('', 'txt', 'csv'))
  
  directorio_crudos <- reactive({ parseDirPath(roots, input$directorio_crudos) })
  directorio_curados <- reactive({ parseDirPath(roots, input$directorio_curados) })
  directorio_graficas <- reactive({ parseDirPath(roots, input$directorio_graficas) })
  directorio_promedios <- reactive({ parseDirPath(roots, input$directorio_promedios) })
  
  output$directorio_crudos <- renderPrint({ directorio_crudos() })
  output$directorio_curados <- renderPrint({ directorio_curados() })
  output$directorio_graficas <- renderPrint({ directorio_graficas() })
  output$directorio_promedios <- renderPrint({ directorio_promedios() })
  
  # Obtener el archivo de parámetros
  observeEvent(input$load_btn, {
    req(input$parametros)  # Verifica que se haya seleccionado un archivo
    df <- read.csv(input$parametros$datapath, stringsAsFactors = FALSE)
    parametros(df)  # Guarda el dataframe en la variable reactiva
  })
  
  # Mostrar directorios al presionar el botón
  observeEvent(input$mostrar_directorios, {
    output$texto_mostrado <- renderText({
      paste("Directorio de datos Crudos:", directorio_crudos(), 
            "\nDirectorio Curados:", directorio_curados(),
            "\nDirectorio de Graficas:", directorio_graficas(),
            "\nDirectorio de Promedios:", directorio_promedios())
    })
  })
  
  # Curar datos
  observeEvent(input$curar_datos, {
    curar_y_exportar_datos_crudos(directorio_crudos(), directorio_curados())
    showModal(modalDialog(
      title = "Datos Curadoss",
      "Los datos han sido curados exitosamente.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Cargar datos curados
  observeEvent(input$cargar_curados, {
    global_data <- cargar_datos_curados_para_usar(directorio_curados())
    global(global_data)  # Actualiza el valor de global con los datos cargados
    showModal(modalDialog(
      title = "Datos Cargados",
      "Los datos curados han sido cargados exitosamente.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Calcular pendientes
  observeEvent(input$calculo_pendientes, {
    showModal(modalDialog(
      title = "Ingrese el intervalo de tiempo",
      numericInput("tiempo_inicial", "Tiempo inicial:", value = 1),
      numericInput("tiempo_final", "Tiempo final:", value = 1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    ))
  })
  
  observeEvent(input$ok, {
    removeModal()
    tiempo_i(input$tiempo_inicial)
    tiempo_f(input$tiempo_final)
    datos <- global()
    pendientes_calculadas <- crear_modelos_promediar_pendientes_y_normalizarlas(datos, tiempo_i(), tiempo_f())
    pendientes(pendientes_calculadas)  # Actualiza el valor de pendientes con los datos procesados
    showModal(modalDialog(
      title = "Pendientes Calculadas",
      "Las pendientes han sido calculadas exitosamente.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  
  # Calcular pendientes
  observeEvent(input$calculo_pendientes_con_log, {
    showModal(modalDialog(
      title = "Ingrese el intervalo de tiempo",
      numericInput("tiempo_inicial", "Tiempo inicial:", value = 1),
      numericInput("tiempo_final", "Tiempo final:", value = 1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_2", "OK")
      )
    ))
  })
  
  observeEvent(input$ok_2, {
    removeModal()
    tiempo_i(input$tiempo_inicial)
    tiempo_f(input$tiempo_final)
    datos <- global()
    pendientes_calculadas <- crear_modelos_promediar_pendientes_y_normalizarlas_con_log(datos, tiempo_i(), tiempo_f())
    pendientes(pendientes_calculadas)  # Actualiza el valor de pendientes con los datos procesados
    showModal(modalDialog(
      title = "Pendientes Calculadas",
      "Las pendientes han sido calculadas exitosamente.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  
  
  # Graficar pendientes
  observeEvent(input$graficar_pendientes, {
    sesiones_guardadas <- trimws(unlist(strsplit(input$sesiones, ",")))
    sesiones$valor <- sesiones_guardadas
    sesion_a_graficar <- sesiones$valor
    datos_a_usar <- pendientes()
    nombres_df <- nombres_filtrados()
    output$grafica_output_pendientes <- renderPlot({
      graficar_pendientes_normalizadas(sesiones_guardadas, datos_a_usar, nombres_df)
    })
  })
  # Graficar pendientes por experimento
  observeEvent(input$graficar_pendientes_por_experimento, {
    experimentos_guardados <- trimws(unlist(strsplit(input$experimentos, ",")))
    experimentos$valor <- experimentos_guardados
    experimento_a_graficar <- experimentos$valor
    datos_a_usar <- pendientes()
    nombres_df <- nombres_filtrados()
    output$grafica_output_pendientes <- renderPlot({
      graficar_pendientes_normalizadas_por_experimento(experimento_a_graficar, datos_a_usar, nombres_df)
    })
  })
  
  
  
  
  
  
  
  
    
  # Graficar según opción seleccionada
  observeEvent(input$graficar_sesion_y_experimento, {
    experimento(input$experimento)
    sesion(input$sesion)
    opcion_seleccionada <- input$selector_tipo_de_grafica
    global_data_value <- global()
    sesion_seleccionada <- sesion()
    experimento_seleccionado <- experimento()
    
    if (!is.null(global_data_value) && !is.null(sesion_seleccionada) && !is.null(experimento_seleccionado)) {
      if (opcion_seleccionada == "CO2") {
          
            output$grafica_output_temporales <- renderPlot({
            lista_dataframes <- global_data_value[[sesion_seleccionada]][[experimento_seleccionado]]
            graficar_datos_combinados(lista_dataframes, "CO2_Measure.ppm_adjusted",
                                      expression(paste(CO[2], " ajustado (ppm)")),
                                      expression(paste("Grafica de ", CO[2], " ajustado (ppm)")))
          })
        
      } else if (opcion_seleccionada == "Log(CO2)") {
       
            output$grafica_output_temporales <- renderPlot({
            lista_dataframes <- global_data_value[[sesion_seleccionada]][[experimento_seleccionado]]
            graficar_datos_combinados(lista_dataframes, "log_CO2_adjusted",
                                      expression(paste("Logaritmo de ", CO[2], " ajustado (ppm)")),
                                      expression(paste("Grafica de logaritmo de ", CO[2], " ajustado (ppm)")))
          })
        
      }else if (opcion_seleccionada == "Derivada") {
        output$grafica_output_temporales <- renderPlot({
          ventana_derivada(input$ventana_derivada)
          wz <-as.integer(ventana_derivada())
          lista_dataframes <- global_data_value[[sesion_seleccionada]][[experimento_seleccionado]]
          lista_dataframes <- lapply(lista_dataframes, aplicar_derivada, wz = wz)
          graficar_datos_combinados(lista_dataframes, "Derivada",
                                    expression(paste("Derivada ", CO[2], " ajustado (ppm)")),
                                    expression(paste("Grafica de Derivada de ", CO[2], " ajustado (ppm)")))
        })
        
      } 
      
      
      
      
      else {
        print("Selecciona una opción válida para graficar.")
      }
    } else {
      print("Selecciona una sesión y un experimento antes de graficar.")
    }
  })
}

# Combina UI y server en una aplicación Shiny
shinyApp(ui = ui, server = server)
