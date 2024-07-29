### FUNCIONES ###



obtener_tabla_de_datos <- function(directorio) {
  # Obtener los nombres de los archivos .txt en el directorio
  archivos <- list.files(directorio, pattern = "\\.TXT$", full.names = TRUE)
  
  # Inicializar vectores para cada columna del data frame
  sesiones <- c()
  experimentos <- c()
  replicas <- c()
  
  # Extraer los números de los nombres de archivo
  for (archivo in archivos) {
    sesion <- as.numeric(gsub("^.*S(\\d+)_.*$", "\\1", archivo))
    experimento <- as.numeric(gsub("^.*E(\\d+)_.*$", "\\1", archivo))
    replica <- as.numeric(gsub("^.*_(\\d+)\\.TXT$", "\\1", archivo))
    
    # Añadir los números a los vectores correspondientes
    sesiones <- c(sesiones, sesion)
    experimentos <- c(experimentos, experimento)
    replicas <- c(replicas, replica)
  }
  
  # Crear el data frame
  df_resultado <- data.frame(
    Sesion = sesiones,
    Experimento = experimentos,
    Replica = replicas,
    stringsAsFactors = FALSE
  )
  
  # Devolver el data frame resultante
  return(df_resultado)
}



#########################################



crear_lista_dataframes <- function(directorio) {
  # Obtener los nombres de los archivos .txt en el directorio
  archivos <- list.files(directorio, pattern = "\\.TXT$", full.names = TRUE)
  
  # Inicializar una lista vacía para almacenar los data frames
  lista_de_dataframes <- list()
  
  # Leer cada archivo .txt y almacenarlo en la lista
  for (archivo in archivos) {
    # Obtener el nombre del archivo sin la extensión
    nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
    
    # Leer el archivo .txt en un data frame
    df <- tryCatch({
      read.table(archivo, header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)
    }, error = function(e) {
      warning(paste("Error leyendo el archivo:", archivo, "-", e$message))
      return(NULL)
    })
    
    # Comprobar si el data frame fue leído correctamente
    if (!is.null(df)) {
      # Verificar y ajustar el número de columnas
      num_columns <- ncol(df)
      df <- df[, 1:num_columns, drop = FALSE]
      
      # Agregar el data frame a la lista con el nombre del archivo
      lista_de_dataframes[[nombre_archivo]] <- df
    }
  }
  
  # Devolver la lista de data frames
  return(lista_de_dataframes)
}


##################################



# Función para reestructurar una lista de data frames por sesión principal Sxxx
reestructurar_por_sesion <- function(lista_datos) {
  # Obtener los nombres de los data frames en la lista
  nombres <- names(lista_datos)
  
  # Crear una lista vacía para almacenar los data frames agrupados por sesión
  sesiones <- list()
  
  # Iterar sobre los nombres de los data frames
  for (nombre in nombres) {
    # Extraer el código de sesión principal Sxxx
    sesion <- gsub("^S(\\d+)_.*$", "S\\1", nombre)
    
    # Verificar si la sesión ya existe en la lista, si no, crearla
    if (!(sesion %in% names(sesiones))) {
      sesiones[[sesion]] <- list()
    }
    
    # Agregar el data frame al grupo correspondiente de la sesión
    sesiones[[sesion]][[nombre]] <- lista_datos[[nombre]]
  }
  
  # Retornar la lista reestructurada por sesiones
  return(sesiones)
}





#####################################################





# Función para reestructurar una lista de data frames por sesión y experimento
reestructurar_por_sesion_y_experimento <- function(lista_datos) {
  # Obtener los nombres de los data frames en la lista
  nombres <- names(lista_datos)
  
  # Crear una lista vacía para almacenar los data frames agrupados por sesión y experimento
  sesiones <- list()
  
  # Iterar sobre los nombres de los data frames
  for (nombre in nombres) {
    # Extraer la sesión Sxxx y el experimento Exxx del nombre del data frame
    sesion <- gsub("^S(\\d+)_E(\\d+)_.*$", "S\\1", nombre)
    experimento <- gsub("^S(\\d+)_E(\\d+)_.*$", "E\\2", nombre)
    
    # Verificar si la sesión ya existe en la lista, si no, crearla
    if (!(sesion %in% names(sesiones))) {
      sesiones[[sesion]] <- list()
    }
    
    # Verificar si el experimento ya existe en la sesión, si no, crearlo
    if (!(experimento %in% names(sesiones[[sesion]]))) {
      sesiones[[sesion]][[experimento]] <- list()
    }
    
    # Agregar el data frame al grupo correspondiente de la sesión y experimento
    sesiones[[sesion]][[experimento]][[nombre]] <- lista_datos[[nombre]]
  }
  
  # Verificar y notificar si hay data frames con nombres no esperados
  for (nombre in nombres) {
    if (!grepl("^S\\d+_E\\d+_.*$", nombre)) {
      warning(paste("El nombre del data frame", nombre, "no sigue el patrón esperado. Se omitirá."))
    }
  }
  
  # Retornar la lista reestructurada por sesiones y experimentos
  return(sesiones)
}


##########################################################



# Función para rellenar los huecos en el tiempo y extrapolar valores
fill_gaps <- function(df) {
  # Crear una secuencia completa de tiempo hasta el segundo 600
  full_time <- data.frame(Time.s. = 0:600)
  
  # Hacer un merge con la secuencia completa de tiempo
  df_full <- full_time %>%
    left_join(df, by = "Time.s.") %>%
    select(Time.s., CO2_Measure.ppm = CO2_Measure.ppm.)  # Seleccionar solo la columna CO2_Measure.ppm con los huecos llenos
  
  # Extrapolar valores NA en la columna CO2_Measure.ppm
  df_full$CO2_Measure.ppm <- zoo::na.approx(df_full$CO2_Measure.ppm, rule = 2)
  
  return(df_full)
}


###########################################################



# Supongamos que ya tienes datos_completos con los data frames llenos y sin la columna de CO2 con los huecos

# Función para aplicar logaritmo a la columna CO2_Measure.ppm y crear una nueva columna
apply_log <- function(df) {
  # Aplicar logaritmo a la columna CO2_Measure.ppm
  df$log_CO2 <- log(df$CO2_Measure.ppm)
  
  return(df)
}


###########################################################



# Supongamos que ya tienes datos_completos con los data frames llenos y sin la columna de CO2 con los huecos

# Función para aplicar logaritmo a la columna CO2_Measure.ppm y crear una nueva columna
aplicar_derivada <- function(df,wz) {
  
  df <- df[-c(1:50), ]
  # Define el tamaño de la ventana para el promedio móvil
  window_size <- wz  # Puedes ajustar este valor según sea necesario
  
  # Calcula el promedio móvil, alineando el resultado al centro
  df$CO2_Measure_Smoothed <- rollmean(df$CO2_Measure.ppm, k = window_size, fill = NA, align = "right")
  
  
  df$Derivada <- c((diff(df$CO2_Measure_Smoothed)/diff(df$Time.s.)), NA)
  
  return(df)
}





##############################################################



# Supongamos que ya tienes datos_con_log con los data frames que contienen las columnas log_CO2 y CO2_Measure.ppm

# Función para ajustar la columna CO2_Measure.ppm y log_CO2
adjust_values <- function(df) {
  # Tomar el primer valor de CO2_Measure.ppm
  first_CO2_value <- df$CO2_Measure.ppm[1]
  
  # Restar el primer valor de CO2_Measure.ppm al resto de valores en esa columna
  df$CO2_Measure.ppm_adjusted <- df$CO2_Measure.ppm - first_CO2_value
  
  # Tomar el primer valor de log_CO2
  first_log_value <- df$log_CO2[1]
  
  # Restar el primer valor de log_CO2 al resto de valores en esa columna
  df$log_CO2_adjusted <- df$log_CO2 - first_log_value
  
  return(df)
}




##############################################################









exportar_data_frames <- function(lista_data_frames, directorio_salida) {
  # Función para exportar cada data frame a un archivo txt
  exportar_txt <- function(df, nombre_archivo, directorio) {
    # Construir el path completo del archivo
    path <- file.path(directorio, paste0(nombre_archivo, ".TXT"))
    
    # Escribir el data frame en un archivo txt con espacios como separadores
    write.table(df, file = path, sep = " ", row.names = FALSE)
  }
  
  # Iterar sobre cada data frame en la lista y exportarlo como txt
  for (i in seq_along(lista_data_frames)) {
    exportar_txt(lista_data_frames[[i]], names(lista_data_frames)[i], directorio_salida)
  }
}




#######################################################


# Definir función para graficar datos combinados de varios data frames
graficar_datos_combinados <- function(lista_dataframes, variable_y, titulo_y,titulo_grafica) {
  
  # Extraer los últimos xx del nombre del data frame como Replica
  nombres_replicas <- sapply(names(lista_dataframes), function(x) {
    str_match(x, "S\\d+_E\\d+_(\\d+)")[,2]
  })
  
  # Combinar todos los data frames en uno solo con una columna de identificación
  datos_combinados <- bind_rows(lapply(seq_along(lista_dataframes), function(i) {
    mutate(lista_dataframes[[i]], Replica = nombres_replicas[i])
  }), .id = "data_frame") %>%
    mutate(Replica = factor(Replica))  # Convertir a factor para ggplot
  
  # Graficar usando ggplot
  ggplot(datos_combinados, aes(x = Time.s., y = !!sym(variable_y), color = Replica)) +
    geom_line(linewidth = 1.2) +
    labs(
      x = "Tiempo (s)",
      y = titulo_y,
      title = titulo_grafica  # Convertir titulo_y a texto con as.character()
    ) +
    scale_color_discrete(name = "Réplica") +
    theme_classic() +
    theme(axis.title = element_text(size = 15,
                                    face = "bold"),
          axis.line = element_line(linewidth = 1.2,
                                   linetype = 1),
          axis.text = element_text( face = "bold",size = 12),
          legend.title = element_text(face = "bold",size = 14),
          legend.text = element_text(face = "bold",size = 13),
          legend.position = c(0.2,0.75),
          legend.background = element_rect(fill = "#ADD8E6", color = "black", size = 0.5), # Fondo azul claro con borde negro
          legend.key = element_rect(fill = "white", color = "black"),
          axis.ticks = element_line(color = 1,
                                    linewidth = 1.5),
          axis.ticks.length = unit(0.25, "cm"),    # Longitud positiva para los ticks hacia afuera
    )
  
  
  
  }









#################################################################################







calculate_means_and_sds <- function(df_list) {
  combined_df <- bind_rows(df_list)
  
  means_df <- combined_df %>%
    group_by(Time.s.) %>%
    summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)), .names = "{col}_{fn}"))
  
  return(means_df)
}





apply_nested_list <- function(nested_list) {
  lapply(nested_list, function(inner_list) {
    lapply(inner_list, function(df_list) {
      calculate_means_and_sds(df_list)
    })
  })
}





###############################################################################



# Función para aplanar una lista de listas
aplanar_lista <- function(lista_anidada) {
  do.call(c, lista_anidada)
}

# Función para cambiar los nombres de los data frames
cambiar_nombres <- function(lista_df) {
  # Obtener los nombres actuales de la lista
  nombres_actuales <- names(lista_df)
  
  # Crear nombres nuevos reemplazando puntos por guiones bajos
  nombres_nuevos <- gsub("\\.", "_", nombres_actuales)
  
  # Asignar los nuevos nombres a la lista de data frames
  names(lista_df) <- nombres_nuevos
  
  return(lista_df)
}






# Función para reestructurar una lista de data frames por sesión
reestructurar_por_sesion <- function(lista_datos) {
  # Obtener los nombres de los data frames en la lista
  nombres <- names(lista_datos)
  
  # Crear una lista vacía para almacenar los data frames agrupados por sesión
  sesiones <- list()
  
  # Iterar sobre los nombres de los data frames
  for (nombre in nombres) {
    # Extraer la sesión Sxxx del nombre del data frame
    sesion <- gsub("^S(\\d+)_.*$", "S\\1", nombre)
    
    # Verificar si la sesión ya existe en la lista, si no, crearla
    if (!(sesion %in% names(sesiones))) {
      sesiones[[sesion]] <- list()
    }
    
    # Agregar el data frame al grupo correspondiente de la sesión
    sesiones[[sesion]][[nombre]] <- lista_datos[[nombre]]
  }
  
  # Verificar y notificar si hay data frames con nombres no esperados
  for (nombre in nombres) {
    if (!grepl("^S\\d+_", nombre)) {
      warning(paste("El nombre del data frame", nombre, "no sigue el patrón esperado. Se omitirá."))
    }
  }
  
  # Retornar la lista reestructurada por sesiones
  return(sesiones)
}










##############################################################################






# Definir función para graficar datos combinados de varios data frames y exportar la gráfica
graficar_datos_combinados_y_exportarlos <- function(lista_dataframes, variable_y, titulo_y, nombre_archivo, directorio_salida) {
  
  # Extraer los últimos xx del nombre del data frame como Replica
  nombres_replicas <- sapply(names(lista_dataframes), function(x) {
    str_match(x, "S\\d+_E\\d+_(\\d+)")[,2]
  })
  
  # Combinar todos los data frames en uno solo con una columna de identificación
  datos_combinados <- bind_rows(lapply(seq_along(lista_dataframes), function(i) {
    mutate(lista_dataframes[[i]], Replica = nombres_replicas[i])
  }), .id = "data_frame") %>%
    mutate(Replica = factor(Replica))  # Convertir a factor para ggplot
  
  # Graficar usando ggplot
  p <- ggplot(datos_combinados, aes(x = Time.s., y = !!sym(variable_y), color = Replica)) +
    geom_line() +
    labs(x = "Tiempo (s)", y = titulo_y, title = paste("Gráfico de", titulo_y)) +
    scale_color_discrete(name = "Réplica")  # Cambiar título de la leyenda por "Réplica"
  
  # Guardar la gráfica en un archivo
  ggsave(file.path(directorio_salida, paste0(nombre_archivo, ".png")), plot = p)
  
  # Opcional: Imprimir mensaje de confirmación
  cat("Gráfica guardada exitosamente en", file.path(directorio_salida, paste0(nombre_archivo, ".png")), "\n")
  
  # Retornar el objeto de la gráfica (opcional)
  return(p)
}



################################################################################


# Definir función para regresión lineal ponderada
regresion_ponderada <- function(datos, var_dependiente, var_independiente, var_desviacion) {
  
  # Calcular los pesos inversamente proporcionales al cuadrado de la desviación estándar
  pesos <- 1 / (datos[[var_desviacion]])^2
  
  # Ajustar el modelo de regresión ponderada
  modelo <- lm(as.formula(paste(var_dependiente, "~", var_independiente)), 
               data = datos, weights = pesos)
  
  # Retornar el modelo ajustado
  return(modelo)
}



###############################################################################
curar_y_exportar_datos_crudos <- function(directorio_entrada,directorio_salida){
  datos <- crear_lista_dataframes(directorio_entrada)
  datos_completos <- lapply(datos, fill_gaps)
  # Aplicar la función a cada data frame en la lista datos_completos
  datos_con_log <- lapply(datos_completos, apply_log)
  # Aplicar la función a cada data frame en la lista datos_con_log
  datos_ajustados <- lapply(datos_con_log, adjust_values)
  exportar_data_frames(datos_ajustados,directorio_salida)
  
}

################################################################################


cargar_datos_curados_para_usar <- function(directorio_entrada){
  
  datos_curados <- crear_lista_dataframes(directorio_entrada)
  
  global <- reestructurar_por_sesion_y_experimento(datos_curados)
  
  return(global)
}


################################################################################

crear_modelos_promediar_pendientes_y_normalizarlas <- function(data,t_i,t_f){
  
  
  # Inicializar una lista para almacenar los modelos
  resultados_modelos <- list()
  
  # Recorremos la lista global
  for (session in names(data)) {
    resultados_modelos[[paste0( session)]] <- list() 
    for (experiment in names(data[[session]])) {
      resultados_modelos[[paste0( session)]][[paste0( experiment)]] <- list() 
      for (replica in names(data[[session]][[experiment]])){
        datos_a_usar <- data[[session]][[experiment]][[replica]]
        datos_intervalo <- datos_a_usar[datos_a_usar$Time.s. >= t_i & datos_a_usar$Time.s. <= t_f, ]
        # Realizar la regresión lineal
        modelo <- lm(CO2_Measure.ppm_adjusted ~ Time.s., data = datos_intervalo)
        # Guardar resumen del modelo en resultados_modelos
        nombre_modelo <- paste( session,"_",  experiment, "_", replica, sep = "")
        resultados_modelos[[paste0( session)]][[paste0( experiment)]][[replica]] <- summary(modelo)
      }
    }
  }
  
  # Crear una lista para almacenar los resultados
  resultados_pendientes <- list()
  
  # Recorrer la lista de modelos y almacenar resultados
  for (session in names(resultados_modelos)) {
    for (experiment in names(resultados_modelos[[session]])) {
      pendientes <- sapply(resultados_modelos[[session]][[experiment]], function(modelo) coef(modelo)[2])
      # Convertir a data frame y agregar columnas de session y experiment
      df <- data.frame(Replica = names(pendientes), Pendiente = pendientes, Sesion = session, Experimento = experiment, stringsAsFactors = FALSE)
      # Extraer solo los últimos números después del último "_"
      df$Replica <- gsub(".*_(\\d+)$", "\\1", df$Replica)
      # Almacenar en la lista de resultados
      resultados_pendientes[[length(resultados_pendientes) + 1]] <- df
    }
  }
  
  # Combinar todos los data frames en uno solo
  resultado_final <- do.call(rbind, resultados_pendientes)
  
  
  
  # Inicializar una lista para almacenar las pendientes normalizadas por sesión
  resultados_pendientes_normalizados <- list()
  
  # Recorrer cada sesión única en resultado_final
  for (session in unique(resultado_final$Sesion)) {
    
    # Calcular promedio de las pendientes de E011 para la sesión actual
    promedio_E011 <- mean(resultado_final$Pendiente[resultado_final$Sesion == session & resultado_final$Experimento == "E011"])
    
    # Filtrar las filas correspondientes a la sesión actual
    session_rows <- resultado_final$Sesion == session
    
    # Recorrer las filas de la sesión actual y almacenar resultados normalizados
    for (row in which(session_rows)) {
      experiment <- resultado_final$Experimento[row]
      pendiente <- resultado_final$Pendiente[row]
      replica <- resultado_final$Replica[row]
      
      pendiente_normalizada <- pendiente / promedio_E011
      
      # Extraer solo los últimos números después del último "_"
      replica <- gsub(".*_(\\d+)$", "\\1", replica)
      
      # Almacenar en la lista de resultados normalizados por sesión
      resultados_pendientes_normalizados[[length(resultados_pendientes_normalizados) + 1]] <- data.frame(Sesion = session, Experimento = experiment, Replica = replica, Pendiente_Normalizada = pendiente_normalizada)
    }
  }
  
  # Combinar todos los data frames en uno solo
  resultado_final_normalizado <- do.call(rbind, resultados_pendientes_normalizados)
  
  
  return(resultado_final_normalizado)
}

################################################################################

 
graficar_pendientes_normalizadas <- function(session_a_graficar,datos,nombres_experimentos){
  
  nombres_experimentos <- nombres_experimentos %>%
    rowwise() %>%
      mutate(Nombre_Combinado = paste(c_across(-Experimento), collapse = "  "))
  nombres_experimentos$Experimento <- as.numeric(nombres_experimentos$Experimento)
  # Filtrar los datos por las sesiones específicas
  datos_filtrados <- datos[datos$Sesion %in% session_a_graficar, ]
  # Extraer solo los números después de la 'E' y convertirlos a numérico
  datos_filtrados$Experimento <- as.numeric(sub("E", "", datos_filtrados$Experimento))
  
  datos_filtrados <- merge(datos_filtrados, nombres_experimentos, by = "Experimento")
  datos_filtrados$Experimento <- paste0("E", as.character(datos_filtrados$Experimento))
  
  # Graficar boxplot y puntos  
  ggplot(datos_filtrados, aes(x = Nombre_Combinado, y = Pendiente_Normalizada, fill = Experimento)) +
    geom_hline(yintercept = 1,   # Agregar la línea horizontal en y = 1
               color = "gray",    # Color de la línea
               linetype = "dashed") + 
    stat_boxplot(geom = "errorbar", # Error bars
                 width = 0.2,
                 linewidth = 0.8)+
    geom_boxplot(alpha = 1,width = 0.5) +  # Añadir transparencia a los boxplots
    geom_point(position = position_jitterdodge(), size = 2, alpha = 0.7) +  # Agregar puntos con jitter para evitar superposiciones
    labs(title = paste("Boxplot y Puntos de Pendientes Normalizadas para", session_a_graficar),
         x = "Experimento",
         y = "Pendiente Normalizada") +
    theme_classic()+
    scale_fill_brewer(palette = "PuBuGn")+
    theme(axis.title = element_text(size = 12,
                                    face = "bold"),
          axis.line = element_line(linewidth = 1.2,
                                   linetype = 1),
          axis.text = element_text( face = "bold",size = 11)
    )+

    facet_wrap(~ Sesion)
  
}


################################################################################





graficar_pendientes_normalizadas_por_experimento <- function(experimento_a_graficar,datos,nombres_experimentos){
  
  nombres_experimentos <- nombres_experimentos %>%
    rowwise() %>%
    mutate(Nombre_Combinado = paste(c_across(-Experimento), collapse = "  "))
  nombres_experimentos$Experimento <- as.numeric(nombres_experimentos$Experimento)
  # Filtrar los datos por las sesiones específicas
  datos_filtrados <- datos[datos$Experimento %in% experimento_a_graficar, ]
  # Extraer solo los números después de la 'E' y convertirlos a numérico
  datos_filtrados$Experimento <- as.numeric(sub("E", "", datos_filtrados$Experimento))
  
  datos_filtrados <- merge(datos_filtrados, nombres_experimentos, by = "Experimento")
  datos_filtrados$Experimento <- paste0("E", as.character(datos_filtrados$Experimento))
  
  # Graficar boxplot y puntos  
  ggplot(datos_filtrados, aes(x = Nombre_Combinado, y = Pendiente_Normalizada, fill = Experimento)) +
    geom_hline(yintercept = 1,   # Agregar la línea horizontal en y = 1
               color = "gray",    # Color de la línea
               linetype = "dashed") + 
    stat_boxplot(geom = "errorbar", # Error bars
                 width = 0.2,
                 linewidth = 0.8)+
    geom_boxplot(alpha = 1,width = 0.5) +  # Añadir transparencia a los boxplots
    geom_point(position = position_jitterdodge(), size = 2, alpha = 0.7) +  # Agregar puntos con jitter para evitar superposiciones
    labs(title = paste("Boxplot y Puntos de Pendientes Normalizadas para", experimento_a_graficar),
         x = "Experimento",
         y = "Pendiente Normalizada") +
    theme_classic()+
    scale_fill_brewer(palette = "PuBuGn")+
    theme(axis.title = element_text(size = 12,
                                    face = "bold"),
          axis.line = element_line(linewidth = 1.2,
                                   linetype = 1),
          axis.text = element_text( face = "bold",size = 11)
    )

}


################################################################################

crear_modelos_promediar_pendientes_y_normalizarlas_con_log <- function(data,t_i,t_f){
  
  
  # Inicializar una lista para almacenar los modelos
  resultados_modelos <- list()
  
  # Recorremos la lista global
  for (session in names(data)) {
    resultados_modelos[[paste0( session)]] <- list() 
    for (experiment in names(data[[session]])) {
      resultados_modelos[[paste0( session)]][[paste0( experiment)]] <- list() 
      for (replica in names(data[[session]][[experiment]])){
        datos_a_usar <- data[[session]][[experiment]][[replica]]
        datos_intervalo <- datos_a_usar[datos_a_usar$Time.s. >= t_i & datos_a_usar$Time.s. <= t_f, ]
        # Realizar la regresión lineal
        modelo <- lm(log_CO2_adjusted ~ Time.s., data = datos_intervalo)
        # Guardar resumen del modelo en resultados_modelos
        nombre_modelo <- paste( session,"_",  experiment, "_", replica, sep = "")
        resultados_modelos[[paste0( session)]][[paste0( experiment)]][[replica]] <- summary(modelo)
      }
    }
  }
  
  # Crear una lista para almacenar los resultados
  resultados_pendientes <- list()
  
  # Recorrer la lista de modelos y almacenar resultados
  for (session in names(resultados_modelos)) {
    for (experiment in names(resultados_modelos[[session]])) {
      pendientes <- sapply(resultados_modelos[[session]][[experiment]], function(modelo) coef(modelo)[2])
      # Convertir a data frame y agregar columnas de session y experiment
      df <- data.frame(Replica = names(pendientes), Pendiente = pendientes, Sesion = session, Experimento = experiment, stringsAsFactors = FALSE)
      # Extraer solo los últimos números después del último "_"
      df$Replica <- gsub(".*_(\\d+)$", "\\1", df$Replica)
      # Almacenar en la lista de resultados
      resultados_pendientes[[length(resultados_pendientes) + 1]] <- df
    }
  }
  
  # Combinar todos los data frames en uno solo
  resultado_final <- do.call(rbind, resultados_pendientes)
  
  
  
  # Inicializar una lista para almacenar las pendientes normalizadas por sesión
  resultados_pendientes_normalizados <- list()
  
  # Recorrer cada sesión única en resultado_final
  for (session in unique(resultado_final$Sesion)) {
    
    # Calcular promedio de las pendientes de E011 para la sesión actual
    promedio_E011 <- mean(resultado_final$Pendiente[resultado_final$Sesion == session & resultado_final$Experimento == "E011"])
    
    # Filtrar las filas correspondientes a la sesión actual
    session_rows <- resultado_final$Sesion == session
    
    # Recorrer las filas de la sesión actual y almacenar resultados normalizados
    for (row in which(session_rows)) {
      experiment <- resultado_final$Experimento[row]
      pendiente <- resultado_final$Pendiente[row]
      replica <- resultado_final$Replica[row]
      
      pendiente_normalizada <- pendiente / promedio_E011
      
      # Extraer solo los últimos números después del último "_"
      replica <- gsub(".*_(\\d+)$", "\\1", replica)
      
      # Almacenar en la lista de resultados normalizados por sesión
      resultados_pendientes_normalizados[[length(resultados_pendientes_normalizados) + 1]] <- data.frame(Sesion = session, Experimento = experiment, Replica = replica, Pendiente_Normalizada = pendiente_normalizada)
    }
  }
  
  # Combinar todos los data frames en uno solo
  resultado_final_normalizado <- do.call(rbind, resultados_pendientes_normalizados)
  
  
  return(resultado_final_normalizado)
}


