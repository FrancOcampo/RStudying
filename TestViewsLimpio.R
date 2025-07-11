##Borra el environment, plots y viewer
base::rm(list = base::ls()); grDevices::graphics.off() # BORRA ENVIRONMENT, PLOTS Y VIEWER

## LIBRERIAS NECESARIAS ####
library(readxl)
library(dygraphs)
library(htmlwidgets)
library(rstudioapi)

## CONFIGURACION DE SLOTS
ruta_base_slots <- file.path(tempdir(), "slots") # Cambia a una ruta temporal para evitar problemas de permisos
#ruta_base_slots <- "T:/_Franco/slots"
dir.create(ruta_base_slots, showWarnings = FALSE)

# Definir los slots visuales
viewer_slots <- list(
  slot1 = file.path(ruta_base_slots, "slot1.html"),
  slot2 = file.path(ruta_base_slots, "slot2.html"),
  slot3 = file.path(ruta_base_slots, "slot3.html"),
  slot4 = file.path(ruta_base_slots, "slot4.html")
)

mostrar_en_slot <- function(nombre_slot, widget) {
  path <- viewer_slots[[nombre_slot]]
  if (is.null(path)) stop("Slot no definido: ", nombre_slot)
  
  # Guardar widget como HTML no-selfcontained
  htmlwidgets::saveWidget(widget, file = path, selfcontained = FALSE)
  
  # Inyectar script JS para autorecarga cada N segundos
  lines <- readLines(path, warn = FALSE)
  
  reload_script <- '
  <script>
  setInterval(function() {
    fetch(window.location.href, {cache: "no-store"})
      .then(resp => resp.text())
      .then(html => {
        if (!document.documentElement.isEqualNode(new DOMParser().parseFromString(html, "text/html").documentElement)) {
          location.reload();
        }
      });
  }, 500); // cada 3 segundos
  </script>
  </body>'
  
  # Reemplaza </body> con script + </body>
  lines <- sub("</body>", reload_script, lines, ignore.case = TRUE)
  writeLines(lines, path)
  
  # Solo abrimos una vez por slot
  flag <- paste0(".viewer_abierto_", nombre_slot)
  
  if (!exists(flag, envir = .GlobalEnv)) {
    rstudioapi::viewer(path)
    assign(flag, TRUE, envir = .GlobalEnv)
  }
}

siguiente_slot <- function() {
  # Obtener nombres de los slots definidos
  nombres_slots <- names(viewer_slots)
  total_slots <- length(nombres_slots)
  
  # Variable global para seguir la posición
  if (!exists(".indice_slot_actual", envir = .GlobalEnv)) {
    assign(".indice_slot_actual", 1, envir = .GlobalEnv)
  }
  
  # Obtener índice actual
  i <- get(".indice_slot_actual", envir = .GlobalEnv)
  
  # Calcular índice siguiente circularmente
  i_siguiente <- if (i > total_slots) 1 else i
  
  # Actualizar el índice para la próxima llamada
  assign(".indice_slot_actual", i_siguiente + 1, envir = .GlobalEnv)
  
  # Retornar nombre del slot
  return(nombres_slots[[i_siguiente]])
}



##Cargo los datos
datos <- readxl::read_excel("T:/_Franco/Book1.xlsx")

EMPLEO <- stats::ts(datos$EMP, frequency = 12, start = base::c(2008,01))
ICA.int <- diff(EMPLEO)
## EXPLORACION GRAFICA ####


## FUNCION GRAFICO DYGRAPH (GENERICA)
crear_dygraph <- function(serie, titulo = "Serie", unidad = "valor") {
  # Formato etiquetas dinámico
  porcentaje_en_rango <- sum(abs(serie) >= 0 & abs(serie) <= 1, na.rm = TRUE) /
    length(na.omit(serie)) * 100
  
  label_formatter <- "function(d) { 
    return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
  }"
  
  label_formatter_ratio <- "function(d) {
    let num = d.toFixed(1);
    return num.replace('.', ',').replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
  }"
  
  selected_formatter <- if (porcentaje_en_rango > 10) {
    label_formatter_ratio
  } else {
    label_formatter
  }
  
  dygraph(serie, main = titulo, ylab = unidad) |>
    dySeries(label = "Serie", color = "black", strokeWidth = 1.5) |>
    dyLegend(width = 400) |>
    dyAxis("x", drawGrid = TRUE, gridLineWidth = 0.2) |>
    dyAxis("y", axisLabelWidth = 75, axisLabelFormatter = JS(selected_formatter)) |>
    dyRangeSelector(height = 20)
}

## EJEMPLOS DE USO DE SLOTS
# 1. Empleo total
graf_empleo <- crear_dygraph(EMPLEO, titulo = "Empleo registrado", unidad = "Miles")
mostrar_en_slot(siguiente_slot(), graf_empleo)

# 2. Variación mensual del empleo
graf_empleo_dif <- crear_dygraph(ICA.int, titulo = "Variación mensual del empleo", unidad = "Δ Miles")
#mostrar_en_slot("comercio", graf_empleo_dif)
mostrar_en_slot(siguiente_slot(), graf_empleo_dif)
#mostrar_en_slot(siguiente_slot(), graf_empleo)


##nuevo: View con Serie Original + Diferenciada en la misma vista
combo <- ts.union(Original = EMPLEO[-1], Diferencia = ICA.int)

graf_combo <- dygraph(combo, main = "Empleo y su variación", ylab = "Miles") |>
  dySeries("Original", color = "black") |>
  dySeries("Diferencia", color = "blue") |>
  dyRangeSelector(height = 20)

mostrar_en_slot(siguiente_slot(), graf_combo)

#View con zoom inicial acotado a últimos años
#graf_zoom <- crear_dygraph(EMPLEO, titulo = "Empleo - últimos años", unidad = "Miles") |>
#  dyOptions(dateWindow = c("2020-01-01", "2024-12-31"))

#mostrar_en_slot("precios", graf_zoom, TRUE)
#mostrar_en_slot("comercio", graf_zoom)

#View con líneas de referencia (bandas de valores)
graf_band <- crear_dygraph(ICA.int, titulo = "Variación mensual con referencia", unidad = "Δ Miles") |>
  dyShading(from = -5, to = 5, color = "#E0F7FA") |>
  dyLimit(0, color = "red", label = "Sin cambio")

mostrar_en_slot(siguiente_slot(), graf_band)

#rm(.indice_slot_actual, envir = .GlobalEnv)
rm(list = ls(pattern = "^\\.viewer_abierto_"), envir = .GlobalEnv)

