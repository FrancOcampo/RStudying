##Borra el environment, plots y viewer
base::rm(list = base::ls()); grDevices::graphics.off() # BORRA ENVIRONMENT, PLOTS Y VIEWER

## LIBRERIAS NECESARIAS ####
library(readxl)
library(dygraphs)
library(htmlwidgets)
library(rstudioapi)

# Try this, but be aware it might not always work as expected
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::executeCommand("viewerClearAll")
}

## CONFIGURACION DE SLOTS
ruta_base_slots <- file.path(tempdir(), "slots") # Cambia a una ruta temporal para evitar problemas de permisos
#ruta_base_slots <- "T:/_Franco/slots"
dir.create(ruta_base_slots, showWarnings = FALSE)

# Definir los slots visuales
viewer_slots <- list(
  empleo = file.path(ruta_base_slots, "slot_empleo.html"),
  comercio = file.path(ruta_base_slots, "slot_comercio.html"),
  exportaciones = file.path(ruta_base_slots, "slot_exportaciones.html"),
  precios = file.path(ruta_base_slots, "slot_precios.html")
)

# Función para renderizar un widget en un slot
#mostrar_en_slot <- function(nombre_slot, widget, abrir = FALSE) {
#  path <- viewer_slots[[nombre_slot]]
##  if (is.null(path)) stop("Slot no definido: ", nombre_slot)
#  
# htmlwidgets::saveWidget(widget, file = path, selfcontained = TRUE)
#  if (abrir || !exists(".slot_abierto", envir = .GlobalEnv)) {
#    rstudioapi::viewer(path)
#    assign(".slot_abierto", TRUE, envir = .GlobalEnv)  # marca que se abrió
#  }else {
#    # Forzamos recarga: path + query string para evitar cache
#    rstudioapi::viewer(paste0(path, "?v=", as.numeric(Sys.time())))
#  }
#}

mostrar_en_slot <- function(nombre_slot, widget, abrir = FALSE) {
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
  flag <- paste0(".slot_abierto_", nombre_slot)
  if (abrir || !exists(flag, envir = .GlobalEnv)) {
    rstudioapi::viewer(path)
    assign(flag, TRUE, envir = .GlobalEnv)
  }
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
mostrar_en_slot("empleo", graf_empleo, TRUE)

# 2. Variación mensual del empleo
graf_empleo_dif <- crear_dygraph(ICA.int, titulo = "Variación mensual del empleo", unidad = "Δ Miles")
#mostrar_en_slot("comercio", graf_empleo_dif)
mostrar_en_slot("empleo", graf_empleo_dif)
mostrar_en_slot("empleo", graf_empleo)
# 3. Otro slot vacío (preparado para uso futuro)
#mostrar_en_slot("precios", crear_dygraph(ts(rep(NA, 12), frequency = 12, start = c(2020,1)), "Vacío", ""))



#Grafico la serie y analizo
#stats::ts.plot(EMPLEO)
#stats::ts.plot(diff(EMPLEO))

#Grafico la serie y analizo
#stats::ts.plot(ICA.int)



## VIEWS ####
### FORMATO DE ETIQUETAS ####

#a_original <- datos$EMP
#inicio <- c(2008, 1)
#z_codigo <- "EMP"
#um <- "Empleo registrado (miles)"

#input_view <- na.omit(a_original)
#input_view <- ts(c(input_view, rep(NA, 12)), frequency = 12, start = inicio)

# Porcentaje de valores entre -1 y 1
#porcentaje_en_rango <- sum(abs(input_view) >= 0 & abs(input_view) <= 1, na.rm = TRUE) /
 # length(na.omit(input_view)) * 100

# Formato para miles
#label_formatter <- "function(d) { 
#  return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
#}"

# Formato para ratios
#label_formatter_ratio <- "function(d) {
 # let num = d.toFixed(1);
#  return num.replace('.', ',').replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
#}"

# Selección de formato según contenido
#selected_formatter <- if (porcentaje_en_rango > 10) {
#  label_formatter_ratio
#} else {
#  label_formatter
#}

### VIEW 1 ####
#v1 <- {
#  dygraph(input_view, main = paste0(z_codigo, " | Serie sin filtrar"), ylab = um) |>
#    dySeries(label = "Original", color = "black", strokeWidth = 1.5) |>
#    dyLegend(width = 400) |>
#    dyAxis("x", drawGrid = TRUE, gridLineWidth = 0.2) |>
#    dyAxis("y", axisLabelWidth = 75, axisLabelFormatter = JS(selected_formatter)) |>
#    dyRangeSelector(height = 20)
#}
#print(v1)
#print(v1)
#print(v1)

