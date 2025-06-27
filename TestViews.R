##Borra el environment, plots y viewer
base::rm(list = base::ls()); grDevices::graphics.off() # BORRA ENVIRONMENT, PLOTS Y VIEWER

## LIBRERIAS NECESARIAS ####
library(readxl)
library(dygraphs)
library(htmlwidgets)

# Function to clear the viewer pane with a blank page
clear_rstudio_viewer <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    blank_html_file <- file.path(temp_dir, "blank.html")
    writeLines("", con = blank_html_file)
    rstudioapi::viewer(blank_html_file)
    # Optionally, clean up the temporary directory after a short delay
    # to ensure the viewer has rendered the blank page
    Sys.sleep(0.1)
    unlink(temp_dir, recursive = TRUE) # Be careful with unlink!
  } else {
    message("RStudio API not available or 'rstudioapi' package not installed. Cannot clear viewer.")
  }
}

# Example usage:
# Script 1 generates two views
# print(v1) # Your dygraph view 1
# print(v2) # Your dygraph view 2

# Before running Script 2, clear the viewer
clear_rstudio_viewer()

# Try this, but be aware it might not always work as expected
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::executeCommand("viewerClearAll")
}

##Cargo los datos
datos <- readxl::read_excel("T:/_Franco/Book1.xlsx")

EMPLEO <- stats::ts(datos$EMP, frequency = 12, start = base::c(2008,01))

## EXPLORACION GRAFICA ####

#Grafico la serie y analizo
stats::ts.plot(EMPLEO)
stats::ts.plot(diff(EMPLEO))
ICA.int <- diff(EMPLEO)

#Grafico la serie y analizo
stats::ts.plot(ICA.int)


## VIEWS ####
### FORMATO DE ETIQUETAS ####

a_original <- datos$EMP
inicio <- c(2008, 1)
z_codigo <- "EMP"
um <- "Empleo registrado (miles)"

input_view <- na.omit(a_original)
input_view <- ts(c(input_view, rep(NA, 12)), frequency = 12, start = inicio)

# Porcentaje de valores entre -1 y 1
porcentaje_en_rango <- sum(abs(input_view) >= 0 & abs(input_view) <= 1, na.rm = TRUE) /
  length(na.omit(input_view)) * 100

# Formato para miles
label_formatter <- "function(d) { 
  return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
}"

# Formato para ratios
label_formatter_ratio <- "function(d) {
  let num = d.toFixed(1);
  return num.replace('.', ',').replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
}"

# Selección de formato según contenido
selected_formatter <- if (porcentaje_en_rango > 10) {
  label_formatter_ratio
} else {
  label_formatter
}

### VIEW 1 ####
v1 <- {
  dygraph(input_view, main = paste0(z_codigo, " | Serie sin filtrar"), ylab = um) |>
    dySeries(label = "Original", color = "black", strokeWidth = 1.5) |>
    dyLegend(width = 400) |>
    dyAxis("x", drawGrid = TRUE, gridLineWidth = 0.2) |>
    dyAxis("y", axisLabelWidth = 75, axisLabelFormatter = JS(selected_formatter)) |>
    dyRangeSelector(height = 20)
}
print(v1)
print(v1)
print(v1)

