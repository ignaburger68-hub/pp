# =====================================================================
# Recolector del feed GBFS de Ecobici (Ciudad de México)
# Proyecto Simulación MIIIO/DIIIO 2-2026 · Tema 3: Gemelos digitales
# Ignacio Chandia y Francisco Correa
#
# Qué hace cada vez que se ejecuta:
#   1. Lee el índice del feed (gbfs.json) y busca la URL de station_status.
#   2. Descarga el estado actual de todas las estaciones.
#   3. Agrega una fila por estación, con la hora de la lectura, al archivo
#      del día:  datos/status/status_AAAA-MM-DD.csv
#   4. Una vez al día guarda también station_information (capacidad,
#      coordenadas):  datos/info/info_AAAA-MM-DD.csv
#   5. Anota cualquier error en datos/errores.log sin detenerse.
#
# Uso:
#   Rscript recolector_ecobici.R          -> una sola lectura (para cron o
#                                            el Programador de tareas)
#   Rscript recolector_ecobici.R --loop   -> lee cada 60 s hasta que se
#                                            cierre la ventana
# Requiere solo el paquete jsonlite: install.packages("jsonlite")
# =====================================================================

suppressPackageStartupMessages(library(jsonlite))

URL_INDICE <- "https://gbfs.mex.lyftbikes.com/gbfs/gbfs.json"
INTERVALO  <- 60      # segundos entre lecturas en modo --loop
options(timeout = 20) # segundos máximos de espera por descarga

# Carpeta de datos: junto a este script (o en el directorio actual)
args_completos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", grep("^--file=", args_completos, value = TRUE))
carpeta_base   <- if (length(archivo_script)) dirname(normalizePath(archivo_script)) else getwd()
CARPETA        <- file.path(carpeta_base, "datos")
dir.create(file.path(CARPETA, "status"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(CARPETA, "info"),   recursive = TRUE, showWarnings = FALSE)

registrar_error <- function(mensaje) {
  linea <- sprintf("%s  %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), mensaje)
  cat(linea, file = file.path(CARPETA, "errores.log"), append = TRUE)
  message(linea)
}

# Busca la URL de un feed (station_status, station_information) en el índice.
# Sirve para GBFS 1.x/2.x (feeds por idioma) y 3.x (lista única).
url_feed <- function(indice, nombre) {
  datos <- indice$data
  feeds <- if (!is.null(datos$feeds)) datos$feeds else datos[[1]]$feeds
  url   <- feeds$url[feeds$name == nombre]
  if (length(url) == 0) stop("El índice no trae el feed ", nombre)
  url[1]
}

# Garantiza que la columna exista (algunos operadores omiten campos opcionales)
columna <- function(df, nombre) if (nombre %in% names(df)) df[[nombre]] else NA

guardar <- function(df, archivo) {
  nuevo <- !file.exists(archivo)
  write.table(df, archivo, sep = ",", row.names = FALSE, col.names = nuevo,
              append = !nuevo, quote = TRUE, na = "", fileEncoding = "UTF-8")
}

leer_status <- function(indice) {
  hora_lectura <- Sys.time()
  feed <- fromJSON(url_feed(indice, "station_status"))
  est  <- feed$data$stations
  df <- data.frame(
    lectura_utc          = format(hora_lectura, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    feed_last_updated    = columna(feed, "last_updated"),
    station_id           = as.character(est$station_id),
    num_bikes_available  = columna(est, "num_bikes_available"),
    num_docks_available  = columna(est, "num_docks_available"),
    num_bikes_disabled   = columna(est, "num_bikes_disabled"),
    num_docks_disabled   = columna(est, "num_docks_disabled"),
    is_renting           = as.integer(columna(est, "is_renting")),
    is_returning         = as.integer(columna(est, "is_returning")),
    last_reported        = columna(est, "last_reported"),
    stringsAsFactors = FALSE
  )
  dia <- format(hora_lectura, "%Y-%m-%d")
  guardar(df, file.path(CARPETA, "status", paste0("status_", dia, ".csv")))
  message(format(hora_lectura, "%H:%M:%S"), "  ", nrow(df), " estaciones guardadas")
}

leer_info_si_falta <- function(indice) {
  archivo <- file.path(CARPETA, "info", paste0("info_", format(Sys.Date()), ".csv"))
  if (file.exists(archivo)) return(invisible())
  est <- fromJSON(url_feed(indice, "station_information"))$data$stations
  nombres <- columna(est, "name")
  # En GBFS 3.x el nombre viene como lista de traducciones: tomar la primera
  if (is.list(nombres)) nombres <- vapply(nombres, function(x) as.character(x$text[1]), "")
  df <- data.frame(
    station_id = as.character(est$station_id),
    name       = nombres,
    lat        = columna(est, "lat"),
    lon        = columna(est, "lon"),
    capacity   = columna(est, "capacity"),
    stringsAsFactors = FALSE
  )
  guardar(df, archivo)
}

una_lectura <- function() {
  tryCatch({
    indice <- fromJSON(URL_INDICE)
    leer_status(indice)
    tryCatch(leer_info_si_falta(indice),
             error = function(e) registrar_error(paste("info:", conditionMessage(e))))
  }, error = function(e) registrar_error(paste("status:", conditionMessage(e))))
}

if ("--loop" %in% commandArgs(trailingOnly = TRUE)) {
  message("Recolectando cada ", INTERVALO, " s en ", CARPETA, " (Ctrl+C para detener)")
  repeat {
    inicio <- Sys.time()
    una_lectura()
    espera <- INTERVALO - as.numeric(difftime(Sys.time(), inicio, units = "secs"))
    if (espera > 0) Sys.sleep(espera)
  }
} else {
  una_lectura()
}
