
# 1. INSTALACIÓN Y CARGA DE PAQUETES -------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # Paquetes básicos
  tidyverse,    # Manipulación de datos
  sf,           # Datos espaciales
  here,         # Manejo de rutas
  httr,         # Descargas web
  curl,         # Verificación de conexión
  osmdata,      # Datos de OpenStreetMap
  readxl,       # Lectura de Excel
  janitor,      # Limpieza de nombres
  fs            # Operaciones con archivos
)

# 2. CONFIGURACIÓN INICIAL -----------------------------------------------------
# Crear estructura de directorios
dirs <- c(
  "data/raw/salud",
  "data/raw/educacion",
  "data/raw/censo",
  "data/raw/renabap",
  "data/raw/OSM",
  "data/processed",
  "data/raw/ovs"
  )

walk(dirs, ~{
  if (!dir.exists(here(.x))) {
    dir.create(here(.x), recursive = TRUE)
    message("✓ Directorio creado: ", .x)
  }
})

# 4. FUNCIONES AUXILIARES ------------------------------------------------------
descargar_archivo <- function(url, destino) {
  tryCatch({
    if (!file.exists(destino)) {
      message("Descargando archivo...")
      GET(url, write_disk(destino, overwrite = TRUE))
      message(paste0("✓ Archivo guardado en: ", destino))
    } else {
      message("El archivo ya existe. Saltando descarga.")
    }
  }, error = function(e) {
    message(paste("Error en la descarga:", e$message))
  })
}

# 4. DESCARGA DE DATOS DE SALUD ------------------------------------------------
dir_salud <- here("data/raw/salud")

url_salud <- "https://catalogo.datos.gba.gob.ar/dataset/91743f68-bc82-4475-baca-7d5d6908eee8/resource/c52f9497-9eab-4ecd-a382-b4e4c6033a02/download/establecimientos_salud_publicos-2025.csv"
archivo_destino <- file.path(dir_salud, "establecimientos_salud_publicos-2025.csv")
descargar_archivo(url_salud, archivo_destino)


# 5. DESCARGA DE DATOS DE SALUD ------------------------------------------------

dir_educacion <- here("data/raw/educacion")
if (!dir.exists(dir_educacion)) dir.create(dir_educacion, recursive = TRUE)

url_educacion <- "https://catalogo.datos.gba.gob.ar/dataset/4becb4b7-0a21-4fef-8f2c-30df7f345a01/resource/08a49256-620d-4d75-9a40-6f57df3be830/download/establecimientos-educativos-09062025.zip"
destino_educacion <- here("data/raw/educacion", basename(url_educacion))

descargar_archivo(url_educacion, destino_educacion)
unzip(destino_educacion, exdir = dir_educacion)
file.remove(destino_educacion)

# 5. DESCARGA RADIOS CENSALES (CONICET) ------------------------------------------------

url_censo <- "https://ri.conicet.gov.ar/bitstream/handle/11336/149711/RADIOS_2022_V2025-1.zip"
dir_censo <- here("data/raw/censo/radios_censales")

if (!dir.exists(dir_censo)) dir.create(dir_censo, recursive = TRUE)
destino_censo <- here("data/raw/censo", basename(url_censo))

descargar_archivo(url_censo, destino_censo)
unzip(destino_censo, exdir = dir_censo)
file.remove(destino_censo)


# 6. DESCARGA DATOS RENABAP ------------------------------------------------

url_renabap <- "https://archivo.habitat.gob.ar/dataset/ssisu/renabap-datos-barrios-gpkg"
dir_renabap <- here("data/raw/renabap")

if (!dir.exists(dir_renabap)) dir.create(dir_renabap, recursive = TRUE)
destino_renabap <- here("data/raw/renabap", basename(url_renabap))

descargar_archivo(url_renabap,destino_renabap)

# 7. DESCARGA DATOS OSM (LA PLATA) ------------------------------------------------

##Patch para hacer andar curl::has_internet()

# Nueva función más robusta
{having_internet <- function(url = "https://www.google.com") {
  result <- try(curl::curl_fetch_memory(url), silent = TRUE)
  !inherits(result, "try-error")
}
  having_internet()
  
  # Sobrescribir curl::has_internet en esta sesión
  assignInNamespace(
    x = "has_internet",
    value = having_internet,
    ns = "curl"
  )
  
  
}

# Definir bounding box para La Plata (coordenadas aproximadas)
bb_la_plata <- getbb("La Plata, Argentina")

# Consulta OSM para avenidas
avenidas <- opq(bbox = bb_la_plata) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary")) %>%
  osmdata_sf()

# Procesar resultados
if (!is.null(avenidas$osm_lines)) {
  avenidas_sf <- avenidas$osm_lines %>%
    select(name, highway, geometry) %>%
    filter(!is.na(name)) %>%
    st_transform(4326)
  
  st_write(avenidas_sf, here("data/raw/OSM/avenidas_osm.gpkg"), append = FALSE)
  } 


# 8. DESCARGA DE DATOS DEL OVS ------------------------------------------------

dir_ovs <- here("data/raw/ovs")
if (!dir.exists(dir_ovs)) dir.create(dir_ovs, recursive = TRUE)

url_ovs <- "https://drive.google.com/uc?export=download&id=1Jql3hyztfuGKhXLzkWjYoCcXZ-fSaJ-S"
destino_ovs <- here(dir_ovs,'ovs_terrenos_ta_2022_06_RMBA.zip')

descargar_archivo(url_ovs,destino_ovs)
unzip(destino_ovs, exdir = dir_ovs)
file.remove(destino_ovs)