#install.packages('tidygeocoder')
library(tidygeocoder)
library(tidyverse)
library(mapview)
library(here)
library(sf)

#inmo <- read_csv("data/raw/inmo_2025-02-02_10-28-06.csv")
#inmo <- inmo %>% filter(district == 'La Plata')
terrenos <- st_read('data/raw/terrenos_nuevos_septiembre23_renaldo.gpkg')
mapview(terrenos)

lp <- terrenos %>% 
  #st_drop_geometry() %>% 
  filter(district == 'La Plata') 

mapview(lp)

lp <- lp%>%
  mutate(
    direccion_limpia = str_trim(address),
    direccion_geocod = str_c(direccion_limpia, ", La Plata, Buenos Aires, Argentina"),
    city = 'La Plata'
  )
  

### Completar con la clave de la API
Sys.setenv(GOOGLEGEOCODE_API_KEY = "")

lp_goog <- 
  lp %>% 
  #head(5) %>% 
  geocode(address = direccion_limpia,
          method = 'google')

dir.create(here('data/processed/ovs'))
save(lp_goog, file = here('data/processed/ovs/lp_goog.Rda'))

lp_osm <- 
  lp %>% 
  #head(5) %>% 
  geocode(address = direccion_limpia,
          method = 'osm')

save(lp_osm, file = here('data/processed/ovs/lp_osm.Rda'))

load(here('data/processed/ovs/lp_goog.Rda'))

#p <- lp_goog %>% select(direccion_geocod,long,lat)
osm <-
  lp_osm %>% 
  filter(!is.na(lat)) %>% 
  select(listing_id,long,lat) %>% 
  st_as_sf(coords = c('long','lat'), crs = 4326) %>% 
  st_transform(5347)

lp <- st_transform(lp, 5347)
st_crs(lp)

lp_filtered <- 
  lp %>% filter(listing_id %in% osm$listing_id) %>% 
  arrange(listing_id)
# Asegurate que ambos objetos estén en el mismo orden
osm <- 
  osm |> arrange(listing_id)

# Ambos ya están en 3857
distancias <- 
  st_distance(lp_filtered, 
              osm,
              by_element = TRUE)

lp_filtered$distancias <- distancias

lp_filtered <- 
  lp_filtered %>% 
  st_drop_geometry() %>% 
  select(listing_id,distancias)

lp <- left_join(lp,
          lp_filtered)

lp <- 
  lp |> 
  mutate(
    distancia_km = as.numeric(distancias) / 1000,
    distancia_cat = case_when(
      as.numeric(distancias) <= 200 ~ "≤ 100 m",
      as.numeric(distancias) > 200 ~ "> 100 m",
      TRUE ~ NA_character_
    )
  )
lp %>% st_drop_geometry() %>%  count(distancia_cat)
mapview(lp, zcol = 'distancia_km', label = 'address')
library(ggspatial)
ggplot(lp) +
  geom_sf(aes(fill = (distancia_cat ))) +
  #scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  #labs(fill = "Distancia (m)") +
  theme_minimal()+
  annotation_map_tile(alpha = 0.3)



lp_goog <- 
  lp_goog %>% 
  filter(!is.na(lat)) %>% 
  select(listing_id,
         long,
         lat) %>% 
  st_as_sf (coords = c('long',
                       'lat'),
            crs = 4326)



# Supongamos que los dos sf tienen: listing_id, geometry

# Transformar ambos a la misma proyección
lp <- st_transform(lp, 3857)
lp_goog <- st_transform(lp_goog, 3857)


resultado <- data.frame(
  listing_id = lp$listing_id,
  distancia_m = mapply(st_distance, st_geometry(lp), st_geometry(lp_goog))
)




lp_goog <- 
  lp_goog %>% 
    filter(!is.na(lat)) %>% 
  #mutate(lat_goog = lat,
  #       long_goog = long) %>% 
  #select(listing_id,
  #       lat_goog,
  #       long_goog)
 st_as_sf(coords = c('long','lat'),
          crs = 4326) %>% 
    select(listing_id)


# Extraer coordenadas como columnas normales
lp_goog %>% st_transform()
lp_goog$coord_goog <- st_coordinates(lp_goog) 

lp <- st_drop_geometry(lp_goog) %>% 
  right_join(lp, by = 'listing_id')


# Unir por listing_id
distancias <- lp_coords |> 
  inner_join(lp_goog_coords, by = "listing_id") |> 
  mutate(distancia_m = st_distance(geom_lp, geom_goog, by_element = TRUE))


################georref
################
################library(readr )
library(tidyverse)
library(geoAr)
library(here)

muestra <- read_csv(here('data/raw/ovs/lp_muestra_inmuebles.csv'))



p <- geoAr::normalizar_direccion(direccion = 'calle 522 n 753',
                                 provincia = 'bs as',
                                 departamento = 'la plata',
                                 max = 1
)

p <- geoAr::normalizar_direccion(direccion = muestra$address[10],
                                 provincia = muestra$prov[10],
                                 departamento = muestra$district[10],
                                 max = 1
)


muestra$prov <- 'Buenos Aires'

muestra <- muestra |> filter(!is.na(address))
tmp <- list()

for (i in seq_len(nrow(muestra))) {
  
  res <- geoAr::normalizar_direccion(
    direccion = muestra$address[i],
    provincia = muestra$prov[i],
    departamento = muestra$district[i],
    max = 1
  )
  
  # Guardar resultado vacío si no hay respuesta
  if (length(res) == 0) {
    tmp[[i]] <- tibble(
      listing_id = muestra$listing_id[i],
      ubicacion_lat = NA,
      ubicacion_lon = NA
    )
  } else {
    # Tomar solo el primer resultado
    tmp[[i]] <- tibble(
      listing_id = muestra$listing_id[i],
      ubicacion_lat = res$ubicacion_lat,
      ubicacion_lon = res$ubicacion_lon
    )
  }
  
  # Asignar nombre al elemento
  names(tmp)[i] <- muestra$listing_id[i]
}


dplyr::bind_rows(tmp)