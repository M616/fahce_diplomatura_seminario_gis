lp  |> 
  pivot_longer(cols = c(promedio_eduhog,
                        dist_sec),
               names_to = "variable",
               values_to = "valor") |> 
  ggplot() +
  geom_sf(aes(fill = valor)) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  facet_wrap(~variable)




###
{library(tidyverse)   # Manipulación de datos  
  library(sf)          # Datos espaciales  
  library(mapview)     # Mapas interactivos  
  library(units)       # Manejo de distancias (metros/km)  
  library(nngeo)       # Para cálculos de distancia más cercana  
  library(here)        # Para manejar el path
  library(janitor)     # Para limpieza de datos
  library(ggspatial)   # Visualización
  library(maptiles)    # Visualización
  library(prettymapr)  # Visualización
  }


mapa1 <- mapview(salud_intersection, col.regions = 'tomato')
mapa2 <- mapview(renabap, alpha = 0.3)
mapa1+mapa2

salud_buffer <- salud_buffer |> filter(nde == 'LA PLATA')

#salud_buffer <- salud_buffer |> st_transform(4326)
mapview(salud_buffer, alpha = 0.3)+
  mapview(renabap, alpha = 0.3, col.regions = 'orange')+
  mapview(salud_intersection, col.regions = 'tomato')


mapview(salud_lp, alpha = 0.3)+
  mapview(renabap_lp, alpha = 0.3, col.regions = 'yellow')+
  mapview(resultado, col.regions = 'tomato')

mapview()
mapview(resultado)
st_crs(salud_intersection)

salud_ej <- 
  salud_sf |> select(nor)
renabap_ej <- renabap |> 
  filter(departamento == 'La Plata' ) |> 
  select(nombre_barrio,cantidad_viviendas_aproximadas )

salud_joined <- 
  st_join(salud_ej,
          renabap_ej,
          join = st_intersects)
glimpse(salud_joined)

