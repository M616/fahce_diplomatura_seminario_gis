install.packages('tidygeocoder')
library(tidygeocoder)
library(tidyverse)
library(mapview)

library(sf)

#inmo <- read_csv("data/raw/inmo_2025-02-02_10-28-06.csv")
#inmo <- inmo %>% filter(district == 'La Plata')
terrenos <- st_read('data/raw/terrenos_nuevos_septiembre23_renaldo.gpkg')
mapview(terrenos)

lp <- terrenos %>% 
  st_drop_geometry() %>% 
  filter(district == 'La Plata') %>% 
  select(address) 


lp <- lp%>%
  mutate(
    direccion_limpia = str_trim(address),
    direccion_geocod = str_c(direccion_limpia, ", La Plata, Buenos Aires, Argentina"),
    city = 'La Plata'
  )
  


Sys.setenv(GOOGLEGEOCODE_API_KEY = "")


resultado2 <- 
  lp %>% head(5) %>% 
  geocode(address = direccion_limpia,
          method = 'google')
          
p <- resultado2 %>% filter(!is.na(lat)) %>% 
  st_as_sf(coords = c('long','lat'),crs=4326)
          