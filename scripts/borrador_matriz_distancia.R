

matriz <- st_distance(centroides,secu)
colnames(matriz) <- secu$clave
rownames(matriz) <- centroides$cod_2022
# Elegimos cuántas claves más cercanas queremos
n <- 5

claves_mas_cercanas <- 
  apply(matriz, 1, function(distancias) {
  sort(distancias)[1:n] |> names()
})


df_largo <- 
  as.data.frame(claves_mas_cercanas) %>%
  tibble::rownames_to_column("orden") %>%
  pivot_longer(
    cols = -orden,
    names_to = "centroide",
    values_to = "secu_clave"
  ) %>%
  mutate(
    orden = as.integer(orden),
    distancia = mapply(function(cen, sec) matriz[cen, sec], centroide, secu_clave)
  )
