nn <-
  st_nn(centroides,
        secu,
        k = 1,
        returnDist = TRUE)

nn[[1]][[4]]  # índices de las 10 escuelas más cercanas al primer centroide
nn[[2]][[4]]  # distancias a esas 10 escuelas

escuelas_cercanas_k <- 
  lapply(nn[[1]], 
         function(idxs) secu$clave[idxs])



tibble(nn[[2]])

lp$dist <-  unlist(nn[['dist']])
