lp  |> 
  pivot_longer(cols = c(promedio_eduhog,
                        dist_sec),
               names_to = "variable",
               values_to = "valor") |> 
  ggplot() +
  geom_sf(aes(fill = valor)) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  facet_wrap(~variable)


