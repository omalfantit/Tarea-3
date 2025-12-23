
library(tidyverse)
library(sf)
library(chilemapas)

library(gganimate)
library(plotly)




poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_RM <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)


paleta <- c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")

ggplot(comunas_RM) + 
  geom_sf(aes(fill = pob_adulto_mayor, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 anios y mas en la Region Metropolitana") +
  theme_minimal(base_size = 13)


zonas_pte_alto <- mapa_zonas %>% 
  filter(codigo_comuna == "13201") %>% 
  inner_join(
    censo_2017_zonas %>% 
      filter(
        substr(geocodigo, 1, 2) == 13,
        as.integer(edad) >= 4
      )


paleta <- c("#628ca5", "#dca761")

ggplot() + 
  geom_sf(data = zonas_pte_alto, aes(fill = poblacion, geometry = geometry)) +
  geom_sf(data = filter(comunas_RM, codigo_comuna == "13201"),
          aes(geometry = geometry), colour = "#2A2B75", fill = NA) +
  ylim(-33.65, -33.57) +
  xlim(-70.64, -70.48) +
  scale_fill_gradientn(colors = paleta, name = "Población") +
  labs(title = "Poblacion de 65 anios y mas en la Comuna de Puente Alto") +
  theme_minimal(base_size = 13)



view(censo_2017_zonas)


