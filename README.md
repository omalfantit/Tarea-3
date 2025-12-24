# Tarea-3
Última tarea del curso de Datos

# Adultos mayores en la comuna de Puente Alto

# Carga de paquetes

library(tidyverse)
library(sf)
library(chilemapas)
library(gganimate)
library(plotly)

# Del Censo 2017, por comunas, creamos una BD para ver la población adulta de las comunas, filtrando por edad, agrupando por código de comuna y sumando dicha población resultado.

poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

# Creamos una BD con las comunas de la RM a la que le unimos las BD antes creada y los códigos territoriales de las comunas

comunas_RM <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)

# Graficaremos, en un mapa, por colores, la cantidad de población adulta

paleta <- c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")

ggplot(comunas_RM) + 
  geom_sf(aes(fill = pob_adulto_mayor, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 anios y mas en la Region Metropolitana") +
  theme_minimal(base_size = 13)

<img width="862" height="515" alt="Rplot" src="https://github.com/user-attachments/assets/41fcd9a4-67a9-4f0c-a708-cb70f13b5533" />



  # De la BD mapa de zonas, creamos una específica para la comuna de Puente ALto, filtrando por su correspondiente código censal. Luego, unimos los resultados del censo 2017 por zonas

  zonas_pte_alto <- mapa_zonas %>% 
  filter(codigo_comuna == "13201") %>% 
  inner_join(
    censo_2017_zonas %>% 
      filter(
        substr(geocodigo, 1, 2) == 13,
        as.integer(edad) >= 4
      )

  # Finalmente, graficamos las zonas de la comuna de Puente Alto según la presencia de adultos mayores.

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

<img width="862" height="515" alt="Rplot01" src="https://github.com/user-attachments/assets/6480cd8c-b40f-4d78-a3c0-6bc49822c4cd" />

