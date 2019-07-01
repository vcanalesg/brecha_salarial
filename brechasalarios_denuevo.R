## Brecha salarial nominal en Chile
## Tomando como inspiración a Duc-Quang Nguyen (https://dqn.website/post/animated-temperature-graphic-in-r/)

# Primero cargamos los paquetes necesarios para el gráfico
library(tidyverse); library(gganimate); library(haven); library(extrafont)
loadfonts()

# Cargar base de datos ESI 2017
# Se puede descargar de la página www.ine.cl
ESI_2017 <- read_sav("datos/esi_2017_personas_usuariosexternos.sav")

# Selecciono variables de interés y les coloco nombres más amigables
# Elimino nivel sin respuesta porque no tiene para este caso importancia analítica
ESI_2017 <- ESI_2017 %>% filter(ocup_ref == 1) %>% 
  select(sexo, fact_per, ingreso_ocup = ing_t_p, nivel_educ = cine, id_directorio, id_identificacion, estrato) %>% 
  as_factor() %>% filter(nivel_educ != "Nivel ignorado")

# Se recodifican algunas categorías de nivel de educación
# "Nunca estudió" lo junta con "Educación preescolar" porque la diferencia no es importante en términos de ingresos de la ocupación
# También junto en una categoría Postítulos y maestría y doctorado
ESI_2017$nivel_educ <- fct_recode(ESI_2017$nivel_educ,"Superior universitaria" = "Educación universitaria", "Superior técnica" = "Educación técnica (Educación superior no universitaria)", "Nunca estudió" = "Educación preescolar", "1º a 4º básico" = "Educación primaria (nivel 1)", "5º a 8º básico" = "Educación primaria (nivel 2)", "Ed. Media" = "Educación secundaria", "Postgrado" = "Postítulo y maestría", "Postgrado" = "Doctorado") 

# Tabla con promedios por nivel educacional
promedio_tot <- ESI_2017 %>% 
  group_by(nivel_educ) %>% 
  summarise(promedio = weighted.mean(ingreso_ocup, w = fact_per)) 

# Tabla promedios por nivel educacional y sexo
promedio_sexo <- ESI_2017 %>% 
  group_by(sexo, nivel_educ) %>% 
  summarise(promedio = weighted.mean(ingreso_ocup, w = fact_per)) %>% 
  ungroup() 

# Límites variable ingresos
ylim <- c(0, 2300000)

# Tabla con valores promedio total, hombres y mujeres separados en dos columnas
# y0, y1 en ejemplo Nguyen, siendo y0 = Hombre, y1 = Mujer
dif <- promedio_sexo %>% spread(key = sexo, value = promedio) %>% 
  rename(y0 = Hombre, y1 = Mujer) %>% 
  mutate(sexo = "Mujer")

dif <- bind_rows(promedio_sexo %>% spread(key = sexo, value = promedio) %>% 
  rename(y0 = Hombre, y1 = Hombre) %>% 
  mutate(sexo = "Hombre"), dif) %>% 
  select(-Mujer)
  
# Etiquetas con la diferencia de ingresos entre hombres y mujeres por nivel educacional.
# Si la diferencia es NA se reemplaza por ""
dif_texto <- dif %>% 
  mutate(diferencia = y0 - y1,
         etiqueta = ifelse(is.na(diferencia), "", paste0("-$", format(diferencia, digits = 0, big.mark = ".", decimal.mark = ",", scientific = FALSE)))) %>% select(-y0, -diferencia)

texto_prom <- promedio_sexo %>% 
  mutate(etiqueta = paste0("$", format(promedio, digits = 0, big.mark = ".", decimal.mark = ",", scientific = FALSE)))

# Etiquetas para cada estado de la transición
texto_leyenda <- tibble(
  etiqueta = c("Hombres (ingreso promedio)", "Mujeres (ingreso promedio)"),
  sexo = c("Hombre", "Mujer")
)

# Etiqueta diferencia ingresos
texto_diferencia <- tibble(
  etiqueta = c("", "Diferencia promedio de ingresos (en rosado)"),
  sexo = c("Hombre", "Mujer"),
  x = "Ed. Media", y = 1850000
)

# Tema de ggplot
mi_tema <- function(base_size = 30) {
  ggplot() +
    geom_hline(yintercept = 0, colour = "darkgrey", alpha = 0.6, size = 0.7) +
    scale_y_continuous(
      name = "", expand = c(0.03, 0), limits = ylim, 
      labels = function(x) paste0("$",format(x, big.mark = ".", decimal.mark = ",", digits = 0, scientific = FALSE)),
      breaks = scales::pretty_breaks(n = 5)
    ) +
    theme_minimal(base_size = base_size ) +
    theme(
      plot.title = element_text(hjust = 0, face = "bold", family = "Courier New", size = 50),
      plot.subtitle = element_text(hjust = 0, family = "Courier New", size = 24),
      plot.caption = element_text(colour = "#666666", 
                                  margin = margin(0, 22, 24, 0, "pt")),
      axis.ticks.length = unit(0.7, "lines"),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y =  element_line(
        color = "#c2c4d6", linetype = "dotted", size = 0.35),
      axis.line = element_blank(),
      axis.text = element_text(family = "Courier New"),
      text = element_text(family = "Courier New")
    )
}

# Gráfico
tamano_fuente <- 30

g <- mi_tema(base_size = tamano_fuente) + 
  scale_colour_identity() +
  geom_point(
    data = promedio_sexo,
    aes(x = nivel_educ, y = promedio), 
    size = 13, colour = ifelse(promedio_sexo$sexo == "Hombre", "#5e06a0", "#dbad08") , alpha = 0.7
  ) +
  geom_text(
    data = dif_texto, hjust = 0.5, vjust = -2, nudge_y = 0.45,
    aes(x = nivel_educ, y = y1, label = etiqueta), 
    size = tamano_fuente / 5 , colour = "#9b1a53", family = "Courier New", fontface = "bold"
  ) +
  geom_text(
    data = texto_prom, hjust = 0.5, vjust = ifelse(texto_prom$sexo == "Hombre", -2, 2.5),
    aes(x = nivel_educ, y = promedio, label = etiqueta),
    size = tamano_fuente / 4.5, colour = ifelse(promedio_sexo$sexo == "Hombre", "#5e06a0", "#725c0b"),
    family = "Courier New", fontface = "bold"
  ) +
  geom_text(
    data = texto_leyenda,
    aes(x = "Ed. Media", y = 2000000, label = etiqueta),
    size = tamano_fuente / 3, colour = ifelse(texto_leyenda$sexo == "Hombre", "#5e06a0", "#dbad08"), 
    family = "Courier New", fontface = "bold"
  ) +
  geom_text(
    data = texto_diferencia,
    aes(x = x, y = y, label = etiqueta),
    size = tamano_fuente / 4, colour = "#9b1a53", family = "Courier New", fontface = "bold"
  ) + 
  scale_x_discrete(name = "", position = "bottom", labels = c("Nunca estudió \n+ \nEd. preescolar", "1º a 4º \nbásico", "5º a 8º \nbásico", "Ed. Media", "Superior \ntécnica", "Superior \nuniversitaria", "Postgrado")) + 
  labs(title = "¿Cuánto menos ganan las mujeres \nen comparación a los hombres?",
       subtitle = "Ingreso promedio de los ocupados por nivel de educacion",
       caption = "Fuente: ESI 2017 | Viz por Verónica Canales")

ga <- g + 
  transition_states(
    sexo, transition_length = 2, state_length = 1, wrap = T
  )

animate(ga, height = 1000, width = 1400, end_pause = 5, nframes = 25)  

anim_save("gap_salarios.gif")
