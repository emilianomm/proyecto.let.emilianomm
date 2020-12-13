## Una mirada al comportamiento de las regiones
library(here)
library(dplyr)
library(lubridate)
library(gghighlight)
library(ggthemes)
library(scales)
library(forcats)
library(ggplot2)

# Este código nos sirve para preparar los datos

path.data = here::here("datos", "producto03", "TotalesPorRegion.csv")
data.region = read.csv(path.data, header = TRUE, encoding = "UTF-8")

data.region = data.region %>% tidyr::pivot_longer(cols = 2:231, names_to = "fecha")
data.region = data.region %>% filter(data.region$Categoria == "Casos acumulados")

# obtener nombre regiones
regiones = data.region$Region[!(duplicated(data.region$Region)|duplicated(data.region$Region, fromLast=FALSE))]
regiones = regiones[regiones != "Total"]

# Filtrar el valor máximo (casos acumulados a la fecha)
val = c()
for(i in 1:length(regiones))
  {
    filtrado = data.region %>% filter(data.region$Region == regiones[i])
    val = c(val, max(filtrado$value))
}


barplot(height = val, names.arg = regiones, main = "Casos acumulados por región", cex.names = 0.3)

# Podemos separar las regiones en grupos
# Grupo 1: Arica-Stgo
# Grupo 2: ohiggins - Magallanes


barplot(height = val, names.arg = regiones, main = "Casos acumulados por región", cex.names = 0.58,
        border = "white", col = "#FE1F14", font.main = 1, family = "sans", cex.main = 1.8, font.lab = 2)

Totales.region = data.frame(regiones, val)

save.path = here::here("datos", "SoloTotalesRegion.RData")
save(Totales.region, file = save.path)

save.path = here::here("datos", "data-x-region.RData")
save(data.region, file = save.path)

## Series de tiempo por region

# Definimos una paleta de colores

mi.palette = c("#FE1F14", "#B1150E", "#FE7872", "#A52C94", "#C06BB4", "#EB9C3A", "#F3C388")


# plot serie regions + highlight
data.region$fecha = rep(seq.Date(str, end, "day"), 17)
plot.regiones = data.region %>% filter(data.region$Region != "Total") %>% 
ggplot() + 
  aes(x = fecha, y = log(value), color = Region) + geom_line() +
  ylab("Cant. Contagios (Log)") + xlab("Mes / 2020") + scale_y_continuous(labels = scales::comma) + theme_minimal() +
   gghighlight(Region == regiones[c(7, 11, 6, 3, 9, 8)]) + scale_color_manual(values = mi.palette) + facet_wrap(~Region) +
  labs(title = "Evolución temporal en los casos activos por regiones") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))  
plot.regiones
saves.path = here::here("figs")
ggsave("serie_contagios-x-region.jpeg", path = saves.path, width = 8.09, height = 5)

plot(x = seq(1,259), y = Series$value)

lm = lm(value ~ seq(1,259), data = Series)

abline(b = lm$coefficients[2], a = lm$coefficients[1], lwd = 2, col = "red")


