
library(here)
library(dplyr)
library(lubridate)
library(gghighlight)
library(ggthemes)
library(scales)
library(forcats)
library(ggplot2)

# Este código nos sirve para preparar los datos

path.data = here::here("datos", "producto09", "HospitalizadosUCIEtario.csv")
data.uci.etario = read.csv(path.data, header = TRUE, encoding = "UTF-8")

# transponer la base de datos
data.uci.etario = data.uci.etario %>% tidyr::pivot_longer(cols = 2:231, names_to = "fecha")


# Crear tipo de datos de fecha en R generando una secuencia nueva
str = as.Date("2020/04/01"); end = as.Date("2020/11/16")
data.uci.etario$fecha = rep(seq.Date(str, end, "day"), 5)

# Crear factores para el analisis con lm()
factor.lvl = data.uci.etario$Grupo.de.edad[!(duplicated(data.uci.etario$Grupo.de.edad)|duplicated(data.uci.etario$Grupo.de.edad, fromLast=FALSE))]

data.uci.etario$Grupo.de.edad = factor(data.uci.etario$Grupo.de.edad, 
                                       levels = factor.lvl, ordered = TRUE,
                      labels = c("Menor de 40", "Entre 40 y 49", "Entre 50 y 59", "Entre 60 y 69", "Mayor o igual a 70"))

# mi.palette -> colores personalizados
mi.palette = c("#FE1F14", "#B1150E", "#FE7872", "#A52C94", "#C06BB4", "#EB9C3A", "#F3C388")

ggplot(data = data.uci.etario) + aes( y = value, x =  Grupo.de.edad, color = Grupo.de.edad) + 
      geom_boxplot() + scale_color_manual(values = mi.palette) + 
      theme_minimal() + xlab("Grupo de edad") + ylab("Hospitalizaciones") + 
      labs(title = "Distribución hospitalizados por edad") + theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
                          legend.title = element_blank(), legend.position = "bottom") +
      scale_x_discrete(labels = NULL)

saves.path = here::here("figs")
ggsave("boxplot_hosp-x-edad.jpeg", path = saves.path, width = 8.09, height = 5)

lm.etario = lm(value ~ Grupo.de.edad - 1, data = data.uci.etario)

save.path = here::here("datos", "lm_etario")
save(lm.etario, file = save.path)

sum.lm = summary(lm.etario)


# Segundo analisis, producto 10 : muertes por edad

path.data = here::here("datos", "producto10", "FallecidosEtario.csv")
fallecido.etario = read.csv(path.data, header = TRUE, encoding = "UTF-8")

# los factores son diferentes, vamos a construir las mismas categorias que en el analisis de
#   hospitalizados

n.cols.data = ncol(fallecido.etario)
fallecido.etario[8,] = vector(length = n.cols.data); fallecido.etario[8,1] = ">=70"

for (i in 2:n.cols.data)
  {
    fallecido.etario[1:7,i] = fallecido.etario[1:7,i] %>% as.numeric()
    fallecido.etario[8, i] = sum(fallecido.etario[c(5, 6, 7),i])
  }

# Borrar filas que estan combinadas en la nueva categoria
fallecido.etario[c(5, 6, 7), ] = NA
fallecido.etario = fallecido.etario %>% na.omit()


# mismo que en la base anterior procedimiento para crear fechas y factores
fallecido.etario = fallecido.etario %>% tidyr::pivot_longer(cols = 2:223, names_to = "fecha")

str = as.Date("2020/04/09"); end = as.Date("2020/11/16")
fallecido.etario$fecha = rep(seq.Date(str, end, "day"), 5)

factor.lvl = fallecido.etario$Grupo.de.edad[!(duplicated(fallecido.etario$Grupo.de.edad)|duplicated(fallecido.etario$Grupo.de.edad, fromLast=FALSE))]

fallecido.etario$Grupo.de.edad = factor(fallecido.etario$Grupo.de.edad, 
                                        levels = factor.lvl, ordered = TRUE,
                                        labels = c("Menor de 40", "Entre 40 y 49", "Entre 50 y 59", "Entre 60 y 69", "Mayor o igual a 70"))

mi.palette = c("#FE1F14", "#B1150E", "#FE7872", "#A52C94", "#C06BB4", "#EB9C3A", "#F3C388")

ggplot(data = fallecido.etario) + aes( y = value, x =  Grupo.de.edad, color = Grupo.de.edad) + 
  geom_boxplot() + scale_color_manual(values = mi.palette) + 
  theme_minimal() + xlab("Grupo de edad") + ylab("Fallecimientos") + 
  labs(title = "Distribución fallecidos por edad") + theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
                                                               legend.title = element_blank(), legend.position = "bottom") +
  scale_x_discrete(labels = NULL)

saves.path = here::here("figs")
ggsave("boxplot_fallec-x-edad.jpeg", path = saves.path, width = 8.09, height = 5)

lm.etario2 = lm(value ~ Grupo.de.edad - 1, data = fallecido.etario)

save.path = here::here("datos", "lm_etario2")
save(lm.etario2, file = save.path)

summary(lm.etario)
