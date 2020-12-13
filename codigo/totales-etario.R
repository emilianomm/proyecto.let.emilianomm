library(here)
library(dplyr)
library(lubridate)
library(gghighlight)
library(ggthemes)
library(scales)
library(forcats)
library(ggplot2)

# Este código nos sirve para preparar los datos

path.data = here::here("datos", "producto16", "CasosGeneroEtario.csv")
casos.etario = read.csv(path.data, header = TRUE, encoding = "UTF-8")

# limpieza de datos para crear las mismas categorias que en los otros analisis

n.cols.data = ncol(casos.etario)
casos.etario[c(35, 36, 37, 38, 39),] = vector(length = n.cols.data)
lvls = c("Menor de 40", "Entre 40 y 49", "Entre 50 y 59", "Entre 60 y 69", "Mayor o igual a 70")

for (i in 35:39)
{
  casos.etario[i, 1] = lvls[i - 34]
}

for (i in 3:n.cols.data)
{
  
  casos.etario[1:34,i] = casos.etario[1:34,i] %>% as.numeric()
  casos.etario[35, i] = sum(casos.etario[c(1:8, 18:25),i])
  casos.etario[36, i] = sum(casos.etario[c(9:10, 26:27),i])
  casos.etario[37, i] = sum(casos.etario[c(11:12, 28:29),i])
  casos.etario[38, i] = sum(casos.etario[c(13:14, 30:31),i])
  casos.etario[39, i] = sum(casos.etario[c(15:17, 32:34),i])
  
}

# Borrar filas que estan combinadas en la nueva categoria
casos.etario[1:34, 1:94] = NA
casos.etario = casos.etario %>% na.omit()
casos.etario = casos.etario[,c(1,3:94)]

casos.etario = casos.etario %>% tidyr::pivot_longer(cols = 2:93, names_to = "fecha")

for (i in 1:length(casos.etario$fecha))
  {
    casos.etario$fecha[i] = gsub("^X", "", casos.etario$fecha[i])
    casos.etario$fecha[i] = gsub("\\.", "/", casos.etario$fecha[i])
    casos.etario$fecha[i] = as_date(casos.etario$fecha[i])
  }


casos.etario$Grupo.de.edad = factor(casos.etario$Grupo.de.edad, 
                                    levels = lvls, ordered = TRUE)
                                    
mi.palette = c("#FE1F14", "#B1150E", "#FE7872", "#A52C94", "#C06BB4", "#EB9C3A", "#F3C388")

ggplot(data = casos.etario) + aes( y = value, x = Grupo.de.edad, color = Grupo.de.edad) + 
  geom_boxplot() + scale_color_manual(values = mi.palette) + 
  theme_minimal() + xlab("Grupo de edad") + ylab("Casos totales") + 
  labs(title = "Distribución casos totales por edad") + theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
                                                           legend.title = element_blank(), legend.position = "bottom") +
  scale_x_discrete(labels = NULL) + scale_y_continuous(labels = scales::number_format())

saves.path = here::here("figs")
ggsave("boxplot_caso-x-edad.jpeg", path = saves.path, width = 8.09, height = 5)


