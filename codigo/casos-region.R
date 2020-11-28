## Una mirada al comportamiento de las regiones
library(here)
library(dplyr)
library(lubridate)

path.data = here::here("datos", "producto03", "TotalesPorRegion.csv")
data.region = read.csv(path.data, header = TRUE, encoding = "UTF-8")

# Antes de transformar los datos
regiones = data.region[1]

data.region = data.region %>% tidyr::pivot_longer(cols = 3:261, names_to = "fecha")


