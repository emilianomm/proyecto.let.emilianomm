library(here)
library(ggplot2)

saves.path = here::here("figs")
data.path = here::here("datos", "producto16", "CasosGeneroEtario.csv")

datos.etario = read.csv(data.path, encoding = "UTF-8")

# r := reshape, permite cambiar el formato de la base de datos >>> mas facil para ggplot
datos.r =datos.etario %>% tidyr::pivot_longer(cols = 3:94, names_to = "fecha")


# gráficar por sexo, si no es raro porque se generar 2 "values" por c/ fecha
indxm = which(datos.r$Sexo == "F")
datos.r.masc = datos.r[indxm,]

indxf = which(datos.r$Sexo == "F")
datos.r.fem = datos.r[indxf,]

ggplot(data = datos.r.masc) + aes(y = value, x = fecha, group = Grupo.de.edad, col = Grupo.de.edad) +
          geom_line() + labs(title = "Evolución contagios por grupo de edad \ Masculino") +
            ylab("Casos Acumulados") + xlab("Fecha")
ggsave("serie_contagios-edad-masculino.jpeg")

ggplot(data = datos.r.fem) + aes(y = value, x = fecha, group = Grupo.de.edad, col = Grupo.de.edad) +
  geom_line()  + labs(title = "Evolución contagios por grupo de edad \ Femenino") +
  ylab("Casos Acumulados") + xlab("Fecha")
ggsave("serie_contagios-edad-femenino.jpeg")

