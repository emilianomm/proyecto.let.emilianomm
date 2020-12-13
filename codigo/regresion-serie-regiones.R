# cargar datos cantidad de población por region

save.path = here::here("datos", "data-x-region.RData")
load(save.path)

save.path = here::here("datos", "SoloTotalesRegion.RData")
load(save.path) # Este objeto nos sirve para obtener los nombre de las regiones

regiones = Totales.region[,1]
betas = c()

for (i in 1:16) {
  
    Series = data.region[data.region$Region == regiones[i],]
    str = as.Date("2020/03/03"); end = as.Date("2020/11/16")
    Series$fecha = seq.Date(str, end, "day")
    lm = lm(value ~ fecha, data = Series)
    print(regiones[i])
    print(lm$coefficients)
    betas = c(betas, lm$coefficients[2])
}

betas

# Calculamos nuestra medida lambda

delta.i = data.frame(regiones, betas); colnames(delta.i) = c("Region", "Delta.i")
save.path = here::here("datos", "delta_i.RData")
save(delta.i, file = save.path)

save.path = here::here("datos", "poblacion_orden-geografico.csv")
pop_data = read.csv(save.path, header = FALSE)
k = 1

Lambda = vector(mode = "numeric", length = 16)

for (i in 1:length(Lambda))
  {
      ponderacion = (delta.i[i,2]*k)/log(pop_data[i,1])
      Lambda[i] = log(ponderacion*Totales.region[i,2])
  }


data.Lambda = data.frame(regiones, Lambda, delta.i[,2], pop_data)
colnames(data.Lambda) = c("Región", "Lambda", "Delta", "Población")

save.path = here::here("datos", "T-Lambda-Data.RData")
save(data.Lambda, file = save.path)




