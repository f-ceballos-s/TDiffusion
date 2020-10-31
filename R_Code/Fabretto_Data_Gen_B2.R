setwd("C:/Users/fc3/Box Sync/Fabretto/Fabretto")

library(readxl)
library(dummies)
library(caret)
library(dplyr)

# Data cleaning

## Baseline

base_p = read_excel("base_raw.xlsx",sheet = "Tipo B", col_names = T)
base_p$time = 0

base_p$cod_par = gsub("\\.", "", base_p$cod_par) # remove dots from id
base_p$Nivel_educativo_del_encuestado = base_p$Nivel_educativo_del_encuestado -1 # change current level to last level
cols.num = seq(43,78,5)
base_p[cols.num] = sapply(base_p[cols.num],as.numeric) # set age of parents as numeric
base_p[, 5:36][is.na(base_p[, 5:36])] = 0
base_p[, c(82:131,133,135,136,138:140,142:144,146:148,150,153,158)][base_p[, c(82:131,133,135,136,138:140,142:144,146:148,150,153,158)] != 1] = 0         # impute non-1 values to 0
base_p[, 83:132][is.na(base_p[, 83:132])] = 0
base_p$part_sat = as.numeric(base_p$part_sat)
base_p[,177] = NULL

base_p$area = rowSums(base_p[,seq(42,77,5)], na.rm=T)
base_p$crop_inc = rowSums(base_p[,seq(44,79,5)], na.rm=T)


## Midline

mid_p = read_excel("mid_raw.xlsx",sheet = "Tipo B", col_names = T)
mid_p$time = 1



mid_p$Comunidad = ifelse(mid_p$Comunidad == "Jobo",1,ifelse(mid_p$Comunidad == "Encino",2,ifelse(mid_p$Comunidad == "Estancia",3,ifelse(mid_p$Comunidad == "El Espino",4,
                         ifelse(mid_p$Comunidad == "Macarali",5,ifelse(mid_p$Comunidad == "Siuce",6,ifelse(mid_p$Comunidad == "San Francisco",7,ifelse(mid_p$Comunidad == "La Florecida",8,
                         ifelse(mid_p$Comunidad == "es de la Florecida/San Francisco",8,9)))))))))
mid_p[mid_p == 999] <- NA  # replace 999 with NA

mid_p[6:79] = sapply(mid_p[6:79],as.numeric)
mid_p$area = rowSums(mid_p[,seq(42,77,5)], na.rm=T)
mid_p$crop_inc = rowSums(mid_p[,seq(44,79,5)], na.rm=T)

mid_p$`3Que_porcentaje_de_sus_ingresos_provino_de_trabajo_fuera_de_la_finca` = as.numeric(mid_p$`3Que_porcentaje_de_sus_ingresos_provino_de_trabajo_fuera_de_la_finca`)
mid_p$cuant_cred = as.numeric(mid_p$cuant_cred)
mid_p$cuant_ahor = as.numeric(mid_p$cuant_ahor)

mid_p[is.na(mid_p)] = 0

names(base_p) %in% names(mid_p)
names(mid_p) %in% names(base_p)
match_b = base_p$cod_par %in% mid_p$cod_par
match_m = mid_p$cod_par %in% base_p$cod_par
base_p = base_p[match_b,]
mid_p = mid_p[match_m,]
mid_p$cod_par = as.character(mid_p$cod_par)

## Merged lines

st_raw = rbind(base_p,mid_p)


### Treatment variable

st_raw$tipo = rep(0,nrow(st_raw))
st_raw$treat1 = rep(0,nrow(st_raw))
for (i in 1:nrow(st_raw)) {
  st_raw$tipo[i] = ifelse(st_raw$Comunidad[i] %in% c(1,3,5),"A",ifelse(st_raw$Comunidad[i] %in% c(4,6,9), "B","C"))
  st_raw$treat1[i] = ifelse(st_raw$tipo[i]=="A",1,0)
}
dm = dummy(st_raw$treat1)

st_raw$Edad_del_encuestado[1:92] = st_raw$Edad_del_encuestado[93:184] - 1
st_raw$cuant_ahor = as.numeric(st_raw$cuant_ahor)

## export df

fab_df = st_raw

write.csv(fab_df,"fab_df_B.csv")
