setwd("C:/Users/fc3/Box Sync/Fabretto/Fabretto")

library(readxl)
library(dplyr)
library(dummies)

base_s = read_excel("base_raw.xlsx",sheet = "Tipo A", col_names = T)
base_s$time = 0

# Data cleaning

## Baseline

base_s$cod_est = gsub("\\.", "", base_s$cod_est) # remove dots from id
base_s$Nivel_educativo_del_encuestado = base_s$Nivel_educativo_del_encuestado -1 # change current level to last level
base_s = base_s[-which(is.na(base_s$pariente_1)),] # remove students with no parents
base_s$pariente_10 = as.numeric(base_s$pariente_10) # set pariente_10 as numeric
cols.num = seq(7,61,6)
base_s[cols.num] = sapply(base_s[cols.num],as.numeric) # set age of parents as numeric
base_s[, 66:105][base_s[, 66:105] != 1] = 0         # impute non-1 values to 0
base_s[, 66:105][is.na(base_s[, 66:105])] = 0
base_s$dist_sat = as.numeric(base_s$dist_sat)
base_s$con_sat[is.na(base_s$con_sat)] = 2   # impute NAs and 2 to 0 know sat
base_s$con_sat[base_s$con_sat != 1] = 0
base_s$part_sat[is.na(base_s$part_sat)] = 2   # impute NAs and 2 to 0 know sat
base_s$part_sat[base_s$part_sat != 1] = 0
base_s[, c(110:112,114:116,118:120,122:124)][base_s[, c(110:112,114:116,118:120,122:124)] != 1] = 0         # impute non-1 values to 0 technology adoption
base_s[, c(110:112,114:116,118:120,122:124)][is.na(base_s[, c(110:112,114:116,118:120,122:124)])] = 0
base_s$motiv_no[is.na(base_s$motiv_no)] = 0  # impute NA to 0
base_s$acc_cred[is.na(base_s$acc_cred)] = 2   # impute non-1 values access to credit
base_s$acc_cred[base_s$acc_cred != 1] = 0
base_s[, 128:129][is.na(base_s[, 128:129])] = 0  # impute NA to 0 quantity and interest of the credit
base_s$ahorro[is.na(base_s$ahorro)] = 2   # impute non-1 values access to credit
base_s$ahorro[base_s$ahorro != 1] = 0
base_s[, 134][is.na(base_s[, 134])] = 0  # impute NA to 0 quantity of savings
base_s$cap_ahor[is.na(base_s$cap_ahor)] = 2   # impute non-1 values access to credit
base_s$cap_ahor[base_s$cap_ahor != 1] = 0
base_s[, c(136,138:139,141:142,144:145,147)][base_s[, c(136,138:139,141:142,144:145,147)] != 1] = 0         # impute non-1 values to 0
base_s[, c(136,138:139,141:142,144:145,147)][is.na(base_s[, c(136,138:139,141:142,144:145,147)])] = 0
base_s$cuant_cred = as.numeric(base_s$cuant_cred)
base_s$cuant_ahor = as.numeric(base_s$cuant_ahor)

## Midline

mid_s = read_excel("mid_raw.xlsx",sheet = "Tipo A", col_names = T)
mid_s$time = 1

mid_s$Comunidad = ifelse(mid_s$Comunidad == "Jobo",1,ifelse(mid_s$Comunidad == "Encino",2,ifelse(mid_s$Comunidad == "Estancia",3,ifelse(mid_s$Comunidad == "El Espino",4,
                         ifelse(mid_s$Comunidad == "Macarali",5,ifelse(mid_s$Comunidad == "Siuce",6,ifelse(mid_s$Comunidad == "San Francisco",7,ifelse(mid_s$Comunidad == "La Florecida",8,
                         ifelse(mid_s$Comunidad == "es de la Florecida/San Francisco",8,9)))))))))
mid_s[mid_s == 999] <- NA  # replace 999 with NA
for (i in 1:nrow(mid_s)) {
  mid_s[i,seq(10,46,6)] = gsub("[a-zA-Z ]", "", mid_s[i,seq(10,46,6)])
}
mid_s[seq(10,46,6)] = sapply(mid_s[seq(10,46,6)],as.numeric)

# mid_s$cuant_cred = as.numeric(mid_s$cuant_cred)
# mid_s$cuant_ahor = as.numeric(mid_s$cuant_ahor)
# 
# names(base_s) %in% names(mid_s)
# names(mid_s) %in% names(base_s)
# match_b = base_s$cod_est %in% mid_s$cod_est
# match_m = mid_s$cod_est %in% base_s$cod_est
# base_s = base_s[match_b,]
# mid_s = mid_s[match_m,]
# mid_s$cod_est = as.character(mid_s$cod_est)

## Merged lines

st_raw = rbind(base_s,mid_s)

### Treatment variable

st_raw$tipo = rep(0,nrow(st_raw))
st_raw$treat1 = rep(0,nrow(st_raw))
for (i in 1:nrow(st_raw)) {
  st_raw$tipo[i] = ifelse(st_raw$Comunidad[i] %in% c(1,3,5),"A",ifelse(st_raw$Comunidad[i] %in% c(4,6,9), "B","C"))
  st_raw$treat1[i] = ifelse(st_raw$tipo[i]=="A",1,0)
}
dm = dummy(st_raw$treat1)

### Household size

st_raw$hhd_sz = rep(0,nrow(st_raw))
for (i in 1:nrow(st_raw)) {
  st_raw$hhd_sz[i] = length(which(st_raw[i,seq(6,60,6)] != ""))
}

### Male-headed household

st_raw$no_fath = rep(0,nrow(st_raw))
for (i in 1:nrow(st_raw)) {
  st_raw$no_fath[i] = ifelse(st_raw[i,seq(6,60,6)] != 3,1,0)
}

### Age of student at baseline

st_raw$Edad_del_encuestado[1:113] = st_raw$Edad_del_encuestado[114:226] - 1

## Clean df

drop = c("pariente_1","edad_par_1","sex_par_1","educ_par_1","conv_par_1", "mot_par_1",
         "pariente_2","edad_par_2","sex_par_2","educ_par_2","conv_par_2","mot_par_2", 
         "pariente_3","edad_par_3","sex_par_3","educ_par_3","conv_par_3","mot_par_3", 
         "pariente_4","edad_par_4","sex_par_4","educ_par_4","conv_par_4","mot_par_4", 
         "pariente_5","edad_par_5","sex_par_5","educ_par_5","conv_par_5","mot_par_5", 
         "pariente_6","edad_par_6","sex_par_6","educ_par_6","conv_par_6","mot_par_6", 
         "pariente_7","edad_par_7","sex_par_7","educ_par_7","conv_par_7","mot_par_7", 
         "pariente_8","edad_8","sex_par_8","educ_par_8","mes_hogar_8","mot_par_8", 
         "pariente_9","edad_9","sex_par_9","educ_par_9","mes_hogar_9","mot_par_9", 
         "pariente_10","edad_10","sex_par_10","educ_par_10","mes_hogar_10","mot_par_10")
fab_df = st_raw[,-which(colnames(st_raw) %in% drop)]
fab_df[,c("tec_merc")] = apply(fab_df[,c("tec_merc")],2,as.numeric)

write.csv(fab_df,"fab_df_A.csv")

## Attrition bias

base_s$attr = ifelse(base_s$cod_est %in% mid_s$cod_est,1,0)

base_s$tipo = rep(0,nrow(base_s))
base_s$treat1 = rep(0,nrow(base_s))
for (i in 1:nrow(base_s)) {
  base_s$tipo[i] = ifelse(base_s$Comunidad[i] %in% c(1,3,5),"A",ifelse(base_s$Comunidad[i] %in% c(4,6,9), "B","C"))
  base_s$treat1[i] = ifelse(base_s$tipo[i]=="A",1,0)
}

### Household size

base_s$hhd_sz = rep(0,nrow(base_s))
for (i in 1:nrow(base_s)) {
  base_s$hhd_sz[i] = length(which(base_s[i,seq(6,60,6)] != ""))
}

### Male-headed household

base_s$no_fath = rep(0,nrow(base_s))
for (i in 1:nrow(base_s)) {
  base_s$no_fath[i] = ifelse(base_s[i,seq(6,60,6)] != 3,1,0)
}

reg = lm(attr ~ treat1 + acc_cred+cuant_cred+ahorro+
                               cuant_ahor+tec_agr+tec_pec+
                               tec_merc+tec_rec+mipa_tec+
                               mipa_cont+hhd_sz+no_fath, data = base_s)

reg = lm(attr ~ treat1, data = base_s)
