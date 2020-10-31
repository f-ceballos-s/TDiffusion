setwd("C:/Users/fc3/Box Sync/Fabretto/Fabretto")

library(tidyverse)
library(Rmisc)
library(plm)
library(huxtable)
library(jtools)
library(lmtest)
library(RCT)
library(pwr)

# Students

fab_df = read.csv("fab_df_A.csv")
fab_df$Comunidad  <- factor(fab_df$Comunidad)
a           <- as.data.frame(model.matrix(~fab_df$Comunidad-1))
colnames(a) <- (substring( names(a), 8, 17))
fab_df <- cbind(fab_df, a)
fab_df[is.na(fab_df)] = 0

Y = colnames(fab_df[c(68,69,73,75,51,55,59,63,89,90)])
D     <- rep("treat1", length(Y))

controls     <- c("no_fath", "hhd_sz", "Genero_del_encuestado", "Edad_del_encuestado", "Nivel_educativo_del_encuestado"
                  # ,"tec_agr_BASE", "acc_cred_BASE", "mipa_tec_BASE", "mipa_cont_BASE"
                  )
# controls     <- c(controls, names(fab_df)[(substring( names(fab_df), 1, 9)=="Comunidad")])

# generate formula for x, xl is for linear models
X <- ""

for(i in 1:length(controls)){
  X <- paste(X, controls[i], "+", sep = "")
}

reg1 = list()
reg2 = list()
st_res = matrix(0,3,10)
for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d,"+time+factor(Comunidad)"))
  reg = lm(form1,data = fab_df)
  reg1[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
  form2 = as.formula(paste(y, "~",x,d,"+time+factor(Comunidad)"))
  reg = lm(form2,data = fab_df)
  reg2[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC0", cluster = "Comunidad"))
}

## Comparison of means

for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d))
  reg = lm(form1,data = fab_df)
  reg1[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC0", cluster = "Comunidad"))
  form2 = as.formula(paste(y, "~",x,d))
  reg = lm(form2,data = fab_df)
  reg2[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC0", cluster = "Comunidad"))
}

export_summs(reg2[[1]],reg2[[2]],reg2[[3]],reg2[[4]],reg2[[5]],reg2[[6]],reg2[[7]],reg2[[8]],reg2[[9]] ,reg2[[10]] ,
             scale = TRUE,
             coefs = c("Treat" = "treat1"),
             # model.names = c("Violence","Illicit crops"),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             to.file = "docx",
             file.name = "tb2.docx"
)

## Difference-in-differences

reg5 = list()
st_res = matrix(0,3,10)
for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d,"+time+",d,":time+factor(Comunidad)"))
  reg = plm(form1,data = fab_df,effect = "individual",model = "within",index = c("cod_est","time"))
  reg5[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC0", cluster = "group"))
}

export_summs(reg5[[1]],reg5[[2]],reg5[[3]],reg5[[4]],reg5[[5]],reg5[[6]],reg5[[7]],reg5[[8]],reg5[[9]] ,reg5[[10]] ,
             scale = TRUE,
             coefs = c("Treat x time" = "treat1:time1",
                       "Time" = "time1"
                       ),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             to.file = "docx",
             file.name = "tb2.docx"
)

## Heterogeneity

### Gender

reg9 = list()
reg10 = list()
fab_df$sex = ifelse(fab_df$Genero_del_encuestado == "M", 0, 1)
for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d,"+time+sex+",d,":time+",d,":sex+","sex:time+",d,":time:sex+","factor(Comunidad)"))
  reg = lm(form1,data = fab_df)
  reg9[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
  form2 = as.formula(paste(y, "~",x,d,"+time+sex+",d,":time+",d,":sex+","sex:time+",d,":time:sex+","factor(Comunidad)"))
  reg = lm(form2,data = fab_df)
  reg10[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
}

export_summs(reg10[[1]],reg10[[2]],reg10[[3]],reg10[[4]],reg10[[5]],reg10[[6]],reg10[[7]],reg10[[8]],reg10[[9]] ,reg10[[10]] ,
             scale = TRUE,
             coefs = c("Treat x time" = "treat1:time",
                       "Treat x time x female" = "treat1:time:sex"),
             # model.names = c("Violence","Illicit crops"),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             to.file = "docx",
             file.name = "tb6.docx"
)

# Parents

fab_df = read.csv("fab_df_B.csv")
fab_df[is.na(fab_df)] = 0
colnames(fab_df)[81] = "perc_ext"
fab_df = fab_df[!duplicated(fab_df[,c("cod_par","time")]),]

Y = colnames(fab_df[c(80,179,154,155,137,141,145,149,175,176)])
D     <- rep("treat1", length(Y))

controls     <- c("Genero_del_encuestado", "Edad_del_encuestado", "Nivel_educativo_del_encuestado")

# generate formula for x, xl is for linear models
X <- ""

for(i in 1:length(controls)){
  X <- paste(X, controls[i], "+", sep = "")
}

reg3 = list()
reg4 = list()
for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d,"+time+factor(Comunidad)"))
  reg = lm(form1,data = fab_df)
  reg3[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
  form2 = as.formula(paste(y, "~",x,d,"+time+factor(Comunidad)"))
  reg = lm(form2,data = fab_df)
  reg4[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
}

## Comparison of means

for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d))
  reg = lm(form1,data = fab_df)
  reg3[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
  form2 = as.formula(paste(y, "~",x,d))
  reg = lm(form2,data = fab_df)
  reg4[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
}

export_summs(reg4[[1]],reg4[[2]],reg4[[3]],reg4[[4]],reg4[[5]],reg4[[6]],reg4[[7]],reg4[[8]],reg4[[9]] ,reg4[[10]] ,
             coefs = c("Treat" = "treat1"),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             to.file = "docx",
             file.name = "tb4.docx"
)

## Difference-in-differences

reg8 = list()
st_res = matrix(0,3,10)
for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d,"+time+",d,":time+factor(Comunidad)"))
  reg = lm(form1,data = fab_df)
  reg8[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC0", cluster = "group"))
}

export_summs(reg8[[1]],reg8[[2]],reg8[[3]],reg8[[4]],reg8[[5]],reg8[[6]],reg8[[7]],reg8[[8]],reg8[[9]] ,reg8[[10]] ,
             scale = TRUE,
             coefs = c("Treat x time" = "treat1:time1",
                       "Time" = "time1"
             ),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             to.file = "docx",
             file.name = "tb2.docx"
)

reg11 = list()
reg12 = list()
fab_df$land = ifelse(fab_df$area <= median(fab_df$area), 1, 0)
for (l in 1:length(Y)) {
  y = Y[l]
  d = D[l]
  x = X
  form1 = as.formula(paste(y, "~",d,"+time+land+",d,":time+",d,":land+","land:time+",d,":time:land+","factor(Comunidad)"))
  reg = lm(form1,data = fab_df)
  reg11[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
  form2 = as.formula(paste(y, "~",x,d,"+time+land+",d,":time+",d,":land+","land:time+",d,":time:land+","factor(Comunidad)"))
  reg = lm(form2,data = fab_df)
  reg12[[l]] = coeftest(reg, vcov. = vcovHC(reg, type = "HC", cluster = "Comunidad"))
}

export_summs(reg12[[1]],reg12[[2]],reg12[[3]],reg12[[4]],reg12[[5]],reg12[[6]],reg12[[7]],reg12[[8]],reg12[[9]] ,reg12[[10]] ,
             scale = TRUE,
             coefs = c("Treat x time" = "treat1:time",
                       "Treat x time x low land" = "treat1:time:land"),
             # model.names = c("Violence","Illicit crops"),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             to.file = "docx",
             file.name = "tb7.docx"
)

# Ex-post power calculations

## Students

### MIPA tec
ef.sz = 7.19/9.25 #sd(fab_df$mipa_tec[which(fab_df$treat1 ==0)], na.rm = T)
pwr.std.mipa.tec = pwr.t2n.test(n1 = 40, n2 = 73, d = ef.sz, sig.level = .5)

### MIPA cont
ef.sz = 1.12/1.87 #sd(fab_df$mipa_cont[which(fab_df$treat1 ==0)], na.rm = T)
pwr.std.mipa.con = pwr.t2n.test(n1 = 40, n2 = 73, d = ef.sz, sig.level = .5)

## Parents

### MIPA tec
ef.sz = 3.84/6.10612 #sd(fab_df$mipa_tec[which(fab_df$treat1 ==0)], na.rm = T)
pwr.prt.mipa.tec = pwr.t2n.test(n1 = 30, n2 = 62, d = ef.sz, sig.level = .5)

### MIPA cont
ef.sz = 0.77/1.325345 #sd(fab_df$mipa_cont[which(fab_df$treat1 ==0)], na.rm = T)
pwr.prt.mipa.con = pwr.t2n.test(n1 = 30, n2 = 62, d = ef.sz, sig.level = .5)

### Credit
ef.sz = 0.26/0.8920034 #sd(fab_df$acc_cred[which(fab_df$treat1 ==0)], na.rm = T)
pwr.prt.cred.tec = pwr.t2n.test(n1 = 30, n2 = 62, d = ef.sz, sig.level = .5)

### Adoption
ef.sz = .19/0.9615199 #sd(fab_df$tec_agr[which(fab_df$treat1 ==0)], na.rm = T)
pwr.prt.adop.con = pwr.t2n.test(n1 = 30, n2 = 62, d = ef.sz, sig.level = .5)

