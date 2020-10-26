setwd("C:/Users/fc3/Box/Fabretto (Federico Ceballos)/Fabretto")

library(tidyverse)
library(Rmisc)
library(foreach)

# Functions

confidence_interval <- function(vector, interval) {
  vec_sd <- sd(vector, na.rm = T)
  n <- length(!is.na(vector))
  vec_mean <- mean(vector, na.rm = T)
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  result <- c("mean" = vec_mean,"lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}

baseR.replace      <- function(x) { replace(x, is.na(x), 0) }

# Students

## Descriptive statistics: students

fab_df = read.csv("fab_df_A.csv")
desc_vars = c("acc_cred_BASE","cuant_cred_BASE","ahorro_BASE",
              "cuant_ahor_BASE","tec_agr_BASE","tec_pec_BASE",
              "tec_merc_BASE","tec_rec_BASE","mipa_tec_BASE",
              "mipa_cont_BASE","hhd_sz","no_fath")

fab_df[,desc_vars] = apply(fab_df[,desc_vars], 2, baseR.replace)

descr_A = as.data.frame(matrix(0,13,4))

for (i in 1:length(desc_vars)) {
  mn = aggregate(as.formula(paste(desc_vars[i],"~tipo")), data = fab_df, FUN =  mean, na.action = )
  descr_A[i,1:3] = as.vector(mn[,2])
  ttest = summary(aov(as.formula(paste(desc_vars[i],"~tipo")), data = fab_df))
  descr_A[i,4] = ttest[[1]][["Pr(>F)"]][1]
}
descr_A[13,1:3] = c(sum(fab_df$tipo == "A"),sum(fab_df$tipo == "B"),sum(fab_df$tipo == "C"))
rownames(descr_A) = c(desc_vars,"N")
colnames(descr_A) = c("Tipo A","Tipo B","Tipo C","P-value")

## Summary statistics by group: Students

conf_var = c(sub("_BASE","",desc_vars),desc_vars)
type = c("A","B","C")

conf_std = data.frame()
for(j in 1:length(type)) {
  b = as.data.frame(foreach(i = 1:length(conf_var), .combine = rbind) %do% {
    a = unlist(confidence_interval(fab_df[which(fab_df$tipo == type[j]),conf_var[i]], .95))
    return(a)
  })
  if(j == 1){
    conf_std = b
  } else {
    conf_std = rbind(conf_std,b)
  }
}
conf_std$var = rep(conf_var[1:12],6)
conf_std$time = rep(c(rep(0,12),rep(1,12)),3)
conf_std$tipo = c(rep("A",24),rep("B",24),rep("C",24))

## boxplots: students

pd <- position_dodge(0.1) # move them .05 to the left and right

cash_df = conf_std[which(conf_std$var == "acc_cred"|conf_std$var == "cuant_cred"|conf_std$var == "ahorro"|conf_std$var == "cuant_ahor"),]
bxp_cash_A = ggplot(cash_df, aes(x=time, y=mean, colour=tipo, group=tipo)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  facet_wrap(~var
             , scales = "free_y")+ 
  theme_bw()

tec_df = conf_std[which(conf_std$var == "tec_agr"|conf_std$var == "tec_pec"|conf_std$var == "tec_rec"|conf_std$var == "tec_merc"),]
bxp_tec_A = ggplot(tec_df, aes(x=time, y=mean, colour=tipo, group=tipo)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  facet_wrap(~var
             , scales = "free_y")+ 
  theme_bw()

mipa_df = conf_std[which(conf_std$var == "mipa_tec"|conf_std$var == "mipa_cont"),]
bxp_mipa_A = ggplot(mipa_df, aes(x=time, y=mean, colour=tipo, group=tipo)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  facet_wrap(~var
             , scales = "free_y")+ 
  theme_bw()

## Agricultural advise

### who
fab_df$mipa_tec[which(is.na(fab_df$mipa_tec))] = 0
who_df = fab_df %>%
  group_by(time,treat1) %>%
  dplyr::summarise(mipa_agr = mean(mipa_tec, na.rm = T),mipa_acc = mean(mipa_cont, na.rm = T))

who_df = reshape(who_df, direction='long', 
                 varying= colnames(who_df)[3:4], 
                 timevar="time",
                 times=c("MIPA Agriculture","MIPA Accounting"),
                 v.names= "MIPA",
                 idvar=c("time","treat1"))
who_df$round = c(0,0,1,1,0,0,1,1)

gph_who_agr = ggplot(data=who_df, aes(x=factor(treat1), y=MIPA, fill=factor(round))) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels= c("Control","Treat")) +
  scale_fill_manual(name = "Time",
                    labels = c("Baseline","Endline"),
                    values = c("darkgoldenrod1","dodgerblue3")) +
  xlab("MIPA test") +
  ylab("Percentage") +
  facet_wrap(~time) +
  ggtitle("Changes in MIPA scores") +
  theme(plot.title = element_text(hjust = 0.5, size=30),
        legend.position="right",
        legend.direction="vertical",
        legend.text=element_text(size=16),
        axis.text=element_text(size=14),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))
ggsave("mipa_sdt.png", width = 40, height = 20, units = "cm")


# Parents

## Descriptive statistics: Parents

fab_df = read.csv("fab_df_B.csv")
desc_vars = c("ingr_BASE","area_BASE",
              "crop_inc_BASE","acc_cred_BASE","cuant_cred_BASE","tec_agr_BASE","tec_pec_BASE",
              "tec_merc_BASE","tec_rec_BASE","mipa_tec_BASE",
              "mipa_cont_BASE","Edad_del_encuestado_BASE","Nivel_educativo_del_encuestado_BASE")

fab_df[,desc_vars] = apply(fab_df[,desc_vars], 2, baseR.replace)

descr_B = as.data.frame(matrix(0,14,3))

for (i in 1:length(desc_vars)) {
  mn = aggregate(as.formula(paste(desc_vars[i],"~tipo")), data = fab_df, FUN =  mean)
  descr_B[i,1:3] = as.vector(mn[,2])
  ttest = summary(aov(as.formula(paste(desc_vars[i],"~tipo")), data = fab_df))
  descr_B[i,4] = ttest[[1]][["Pr(>F)"]][1]
}

descr_B[13,1:3] = c(sum(fab_df$tipo == "A"),sum(fab_df$tipo == "B"),sum(fab_df$tipo == "C"))
rownames(descr_B) = c(desc_vars,"N")
colnames(descr_B) = c("Tipo A","Tipo B","Tipo C","P-value")

## Summary statistics by group: Parents

conf_var = c(sub("_BASE","",desc_vars),desc_vars)
type = c("A","B","C")

conf_par = data.frame()
for(j in 1:length(type)) {
  b = as.data.frame(foreach(i = 1:length(conf_var), .combine = rbind) %do% {
    a = unlist(confidence_interval(fab_df[which(fab_df$tipo == type[j]),conf_var[i]], .95))
    return(a)
  })
  if(j == 1){
    conf_par = b
  } else {
    conf_par = rbind(conf_par,b)
  }
}
conf_par$var = rep(conf_var[1:13],6)
conf_par$time = rep(c(rep(0,13),rep(1,13)),3)
conf_par$tipo = c(rep("A",26),rep("B",26),rep("C",26))

## boxplots: parents

pd <- position_dodge(0.1) # move them .05 to the left and right

cash_df = conf_par[which(conf_par$var == "acc_cred"|conf_par$var == "cuant_cred"|conf_par$var == "ingr"|conf_par$var == "perc_ext"),]
bxp_cash_B = ggplot(cash_df, aes(x=time, y=mean, colour=tipo, group=tipo)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  facet_wrap(~var
             , scales = "free_y")+ 
  theme_bw()

tec_df = conf_par[which(conf_par$var == "tec_agr"|conf_par$var == "tec_pec"|conf_par$var == "tec_rec"|conf_par$var == "tec_merc"),]
bxp_tec_B = ggplot(tec_df, aes(x=time, y=mean, colour=tipo, group=tipo)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  facet_wrap(~var
             , scales = "free_y")+ 
  theme_bw()

mipa_df = conf_par[which(conf_par$var == "mipa_tec"|conf_par$var == "mipa_cont"),]
bxp_mipa_B = ggplot(mipa_df, aes(x=time, y=mean, colour=tipo, group=tipo)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  facet_wrap(~var
             , scales = "free_y")+ 
  theme_bw()

## Agricultural advise

fab_df$mipa_tec[which(is.na(fab_df$mipa_tec))] = 0
who_df = fab_df %>%
  group_by(time,treat1) %>%
  dplyr::summarise(mipa_agr = mean(mipa_tec, na.rm = T),mipa_acc = mean(mipa_cont, na.rm = T))

who_df = reshape(who_df, direction='long', 
                 varying= colnames(who_df)[3:4], 
                 timevar="time",
                 times=c("MIPA Agriculture","MIPA Accounting"),
                 v.names= "MIPA",
                 idvar=c("time","treat1"))
who_df$round = c(0,0,1,1,0,0,1,1)

gph_who_mipa = ggplot(data=who_df, aes(x=factor(treat1), y=MIPA, fill=factor(round))) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels= c("Control","Treat")) +
  scale_fill_manual(name = "Time",
                    labels = c("Baseline","Endline"),
                    values = c("darkgoldenrod1","dodgerblue3")) +
  xlab("MIPA test") +
  ylab("Percentage") +
  facet_wrap(~time) +
  ggtitle("Changes in MIPA scores") +
  theme(plot.title = element_text(hjust = 0.5, size=30),
        legend.position="right",
        legend.direction="vertical",
        legend.text=element_text(size=16),
        axis.text=element_text(size=14),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))
ggsave("mipa_prt.png", width = 40, height = 20, units = "cm")

### who
fab_df$qui_agr[which(is.na(fab_df$qui_agr))] = 0
who_df = fab_df %>%
  group_by(time,treat1,qui_agr) %>%
  tally()

a = fab_df[which(fab_df$qui_agr != 0),] %>%
  group_by(time,treat1) %>%
  tally() %>%
  mutate(perc = if_else(treat1 == 0, n/62,n/30))
a$qui_agr = rep(7,4)

who_df = rbind(who_df,a)
who_df = who_df %<>%
  group_by(treat1) %>%
  expand(qui_agr,time) %>% #in each id expand the dates
  left_join(who_df) %>%
  mutate(perc = if_else(treat1 == 0, n/62,n/30))
who_df$treat1 = ifelse(who_df$treat1 == 1, "Treatment","Control")

x.lb = c("None","Family \n(hhd)","Family \n(ext)","Friend","Gvmt. \n Extension","SATec \ntutor \n/ Student","Other","Any")

gph_who_agr2 = ggplot(data=who_df, aes(x=factor(qui_agr), y=perc, fill=factor(time))) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels= x.lb) +
  scale_fill_manual(name = "Time",
                    labels = c("Baseline","Endline"),
                    values = c("darkgoldenrod1","dodgerblue3")) +
  xlab("Source") +
  ylab("Percentage") +
  facet_wrap(~treat1)+
  ggtitle("Source of agricultural advice") +
  theme(plot.title = element_text(size = 30),
        legend.direction="vertical",
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))
ggsave("adv_prt.png", width = 40, height = 20, units = "cm")

### what

## Loans
fab_df$qui_cred[which(is.na(fab_df$qui_cred))] = 0
who_df = fab_df %>%
  group_by(time,treat1,qui_cred) %>%
  tally()

a = fab_df[which(fab_df$qui_cred != 0),] %>%
  group_by(time,treat1) %>%
  tally() %>%
  mutate(perc = if_else(treat1 == 0, n/62,n/30))
a$qui_cred = rep(7,2)

who_df = rbind(who_df,a)
who_df = who_df %<>%
  group_by(treat1) %>%
  expand(qui_cred,time) %>% #in each id expand the dates
  left_join(who_df) %>%
  mutate(perc = if_else(treat1 == 0, n/62,n/30))
who_df$treat1 = ifelse(who_df$treat1 == 1, "Treatment","Control")

x.lb = c("None","Bank","Savings \ncoop.","NGO \n(Not Fabretto)","Fabretto","Any")

gph_who_cred2 = ggplot(data=who_df, aes(x=factor(qui_cred), y=perc, fill=factor(time))) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels= x.lb) +
  scale_fill_manual(name = "Time",
                    labels = c("Baseline","Endline"),
                    values = c("darkgoldenrod1","dodgerblue3")) +
  xlab("Source") +
  ylab("Percentage") +
  facet_wrap(~treat1)+
  ggtitle("Source of loans") +
  theme(plot.title = element_text(size = 30),
        legend.direction="vertical",
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        strip.text.x = element_text(size = 12),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))
ggsave("loan_prt.png", width = 40, height = 20, units = "cm")
