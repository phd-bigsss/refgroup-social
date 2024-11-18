set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
rm(list=ls())
# load(here::here("input/data/proc/study2.RData"))
load(here::here("input/data/proc/study2.RData")); elsoc_long<- elsoc_long
pacman::p_load(correlation,lme4,plm,panelr,texreg,dplyr,estimatr,marginaleffects,ggplot2,sjmisc,MLMusingR,ordinal)  
dfreg <-
  elsoc_long %>%
  group_by(idencuesta) %>%
  mutate(just_educ = if_else(ola == 5 & is.na(just_educ), last(just_educ[ola == 6]), just_educ),
         just_salud = if_else(ola == 5 & is.na(just_salud), last(just_salud[ola == 6]), just_salud),
         just_pension = if_else(ola == 5 & is.na(just_pension), last(just_pension[ola == 6]), just_pension)
  ) %>%
  ungroup() 

dfreg$market<- rowMeans(dfreg[,c("just_educ","just_salud","just_pension")],na.rm = T)
dfreg$market <- ((dfreg$market-min(dfreg$market,na.rm = T))/(max(dfreg$market,na.rm = T)-min(dfreg$market,na.rm = T)))*100
sjlabelled::set_label(dfreg$market) <- "Market-based distribution of social services (3 items)"

dfreg$just_educ <- ((dfreg$just_educ-min(dfreg$just_educ,na.rm = T))/(max(dfreg$just_educ,na.rm = T)-min(dfreg$just_educ,na.rm = T)))*100
dfreg$just_salud <- ((dfreg$just_salud-min(dfreg$just_salud,na.rm = T))/(max(dfreg$just_salud,na.rm = T)-min(dfreg$just_salud,na.rm = T)))*100
dfreg$just_pension <- ((dfreg$just_pension-min(dfreg$just_pension,na.rm = T))/(max(dfreg$just_pension,na.rm = T)-min(dfreg$just_pension,na.rm = T)))*100

dfreg$egp6_hh <- car::recode(
  dfreg$egp11_hh_label,
  recodes = "
'I Higher Controllers'='Service class (I-higher grade)';
'II Lower Controllers'='Service class (II-lower grade)';
c('IIIa Routine Nonmanual','IIIb Lower Sales-Service')='Routine nonmanuals (IIIa.b)';
c('IVa Self-employed with employees','IVb Self-employed with no employees','IVc Self-employed Farmer')='Self-employed (IVa.b.c)';
c('V Manual Supervisors','VI Skilled Worker')='Skilled manual workers and supv. (V+VI)';
c('VIIa Unskilled Worker','VIIb Farm Labor')='Non-skilled manual workers (VIIa.b)';NA='No information'",
levels = c(
  "Service class (I-higher grade)",
  "Service class (II-lower grade)",
  "Routine nonmanuals (IIIa.b)",
  "Self-employed (IVa.b.c)",
  "Skilled manual workers and supv. (V+VI)",
  "Non-skilled manual workers (VIIa.b)",
  'No information'
)
);frq(dfreg$egp6_hh)


dfreg$quintil <- as.numeric(dfreg$quintil_fx)
dfreg$oesch5_r_fx <- factor(dfreg$oesch5_r_fx,ordered = F)


dfw1<- dfreg %>% filter(ola==1) %>% 
  select(just_educ,educ,quintil1,sexo,edad,strong_tie_t1,empst,egp8_hh_label_na,know_low,know_mid,know_hig,know_total) %>% 
  na.omit()

frq(dfw1$know_hig)

dfreg$n_total


lm_educ<- lm(just_educ~sexo+edad+strong_tie_t1+empst+egp8_hh_label_na,data = dfw1)
lm_educ_class<- lm(just_educ~educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na,data = dfw1)
lm_educ_high<- lm(just_educ~educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na+know_hig+know_mid+know_low+know_total,data = dfw1)

screenreg(list(lm_educ,lm_educ_class,lm_educ_high))

library(tidyverse)
library(broom)
library(scales)
library(ggdag)
library(dagitty)
library(truncnorm)
library(ipw)
library(WeightIt)

# Logit model to predict net use
model_predict_net <- glm(know_hig ~ educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na+know_total,
                         family = binomial(link = "logit"),
                         data = dfw1)

# Generate propensity scores and IPWs
net_data_ipw <- augment_columns(model_predict_net, dfw1,
                                type.predict = "response") %>% 
  rename(propensity = .fitted) %>% 
  mutate(ipw = (know_hig / propensity) + ((1 - know_hig) / (1 - propensity)))

model_predict_net_balances <- glm(know_hig ~ educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na+know_total,
                         family = binomial(link = "logit"),
                         data = net_data_ipw,weights = ipw)
screenreg(list(model_predict_net,model_predict_net_balances))

lm_educ_high<- lm(just_educ~educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na+know_hig,data = net_data_ipw)
lm_educ_high2<- lm(just_educ~educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na*know_hig,data = net_data_ipw)
lm_educ_high_ipw<- lm(just_educ~educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na+know_hig,data = net_data_ipw,weights = ipw)
lm_educ_high_ipw2<- lm(just_educ~educ+quintil1+sexo+edad+strong_tie_t1+empst+egp8_hh_label_na*know_hig,data = net_data_ipw,weights = ipw)
screenreg(list(lm_educ_high,lm_educ_high2,lm_educ_high_ipw,lm_educ_high_ipw2))

# dfreg$isei08res_ter_fx <- as.factor(ntile(dfreg$isei08res_fx,3));frq(dfreg$isei08res_ter_fx)

# Deflator
# For 2021: CPI = 100 (base year)
# For 2019: IPC = 3.0%
# For 2018: IPC = 2.6%
# For 2017: IPC = 2,3#
# For 2016: IPC = 2.7%

# The CPI values relative to 2021 are:
# 2019 CPI: 97.09
# 2018 CPI: 97.47
# 2017 CPI: 97.75
# 2016 CPI: 97.37

dfreg$cpi <- dfreg$ola
dfreg$cpi <- car::recode(dfreg$cpi,"1=97.37;2=97.75;3=97.47;4=97.09;5=100")
dfreg$real_income <- as.numeric((dfreg$ing_pc/dfreg$cpi)* 100)
dfreg$real_income_eq <- as.numeric((dfreg$ing_pc_eq/dfreg$cpi)* 100)
summary(dfreg$real_income)
summary(dfreg$real_income_eq)

dfreg$ing_pc_100 <- (dfreg$real_income_eq) # each unit is 100.000 CLP
dfreg$decile <- ntile(dfreg$ing_pc_100,10)

df1 <-  
  dfreg %>%
  filter(muestra==1) %>%
  # filter(ola %in% c(1,3,5)) %>%
  # filter(m0_edad %in% c(15:64)) %>%
  filter(ing_pc>0) %>%
  select(id="idencuesta",ola,
         strong_tie_t1,
         empst,
         just_educ,
         just_salud,
         just_pension,
         market,
         income=ing_pc_100,
         educ=educyear,
         # class=isei08res_ter_fx,
         age=m0_edad,
         sex=sexo_fx,
         weights1=ponderador_long_total,
         ponderador01,ponderador02
  ) %>%
  mutate(income=log(income)) %>%
  arrange(ola)
table(df1$ola)

df1 <-   na.omit(df1)

# fix to complete cases
idfreq<- frq(df1$id) %>% data.frame() %>% select(val,frq);filter<- idfreq %>% filter(frq>=1);df1 <- df1 %>% filter(id %in% filter$val)

dim(df1)

df1$time <- as.numeric(df1$ola)
table(df1$time)
# Model w/o isei range
model_wo_range <- 'diversitycfa_norange =~ diversityclass+crossclass + isei_sd+know_total'
fit_range<- lavaan::cfa(model = model_wo_range,data = df1,cluster = "id",
                        sampling.weights = "weights1"
)
lavaan::summary(fit_range,standardized=T,fit.measures=T)
# Model using blau, sd, extensivity and number of different "social classes" known (cross-class) is the best

factorscores<- lavaan::lavPredict(fit_range,newdata = df1)
idx <- lavaan::lavInspect(fit_range, "case.idx")
fscores <- lavaan::lavPredict(fit_range)
## loop over factors
for (fs in colnames(fscores)) {
  df1[idx, fs] <- fscores[ , fs]
}

df1 <- df1 %>% rename(diversitycfa=diversitycfa_norange);df1$diversityclass <- df1$diversitycfa 
paneldata <- panelr::panel_data(df1, id = id, wave = time)