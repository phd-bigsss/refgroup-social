set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
rm(list=ls())
# load(here::here("input/data/proc/study2.RData"))
load(here::here("input/data/original/esi-2023---personas.rdata"))
pacman::p_load(
  correlation,
  lme4,
  plm,
  panelr,
  texreg,
  dplyr,
  estimatr,
  marginaleffects,
  ggplot2,
  sjmisc,
  MLMusingR,
  ggExtra,
  sjPlot
)  


view_df(esi_2023_personas)

frq(esi_2023_personas$edad)


frq(esi_2023_personas$mes_central)
