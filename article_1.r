library(dplyr)
library(readxl)
library(ggplot2)
library(patchwork)

data = read_excel("mereni_davek_IZ-facetovy_syndrom_2.xlsx", sheet ="results")

glimpse(data)

# 2 people have pohlavi=="O", replaced with "F"
# TODO: consult
data = data %>%
  mutate(pohlavi = if_else(pohlavi == "O" | pohlavi == "f", "F", pohlavi)) %>%
  mutate(Eff_dose = as.numeric(Eff_dose)) %>%
  mutate(topogram = as.factor(topogram)) %>%
  mutate(protokol = recode(protokol,
                           "Specials^INTERVENCE_LowDose_High_KV (Adult)" = "Low_HQ",
                           "Specials^INTERVENCE_LowDose (Adult)" = "Low",
                           "Specials^INTERVENCE_Standard (Adult)" = "Std"
                           ))


###
# DATA EXPLORATION
###

hist(data$vek)
par(mfrow=c(1,2))
hist(data$vek[data$pohlavi=="F"])
hist(data$vek[data$pohlavi=="M"])
par(mfrow=c(1,1))

par(mfrow = c(1, 3))
hist(data$VAS_pred)
hist(data$VAS_po)
hist(data$delta_VAS)
par(mfrow = c(1, 1))

table(data$pohlavi)

par(mfrow=c(1,2))
hist(data$Total_mAS)
hist(log(data$Total_mAS))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(data$Total_DLP)
hist(log(data$Total_DLP))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(data$Eff_dose)
hist(log(data$Eff_dose))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(data$CTDI_vol)
hist(log(data$CTDI_vol))
par(mfrow=c(1,1))

table(data$topogram) # only 4% in topogram=="2"

table(data$pocet_opakovani_scanu)
table(data$pocet_urovni)
# pocet_urovni is pretty much linearly dependent on pocet_opakovani_scanu
# not so much on druh_vykonu
summary(lm(pocet_opakovani_scanu~pocet_urovni+druh_vykonu, data=data))




###
# MODELS
###

M1 = lm(Eff_dose ~ vek, data = data)
M2 = lm(log(Eff_dose) ~ vek, data = data)

