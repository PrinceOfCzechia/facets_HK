library(dplyr)
library(readxl)
library(ggplot2)
library(patchwork)

data = read_excel("facetovy_syndrom_ext.xlsx", sheet ="results_2")

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

mri = data %>%
  filter(MR == "ano")

data = data %>%
  select(-c(modic_L4_R1, R2_m_L4, R3_m_L4, modic_L5_R1, R2_m_L5, R3_m_L5))


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

par(mfrow=c(1,2))
hist(data$vyska[data$pohlavi=="M"])
hist(data$vyska[data$pohlavi=="F"])
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(data$hmotnost[data$pohlavi=="M"])
hist(data$hmotnost[data$pohlavi=="F"])
par(mfrow=c(1,1))


# article 2 exclusive variables
table(data$L4_osetrena)
table(data$L5_osetrena)
table(data$L4_osetrena, data$L5_osetrena) # most people have both

par(mfrow=c(1,2))
hist(data$psoas_L_plocha)
hist(data$psoas_R_plocha)
par(mfrow=c(1,1))
hist(data$psoas_R_plocha-data$psoas_L_plocha)

par(mfrow=c(1,2))
hist(data$psoas_L_denzita)
hist(data$psoas_R_denzita)
par(mfrow=c(1,1))
summary(data$psoas_L_denzita)
summary(data$psoas_R_denzita)
hist(data$psoas_R_denzita-data$psoas_L_denzita)

summary(data$psoas_muscle_index)
hist(data$psoas_muscle_index)



###
# MODELS
###

M1 = lm(Eff_dose ~ vek, data = data)
M2 = lm(log(Eff_dose) ~ vek, data = data)

