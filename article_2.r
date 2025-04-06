library(dplyr)
library(readxl)
library(ggplot2)
library(patchwork)

data = read_excel("facetovy_syndrom_ext.xlsx", sheet ="results_2")

glimpse(data)

# 2 people have pohlavi=="O", replaced with "F"
data = data %>%
  mutate(pohlavi = if_else(pohlavi == "O" | pohlavi == "f", "F", pohlavi)) %>%
  mutate(eff_base = as.numeric(eff_base)) %>%
  mutate(topogram = as.factor(topogram)) %>%
  mutate(protokol = recode(protokol,
                           "Specials^INTERVENCE_LowDose_High_KV (Adult)" = "Low_HQ",
                           "Specials^INTERVENCE_LowDose (Adult)" = "Low",
                           "Specials^INTERVENCE_Standard (Adult)" = "Std"
                           )) %>%
  mutate(druh_vykonu = as.factor(druh_vykonu)) %>%
  mutate(lekar = as.factor(lekar)) %>%
  mutate(protokol = as.factor(protokol)) %>%
  mutate(protokol = relevel(protokol, ref="Low_HQ")) %>%
  mutate(FBSS = as.factor(FBSS))

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
hist(data$eff_base)
hist(log(data$eff_base))
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

summary(data$eff_base)

###
# PLOTS
###

# gender
boxplot(data$eff_base~data$pohlavi)
summary(data$eff_base[data$pohlavi=="F"])
summary(data$eff_base[data$pohlavi=="M"])
summary(lm(eff_base ~ pohlavi, data=data)) #R^2==0.2%

#age
plot(data$eff_base~data$vek)
lines(lowess(data$vek, data$eff_base), col="red")
summary(lm(eff_base ~ vek, data=data)) #R^2==0.7%

#BMI
plot(data$eff_base~data$BMI)
lines(lowess(data$BMI, data$eff_base), col="red")
summary(lm(eff_base ~ BMI, data=data)) #R^2==8.1%

# druh_vykonu
boxplot(data$eff_base~data$druh_vykonu)
summary(data$eff_base[data$druh_vykonu=="KOT"])
summary(data$eff_base[data$druh_vykonu=="RF"])
summary(lm(eff_base ~ druh_vykonu, data=data)) #R^2==1.0%

# lekar
boxplot(data$eff_base~data$lekar)
summary(data$eff_base[data$lekar=="Jandura"])
summary(data$eff_base[data$lekar=="Ryška"]) # more senior doctor
summary(data$eff_base[data$lekar=="Vajda"])
summary(lm(eff_base ~ lekar, data=data)) #R^2==23.0%

# pocet_urovni not independent of doctor
fisher.test(table(data$lekar, data$pocet_urovni))

# pocet_urovni
boxplot(data$eff_base~data$pocet_urovni)
summary(lm(eff_base ~ as.factor(pocet_urovni), data=data)) #R^2==4.2%

# pocet_opakovani_scanu
boxplot(data$eff_base~data$pocet_opakovani_scanu)
summary(lm(eff_base ~ as.numeric(pocet_opakovani_scanu), data=data)) #R^2==52.5%

cor(data$pocet_urovni, data$pocet_opakovani_scanu) # \rho=36% => safe to use both (?)

# delta_VAS
plot(data$eff_base~data$delta_VAS)
lines(lowess(data$delta_VAS, data$eff_base), col="red")
summary(lm(eff_base~delta_VAS, data=data)) # R^2~0

# VAS_pred
plot(data$eff_base~data$VAS_pred)
lines(lowess(data$VAS_pred, data$eff_base), col="red")
summary(lm(eff_base~VAS_pred, data=data)) # R^2=6.3%

# protokol
boxplot(data$eff_base~data$protokol)
summary(data$eff_base[data$protokol=="Low"])
summary(data$eff_base[data$protokol=="Low_HQ"])
summary(data$eff_base[data$protokol=="Std"])
summary(lm(eff_base ~ protokol, data=data, )) #R^2==32.4%

# FBSS - had surgery before, still hurts
boxplot(data$eff_base~data$FBSS)
summary(lm(eff_base~FBSS, data=data)) #R^2~0

# topogram - not expected to be significant
boxplot(data$eff_base~data$topogram)
summary(lm(eff_base~topogram, data=data)) #R^2=0.9%


###
# MODELS
###

M1 = lm(eff_base ~ vek, data = data)
M2 = lm(log(eff_base) ~ vek, data = data)



###
###
# artrosis, modic
###
###

subdata = data %>% filter(data$HUAC_MULTIFIDI != "MISSING")
subdata = subdata %>%
  mutate(psoas_index = psoas_muscle_index*1e4) %>%
  mutate(artroza_L4_stupen_R1 = as.factor(artroza_L4_stupen_R1)) %>%
  mutate(artroza_L5_stupen_R1 = as.factor(artroza_L5_stupen_R1)) %>%
  filter(artroza_L4_stupen_R1 != -10000) %>%
  filter(artroza_L5_stupen_R1 != -10000) %>%
  mutate(HUAC_psoas = as.numeric(HUAC_psoas)) %>%
  filter(HUAC_psoas > 0) %>% # cut three outliers
  mutate(HUAC_MULTIFIDI = as.numeric(HUAC_MULTIFIDI))

L4 = subdata %>% filter(L4_osetrena == "ano")
L5 = subdata %>% filter(L5_osetrena == "ano")

# Does lower psoas index => higher pain pre-procedure?
SM1 = lm(VAS_pred~psoas_index, data=subdata)

ggplot(subdata, aes(x = psoas_index, y = VAS_pred, color = pohlavi)) +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = c("F" = "hotpink", "M" = "dodgerblue2")) +
  labs(x = "Psoas Index x 10,000", y = "VAS Predicted", color = "Gender") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() # probably does

# Is there a relation of psoas_index and eff_dose?
SM2 = lm(log(eff_base)~psoas_index, data=subdata)

ggplot(subdata, aes(x = psoas_index, y = eff_base, color = pohlavi)) +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = c("F" = "hotpink", "M" = "dodgerblue2")) +
  labs(x = "Psoas Index x 10,000", y = "E", color = "Gender") +
  stat_function(
    fun = function(x) exp(SM2$coefficients["(Intercept)"]) * exp(SM2$coefficients["psoas_index"] * x),
    color = "black",
    linetype = 2,
    linewidth = 1
  ) +
  theme_minimal() # also yes

# artrosis level vs psoas index
# L4
boxplot(L4$psoas_muscle_index~L4$artroza_L4_stupen_R1)
anova(lm(psoas_muscle_index~artroza_L4_stupen_R1, data=L4),
      lm(psoas_muscle_index~1, data=L4))
# L5
boxplot(L5$psoas_muscle_index~L5$artroza_L5_stupen_R1)
anova(lm(psoas_muscle_index~artroza_L5_stupen_R1, data=L5),
      lm(psoas_muscle_index~1, data=L5))



#
# Does lower HUAC_psoas => higher pain pre-procedure?
#
SM3 = lm(VAS_pred~HUAC_psoas, data=subdata)

ggplot(subdata, aes(x = HUAC_psoas, y = VAS_pred, color = pohlavi)) +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = c("F" = "hotpink", "M" = "dodgerblue2")) +
  labs(x = "HUAC psoas", y = "VAS Predicted", color = "Gender") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() # probably does

# Is there a relation of HUAC_psoas and eff_dose?
SM4 = lm(log(eff_base)~HUAC_psoas, data=subdata)

ggplot(subdata, aes(x = HUAC_psoas, y = eff_base, color = pohlavi)) +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = c("F" = "hotpink", "M" = "dodgerblue2")) +
  labs(x = "HUAC psoas", y = "E", color = "Gender") +
  theme_minimal() # also yes

# artrosis level vs HUAC_psoas
# L4
boxplot(L4$HUAC_psoas~L4$artroza_L4_stupen_R1)
anova(lm(HUAC_psoas~artroza_L4_stupen_R1, data=L4),
      lm(HUAC_psoas~1, data=L4))
# L5
boxplot(L5$HUAC_psoas~L5$artroza_L5_stupen_R1)
anova(lm(HUAC_psoas~artroza_L5_stupen_R1, data=L5),
      lm(HUAC_psoas~1, data=L5))


#
# Does lower HUAC_MULTIFIDI => higher pain pre-procedure?
#
SM5 = lm(VAS_pred~HUAC_MULTIFIDI, data=subdata)

ggplot(subdata, aes(x = HUAC_MULTIFIDI, y = VAS_pred, color = pohlavi)) +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = c("F" = "hotpink", "M" = "dodgerblue2")) +
  labs(x = "HUAC psoas", y = "VAS Predicted", color = "Gender") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() # probably does

# Is there a relation of HUAC_MULTIFIDI and eff_dose?
SM6 = lm(log(eff_base)~HUAC_MULTIFIDI, data=subdata)

ggplot(subdata, aes(x = HUAC_MULTIFIDI, y = eff_base, color = pohlavi)) +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = c("F" = "hotpink", "M" = "dodgerblue2")) +
  labs(x = "HUAC psoas", y = "E", color = "Gender") +
  theme_minimal() # also yes

# artrosis level vs HUAC_MULTIFIDI
# L4
boxplot(L4$HUAC_MULTIFIDI~L4$artroza_L4_stupen_R1)
anova(lm(HUAC_MULTIFIDI~artroza_L4_stupen_R1, data=L4),
      lm(HUAC_MULTIFIDI~1, data=L4))
# L5
boxplot(L5$HUAC_MULTIFIDI~L5$artroza_L5_stupen_R1)
subdata = subdata %>% mutate(artroza_L5_stupen_R1 = relevel(artroza_L5_stupen_R1, ref="3"))
anova(lm(HUAC_MULTIFIDI~artroza_L5_stupen_R1, data=L5),
      lm(HUAC_MULTIFIDI~1, data=L5))
summary(lm(HUAC_MULTIFIDI~artroza_L5_stupen_R1, data=L5))


#
# artrosis level vs eff_base
#
SM7 = lm(eff_base~artroza_L4_stupen_R1, data=L4)
anova(SM7, lm(eff_base~1, data=L4))
SM8 = lm(eff_base~artroza_L5_stupen_R1, data=L5)
anova(SM8, lm(eff_base~1, data=L5))
