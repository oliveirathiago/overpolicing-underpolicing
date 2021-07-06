library(tidyverse)
library(MplusAutomation)
library(lme4)
library(sjstats)
library(MASS)
library(texreg)
library(lavaan)
library(estimatr)
options(scipen=999)

######################################################
# Cleaning data

load('data/dfwide_new.Rdata')
load('data/dflong_new.Rdata')

dfwide <- dfwide %>%
  mutate(idwide = as.numeric(row.names(dfwide)),
         vio_1 = vio1_1 + vio2_1 + vio3_1 + vio4_1 + vio5_1 + vio6_1,
         vio_2 = vio1_2 + vio2_2 + vio3_2 + vio4_2 + vio5_2 + vio6_2,
         vio_3 = vio1_3 + vio2_3 + vio3_3 + vio4_3 + vio5_3 + vio6_3,
         gunall_1 = gun_1 == T,
         gunall_2 = gun_2 == T,
         gunall_3 = gun_3 == T,
         area1 = area == 1,
         area2 = area == 2,
         area3 = area == 3,
         area4 = area == 4,
         area5 = area == 5,
         area6 = area == 6,
         area7 = area == 7,
         area8 = area == 8,
         class = as.numeric(class)) %>%
  mutate_at(vars(toplaw_1, toplaw_2, toplaw_3, toppol_1, toppol_2, toppol_3), as.factor) %>%
  replace_na(list(gunall_1 = 0,
                  gunall_2 = 0,
                  gunall_3 = 0,
                  phyhar_1 = 0,
                  phyhar_2 = 0,
                  phyhar_3 = 0,
                  verhar_1 = 0,
                  verhar_2 = 0,
                  verhar_3 = 0)) %>%
  mutate_at(vars(law1_1, law1_2, law1_3, djpol_1, djpol_2, djpol_3), list(~case_when(
    . == '5' ~ 1, 
    . == '4' ~ 2,
    . == '3' ~ 3,
    . == '2' ~ 4,
    . == '1' ~ 5,
    TRUE ~ as.numeric(NA))))

dfwide$toppol_1 <- factor(dfwide$toppol_1, levels = c("Consent", "Protest", "Rejection", "Coercion"))
dfwide$toppol_2 <- factor(dfwide$toppol_2, levels = c("Consent", "Protest", "Rejection", "Coercion"))
dfwide$toppol_3 <- factor(dfwide$toppol_3, levels = c("Consent", "Protest", "Rejection", "Coercion"))
dflong$toppol   <- factor(dflong$toppol,   levels = c("Consent", "Protest", "Rejection", "Coercion"))


prepareMplusData(dfwide, filename = 'data/mplusdata.dat',
                 keepCols = c('idwide',
                              'und1_1', 'und2_1', 'und3_1', 'law1_1',
                              'und1_2', 'und2_2', 'und3_2', 'law1_2',
                              'und1_3', 'und2_3', 'und3_3', 'law1_3',
                              'ovp1_1', 'ovp2_1',
                              'ovp1_2', 'ovp2_2',
                              'ovp1_3', 'ovp2_3',
                              'gunall_1', 'gunall_2', 'gunall_3',
                              'area1', 'area2', 'area3', 'area4', 'area5', 'area6', 'area7', 'area8',
                              'white', 'male', 'class',
                              'law3_1', 'law5_1',
                              'law3_2', 'law5_2',
                              'law3_3', 'law5_3',
                              'napol1_1', 'napol2_1',
                              'napol1_2', 'napol2_2',
                              'napol1_3', 'napol2_3',
                              'lawcons_1', 'lawcons_2', 'lawcons_3',
                              'toppol_1', 'toppol_2', 'toppol_3',
                              'polcons_1', 'polcons_2', 'polcons_3',
                              'pj1_1', 'pj2_1', 'pj3_1', 'pj4_1',
                              'pj1_2', 'pj2_2', 'pj3_2', 'pj4_2',
                              'pj1_3', 'pj2_3', 'pj3_3', 'pj4_3',
                              'eff1_1', 'eff2_1', 'eff3_1', 'eff4_1', 'eff5_1', 'eff6_1',
                              'eff1_2', 'eff2_2', 'eff3_2', 'eff4_2', 'eff5_2', 'eff6_2',
                              'eff1_3', 'eff2_3', 'eff3_3', 'eff4_3', 'eff5_3', 'eff6_3'))


und <- read.table('data/scores/undscores.txt') %>%
  rename('idwide' = 'V13',
         'und_1' = 'V14',
         'und_2' = 'V16',
         'und_3' = 'V18')

ovp <- read.table('data/scores/ovpscores.txt') %>%
  rename('idwide' = 'V7',
         'ovp_1' = 'V8',
         'ovp_2' = 'V10',
         'ovp_3' = 'V12')

pj <- read.table('data/scores/pjscores.txt') %>%
  rename('idwide' = 'V13',
         'pj_1' = 'V14',
         'pj_2' = 'V16',
         'pj_3' = 'V18')

eff <- read.table('data/scores/effscores.txt') %>%
  rename('idwide' = 'V19',
         'eff_1' = 'V20',
         'eff_2' = 'V22',
         'eff_3' = 'V24')

dfwide <- dfwide %>%
  left_join(pj[, c('idwide', 'pj_1', 'pj_2', 'pj_3')], by = 'idwide') %>%
  left_join(eff[, c('idwide', 'eff_1', 'eff_2', 'eff_3')], by = 'idwide') %>%
  left_join(ovp[, c('idwide', 'ovp_1', 'ovp_2', 'ovp_3')], by = 'idwide') %>%
  left_join(und[, c('idwide', 'und_1', 'und_2', 'und_3')], by = 'idwide')

dflong <- dfwide %>%
  dplyr::select(-idwide, -area1, -area2, -area3, -area4, -area5, -area6, -area7, -area8) %>%
  reshape(timevar = 'wave',
          idvar = 'quest',
          direction = 'long',
          sep = '_',
          varying = c(6:164)) %>%
  filter(!is.na(panelid)) %>%
  arrange(wave) %>%
  mutate_at(vars(area), as.factor) %>%
  mutate(area = relevel(area, ref = 2))


dflong <- mutate(dflong, newid = as.numeric(row.names(dflong)))
prepareMplusData(dflong, filename = 'data/continuum.dat',
                 keepCols = c('newid',
                              'napol1', 'napol2', 
                              'fearpolgen', 'fearpolper',
                              'toppol'))

continuum_long <- read.table('data/continuum_long.txt') %>%
  rename('newid' = 'V4',
         'legitimacy' = 'V5')

dflong <- dflong %>%
  left_join(continuum_long[, c('newid', 'legitimacy')], by = 'newid')
##################################################

# Table 1
dfwide %>%
  group_by(area) %>%
  summarise(#age = mean(age_1, na.rm = T),
    #male = mean(male, na.rm = T),
    white = mean(white, na.rm = T),
    class = mean(class, na.rm = T),
    ovp = mean(ovp_1, na.rm = T),
    und = mean(und_1, na.rm = T),
    stop = mean(stop_1, na.rm = T),
    gun = mean(gun_1, na.rm = T))


# Correlation between similar survey items
#cor_ovpnapol <- cor(dfwide[, c("ovp1_1", "napol2_1")], use = "complete.obs")

# Correlation matrices between latent constructs
cor_constucts_1 <- cor(dflong[dflong$wave == 1, c('ovp', 'und', 'pj', 'eff', 'legitimacy')], use = 'complete.obs')
cor_constucts_2 <- cor(dflong[dflong$wave == 2, c('ovp', 'und', 'pj', 'eff', 'legitimacy')], use = 'complete.obs')
cor_constucts_3 <- cor(dflong[dflong$wave == 3, c('ovp', 'und', 'pj', 'eff', 'legitimacy')], use = 'complete.obs')

# Calculating 95% CIs for CLPM (Table 2):
panel_model <- data.frame(path = c('ovp-ovp', 'ovp-und', 'ovp-gunT1', 'ovp-gunT2', 'ovp-gunT3',
                                   'ovp-area2', 'ovp-area3', 'ovp-area4', 'ovp-area5', 'ovp-area6', 
                                   'ovp-area7', 'ovp-area8', 'ovp-race', 'ovp-gender', 'ovp-class',
                                   'und-ovp', 'und-und', 'und-gunT1', 'und-gunT2', 'und-gunT3',
                                   'und-area2', 'und-area3', 'und-area4', 'und-area5','und-area6', 
                                   'und-area7', 'und-area8', 'und-race', 'und-gender', 'und-class'),
                          coef = c(0.600, 0.205, 0.869, 0.511, 0.535, -0.134, -0.060, 0.274, -0.445, 
                                   -0.415, -0.478, -0.340, -0.205, -0.138, 0.103,
                                   0.187, 0.767, 0.759, 0.020, 0.532, -0.157, -0.166, -0.436, -0.421,
                                   -0.864, -1.096, -0.717, 0.169, -0.471, -0.168),
                          se = c(0.029, 0.029, 0.139, 0.194, 0.209, 0.159, 0.159, 0.169, 0.161, 0.170, 
                                 0.184, 0.203, 0.088, 0.094, 0.034,
                                 0.032, 0.027, 0.133, 0.188, 0.208, 0.156, 0.165, 0.167, 0.167, 0.175, 
                                 0.196, 0.205, 0.088, 0.098, 0.038)) %>%
  mutate(lowerbound = round(coef - 1.96 * se, 2),
         upperbound = round(coef + 1.96 * se, 2)) %>%
  print()

#####
# Checking random intercepts and random coefficients models

ranint <- lmer(legitimacy ~ wave + (1 | quest), dflong, REML = F)
rancoe <- lmer(legitimacy ~ wave + (wave | quest), dflong, REML = F)
screenreg(list(ranint, rancoe))

#####

# Table 3
hlm1c <- lmer(legitimacy ~ wave + ovp + (wave | quest), dflong, REML = F)
hlm2c <- lmer(legitimacy ~ wave + und + (wave | quest), dflong, REML = F)
hlm3c <- lmer(legitimacy ~ wave + ovp + und + (wave | quest), dflong, REML = F)
hlm4c <- lmer(legitimacy ~ wave + ovp + und + pj + eff + (wave | quest), dflong, REML = F)
hlm5c <- lmer(legitimacy ~ wave + ovp + und + pj + eff + white + male + class + factor(area) + 
                (wave | quest), dflong, REML = F)

screenreg(list(hlm1c, hlm2c, hlm3c, hlm4c, hlm5c), ci.force = T)

hlm1c <- lmer(legitimacy ~ wave + ovp + (1 | quest), dflong, REML = F)
hlm2c <- lmer(legitimacy ~ wave + und + (1 | quest), dflong, REML = F)
hlm3c <- lmer(legitimacy ~ wave + ovp + und + (1 | quest), dflong, REML = F)
hlm4c <- lmer(legitimacy ~ wave + ovp + und + pj + eff + (1 | quest), dflong, REML = F)
hlm5c <- lmer(legitimacy ~ wave + ovp + und + pj + eff + white + male + class + factor(area) + 
                (1 | quest), dflong, REML = F)

screenreg(list(hlm1c, hlm2c, hlm3c, hlm4c, hlm5c), ci.force = T)

# (with fixed effects)
#fixef <- lm(continuum ~ ovp + und + factor(wave) + factor(quest), dflong)
#fixef_pj <- lm(continuum ~ ovp + und + pj + eff + factor(wave) + factor(quest), dflong)
#screenreg(list(fixef, fixef_pj), omit.coef = 'quest|wave')


#####
# Checking poisson and nb and random intercepts and coefficients
nb_intercept <- glmer.nb(vio ~ wave + (1 | quest), dflong)
nb_coef <- glmer.nb(vio ~ wave + (wave | quest), dflong)

screenreg(list(nb_intercept, nb_coef))

var(dflong$vio) > mean(dflong$vio)
#####

# Table 4
nb.m1c <- glmer.nb(vio ~ legitimacy + wave + (1 | quest), dflong)
nb.m2c <- glmer.nb(vio ~ legitimacy + wave +  white + male + class + factor(area) + (1 | quest), dflong)
nb.m3c <- glmer.nb(vio ~ legitimacy * ovp + legitimacy * und + wave +  white + male + class + factor(area) + (1 | quest),
                   dflong)

#poisson.m1c <- glmer(vio ~ legitimacy + wave + (1 | quest), dflong, family = poisson(link = 'log'), nAGQ = 100)
#poisson.m2c <- glmer(vio ~ legitimacy + wave +  white + male + class + factor(area) + (1 | quest), dflong, 
#                     family = poisson(link = 'log'), nAGQ = 100)

screenreg(list(nb.m1c, nb.m2c, nb.m3c), ci.force = T)
#screenreg(list(poisson.m1c, poisson.m2c), ci.force = T)

# Longitudinal mediation

mediation_data <- dflong %>%
  dplyr::select(quest, wave, ovp, und, legitimacy, vio) %>%
  reshape(timevar = 'wave',
          idvar = 'quest',
          direction = 'wide',
          sep = '_')

prepareMplusData(mediation_data, filename = 'data/mediation_data.dat')

# Calculating 95% CIs for the mediation model (Table 5):
mediation_model <- data.frame(path = c('A1', 'A2', 'B', 'C', 'D1', 'D2', 'E', 'F1', 'F2', 'D1*E', 'D2*E'),
                              coef = c(0.795, 0.986, 0.239, 0.183, -0.119, -0.213, -0.059, 0.001, 0.014, 0.007, 0.013),
                              se = c(0.015, 0.004, 0.028, 0.022, 0.018, 0.017, 0.029, 0.031, 0.030, 0.004, 0.006)) %>%
  mutate(lowerbound = round(coef - 1.96 * se, 2),
         upperbound = round(coef + 1.96 * se, 2)) %>%
  print()

