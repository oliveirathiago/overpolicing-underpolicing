library(tidyverse)
library(lavaan)
options(scipen=999)

load('data/dfwide_new.Rdata')

indicators <- c('ovp1_1', 'ovp2_1',
                'und1_1', 'und2_1', 'und3_1', 'law1_1',
                'pj1_1', 'pj2_1', 'pj3_1', 'pj4_1',
                'eff1_1', 'eff2_1', 'eff3_1', 'eff4_1', 'eff5_1', 'eff6_1')

cfa4 <- 'ovp =~ ovp1_1 + ovp2_1
         und =~ und1_1 + und2_1 + und3_1 + law1_1
         pj =~ pj1_1 + pj2_1 + pj3_1 +  pj4_1
         eff =~ eff1_1 + eff2_1 + eff3_1 + eff4_1 + eff5_1 + eff6_1
        '
cfa4.fit <- cfa(cfa4, dfwide,std.lv = T, ordered = indicators)
summary(cfa4.fit, fit.measures = T)


cfa3_ovpund <- 'ovp =~ ovp1_1 + ovp2_1 + und1_1 + und2_1 + und3_1 + law1_1
                pj =~ pj1_1 + pj2_1 + pj3_1 +  pj4_1
                eff =~ eff1_1 + eff2_1 + eff3_1 + eff4_1 + eff5_1 + eff6_1
              '
cfa3_ovpund.fit <- cfa(cfa3_ovpund, dfwide, std.lv = T, ordered = indicators)

cfa3_undeff <- 'ovp =~ ovp1_1 + ovp2_1
                und =~ und1_1 + und2_1 + und3_1 + law1_1 + pj1_1 + pj2_1 + pj3_1 +  pj4_1
                eff =~ eff1_1 + eff2_1 + eff3_1 + eff4_1 + eff5_1 + eff6_1
              '
cfa3_undeff.fit <- cfa(cfa3_undeff, dfwide, std.lv = T, ordered = indicators)

cfa3_ovopj <- 'ovp =~ ovp1_1 + ovp2_1 + pj1_1 + pj2_1 + pj3_1 +  pj4_1
               und =~ und1_1 + und2_1 + und3_1 + law1_1
               eff =~ eff1_1 + eff2_1 + eff3_1 + eff4_1 + eff5_1 + eff6_1
              '
cfa3_ovppj.fit <- cfa(cfa3_ovopj, dfwide, std.lv = T, ordered = indicators)

cfa1 <- 'ovp =~ ovp1_1 + ovp2_1 + und1_1 + und2_1 + und3_1 + law1_1 + pj1_1 + pj2_1 + pj3_1 +  pj4_1 + eff1_1 + eff2_1 + eff3_1 + eff4_1 + eff5_1 + eff6_1 '
cfa1.fit <- cfa(cfa1, dfwide, std.lv = T, ordered = indicators)


# Table A2
cbind(FourFactor = inspect(cfa4.fit, 'fit.measures'), 
      ThreeFactor_OVPUND = inspect(cfa3_ovpund.fit, 'fit.measures'), 
      ThreeFactor_UNDEFF = inspect(cfa3_undeff.fit, 'fit.measures'),
      ThreeFactor_OVPPJ = inspect(cfa3_ovppj.fit, 'fit.measures'),
      OneFactor = inspect(cfa1.fit, 'fit.measures')
)[c('chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper'),]
