################################################################################
### GOODNESS OF FIT FOR DOSE-RESPONSE META-ANALYSIS OF BINARY OUTCOMES
###       Andrea Discacciati, Alessio Crippa, Nicola Orsini
###                       andrea.discacciati@ki.se
###
###         Example 2 - Coffee consumption and risk of stroke
################################################################################

library(dosresmeta)
library(rms)
library(tidyverse)
library(gridExtra)

rm(list=ls())
savefigures <- 0

data("coffee_stroke")

###
### FIXED - LINEAR
###
lin.f <- dosresmeta(formula = logrr ~ dose, type = type, id = id,
                    se = se, cases = cases, n = n,  data = coffee_stroke, method = "fixed")
summary(lin.f)

## GOF
gof.lin.f <- gof(lin.f)
gof.lin.f

## FIGURE
fig3a <- coffee_stroke %>%
  group_by(id) %>%
  mutate(scaled = dose - dose[se == 0]) %>%
  filter(se != 0) %>%
  bind_cols(gof.lin.f$tdata) %>%
  ggplot(aes(scaled, tresiduals)) +
  geom_point(aes(shape = factor(nordic)), size = 5) +
  geom_hline(yintercept = 0, lwd = .25) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Coffee consumption (cups/day)", y = "Decorrelated residuals", 
       title = "A) Model 1") +  guides(shape = FALSE) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


###
### FIXED - SPLINE
###
knots <- quantile(coffee_stroke$dose, c(.25, .5, .75))
spl.f <- dosresmeta(formula = logrr ~ rcs(dose, knots), type = type, id = id,
                    se = se, cases = cases, n = n,  data = coffee_stroke, method = "fixed")
summary(spl.f)

## GOODNESS OF FIT
gof(spl.f)

###
### FIXED - SPLINE + INTERACTION
###
spl.int.f <- dosresmeta(formula = logrr ~ rcs(dose, knots), type = type, id = id, mod = ~ nordic,
                    se = se, cases = cases, n = n,  data = coffee_stroke, method = "fixed")
summary(spl.int.f)

## GOODNESS OF FIT
gof.spl.int.f <- gof(spl.int.f)
gof.spl.int.f

pchisq(sum(gof.lin.f$tdata$tresiduals^2) - sum(gof.spl.int.f$tdata$tresiduals^2), 
       gof.lin.f$deviance$df - gof.spl.int.f$deviance$df, lower.tail = F)


## FIGURE
fig3b <- coffee_stroke %>%
  group_by(id) %>%
  mutate(scaled = dose - dose[se == 0]) %>%
  filter(se != 0) %>%
  bind_cols(gof.spl.int.f$tdata) %>%
  ggplot(aes(scaled, tresiduals)) +
  geom_point(aes(shape = factor(nordic)), size = 5) +
  geom_hline(yintercept = 0, lwd = .25) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Coffee consumption (cups/day)", y = "Decorrelated residuals", 
       title = "B) Model 3") +  guides(shape = FALSE) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


fig3 <- grid.arrange(fig3a, fig3b, nrow = 2)
fig3
if (savefigures == 1) ggsave("figure3.pdf", fig3, width = 7, height = 9)

## PREDICTION and FIGURE
fig4 <- expand.grid(dose = seq(0, 10, .25), nordic = c(0, 1)) %>%
  bind_cols(predict(spl.int.f, ., expo = T)) %>%
  ggplot(aes(dose, pred, linetype = factor(nordic))) +
  geom_line(cex = 1.5) + geom_hline(yintercept = 1, lty = "dotted") +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_y_continuous(trans = "log", breaks = seq(.6, 1.1, .1), limits = c(.6, 1.1)) +
  labs(x = "Coffee consumption (cups/day)", y = "Relative Risk") +  
  guides(linetype = FALSE) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
fig4

if (savefigures == 1) ggsave("figure4.pdf", fig4, width = 7, height = 5)

# sessionInfo()
# R version 3.4.1 (2017-06-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Sierra 10.12.6
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] gridExtra_2.2.1  bindrcpp_0.2     dplyr_0.7.2      purrr_0.2.2.2    readr_1.1.1     
# [6] tidyr_0.6.3      tibble_1.3.3     tidyverse_1.1.1  rms_5.1-1        SparseM_1.77    
# [11] Hmisc_4.0-3      ggplot2_2.2.1    Formula_1.2-2    survival_2.41-3  lattice_0.20-35 
# [16] dosresmeta_2.0.0 mvmeta_0.4.7