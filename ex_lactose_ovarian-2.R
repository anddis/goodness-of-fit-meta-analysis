################################################################################
### GOODNESS OF FIT FOR DOSE-RESPONSE META-ANALYSIS OF BINARY OUTCOMES
###       Andrea Discacciati, Alessio Crippa, Nicola Orsini
###                       andrea.discacciati@ki.se
###
###         Example 1 - Lactose intake and risk of ovarian cancer
################################################################################

library(dosresmeta)
library(rms)
library(tidyverse)
library(gridExtra)

rm(list=ls())
savefigures <- 0

data("milk_ov")
milk_ov$dose <- 10*milk_ov$dose

###
### FIXED - LINEAR
###
lin.f <- dosresmeta(formula = logrr ~ dose, type = type, id = id, 
                    se = se, cases = case, n = n,  data = milk_ov, method = "fixed")
summary(lin.f)
predict(lin.f, delta = 10, expo = TRUE)

## GOODNESS OF FIT
gof.lin.f <- gof(lin.f)
gof.lin.f

## FIGURE
fig2a <- milk_ov %>%
  group_by(id) %>%
  mutate(scaled = dose - dose[se == 0]) %>%
  filter(se != 0) %>%
  bind_cols(gof.lin.f$tdata) %>%
  ggplot(aes(scaled, tresiduals, linetype = type, shape = type)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0, lwd = .25) +
  scale_shape_manual(values = c(1, 16)) + scale_linetype_manual(values = c(2, 1)) +
  geom_smooth(se = F, col = "black", method = 'loess') +
  labs(x = "Lactose intake (grams/day)", y = "Decorrelated residuals", 
       title = "A) Model 1") +  guides(shape = FALSE, linetype = FALSE) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

###
### FIXED - LINEAR + INTERACTION
###
lin.int.f <- dosresmeta(formula = logrr ~ dose, type = type, id = id, mod = ~ type,
                    se = se, cases = case, n = n,  data = milk_ov, method = "fixed")
summary(lin.int.f)

## GOODNESS OF FIT
gof.int.f <- gof(lin.int.f)
gof.int.f

pchisq(sum(gof.lin.f$tdata$tresiduals^2) - sum(gof.int.f$tdata$tresiduals^2), 
       gof.lin.f$deviance$df - gof.int.f$deviance$df, lower.tail = F)

## FIGURE
fig2b <- milk_ov %>%
  group_by(id) %>%
  mutate(scaled = dose - dose[se == 0]) %>%
  filter(se != 0) %>%
  bind_cols(gof.int.f$tdata) %>%
  ggplot(aes(scaled, tresiduals, linetype = type, shape = type)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0, lwd = .25) +
  scale_shape_manual(values = c(1, 16)) + scale_linetype_manual(values = c(2, 1)) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Lactose intake (grams/day)", y = "Decorrelated residuals", 
       title = "B) Model 2") +  guides(shape = FALSE, linetype = FALSE) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


fig2 <- grid.arrange(fig2a, fig2b, nrow = 2)
fig2
if (savefigures == 1) ggsave("figure2.pdf", fig2, width = 7, height = 9)


## PREDICTIONS
expand.grid(dose = c(0, 10), type = c(0, 1)) %>%
              predict(lin.int.f, ., expo = TRUE)

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