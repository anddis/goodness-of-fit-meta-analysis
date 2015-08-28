################################################################################
### GOODNESS OF FIT FOR DOSE-RESPONSE META-ANALYSIS OF BINARY OUTCOMES
###       Andrea Discacciati, Alessio Crippa, Nicola Orsini
###                       andrea.discacciati@ki.se
###
###         Example 2 - Coffee consumption and risk of stroke
################################################################################

require("dosresmeta")
require("rms")

rm(list=ls())
savefigures <- 0

source("functions.R")

coffee <- read.table("data/stroke.txt")
coffee$scaled <- unlist(lapply(split(coffee, coffee$id), function(x) x[, "dose"] - x[1, "dose"]))
coffee_nonref <- subset(coffee, se != 0)

###
### FIXED - LINEAR
###
lin.f <- dosresmeta(formula = logrr ~ dose, type = study, id = id,
                    se = se, cases = case, n = n,  data = coffee, method = "fixed")
summary(lin.f)

## GOF
tdata.lin.f <- as.data.frame(gof(lin.f)$tdata)
colnames(tdata.lin.f) <- c("tlogrr", "tdose")
lm.lin.f <- lm(tlogrr ~ -1 + tdose, data = tdata.lin.f)
summary.dr(lm.lin.f)
sum(lm.lin.f$residuals^2)
lm.lin.f$df
pchisq(sum(lm.lin.f$residuals^2), lm.lin.f$df, lower.tail = F)

## FIGURE
if(savefigures == 1) pdf(file = "figure3.pdf", width = 7, height = 9)
par(mfrow=c(2,1))
plot(coffee_nonref$scaled, lm.lin.f$residuals, main = "Model 1", ylim = c(-3,5), xlim=c(0,10), bty = "l",
     ylab = "Decorrelated residuals", xlab = "Coffee consumption (cups/day)", 
     pch = (coffee_nonref$nordic == 0) + (coffee_nonref$nordic == 1)*16, las=1, cex = 2)
title("A)", adj = 0)
abline(a=0, b=0)
lines(lowess(coffee_nonref$scaled, lm.lin.f$residuals), lwd = 4)

###
### FIXED - SPLINE
###
knots <- quantile(coffee$dose, c(.25, .5, .75))
spl.f <- dosresmeta(formula = logrr ~ rcs(dose, knots), type = study, id = id,
                    se = se, cases = case, n = n,  data = coffee, method = "fixed")
summary(spl.f)

## GOODNESS OF FIT
tdata.spl.f <- as.data.frame(gof(spl.f)$tdata)
colnames(tdata.spl.f) <- c("tlogrr", "tdose", "tdose2")
lm.spl.f <- lm(tlogrr ~ -1 + tdose + tdose2, data = tdata.spl.f)
summary.dr(lm.spl.f)
sum(lm.spl.f$residuals^2)
lm.spl.f$df
pchisq(sum(lm.spl.f$residuals^2), lm.spl.f$df, lower.tail = F)

pchisq(sum(lm.lin.f$residuals^2)-sum(lm.spl.f$residuals^2), lm.lin.f$df-lm.spl.f$df, lower.tail = F)

###
### FIXED - SPLINE + INTERACTION
###
spl.int.f <- dosresmeta(formula = logrr ~ rcs(dose, knots), type = study, id = id, mod = ~ nordic,
                    se = se, cases = case, n = n,  data = coffee, method = "fixed")
summary(spl.int.f)

## GOODNESS OF FIT
tdata.spl.int.f <- as.data.frame(gof(spl.int.f)$tdata)
colnames(tdata.spl.int.f) <- c("tlogrr", "tdose", "tdose2", "tdoseXnordic", "tdose2Xnordic")
lm.spl.int.f <- lm(tlogrr ~ -1 + tdose + tdose2 + tdoseXnordic + tdose2Xnordic, data = tdata.spl.int.f)
summary.dr(lm.spl.int.f)
sum(lm.spl.int.f$residuals^2)
lm.spl.int.f$df
pchisq(sum(lm.spl.int.f$residuals^2), lm.spl.int.f$df, lower.tail = F)

## FIGURE
plot(coffee_nonref$scaled, lm.spl.int.f$residuals, main = "Model 3", ylim = c(-3,5), xlim=c(0,10), bty = "l",
     ylab = "Decorrelated residuals", xlab = "Coffee consumption (cups/day)", 
     pch = (coffee_nonref$nordic == 0) + (coffee_nonref$nordic == 1)*16, las=1, cex = 2)
title("B)", adj = 0)
abline(a=0, b=0)
lines(lowess(coffee_nonref$scaled[coffee_nonref$nordic == 0], 
             lm.spl.int.f$residuals[coffee_nonref$nordic == 0]), lwd = 4, lty = 2)
lines(lowess(coffee_nonref$scaled[coffee_nonref$nordic == 1], 
             lm.spl.int.f$residuals[coffee_nonref$nordic == 1]), lwd = 4)
if(savefigures == 1) dev.off()

pchisq(sum(lm.spl.f$residuals^2)-sum(lm.spl.int.f$residuals^2), lm.spl.f$df-lm.spl.int.f$df, lower.tail = F)

## PREDICTION
newdata <- data.frame(expand.grid(dose = seq(0, 10, .25), nordic = c(0, 1)))
pred.spl.int.f <- predict(spl.int.f, newdata, exp = T)

## FIGURE
if(savefigures == 1) pdf(file = "figure4.pdf", width = 7, height = 5)
par(mfrow=c(1,1))
plot(newdata[1:41, 1], pred.spl.int.f[1:41, 5], type = "l", log= "y", ylim= c(0.6,1.1), lwd = 4,
     bty = "l", xlab = "Coffee consumption (cups/day)", ylab = "Relative Risk", las = 1)
lines(newdata[42:82, 1],  pred.spl.int.f[42:82, 5], lwd = 4, lty = 5)
abline(a = 0, b = 0, lty = 2)
if(savefigures== 1) dev.off()

# sessionInfo()
# R version 3.2.0 (2015-04-16)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.10.5 (Yosemite)
# 
# locale:
#   [1] sv_SE.UTF-8/sv_SE.UTF-8/sv_SE.UTF-8/C/sv_SE.UTF-8/sv_SE.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets 
# [7] methods   base     
# 
# other attached packages:
#   [1] curl_0.9.3       rms_4.3-1        SparseM_1.6     
# [4] Hmisc_3.16-0     ggplot2_1.0.1    Formula_1.2-1   
# [7] survival_2.38-1  lattice_0.20-31  dosresmeta_2.0.0
# [10] mvmeta_0.4.7  