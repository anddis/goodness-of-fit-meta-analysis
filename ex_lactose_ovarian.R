################################################################################
### GOODNESS OF FIT FOR DOSE-RESPONSE META-ANALYSIS OF BINARY OUTCOMES
###       Andrea Discacciati, Alessio Crippa, Nicola Orsini
###                       andrea.discacciati@ki.se
###
###         Example 1 - Lactose intake and risk of ovarian cancer
################################################################################

require("dosresmeta")
require("rms")

rm(list=ls())
savefigures <- 0

source("functions.R")

lactose <- read.table("data/ovarian.txt")
lactose$dose <- lactose$dose * 10
lactose$scaled <- unlist(lapply(split(lactose, lactose$id), function(x) x[, "dose"] - x[1, "dose"]))
lactose_nonref <- subset(lactose, se != 0)

###
### FIXED - LINEAR
###
lin.f <- dosresmeta(formula = logrr ~ dose, type = type, id = id, 
                    se = se, cases = case, n = n,  data = lactose, method = "fixed")
summary(lin.f)

## GOODNESS OF FIT
predict(lin.f, delta=10, expo = TRUE)
tdata.lin.f <- as.data.frame(gof(lin.f)$tdata)
colnames(tdata.lin.f) <- c("tlogrr", "tdose")
lm.lin.f <- lm(tlogrr ~ -1 + tdose, data = tdata.lin.f)
summary.dr(lm.lin.f)
sum(lm.lin.f$residuals^2)
lm.lin.f$df
pchisq(sum(lm.lin.f$residuals^2), lm.lin.f$df, lower.tail = F)

## FIGURE
if(savefigures == 1) pdf(file = "figure2.pdf", width = 7, height = 9)
par(mfrow=c(2,1))
plot(lactose_nonref$scaled, lm.lin.f$residuals, main = "Model 1", ylim = c(-3.5, 2.5),
     ylab = "Decorrelated residuals", xlab = "Lactose intake (grams/day)", bty = "l",
     pch = (lactose_nonref$type == "cc") + (lactose_nonref$type == "ir")*16, las=1, cex = 2)
title("A)", adj = 0)
abline(a=0, b=0)
lines(lowess(lactose_nonref$scaled[lactose_nonref$type == "cc"], 
             lm.lin.f$residuals[lactose_nonref$type == "cc"]), lwd = 4, lty = 2)
lines(lowess(lactose_nonref$scaled[lactose_nonref$type == "ir"], 
             lm.lin.f$residuals[lactose_nonref$type == "ir"]), lwd = 4)

###
### FIXED - LINEAR + INTERACTION
###
lin.int.f <- dosresmeta(formula = logrr ~ dose, type = type, id = id, mod = ~ factor(type),
                    se = se, cases = case, n = n,  data = lactose, method = "fixed")
summary(lin.int.f)

## GOODNESS OF FIT
tdata.int.f <- as.data.frame(gof(lin.int.f)$tdata)
colnames(tdata.int.f) <- c("tlogrr", "tdose", "tdoseXtype")
lm.int.f <- lm(tlogrr ~ -1 + tdose + tdoseXtype, data = tdata.int.f)
summary.dr(lm.int.f)
sum(lm.int.f$residuals^2)
lm.int.f$df
pchisq(sum(lm.int.f$residuals^2), lm.int.f$df, lower.tail = F)

pchisq(sum(lm.lin.f$residuals^2)-sum(lm.int.f$residuals^2), lm.lin.f$df-lm.int.f$df, lower.tail = F)

## FIGURE
plot(lactose_nonref$scaled, lm.int.f$residuals, main = "Model 2", ylim = c(-3.5, 2.5),
     ylab = "Decorrelated residuals", xlab = "Lactose intake (grams/day)", bty = "l",
     pch = (lactose_nonref$type == "cc") + (lactose_nonref$type == "ir")*16, las=1, cex = 2)
title("B)", adj = 0)
abline(a=0, b=0)
lines(lowess(lactose_nonref$scaled[lactose_nonref$type == "cc"], 
             lm.int.f$residuals[lactose_nonref$type == "cc"]), lwd = 4, lty = 2)
lines(lowess(lactose_nonref$scaled[lactose_nonref$type == "ir"], 
             lm.int.f$residuals[lactose_nonref$type == "ir"]), lwd = 4)
if(savefigures == 1) dev.off()

## PREDICTIONS
newdata <- data.frame(dose = rep(c(0, 10), 2), type = c(0, 0, 1, 1))
predict(lin.int.f, newdata, expo = TRUE)

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