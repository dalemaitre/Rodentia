# ===========================================================================================
# Title: Rodentia - Analysis of the volumes of the inner and middle ear
# Date: 2025-12-24
# -------------------------------------------------------------------------------------------
# R version: 4.4.2
# Required extension: MASS
# -------------------------------------------------------------------------------------------
# Aims: Analyze the bivariate association between ear volumes and skull length (expressed as log), depending on the locomotion repertoire
# Input (from the script "01_RodentiaRawData.R"): 
# - data set for the analyses: variables standardized by skull length
# - graphical parameters: color and symbols
# Output:
# - OLS regression of log volume on log skull length, 
# with or without ecotype as a covariate
# - bivariate plots of log volume on log skull length
# ===========================================================================================

# ---- Source data ----

# Source data
source("01_RodentiaRawData.R")

# New subset with three ecotypes only (arboreal, fossorial, gliding)
data.rod.loco <- subset(data.rod, Loco %in% c("arb", "fos", "gli"))
data.rod.loco$Loco <- factor(data.rod.loco$Loco)

# ---- 1. Bivariate associations: inner ear vs. skull length ----

# ---- 1.1 Regression of volumes on skull length ----

# Inner ear
data.rod.ie.mod1 <- lm(log(V_IE)~log(SL), data = data.rod)
summary(data.rod.ie.mod1)

# Vestibular part
data.rod.scc.mod1 <- lm(log(V_SCC)~log(SL), data = data.rod)
summary(data.rod.scc.mod1)

# Cochlea
data.rod.co.mod1 <- lm(log(V_CO)~log(SL), data = data.rod)
summary(data.rod.co.mod1)

# ---- 1.2 Regression of volumes on skull length with effect of ecotype ----

# Inner ear
data.rod.ie.mod2 <- lm(log(V_IE)~log(SL)+Loco, data = data.rod.loco)
summary(data.rod.ie.mod2)

# Vestibular part
data.rod.scc.mod2 <- lm(log(V_SCC)~log(SL)+Loco, data = data.rod.loco)
summary(data.rod.scc.mod2)

# Cochlea
data.rod.co.mod2 <- lm(log(V_CO)~log(SL)+Loco, data = data.rod.loco)
summary(data.rod.co.mod2)

# ---- 1.3 Visualization ----

par(las = 1)

# Inner ear
plot(log(V_IE)~log(SL), data = data.rod, 
     main = "Inner ear", 
     col = col.loco, bg = col.loco, pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_IE), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)
lines(data.rod.ie.mod1$fitted.values~data.rod.ie.mod1$model$`log(SL)`, 
      col = "red", lty = 1, lwd = 1)

# Semicircular canals
plot(log(V_SCC)~log(SL), data = data.rod, 
     main = "Vestibular part", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_SCC), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)
lines(data.rod.scc.mod1$fitted.values~data.rod.scc.mod1$model$`log(SL)`, 
      col = "red", lty = 1, lwd = 1)

# Cochlea
plot(log(V_CO)~log(SL), data = data.rod, 
     main = "Cochlea", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_CO), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)
lines(data.rod.co.mod1$fitted.values~data.rod.co.mod1$model$`log(SL)`, 
      col = "red", lty = 1, lwd = 1)

# ---- 2. Bivariate associations: middle ear vs. skull length ----

# ---- 2.1 Regression of volumes on skull length ----

# Stapes
data.rod.st.mod1 <- lm(log(V_st)~log(SL), data = data.rod)
summary(data.rod.st.mod1)

# Incus
data.rod.in.mod1 <- lm(log(V_in)~log(SL), data = data.rod)
summary(data.rod.in.mod1)

# Malleus
data.rod.ma.mod1 <- lm(log(V_ma)~log(SL), data = data.rod)
summary(data.rod.ma.mod1)

# Malleus + incus
data.rod.main.mod1 <- lm(log(V_ma+V_in)~log(SL), data = data.rod)
summary(data.rod.main.mod1)

# ---- 1.2 Regression of volumes on skull length with effect of ecotype ----

# Stapes
data.rod.st.mod2 <- lm(log(V_st)~log(SL)+Loco, data = data.rod.loco)
summary(data.rod.st.mod2)

# Incus
data.rod.in.mod2 <- lm(log(V_in)~log(SL)+Loco, data = data.rod.loco)
summary(data.rod.in.mod2)

# Malleus
data.rod.ma.mod2 <- lm(log(V_ma)~log(SL)+Loco, data = data.rod.loco)
summary(data.rod.ma.mod2)

# Malleus + incus
data.rod.main.mod2 <- lm(log(V_ma+V_in)~log(SL)+Loco, data = data.rod.loco)
summary(data.rod.main.mod2)

# ---- 1.3 Visualization ----

par(las = 1)

# Stapes
plot(log(V_st)~log(SL), data = data.rod, 
     main = "Stapes", 
     col = col.loco, bg = col.loco, pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_st), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)
lines(data.rod.st.mod1$fitted.values~data.rod.st.mod1$model$`log(SL)`, 
      col = "red", lty = 1, lwd = 1)

# Incus
plot(log(V_in)~log(SL), data = data.rod, 
     main = "Incus", 
     col = col.loco, bg = col.loco, pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_in), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)
lines(data.rod.in.mod1$fitted.values~data.rod.in.mod1$model$`log(SL)`, 
      col = "red", lty = 1, lwd = 1)

# Malleus
plot(log(V_ma)~log(SL), data = data.rod, 
     main = "Malleus", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_ma), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)
lines(data.rod.ma.mod1$fitted.values~data.rod.ma.mod1$model$`log(SL)`, 
      col = "red", lty = 1, lwd = 1)

# Malleus + incus
plot(log(V_ma+V_in)~log(SL), data = data.rod, 
     main = "Malleus + Incus", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_ma+data.rod$V_in), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)
lines(data.rod.main.mod1$fitted.values~data.rod.main.mod1$model$`log(SL)`, 
      col = "red", lty = 1, lwd = 1)
