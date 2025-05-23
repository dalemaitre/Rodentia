# ===========================================================================================
# Title: Rodentia - Analysis of the volumes of the inner and middle ear
# Date: 2025-05-23
# -------------------------------------------------------------------------------------------
# R version: 4.4.2
# Required extension: MASS
# -------------------------------------------------------------------------------------------
# Aims: Analyze the bivariate association between ear volumes and skull length (expressed as log), depending on the locomotion repertoire
# Input (from the script "01_RodentiaRawData.R"): 
# - data set for the analyses: variables standardized by skull length
# - graphical parameters: color and symbols
# Output: bivariate plots of log volume on log skull length
# ===========================================================================================

# ---- Source data ----

# Source data
source("01_RodentiaRawData.R")

# ---- Bivariate associations: inner ear vs. skull length ----

par(las = 1)

# Inner ear
plot(log(V_IE)~log(SL), data = data.rod, 
     main = "Inner ear", 
     col = col.loco, bg = col.loco, pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_IE), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)

# Semicircular canals
plot(log(V_SCC)~log(SL), data = data.rod, 
     main = "Semicircular canals", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_SCC), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)

# Cochlea
plot(log(V_CO)~log(SL), data = data.rod, 
     main = "Cochlea", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_CO), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)

# ---- Bivariate associations: middle ear vs. skull length ----

par(las = 1)

# Stapes
plot(log(V_st)~log(SL), data = data.rod, 
     main = "Stapes", 
     col = col.loco, bg = col.loco, pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_st), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)

# Malleus + incus
plot(log(V_ma+V_in)~log(SL), data = data.rod, 
     main = "Malleus + Incus", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_ma+data.rod$V_in), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)

# Incus
plot(log(V_in)~log(SL), data = data.rod, 
     main = "Incus", 
     col = col.loco, bg = col.loco, pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_in), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)

# Malleus
plot(log(V_ma)~log(SL), data = data.rod, 
     main = "Malleus", 
     col = col.loco, bg = col.loco,  pch = pch.clade)
text(log(data.rod$SL), log(data.rod$V_ma), 
     labels = data.rod$Abbr, col = col.loco, pos = 4, cex = .7)



