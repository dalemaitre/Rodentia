# ===========================================================================================
# Title: Rodentia - Prepare data set and graphical parameters
# Date: 2025-05-23
# -------------------------------------------------------------------------------------------
# R version: 4.4.2
# Required extensions: -
# -------------------------------------------------------------------------------------------
# Aims: Prepare the data set and the graphical parameters for the analyses
# Input: File "Rodentia_IE.csv" with all measurements
# Output: 
# - 2 data sets for the analyses (variables standardized by skull length or not)
# - graphical parameters: color and symbols
# ===========================================================================================

# ---- Prepare the new data set ----

# Load data set 

Rodentia_IE <- read.csv("Rodentia_IE.csv", na.strings = "")

# Separate Linnean name and specimen ID

spec <- strsplit(Rodentia_IE$Taxa, " ")
LinneanName <- rep(NA, length(spec))
SpecID <- rep(NA, length(spec))
for (i in 1:length(spec)) {
  LinneanName[i] <- paste(spec[[i]][1], spec[[i]][2], sep = "_")
  if (length(spec[[i]]) == 3) { SpecID[i] <- spec[[i]][3] }
  if (length(spec[[i]]) == 4) { SpecID[i] <- spec[[i]][4] }
}

# Find variables with missing values

var_miss <- rep(F, ncol(Rodentia_IE))
for (i in 1:length(var_miss)){
  if (anyNA(Rodentia_IE[, i]) == T) { var_miss[i] <- T }
}
colnames(Rodentia_IE)[which(var_miss == T)]

# Find variables with zeros (only secondary common crus: length = 0 when absent)

var_null <- rep(F, ncol(Rodentia_IE))
for (i in 1:length(var_null)){
  if (var_miss[i] == F) {
    for (j in 1:nrow(Rodentia_IE)) {
      if (Rodentia_IE[[i]][j] == 0) { var_null[i] <- T }
    }
  }
}
colnames(Rodentia_IE)[which(var_null == T)]

# New data set

data.rod <- cbind(LinneanName, SpecID, Rodentia_IE)
rownames(data.rod) <- data.rod$Taxa

# Categorical variables

# Locomotion
data.rod$Loco <- factor(data.rod$Loco)
table(data.rod$Loco)
# Clade
data.rod$Clade <- factor(data.rod$Clade)
table(data.rod$Clade)


# ---- Create a data set with standardized variables ----

# Standardization:  
#  * divide all linear dimensions (height, width, diameter of each canal + cochlea length) by the skull length SL  
# * divide all volumes by SL^3  

# Find variables to standardize

# Selection of variables: semicircular canals
asc <- grep("ASC", colnames(data.rod))  # ASC measurements
psc <- grep("PSC", colnames(data.rod))  # PSC measurements
lsc <- grep("LSC", colnames(data.rod))  # LSC measurements
# Selection of variables: primary and secondary common crus
cc <- which(colnames(data.rod) %in% c("LCc", "LsCc"))
# Selection of variables: cochlea length
co_l <- which(colnames(data.rod) == "CO_l")
# Selection of variables: volumes
vol <- grep("V", colnames(data.rod))


# New data set with standardized variables

# Create new data set
data.rod.sl <- data.rod
# Standardize variables
varStd1 <- c(asc, psc, lsc, cc, co_l)
data.rod.sl[, varStd1] <- data.rod.sl[, varStd1] / data.rod.sl$SL  # divide by SL
data.rod.sl[, vol] <- data.rod.sl[, vol] / data.rod.sl$SL^3  # divide by SL^3
# Give new variable names
names(data.rod.sl)[varStd1] <- paste(names(data.rod.sl)[varStd1], "_SL", sep = "")  # linear measurements
names(data.rod.sl)[vol] <- paste(names(data.rod.sl)[vol], "_SL3", sep = "")  # volumes

# ---- Set the graphical parameters ----

# Define fossils for visualization
fos <- which(data.rod$Loco == "unb")

# Locomotion

# Categories
loco <- data.frame(abb = levels(data.rod$Loco), 
                   names = c("arboreal", "fossorial", "generalist", "gliding", "unknown"), 
                   col = c("lightgreen", "orange", "purple", "blue", "black"), 
                   pch = c(24, 25, 21, 22, 23))
# Categories without fossils
loco.noFos <- data.frame(names = c("arboreal", "fossorial", "generalist", "gliding"), 
                         col = c("lightgreen", "orange", "purple", "blue"), 
                         pch = c(24, 25, 21, 22))
# By specimen
col.loco <- rep("black", nrow(data.rod))
pch.loco <- rep(23, nrow(data.rod))
for (i in 1:nrow(data.rod)) {
  loco_i = which(loco$abb == data.rod$Loco[i])
  col.loco[i] <- loco$col[loco_i]
  pch.loco[i] <- loco$pch[loco_i]
}

# Clade

# Categories
clade <- data.frame(abb = levels(data.rod$Clade), 
                    names = c("Anomalurops", "Dermoptera", "Glirid", "Marsupialia",  "Murid", "Squirrel", "Tupaia"), 
                    col = c("red", "turquoise", "pink", "gold", "brown", "orange", "green"), 
                    pch = c(3, 4, 24, 1, 2, 25, 5))
# By specimen
col.clade <- rep("black", nrow(data.rod))
pch.clade <- rep(23, nrow(data.rod))
for (i in 1:nrow(data.rod)) {
  clade_i = which(clade$abb == data.rod$Clade[i])
  col.clade[i] <- clade$col[clade_i]
  pch.clade[i] <- clade$pch[clade_i]
}
