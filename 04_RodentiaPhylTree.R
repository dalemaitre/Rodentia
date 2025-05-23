# ===========================================================================================
# Title: Rodentia - Computation of the consensus tree
# Date: 2025-05-23
# -------------------------------------------------------------------------------------------
# R version: 4.4.2
# Required extensions: ape, geomorph, phytools
# -------------------------------------------------------------------------------------------
# Aims: Compute the consensus tree
# Input: 
# - data set for the analyses: variables standardized by skull length (from the script "01_RodentiaRawData.R")
# - 1000 phylogenetic trees (downloaded from <https://vertlife.org/phylosubsets/> on 2025-01-20)
# Output: 
# - Consensus tree for the analysis
# - Phylogenetic signal for the different sets of variables
# ===========================================================================================

# ---- Load packages and source data ----

# Packages
library(ape)
library(geomorph)
library(phytools)

# Source data
source("01_RodentiaRawData.R")

# ---- 1. Prepare the tree and the data set ----

# Note: It is possible to directly begin at point 1.2 if the file RodentiaTree.nex is already available

# ---- 1.1 Compute the consensus tree ----

# Load 1000 initial trees
rod.tree.all <- read.nexus("Rodentia_1000.nex")

# Compute the consensus tree

# majority rule consensus tree, no consensus edge lengths
rod.tree.noEdgeLength <- consensus(rod.tree.all, p = 0.5, rooted = T)
# majority rule consensus tree, with consensus edge lengths
rod.tree.ini0 <- consensus.edges(rod.tree.all, p = 0.5, rooted = T, method = "mean.edge")  
# Remove extra edge length (problem in the function)
rod.tree.ini <- rod.tree.ini0
rod.tree.ini$edge.length <- rod.tree.ini0$edge.length[-length(rod.tree.ini0$edge.length)]

# Plot consensus trees
plot(rod.tree.noEdgeLength, cex = 0.7)  # no consensus edge lengths
plot(rod.tree.ini0, cex = 0.7)  # with consensus edge lengths, not corrected
plot(rod.tree.ini, cex = 0.7)  # with consensus edge lengths, corrected

# Remove branches and tip names for the extra species (error in the initial download)
rod.tree <- drop.tip(rod.tree.ini, c("Nannosciurus_melanotis", "Xerus_erythropus"))

# Save the tree file
#write.nexus(rod.tree, file = "RodentiaTree.nex")

# ---- 1.2 Prepare the tree with the correct names ----

# Because the exact taxa used here could not always be found in the VertLife database, 
# we choose the closest relatives of the absent taxa in that database.
# Also, the Linnean names in the data base were not always exactly the same as what we used. 
# Therefore, we had to modify some taxa names in the tree branch tips to get an exact correspondence. 

# Load consensus tree (only if already saved)
#rod.tree <- read.nexus("RodentiaTree.nex")

# Plot the tree
plot(rod.tree)

# Identify modified taxa names in the data base
modif <- which(!is.na(Rodentia_IE$phylTree))  # modified taxa names
data.rod$LinneanName[modif]  # real names of taxa
Rodentia_IE$phylTree[modif]  # names used in the phylogenetic tree

# Modify branch tip names in the tree
tips.ini <- rod.tree$tip.label  # tree branch tips
tips <- tips.ini 
for (i in 1:length(tips)) { 
  # Find taxa names to modify in the tree
  if (tips.ini[i] %in% Rodentia_IE$phylTree[modif]) {
    spi <- which(Rodentia_IE$phylTree[modif] == tips.ini[i])[1]
    tips[i] <- data.rod$LinneanName[modif[spi]]
  }
}
#print(cbind(tips.ini, tips))

# Check congruence between taxa names

# extant taxa
unb <- which(data.rod$Loco == "unb")  # fossil taxa
taxa.noFos <- data.rod$LinneanName[-unb]
all.equal(sort(tips), unique(taxa.noFos))
# extant taxa without Eutamias sp.
spi <- which(data.rod$LinneanName == "Eutamias_sp.")
all.equal(sort(tips), unique(data.rod$LinneanName[-c(unb, spi)]))
#print(cbind(sort(tips), unique(data.rod$LinneanName[-c(unb, spi)])))

# Replace tip names in the tree
rod.tree$tip.label <- tips

# ---- 1.3 Prepare data for phylogenetic analysis ----

# New data set: no fossil

# Find fossil
fos <- which(data.rod$Loco == "unb")
# New data set: no fossil
data.rodPhylo <- data.rod[-fos,]
data.rodPhylo.sl <- data.rod.sl[-fos,]

# Find Eutamias sp.
spi <- which(data.rodPhylo$LinneanName == "Eutamias_sp.")
data.rodPhylo$LinneanName[spi] <- "Eutamias_sibiricus"
# Duplicated species names
dupl <- which(duplicated(data.rodPhylo$LinneanName))

# Average duplicated specimens (only when 2 specimens of the same taxon), as well as *Eutamias sp.* and *Eutamias sibiricus*
  
# Variables to average
varAver <- 8:ncol(data.rod)
# Initialization
uni <- dupl
data.aver <- data.rodPhylo[dupl, varAver]
# Compute average values
for (i in 1:length(dupl)) {
  spi <- which(data.rodPhylo$LinneanName == data.rodPhylo$LinneanName[dupl[i]])
  uni[i] <- spi[1]  # 
  data.aver[i, ] <- apply(data.rodPhylo[spi, varAver], 2, mean)  # average
}
# Replace by the mean in the data set for phylogenetic analyses
data.rodPhylo[uni, varAver] <- data.aver
# Remove duplicated
data.rodPhylo <- data.rodPhylo[-dupl, ]
# Give Linnean names for row names
rownames(data.rodPhylo) <- data.rodPhylo$LinneanName

# Check congruence between taxa names
all.equal(rownames(data.rodPhylo), sort(rod.tree$tip.label))
#print(cbind(rownames(data.rodPhylo), sort(rod.tree$tip.label)))

# ---- 2. Phylogenetic signal ----

# Here we use the variables without standardization by skull length

# ---- 2.1 For one variable: Blomberg's K ----

# Skull length
sl <- setNames(data.rodPhylo$SL, rownames(data.rodPhylo))
phylosig(rod.tree, sl, method = "K", test = T)

# ---- 2.2 For several variables: Kmult (Adams 2014) ----

# Here we exclude skull length for the computations of the phylogenetic signal

# ---- 2.2.1 Measurements on the inner ear (no volume) ----

# Prepare data set (not standardized)
vari <- which(names(data.rodPhylo) == "SL")  # find variable SL
ROD <- as.matrix(data.rodPhylo[, (vari+1):ncol(data.rodPhylo)])  # select only numerical variables, without SL
ROD.std <- apply(ROD, 2, scale)  # center and scale
rownames(ROD.std) <- rownames(ROD)

# Select volumes
varv <- grep("V_", colnames(ROD.std))
physignal(ROD.std[, -varv], phy = rod.tree)

# ---- 2.2.2 Measurements including volumes ----

# Because the volume is missing for some specimens, we remove them

# Specimens with missing volumes
sp_miss <- rep(F, nrow(ROD))
for (i in 1:length(sp_miss)){
  if (anyNA(ROD[i, ]) == T) { sp_miss[i] <- T }
}
spi <- which(sp_miss == T)
rownames(ROD)[spi]

# Remove species from tree: 
rod.tree.forVol <- drop.tip(rod.tree, rownames(ROD)[spi])

# Phylogenetic signal for volumes (IE excluded because redundant with SCC and CO)
varv.noIE <- which(colnames(ROD.std) %in% c("V_SCC", "V_CO", "V_ma", "V_in", "V_st"))
physignal(ROD.std[-spi, varv.noIE], phy = rod.tree.forVol)

# Phylogenetic signal for all variables except volumes and skull length: 
varIE <- which(colnames(ROD.std) == "V_IE")
physignal(ROD.std[-spi, -varIE], phy = rod.tree.forVol)
