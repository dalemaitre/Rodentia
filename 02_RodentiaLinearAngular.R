# ===========================================================================================
# Title: Rodentia - Analysis of the linear and angular measurements on the inner ear
# Date: 2025-05-23
# -------------------------------------------------------------------------------------------
# R version: 4.4.2
# Required extension: MASS
# -------------------------------------------------------------------------------------------
# Aims: Analyze the 16 measurements for the inner ear, standardized by skull length
# Input (from the script "01_RodentiaRawData.R"): 
# - data set for the analyses: variables standardized by skull length
# - graphical parameters: color and symbols
# Output: 
# - PCA of the 16 variables
# - linear regression of PC1 on SL (skull length)
# - CVA of the first 3 principal components
# ===========================================================================================

# ---- Packages and source data ----

# Packages
library(MASS)  # statistical analyses

# Source data
source("01_RodentiaRawData.R")

# ---- 1. Principal component analysis ----

# ---- 1.1 Prepare variable matrix ----

# Here we use variables standardized by skull length

# Select all variables except skull length
M1.sl <- as.matrix(data.rod.sl[, 9:ncol(data.rod.sl)])  # create matrix
rownames(M1.sl) <- data.rod.sl$Abbr  # give row names

# Remove variables with missing values (volumes)
v2 <- grep("V_", colnames(M1.sl))  # volumes
M2.sl <- M1.sl[,-v2]  # raw variables, no volumes

# ---- 1.2 Principal component analysis (PCA) ----

# PCA on centered and scales variables
pca.rod.sl <- prcomp(M2.sl, center = T, scale. = T)
summary(pca.rod.sl)

# Variance explained (percentage)
pca.rod.sl$var <- 100*pca.rod.sl$sdev^2 / sum(pca.rod.sl$sdev^2)
# Scree plot
names(pca.rod.sl$var) <- 1:length(pca.rod.sl$var)
barplot(pca.rod.sl$var, col = "darkblue", las = 1, 
        main = "Scree plot of the PCA", 
        xlab = "Dimension", ylab = "% of tot. var.")

# PC scores - PC1 vs. PC2
pc <- c(1, 2)
plot(pca.rod.sl$x[, pc[1]], pca.rod.sl$x[, pc[2]], 
     col = col.loco, bg = col.loco, 
     las = 1, pch = pch.clade, 
     main = "PC1 vs. PC2", 
     xlab = paste("PC", pc[1], sep = ""), 
     ylab = paste("PC", pc[2], sep = ""))
abline(h = 0, lty = "dashed") ; abline(v = 0, lty = "dashed")
text(pca.rod.sl$x[ , pc[1]], pca.rod.sl$x[ , pc[2]], 
     labels = data.rod$Abbr, pos = 4, cex = .7, col = col.loco)
legend("bottomright",  # position of the legend
       title = "Clade", # title of the legend
       legend = clade$names,  # text of the legend
       pch = clade$pch,  # symbols of the legend
       pt.bg = "black", 
       cex = .6, 
       border = F)  # color of the legend

# PC scores - PC2 vs. PC3
pc <- c(2,3)
plot(pca.rod.sl$x[, pc[1]], pca.rod.sl$x[, pc[2]], 
     col = col.loco, bg = col.loco, 
     las = 1, pch = pch.clade, 
     main = "PC2 vs. PC3", 
     xlab = paste("PC", pc[1], sep = ""), 
     ylab = paste("PC", pc[2], sep = ""))
abline(h = 0, lty = "dashed") ; abline(v = 0, lty = "dashed")
text(pca.rod.sl$x[ , pc[1]], pca.rod.sl$x[ , pc[2]], 
     labels = data.rod$Abbr, pos = 4, cex = .7, col = col.loco)
legend("topright",  # position of the legend
       title = "Clade", # title of the legend
       legend = clade$names,  # text of the legend
       pch = clade$pch,  # symbols of the legend
       pt.bg = "black", 
       cex = .6, 
       border = F)  # color of the legend

# PC loadings
pci <- 1
barplot(pca.rod.sl$rotation[, pci], col = "darkblue", las = 2, 
        xlab = "Variables", ylab = "Loadings", cex.names = .7, 
        main = paste("Dimension", pci))
pci <- 2
barplot(pca.rod.sl$rotation[, pci], col = "darkblue", las = 2, 
        xlab = "Variables", ylab = "Loadings", cex.names = .7, 
        main = paste("Dimension", pci))
pci <- 3
barplot(pca.rod.sl$rotation[, pci], col = "darkblue", las = 2, 
        xlab = "Variables", ylab = "Loadings", cex.names = .7, 
        main = paste("Dimension", pci))

# ---- 2. Allometric changes ----

# In this section we test the association between the first principal component and skull length, with or without the effect of clade.  

# ---- 2.1 Model 1: PC1 on SL ----

# Build the data frame
rod.sl.df <- data.frame(clade = data.rod.sl$Clade, 
                        loco = data.rod.sl$Loco, 
                        sl = data.rod.sl$SL, 
                        pc1 = pca.rod.sl$x[,1], 
                        pc2 = pca.rod.sl$x[,2], 
                        pc3 = pca.rod.sl$x[,3], 
                        pc4 = pca.rod.sl$x[,4])
rownames(rod.sl.df) <- rownames(data.rod.sl)

# Linear regression of PC1 on SL
rod.sl.mod1 <- lm(pc1~sl, data = rod.sl.df)
summary(rod.sl.mod1)

# Scatter plot
plot(pc1~sl, data = rod.sl.df,  
     asp = 1, las = 1, 
     col = col.loco, bg = col.loco,  # color
     pch = pch.clade, 
     main = "PC1 = f(SL)", 
     xlab = "SL (mm)", ylab = "PC1")
# Names
text(rod.sl.df$sl, rod.sl.df$pc1, 
     labels = data.rod$Abbr, pos = 4, cex = .7, col = col.loco)
# Regression line
lines(rod.sl.mod1$fitted.values~rod.sl.mod1$model$sl, 
      col = "red", 
      lty = 1,  # solid line
      lwd = 1)  # line width
# Add a legend
legend("topright",  # position of the legend
       title = "Clade", # title of the legend
       legend = clade$names,  # text of the legend
       pch = clade$pch,  # symbols of the legend
       pt.bg = "black", 
       cex = .6, 
       border = F)  # color of the legend

# ---- 2.2 Models 2 and 3: Effect of clade ----

# Define the subset (specimen removed when only 1 case per group)
rod.sl.df.clade <- subset(rod.sl.df, clade %in% c("gl", "sq"))
rod.sl.df.clade$clade <- factor(rod.sl.df.clade$clade)

# Linear regression on SL and clade: additive effects
rod.sl.mod2 <- lm(pc1~sl+clade, data = rod.sl.df.clade)
summary(rod.sl.mod2)

# Linear regression on SL and clade: interaction effects
rod.sl.mod3 <- lm(pc1~sl*clade, data = rod.sl.df.clade)
summary(rod.sl.mod3)

# ---- 3. Linear classification: canonical variate analysis (CVA) ----

# In this section, we try to find association of variables that discriminate among locomotor groups. We use a reduced number of variables (the 3 first principal components), because the number of variables has to be lower than the number of cases in each group. 
# Here we do a canonical variate analysis (CVA), the extension of a linear discriminant analysis (LDA) for more than 2 groups (note that in R, the function used is `lda`, so the variables are named LD1 and LD2 in the R output).  

# Define the subset (specimen removed when only 1 case per group or unknown)
rod.sl.df.loco <- subset(rod.sl.df, loco %in% c("arb", "fos", "gli"))
rod.sl.df.loco$loco <- factor(rod.sl.df.loco$loco)

# CVA
rod.sl.loco.cva <- lda(loco~pc1+pc2+pc3, data = rod.sl.df.loco)
rod.sl.loco.cva

# Classification of the observations
rod.sl.loco.cva.pred <- predict(rod.sl.loco.cva, rod.sl.df.loco)
# Confusion matrix (counts)
table(rod.sl.df.loco$loco, rod.sl.loco.cva.pred$class)
# Confusion matrix (proportions)
prop.table(table(rod.sl.df.loco$loco, rod.sl.loco.cva.pred$class))

# Prediction for the unknown
rod.sl.loco.cva.pred.new <- predict(rod.sl.loco.cva, subset(rod.sl.df, loco == "unb"))
rod.sl.loco.cva.pred.new

# Visualization of the scores along the two discriminant vectors.

# Scatter plot
plot(rod.sl.loco.cva, 
     las = 1, 
     col = col.loco[which(data.rod.sl$Loco %in% c("arb", "fos", "gli"))], 
     main = "CVA", xlab = "CV1", ylab = "CV2")
# New specimens as a black points
points(rod.sl.loco.cva.pred.new$x, col = "black", pch = 16)
text(rod.sl.loco.cva.pred.new$x, pos = 4, cex = .5, 
     labels = data.rod.sl$Abbr[which(data.rod.sl$Loco == "unb")])

