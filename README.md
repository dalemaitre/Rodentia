# Rodentia
R scripts for the analysis of ear morphology in Rodentia

In this document we briefly describe how to do the analyses for the manuscript *Glimpse into the past – Analyses of the bony labyrinth in extinct rodents (Eusciurida; Rodentia; Mammalia) for implications of the locomotory behaviour* 

R scripts are provided in the GitHub repository.  

# 1. System requirement

For the analyses, we use the software R, which is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS. The scripts were run with R version 4.4.2. 

It is recommended (but not mandatory) to use R with RStudio, a free integrated development environment (IDE). RStudio runs on a wide variety of UNIX platforms, Windows and MacOS. It requires R version 3.6.0 or more recent. For the analyses, we used RStudio Desktop version 2024.12.0. 

Some R scripts require the installation of supplementary packages. For the analyses, we used the following packages:  

* `ape` version 5.8-1  
* `geomorph` version 4.0.10  
* `phytools` version 2.4-4  
* `MASS` version 7.3-61
* `Morpho` version 2.13

# 2. Installation guide

## Download and install R and RStudio

1. To download and install R, follow the instruction on the R Project webpage: <https://www.r-project.org/>. 

2. **[Optional, but recommended]** After installing R (> 3.6.0), download and install the desktop version of RStudio: <https://posit.co/download/rstudio-desktop/>. 

The downloading and installation of R and RStudio will take a few minutes, depending on the quality of the internet connection. 

## Download and install the required R packages

After the installation of R and then RStudio, installation of supplementary packages is required for the analyses.  

To install the latest version of a package, open RStudio (or R if RStudio is not installed), then write the following prompt in the console (here for the package `ggplot2`):
```{r install}
install.packages("ape")  # to install ape 
```

Run similar operations for all the packages listed above by replacing `ape` with the name of the package.  

The downloading and installation of each package will take a few minutes, depending on the quality of the internet connection. Some packages depend on other packages and hence their installation requires the installation of the packages on which the depend.  

If asked, we recommend using stabilized versions of the package, and not binaries. 

# 3. Demonstration and instruction for use

## Preparation

1. Download all R scripts and raw data files (see **section 4. Description of the scripts and data**)

2. Put all R scripts and raw data files in a folder <MyFolder>. 

## Creation of an R project with RStudio [OPTIONAL]

If you are using RStudio, it is recommended to create an R project:  

1. Open RStudio  
2. On the upper right corner of the window: Project > New Project > Existing Directory  
3. Browse to select the directory <MyFolder> with the scripts and raw data files  
4. Click on "Create Project"  

After creating the project, the working directory is set to the directory <MyFolder>.  

The name of the project appears on the upper right corner of the window. There you can change projects, or close all projects (and hence go back to the default directory).  

## How to run scripts

1. Set the working directory so that it corresponds to the folder where your scripts and data are (use `/` for Windows systems and `\` for Mac OS). If you have an R project (see above), the working directory is already correct.
```{r wd}
setwd("C:/.../MyFolder")  # change the path to fit the relevant folder
```

2. Open the first file `01_RodentiaRawData.R` and run all lines in the console. This script has to be run before all the others, because it is where the data sets and graphical parameters are created. In RStudio, data, values and functions are visible in the `Environment` tab. Alternatively, it is possible to run the script with the command:   
```{r rawData}
source("01_RodentiaRawData.R")
```

3. Open the script of interest. All R scripts other than `01_RodentiaRawData.R` can be run independently.  

4. Load the packages relevant for the script (listed in the description of the script). This enable an access to supplementary R functions and data sets not provided with eh base R environment.
```{r geomorph}
library(geomorph)  # here an example to load the package "geomorph"
```

5. Now you can run the commands in the console.  

# 4. Description of the scripts and data sets

## Description of the raw data

Data files are:

* `Rodentia_IE.csv`: measurements used for the analyses
* `Rodentia_1000.nex`: 1000 phylogenetic trees of the extant taxa in NEXUS format, downloaded from <https://vertlife.org/phylosubsets/> on 2025-01-20 => used for the computation of the phylogenetic signal
* `RodentiaTree.nex`: consensus tree computed based on `Rodentia_1000.nex` => used for the computation of the phylogenetic signal

## Description of the scripts

We provide 4 scripts that correspond to the analyses performed using R, as described in the main text. Four files are provided: 

* `01_RodentiaRawData.R`: load and prepare data set, set graphical parameters *[running time: 1s]*  
* `02_RodentiaLinearAngular.R`: PCA, linear regressions and CVA of the measurements of the inner ear, standardized by skull length *[running time: 3s]*  
* `03_RodentiaVolumes.R`: bivariate association between skull shape and inner and middle ear volumes *[running time: 3s]*  
* `04_RodentiaPhylTree.R`: phylogenetic signal in different sets of variables *[running time: 18s]*  
