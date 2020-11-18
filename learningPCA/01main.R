#=============================================================================
#TITLE: MAIN SCRIPT - FACTORS FOR OBESITY IN THE U.S.A.
#AUTHORS: CYRUS RAJ GAUTAM, FERDINAND GRUENENWALD
#=============================================================================
#
#
# show which version of R we are using for reproducibility
R.version.string
#  we used : "R version 4.0.3 (2020-10-10)"
#
# =============================================================================
# NOTES
# • In this project I will explore methods to visualize and analyze high 
#   dimensional dat
# • I start by creating a small artificial data set, which we then analyze and 
#   see what results of a principle component analysis are expected to look like
# • then we will try to implement the methods on a larger real dataset. 
#   I chose to analyze the data set from Food environment atlas, which captures 
#   many factors that one would expect to impact dietary trends  for the 
#   population of the United States of America 
#
# =============================================================================
# --- global variables ---
# we indicate variable that is static and should remain the same throughout the
# project

wk.dir <- getwd() #get working directory

#load the datasets that we will use:
#Loading the data of Obesity rate and Fast Foood Restaurant

# =============================================================================


#===============================================================================
#Installing various packages to use in our program.
install.packages("ggplot2")
install.packages("reshape2")
install_github("genomicsclass/rafalib")
install.packages("gplots")
install.packages("zoo")
install.packages("cluster")
install.packages("RColorBrewer")

#Calling the installed packages
library(ggplot2)
library(reshape2)
library(devtools)
library(rafalib)
library(gplots)


#=============================================================================
  # --- folder management ---
  
  # names of project folders ("figures", "data.raw","data.clean","results")
  # store names of the folders in an object
  folder.names <- c("1.data.raw","2.data.clean", "3.results",
                    "4.figures")
# and make the folders if they don't exit yet. No need to understand this now
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

# you need to store in an object the file path to these folders so we can 
# read from them and write to them.
#the raw.data folder should already have the data sets in it
path.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
path.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
path.results <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
path.fig <- paste(wk.dir, "/", folder.names[4], "/", sep = "")
# ==============================================================================
# --- run scripts ---
#Simulation:
source(paste(wk.dir, "/", '02.Simulation.R', sep = ""))
source(paste(wk.dir, "/", '03.FEA.Cnty.Analysis.R', sep = ""))

# ==== end =====================================================================
#===============================================================================
#===============================================================================

