### BounderR_MASTERSCRIPT

# This script CALLS VARIOUS BOUNDER MODULES AND INTEGRATES OUTPUT!

#-------------------------------------------------------------------------------
# Clean current workspace. WARNING: deletes everything in current workspace

rm(list = ls())
ls()


#-------------------------------------------------------------------------------
# Setting Working Directory locations:

# DATA:

# Where is your data located?

folder.names <- c(
    folder.name1 = c("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/BounderR/TimeSpentExtending/embed-into-BounderR/BATCH_CTRL_s15_Huang4-01_ed4_v20160816"), 
    folder.name2 = c("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/BounderR/TimeSpentExtending/embed-into-BounderR/BATCH-ANALYSIS_RENYI2-6_ED4_v20160816") )
n.fold <- length(folder.names)
cat("Number of folders to analyse :", n.fold)
folder.names[]

# What do you want to call your datasets (e.g. "Ctrl" and "Drug1")

dataset.names <- c(
	dataset1 = "Ctrl_s15_Huang4-01"  , # <---- Insert dataset names here.
	dataset2 = "Ctrl_pool_Renyi2-6"	   # <---- Insert dataset names here.
	)


# SCRIPTS:

Loc.Modules <- c("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/BounderR/TimeSpentExtending/embed-into-BounderR")
scripts <- c("BounderR Module 1_v20160816.R", "BounderR Module 1-1 Import Coordinates_v20160816.R", "BounderR Module 2.R")


#-------------------------------------------------------------------------------
# Objects to keep between script runs when cleaning workspace:

keep <- c("Loc.Modules", "folder.names", "dataset.names", "n.fold", "scripts", "metalist", "objectnames", "keep", "iter", "i", "BounderR")  


#-------------------------------------------------------------------------------
# BounderR ("run suite of BounderR modules") function calls all defined scripts, executes one after another on one defined dataset

BounderR <- function() {
	
	for (i in seq_along(scripts)) {
		setwd(Loc.Modules);
		source(scripts[i], echo = FALSE);
		ls()
	}		
}


#-------------------------------------------------------------------------------
# Loop BounderR over all defined folders on all defined scripts.

metalist    <- list()
objectnames <- list()

for (iter in seq_along(folder.names)) {
	rm(list = setdiff(ls(), keep))

	# RUN BOUNDER MODULES AS A FUNCTION
	BounderR()
	
	# Save variables before iteration on next folder:
	objectnames[[iter]] <- setdiff(ls(), keep)
	dataset <- lapply(objectnames[[iter]], get)
	names(dataset) <- objectnames[[iter]]
	metalist[[iter]] <- dataset
	rm(dataset)
	rm(list = setdiff(ls(), keep))	
}

names(metalist) <- dataset.names
names(metalist)

ls()


save.image(file = "LastWorkspace.Rdata")








