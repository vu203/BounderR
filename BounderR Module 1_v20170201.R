
#-------------------------------------------------------------------------------


# This script forms part of a set of analysis scripts for quantifying dynamic 
# behaviour of filopodia with the Bounder Fiji plugin. 

# More information available from: vu203@cam.ac.uk. 


#-------------------------------------------------------------------------------
# BounderR Module 1A  -  IMPORTING DATA TABLES 
#-------------------------------------------------------------------------------


# This script performs the following: 


# MODULE 1A:  DATA IMPORT

# - Imports all data files in working directory named "... Filopodia.txt"
# - Extracts values for multiple parameters (e.g. Length, dL, DCTM) for each 
#     protrusion and combines them in data tables combining all measured
#     protrusions .
# - Stores the following variables within the current R environment:

#   - Length:  Length of structure 
#   - dL:      Change in length from previous timepoint
#   - DCTM:    Direction-corrected tip movement between adjacent timepoints
#                (DCTM>0: extension, DCTM<0: retraction )
#   - dB:      Base movement between adjacent timepoints 
#                (computed as dB = dL - DCTM)
#                (dB>0: distal movement, dB<0: proximal movement)
#   - TipF:	   Tip fluorescence  (thresholded pixel intensity)
# 	- BaseF:   Base fluorescence (mean pixel intensity)
#   - ProjF:   Projection fluorescence (mean pixel intensity)
#   - BodyF:   Body fluorescence


# MODULE 1B:  DATA CLEAN-UP 

# - Filters DCTM and dB data so as to exclude 1% of extreme values, 
#     (cutoff between 0.5th and 99.5th percentiles) which may have resulted 
#     from incorrect segmentation.
# - Smoothes the noise in DCTM values by applying travelling mean


#-------------------------------------------------------------------------------
# USER INPUT:  Edit this section.


# What is the rate of timelapse acquisition?

spt <- 2  # seconds per timepoint
pxw <- 0.065 # pixel width in microns

# For manual input (single folder to analyse):

# setwd("")  #  <---- Set the directory filename containing your data and uncomment. Skip step below (automated input from parent script)
# e.g. setwd("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/TOTAL-ANALYSIS_RENYI2-6_ED4") 


# For automated scripted input of multiple folders (edit in parent script):

module.name <- "BounderR Module1.R"
initiate <- function() {

	if (exists("iter") == FALSE) {
		stop("Please define source folders in parent script.")
	} else { 

		if(iter == 1) {
			setwd(folder.names[1])
			print(module.name)
			print("EXECUTING ON:")
			print(folder.names[1])
		} else if(iter == 2) {
			setwd(folder.names[2])
			print(module.name)
			print("EXECUTING ON:")
			print(folder.names[2])
		} else print("Error: iter OUT OF RANGE, folders not defined. Get some fresh air. Make more folder names in parent script.")
	}
}

initiate()

getwd()
list.files()


# END OF REQUIRED USER INPUT. 


#-------------------------------------------------------------------------------
# Read all data tables of "[...] filopodia.txt"


Loc <- getwd()
all.names <- list.files(as.character(Loc), pattern = "*Filopodia.txt")
all.names2 <- list.files(as.character(Loc), pattern = "*Coordinates.txt")
all.names
all.names2

# Measure max number of rows in any table, important for binding tables later:

n.tables <- length(all.names)
n.tables
rows <- vector(mode = "numeric", length = 0)

for (i in seq_along(all.names)) {
	rows <- append(rows, nrow(read.table(all.names[i], sep = "\t", skip = 1)))
}

max.t <- max(unlist(rows))
max.t  # Max number of rows per table.
rm(rows) 


# --- --- --- OPTIONAL USER INPUT: --- --- ---
# Insert a specifier for import (e.g. "CTRL" or "s09") in order to select only a 
# subset of tables for processing.
# TODO(vasja): input this as a string on top of script (in "User input").

extract <- grep(pattern = c(""), x = all.names)  
#                            ^ Insert specifier inside "" (e.g. "CTRL"). 
extract


# Now import all " filopodia.txt" tables into a data frame called "tip.table": 

tip.table <- data.frame(matrix(NA, nrow = max.t, ncol = 0))

for (i in extract) {
	all.names[i]
		vec      	   = readLines(all.names[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))
		tab      	   = read.table(all.names[i], sep = "\t", skip = 1)
		colnames(tab)  = header
		top.up.rows    = max.t - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab), 
						nrow = top.up.rows))
		colnames(top.up.table) = header
		tip.table.i    = rbind(tab, top.up.table)
		tip.table	   = cbind(tip.table, tip.table.i)	
		rm(vec, header, tab, top.up.rows, top.up.table, tip.table.i)			
}
tip.table <- tip.table[names(tip.table) != "dT"]
tip.table[1:25, 1:9]  # Print top of the table for example filopodium 1.

coord.table <- data.frame(matrix(NA, nrow = max.t, ncol = 0))  # CAREFUL!! Problems with current export format. See Module 1.1 (Import Coordinates)

for (i in extract) {
	all.names2[i]
		vec      	   = readLines(all.names2[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))
		tab      	   = read.table(all.names2[i], sep = "\t", skip = 1)
		colnames(tab)  = header
		top.up.rows    = max.t - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab), 
						nrow = top.up.rows))
		colnames(top.up.table) = header
		coord.table.i  = rbind(tab, top.up.table)
		coord.table	   = cbind(coord.table, coord.table.i)	
		rm(vec, header, tab, top.up.rows, top.up.table, coord.table.i)			
}
coord.table <- coord.table[, - (which((names(coord.table) == " ")))]
coord.table[1:20, 1:12]

#-------------------------------------------------------------------------------

# Creating tables by metric (Length, dL, DCTM, dB, Tip Fl., Base Fl.),
# as well as corresponding timepoints of the original timelapse (T) and 
# the time from the first appearance of the filopodium (dT).
# Each table combines data for that metric for all filopodia, from all tables.

names(tip.table[1:9])

 all.T      <- tip.table[, grep(pattern = c("^T "), x = names(tip.table), 
  value = FALSE)] 

all.dT     <- tip.table[, grep(pattern = c("dT"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dS     <- all.dT * spt  # Seconds per timepoint defined on top of script.

dS.vector  <- apply(all.dS, 1, mean, na.rm=TRUE)

all.base   <- tip.table[, grep(pattern = c("Base Mean"), x = names(tip.table),
  value = FALSE, fixed = TRUE)]

all.body   <- tip.table[, grep(pattern = c("Body Mean"), x = names(tip.table),
  value = FALSE, fixed = TRUE)]
  
all.base.nor <- all.base / all.body  

all.tip    <- tip.table[, grep(pattern = c("Tip Mean"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)] 

all.th.tip <- tip.table[, grep(pattern = c("Tip Th Mean"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]    # Added 17.8.

all.proj   <- tip.table[, grep(pattern = c("Proj"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]

all.tip.nor1 <- all.tip / all.proj  # normalise to projection intensity
all.tip.nor2 <- all.tip / all.body  # normalise to GC body intensity

all.length <- tip.table[, grep(pattern = c("Length"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dL     <- tip.table[, grep(pattern = c("dL"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dctm   <- tip.table[, grep(pattern = c("DCTM"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dcbm  <- tip.table[, grep(pattern = c("DCBM"), x = names(tip.table),  # Added 17.8.
  value = FALSE, fixed = TRUE)]
  
all.dB     <- all.dctm - all.dL

ls()


# SAVING WORKSPACE with raw tables prior to filtering.

# condition <- "Ctrl"
#      ^ Insert session name here. E.g. conditionX, d1, etc.

# Name
# newdirname = paste ("R-Analysis_", condition, sep = "")
# newdirname
# dir.create(newdirname)  # TODO(vasja): Check directory not present already, check doesn't overwrite!
# setwd(newdirname)
# getwd()
# save.image(file = paste("01_BounderImport_RAW_", condition, ".Rdata", sep = ""))

# Export each table as a raw data table (in case it is useful for 
# external processing).

# Using a data frame below in order to signal error if lengths not matching.

# Specifying data export:
 # from = c ("all.dS","all.base","all.tip","all.length","all.dL","all.dctm","all.dB")
 # content = c("dS", "Base", "Tip", "Length", "dL-raw", "DCTM-raw", "dB-raw")
 # file.names = paste(paste(condition, content, sep = "_"), ".csv", sep = "") 
  
#  if(length(from) == length(file.names)) {
#  	print(noquote("Saving tables into .csv files..."))
#  } else {
#  	print("ERROR: please check 'from' and 'file.names' have equal number of elements. Check output files.")
    # stop  # TODO(vasja) - make script terminate here if condition not satisfied
#    }	
# Exporting raw tables ...  	  
# export <- data.frame(cbind(from, content, file.names))
# for (i in seq_along(file.names)) {
#	name = as.character(export$file.names[i])	
#	get = get(as.character(export$from[i]))
#	write.csv(x = get, file = as.character(name))
#	rm(name, get)
#}

rm(i, export, from, content, file.names)
ls()

#-------------------------------------------------------------------------------
# BounderR Module 1B  -  DATA CLEAN-UP 
#-------------------------------------------------------------------------------

# The 99% filter for DCTM and dB values:
# Remove data outside the 0.5-99.5th percentiles for DCTM and dB. These are 
# likely to be erroneous due to large displacement of the tip when experiencing 
# driftin in and out of focus, or the length being fully/partially reconstructed. 
# Base displacement of very large magnitude occur when very small widening of the 
# protrusion near the base moves the asigned base position outwards (due to 
# discrete cutoff in base assignment during the segmentation process, as specified
# with ROI erosion-dilation [ED] cycles). 

# The smoothing filter for DCTM and dB values: 
# Applies a moving average smoothing to DCTM and dB tables, by default with a step 
# size of 5.


#-------------------------------------------------------------------------------
# Create DCTM and dB value tables with a 99 percent distribution filter 
# (0.5-99.5th percentiles)

dctm.mincut <- quantile(unlist(all.dctm), probs = c(0.005, 0.995), na.rm = TRUE)[1] 
dctm.maxcut <- quantile(unlist(all.dctm), probs = c(0.005, 0.995), na.rm = TRUE)[2]

dB.mincut <- quantile(unlist(all.dB), probs = c(0.005, 0.995), na.rm = TRUE)[1] 
dB.maxcut <- quantile(unlist(all.dB), probs = c(0.005, 0.995), na.rm = TRUE)[2] 

dcbm.mincut <- quantile(unlist(all.dcbm), probs = c(0.005, 0.995), na.rm = TRUE)[1] 
dcbm.maxcut <- quantile(unlist(all.dcbm), probs = c(0.005, 0.995), na.rm = TRUE)[2] 

RemoveOutliers <- function(x, mincut, maxcut) {
	y <- x
	y[x < mincut] <- NA
	y[x > maxcut] <- NA
	y
}

all.dctm99 <- RemoveOutliers(all.dctm, dctm.mincut, dctm.maxcut)
all.dB99   <- RemoveOutliers(all.dB, dB.mincut, dB.maxcut)
all.dcbm99   <- RemoveOutliers(all.dcbm, dcbm.mincut, dcbm.maxcut)

#-------------------------------------------------------------------------------
# Create DCTM and dB value tables smoothed with a travelling mean filter 

MovingAverage <- function(x, w = 5) {
		filter(x, rep(1/w, w), sides = 2)
}

sm.dctm99 <- apply(all.dctm99, 2, MovingAverage)  # Mean-smoothed DCTM values after 99% filter
sm.dB99   <- apply(all.dB99, 2, MovingAverage)   # Mean-smoothed dB values after 99% filter 
sm.dcbm99   <- apply(all.dcbm, 2, MovingAverage)   # Mean-smoothed dB values after 99% filter 



# write.csv(x = sm.dctm99, file = paste(condition, "DCTM-filtered.csv", sep = "_"))
# write.csv(x = sm.dB99, file = paste(condition, "dB-filtered.csv", sep = "_"))
# getwd()

# In order to streamline subsequent downstream processing, convert
# sm.dctm99 to all.dctm (leaving user the option to work with either
# without affecting the code in those modules).

all.fdctm <- sm.dctm99
all.fdB   <- sm.dB99 
all.fdcbm   <- sm.dcbm99 

rm(sm.dB99, sm.dctm99, sm.dcbm99)
ls()
	

