#-------------------------------------------------------------------------------
# BounderR Module 1.1  -  IMPORTING COORDINATES DATA TABLE
#-------------------------------------------------------------------------------

# This script forms part of a set of analysis scripts for quantifying dynamic 
# behaviour of filopodia with the Bounder Fiji plugin. 

# Required: run Module 1A, lines 1-118 (Module 1.1 assumes existing variables Loc, max.t, all.names2).


# For manual input (single folder to analyse):

# setwd("")  #  <---- Set the directory filename containing your data and uncomment.
# e.g. setwd("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/TOTAL-ANALYSIS_RENYI2-6_ED4") 


# For automated scripted input of multiple folders (edit in parent script):

module.name <- "BounderR Module1-1 Import Coordinates.R"
initiate <- function() {

	if (exists("iter") == FALSE) {
		stop("Please define source folders in parent script.")
	} else { 

		if(iter == 1) {
			setwd(folder.names[1])
			print(module.name);
			print("EXECUTING ON:");
			print(folder.names[1])
		} else if(iter == 2) {
			setwd(folder.names[2])
			print(folder.names[2])
			print(module.name)
			print("EXECUTING ON:")
			cat(folder.names[2])
		} else cat("Error: iter OUT OF RANGE, folders not defined. Get some fresh air. Make more folder names in parent script.")
	}
}
iter
initiate()

getwd()
list.files()

#-------------------------------------------------------------------------------
# Coordinates import  # This section created 'bb' (base backprojections count) in previous versions, this is not accurate anymore, nor required. Add 'bb' elsewhere.

rows.c <- vector(mode = "numeric", length = 0)  
for (i in seq_along(all.names2)) {
	rows.c <- append(rows.c, nrow(read.table(all.names2[i], sep = "\t", skip = 1)))
}
max.c <- max(rows.c)

coord.table <- data.frame(matrix(NA, nrow = max.c, ncol = 0))
for (i in extract) {
	all.names2[i]
		vec      	   = readLines(all.names2[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))
		tab      	   = read.table(all.names2[i], sep = "\t", skip = 1)
		colnames(tab)  = header
		top.up.rows    = max.c - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab),  
						nrow = top.up.rows))
		colnames(top.up.table) = header
		coord.table.i  = rbind(tab, top.up.table)
		coord.table	   = cbind(coord.table, coord.table.i)	
		rm(vec, header, tab, top.up.rows, top.up.table, coord.table.i)			
}
coord.table <- coord.table[, - (which((names(coord.table) == " ")))]  # Removes extra columns at the beginning of each GC table

# Equalise number of rows between tip.table and coord.table
# comment on 15.03.2017: 

# (kept this section for compatibility with old versions of Bounder (before the 
# Coordinates table was made consistent with the Filopodia table in CAD-Bounder),
# however can no longer use this method to accurately set bb. Changed the assignment # of bb so this is now in the parent script (MASTERSCRIPT).

mm <- max.t - max.c   # mm for "mismatch" between the two tables, effectively the n of NA rows present in one but not the other (called 'bb' in previous versions)
mm.table <- data.frame(matrix(NA, nrow = mm, ncol = ncol(coord.table)))
colnames(mm.table) <- colnames(coord.table)
coord.table <- rbind(mm.table, coord.table)
rm(mm, mm.table)

# Needs cleaning up: 1) extra columns removed (?) and 2) NAs at bottom need pushing to top to match rownumber to T from the Filopodia table (tip.table). Fixed above (bb.table). 3) Missing rows instead of NA rows when structure removed in Edit tracks (see e.g. in this dataset structure number 8, timepoint 92, compare in Filopodia vs Coordinates table): fixed in code below with the use of matching vector. 4) NAs at the bottom of tables coming out as 0.000. fixed in code below with the row sums == 0 (within if statement).


# Calculate euclidean distance from Base(X,Y) to Tip (X,Y)

X.bases <- coord.table [, grep(pattern = c("Base X" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]
Y.bases <- coord.table [, grep(pattern = c("Base Y" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]
X.tips  <- coord.table [, grep(pattern = c("Tip X" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]
Y.tips  <- coord.table [, grep(pattern = c("Tip Y" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]
  
all.euclid <- sqrt( (X.tips - X.bases)^2 + (Y.tips - Y.bases)^2 )

all.straightness <- all.euclid/all.length
all.waviness <- 1 - all.straightness

ls()

