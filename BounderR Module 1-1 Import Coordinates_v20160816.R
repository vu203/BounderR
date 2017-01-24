### TODO(vasja): generalise '12' columns in "table1" loop (currently lines 88-89), make global variable, columns per filopodium in filo.txt (cpf.F) and columns per filo in coordinates.txt (cpf.C)  
### TODO(vasja): generalise '12' columns in "table1" loop (currently lines 88-89)


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
# Coordinates import

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
ncol(coord.table)
bb <- max.t - max.c   # bb for "base backprojections", effectively the n of NA rows
bb.table <- data.frame(matrix(NA, nrow = bb, ncol = ncol(coord.table)))
colnames(bb.table) <- colnames(coord.table)
coord.table <- rbind(bb.table, coord.table)



# Needs cleaning up: 1) extra columns removed (?) and 2) NAs at bottom need pushing to top to match rownumber to T from the Filopodia table (tip.table). Fixed above (bb.table). 3) Missing rows instead of NA rows when structure removed in Edit tracks (see e.g. in this dataset structure number 8, timepoint 92, compare in Filopodia vs Coordinates table): fixed in code below with the use of matching vector. 4) NAs at the bottom of tables coming out as 0.000. fixed in code below with the row sums == 0 (within if statement).

# This loop below is not required for a clean dataset in the future versions of Bounder if the code for Coordinates table in Fiji is made consistent with the code for Filopodia table. For now, clean up with:


# min.dT <- min(all.dT, na.rm = TRUE)
# max.dT <- max(all.dT, na.rm = TRUE)

# dT.vector <- min.dT:max.dT
# table1 <- data.frame(matrix(NA, nrow = max.t, ncol = 0))
# for (i in 1:(ncol(coord.table)/12)) {					# Change to 'clean' division
#	minitable = coord.table[, ((i-1)*12+1):((i)*12)]  # 12 is current number of columns per filo # Changed 12 to 12. 17.8.2016
#	row.sums = rowSums(minitable)
#	if (length(which(row.sums == 0)) > 0) {				# Zeroes instead of NAs at bottom of tables
#		minitable[which(row.sums == 0), ] <- NA
#	}
#	dT = minitable[, 1] - min(minitable[, 1], na.rm = TRUE)
#	minitable = cbind(dT, minitable)      
#	matching.vector = match(dT.vector, minitable[, 1]) 
#	matched.table = minitable[matching.vector, ]
#	table1 = cbind(table1, matched.table)
#}
#coord.table <- table1



	

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

