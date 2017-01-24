

#-------------------------------------------------------------------------------
# BounderR Module 2  -  EXTRACTING DESCRIPTIVE METRICS per filopodium
#-------------------------------------------------------------------------------
# And look at cross-correlations.

# - Max Length							:   max.lengths
# - Median tip extension rate (DCTM +ve)  :	med.rate.extens
# - Median tip retraction rate (DCTM -ve) :	med.rate.retract
# - Median base invasion (dB +ve)		:	med.dB.invas
# - Median base retraction (dB -ve)		: 	med.dB.retract
# - Consistency of growth (the ACF root) 
#										:	acf.dctm.roots
# - Initial DCTM (new filo only)		:	dctm99.new.early.mean / .med
# - Initial dB (new filo)				:	dB99.new.early.mean / .med
# - Mean straightness					: 	straightness; waviness (1 - straightness)
#-------------------------------------------------------------------------------

# Optional: Suppress plotting

draw.plots = FALSE


Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }




# FILE MANAGEMENT:	
# Set the folders containing the data. See parent script 
# ("BounderR-structuresmanagement.R") for script input of multiple folders.

# For manual input (single folder to analyse):

# setwd("")  #  <---- Set the directory filename containing your data and uncomment.
# e.g. setwd("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/TOTAL-ANALYSIS_RENYI2-6_ED4") 

# For automated scripted input of multiple folders (edit in parent script):

module.name <- "BounderR Module2.R"
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

#-------------------------------------------------------------------------------
# Max length


head(all.length)

max.lengths <- apply(all.length, 2, max, na.rm = TRUE)

if(draw.plots == TRUE) {

	dev.new()
		hist(max.lengths, 
			col = 'cadetblue', border = 'white',
			main = "Histogram of Max Lengths",
			xlab = expression("Max Length [" * mu * "m]") )
}

#-------------------------------------------------------------------------------
# Tip extension and rectraction rates (median per filopodium)
# TODO(vasja): Consider if this needs adapting to reflect the states based on fdctm values (below)


MedianOfPositive <- function(x) {
	median(subset(x, x > 0), na.rm = TRUE)
}
MedianOfNegative <- function(x) {
	median(subset(x, x < 0), na.rm = TRUE)
}


med.rate.extens <- apply(all.dctm99, 2, MedianOfPositive)
med.rate.retract <- apply(all.dctm99, 2, MedianOfNegative) 

if( draw.plots == TRUE) {
dev.new()
	hist(med.rate.extens,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median invasion rates per base",
		xlab = expression("Median dB (> 0) [" * mu * "m / 2 s]") )
dev.new()		
	hist(med.rate.retract,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median retraction rates per base",
		xlab = expression("Median dB (< 0) [" * mu * "m / 2 s]") )
}

# Base invasion and rectraction rates (median per filopodium)

med.dB.invas <- apply(all.dB99, 2, MedianOfPositive)
med.dB.retract <- apply(all.dB99, 2, MedianOfNegative) 

if( draw.plots == TRUE) {
dev.new()
	hist(med.dB.invas,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median invasion rates per base",
		xlab = expression("Median dB (> 0) [" * mu * "m / 2 s]") )
dev.new()		
	hist(med.dB.retract,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median retraction rates per base",
		xlab = expression("Median dB (< 0) [" * mu * "m / 2 s]") )
}




#-------------------------------------------------------------------------------
# Consistency of growth (DCTM ACF roots)

dctm <- all.dctm99

MaxLag <- 120

AcfTable <- function(x, L) {
	y <- data.frame(matrix(NA, ncol = ncol(x), nrow = L + 1))
	colnames(y) <- colnames(x)
	for (i in 1:ncol(x)) {
		acf.i <- as.vector(acf(x[, i], na.action = na.pass, lag.max = MaxLag, plot = FALSE)[[1]])
		y[, i] <- acf.i
		rm(acf.i)
		}
	y	
}
acf.dctm <- AcfTable(dctm, 120) 

# Extract first negative in column (the more strongly correlated the timepoints, the longer it takes for ACF to decay to 0; the offset of decay to 0 is taken as a measure of growth consistency ["ACF (DCTM) crosspoint")]

FirstNegative <- function(x) {
	if(sum(!is.na(x)) > 0) {				
		min (which(x <= 0), na.rm = TRUE)	# requires at least one non-NA value in vector (if >3 elements in dctm column acf is not computed, so acf is NA throughout, which returns Inf) 
	} else {
		NA									# returns NA in place of Inf
	}
}

acf.dctm.roots <- apply (acf.dctm, 2, FirstNegative)

if( draw.plots == TRUE) {
	hist(acf.dctm.roots, breaks = 25, col = 'salmon', border = 'white')
}
acf.dctm.roots 


#-------------------------------------------------------------------------------
# Extracting new vs pre-existing filopodia: 

min.dT <- apply(all.dT, 2, min, na.rm=TRUE)
new <- which(min.dT < 0)
exist <- which(min.dT >= 0)


#-------------------------------------------------------------------------------
# Initial DCTM and dB [over nt (10) timepoints] for new filopodia

nt <- 10  # Number of timepoints of interest post-formation
bb  # Base backprojections (variable defined in Module 1)
early <- (bb+1):(bb+1+nt)  # vector of the timepoints required in this section 
early

dctm99.new.early.med <- apply(all.dctm99[early, new], 2, median, na.rm = TRUE)
dctm99.new.early.mean <- apply(all.dctm99[early, new], 2, mean, na.rm = TRUE) 

if( draw.plots == TRUE) {
dev.new()
	hist(dctm99.new.early.mean,
		col = 'cadetblue', border = 'white',
		main = "Histogram of initial DCTM per filopodium",
		xlab = expression("DCTM [" * mu * "m / 2 s]") )
}

dB99.new.early.med <- apply(all.dB99[early, new], 2, median, na.rm = TRUE)
dB99.new.early.mean <- apply(all.dB99[early, new], 2, mean, na.rm = TRUE)

if( draw.plots == TRUE) {
	dev.new()
		hist(dB99.new.early.mean,
			col = 'cadetblue', border = 'white',
			main = "Histogram of initial dB per filopodium",
			xlab = expression("DCTM [" * mu * "m / 2 s]") )
		dB99.new.early.mean

	dev.new()
		plot(dctm99.new.early.mean, dB99.new.early.mean, 
			ylim = c(-0.5,0.5), xlim = c(-0.5,0.5),
			pch = 16, col = 'black',
			main = "DCTM vs dB per filopodium over initial 10 timepoints",
			xlab = expression("Mean DCTM [" * mu * "m / 2 s]"),
			ylab = expression("Mean dB [" * mu * "m / 2 s]") )
		abline(h = 0, col = 'red', lty = 3)
		abline(v = 0, col = 'red', lty = 3)	
}

#-------------------------------------------------------------------------------
# Straightness of filopodia:

# 1. Calculate euclidean distance from Base(X,Y) to Tip (X,Y)
# 2. straightness = euclidean distance / length 

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
all.waviness     <- 1 - all.straightness

straightness.mean <- apply(all.straightness, 2, mean, na.rm = TRUE)
waviness.mean     <- apply(all.waviness, 2, mean, na.rm = TRUE)
length.mean       <- apply(all.length, 2, mean, na.rm = TRUE)


if( draw.plots == TRUE) {
	dev.new()
		matplot(unlist(all.length), unlist(all.waviness), 
		main = "Waviness vs Length",
		ylab = "Waviness", xlab = expression("Length [" * mu * "m]"),
		pch = 4, cex = 0.4, col = rgb(0,0,0,0.2))
	    abline(h = 0.38, col = rgb(1,0,0,0.5), lty = 3)	
	    abline(v = 1.8, col = rgb(1,0,0,0.5), lty = 3)

	dev.new()
		matplot(length.mean, waviness.mean,
		main = "Waviness vs Length (means per filopodium)",
		ylab = "Mean Waviness", xlab = expression("Mean Length [" * mu * "m]"),
		pch = 4, cex = 0.4, col = rgb(0,0,0,0.8))
	    abline(h = 0.38, col = rgb(1,0,0,0.5), lty = 3)	
	    abline(v = 1.8, col = rgb(1,0,0,0.5), lty = 3)
}



#--------------------------------------------------------------------------------
# Time Extending, Time Retracting, Time Stalling



pxw <- pxw  # USER-DEFINED AT TOP OF SCRIPT

threshold.ext.per.t = pxw
threshold.retr.per.t = -pxw

# TipState function. i) Absolute (number of timepoints in each state)

TipState.Abs <- function(x) {
	tip.state <- cut(x,
		breaks = c(-Inf, threshold.retr.per.t, threshold.ext.per.t, Inf),
		labels = c("Retr", "Stall", "Ext"))
	retrun(summary(tip.state))	
}

# TipState function. ii) Relative (proportion of time in each state)

TipState.Rel <- function(x) {
	tip.state <- cut(x,
		breaks = c(-Inf, threshold.retr.per.t, threshold.ext.per.t, Inf), 
		labels = c("Retr", "Stall", "Ext"))	
	return(summary(tip.state)/Count(tip.state))
	}

all.tip.states <- apply(all.fdctm, 2, TipState.Rel)

all.time.ext   <- all.tip.states["Ext", ]
all.time.stall <- all.tip.states["Stall", ]
all.time.retr  <- all.tip.states["Retr", ]




