### BounderR Module 3: 

# Plotting of descriptive metrics (across conditions) with basic statistics.

# 3A: Plots box plots and CDFs [and histograms (if N.datasets<3)] for FILOPODIUM STATS: 
	
	# Max Length
	# Median extension rate	:
	# Median retraction rate:
	# Tip persistence	 	: 	(acf.dctm.crosspoints)
	# Initial DCTM (new filo)
	# Initial dB (new filo)
	# Initial DCTM/dB (new filo) [also in progress]
	# Mean waviness
	# also: skewness [in progress]
	# also: duration(ext) vs duration(retr) [in progress]
	
		
# 3B: Timecourse graphs:

	# Length over time (newFilo)
	# DCTM over time (newFilo)
	# dB over time (newFilo)
	
	
	
setwd(Loc.Modules)
source("GraphingTemplates.R")

# par(mfrow = c(2,2))


Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }








#------------------------------------------------------------------------------
# Med. extension rate

dev.new()
par(mfrow = c(2,2))

x1 <- "med.rate.extens"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Median Extension Rate Per Filopodium",              # <---- Remember to edit here!
    curr.Ylab = expression("Median DCTM (> 0) [" * mu * "m / 2 s]")  # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Median Extension Rate (CDF)",
	curr.Xlab = expression("Median DCTM (> 0) [" * mu * "m / 2 s]") )

rm(x1, curr.data)


#------------------------------------------------------------------------------
# Med. retraction rate

x1 <- "med.rate.retract"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Median Retraction Rate Per Filopodium",              # <---- Remember to edit here!
    curr.Ylab = expression("Median DCTM (< 0) [" * mu * "m / 2 s]")   # <---- Remember to edit here!
    )
legend("bottomright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Median Retraction Rate (CDF)",
	curr.Xlab = expression("Median DCTM (< 0) [" * mu * "m / 2 s]"), 
	legend.where = "topleft")
	
rm(x1, curr.data)


#------------------------------------------------------------------------------
# Med base invasion

dev.new()
par(mfrow = c(2,2))

x1 <- "med.dB.invas"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Median Invasion Rate Per Base",              # <---- Remember to edit here!
    curr.Ylab = expression("Median dB (> 0) [" * mu * "m / 2 s]")  # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Median Base Invasion (CDF)",
	curr.Xlab = expression("Median dB (> 0) [" * mu * "m / 2 s]") )

rm(x1, curr.data)


#------------------------------------------------------------------------------
# Med base retraction

x1 <- "med.dB.retract"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Median Retraction Rate Per Base",              # <---- Remember to edit here!
    curr.Ylab = expression("Median dB (< 0) [" * mu * "m / 2 s]")   # <---- Remember to edit here!
    )
legend("bottomright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Median Base Retraction (CDF)",
	curr.Xlab = expression("Median dB (< 0) [" * mu * "m / 2 s]"),
	legend.where = "topleft" )
rm(x1, curr.data)



#------------------------------------------------------------------------------
# Initial DCTM

dev.new()

par(mfrow = c(2,2))

x1 <- "dctm99.new.early.med"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Initial DCTM in nascent filopodia",              # <---- Remember to edit here!
    curr.Ylab =  expression("DCTM (median for t = 1-10)  [" * mu * "m / 2 s]")   # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Initial DCTM in nascent filopodia (CDF)",
	curr.Xlab = expression("DCTM (median for t = 1-10)  [" * mu * "m / 2 s]")  )
rm(x1, curr.data)


#------------------------------------------------------------------------------
# Initial dB

x1 <- "dB99.new.early.med"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Initial Base Movement in nascent filopodia",              # <---- Remember to edit here!
    curr.Ylab =  expression("dB (median for t = 1-10)  [" * mu * "m / 2 s]")   # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Initial dB in nascent filopodia (CDF)",
	curr.Xlab = expression("dB (median for t = 1-10)  [" * mu * "m / 2 s]")  )


rm(x1, curr.data)

#------------------------------------------------------------------------------
# Initial DCTM vs dB (scatter)

x1 <- "dctm99.new.early.med"
y1 <- "dB99.new.early.med"

x1 <- metalist[[1]]$"dctm99.new.early.med"
y1 <- metalist[[1]]$"dB99.new.early.med"

x2 <- metalist[[2]]$"dctm99.new.early.med"
y2 <- metalist[[2]]$"dB99.new.early.med"


# curr.data.X <- StandardGraphInput(x1); curr.data.X
# curr.data.Y <- StandardGraphInput(y1); curr.data.Y

curr.cols$v 
c1 <- c(curr.cols[, 1], 0.5)
c2 <- c(curr.cols[, 2], 0.5)


dev.new()
plot(x1, y1, 
    ylim = c(-0.5,0.5), xlim = c(-0.5,0.5),
    type = "p", pch = 16, 
    col = rgb(c1[1], c1[2], c1[3], 0.5),
    main = "DCTM vs dB per filopodium over initial 10 timepoints",
	xlab = expression("Mean DCTM [" * mu * "m / 2 s]"),
	ylab = expression("Mean dB [" * mu * "m / 2 s]")
    )
    abline(h = 0, col = 'grey80', lty = 2)
    abline(v = 0, col = 'grey80', lty = 2)
points(x2, y2, type = "p", pch = 16, col = rgb(c2[1], c2[2], c2[3], 0.5))


#------------------------------------------------------------------------------
# Max length

dev.new()
par(mfrow = c(2, 2))

x1 <- "max.lengths"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Max Filopodium Lengths",              # <---- Remember to edit here!
    curr.Ylab = expression("Maximum Filopodium Length [" * mu * "m]")  # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Max Filopodium Lengths (CDF)",
	curr.Xlab = expression("Length [" * mu * "m]")  )
	
rm(x1, curr.data)


#------------------------------------------------------------------------------
# Waviness

x1 <- "waviness.mean"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Mean Waviness (per filopodium)",  # <---- Remember to edit here!
    curr.Ylab = "Waviness [au]"                     # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Mean Waviness (CDF)",
	curr.Xlab = "Waviness [au]"  )
	
rm(x1, curr.data)


#------------------------------------------------------------------------------
# ACF DCTM roots


dev.new()
par(mfrow = c(2, 2))


x1 <- "acf.dctm.roots"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Root of DCTM ACF (per filopodium)",              # <---- Remember to edit here!
    curr.Ylab =  expression(Delta * "T [# timepoints]")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Root of DCTM ACF (CDF)",
	curr.Xlab = expression(Delta * "T [# timepoints]") )


#------------------------------------------------------------------------------
# ACF DCTM decay


max.L <- nrow(metalist[[1]]$acf.dctm)  # local variable (only this section)
acf.offset <- 1:max.L	# local

acf.dctm1.means <- apply(metalist[[1]]$acf.dctm, 1, mean, na.rm = TRUE)  # local
acf.dctm2.means <- apply(metalist[[2]]$acf.dctm, 1, mean, na.rm = TRUE)  # local
acf.dctm1.ci <- apply(metalist[[1]]$acf.dctm, 1, CI)  # local
acf.dctm2.ci <- apply(metalist[[2]]$acf.dctm, 1, CI)  # local

n1 <- ncol(metalist[[1]]$acf.dctm)
n2 <- ncol(metalist[[2]]$acf.dctm)

metalist[[1]]$acf.dctm[1:10, 1:10]

# Plot autocorrelation function for DCTM:

	matplot(acf.offset, acf.dctm1.means, 
		ylab = "ACF",
		xlab = "ACF offset (timepoints)",
		main = "Mean autocorrelation of DCTM",
		xlim = c(0, 50),
		type = 'l',
		lwd = 3, 
		col = rgb(t(curr.cols[1]))
		)
	matplot(acf.offset, acf.dctm2.means, 
		add = TRUE,
		type = 'l',
		lwd = 3, 
		col = rgb(t(curr.cols[2]))
		)
		
	# Draw error range for dataset1
	ci.hi1 = acf.dctm1.means + acf.dctm1.ci
	ci.lo1 = acf.dctm1.means - acf.dctm1.ci
	
	ci.hi2 = acf.dctm2.means + acf.dctm2.ci
	ci.lo2 = acf.dctm2.means - acf.dctm2.ci
	
	DrawErrorAsPolygon(acf.offset, ci.hi1, ci.lo1, tt = 1:(max.L-1), 
		col = rgb( t(curr.cols[1]), alpha = 0.2) )
	DrawErrorAsPolygon(acf.offset, ci.hi2, ci.lo2, tt = 1:(max.L-1), 
		col = rgb( t(curr.cols[2]), alpha = 0.2) )

	abline(h = 0, col = "black")
	
	legendstrings = paste(dataset.names, " (n = ", c(n1, n2), ")", sep = "")
	
	legend("topright", 
		legend = legendstrings,
		cex = 1,
		bty = "n",
		lwd = 3,
		col = rgb(t(curr.cols) )	
		)

rm(x1, curr.data, n1, n2, acf.offset, max.L, acf.dctm1.means, acf.dctm2.means, acf.dctm1.ci, acf.dctm2.ci, ci.hi1, ci.lo1, ci.hi2, ci.lo2)


#------------------------------------------------------------------------------
# Time spent extending

dev.new()
par(mfrow = c(3, 2))

x1 <- "all.time.ext"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time tip extending",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where tip speed > 0.065 " * mu * "m / s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Time tip extending (CDF)",
	curr.Xlab = expression("Time where tip speed > 0.065 " * mu * "m / 2s")
	)

#------------------------------------------------------------------------------
# Time spent retracting

x1 <- "all.time.retr"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time tip retracting",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where tip speed < -0.065 " * mu * "m / 2s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Time tip retracting (CDF)",
	curr.Xlab = expression("Time where tip speed >0.065 " * mu * "m / 2s")
	)

 #------------------------------------------------------------------------------
# Time spent stalling

x1 <- "all.time.stall"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time tip stalling",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where abs(FDCTM) < 0.065 " * mu * "m / 2s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Time tip stalling (CDF)",
	curr.Xlab = expression("Time where abs(FDCTM) < 0.065 " * mu * "m / 2s")
	)



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Module 3B: Timecourse graphs


names(metalist[[1]])
names(metalist)

# For condition 1:
 
new <- metalist[[1]]$new
metalist[[1]]$all.length[, new]

new.lengths <- metalist[[1]]$all.length[, new]
new.dctm    <- metalist[[1]]$all.dctm99[, new]
new.dB    <- metalist[[1]]$all.dB99[, new]


new.dS      <- metalist[[1]]$all.dS[, new]
dS.vector   <- metalist[[1]]$dS.vector

mean.new.lengths <- apply(new.lengths, 1, mean, na.rm = TRUE)
mean.new.dctm    <- apply(new.dctm, 1, mean, na.rm = TRUE)
mean.new.dB 	 <- apply(new.dB, 1, mean, na.rm = TRUE)

median.new.lengths <- apply(new.lengths, 1, median, na.rm = TRUE) 
median.new.dctm    <- apply(new.dctm, 1, median, na.rm = TRUE)
median.new.dB    <- apply(new.dB, 1, median, na.rm = TRUE)

ci.new.lengths    <-  apply(new.lengths, 1, CI) 
ci.new.dctm       <-  apply(new.dctm, 1, CI) 
ci.new.dB         <-  apply(new.dctm, 1, CI) 


# problem: some of those newly in existence start off very long already!
from.short <- which(new.lengths[21, ] < 2)

# reassign the above accordingly: 

mean.new.lengths <- apply(new.lengths[, from.short], 1, mean, na.rm = TRUE)
mean.new.dctm    <- apply(new.dctm[, from.short], 1, mean, na.rm = TRUE)
mean.new.dB    <- apply(new.dB[, from.short], 1, mean, na.rm = TRUE)

ci.new.lengths    <-  apply(new.lengths[, from.short], 1, CI) 
ci.new.dctm       <-  apply(new.dctm[, from.short], 1, CI) 
ci.new.dB       <-  apply(new.dB[, from.short], 1, CI) 

#------------------------------------------------------------------------------
# 3B.i --- Plotting LENGTHS timecourse:

dev.new()
par(mfrow = c(2,2))
matplot(new.dS[, from.short], new.lengths[, from.short],
	type = "l",
	lty = 1,
	col = "#00CCCC10",
	main = "Filopodium length over time",
	xlab = "Time [s]",
	ylab = expression("Length [" * mu * "m]"),
	xlim = c(0, 150),
	ylim = c(0, 10)
)
lines(dS.vector, mean.new.lengths,
	lwd = 4,
	col = "#00CCCC")	

ci.hi = mean.new.lengths + ci.new.lengths
ci.lo =	mean.new.lengths - ci.new.lengths

DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, tt = 1:120, col = "#00CCCC40")

abline(h = 0, col = "black", lty = 3)

# head(new.lengths[20:26, ])


# 3B.ii --- Plotting DCTM timecourse:


matplot(new.dS[, from.short], new.dctm[, from.short],
	type = "l",
	lty = 1,
	col = "#66FF6610",
	main = "Tip movement over time",
	xlab = "Time [s]",
	ylab = expression("DCTM [" * mu * "m / 2 s]"),
	xlim = c(0, 60),
	ylim = c(-0.3, 0.3)
)
lines(dS.vector, mean.new.dctm,
	lwd = 4,
	col = "#66FF66" )
ci.hi = mean.new.dctm + ci.new.dctm
ci.lo =	mean.new.dctm - ci.new.dctm
	

DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, tt = 1:120, col = "#66FF6640")

abline(h = 0, col = "black", lty = 3)


# 3B.iii --- Plotting DB timecourse:


matplot(new.dS[, from.short], new.dB[, from.short],
#	add = TRUE,
	type = "l",
	lty = 1,
	col = "#FFB26610",
	main = "Base movement over time",
	xlab = "Time [s]",
	ylab = expression("DB [" * mu * "m / 2 s]"),
	xlim = c(0, 60),
	ylim = c(-0.8, 0.8)
)
lines(dS.vector, mean.new.dB,
	lwd = 4,
	col = "#FFB266" )
ci.hi = mean.new.dB + ci.new.dB
ci.lo =	mean.new.dB - ci.new.dB
	
	
DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, tt = 1:120, col = "#FFB26640")

abline(h = 0, col = "black", lty = 3)


