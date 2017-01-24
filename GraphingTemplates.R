# GRAPHING TEMPLATES:

# ------------------------------------------------------------------------------

# Two (or more) boxplots (with jitter)

	# applied to:
	# Max lengths
	# Median extension rate	:
	# Median retraction rate:
	# Growth consistency 	: 	(acf.dctm.crosspoints)
	# Initial DCTM (new filo)
	# Initial dB (new filo)
	# Initial DCTM/dB (new filo)
	# Mean straightness

# Plain histogram 
# Histogram with horizontal boxplot:
# Two overlapping histograms (with transparency)
# CDFs 
# Scatterplots

# ------------------------------------------------------------------------------
# Import colour schemes (internal)

setwd(Loc.Modules)
source("ColourSchemes.R")
print(colourscheme.names)


# ------------------------------------------------------------------------------

# TWO BOXPLOTS (with jitter)

	# applied to:
	# Max lengths
	# Median extension rate	:
	# Median retraction rate:
	# Growth consistency 	: 	(acf.dctm.crosspoints)
	# Initial DCTM (new filo)
	# Initial dB (new filo)
	# Initial DCTM/dB (new filo)
	# Mean straightness



# 1. Standardise data input for graphs:


# Choose among colourscheme.names (from ColourSchemes.R)
curr.cols = triad1  # used for manual-v-batch
curr.cols = triad1[, 3:2]  # used for huang-renyi
curr.cols


print(noquote("Importing graphing functions..."))

StandardGraphInput <- function(x) {
    
    # 1. Extract data from metalist
    x = as.character(x)
    parameter.to.plot <- which(objectnames == x)
#   parameter.to.plot <- which(objectnames[[1]] == x)
    data1 = metalist[[1]][parameter.to.plot] 
    data2 = metalist[[2]][parameter.to.plot] 
    
    # 2. Organise into table for plotting (curr.data)
    n1 = length(unlist(data1)) 
    n2 = length(unlist(data2))
    
    curr.data = data.frame(
        "Value" = c(unlist(data1), unlist(data2)), 
        "Source" = c(rep(dataset.names[1], n1), rep(dataset.names[2], n2)))
        # fix alphabetical ordering of X axis, to actual order of dataset names:
        curr.data$Source <- factor(curr.data$Source, levels = dataset.names) 
    
    return(curr.data)
} 

StandardGraphInput <- function(x) {
    
    # 1. Extract data from metalist
    x = as.character(x)
#    parameter.to.plot <- which(objectnames == x)
   parameter.to.plot <- which(objectnames[[1]] == x)
    data1 = metalist[[1]][parameter.to.plot] 
    data2 = metalist[[2]][parameter.to.plot] 
    
    # 2. Organise into table for plotting (curr.data)
    n1 = length(unlist(data1)) 
    n2 = length(unlist(data2))
    
    curr.data = data.frame(
        "Value" = c(unlist(data1), unlist(data2)), 
        "Source" = c(rep(dataset.names[1], n1), rep(dataset.names[2], n2)))
        # fix alphabetical ordering of X axis, to actual order of dataset names:
        curr.data$Source <- factor(curr.data$Source, levels = dataset.names) 
    
    return(curr.data)
}


objectnames

# 2. Quick stats - returns p value for t-test (if large and normally distributed sample) or mann-whitney (otherwise)
#    function call: QuickStats("waviness.mean")

QuickStats <- function(x) {
 
  # 1. Extract data from metalist
    x = as.character(x)
    parameter.to.plot <- which(objectnames[[1]] == x)
    data1 = metalist[[1]][parameter.to.plot] 
    data2 = metalist[[2]][parameter.to.plot] 
    n1 = length(unlist(data1)) 
    n2 = length(unlist(data2))
      
  # 2. Large sample size?
	n.over.30 <- (n1 > 30) & (n2 < 30)
	
  # 3. For big samples:
    if (n.over.30 == TRUE) {
        
       # Test for normal distribution:
        shapiro1 <- shapiro.test(unlist(data1))$p.value
        shapiro2 <- shapiro.test(unlist(data2))$p.value
        appr.normal <- (shapiro1 > 0.1) & (shapiro2 > 0.1)
        
        # Conduct t-test if distr roughly normal
        if (appr.normal == TRUE) {
            t <- t.test(unlist(data1), unlist(data2))$p.value
            t2 <- signif(t, 2)
            z <- c("T-test", paste("p = ", t))
        } 
    
  # For small samples, and large but non-normally distributed samples       
    } else {
        mw <- wilcox.test(Value ~ Source, data = curr.data)$p.value
        mw2 <- signif(mw, 2)  # significant digits
        z <- c("Mann-Whitney", paste("p = ", mw2))
    }  
    return(z)
}



# 2. Create boxplot

Boxplot2 <- function(x, curr.title, curr.Ylab, col = rgb(t(curr.cols))) {

    #dev.new()
    		ylo = min(curr.data$Value, na.rm = TRUE)
            yhi = 1.1 * max(curr.data$Value, na.rm = TRUE)
        boxplot(Value ~ Source, data = curr.data,
            outpch = NA,
            col = col,
            main = curr.title,
            ylab = curr.Ylab,
            ylim = c(ylo, yhi)
            )          
        stripchart(Value ~ Source, data = curr.data,
            add = TRUE,
            vertical = TRUE,
            method = "jitter",
            pch = 4, cex = 0.5,
            col = rgb(0,0,0,0.4)   
            ) 
                       
}

print(noquote("Thank you. Graphing functions imported."))



# ------------------------------------------------------------------------------
# CDFs 

# same examples as boxplot above:

CdfPlot2 <- function(x, curr.title, curr.Xlab, legend.where = "bottomright") {
	
	# Extract data
    x = as.character(x)
   parameter.to.plot <- which(objectnames[[1]] == x)
#    parameter.to.plot <- which(objectnames == x)
    data1 = unlist(metalist[[1]][parameter.to.plot])
    data2 = unlist(metalist[[2]][parameter.to.plot])
    n1 =  length(unlist(data1)) 
    n2 = length(unlist(data2))
	
	# dev.new()
		hilo = range(c(data1, data2), na.rm = TRUE)
		legendstrings = paste(dataset.names, " (n = ", c(n1, n2), ")", sep = "")
		
	plot(ecdf(data1), 
		main = curr.title,
		ylab = "Cumulative distribution",
		xlab = curr.Xlab,
		xlim = hilo,
		cex = 0,
		lwd = 3,
		verticals = TRUE,
		col = rgb(t(curr.cols))[1] 
		)
	plot(ecdf(data2), add = TRUE, 
		cex = 0,
		lwd = 3,
		verticals = TRUE,
		col = rgb(t(curr.cols))[2]
		)
	legend(legend.where, 
		legend = legendstrings,
		cex = 1,
		bty = "n",
		lwd = 3,
		col = rgb(t(curr.cols))
	)	
}
	
# example: 
# CdfPlot2("max.lengths", "Max Filopodium Length (CDF)", expression ("Length [" * mu * "m]"))


# ------------------------------------------------------------------------------

# Big ACF Plot:


Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }



# ------------------------------------------------------------------------------
# TODO(vasja) Two or more boxplots (without jitter)

# Plain histogram: 

# Histogram with horizontal boxplot: (develop this with dctms(total))

# Two overlapping histograms (with transparency)


# data1 = y1
# data2 = y2



# ------------------------------------------------------------------------------
# Correlation scatterplots