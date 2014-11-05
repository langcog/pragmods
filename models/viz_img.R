
## Used for displaying the raw matrix:
library(plotrix)
library(png)

######################################################################
## Visualize a matrix (any number of rows; at most three columns)
## in terms of smiley faces based on pictures.

ImageViz = function(m, print.matrix=FALSE, stim="face", bws=rep(F,nrow(m))) {
  ## Error checking:
  if (ncol(m) > 4) {
    stop(paste("Apologies: I can't handle more than 4 columns/properties"))
  }
  ## Parameters for the plotting:
  plot.width = nrow(m)*2
  panels = nrow(m)
  plot.height = 2
  if (print.matrix) {
    plot.width = plot.width + 2
    panels = panels + 1
  }
  ## Plot the smileys:
#   dev.new(width=plot.width, height=plot.height)
  par(mfrow=c(1,panels), oma=c(0,0,0,0), mar=c(0,0,0,0))
  for (i in 1:nrow(m)) {
    StimImg(m[i, ],stim,bw=bws[i])
  }
  ## Add a final column for the matrix if requested:
  if (print.matrix) {
    ## Blank plot window:
    plot(c(0,20), c(0,1), type='n', xlab='', ylab='', axes=F)
    ## Add the matrix:
    addtable2plot(0, 0.5, m, display.rownames=TRUE, bty='o', cex=0.8)
  }  
	
}

######################################################################
## Generate stimulus images based on a set of binary values:
##
## vals: a binary vector of length 0-4. For each coordinate, 0 means
## absence of the property, and 1 means presence.

StimImg = function(vals=c(0,0,0,0), stim="face", bw=F) {			
  ## Basic locating parameters:
  xval = 0
  yval = 0
  xlim = c(1,2)
  ylim = c(1,2)  
  
  ## Make sure we have a 4-membered value list:
  shortage = 4 - length(vals)
  if (shortage > 0) {
    vals = c(vals, rep(0, shortage))
  }
  
  ## load features
  if (stim == "face") {
  	base <- readPNG("stim/face.png")
  	feature.names <- c("hat","glasses","mustache","tie")
  } else if  (stim=="snowman") {
  	base <- readPNG("stim/snowman.png")
  	feature.names <- c("beanie","scarf","gloves","belt")
  } else if (stim == "sundae") {
  	base <- readPNG("stim/sundae.png")
  	feature.names <- c("cherry","cream","chocolate","banana")
  } else {
  	stop("no stim of that name")
  }  

  features <- list()
  
  for (i in 1:length(feature.names)) {
  	features[[i]] <- readPNG(paste("stim/",feature.names[i],".png",sep=""))
  }
  
  ## Start an empty plot window with the right dimensions:
  plot(xlim, ylim, type='n', xlab='', ylab='', axes=F)  
  
  if (bw==T) { # if we are plotting this guy in BW
    base.bw <- base
    base.m <- apply( base, c(1,2), mean )
    for (i in 1:3) {
      base.bw[,,i] <- base.m
    }
    base <- base.bw
  }
  rasterImage(base,1,1,2,2)
  
  for (i in 1:length(vals)) {
  	if (vals[i]==1) {
      		rasterImage(features[[i]],1,1,2,2)
	}
  }  
}

######################################################################
## Make a matrix of 2xN images

MatrixViz = function(m, print.matrix=FALSE, stim="face", panels=c(2,5)) {
  
  ## Parameters for the plotting:
  plot.width = nrow(m)*2
  plot.height = 2

  ## Plot the smileys:
  par(mfcol=panels, oma=c(0,0,0,0), mar=c(0,0,0,0))
  for (i in 1:nrow(m)) {
    StimImg(m[i, ],stim)
  }  
}

