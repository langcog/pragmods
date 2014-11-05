
## Used for displaying the raw matrix:
library(plotrix)

######################################################################
## Visualize a matrix (any number of rows; at most three columns)
## in terms of smiley faces with properties: hat, glasses, mustache.
## Value is a 1x3 plot window with specified dimensions so that
## everything lands where it should.

MatrixViz = function(m, print.matrix=FALSE) {
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
  dev.new(width=plot.width, height=plot.height)
  par(mfrow=c(1,panels), oma=c(0,0,0,0), mar=c(0,0,0,0))
  for (i in 1:nrow(m)) {
    Smiley(m[i, ])
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
## Generate smiley images based on a set of binary values:
##
## vals: a binary vector of length 0-3. For each coordinate, 0 means
## absence of the property, and 1 means presence.
##
## vals[1]: add a hat
## vals[2]: add glasses
## vals[3]: add a mustache
## vals[4]: add hair

Smiley = function(vals=c(0,0,0,0)) {
  ## Basic locating parameters:
  xval = 0
  yval = 0
  xlim = c(-1.75,1.75)
  ylim = c(-1.75,1.75)  
  ## Make sure we have a 4-membered value list:
  shortage = 4 - length(vals)
  if (shortage > 0) {
    vals = c(vals, rep(0, shortage))
  }
  ## Properties:  
  hat = vals[1]
  glasses = vals[2]
  mustache = vals[3]
  hair = vals[4]
  ## Start an empty plot window with the right dimensions:
  plot(xlim, ylim, type='n', xlab='', ylab='', axes=F)  
  ## Head:
  symbols(xval, yval, circles=1, bg='yellow', inches=FALSE, add=TRUE)
  ## Eyes:
  eye.adj = 0.3
  eye.size = 0.1
  eye.xvals = c(xval-eye.adj , xval+eye.adj )
  eye.yvals =  c(yval+eye.adj, yval+eye.adj )
  symbols(eye.xvals, eye.yvals, circles=c(eye.size,eye.size), bg='black', inches=F, add=TRUE)
  ## Nose:
  nose.adj = 0.2
  nose.size = 0.1
  symbols(xval, yval-nose.adj, nose.size, bg='black', inches=F, add=TRUE)
  ## Mouth (as a parabola):
  mouth.lim = c(xval-0.5, xval+0.5)
  curve((xval-0.75) + x^2, add=TRUE, lwd=10, from=mouth.lim[1], to=mouth.lim[2])
  ## Used for hair and mustache:
  hair.col = 'darkgoldenrod4'
  ## Hair should go on before hat!
  if (hair == 1) {    
    hair.y = yval+0.65
    for (xjit in seq(40,20, by=-1)) {    
      hx = rep(xval, 2000)
      hy = rep(hair.y, 2000)
      points(jitter(hx,factor=xjit), jitter(hy, factor=3), pch=19, col=hair.col)
      hair.y = hair.y + 0.02
    }
    
  }    
  ## Hat (two rectangles):
  if (hat == 1) {
    hat.col = 'black'
    hat.levels = c(yval+1, yval+1.5)
    hat.matrix = matrix(
      c(1.75, 1, ## Broad lower level.
        0.5, 0.5), ## Smaller upper level.
      nrow=2)    
    symbols(c(xval, xval), hat.levels, rectangles=hat.matrix, bg=hat.col, inches=FALSE, add=TRUE)
  }
  ## Glasses:
  if (glasses == 1) {
    glasses.col = 'gray30'
    glasses.lwd = 5
    segments(eye.xvals[1], eye.yvals[1], eye.xvals[2], eye.yvals[2], lwd=glasses.lwd, col=glasses.col)
    ear.xvals = c(xval-1, xval+1)
    ear.yvals = c(yval+0.45, yval+0.45)
    ## Add the earpieces and stem first:
    segments(eye.xvals[1],eye.yvals[1], ear.xvals[1], ear.yvals[1], lwd=glasses.lwd, col=glasses.col)
    segments(eye.xvals[2],eye.yvals[2], ear.xvals[2], ear.yvals[2], lwd=glasses.lwd, col=glasses.col)
    ## Use the same eye parameters as above for the frames/lenses:
    symbols(eye.xvals, eye.yvals, circles=c(0.2, 0.2), fg=glasses.col, bg=glasses.col, inches=F, add=TRUE)
  }
  ## Mustache (currently somewhat random in appearance due to use of jitter; we might want to fix it):
  if (mustache == 1) {
    gx = rep(xval, 500)
    gy = rep(yval-0.45, 500)   
    points(jitter(gx,factor=14), jitter(gy, factor=3), pch=19, col=hair.col)
  }
      
}

######################################################################
## An experimental function for visualizing a matrix (a Frank-Goodman
## one as given by fg in tests.R).  m is the matrix and display.matrix=TRUE
## (the default) includes a picture of the actual matrix for reference.

FrankGoodmanViz = function(m, display.matrix=TRUE) {  
  ## Function for visualizing a row vector (object with properties):
  VisualizeObject = function(vec, x, y=0.5) {
    if (vec[1] == 1) {
      col = 'blue'
    }
    if (vec[2] == 1) {
      col = 'green'
    }
    if (vec[3] == 1) {
      symbols(x, y=y, square=0.8, bg=col, add=TRUE, inches=FALSE) 
    }
    if (vec[4] == 1) {
       symbols(x, y=y, circles=0.4, bg=col, add=TRUE, inches=FALSE) 
    }
  }
  ## Blank plot window:
  plot(c(0,1), c(0,1), xlim=c(0.5,3.5), ylim=c(0,1), axes=FALSE, type='n', xlab='', ylab='', main='')  
  for (i in 1:nrow(m)) {
    VisualizeObject(m[i, ], i)
  }
  if (display.matrix) {   
    ## Location parameters:
    textx = 0.5
    texty = 1
    xjust = 0
    ## Add the matrix:
    addtable2plot(textx, texty, xjust=xjust, yjust=0, m, display.rownames=TRUE, bty='o')
  }
}
