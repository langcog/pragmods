library(rrrsa)
source("agents.R")
source("helper.R")
source("util.R")
source("viz_img.R")

## Stiller scales
simple = matrix(
  c(0,   0,
    0,   1,
    1,   1), byrow=T, nrow=3,
  dimname=list(
    c('foil', 'target', 'logical'), # Row names; referents.
    c('hat', 'glasses'))) # Column names; messages.

simple_normalized <- t(L0(simple))
rsa.reason(simple_normalized, depth = 0, alpha = 1)
rsa.reason(simple_normalized, depth = 1, alpha = 1)
rsa.reason(simple_normalized, depth = 1, alpha = 2)

LSL(simple)

rsa.reason(simple_normalized, depth = 2, alpha = 1)

LSLSL(simple)


## For experiment levels-levels
complex = matrix(
  c(0,   0,   1,
    0,   1,   1,
    1,   1,   0), byrow=T, nrow=3,
  dimname=list(
    c('foil', 'target', 'logical'), # Row names; referents.
    c('hat', 'glasses', 'mustache'))) # Column names; messages.


# Depth 2 separation for levels-levels demo
d2 = matrix(
  c(0,   0, 1,    1,  
    0,  1, 1,     1, 
    1, 1, 0,      0 ), byrow=FALSE, nrow=4,
  dimnames=list(paste("t", seq(1,4),sep=''), ## Row names; refernets.
                c('hat', 'glasses', 'mustache'))) ## Column names; messages.

L(S(L(d2)))
L(S(L(S(L(d2)))))

pdf("../plots/depth2.pdf", width=7.5, height = 2)
ImageViz(d2)
dev.off()
