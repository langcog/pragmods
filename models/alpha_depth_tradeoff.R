library(rrrsa)
source("agents.R")
source("helper.R")
source("util.R")

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

