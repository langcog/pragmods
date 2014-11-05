#!/usr/bin/env R

source('util.R')

######################################################################
## SPEAKERS

##################################################
## This function is used to take a set-theoretic
## matrix to one that is probabilistic. It is used
## only to initiate play.
##
## Argument:
## m: a 2d matrix
##
## Value:
## a matrix with the same dimensions as m in which
## rows have been turned into probability distributions

S0 = function(m) {
  ## Preserve these in case they get lost in processing:
  row.names = rownames(m)
  col.names = colnames(m)
  ## Note: apply with 1 as the second argument remaps the
  ## row values, but it also tranposes the matrix. Here,
  ## we use t() to transpose back, for mental hygiene.
  m = t(apply(m, 1, VecNormalize))
  ## Make sure we still have the row and column names:
  rownames(m) = row.names
  colnames(m) = col.names
  return(m)
}

##################################################
## A speaker who
## 
## * incorporates no prior. (prior is a keyword
## argument to ensure compatibility with the other
## functions; if a value is supplied, it is NOT
## passed to Speaker
##
## * does not argmax
##
## For additional details on the arguments, see Speaker

S = function(m, sem=m, costs=NULL, prior=NULL, resort.to.uniform=TRUE) {
  m = Speaker(m, sem=sem, costs=costs, prior=NULL, argmax=FALSE, resort.to.uniform=resort.to.uniform)
  return(m)
}

##################################################
## A speaker who
## 
## * incorporates no prior. (prior is a keyword
## argument to ensure compatibility with the other
## functions; if a value is supplied, it is NOT
## passed to Speaker
##
## * does argmax
##
## For additional details on the arguments, see Speaker

Sstar = function(m, sem=m, costs=NULL, prior=NULL, resort.to.uniform=TRUE) {
  m = Speaker(m, sem=sem, costs=costs, prior=NULL, argmax=TRUE, resort.to.uniform=resort.to.uniform)
  return(m)    
}

##################################################
## General speaker function:
##
## Arguments:
## m: a 2d matrix
## sem: the underlying semantic matrix (not used, but given as an argument
## so that this function as the same signature as the Listner ones (default: m)
## costs: a matrix with the same dimension as m giving the costs (default: no costs)
## prior: not used; here only to ensure that Speaker and Listener functions have the same signature
## argmax: whether to always take the maximum rows values (default: TRUE)
##
## Value
## A matrix with the same dimensions as m.

Speaker = function(m, sem=m, costs=NULL, prior=NULL, argmax=TRUE, resort.to.uniform=TRUE) {
  m = t(m)
  ## Preserve these in case they get lost in processing:
  row.names = rownames(m)
  col.names = colnames(m)
  ## All 0s cost function if none was supplied:
  if (is.null(costs)) {
    costs = UniformCosts(m)
  }
  
  ## Replace any all-0 rows with uniform distributions:
  if (resort.to.uniform) {
    m = t(apply(m, 1, ZerosVector2UniformDistibution))
  } 
  else {
    m = t(apply(m, 1, ZerosVector2ZerosVector))
  }    
    
  ## Subtract costs:
  m = m - costs
  ## Maximize if requested:
  if (argmax) {
    m = t(apply(m, 1, VecMax))
  }
  ## Normalize:
  m = t(apply(m, 1, VecNormalize))
  ## Make sure we still have the row and column names:
  rownames(m) = row.names
  colnames(m) = col.names
  return(m)
}

######################################################################
## LISTENERS

##################################################
## A speaker who
## 
## * incorporates no prior. (prior is a keyword
## argument to ensure compatibility with the other
## functions; if a value is supplied, it is NOT
## passed to Listener)
##
## * does not argmax
##
## For additional details on the arguments, see Listener

L = function(m, sem, costs=NULL, prior=NULL) {
  m = Listener(m, sem, costs=costs, prior=UniformDistribution(nrow(m)), argmax=FALSE)
  return(m)
}

##################################################
## A speaker who
## 
## * incorporates a prior (default is uniform)
## * does not argmax
##
## For additional details on the arguments, see Listener

Lbayes = function(m, sem, costs=NULL, prior=UniformDistribution(nrow(m))) {
  m = Listener(m, sem, costs=costs, prior=prior, argmax=FALSE)
  return(m)
} 

##################################################
## A speaker who
## 
## * incorporates no prior. (prior is a keyword
## argument to ensure compatibility with the other
## functions; if a value is supplied, it is NOT
## passed to Listener)
## * does argmax
##
## For additional details on the arguments, see Listener

Lstar = function(m, sem, costs=NULL, prior=NULL) {
  m = Listener(m, sem, costs=costs, prior=UniformDistribution(nrow(m)), argmax=TRUE)
  return(m)
} 

##################################################
## A speaker who
## 
## * incorporates a prior (default is uniform)
## * does argmax
##
## For additional details on the arguments, see Listener.

Lstarbayes = function(m, sem, costs=NULL, prior=UniformDistribution(nrow(m))) {
  m = Listener(m, sem, costs=costs, prior=prior, argmax=TRUE)
  return(m)
}

##################################################
## General listener function:
##
## Arguments:
## m: a 2d matrix
## sem: the underlying semantic matrix, required for cases where a surprise row is encountered
## costs: a matrix with the same dimension as m giving the costs (default: no costs)
## prior: the prior to impose over worlds (columns in m; default: an even prior)
## argmax: whether to zero-out non-maximal row values (default: TRUE)
##
## Value:
## A matrix with the same dimensions as m.

Listener = function(m, sem, costs=NULL, prior=UniformDistribution(nrow(m)), argmax=TRUE) {  
  ## Transpose:
  m = t(m)
  ## Preserve these in case they get lost in processing:
  row.names = rownames(m)
  col.names = colnames(m)  
  ## All 0s cost function if none was supplied:
  if (is.null(costs)) {
    costs = UniformCosts(m)
  }
  ## Replace any surprise rows with the underlying semantics.
  ## We impose costs iff we do not do the all-0s row replacement!
  for (i in 1:nrow(m)) {
    ## Replacement in the all 0s case:
    if (ZerosVector(m[i, ])) {
      m[i, ] = t(sem)[i, ]
    }
    ## Impose costs otherwise:
    else {
      m[i, ] = m[i, ] - costs[i, ]
    }    
  }  
  ## Impose the prior and renormalize:
  normalizer = function(row){ (row*prior) / sum(row*prior) }
  m = t(apply(m, 1, normalizer))  
  ## Maximize:
  if (argmax) {
    m = t(apply(m, 1, VecMax))
  }
  ## Normalize again:
  m = t(apply(m, 1, VecNormalize))
  ## Make sure we stil have the row and column names:
  rownames(m) = row.names
  colnames(m) = col.names
  return(m)
}

######################################################################
## This is a Frank-Goodman speaker, using surprisals:
##
## Argument:
## m: a truth-conditional (i.e., binary) matrix, referents/worlds as rows and messages as columns
##
## Value:
##
## the new speaker matrix with the same dimensions as m where
##
##   M[r_i, m_j] = P(w_i|r_j) = (1/|w_i|) / sum_{w' in W} 1/|w'|)
##
## where |w| is the semantic interpretation of w
## and W is the set of all words true of r_i.

SurprisalSpeaker = function(m) {
  ## Inverse column sums give the word interpretations:
  interpret = 1 / apply(m, 2, sum)
  ## Speaker choices:
  produce = apply(m, 1, function(row){ sum(row * interpret) })
  ## Function to apply to each row:
  func = function(row) {
    ## Speaker choices:
    produce = sum(row * interpret)
    if (produce > 0) {
      val = (interpret/produce)
    }
    else {
      val = rep(0, ncol(m))
    }
    ## Multiply by the binary vector to ensure truth-functionality:
    val = val * row
    return(val)
  }
  m = t(apply(m, 1, func))
  return(m)    
}

######################################################################
## Separate implementation of the Frank-Goodman model, which is
## equivalent to Lbayes(SurprisalSpeaker(mat)).

FG = function(m, prior=UniformDistribution(nrow(m))) {
  ## Get the model posterior:
  m = SurprisalSpeaker(m)
  ## Incorporate the prior and transpose:
  m = t(m * prior)
  ## Normalize:
  m = t(apply(m, 1, function(row){row/sum(row)}))
  ## Return the tranpose to match with the IBR norms:
  return(m)
}
  
######################################################################
## Assorted wrappers of agents for simulation
##
## Note that resort.to.uniform = FALSE allows for null rows in Experiment 0

#c("L0","LS","LSL","LSLS","LSLSL","LSLSLS")

L0 <- function(m,prior=UniformDistribution(nrow(m))) {
  Lbayes(m,prior=prior)
}

LS <- function(m,prior=UniformDistribution(nrow(m))) {
  Lbayes(S0(m),
         m,prior=prior)
}

LSL <- function(m,prior=UniformDistribution(nrow(m))) {
  Lbayes(S(Lbayes(m,prior=prior),
           m,resort.to.uniform=FALSE),
         m,prior=prior)
}

LSLS <- function(m,prior=UniformDistribution(nrow(m))) {
  Lbayes(S(Lbayes(S0(m),
                  m,prior=prior),
           m,resort.to.uniform=FALSE),
         m,prior=prior)
}

LSLSL <- function(m,prior=UniformDistribution(nrow(m))) {
  Lbayes(S(Lbayes(S(Lbayes(m,prior=prior)),
                  m,prior=prior),
           m,resort.to.uniform=FALSE),
         m,prior=prior)
}

LSLSLS <- function(m,prior=UniformDistribution(nrow(m))) {
  Lbayes(S(Lbayes(S(Lbayes(S0(m),
                           m,prior=prior),
                    m,resort.to.uniform=FALSE),
                  m,prior=prior),
           m,resort.to.uniform=FALSE),
         m,prior=prior)
}

LSLSLSLS <- function(m,prior=UniformDistribution(nrow(m))) {
  Lbayes(S(Lbayes(S(Lbayes(S(Lbayes(S0(m),
                                    m,prior=prior),
                             m,resort.to.uniform=FALSE),
                           m,prior=prior),
                    m,resort.to.uniform=FALSE),
                  m,prior=prior),
           m,resort.to.uniform=FALSE),  
           m,prior=prior)
}
