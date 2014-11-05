#!/usr/bin/env R

source('util.R')
source('agents.R')

######################################################################
## CLASSICAL IBR
##
## 
## m: the initial semantic matrix (0s and 1s only)
## costs: list with indices 1 (listener) and 2 (speaker); default: NULL, which imposes even costs
## prior: prior over worlds (rows in m); default is a uniform distribution
## maxiter: prevents an infinite loop (possible only if there is a bug; default: 100)
## listener.is.first: whether the sequence should begin with the listener (T) or speaker (F); default is T
##
## Value:
##
## A list seq with numeric keys, where the values are strategies.
## length(seq) gives the depth of iteration

IBR = function(m, costs=NULL, prior=UniformDistribution(nrow(m)), maxiter=100,
               argmax_on=TRUE, listener.is.first=FALSE) {
  seq = Iterator(m, costs=costs, prior=prior, argmax=argmax_on, maxiter=maxiter,
                 digits=100, listener.is.first=listener.is.first)
  return(seq)
}

######################################################################

SurprisalIBR = function(m, costs=NULL, prior=UniformDistribution(nrow(m)), maxiter=100) {
  seq = Iterator(m, costs=costs, prior=prior, argmax=TRUE, maxiter=maxiter, digits=100, initial.speaker=SurprisalSpeaker)
  return(seq)
}

######################################################################
## Arguments:
##
## m: initial 1/0 matrix capturing the underlying semantics
## costs: list with indices 1 (listener) and 2 (speaker), regardless of listener.is.first; default: NULL, which imposes even costs
## prior: prior over worlds (rows in m); default is a uniform distribution
## maxiter: prevents an infinite loop (possible only if there is a bug; default: 100)
## digits: number of decimal places to consider when calculating equality of values
## initial.speaker: base agent if !listener.is.first; could be S0 (for classical IBR) or SurprisalSpeaker (for a Frank-Goodman start); ignored if listener.is.first; default: S0
## initial.listener: base agent if listener.is.first; could be Lstarbayes (for R0 IBR); ignored unless listener.is.first; default: Lstarbayes
## listener.is.first: whether the sequence should begin with the listener (T) or speaker (F); default is T
##
## Value:
##
## A list seq with numeric keys, where the values are strategies.
## length(seq) gives the depth of iteration

Iterator = function(m, costs=NULL, prior=UniformDistribution(nrow(m)),
                    argmax=TRUE, maxiter=100, digits=100, listener.is.first=FALSE,
                    initial.speaker=S0, initial.listener=Lstarbayes) {
  ## We will use funcs to access the right functions:
  funcs = list()
  if (listener.is.first) {
    funcs[[1]] = Speaker
    funcs[[2]] = Listener
  }
  else {
    funcs[[1]] = Listener
    funcs[[2]] = Speaker
  }

  costs = CostsForIterator(m, costs, listener.is.first)

  ## Output data structure:
  seq = list()
  ## Get the base agent:
  seq[[1]] = if (listener.is.first) {
               initial.listener(m, prior=prior)
             }
             else {
               initial.speaker(m)
             }
  ## We need at least one more agent:
  seq[[2]] = funcs[[1]](seq[[1]], m, costs=costs[[1]], prior=prior, argmax=argmax)
  ## Now we can iterate:
  i = 3
  while (i <= maxiter) {
    ## R won't allow 0 indices in vectors or list keys, so we add 1 to obtain
    ## the value accessor index used to pick the right function and cost function:
    valueIndex = (i %% 2) + 1
    ## Get the function we need (S or L):
    func = funcs[[valueIndex]]
    ## Get the next strategy:    
    val = func(seq[[i-1]], m, costs=costs[[valueIndex]], prior=prior, argmax=argmax)
    ## If the next strategy is the same as the previous one of this type (2 back)
    ## then we have converged.
    if (MatrixEquality(val, seq[[i-2]], digits=digits)) {
      return(seq)
    }
    ## Where we haven't converged, we add the new strategy and increment the counter:
    else {      
      seq[[i]] = val
      i = i + 1
    }    
  }
  return(seq)
}

## Prepare the costs for Iterator.  Returns a list whose first element is the
## costs of the listener (iff listener.is.first) or speaker (otherwise).
##
## Arguments:
## costs: same as in Iterator
## listener.is.first: same as in Iterator
CostsForIterator = function(m, costs, listener.is.first) {
  result = list()
  ## Uniform costs if no costs are supplied:
  if (is.null(costs)) {
    if (listener.is.first) {
      result[[1]] = UniformCosts(m)    # speaker
      result[[2]] = t(UniformCosts(m)) # listener
    }
    else {
      result[[1]] = t(UniformCosts(m)) # listener
      result[[2]] = UniformCosts(m)    # speaker
    }
  }
  ## Use the provided costs, but swap them if listen.is.first:
  else {
    if (listener.is.first) {
      result[[1]] <- costs[[2]]
      result[[2]] <- costs[[1]]
    }
    else {
      result <- costs
    }
  }
  return(result)
}
