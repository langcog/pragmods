#!/usr/bin/env R

## Utilities for the other functions

######################################################################
## Turn a vector into a probability distribution, but return an even
## distribution if the row contains no positive elements:
##
## Argument:
## row: a numeric vector
## Value:
## a numeric vector of the same length as row

VecNormalize = function(row){
  if (sum(row) > 0) {
    return(row/sum(row))
  }
  else {
    return(rep(0, length(row)))
  }
}

######################################################################
## Turn a matrix into a row-wise probability distribution
##
## Argument:
## a numeric matrix
## Value:
## a numeric matrix of the same size as the input.

Rownorm = function(m, normalizer=VecNormalize) {
  t(apply(m, 1, normalizer))
}

######################################################################
## Identify the max element in a row and zero out all non-maximal ones
##
## Argument:
## row: a numeric vector
## Value:
## a numeric vector of the same length as row

VecMax = function(row){  
  row = sapply(row, function(x){ifelse(x==max(row), x, 0)})
  return(row)
}

######################################################################
## Test whether two matrices are equivalent up to the level of
## precision given by digits.
##
## Arguments
## m1, m2: 2d matrices
## digits: the level of precision at which to compare the matrices (default: 20)

MatrixEquality = function(m1, m2, digits=100) {
  m1 = round(m1, digits)
  m2 = round(m2, digits)
  ## Reduce the matrices to vectors of booleans:
  cmp = unique(as.numeric(m1) == as.numeric(m2))
  ## If cmp contains FALSE, then the whole thing is false:
  if (FALSE %in% cmp)
    return(FALSE)
  else{   
    return(TRUE)
  }
}

######################################################################
## Determine whether the vector x contains all and only 0s.
##
## Argument:
## x: a numeric vector
## Value:
## boolean: TRUE if x contains only 0s, else FALSE

ZerosVector = function(x) {
  rowset = unique(as.numeric(x))
  ## Check for all zeros:
  if (length(rowset) == 1 & rowset[1] == 0) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

######################################################################
## Determine whether the vector x contains all and only 1s.
##
## Argument:
## x: a numeric vector
## Value:
## boolean: TRUE if x contains only 1s, else FALSE

UnitVector = function(x) {
  rowset = unique(as.numeric(x))
  ## Check for all 1s:
  if (length(rowset) == 1 & rowset[1] == 1) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

######################################################################
## Determine whether a matric contains a Universal (Unit) vector:

ContainsUniversalRow = function(m) {
  ContainsUniversalVector(m, 1)  
}

ContainsUniversalCol = function(m) {
  ContainsUniversalVector(m, 2)  
}

ContainsUniversalVector = function(m, dir) {
  return(ContainsVectorType(m, dir, UnitVector))
}

ContainsZerosRow = function(m) {
  return(ContainsZerosVector(m, 1))
}

ContainsZerosCol = function(m) {
  return(ContainsZerosVector(m, 2))
}

ContainsZerosVector = function(m, dir) {
  return(ContainsVectorType(m, dir, ZerosVector))
}

ContainsVectorType = function(m, dir, func) {
  colvals = apply(m, dir, func)
  if (TRUE %in% colvals) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
######################################################################
## Determine whether the matrix m contains a row of all 0s.

ContainsZerosRow = function(m) {
  vals = apply(m, 1, ZerosVector)
  if (TRUE %in% vals) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

######################################################################
##

ContainsRowRepeats = function(m) {
  ContainsRepeats(m, 1)
}

ContainsColRepeats = function(m) {
  ContainsRepeats(m, 2)
}

ContainsRepeats = function(m, dir) {
  s = apply(m, dir, function(x){paste(x, collapse='')})
  if (length(s) > length(unique(s))) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }  
}
  

######################################################################
## If the numeric vector argument row contains only 0s, map it to a
## uniform distribution:
##
## Argument:
## row: a numeric vector
## Value:
## a numeric vector of the same length as row

ZerosVector2UniformDistibution = function(row) {
  if (ZerosVector(row)) {
    return(rep(1/length(row), length(row)))
  }
  else {
    return(row)
  }
}

######################################################################
## If the numeric vector argument row contains only 0s, map it to zeros.
##
## Argument:
## row: a numeric vector
## Value:
## a numeric vector of the same length as row

ZerosVector2ZerosVector = function(row) {
  if (ZerosVector(row)) {
    return(rep(0, length(row)))
  }
  else {
    return(row)
  }
}

######################################################################
## Create a uniform distribution of length n:
##
## Argument:
## n: an integer
## Value:
## A numeric vector of length n.

UniformDistribution = function(n) {
  return(rep(1/n, n))
}

######################################################################
## Create an all 0s cost matrix.
##
## Argument:
## m: a 2d matrix
## Value:
## An all zeros matrix with the same dimensions as m

UniformCosts = function(m) {
  return(matrix(rep(0, length(m)), byrow=T, nrow=nrow(m), dimnames=list(rownames(m), colnames(m))))
}

######################################################################
## Given a binary vector x, return the indices of the 1 values:
##
## Argument:
## x: binary vector
##
## Value:
## An integer vector:
##
## For example, GetOneValuedIndices(c(0,1,0,1)) returns c(2,4).

GetOneValuedIndices = function(x){
  vals = c()
  for (i in 1:length(x)) {
    if (x[i] == 1){
      vals = c(vals, i)
    }
  }
  return(vals)
}

######################################################################
## Map an integer to a binary vector:
##
## Arguments:
## i: integer to convert
## length: length of the resulting vector
##
## Value:
## a binary vector

Integer2BinaryVector = function(i, length) {
  ## This produces a string of 1s and 0s of length 32:
  s = binary(i)  
  ## Split the binary number into digits:
  vals = strsplit(s, '')[[1]]
  ## Convert from string to vector:
  vals = as.numeric(vals)
  ## Get the suffix of the desired length:
  ind = length(vals)-length+1
  vals = vals[ind:length(vals)]  
  ## Return:
  return(vals)
}

## From http://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r.
binary = function(x){paste(sapply(strsplit(paste(rev(intToBits(x))),""),`[[`,2),collapse="")}

######################################################################
## Map a string s of 1s and 0s to a matrix of dimension nrow. It is
## assumed that s is given row-wise, in the sense that a row is created
## after every nrow elements.
##
## Arguments:
##
## s: a string of 1s and 0s -- must be a length divisible by nrow
## nrow: number of rows in the resulting matrix
## row.names: optional row names to add to the output matrix
## col.names: optional column names to add to the output matrix
##
## Value:
## A binary matrix.
##
## Example
## Str2Matrix('010101110', 3, row.names=NULL, col.names=NULL)
##
##      [,1] [,2] [,3]
## [1,]    0    1    0
## [2,]    1    0    1
## [3,]    1    1    0

Str2Matrix = function(s, nrow, row.names=NULL, col.names=NULL) {
  vals = strsplit(s, '')[[1]]
  vals = as.numeric(vals)
  m = matrix(vals, nrow=nrow, byrow=TRUE)
  ## Intuitive row and column names to track changing conditions:
  if (is.null(row.names)) {
    row.names = paste('t', seq(1,nrow(m)), sep='')
  }
  if (is.null(col.names)) {
    col.names = paste('m', seq(1,ncol(m)), sep='')
  }
  rownames(m) = row.names
  colnames(m) = col.names  
  return(m)
}

######################################################################
## Map a matrix into a string that is canonical in the sense that
## the rows have been sorted, which means that matrices differing only
## in their rows have the same output.

Matrix2CanonicalStr = function(m) {
  ## Turn the rows into string:
  s = apply(m, 1, function(x){paste(x, collapse='')})
  ## Sort the strings into a canonical order:
  s = sort(s)
  s = paste(s, collapse='')
  vec = as.numeric(strsplit(s, '')[[1]])
  m = matrix(vec, byrow=T, nrow=nrow(m))
  s = apply(m, 2, function(x){paste(x, collapse='')})
  s = sort(s)
  ## Turn them into a single row:
  s = paste(s, collapse='')
  return(s)
}

######################################################################
## A separating system is one in which each row has a single 1.

## Returns TRUE if row contains exactly one 1, else FALSE:
OneHotVector = function(vec) {
  return(sum(vec) == 1 & length(vec[vec > 0]) == 1)
}

IsSeparatingSystem = function(m) { 
  ## Apply row-wise:
  vals = apply(m, 1, OneHotVector)
  ## Test:
  if (FALSE %in% vals) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}
  
