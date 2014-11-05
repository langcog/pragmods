######################################################################
## DEMOS:

## Example from p. 9ff of the Jaeger handbook article:
ScalarImplicature = function() {
  print("======================================================================")
  print('Scalars')    
  print(IBR(scalars))
}

## No Scales condition from the Stiller et al. paper:
StillerNoscales = function() {
  print("======================================================================")
  print('Stiller no scales.')    
  print(IBR(stiller.noscales))
  MatrixViz(stiller.noscales)  
}

## Science paper referential game:
FrankGoodman = function() {
  print("======================================================================")
  print('Frank-Goodman')    
  print(IBR(fg))
}

## Example from p. 16-17 of the Jaeger handbook article:
Division = function() {
  print("======================================================================")
  print('Division of pragmatic labor (costly messages)') 
  print(IBR(hornutil, costs=horncosts))
  
}

## Evaluate from a string:
FromString = function(s) {
  print("======================================================================")
  print(paste('Processing string', s) )
  return(eval(parse(text=s)))
}

## 4x4 matrix requring 7 iterations (the max):
Matrix7 = function() {
  print("======================================================================")
  print('4 x 4 matrix requiring 7 iterations in IBR')
  print(IBR(m7))
  ImageViz(m7, print.matrix=TRUE)
}

## All demos:
Demos = function() {  
  ScalarImplicature()
  StillerNoscales()
  FrankGoodman()
  Division()
  print(FromString('Lstar(Sstar(Lstar(S0(fg), fg)), fg)'))
  Matrix7()
}

Demos()

