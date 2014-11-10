source('models/viz_img.R')

######################################################################
## SCENARIOS:

## standard 
standard = matrix(
  c(1,   1,
    0,   1,
    0,   0), byrow=TRUE, nrow=3,
  dimname=list(
    c('r1', 'r2', 'r3'), # Row names; referents.
    c('hat', 'glasses'))) # Column names; messages.

## 3-levels/levels "complex"
complex = matrix(
  c(1,   1,   0,
    0,   1,   1,
    0,   0,   1), byrow=TRUE, nrow=3,
  dimname=list(
    c('r1', 'r2', 'r3'), # Row names; referents.
    c('hat', 'glasses','mustache'))) # Column names; messages.

## 3-levels/twins
twins = matrix(
  c(1,   1,   0,
    1,   0,   1,
    1,   0,   1), byrow=TRUE, nrow=3,
  dimname=list(
    c('r1', 'r2', 'r3'), # Row names; referents.
    c('hat', 'glasses','mustache'))) # Column names; messages.

## 3-levels/oddman
oddman = matrix(
  c(1,   1,   0,
    1,   0,   1,
    0,   1,   1), byrow=TRUE, nrow=3,
  dimname=list(
    c('r1', 'r2', 'r3'), # Row names; referents.
    c('hat', 'glasses','mustache'))) # Column names; messages.

## 4-size
# notation is size(num referents).(num features 
size2.2 = matrix(c(0, 1, 
                   1, 1),
                 byrow=TRUE, nrow=2, ncol=2, 
                 dimname=list(c('ref1', 'ref2'), 
                              c('hat', 'glasses')))

size3.2 = matrix(c(0, 0, 
                   0, 1, 
                   1, 1), 
                 byrow=TRUE, nrow=3, ncol=2, 
                 dimname=list(c('ref1', 'ref2', 'ref3'), 
                              c('hat', 'glasses')))

size4.2 = matrix(c(0, 0, 
                   0, 0, 
                   0, 1, 
                   1, 1), 
                 byrow=TRUE, nrow=4, ncol=2, 
                 dimname=list(c('ref1', 'ref2', 'ref3', 'ref4'), 
                              c('hat', 'glasses')))

size2.3 = matrix(c(0, 1, 1, 
                   1, 1, 1), 
                 byrow=TRUE, nrow=2, ncol=3, 
                 dimname=list(c('ref1', 'ref2'), c
                              ('hat', 'glasses', 'moustache')))

size3.3 = matrix(c(0, 0, 1, 
                   0, 1, 1, 
                   1, 1, 1),
                 byrow=TRUE, nrow=3, ncol=3, 
                 dimname=list(c('ref1', 'ref2', 'ref3'), 
                              c('hat', 'glasses', 'moustache')))

size4.3 = matrix(c(0, 0, 1, 
                   0, 0, 1, 
                   0, 1, 1, 
                   1, 1, 1), 
                 byrow=TRUE, nrow=4, ncol=3, 
                 dimname=list(c('ref1', 'ref2', 'ref3','ref4'), 
                              c('hat', 'glasses', 'moustache')))

size2.4 = matrix(c(0, 1, 1, 1, 
                   1, 1, 1, 1), 
                 byrow=TRUE, nrow=2, ncol=4, 
                 dimname=list(c('ref1', 'ref2'), 
                              c('hat', 'glasses', 'moustache', 'bowtie')))

size3.4 = matrix(c(0, 0, 1, 1, 
                   0, 1, 1, 1, 
                   1, 1, 1, 1), 
                 byrow=TRUE, nrow=3, ncol=4, 
                 dimname=list(c('ref1', 'ref2', 'ref3'), 
                              c('hat', 'glasses', 'moustache', 'bowtie')))

size4.4 = matrix(c(0, 0, 1, 1, 
                   0, 0, 1, 1, 
                   0, 1, 1, 1, 
                   1, 1, 1, 1), 
                 byrow=TRUE, nrow=4, ncol=4, 
                 dimname=list(c('ref1', 'ref2', 'ref3', 'ref4'), 
                              c('hat', 'glasses', 'moustache', 'bowtie')))


######################################################################
## PRINT

disps <- list(standard, complex, twins, oddman, 
              size2.2, size3.2, size4.2, 
              size2.3, size3.3, size4.3, 
              size2.4, size3.4, size4.4)
names <- c("ex-standard","ex-complex","ex-twins","ex-oddman",
           "size 2x2", "size 3x2", "size 4x2",
           "size 2x3", "size 3x3", "size 4x3",
           "size 2x4", "size 3x4", "size 4x4")

for (d in 1:length(disps)) {
  pdf(paste("plots/",names[d]))
  ImageViz(disps[d])
  dev.off()
}