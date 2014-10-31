---
title: "wx3_analysis"
author: "Avery Katko"
date: "August 3, 2014"
output: pdf_document
---


```r
rm(list = ls())
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 2.15.2
```

```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 2.15.1
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 2.15.1
```

```r
library(bootstrap)
```

```
## Warning: package 'bootstrap' was built under R version 2.15.3
```

```r
setwd("~/github/local/pragmods")
```

```
## Error: cannot change working directory
```

```r
# source('useful.R')
d <- read.csv("seq_data/pragmods_wx3.anondata.csv")
```

```
## Warning: cannot open file 'seq_data/pragmods_wx3.anondata.csv': No such
## file or directory
```

```
## Error: cannot open the connection
```

```r
# head(d)
```


Exclude participants that either failed manipulation check or were rejected.


```r
exclude <- d$assignmentstatus == "Rejected" |
  d$Answer.name_check_correct == "\"FALSE\""
```

```
## Error: object 'd' not found
```

```r
sum(exclude)
```

```
## Error: object 'exclude' not found
```

```r
mean(exclude)
```

```
## Error: object 'exclude' not found
```

```r

d <- subset(d, exclude == FALSE)
```

```
## Error: object 'd' not found
```

```r

d$Answer.choice_correct_1 <- factor(as.logical(d$Answer.choice_correct_1))
```

```
## Error: object 'd' not found
```

```r
d$Answer.choice_correct_2 <- factor(as.logical(d$Answer.choice_correct_2))
```

```
## Error: object 'd' not found
```

```r
d$Answer.choice_correct_3 <- factor(as.logical(d$Answer.choice_correct_3))
```

```
## Error: object 'd' not found
```

```r
d$Answer.choice_correct_4 <- factor(as.logical(d$Answer.choice_correct_4))
```

```
## Error: object 'd' not found
```

```r
d$Answer.choice_correct_5 <- factor(as.logical(d$Answer.choice_correct_5))
```

```
## Error: object 'd' not found
```

```r
d$Answer.choice_correct_6 <- factor(as.logical(d$Answer.choice_correct_6))
```

```
## Error: object 'd' not found
```


Restructure data to have trial variable. (working around mturk data submission limitations; can't safely submit arrays, so we have to use separate variables for each trial)


```r
d$level_1 <- factor(rep(0, nrow(d)))
```

```
## Error: object 'd' not found
```

```r
d$level_2 <- factor(rep(1, nrow(d)))
```

```
## Error: object 'd' not found
```

```r
d$level_3 <- factor(rep(0, nrow(d)))
```

```
## Error: object 'd' not found
```

```r
d$level_4 <- factor(rep(1, nrow(d)))
```

```
## Error: object 'd' not found
```

```r
d$level_5 <- factor(rep(0, nrow(d)))
```

```
## Error: object 'd' not found
```

```r
d$level_6 <- factor(rep(1, nrow(d)))
```

```
## Error: object 'd' not found
```

```r

trial.df <- function(tn) {
    df <- data.frame(id = d$workerid, seqCond = d$Answer.sequence_condition, 
        trial = factor(rep(tn, nrow(d))), level = d[sprintf("level_%d", tn)], 
        item = d[sprintf("Answer.item_%d", tn)], targetProp = d[sprintf("Answer.target_prop_%d", 
            tn)], distractorProp = d[sprintf("Answer.distractor_prop_%d", tn)], 
        targetPosition = d[sprintf("Answer.target_position_%d", tn)], distractorPosition = d[sprintf("Answer.distractor_position_%d", 
            tn)], choice = d[sprintf("Answer.choice_%d", tn)], choiceCorrect = d[sprintf("Answer.choice_correct_%d", 
            tn)])
    names(df) <- c("id", "seqCond", "trial", "level", "item", "targetProp", 
        "distractorProp", "targetPosition", "distractorPosition", "choice", 
        "choiceCorrect")
    return(df)
}

d2 <- rbind(trial.df(1), trial.df(2), trial.df(3), trial.df(4), trial.df(5), 
    trial.df(6))
```

```
## Error: object 'd' not found
```

```r
summary(d2)
```

```
## Error: object 'd2' not found
```



```r
# statistics for boolean factors; copied from useful.R, with a slightly
# different mean function to work with the choiceCorrect factor
l.mean <- function(...) {
    mean(as.logical(...))
}
l.theta <- function(x, xdata, na.rm = T) {
    l.mean(xdata[x], na.rm = na.rm)
}
l.ci.low <- function(x, na.rm = T) {
    l.mean(x, na.rm = na.rm) - quantile(bootstrap(1:length(x), 1000, l.theta, 
        x, na.rm = na.rm)$thetastar, 0.025, na.rm = na.rm)
}
l.ci.high <- function(x, na.rm = T) {
    quantile(bootstrap(1:length(x), 1000, l.theta, x, na.rm = na.rm)$thetastar, 
        0.975, na.rm = na.rm) - l.mean(x, na.rm = na.rm)
}

ms <- aggregate(choiceCorrect ~ trial + level, data = d2, l.mean)
```

```
## Error: object 'd2' not found
```

```r
ms$cil <- aggregate(choiceCorrect ~ trial + level, data = d2, l.ci.low)$choiceCorrect
```

```
## Error: object 'd2' not found
```

```r
ms$cih <- aggregate(choiceCorrect ~ trial + level, data = d2, l.ci.high)$choiceCorrect
```

```
## Error: object 'd2' not found
```

```r

# colorblind-friendly color palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
    "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
    "#D55E00", "#CC79A7")
```


Compare performance on trials, in chronological order.


```r
ggplot(data = ms, aes(x = trial, y = choiceCorrect, fill = level)) + geom_bar(stat = "identity", 
    color = "black") + geom_errorbar(aes(ymin = choiceCorrect - cil, ymax = choiceCorrect + 
    cih), width = 0.2) + # facet_grid(. ~ seqCond) +
theme_bw() + scale_fill_manual(values = cbPalette)
```

```
## Error: object 'ms' not found
```


Compare performance on level-0 vs level-1 trials.


```r
ggplot(data = ms, aes(x = level, y = choiceCorrect, fill = trial)) + geom_bar(stat = "identity", 
    color = "black") + geom_errorbar(aes(ymin = choiceCorrect - cil, ymax = choiceCorrect + 
    cih), width = 0.2) + # facet_grid(. ~ seqCond) +
theme_bw() + scale_fill_manual(values = cbPalette)
```

```
## Error: object 'ms' not found
```


Chi-squared test between level-1 inferences (TODO...)


```r
col_foil <- aggregate(choice ~ seqCond, data = d2[d2$level=="1",], 
          function(...){sum(... == "\"foil\"")})
```

```
## Error: object 'd2' not found
```

```r

col_target <- aggregate(choice ~ seqCond, data = d2[d2$level=="1",], 
          function(...){sum(... == "\"target\"")})
```

```
## Error: object 'd2' not found
```

```r

col_logical <- aggregate(choice ~ seqCond, data = d2[d2$level=="1",], 
          function(...){sum(... == "\"logical\"")})
```

```
## Error: object 'd2' not found
```

```r

cont_table <- data.frame(cbind(col_foil,col_target[2],col_logical[2]))
```

```
## Error: object 'col_foil' not found
```

```r
names(cont_table) <- c("seqCond","foil","target","logical")
```

```
## Error: object 'cont_table' not found
```

```r

cont_table
```

```
## Error: object 'cont_table' not found
```

```r
#chisq.test(cont_table[1:2,2:4])
```

