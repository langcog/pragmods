Title
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **Help** toolbar button for more details on using R Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

---
title: 
author: 
date: 
---


```r
# Basic study statistics: Number of participants, dates when it was ran,
# bugs and issues present in the study, reasons to be concerned that the
# data is unreliable, etc.
```



```r
# All the data pre-procesing. E.g. merging CSV files, adding features from
# excel sheets, determine if choices were correct
```



```r
# Statistical analysis - (regressions, anova, chi square, factor analysis,
# etc.)
```



```r
# Visualization - the reason this is after statistical analysis is that
# some properties of the graphs (e.g. a regression line or confidence
# intervals) are in themselves statistical analysis computed in the
# previous section
```



```r
summary(cars)
```

```
##      speed           dist    
##  Min.   : 4.0   Min.   :  2  
##  1st Qu.:12.0   1st Qu.: 26  
##  Median :15.0   Median : 36  
##  Mean   :15.4   Mean   : 43  
##  3rd Qu.:19.0   3rd Qu.: 56  
##  Max.   :25.0   Max.   :120
```


You can also embed plots, for example:


```r
plot(cars)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


