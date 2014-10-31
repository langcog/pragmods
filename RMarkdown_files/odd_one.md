Odd one out and non-verbal communication
========================================================

---
author: Andrés Gómez Emilsson
date: May 14 2014
participants: 200
output: pdf_document
---

![alt text]['sd']



```r

patch <- read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/Spring2014/patch_oddone_no_fam_14_may_PATCH.csv",
                           header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)

patch$odd_one <- patch$Answer.choice=="\"odd_one\""
patch$twin_1 <- patch$Answer.choice=="\"twin_1\""
patch$twin_2 <- patch$Answer.choice=="\"twin_2\""

compliant <- patch$Answer.manip_check_target == "\"2\"" & patch$Answer.manip_check_dist == "\"2\"" & patch$Answer.manip_check_foil == "\"2\"" & patch$Answer.name_check_correct == "\"TRUE\""
patch_compliant <- subset(patch,compliant)

length(patch$Answer.item)
```

```
## [1] 200
```

```r

```





```
## [1] "number of compliant participants"
```

```
## [1] 178
```








```r
# Statistical analysis - (regressions, anova, chi square, factor analysis,
# etc.)

summary(aov(odd_one ~ Answer.linguistic_framing_condition * Answer.item, data = patch_compliant))
```

```
## Warning: variable 'Answer.item' converted to a factor
```

```
##                                                  Df Sum Sq Mean Sq F value
## Answer.linguistic_framing_condition               1   0.48   0.479    4.71
## Answer.item                                       5   0.78   0.157    1.54
## Answer.linguistic_framing_condition:Answer.item   5   0.36   0.073    0.71
## Residuals                                       166  16.90   0.102        
##                                                 Pr(>F)  
## Answer.linguistic_framing_condition              0.031 *
## Answer.item                                      0.181  
## Answer.linguistic_framing_condition:Answer.item  0.615  
## Residuals                                               
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r


summary(glm(odd_one ~ Answer.linguistic_framing_condition + twenties, data = patch_compliant))
```

```
## 
## Call:
## glm(formula = odd_one ~ Answer.linguistic_framing_condition + 
##     twenties, data = patch_compliant)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.2141  -0.1178  -0.1044  -0.0082   0.8956  
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                          -0.7483     0.4540   -1.65    0.101  
## Answer.linguistic_framing_condition   0.0962     0.0476    2.02    0.045 *
## twentiesTRUE                         -0.1096     0.0480   -2.28    0.024 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for gaussian family taken to be 0.1001)
## 
##     Null deviance: 18.522  on 177  degrees of freedom
## Residual deviance: 17.521  on 175  degrees of freedom
## AIC: 100.5
## 
## Number of Fisher Scoring iterations: 2
```

```r

```






```r
# Visualization - the reason this is after statistical analysis is that some properties of the graphs (e.g. a regression line or confidence intervals) are in themselves statistical analysis computed in the previous section

library(plyr)
```

```
## Warning: package 'plyr' was built under R version 2.15.1
```

```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 2.15.1
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 2.15.2
```

```r
library(binom)
```

```
## Warning: package 'binom' was built under R version 2.15.3
```

```r
library(bootstrap)
```

```
## Warning: package 'bootstrap' was built under R version 2.15.3
```

```
## 
## Attaching package: 'bootstrap'
## 
## The following object(s) are masked _by_ '.GlobalEnv':
## 
##     patch
```

```r





#statistics for boolean factors; copied from useful.R, with a slightly different mean function to work with the choiceCorrect factor
l.mean <- function(...){mean(as.logical(...))}
l.theta <- function(x,xdata,na.rm=T) {l.mean(xdata[x],na.rm=na.rm)}
l.ci.low <- function(x,na.rm=T) {
  l.mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,l.theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
l.ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,l.theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - l.mean(x,na.rm=na.rm)}




# Plot the histogram for the answers given on each item (aggreagating linguistic frame conditions)
#ms <- aggregate(odd_one ~ Answer.linguistic_framing_condition + Answer.item, data = patch_compliant,l.mean)

#ms$cil <- aggregate(odd_one ~ Answer.linguistic_framing_condition + Answer.item, data = patch_compliant, l.ci.low)$odd_one
#ms$cih <- aggregate(odd_one ~ Answer.linguistic_framing_condition + Answer.item, data = patch_compliant, l.ci.high)$odd_one



md <- melt(patch_compliant, measure.vars = c("odd_one","twin_1","twin_2"), variable.name="object", value.name="chosen")


ms <- ddply(md, .(object, Answer.item), #Answer.item, 
            summarise, 
            c = mean(chosen),
            n = sum(chosen), 
            l = length(chosen),
            sdc = sd(chosen),
            c.cih = c + l.ci.high(chosen),
            c.cil = c - l.ci.low(chosen))


ms$item <- factor(ms$Answer.item)
levels(ms$item) <- c("boat","friend", "pizza", "snowman", "sundae", "Christmas tree")

ggplot(ms, aes(x= item, y=c, fill=object)) + 
  geom_bar(position=position_dodge()) + 
  geom_linerange(aes(ymin=c.cil,ymax=c.cih), 
                 position=position_dodge(width=.9)) + 
  ylab("Probability of choosing") +
  scale_fill_manual(values=cbPalette)
```

```
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r



# Visualizing the two conditions separately: Show the histogram by item and by condition

ms <- ddply(patch_compliant, .(Answer.linguistic_framing_condition,Answer.item),
      function(x) {
        y <- data.frame(choice=c("odd_one","twin_1","twin_2"),
                        proportion=c(mean(x$odd_one),mean(x$twin_1),mean(x$twin_2)))
        return(y)
      })

ms$frame <- factor(ms$Answer.linguistic_framing_condition)
levels(ms$frame) <- c("patch","word")
ms$item <- factor(ms$Answer.item)

qplot(item, proportion, fill=choice, facets = . ~ frame, 
      geom="bar",
      data=ms)
```

```
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

```r

patch_table <- aggregate(cbind(odd_one,
                               twin_1,
                               twin_2) ~ 
                           Answer.linguistic_framing_condition , data=patch_compliant, sum)

```






Complete Basic info:

May 14th 2014

200 subjects | 3FC | NO familiarization | random out of 6 | Linguistic framing: One 
word or color patch | Participants asked to count the tshree elements
scale, level 1
Code: PATCH

patch_oddone_no_fam_14_may_PATCH.csv

var participant_response_type = 0;

var participant_feature_count = 2; // They count all of the props

var linguistic_framing = random(9,10); // 

var question_type = 0;

var target_filler_sequence = 0;

var familiarization_status = 0;

var stim_index = random(0,5);

var scale_and_level = 8;

var img_size = 200; // needs to be implemented, currently just a placeholder   

var cond = random(1,4);

var file_number_to_use_for_referents = '3'

render("odd_one.Rmd", "pdf_document")

#/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/RMarkdown_files
#Rscript -e "require(knitr); require(markdown); knit('$odd_one.rmd', '$odd_one.md'); #markdownToHTML('$odd_one.md', '$odd_one.html')"

pandoc -s odd_one.html -o odd_one.html.pdf


['sd']: /Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/pragmods/cleanPragmods/experiments_in_pictures/odd_one_out_linguisticFraming9.png
