



library(reshape)
library(ggplot2)

forced_choice_no_fam_6random_count_ALLS.csv
forced_choice_no_fam_6random_NOcount_ALNC.csv
forced_choice_no_fam_4random_count_12_january_least_LEAS.csv



## With "Favorite"
favorite <- read.csv("forced_choice_no_fam_6random_count_ALLS.csv",
                       header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)

count_compliant <- favorite$Answer.manip_check_target == "\"2\"" & favorite$Answer.manip_check_dist == "\"1\""
favorite_wrong <- subset(favorite,!count_compliant)
favorite <- subset(favorite,count_compliant)


favorite$target <- favorite$Answer.choice=="\"target\""
favorite$logical <- favorite$Answer.choice=="\"logical\""
favorite$foil <- favorite$Answer.choice=="\"foil\""

ms.favorite <- aggregate(cbind(target,
                                 logical,
                                 foil) ~ 
                             Answer.item, data=favorite, sum)

es_np_favorite <- mean(favorite$target)

chisq.test(ms.favorite[2:3,2:4])
summary(aov(target ~ as.factor(Answer.item) + as.factor(Answer.target_position), data = favorite))
favorite_wrong$target <- favorite_wrong$Answer.choice=="\"target\""



## With "least favorite"
least_favorite <- read.csv("forced_choice_no_fam_4random_count_12_january_least_LEAS.csv",
                     header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)

count_compliant_least <- least_favorite$Answer.manip_check_target == "\"2\"" & least_favorite$Answer.manip_check_dist == "\"1\""
least_favorite_wrong <- subset(least_favorite,!count_compliant_least)
least_favorite <- subset(least_favorite,count_compliant_least)


least_favorite$target <- least_favorite$Answer.choice=="\"target\""
least_favorite$logical <- least_favorite$Answer.choice=="\"logical\""
least_favorite$foil <- least_favorite$Answer.choice=="\"foil\""

ms.least_favorite <- aggregate(cbind(target,
                               logical,
                               foil) ~ 
                           Answer.item, data=least_favorite, sum)

es_np_least_favorite <- mean(least_favorite$target)

chisq.test(ms.least_favorite[2:3,2:4])
summary(aov(target ~ as.factor(Answer.item) + as.factor(Answer.target_position) + as.factor(Answer.gender), data = least_favorite))
least_favorite_wrong$target <- least_favorite_wrong$Answer.choice=="\"target\""



# Mixed - Base Rate for favorite and least favorite 


mixed <- read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/Spring2014/scale_6stimuli_no_fam_prior_5_april_PRIOLF.csv",
                     header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)


mixed$target <- mixed$Answer.choice=="\"target\""
mixed$logical <- mixed$Answer.choice=="\"logical\""
mixed$foil <- mixed$Answer.choice=="\"foil\""

mixed <-

mixed_fav = mixed[mixed$Answer.linguistic_framing == 7,]
mixed_lea = mixed[mixed$Answer.linguistic_framing == 8,]

ms.mixed_fav <- aggregate(cbind(target,
                           logical,
                           foil) ~ 
                          Answer.item, data=mixed_fav, sum)

ms.mixed_lea <- aggregate(cbind(target,
                                logical,
                                foil) ~ 
                            Answer.item, data=mixed_lea, sum)


aov_mixed = glm(logical ~ foil + Answer.item, data = mixed_fav)
summary(aov_mixed)

# Experiment 3
library(plyr)
library(binom)
sum(mixed_fav$logical)
sum(mixed_lea$logical)

# fav 0 item
9/116
binom.bayes(9,116)$lower
binom.bayes(9,116)$upper

# least 0 item
105/134
binom.bayes(105,134)$lower
binom.bayes(105,134)$upper

# fav 1 item
24/116
binom.bayes(24,116)$lower
binom.bayes(24,116)$upper

# least 1 item
9/134
binom.bayes(9,134)$lower
binom.bayes(9,134)$upper

# fav 2 item
81/116
binom.bayes(81,116)$lower
binom.bayes(81,116)$upper

# least 2 item
20/134
binom.bayes(20,134)$lower
binom.bayes(20,134)$upper


library(ggplot2)

CI_up <- c(0.1367946, 0.8467935, 0.2872714, 0.12, 0.7761514, 0.2168036)
CI_down <- c(0.03908287, 0.708275, 0.1409339, 0.034, 0.6106456, 0.09665401)
yVar <- c(0.07758621, 0.7835821, 0.2068966, 0.06716418, 0.6982759, 0.1492537)
Referent <- c('0 items', '0 items', '1 item', '1 item', '2 items', '2 items')

adjectiveCond <- c('Favorite', 'Least Favorite', 'Favorite', 'Least Favorite', 'Favorite', 'Least Favorite')
plotDataFrame <- data.frame(Referent, adjectiveCond, yVar, CI_up, CI_down)

dodge <- position_dodge(width = .95)
p <- ggplot(plotDataFrame, aes(x=adjectiveCond, y = yVar, fill=Referent)) +
  geom_bar(position="dodge", stat="identity", width=c(.9, .9, .9, .9, .9, .9)) +
  geom_errorbar(aes(ymin=CI_down,ymax=CI_up),
                position=dodge,width=0.1,size=0.9) +
  xlab("") + ylab("Probability of choosing referent") + xlim('Favorite', 'Least Favorite')
cbPalette <- c("#929599", "#A69D20", "#5694A9", "#566469")
p <- p + theme(axis.text.x = element_text(size=20))
p + ggtitle("Experiment 3 - Base Rates") + scale_fill_manual(values=c("Cost condition", '0 items'=cbPalette[1], '1 item'=cbPalette[4], '2 items'= cbPalette[3]))




# Maybe incorporate this and previews study. Add above values to:


# fav 0 item
18 / 184
binom.bayes(18,184)$lower
binom.bayes(18,184)$upper

# least 0 item
148 / 200
binom.bayes(148,200)$lower
binom.bayes(148,200)$upper

# fav 1 item
39/184
binom.bayes(39,184)$lower
binom.bayes(39,184)$upper

# least 1 item
18/200
binom.bayes(18,200)$lower
binom.bayes(18,200)$upper

# fav 2 item
125/184
binom.bayes(125,184)$lower
binom.bayes(125,184)$upper

# least 2 item
34/200
binom.bayes(34,200)$lower
binom.bayes(34,200)$upper




CI_up <- c(0.1470064, 0.7970393, 0.275215, 0.1355612, 0.7435836, 0.2266095)
CI_down <- c(0.06116417, 0.6760722, 0.1576761, 0.05618748, 0.6095199, 0.122858)
yVar <- c(0.09782609, 0.74, 0.2119565, 0.09, 0.6793478, 0.17)
Referent <- c('0 items', '0 items', '1 item', '1 item', '2 items', '2 items')

adjectiveCond <- c('Favorite', 'Least Favorite', 'Favorite', 'Least Favorite', 'Favorite', 'Least Favorite')
plotDataFrame <- data.frame(Referent, adjectiveCond, yVar, CI_up, CI_down)

dodge <- position_dodge(width = .95)
p <- ggplot(plotDataFrame, aes(x=adjectiveCond, y = yVar, fill=Referent)) +
  geom_bar(position="dodge", stat="identity", width=c(.9, .9, .9, .9, .9, .9)) +
  geom_errorbar(aes(ymin=CI_down,ymax=CI_up),
                position=dodge,width=0.1,size=0.9) +
  xlab("") + ylab("Probability of choosing referent") + xlim('Favorite', 'Least Favorite')
cbPalette <- c("#929599", "#A69D20", "#5694A9", "#566469")
p <- p + theme(axis.text.x = element_text(size=20))
p + ggtitle("Experiment 3 - Base Rates, n = 384 (both experiment waves combined)") + scale_fill_manual(values=c("Cost condition", '0 items'=cbPalette[1], '1 item'=cbPalette[4], '2 items'= cbPalette[3]))

ms <- aggregate(cbind(target,foil,logical) ~ Answer.linguistic_framing_condition + Answer.item, mixed, mean)







library(plyr)
library(reshape2)
library(ggplot2)
library(binom)
#### ACROSS ITEMS
md <- melt(mixed, measure.vars = c("target","foil","logical"), variable.name="object", value.name="chosen")
ms <- ddply(md, .(Answer.linguistic_framing_condition, object), #Answer.item, 
            summarise, 
            c = mean(chosen),
            n = sum(chosen), 
            l = length(chosen),
            c.cih = binom.bayes(sum(chosen),length(chosen),tol=.0001)$upper,
            c.cil = binom.bayes(sum(chosen),length(chosen),tol=.0001)$lower)

ms$framing <- factor(ms$Answer.linguistic_framing_condition)
levels(ms$framing) <- c("favorite","least favorite")

ggplot(ms, aes(x= framing, y=c, fill=object)) + 
  geom_bar(position=position_dodge()) + 
  geom_linerange(aes(ymin=c.cil,ymax=c.cih), 
                 position=position_dodge(width=.9)) + 
  ylab("Probability of choosing referent")





#### NOW BY ITEM
md <- melt(mixed, measure.vars = c("target","foil","logical"), variable.name="object", value.name="chosen")
ms <- ddply(md, .(Answer.linguistic_framing_condition, object, Answer.item), #Answer.item, 
      summarise, 
      c = mean(chosen),
      n = sum(chosen), 
      l = length(chosen),
      c.cih = binom.bayes(sum(chosen),length(chosen),tol=.01)$upper,
      c.cil = binom.bayes(sum(chosen),length(chosen),tol=.01)$lower)

ms$framing <- factor(ms$Answer.linguistic_framing_condition)
levels(ms$framing) <- c("favorite","least favorite")

ggplot(ms, aes(x= framing, y=c, fill=object)) + 
  geom_bar(position=position_dodge()) + 
#   geom_linerange(aes(ymin=c.cil,ymax=c.cih), 
#                 position=position_dodge(width=.9)) + 
  ylab("Probability of choosing referent") + 
  facet_wrap(~Answer.item)
