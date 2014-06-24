likert_no_fam_boat_oneword_9_december_FUNU.csv
likert_no_fam_friend_oneword_2_december_JSLD.csv
likert_no_fam_snowman_oneword_9_december_FINA.csv
likert_no_fam_sundaes_oneword_9_december_LSUN.csv

likert_ow_boat = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/likert_no_fam_boat_oneword_9_december_FUNU.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)
likert_ow_friend = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/likert_no_fam_friend_oneword_2_december_JSLD.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)
likert_ow_snowman = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/likert_no_fam_snowman_oneword_9_december_FINA.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)
likert_ow_sundaes = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/likert_no_fam_sundaes_oneword_9_december_LSUN.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)

one_word_likert = rbind(likert_ow_boat, likert_ow_friend, likert_ow_snowman, likert_ow_sundaes)

one_word_likert$target = one_word_likert$Answer.Likert_value_target
one_word_likert$logical = one_word_likert$Answer.Likert_value_logical
one_word_likert$foil = one_word_likert$Answer.Likert_value_foil

one_word_likert$Likert_sum = one_word_likert$Answer.Likert_value_target +
  one_word_likert$Answer.Likert_value_logical +
  one_word_likert$Answer.Likert_value_foil


one_word_likert$target_norm = one_word_likert$target / one_word_likert$Likert_sum
one_word_likert$logical_norm = one_word_likert$logical / one_word_likert$Likert_sum
one_word_likert$foil_norm = one_word_likert$foil / one_word_likert$Likert_sum


ms.one_word_likert <- aggregate(cbind(target,
                                       logical,
                                       foil) ~ 
                                   Answer.item, data=one_word_likert, mean)

ms.one_word_likert

colnames(likert_ow_boat)
length(one_word_betting$Answer.Likert_value_target)


likert_regression = glm(target ~ Answer.item + logical + foil, data = one_word_likert)
summary(likert_regression)


library(plyr)
library(reshape2)
library(ggplot2)
library(binom)


mdL_norm <- melt(one_word_likert, measure.vars = c("target_norm","logical_norm","foil_norm"), variable.name="object", value.name="chosen")
mdL <- melt(one_word_likert, measure.vars = c("target","logical","foil"), variable.name="object", value.name="chosen")

mLiker <- ddply(mdL_norm, .(object, Answer.item), #Answer.item, 
            summarise, 
            c = mean(chosen),
            n = sum(chosen), 
            l = length(chosen),
            c.cih = mean(chosen) + (sd(chosen)/length(chosen)^.5),
            c.cil = mean(chosen) - (sd(chosen)/length(chosen)^.5))

            #c.cih = binom.bayes(sum(chosen),length(chosen),tol=.01)$upper,
            #c.cil = binom.bayes(sum(chosen),length(chosen),tol=.01)$lower)

mLiker$item <- factor(mLiker$Answer.item)
levels(mLiker$item) <- c("boat","friend", "snowman", "sundae")

ggplot(mLiker, aes(x= item, y=c, fill=object)) + 
  geom_bar(position=position_dodge()) + 
  geom_linerange(aes(ymin=c.cil,ymax=c.cih), 
                 position=position_dodge(width=.9)) + 
  ylab("Likert value on this referent")