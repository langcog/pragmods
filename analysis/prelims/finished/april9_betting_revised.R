# Betting on 1 word


betting_no_fam_boat_oneword_8_december_LZZZ.csv
betting_no_fam_friend_oneword_1_december_AGHP.csv
betting_no_fam_snowman_oneword_8_december_LSSS.csv
betting_no_fam_sundae_oneword_8_december_BSUN.csv

one_word_boat = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/betting_no_fam_boat_oneword_8_december_LZZZ.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)
one_word_friend = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/betting_no_fam_friend_oneword_1_december_AGHP.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)
one_word_snowman = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/betting_no_fam_snowman_oneword_8_december_LSSS.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)
one_word_sundae = read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/betting_no_fam_sundae_oneword_8_december_BSUN.csv",header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)

one_word_betting = rbind(one_word_boat, one_word_friend, one_word_snowman, one_word_sundae)

ms.one_word_betting <- aggregate(cbind(Answer.money_allocated_to_target,
                                    Answer.money_allocated_to_logical,
                                    Answer.money_allocated_to_foil) ~ 
                                Answer.item, data=one_word_betting, mean)

colnames(one_word_friend)
one_word_friend$Answer.money_allocated_to_target


one_word_betting$max_like_target <- (one_word_betting$Answer.money_allocated_to_target > one_word_betting$Answer.money_allocated_to_logical) & (one_word_betting$Answer.money_allocated_to_target > one_word_betting$Answer.money_allocated_to_foil)
one_word_betting$max_like_logical <- (one_word_betting$Answer.money_allocated_to_logical > one_word_betting$Answer.money_allocated_to_target) & (one_word_betting$Answer.money_allocated_to_logical > one_word_betting$Answer.money_allocated_to_foil)
one_word_betting$max_like_foil <- (one_word_betting$Answer.money_allocated_to_foil > one_word_betting$Answer.money_allocated_to_target) & (one_word_betting$Answer.money_allocated_to_foil > one_word_betting$Answer.money_allocated_to_logical)

# With logic for distribution of tied
one_word_betting$max_like_target_shared <- (one_word_betting$Answer.money_allocated_to_target == one_word_betting$Answer.money_allocated_to_logical &
                                            one_word_betting$Answer.money_allocated_to_target >  one_word_betting$Answer.money_allocated_to_foil) | 
  (one_word_betting$Answer.money_allocated_to_target == one_word_betting$Answer.money_allocated_to_foil &
     one_word_betting$Answer.money_allocated_to_target >  one_word_betting$Answer.money_allocated_to_logical)

one_word_betting$max_like_logical_shared <- (one_word_betting$Answer.money_allocated_to_logical == one_word_betting$Answer.money_allocated_to_target &
                                            one_word_betting$Answer.money_allocated_to_logical >  one_word_betting$Answer.money_allocated_to_foil) | 
  (one_word_betting$Answer.money_allocated_to_logical == one_word_betting$Answer.money_allocated_to_foil &
     one_word_betting$Answer.money_allocated_to_logical >  one_word_betting$Answer.money_allocated_to_target)

one_word_betting$max_like_foil_shared <- (one_word_betting$Answer.money_allocated_to_foil == one_word_betting$Answer.money_allocated_to_target &
                                            one_word_betting$Answer.money_allocated_to_foil >  one_word_betting$Answer.money_allocated_to_logical) | 
  (one_word_betting$Answer.money_allocated_to_foil == one_word_betting$Answer.money_allocated_to_logical &
     one_word_betting$Answer.money_allocated_to_foil >  one_word_betting$Answer.money_allocated_to_target)

# And the tie is the largest in additon to being equal to the other
one_word_betting$max_like_target_balanced <- one_word_betting$max_like_target_shared * 1/2 + one_word_betting$max_like_target
one_word_betting$max_like_logical_balanced <- one_word_betting$max_like_logical_shared * 1/2 + one_word_betting$max_like_logical
one_word_betting$max_like_foil_balanced <- one_word_betting$max_like_foil_shared * 1/2 + one_word_betting$max_like_foil

ms.max_like <- aggregate(cbind(max_like_target_balanced,
                               max_like_logical_balanced,
                               max_like_foil_balanced) ~ Answer.item, data=one_word_betting, mean)

ms.noties <- aggregate(cbind(Answer.money_allocated_to_target,
                             Answer.money_allocated_to_logical,
                             Answer.money_allocated_to_foil) ~ Answer.item, 
                       data=subset(one_word_betting,
                                   Answer.money_allocated_to_target!=Answer.money_allocated_to_logical), mean)


summary(glm(Answer.money_allocated_to_target ~ Answer.item, data = one_word_betting))


# As a third idea compute a metric of "certainty" based on the money distributions so that you become
# able to approximate the forced choice results


# Forced choice. Needs analysis_on_3FC.R - One word
one_word_separated_together
ms.one_word_3fc <- aggregate(cbind(target,
                                   logical,
                                   foil) ~ 
                               Answer.item, data=one_word_separated_together, mean)





library(plyr)
library(reshape2)
library(ggplot2)
library(binom)

one_word_betting$money_target =  one_word_betting$Answer.money_allocated_to_target
one_word_betting$money_logical =  one_word_betting$Answer.money_allocated_to_logical
one_word_betting$money_foil =  one_word_betting$Answer.money_allocated_to_foil


md <- melt(one_word_betting, measure.vars = c("money_target","money_logical","money_foil"), variable.name="object", value.name="chosen")
ms <- ddply(md, .(object, Answer.item), #Answer.item, 
            summarise, 
            c = mean(chosen),
            n = sum(chosen), 
            l = length(chosen),
            sdc = sd(chosen),
            c.cih = c + (sdc/l^.5),
            c.cil = c - (sdc/l^.5))


ms$item <- factor(ms$Answer.item)
levels(ms$item) <- c("boat","friend", "snowman", "sundae")

ggplot(ms, aes(x= item, y=c, fill=object)) + 
  geom_bar(position=position_dodge()) + 
  geom_linerange(aes(ymin=c.cil,ymax=c.cih), 
                 position=position_dodge(width=.9)) + 
  ylab("Money allocated to this referent")
