
patch <- read.csv("/Users/andesgomez/Documents/Stanford/Autumn2013-Masters/PayedWork/andres_data/Spring2014/patch_oddone_no_fam_14_may_PATCH.csv",
                           header=TRUE, sep="\t", row.names=NULL, stringsAsFactors = FALSE)


patch$odd_one <- patch$Answer.choice=="\"odd_one\""
patch$twin_1 <- patch$Answer.choice=="\"twin_1\""
patch$twin_2 <- patch$Answer.choice=="\"twin_2\""

compliant <- patch$Answer.manip_check_target == "\"2\"" & patch$Answer.manip_check_dist == "\"2\"" & patch$Answer.manip_check_foil == "\"2\"" & patch$Answer.name_check_correct == "\"TRUE\""
patch_compliant <- subset(patch,compliant)
#patch_compliant <- patch
#patch_not_compliant <- subset(patch,!compliant)

patch_subset = subset(patch_compliant, patch_compliant$Answer.linguistic_framing_condition == 9)
word_subset = subset(patch_compliant, patch_compliant$Answer.linguistic_framing_condition == 10)


patch_table <- aggregate(cbind(odd_one,
                                  twin_1,
                                  twin_2) ~ 
                           Answer.linguistic_framing_condition , data=patch_compliant, sum)

chisq.test(patch_table[1:2,2:4])
simplified = matrix(c(0,0,0,0), ncol = 2, nrow = 2)
simplified[,1] = patch_table[1:2,2]
simplified[,2] = patch_table[1:2,3] + patch_table[1:2,4]
chisq.test(simplified)


patch_table <- aggregate(cbind(odd_one,
                               twin_1,
                               twin_2) ~ 
                           Answer.linguistic_framing_condition + Answer.item, data=patch_compliant, sum)

#chisq.test(patch_table[1:6,2:4])
chisq.test(patch_table[1:2,2:4])

patch_by_item <- aggregate(cbind(odd_one,
                               twin_1,
                               twin_2) ~ 
                           Answer.item, data=patch_subset, sum)


word_by_item <- aggregate(cbind(odd_one,
                                 twin_1,
                                 twin_2) ~ 
                             Answer.item, data=word_subset, sum)

mean(patch_subset$odd_one)
mean(word_subset$odd_one)
mean(patch_compliant$odd_one)

## Demographic processing

patch_compliant$males = patch_compliant$Answer.gender == "\"m\"" | patch_compliant$Answer.gender == "\"male\"" | patch_compliant$Answer.gender == "\"M\"" | patch_compliant$Answer.gender == "\"Male\"" | patch_compliant$Answer.gender == "\"MALE\""
patch_compliant$females = patch_compliant$Answer.gender == "\"f\"" | patch_compliant$Answer.gender == "\"female\"" | patch_compliant$Answer.gender == "\"F\"" | patch_compliant$Answer.gender == "\"Female\"" | patch_compliant$Answer.gender == "\"FEMALE\""

patch_compliant$twenties =  patch_compliant$Answer.age == "\"20\"" | patch_compliant$Answer.age == "\"21\"" | patch_compliant$Answer.age == "\"22\"" | patch_compliant$Answer.age == "\"23\"" | patch_compliant$Answer.age == "\"24\"" | patch_compliant$Answer.age == "\"25\"" | patch_compliant$Answer.age == "\"26\"" | patch_compliant$Answer.age == "\"27\"" | patch_compliant$Answer.age == "\"28\"" | patch_compliant$Answer.age == "\"29\"" 
patch_compliant$thirties = patch_compliant$Answer.age == "\"30\"" | patch_compliant$Answer.age == "\"31\"" | patch_compliant$Answer.age == "\"32\"" | patch_compliant$Answer.age == "\"33\"" | patch_compliant$Answer.age == "\"34\"" | patch_compliant$Answer.age == "\"35\"" | patch_compliant$Answer.age == "\"36\"" | patch_compliant$Answer.age == "\"37\"" | patch_compliant$Answer.age == "\"38\"" | patch_compliant$Answer.age == "\"39\"" 
patch_compliant$fourties = patch_compliant$Answer.age == "\"40\"" | patch_compliant$Answer.age == "\"41\"" | patch_compliant$Answer.age == "\"42\"" | patch_compliant$Answer.age == "\"43\"" | patch_compliant$Answer.age == "\"44\"" | patch_compliant$Answer.age == "\"45\"" | patch_compliant$Answer.age == "\"46\"" | patch_compliant$Answer.age == "\"47\"" | patch_compliant$Answer.age == "\"48\"" | patch_compliant$Answer.age == "\"49\"" 
patch_compliant$fifties = patch_compliant$Answer.age == "\"50\"" | patch_compliant$Answer.age == "\"51\"" | patch_compliant$Answer.age == "\"52\"" | patch_compliant$Answer.age == "\"53\"" | patch_compliant$Answer.age == "\"54\"" | patch_compliant$Answer.age == "\"55\"" | patch_compliant$Answer.age == "\"56\"" | patch_compliant$Answer.age == "\"57\"" | patch_compliant$Answer.age == "\"58\"" | patch_compliant$Answer.age == "\"59\"" 





summary(aov(odd_one ~  Answer.linguistic_framing_condition + Answer.item, data = patch_compliant))

summary(glm(odd_one ~  Answer.linguistic_framing_condition + twenties, data = patch_compliant))


patch_table <- aggregate(cbind(odd_one,
                               twin_1,
                               twin_2) ~ 
                           Answer.linguistic_framing_condition, data=patch_compliant, sum)
patch_table







library(plyr)
library(reshape2)
library(ggplot2)
library(binom)


md <- melt(patch_compliant, measure.vars = c("odd_one","twin_1","twin_2"), variable.name="object", value.name="chosen")
ms <- ddply(md, .(object, Answer.item), #Answer.item, 
            summarise, 
            c = mean(chosen),
            n = sum(chosen), 
            l = length(chosen),
            sdc = sd(chosen),
            c.cih = c + (sdc/l^.5),
            c.cil = c - (sdc/l^.5))


ms$item <- factor(ms$Answer.item)
levels(ms$item) <- c("boat","friend", "pizza", "snowman", "sundae", "Christmas tree")

ggplot(ms, aes(x= item, y=c, fill=object)) + 
  geom_bar(position=position_dodge()) + 
  geom_linerange(aes(ymin=c.cil,ymax=c.cih), 
                 position=position_dodge(width=.9)) + 
  ylab("Probability of choosing")

head(patch_compliant)

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

patch_table <- aggregate(cbind(odd_one,
                               twin_1,
                               twin_2) ~ 
                           Answer.linguistic_framing_condition , data=patch_compliant, sum)
md <- melt(patch_table, id.vars=c("Answer.item","Answer.linguistic_framing_condition"))



qplot()