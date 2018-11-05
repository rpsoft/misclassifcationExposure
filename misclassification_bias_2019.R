#################################################
# MPH Advanced Epidemiology 2019                #
# Surveys and measurement biases                #
# Megan McMinn <megan.mcminn@glasgow.ac.uk      #
# Shiny App data & code                         #
# R 3.5.1
#################################################


#################################################
# Non-differential misclassifcation of exposure #
#################################################
# Hypothetical data and situation is taken from Rothman 2012 (Epidemiology: an introduction 2nd ed. p. 135)

## Background text to include as explanation:
# This table (true.dist) reprents hypothetical data for a Case-Control study investigating the effects of a high fat diet on the odds of having a subsequent Myocardial infarction. 
# The participant's diets are recorded, and classified as 'high fat' or 'not'. As there will be an arbitrary cut-off point at which a diet is regarded as being 'high fat',
# it is plausible that some of the participants will have their exposure status misclassified. 

## This first table shows the situation where all subjects are correctly classified with respect 
## to the outcome (i.e. MI), and the exposure (i.e. eating a high fat diet).
## In reality, this distribtion is almost never truely know. 

true.dist <- as.data.frame(matrix(c(250, 450, 100, 900), byrow = FALSE, nrow = 2, ncol = 2), row.names = c("Exposed", "Unexposed"))
colnames(true.dist) <- c("MI Case", "Control")
round(fisher.test(true.dist)$estimate,2)

## Now we investigate the effects of non-differential misclassification of their exposure status. 
## (i.e. some of the exposed subjects are incorrectly classified as 'not exposed' and vice versa.)

## Misclassifies exposure status (both ways). Disease status doesn't change. 
miscl.exposure <- function(true.data, exp.perc, unexp.perc) {
  ## First, calculate the table for the misclassified unexposed
  new.case.exp <- round(true.data[1,1] + (true.data[2,1]*unexp.perc),0)
  new.case.unexp <- sum(true.data[,1]) - new.case.exp
  new.table <- c(new.case.exp, new.case.unexp) ## column 1 cases
  new.cont.exp <- round(true.data[1,2] + (true.data[2,2]*unexp.perc),0)
  new.cont.unexp <- sum(true.data[,2]) - new.cont.exp
  new.table <- cbind(new.table, c(new.cont.exp, new.cont.unexp)) ## column 2 controls

  # Then do the misclassified exposed
  new.case.unexp <- round(new.table[2,1] + (true.data[1,1]*exp.perc),0)
  new.case.exp <- sum(new.table[,1]) - new.case.unexp
  new.table2 <- c(new.case.exp, new.case.unexp) ## column 1 cases
  new.cont.unexp <- round(new.table[2,2] + (true.data[1,2]*exp.perc),0)
  new.cont.exp <- sum(new.table[,2]) - new.cont.unexp
  new.table2 <- cbind(new.table2, c(new.cont.exp, new.cont.unexp)) ## column 2 controls
  
  colnames(new.table2) <- c("MI Case", "Control")
  rownames(new.table2) <- c("Exposed (High fat diet)", "Unexposed")
  new.table3 <- cbind(new.table2, rowSums(new.table2))
  new.table3 <- rbind(new.table3, colSums(new.table3))
  colnames(new.table3)[3] <- "Total"
  rownames(new.table3)[3] <- "Total"
  ## Results

  a = new.table3
  b = round(fisher.test(new.table2)$estimate,1)
  c = c(round(fisher.test(new.table2, conf.int = TRUE)$conf.int[1],1), round(fisher.test(new.table2, conf.int = TRUE)$conf.int[2],1))
  
  return(list(a,b,c))
 
 }

# This is the correct classification, giving us the true OR = 5.0
miscl.exposure(true.dist, 0, 0) -> D

D[[1]] %>% typeof()




# Now we consider the case where 20% of the 'unexposed' to having a high fat diet:
miscl.exposure(true.dist, exp.perc = 0, unexp.perc = 0.2)[[1]]
# Finally, where 20% of each exposure category are misclassified:
miscl.exposure(true.dist, exp.perc = 0.2, unexp.perc = 0.2)
miscl.exposure(true.dist, exp.perc = 0.5, unexp.perc = 0.7)

## In each case, we see that the non-differential misclassification of the exposure biases the result towards the null. 
## Nondifferential misclassification of a dichotomous exposure always biases toward the null. 

## Shiny app:
## I'm envisioning two sliders, allowing the students to change the amounts of exp.perc and unexp.perc (each ranging from 0 - 100%) 
## The table can be updated from the sliders, to show the new numbers of exp/unexposed in each group, and the OR results below. 
## (or keep a copy of the true.dist table, and the new table appears next to it to allow for comparison?)


###########################
# Non-participation bias  #
###########################

# Cases of uveal melanoma and the use of mobile phones
# use of mobile phones
# population controls

## This data are taken from a subset of a study exploring the effects of mobile phone use and the odds of being diagnosed with uveal melaoma. 
# 455 cases and 827 controls
# Asked to rate mobile phone use as 'Never', 'Sporodic' and 'Regular' (amongst other questions)

# This subset compares the 'Never' and 'Regular'  groups (so smaller numbers)
stang09_p <- as.data.frame(matrix(c(136, 297, 107, 165), byrow=TRUE, nrow = 2), row.names = c("RegularUse", "NoUse"))
colnames(stang09_p) <- c("Case", "Control")

# Non participants (persons who were invited to participate in the study, but refused) were asked to complete a short questionnaire, which asked
# to classify their mobile phone use as "not' or 'regular'
stang09_np_short <- as.data.frame(matrix(c(3, 72,  7, 212), byrow=TRUE, nrow = 2), row.names = c("RegularUse", "NoUse"))
colnames(stang09_np_short) <- c("Case", "Control")


# Using only data from the Participants of the study, finding borderline significant value, and a <1 OR, 
# suggesting that regular use is assoc with lower odds of developing melanoma
fisher.test(stang09_p) -> f1
round(fisher.test(stang09_p)$estimate,2) -> f1r
# 0.71

# Using only the data from the Non-participants, we find an OR > 1, and is not significant. 
fisher.test(stang09_np_short) -> f2
round(fisher.test(stang09_np_short)$estimate,2) ->f2r
# 1.26

## This shows the impact of non-participation bias

## Combining both participants and non-participants, we see the 'true' OR for this sample is 1.25. 
stang09_full <- stang09_p + stang09_np_short
fisher.test(stang09_full) -> ff
round(fisher.test(stang09_full)$estimate,2) ->ffr
# 1.25

## Discussion can be had on non-participants who refused to take part in the short questionnaire 

