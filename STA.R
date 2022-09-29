#load packages
library(tidyverse)
library(dplyr)
library(rstatix)
library(readxl)

#Import TOM_STA dataset 

#### Broad data wrangling ####

#data needs to be in the following (wide) format:
#column 1: Participant number (for identification only, not used directly)
#column 2: Between-participant group (high versus low emotion perceptual ability)
#column 3: Dependent variable (numbered 1 or 2 to represent the q_type i.e. feel/think//do)
#columns 4+: Values for each within-participant condition (average question response spread across correct response & subcategory)

#Remove unnecessary columns
TOM_STA <- TOM_STA[ , -which(names(TOM_STA) %in% c("participant_id","answer_correct", "social_inference"))]

#change correct_response and sub_category from numbers to labels - these will form the column names
TOM_STA$correct_response <- factor(TOM_STA$correct_response,
                                   levels = c(1,2),
                                   labels = c("yes", "no") )
TOM_STA$sub_category <- factor(TOM_STA$sub_category,
                               levels = c(1:5),
                               labels = c("sincere", "simple sarcasm", "paradoxical sarcasm", "lies", "sarcasm")  )

#re-name emotion to 'btwn_groups'
TOM_STA$btwn_groups <- TOM_STA$emotion


#### Load STA CMR into R ####

#first download from https://github.com/michaelkalish/STA/blob/master/STACMR-R/STACMR-R.pdf

#allocate more memory for Java
options(java.parameters = "-Xmx8192m") #8192m = 8GB

#Change R working directory to STACMR-R folder 
setwd("C:/Users/jaral005/Documents/RStudio/STA-master/STACMR-R")

#Link to java
source("staCMRsetup.R")

#### DO VS THINK ####

#Label dependent variables
dv1 = "do" #x-axis
dv2 = "think"  #y-axis

#Pull out rows with do and think questions and assign do_think 
do_think <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#add a new variable called 'dv' which will be q_type converted into a numeric 1 or 2 
do_think$dv = 1
do_think$dv[do_think$q_type == dv2] <- 2

#calculate the average question response across all questions for each participant & label this new variable 'meanEndorse'
do_think <- do_think %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, dv, correct_response, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

#Ensure NAs have been removed 
sum(is.nan(do_think$meanEndorse))
sum(is.nan(do_think$correct_response))

#create a new column which merges sub_category & correct_response ready for spreading data into wide format and name it 'corr_cat'
do_think$corr_cat <- paste(do_think$sub_category, do_think$correct_response, sep = "-")
View(do_think)

#remove the following columns which aren't needed any more (i.e. correct_response & sub_category)
do_think <- do_think[ , -which(names(do_think) %in% c("correct_response","sub_category"))]

#re-arrange data to wide format
do_think <- do_think %>%
  spread(key = corr_cat, value = meanEndorse)  

#change dataframe to match with the STA sample data
do_think <- data.frame(do_think)
class(do_think)

#check all variables are numeric except btwn_groups which should be a character 
str(do_think) 

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = do_think, groups = list( c(1:2), c(3:4), c(5:6), c(7:8), c(9:10), c(11:12), c(13:14), c(15:16), c(17:18), c(19:20) ),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out1 = staCMR (data=do_think)
out1$fval #fit value is 101.03

#add 1D predictions to STA plot
staPLOT(data = do_think, groups = list( c(1:2), c(3:4), c(5:6), c(7:8), c(9:10), c(11:12), c(13:14), c(15:16), c(17:18), c(19:20) ),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out1$x)

# find p-value 
out = staCMRFIT(do_think, nsample=10000)
out$p # p = <.001

#histogram of bootstrap distribution of fits
hist(out$fits, breaks = 50)


#### THINK VS FEEL ####

#Label dependent variables
dv1 = "feel" #x-axis
dv2 = "think"  #y-axis

#Pull out rows with do and think questions and assign think_feel
think_feel <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#add a new variable called 'dv' which will be q_type converted into a numeric 1 or 2 
think_feel$dv = 1
think_feel$dv[think_feel$q_type == dv2] <- 2

#calculate the average question response across all questions for each participant & label this new variable 'meanEndorse'
think_feel <- think_feel %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, dv, correct_response, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

#Ensure NAs have been removed 
sum(is.nan(think_feel$meanEndorse))
sum(is.nan(think_feel$correct_response))

#create a new column which merges sub_category & correct_response ready for spreading data into wide format and name it 'corr_cat'
think_feel$corr_cat <- paste(think_feel$sub_category, think_feel$correct_response, sep = "-")
View(think_feel)

#remove the following columns which aren't needed any more (i.e. correct_response & sub_category)
think_feel <- think_feel[ , -which(names(think_feel) %in% c("correct_response","sub_category"))]

#re-arrange data to wide format
think_feel <- think_feel %>%
  spread(key = corr_cat, value = meanEndorse)  
View(think_feel)
think_feel <- think_feel %>%
  spread(key = corr_cat, value = meanEndorse)  

#change dataframe to match with the STA sample data
think_feel <- data.frame(think_feel)
class(think_feel)

#check all variables are numeric except btwn_groups which should be a character 
str(think_feel) 

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = think_feel, groups = list( c(1:2), c(3:4), c(5:6), c(7:8), c(9:10), c(11:12), c(13:14), c(15:16), c(17:18), c(19:20) ),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out1 = staCMR (data=think_feel)
out1$fval #fit value is 121.02

#add 1D predictions to STA plot
staPLOT(data = think_feel, groups = list( c(1:2), c(3:4), c(5:6), c(7:8), c(9:10), c(11:12), c(13:14), c(15:16), c(17:18), c(19:20) ),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out1$x)

# find p-value 
out = staCMRFIT(think_feel, nsample=10000)
out$p # p = <.001

#histogram of bootstrap distribution of fits
hist(out$fits, breaks = 50)

#### FEEL VS DO ####
#Label dependent variables
dv1 = "feel" #x-axis
dv2 = "do"  #y-axis

#Pull out rows with do and think questions and assign do_feel 
do_feel  <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#add a new variable called 'dv' which will be q_type converted into a numeric 1 or 2 
do_feel $dv = 1
do_feel $dv[do_feel $q_type == dv2] <- 2

#calculate the average question response across all questions for each participant & label this new variable 'meanEndorse'
do_feel  <- do_feel  %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, dv, correct_response, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

#Ensure NAs have been removed 
sum(is.nan(do_feel $meanEndorse))
sum(is.nan(do_feel $correct_response))

#create a new column which merges sub_category & correct_response ready for spreading data into wide format and name it 'corr_cat'
do_feel $corr_cat <- paste(do_feel $sub_category, do_feel $correct_response, sep = "-")
View(do_feel )

#remove the following columns which aren't needed any more (i.e. correct_response & sub_category)
do_feel  <- do_feel [ , -which(names(do_feel ) %in% c("correct_response","sub_category"))]

#re-arrange data to wide format
do_feel  <- do_feel  %>%
  spread(key = corr_cat, value = meanEndorse)  
View(do_feel )
do_feel  <- do_feel  %>%
  spread(key = corr_cat, value = meanEndorse)  

#change dataframe to match with the STA sample data
do_feel  <- data.frame(do_feel )
class(do_feel )

#check all variables are numeric except btwn_groups which should be a character 
str(do_feel ) 

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = do_feel , groups = list( c(1:2), c(3:4), c(5:6), c(7:8), c(9:10), c(11:12), c(13:14), c(15:16), c(17:18), c(19:20) ),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out1 = staCMR (data=do_feel )
out1$fval #fit value is 114.63

#add 1D predictions to STA plot
staPLOT(data = do_feel , groups = list( c(1:2), c(3:4), c(5:6), c(7:8), c(9:10), c(11:12), c(13:14), c(15:16), c(17:18), c(19:20) ),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out1$x)

# find p-value 
out = staCMRFIT(do_feel , nsample=10000)
out$p # p = <.001

#histogram of bootstrap distribution of fits
hist(out$fits, breaks = 50)