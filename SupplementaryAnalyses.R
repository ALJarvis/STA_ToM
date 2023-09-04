#load packages
library(tidyverse)
library(dplyr)
library(rstatix)
library(readxl)

#Import TOM_STA dataset


# Data wrangling ----------------------------------------------------------

#Remove unnecessary columns
TOM_STA <- TOM_STA[ , -which(names(TOM_STA) %in% c("participant_id", "social_inference"))]

#change answer_correct from labels to numbers - these will form the column names 
TOM_STA$answer_correct <- sapply(TOM_STA$answer_correct,as.numeric) #0= FALSE, 1=TRUE

#change correct_response and sub_category from numbers to labels - these will form the column names
TOM_STA$sub_category <- factor(TOM_STA$sub_category,
                               levels = c(1:5),
                               labels = c("sincere", "simple sarcasm", "paradoxical sarcasm", "lies", "sarcasm")  )

#re-name emotion to 'btwn_groups'
TOM_STA$btwn_groups <- TOM_STA$emotion

#Create new variable which combines q_no and sub_category
TOM_STA$question <- paste(TOM_STA$q_no, TOM_STA$sub_category, sep = "_")


# LOAD STACMR -------------------------------------------------------------

#### Load STA CMR into R ####

#first download from https://github.com/michaelkalish/STA/blob/master/STACMR-R/STACMR-R.pdf

#allocate more memory for Java
options(java.parameters = "-Xmx8192m") #8192m = 8GB

#Change R working directory to STACMR-R folder 
setwd("C:/Users/jaral005/Documents/RStudio/STA-master/STACMR-R")

#Link to java
source("staCMRsetup.R")



# Do and think ------------------------------------------------------------

#Label dependent variables
dv1 = "do" #x-axis
dv2 = "think"  #y-axis

#Pull out rows with do and think questions and assign do_think 
do_think <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#add a new variable called 'dv' which will be q_type converted into a numeric 1 or 2 
do_think$dv = 1
do_think$dv[do_think$q_type == dv2] <- 2

#calculate the average correct response across all questions for each participant
do_think <- do_think %>%
  filter(!is.na(answer_correct)) %>% # filter out na responses
  group_by(ID, btwn_groups, dv, sub_category) %>%
  summarize(meanCorrect = mean(answer_correct))

#re-arrange data to wide format
do_think <- do_think %>%
  spread(key = sub_category, value = meanCorrect) 

#change dataframe to match with the STA sample data
do_think <- data.frame(do_think)
class(do_think)

#check all variables are numeric except btwn_groups which should be a character 
str(do_think) 

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = do_think, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out1 = staCMR (data=do_think)
out1$fval #fit value is 29.15

#add 1D predictions to STA plot
staPLOT(data = do_think, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out1$x)

# find p-value 
out = staCMRFIT(do_think, nsample=10000)
out$p # p = <.001

#histogram of bootstrap distribution of fits
hist(out$fits, breaks = 50)



# Do and feel -------------------------------------------------------------

#Label dependent variables
dv1 = "do" #x-axis
dv2 = "feel"  #y-axis

#Pull out rows with do and think questions and assign do_feel 
do_feel <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#add a new variable called 'dv' which will be q_type converted into a numeric 1 or 2 
do_feel$dv = 1
do_feel$dv[do_feel$q_type == dv2] <- 2

#calculate the average correct response across all questions for each participant
do_feel <- do_feel %>%
  filter(!is.na(answer_correct)) %>% # filter out na responses
  group_by(ID, btwn_groups, dv, sub_category) %>%
  summarize(meanCorrect = mean(answer_correct))

#re-arrange data to wide format
do_feel <- do_feel %>%
  spread(key = sub_category, value = meanCorrect) 

#change dataframe to match with the STA sample data
do_feel <- data.frame(do_feel)
class(do_feel)

#check all variables are numeric except btwn_groups which should be a character 
str(do_feel) 

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = do_feel, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out1 = staCMR (data=do_feel)
out1$fval #fit value is 29.15

#add 1D predictions to STA plot
staPLOT(data = do_feel, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out1$x)
# find p-value 
out = staCMRFIT(do_feel, nsample=10000)
out$p # p = .021

#histogram of bootstrap distribution of fits
hist(out$fits, breaks = 50)



# Think and feel ----------------------------------------------------------

#Label dependent variables
dv1 = "think" #x-axis
dv2 = "feel"  #y-axis

#Pull out rows with do and think questions and assign think_feel 
think_feel <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#add a new variable called 'dv' which will be q_type converted into a numeric 1 or 2 
think_feel$dv = 1
think_feel$dv[think_feel$q_type == dv2] <- 2

#calculate the average correct response across all questions for each participant 
think_feel <- think_feel %>%
  filter(!is.na(answer_correct)) %>% # filter out na responses
  group_by(ID, btwn_groups, dv, sub_category) %>%
  summarize(meanCorrect = mean(answer_correct))

#re-arrange data to wide format
think_feel <- think_feel %>%
  spread(key = sub_category, value = meanCorrect) 

#change dataframe to match with the STA sample data
think_feel <- data.frame(think_feel)
class(think_feel)

#check all variables are numeric except btwn_groups which should be a character 
str(think_feel) 

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = think_feel, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out1 = staCMR (data=think_feel)
out1$fval #fit value is 29.15

#add 1D predictions to STA plot
staPLOT(data = think_feel, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out1$x)
# find p-value 
out = staCMRFIT(think_feel, nsample=10000)
out$p # p = .013

#histogram of bootstrap distribution of fits
hist(out$fits, breaks = 50)
