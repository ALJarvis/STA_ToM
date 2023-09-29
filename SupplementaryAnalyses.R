#load packages
library(tidyverse)
library(dplyr)
library(rstatix)
library(readxl)


library(readxl)
TOM_STA <- read_excel("C:/Users/jaral005/OneDrive - University of South Australia/Desktop/STA/Datasets/TOM_STA.xlsx", 
                      na = "NA")
View(TOM_STA)

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

#Create separate dfs for yes and no items
do_think_yes <- do_think %>%
  filter(q_type %in% c("do", "think"), correct_response == 1)

do_think_no <- do_think %>%
  filter(q_type %in% c("do", "think"), correct_response == 2)

#calculate the average question response across all questions for each participant & label this new variable 'meanEndorse'
do_think_yes <- do_think_yes %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

do_think_no <- do_think_no %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

#re-arrange data to wide format
do_think_yes <- do_think_yes %>%
  spread(key = sub_category, value = meanEndorse)

do_think_no <- do_think_no %>%
  spread(key = sub_category, value = meanEndorse) 

#change dataframe to match with the STA sample data
do_think_yes <- data.frame(do_think_yes)
class(do_think_yes)

do_think_no <- data.frame(do_think_no)
class(do_think_no)

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = do_think_yes, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

staPLOT(data = do_think_no, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out1 = staCMR (data=do_think_yes)
out1$fval #fit value is 37.53

out2 = staCMR (data=do_think_no)
out1$fval #fit value is 

#add 1D predictions to STA plot
staPLOT(data = do_think_yes, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out1$x)

staPLOT(data = do_think_no, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out2$x)


# find p-value 
out1 = staCMRFIT(do_think_yes , nsample=10000)
out1$p # p = <.001

out2 = staCMRFIT(do_think_no , nsample=10000)
out2$p # p = <.001


# Do and feel yes ---------------------------------------------------------

#Label dependent variables
dv1 = "do" #x-axis
dv2 = "feel"  #y-axis

#Pull out rows with do and feel questions and assign do_feel 
do_feel <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#Create separate dfs for yes and no items
do_feel_yes <- do_feel %>%
  filter(q_type %in% c("do", "feel"), correct_response == 1)

do_feel_no <- do_feel %>%
  filter(q_type %in% c("do", "feel"), correct_response == 2)

#calculate the average question response across all questions for each participant & label this new variable 'meanEndorse'
do_feel_yes <- do_feel_yes %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

do_feel_no <- do_feel_no %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

#re-arrange data to wide format
do_feel_yes <- do_feel_yes %>%
  spread(key = sub_category, value = meanEndorse)

do_feel_no <- do_feel_no %>%
  spread(key = sub_category, value = meanEndorse) 

#change dataframe to match with the STA sample data
do_feel_yes <- data.frame(do_feel_yes)
class(do_feel_yes)

do_feel_no <- data.frame(do_feel_no)
class(do_feel_no)

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = do_feel_yes, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

staPLOT(data = do_feel_no, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out3 = staCMR (data=do_feel_yes)
out3$fval #fit value is 51.82

out4 = staCMR (data=do_feel_no)
out4$fval #38.49

#add 1D predictions to STA plot
staPLOT(data = do_feel_yes, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out3$x)

staPLOT(data = do_feel_no, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out4$x)


# find p-value 
out3 = staCMRFIT(do_feel_yes , nsample=10000)
out3$p # p = <.001

out4 = staCMRFIT(do_feel_no , nsample=10000)
out4$p # p = <.001



# Think and feel yes ------------------------------------------------------
#Label dependent variables
dv1 = "think" #x-axis
dv2 = "feel"  #y-axis

#Pull out rows with think and feel questions and assign think_feel 
think_feel <- TOM_STA[TOM_STA$q_type %in% c(dv1, dv2),]

#Create separate dfs for yes and no items
think_feel_yes <- think_feel %>%
  filter(q_type %in% c("feel", "think"), correct_response == 1)

think_feel_no <- think_feel %>%
  filter(q_type %in% c("feel", "think"), correct_response == 2)

#calculate the average question response across all questions for each participant & label this new variable 'meanEndorse'
think_feel_yes <- think_feel_yes %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

think_feel_no <- think_feel_no %>%
  filter(!is.na(q_response)) %>% # filter out na responses
  group_by(ID, btwn_groups, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

#re-arrange data to wide format
think_feel_yes <- think_feel_yes %>%
  spread(key = sub_category, value = meanEndorse)

think_feel_no <- think_feel_no %>%
  spread(key = sub_category, value = meanEndorse) 

#change dataframe to match with the STA sample data
think_feel_yes <- data.frame(think_feel_yes)
class(think_feel_yes)

think_feel_no <- data.frame(think_feel_no)
class(think_feel_no)

#draw plot, conduct CMR, add predicted points to plot:
staPLOT(data = think_feel_yes, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

staPLOT(data = think_feel_no, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1))

#Find best-fitting monotonic points and print fit value 
out5 = staCMR (data=think_feel_yes)
out5$fval #fit value is 11.22

out6 = staCMR (data=think_feel_no)
out6$fval #fit value is 103.55

#add 1D predictions to STA plot
staPLOT(data = think_feel_yes, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out5$x)

staPLOT(data = think_feel_no, groups = list( c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)),
        grouplabels = list("Low-Lies", "Low-Paradoxical sarcasm", "Low-Sarcasm", "Low-Simple sarcasm", "Low-Sincere", 
                           "High-Lies", "High-Paradoxical sarcasm", "High-Sarcasm", "High-Simple sarcasm", "High-Sincere"),
        axislabels = list(dv1, dv2), 
        xlim = c(0,1), ylim = c(0,1), pred = out6$x)


# find p-value 
out5 = staCMRFIT(think_feel_yes , nsample=10000)
out5$p # p = .021

out6 = staCMRFIT(think_feel_no , nsample=10000)
out6$p # p = <.001