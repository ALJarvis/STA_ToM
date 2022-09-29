#Load packages
library(gtools)
library(psych)
library(tidyverse)
library(dplyr)
library(rstatix)
library(readxl)
library(ggpubr)
library(datarium)
library(ggplot2)
library(emmeans)
library(afex)
library(Hmisc)

#Import TOM_STA dataset

#### General formatting ####

#convert variables into factors
TOM_STA$sub_category <- factor(TOM_STA$sub_category)
TOM_STA$correct_response <- factor(TOM_STA$correct_response)
TOM_STA$emotion <- factor(TOM_STA$emotion)

#Re-name from numbers to labels
levels(TOM_STA$emotion) <- c("High", "Low")
levels(TOM_STA$sub_category) <- c("Sincere", "Simple sarcasm", "Paradoxical sarcasm", "Lies", "Sarcasm")
levels(TOM_STA$correct_response) <- c("Yes", "No")


#### Plot data ####

#First create meanEndorsement and filter out say questions 
full_STA <- TOM_STA %>%
  filter(!is.na(q_response)) %>% # filter out na trials
  filter(q_type != "say") %>% #filter out say questions
  group_by(ID, emotion, q_type, sub_category, correct_response) %>%
  dplyr::summarize(meanEndorse = mean(q_response))

#Re-order q_type levels
full_STA$q_type <- factor(full_STA$q_type, levels = c("think", "do", "feel"))

#Create tiff to save as image
tiff(filename = "anova_plot.tiff", width = 8, height = 10, units = "in", res = 600, compression = "lzw")

#Create colour palette
colours <- c("#56b4e9", "#0072B2")

#Plot data
b <- ggplot(data = full_STA) +
  aes(x = q_type, y = meanEndorse, fill = emotion) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5, position = position_dodge (0.9)) + #position_dodge centers error bars
  facet_wrap(sub_category ~ correct_response, nrow = 5) +
  xlab("Question type") +
  ylab("Participants' mean endorsements") +
  theme_classic() +
  scale_fill_manual(values=colours)  
plot(b)

#Save plot
dev.off()

#Detach library as it masks dplyr
detach(package:Hmisc, unload = TRUE)

#### ANOVA ####

#Check for extreme outliers
full_STA %>%
  group_by(emotion, sub_category) %>%
  identify_outliers(meanEndorse)

#Check for normality
full_STA %>%
  group_by(emotion, sub_category) %>%
  shapiro_test(meanEndorse) 

#Create QQ plots
ggqqplot(full_STA, "meanEndorse", ggtheme = theme_bw()) +
  facet_grid(emotion ~ sub_category, labeller = "label_both") 

#Check for homogeneity of variance
full_STA %>%
  group_by(sub_category) %>%
  levene_test(meanEndorse ~ emotion) 

#Run the ANOVA
aovfull <- aov_ez("ID", "meanEndorse", full_STA, between = c("emotion"), within = c("correct_response", "sub_category", "q_type"))
nice(aovfull, es="pes", correction = "GG")

#Planned contrasts
interaction1 <- emmeans(aovfull, "q_type", by = c("sub_category", "emotion")) #interactions
pairs(interaction1)

interaction2 <- emmeans(aovfull, "q_type", by = c("sub_category", "correct_response")) #interactions
pairs(interaction2)
