install.packages("MASS")
install.packages("corrplot")
install.packages("FSelectorRcpp")
library(tidyverse)
library(dplyr)
library(reshape2)
library(tableone)
library(plyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(ggcorrplot)
library(FSelectorRcpp)
library(randomForest)
library(caret)


wave2 = read_csv("wave2-dataset.csv")

#modifying the dataset to able to compare the effects of type of masks on respiratory health outcomes, 
#RRWVs that used the same type of mask in both time periods (Sept 11, 2001 and between Sept 12, 2001 to Dec 31, 2001) 
##need to be grouped together. If a survey responder, didn’t respond to the face mask questions in either Sept 11, 2001 or between 
#Sept 12, 2001 to Dec 31, 2001 time point, they will be excluded from the analysis. 
#If they reported using differing types of face mask in two time periods, there will excluded.
##



organizebymasktype = wave2 %>% 
  select(worker_e, type_911, type_912, training, m3_fittest_f1, dust_cat, smkstatus, occ_trans_e, 
         cough_30d_f1, cough_freq_f1, cough_cons_f1, asthma_f1, asthma_12m_f1, sinus_30d_f1, wheeze_30d_f1, brthless_30d_f1,
         gender, age_911grp, new_education, census_new, income_new, boroughcode, mar_911_new, mar_new, curr_employment, new_employed_911)%>%
  filter(worker_e == "Yes")%>%
  mutate(Mask_type = case_when(type_911 == "1" & type_912 == "1" ~ 1, 
                               type_911 == "2" & type_912 =="2" ~ 2,
                               type_911 == "3" & type_912 =="3" ~ 3,
                               type_911 == "4" & type_912 =="4" ~ 4))%>%
  filter(!is.na(Mask_type))%>%
  dplyr::relocate(Mask_type)


#relabel outcomes measures from no labeled as 2 in the dataset to 0 

organizebymasktype$cough_30d_f1[organizebymasktype$cough_30d_f1 ==2] <-0
organizebymasktype$cough_cons_f1 [organizebymasktype$cough_cons_f1 ==2] <- 0
organizebymasktype$cough_freq_f1[organizebymasktype$cough_freq_f1 == 2] <-0
organizebymasktype$asthma_f1[organizebymasktype$asthma_f1 == 2] <-0
organizebymasktype$asthma_12m_f1[organizebymasktype$asthma_12m_f1 == 2] <-0
organizebymasktype$sinus_30d_f1[organizebymasktype$sinus_30d_f1 ==2] <-0
organizebymasktype$wheeze_30d_f1[organizebymasktype$wheeze_30d_f1 ==2] <-0
organizebymasktype$brthless_30d_f1[organizebymasktype$brthless_30d_f1 ==2] <-0

write.csv(organizebymasktype, "maskdata.csv")

#Data Preparation for the sinus outcome. For this submission, I will focus on Sinus outcome prediction based on mask type.

maskdata <- read.csv("maskdata.csv")

#First removing the other outcomes measures so we can focus on the sinus outcome 
#and removing the columns used to create the Mask_type column

sinus30d = maskdata %>%
  select(-worker_e, -type_911, -type_912, -cough_30d_f1, 
         -cough_freq_f1, -cough_cons_f1, -asthma_f1, -asthma_12m_f1,
         -wheeze_30d_f1, -brthless_30d_f1, -X)

##Removing the following: 
##there are only 2 people with "<18" (too small of a cohort) 
##Group 3 mask type - compared to Group Mas 1,2, and 4 - this ia small amount of data that represents the other type of mask
##outlier 8 - removing as this data is not part of the data codebook
#removing all NA. All data is categorical so, I did not impute data.

sinus30d <- sinus30d %>%
  mutate_if(is.integer, as.factor)

summary(sinus30d)

sinus30d = sinus30d %>%
  filter(age_911grp != "<18")

sinus30d = sinus30d %>%
  filter(Mask_type != "3")


sinus30d = sinus30d %>%
  filter(!is.na(sinus_30d_f1))


sinus30d = sinus30d %>%
  filter(m3_fittest_f1 != 8)


sinus30d = na.omit(sinus30d)

summary(sinus30d)


#Logistic Regression Model 

# The objective of this project is to determine if the different types of face mask worn
# has an effect on respiratory outcome - here sinus outcome. 
# Lets look at the relationship between mask and sinus outcome
# Mask_type = 4 represents no mask, we will use this as reference

set.seed(123)

n = nrow(sinus30d)
trainindex <- sample(1:n, size = round(0.7*n))
Logit_sinusmodel_train <- sinus30d[trainindex, ]
Logit_sinusmodel_testing <- sinus30d[-trainindex, ]

sinusmodeljustmask <- glm(sinus_30d_f1 ~ relevel(factor(Mask_type), ref = "4"),
                          data = Logit_sinusmodel_train, family =binomial(link = logit))

summary(sinusmodeljustmask)

predictsinusmodeljustmask <- predict(sinusmodeljustmask, Logit_sinusmodel_train, type = "response")
predictsinusmodeljustmask

outcome_prediction_justmask <- as.factor(ifelse(predictsinusmodeljustmask > 0.5, 1,0))


confusionMatrix(outcome_prediction_justmask, Logit_sinusmodel_train$sinus_30d_f1)

#This justmask model does suggest that if the person went from No mask to Group 1 mask type (Type1 (full-face respiratory) or Type 2 (half-face respiratory),
#there is a log odd decrease of -0.50803 in develop sinus symtoms which is statistically significant.
#However, a log odd decrease of -0.04836 from no mask to Group 2 mask type (Group 2=Type 3 (disposable mask with N95 to P100 rating)) were not statistically significant
#as the accuracy (0.5863) of this model is low for detecting sinus disease . 
#It would good to explore if other variables in this dataset will help improve the accuracy.



##Feature Selection 
#using Chi-square test to select variables that are statistically significant - p<0.05 for the sinus outcome


rowsdata <- c("Mask_type", "training", "m3_fittest_f1", 
              "dust_cat", "smkstatus", "occ_trans_e", "gender", 
              "age_911grp", "new_education", "census_new", "income_new",
              "boroughcode", "mar_911_new",  "mar_new", 
              "curr_employment", "new_employed_911")

outcome <- c("sinus_30d_f1")


chifeatureselection <- CreateTableOne(
  factorVars = rowsdata,
  data = sinus30d, 
  strata = outcome,
  includeNA = T, 
  smd = T, 
  addOverall = FALSE,
  test = T,
  testApprox = chisq.test,
  testExact = fisher.test)

chifeatureselection <- print(chifeatureselection, 
                             catDigits = 1,
                             showAllLevels = T,
                             test = T,
                             missing = T, 
                             quote = F,
                             explain = F)


write.csv(chifeatureselection, "chifeatureselection.csv")




#Will also use this table to create demo table for this analysis. 

#Chi square from this table indicate these variables 6 had p =<0.05 -  Mask_type + training + dust_cat +  occ_trans_e +   new_education +  curr_employment, 

#Also explored Information Gain method to explore variables

attribute_infogain <-information_gain(formula = sinus_30d_f1 ~., data = sinus30d)
attribute_infogain %>%
  arrange(desc(importance))

#Looking at top 6 as chi square test suggested 6 statistically significant variables. 
#Top 6 included - Mask_type + curr_employment + new_education + training + dust_cat +  occ_trans_e +  mar_new
#mar_new was not significant as per chi square test and 
#training was ranked number 11 in importance however, it was statistically significant as per the chi-square test

#Plan
#model1 - includes both mar_new and training in the model
#model2 - excludes both mar_new and training in the model


#model1

sinus_model1 <- glm(sinus_30d_f1 ~ relevel(factor(Mask_type), ref = "4") + 
                   dust_cat + occ_trans_e + 
                   new_education +  curr_employment +
                     mar_new + training,
                  data = Logit_sinusmodel_train, family =binomial(link = logit))


predict_sinus_model1 <- predict(sinus_model1, Logit_sinusmodel_train, type = "response")
predict_sinus_model1

outcome_prediction_model1 <- as.factor(ifelse(predict_sinus_model1 > 0.5, 1,0))


confusionMatrix(outcome_prediction_model1, Logit_sinusmodel_train$sinus_30d_f1)

#model2

sinus_model2 <- glm(sinus_30d_f1 ~ relevel(factor(Mask_type), ref = "4") + 
                      dust_cat + occ_trans_e + 
                      new_education +  curr_employment,
                      data = Logit_sinusmodel_train, family =binomial(link = logit))


predict_sinus_model2 <- predict(sinus_model2, Logit_sinusmodel_train, type = "response")
predict_sinus_model2

outcome_prediction_model2 <- as.factor(ifelse(predict_sinus_model2 > 0.5, 1,0))


confusionMatrix(outcome_prediction_model2, Logit_sinusmodel_train$sinus_30d_f1)


#model1 is slightly better - Model1 accuracy = 0.6094, Model2 accuracy = 0.604 


#Random Forest to compare outcome with these model1 and model2
#number of trees picked on number of attributes x 10


randomforestsinus_model1 <- randomForest(sinus_30d_f1 ~ Mask_type + dust_cat + 
                                   occ_trans_e +  
                                   new_education + curr_employment +
                                  training + mar_new, 
                                  data = Logit_sinusmodel_train, ntree = 70, importance=T)

randomforestsinus_model1



randomforestsinus_model2 <- randomForest(sinus_30d_f1 ~ Mask_type + dust_cat + 
                                           occ_trans_e +  
                                           new_education + curr_employment,
                                         data = Logit_sinusmodel_train, ntree = 50, importance=T)


randomforestsinus_model2 

plot(randomforestsinus_model2)

#The OOB error rate for model 1 was 40.83% and for model 2 was 41.29% suggesting model 1 is slightly better.



#I would pick Logistic Regression Model 1 to testing data logit_sinusmodel_testing
