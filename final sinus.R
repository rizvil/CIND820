install.packages("ggplot2")
install.packages("plotROC")
install.packages("pROC")
install.packages("ROCR")
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
library(e1071)
library(GGally)
library(pROC)
library(ROCR)


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

#organizebymasktype$cough_30d_f1[organizebymasktype$cough_30d_f1 ==2] <-0
#organizebymasktype$cough_cons_f1 [organizebymasktype$cough_cons_f1 ==2] <- 0
#organizebymasktype$cough_freq_f1[organizebymasktype$cough_freq_f1 == 2] <-0
#organizebymasktype$asthma_f1[organizebymasktype$asthma_f1 == 2] <-0
#organizebymasktype$asthma_12m_f1[organizebymasktype$asthma_12m_f1 == 2] <-0
#organizebymasktype$sinus_30d_f1[organizebymasktype$sinus_30d_f1 ==2] <-0
#organizebymasktype$wheeze_30d_f1[organizebymasktype$wheeze_30d_f1 ==2] <-0
#organizebymasktype$brthless_30d_f1[organizebymasktype$brthless_30d_f1 ==2] <-0

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
  filter(!is.na(sinus_30d_f1))

sinus30d$age_911grp <- as.character(sinus30d$age_911grp)

sinus30d = sinus30d %>%
  filter(age_911grp != "<18")

sinus30d$age_911grp <- as.factor(sinus30d$age_911grp)

sinus30d$Mask_type <- as.character(sinus30d$Mask_type)

sinus30d = sinus30d %>%
  filter(Mask_type != "3")

sinus30d$Mask_type <- as.factor(sinus30d$Mask_type)

sinus30d = sinus30d %>%
  dplyr::mutate(m3_fittest_f1 = replace_na(m3_fittest_f1, 2))


sinus30d$m3_fittest_f1[sinus30d$m3_fittest_f1==3] <- 2
sinus30d$m3_fittest_f1 <- as.character(sinus30d$m3_fittest_f1)

sinus30d = sinus30d %>%
  filter(m3_fittest_f1 != "8")


sinus30d$m3_fittest_f1 <- as.factor(sinus30d$m3_fittest_f1)


summary(sinus30d)

sinus30d = na.omit(sinus30d)

summary(sinus30d)

write.csv(sinus30d, "sinus30d.csv")


  


masksinusplot <- ggplot(sinus30d, 
                        aes(x= Mask_type, fill = sinus_30d_f1)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=..count..), stat = "count", position=position_fill(vjust=0.5))+
  labs(y= "Percentage", x= "Mask Type", title = "Sinus Disease based on Mask Type Group")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "Sinus Disese",
                    breaks =c("1", "2"),
                    labels =c("Yes", "No"))

masksinusplot




#Logistic Regression Model 

# The objective of this project is to determine if the different types of face mask worn
# has an effect on respiratory outcome - here sinus outcome. 
# Lets look at the relationship between mask and sinus outcome
# Mask_type = 4 represents no mask, we will use this as reference

set.seed(123)

n = nrow(sinus30d)
trainindex <- sample(1:n, size = round(0.7*n))
sinusmodel_train <- sinus30d[trainindex, ]
sinusmodel_testing <- sinus30d[-trainindex, ]


glm_sinus_justmask <- glm(relevel(factor(sinus_30d_f1), ref=2) ~ relevel(factor(Mask_type), ref = "4"),
                          data = sinusmodel_train, family =binomial(link = logit))

summary(glm_sinus_justmask)

pred_glm_sinus_justmask <- predict(glm_sinus_justmask, sinusmodel_train, type = "response")
pred_glm_sinus_justmask

prediction_glm_justmask <- as.factor(ifelse(pred_glm_sinus_justmask > 0.5, 1,2))
cm_glm_sinus_justmask <- confusionMatrix(prediction_glm_justmask, sinusmodel_train$sinus_30d_f1)

cm_glm_sinus_justmask 



##Feature Selection 
#using Chi-square test to select variables that are statistically significant - p<0.05 for the sinus outcome


rowsdata <- c("Mask_type", "training", "m3_fittest_f1", 
              "dust_cat", "smkstatus", "occ_trans_e", "gender", 
              "age_911grp", "new_education", "census_new", "income_new",
              "boroughcode", "mar_911_new",  "mar_new", 
              "curr_employment", "new_employed_911")

outcome <- c("sinus_30d_f1")


sinus_chifeatureselection <- CreateTableOne(
  factorVars = rowsdata,
  data = sinus30d, 
  strata = outcome,
  includeNA = T, 
  smd = T, 
  addOverall = FALSE,
  test = T,
  testApprox = chisq.test,
  testExact = fisher.test)

sinus_chifeatureselection <- print(sinus_chifeatureselection, 
                             catDigits = 1,
                             showAllLevels = T,
                             test = T,
                             missing = T, 
                             quote = F,
                             explain = F)


write.csv(sinus_chifeatureselection, "sinus_chifeatureselection.csv")


#Information Gain to select features

attribute_infogain <-information_gain(formula = sinus_30d_f1 ~., data = sinus30d)

infogain <- attribute_infogain %>%
  arrange(desc(importance))
  
write.csv(infogain, "infogain.csv")  

#chi-square variable - Mask_type + training + dust_cat + smkstatus + smkstatus + occ_trans_e + new_education + income_new + mar_new + curr_employment  
#information gain - Mask_type + new_education + dust_cat + curr_employment  + income_new + occ_trans_e +   mar_new +  mar_911_new +  age_911grp  + smkstatus


set.seed(123)
glm_sinus_chimodel <- glm(sinus_30d_f1 ~ relevel(factor(Mask_type), ref = "4") + training + 
                            dust_cat + smkstatus + occ_trans_e +
                            new_education + income_new + mar_new + curr_employment,
                      data = sinusmodel_train,
                      family =binomial(link = logit))


pred_glm_sinus_chimodel <- predict(glm_sinus_chimodel, sinusmodel_train)
pred_glm_sinus_chimodel

summary(glm_sinus_chimodel)

prediction_glm_sinus_chimodel <- as.factor(ifelse(pred_glm_sinus_chimodel > 0.5, 1,2))
cm_glm_sinus_chimodel <- confusionMatrix(prediction_glm_sinus_chimodel, sinusmodel_train$sinus_30d_f1)

cm_glm_sinus_chimodel

AUC_glm_sinus_chimodel <- prediction(as.numeric(prediction_glm_sinus_chimodel), as.numeric(sinusmodel_train$sinus_30d_f1))
AUC_glm_sinus_chimodel_performance_value <- performance(AUC_glm_sinus_chimodel, measure = "auc")
AUC_glm_sinus_chimodel_performance <- performance(AUC_glm_sinus_chimodel, measure='tpr', x.measure='fpr')
AUC_glm_sinus_chimodel_value <- unlist(AUC_glm_sinus_chimodel_performance_value@y.values)
AUC_glm_sinus_chimodel_value <- as.data.frame(AUC_glm_sinus_chimodel_value)



set.seed(123)
glm_sinus_igmodel <- glm(relevel(factor(sinus_30d_f1), ref = "2") ~ relevel(factor(Mask_type), ref = "4") + new_education + 
                           dust_cat + curr_employment  +
                           income_new + occ_trans_e +   mar_new +  mar_911_new +  age_911grp  + smkstatus,
                         data = sinusmodel_train,
                          family =binomial(link = logit))


pred_glm_sinus_igmodel <- predict(glm_sinus_igmodel, sinusmodel_train)
pred_glm_sinus_igmodel

summary(glm_sinus_igmodel)

prediction_glm_sinus_igmodel <- as.factor(ifelse(pred_glm_sinus_igmodel > 0.5, 1,2))
cm_glm_sinus_igmodel <- confusionMatrix(prediction_glm_sinus_igmodel, sinusmodel_train$sinus_30d_f1)

cm_glm_sinus_igmodel


AUC_glm_sinus_igmodel <- prediction(as.numeric(prediction_glm_sinus_igmodel), as.numeric(sinusmodel_train$sinus_30d_f1))
AUC_glm_sinus_igmodel_performance_value <- performance(AUC_glm_sinus_igmodel, measure = "auc")
AUC_glm_sinus_igmodel_performance <- performance(AUC_glm_sinus_igmodel, measure='tpr', x.measure='fpr')
AUC_glm_sinus_igmodel_value <- unlist(AUC_glm_sinus_igmodel_performance_value@y.values)
AUC_glm_sinus_igmodel_value <- as.data.frame(AUC_glm_sinus_igmodel_value)




#####random forest###########



set.seed(123)
rfsinus_chimodel <- randomForest(sinus_30d_f1 ~ Mask_type + training + dust_cat + smkstatus + 
                                occ_trans_e + new_education + income_new + mar_new + curr_employment,
                              data = sinusmodel_train, 
                              ntree = 500, 
                              importance=T)

summary(rfsinus_chimodel)

importance(rfsinus_chimodel)

varImpPlot(rfsinus_chimodel)

pred_rf_sinus_chimodel <- predict(rfsinus_chimodel, sinusmodel_train)
cm_rf_sinus_chimodel <- confusionMatrix(pred_rf_sinus_chimodel, sinusmodel_train$sinus_30d_f1)
cm_rf_sinus_chimodel


AUC_rf_sinus_chimodel <- prediction(as.numeric(pred_rf_sinus_chimodel), as.numeric(sinusmodel_train$sinus_30d_f1))
AUC_rf_sinus_chimodel_performance_value <- performance(AUC_rf_sinus_chimodel, measure = "auc")
AUC_rf_sinus_chimodel_performance <- performance(AUC_rf_sinus_chimodel, measure='tpr', x.measure='fpr')
AUC_rf_sinus_chimodel_value <- unlist(AUC_rf_sinus_chimodel_performance_value@y.values)
AUC_rf_sinus_chimodel_value <- as.data.frame(AUC_rf_sinus_chimodel_value)



importance(rfsinus_chimodel)
set.seed(123)
rfsinus_igmodel <- randomForest(sinus_30d_f1 ~ Mask_type + new_education + 
                                  dust_cat + curr_employment  +
                                  income_new + occ_trans_e +   mar_new +  mar_911_new +  
                                  age_911grp  + smkstatus,
                                 data = sinusmodel_train, 
                                 ntree = 100, 
                                 importance=T)

summary(rfsinus_igmodel)

importance(rfsinus_igmodel)

varImpPlot(rfsinus_igmodel)

pred_rf_sinus_igmodel <- predict(rfsinus_igmodel, sinusmodel_train)
cm_rf_sinus_igmodel <- confusionMatrix(pred_rf_sinus_igmodel, sinusmodel_train$sinus_30d_f1)
cm_rf_sinus_igmodel



AUC_rf_sinus_igmodel <- prediction(as.numeric(pred_rf_sinus_igmodel), as.numeric(sinusmodel_train$sinus_30d_f1))
AUC_rf_sinus_igmodel_performance_value <- performance(AUC_rf_sinus_igmodel, measure = "auc")
AUC_rf_sinus_igmodel_performance <- performance(AUC_rf_sinus_igmodel, measure='tpr', x.measure='fpr')
AUC_rf_sinus_igmodel_value <- unlist(AUC_rf_sinus_igmodel_performance_value@y.values)
AUC_rf_sinus_igmodel_value <- as.data.frame(AUC_rf_sinus_igmodel_value)


#####naiveBayes#######



set.seed(123)
#naive_sinus_chimodel <- naiveBayes(sinus_30d_f1 ~ Mask_type + training + dust_cat + smkstatus + 
                                     #occ_trans_e + new_education + income_new + mar_new + curr_employment,
                                    #data = sinusmodel_train)



a = sinusmodel_train %>% 
  select( Mask_type,  training , dust_cat , smkstatus , 
          occ_trans_e , new_education , income_new , mar_new , curr_employment,)

b= sinusmodel_train$sinus_30d_f1


naive_sinus_chimodel <- train(a,b, 'nb', trControl = trainControl(method = 'cv', number=10))  
naive_sinus_chimodel



pred_naive_sinus_chimodel <- predict(naive_sinus_chimodel, sinusmodel_train)
cm_naive_sinus_chimodel <- confusionMatrix(pred_naive_sinus_chimodel, sinusmodel_train$sinus_30d_f1)
cm_naive_sinus_chimodel



#AUC of Naive Chi

AUC_naive_sinus_chimodel <- prediction(as.numeric(pred_naive_sinus_chimodel), as.numeric(sinusmodel_train$sinus_30d_f1))
AUC_naive_sinus_chimodel_performance_value <- performance(AUC_naive_sinus_chimodel, measure = "auc")
AUC_naive_sinus_chimodel_performance <- performance(AUC_naive_sinus_chimodel, measure='tpr', x.measure='fpr')
AUC_naive_sinus_chimodel_value <- unlist(AUC_naive_sinus_chimodel_performance_value@y.values)
AUC_naive_sinus_chimodel_value <- as.data.frame(AUC_naive_sinus_chimodel_value)

chinbvarimp <- varImp(naive_sinus_chimodel)
plot(chinbvarimp)



set.seed(123)
#naive_sinus_igmodel <- naiveBayes(sinus_30d_f1 ~ Mask_type + new_education + 
                                   # dust_cat + curr_employment  +
                                    #income_new + occ_trans_e +   mar_new +  mar_911_new +  
                                    #age_911grp  + smkstatus,
                                     #data = sinusmodel_train)



c = sinusmodel_train %>% 
  select( Mask_type, new_education ,
            dust_cat , curr_employment  ,
            income_new , occ_trans_e ,   mar_new ,  mar_911_new ,  
            age_911grp  , smkstatus)

d= sinusmodel_train$sinus_30d_f1


naive_sinus_igmodel <- train(c,d, 'nb', trControl = trainControl(method = 'cv', number=10))  
naive_sinus_igmodel


naive_sinus_igmodel



pred_naive_sinus_igmodel <- predict(naive_sinus_igmodel, sinusmodel_train)
cm_naive_sinus_igmodel <- confusionMatrix(pred_naive_sinus_igmodel, sinusmodel_train$sinus_30d_f1)
cm_naive_sinus_igmodel

ignbvarimp <- varImp(naive_sinus_igmodel)
plot(ignbvarimp)

AUC_naive_sinus_igmodel <- prediction(as.numeric(pred_naive_sinus_igmodel), as.numeric(sinusmodel_train$sinus_30d_f1))
AUC_naive_sinus_igmodel_performance_value <- performance(AUC_naive_sinus_igmodel, measure = "auc")
AUC_naive_sinus_igmodel_performance <- performance(AUC_naive_sinus_igmodel, measure='tpr', x.measure='fpr')
AUC_naive_sinus_igmodel_value <- unlist(AUC_naive_sinus_igmodel_performance_value@y.values)
AUC_naive_sinus_igmodel_value <- as.data.frame(AUC_naive_sinus_igmodel_value)
                       

sinus_model_compare <- data.frame(Model = c('RF Chi',
                                      'RF IG',
                                      'LR Chi',
                                      'LR IG',
                                      'NB Chi',
                                      'NB IG'),
                            Accuracy = c(cm_rf_sinus_chimodel$overall[1],
                                         cm_rf_sinus_igmodel$overall[1],
                                         cm_glm_sinus_chimodel$overall[1],
                                         cm_glm_sinus_igmodel$overall[1],
                                         cm_naive_sinus_chimodel$overall[1],
                                         cm_naive_sinus_igmodel$overall[1]),
                            Accuracy_min = c(cm_rf_sinus_chimodel$overall[3],
                                         cm_rf_sinus_igmodel$overall[3],
                                         cm_glm_sinus_chimodel$overall[3],
                                         cm_glm_sinus_igmodel$overall[3],
                                         cm_naive_sinus_chimodel$overall[3],
                                         cm_naive_sinus_igmodel$overall[3]),
                            Accuracy_max = c(cm_rf_sinus_chimodel$overall[4],
                                             cm_rf_sinus_igmodel$overall[4],
                                             cm_glm_sinus_chimodel$overall[4],
                                             cm_glm_sinus_igmodel$overall[4],
                                             cm_naive_sinus_chimodel$overall[4],
                                             cm_naive_sinus_igmodel$overall[4]),
                            Sensitivity = c(cm_rf_sinus_chimodel$byClass[1],
                                            cm_rf_sinus_igmodel$byClass[1],
                                            cm_glm_sinus_chimodel$byClass[1],
                                            cm_glm_sinus_igmodel$byClass[1],
                                            cm_naive_sinus_chimodel$byClass[1],
                                            cm_naive_sinus_igmodel$byClass[1]),
                            AUC = c(AUC_rf_sinus_chimodel_value$AUC_rf_sinus_chimodel_value, 
                                    AUC_rf_sinus_igmodel_value$AUC_rf_sinus_igmodel_value,
                                    AUC_glm_sinus_chimodel_value$AUC_glm_sinus_chimodel_value , 
                                    AUC_glm_sinus_igmodel_value$AUC_glm_sinus_igmodel_value,
                                    AUC_naive_sinus_igmodel_value$AUC_naive_sinus_igmodel_value,
                                    AUC_naive_sinus_igmodel_value$AUC_naive_sinus_igmodel_value))
                            

sinus_model_compare

ggplot(aes(x=Model, y=Sensitivity), data=sinus_model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Sensitivity of Models') +
  xlab('Models') +
  ylab('Sensitivity')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent)                                                      

ggplot(aes(x=Model, y=Accuracy), data=sinus_model_compare) +
  geom_point(stat='identity',  color = "red") +
  labs(y= "Overall Accuracy", x= "Models", title = "Comparative Accuracy of Models")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_errorbar(aes(ymin=Accuracy_min, ymax=Accuracy_max), width=.1, color = "blue")+ 
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


model_compare
  
  

#Roc


roc_rf_ch <- data.frame(fpr = unlist(AUC_rf_sinus_chimodel_performance@x.values), tpr=unlist(AUC_rf_sinus_chimodel_performance@y.values))
roc_rf_ch$method <- "RF Chi"
roc_rf_ig <- data.frame(fpr = unlist(AUC_rf_sinus_igmodel_performance@x.values), tpr=unlist(AUC_rf_sinus_igmodel_performance@y.values))
roc_rf_ig$method <- "RF IG"
roc_glm_ch <- data.frame(fpr = unlist(AUC_glm_sinus_chimodel_performance@x.values), tpr=unlist(AUC_glm_sinus_chimodel_performance@y.values))
roc_glm_ch$method <- "GLM Chi"
roc_glm_ig <- data.frame(fpr = unlist(AUC_glm_sinus_igmodel_performance@x.values), tpr=unlist(AUC_glm_sinus_igmodel_performance@y.values))
roc_glm_ig$method <- "GLM IG"
roc_nb_ch <- data.frame(fpr = unlist(AUC_naive_sinus_chimodel_performance@x.values), tpr=unlist(AUC_naive_sinus_chimodel_performance@y.values))
roc_nb_ch$method <- "NB Chi"
roc_nb_ig <- data.frame(fpr = unlist(AUC_naive_sinus_igmodel_performance@x.values), tpr=unlist(AUC_naive_sinus_igmodel_performance@y.values))
roc_nb_ig$method <- "NB IG"


sinus_AUC <- rbind(roc_rf_ch, roc_rf_ig, roc_glm_ch, roc_glm_ig, roc_nb_ch, roc_nb_ig) %>%
  ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
  geom_line() +
  geom_abline(linetype=2) +
  labs(y= "True Postive Rate", x= "False Positive Rate", title = "Receiver operating characteristic (ROC) curve of Models")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(lim=c(0,1)) +
  scale_y_continuous(lim=c(0,1)) +
  theme(legend.position=c(0.8,0.2), legend.title=element_blank())






###running the model on the test dataset####


set.seed(123)
rfsinus_igmodel <- randomForest(sinus_30d_f1 ~ Mask_type + new_education + 
                                  dust_cat + curr_employment  +
                                  income_new + occ_trans_e +   mar_new +  mar_911_new +  
                                  age_911grp  + smkstatus,
                                data = sinusmodel_testing, 
                                ntree = 500, 
                                importance=T)

summary(rfsinus_igmodel)

importance(rfsinus_igmodel)

pred_rf_sinus_igmodelt <- predict(rfsinus_igmodel, sinusmodel_testing)
cm_rf_sinus_igmodelt <- confusionMatrix(pred_rf_sinus_igmodelt, sinusmodel_testing$sinus_30d_f1)
cm_rf_sinus_igmodelt

