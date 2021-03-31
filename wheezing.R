install.packages("ROSE")
install.packages("smotefamily")
install.packages("ggplot2")

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
library(ROSE)


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


Wheezing = maskdata %>%
  select(-worker_e, -type_911, -type_912, -cough_30d_f1, 
         -cough_freq_f1, -cough_cons_f1, -asthma_f1, -asthma_12m_f1, -sinus_30d_f1,
         -brthless_30d_f1, -X)

wheezing <- Wheezing %>%
  mutate_if(is.integer, as.factor)

summary(wheezing)

wheezing <- wheezing %>%
  filter(!is.na(wheeze_30d_f1))


wheezing = wheezing %>%
  dplyr::mutate(m3_fittest_f1 = replace_na(m3_fittest_f1, 2))
wheezing$m3_fittest_f1[wheezing$m3_fittest_f1==3] <- 2
wheezing$m3_fittest_f1 <- as.character(wheezing$m3_fittest_f1)

wheezing = wheezing %>%
  filter(m3_fittest_f1 != "8")

wheezing = na.omit(wheezing)


wheezing$m3_fittest_f1 <- as.factor(wheezing$m3_fittest_f1)

wheezing$age_911grp <- as.character(wheezing$age_911grp)

wheezing = wheezing %>%
  filter(age_911grp != "<18")

wheezing$age_911grp <- as.factor(wheezing$age_911grp)
         
wheezing$Mask_type <- as.character(wheezing$Mask_type)

wheezing = wheezing %>%
  filter(Mask_type != "3")

wheezing$Mask_type <- as.factor(wheezing$Mask_type)

write.csv(wheezing, "wheezing.csv")

maskwheezeplot <- ggplot(wheezing, 
                        aes(x= Mask_type, fill = wheeze_30d_f1)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=..count..), stat = "count", position=position_fill(vjust=0.5))+
  labs(y= "Percentage", x= "Mask Type", title = "Wheezing based on Mask Type Group")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "Wheezing",
                      breaks =c("2", "1"),
                      labels =c("No", "Yes"))

maskwheezeplot


table(wheezing$wheeze_30d_f1)

set.seed(123)

n = nrow(wheezing)
trainindex <- sample(1:n, size = round(0.7*n))
wheezemodel_train <- wheezing[trainindex, ]
wheezemodel_testing <- wheezing[-trainindex, ]


table(wheezemodel_train$wheeze_30d_f1)
table(wheezing$wheeze_30d_f1)


rowsdata <- c("Mask_type", "training", "m3_fittest_f1", 
              "dust_cat", "smkstatus", "occ_trans_e", "gender", 
              "age_911grp", "new_education", "census_new", "income_new",
              "boroughcode", "mar_911_new",  "mar_new", 
              "curr_employment", "new_employed_911")

outcome <- c("wheeze_30d_f1")


wheeze_chifeatureselection <- CreateTableOne(
  factorVars = rowsdata,
  data = wheezing, 
  strata = outcome,
  includeNA = T, 
  smd = T, 
  addOverall = FALSE,
  test = T,
  testApprox = chisq.test,
  testExact = fisher.test)

wheeze_chifeatureselection <- print(wheeze_chifeatureselection, 
                                   catDigits = 1,
                                   showAllLevels = T,
                                   test = T,
                                   missing = T, 
                                   quote = F,
                                   explain = F)


write.csv(wheeze_chifeatureselection, "wheeze_chifeatureselection.csv")




#Information Gain to select features

attribute_infogain <-information_gain(formula = wheeze_30d_f1 ~., data = wheezing)

wheeze_infogain <- attribute_infogain %>%
  arrange(desc(importance))

write.csv(wheeze_infogain, "wheeze_infogain.csv")  


#logistic regression

table(wheezemodel_train$wheeze_30d_f1)


relevel(factor(wheeze_30d_f1), ref=2)

set.seed(123)
glm_wheeze_chimodel <- glm(relevel(factor(wheeze_30d_f1), ref=2) ~ relevel(factor(Mask_type), ref = "4") + 
                             smkstatus +
                             dust_cat +
                             curr_employment +
                             new_education +
                             mar_911_new +
                             mar_new +
                             occ_trans_e +
                             income_new +
                             boroughcode,
                           data = wheezemodel_train,
                          family =binomial(link = logit))


pred_glm_wheeze_chimodel <- predict(glm_wheeze_chimodel, wheezemodel_train)
pred_glm_wheeze_chimodel

summary(glm_wheeze_chimodel)

prediction_glm_wheeze_chimodel <- as.factor(ifelse(pred_glm_wheeze_chimodel > 0.5, 2,1))
cm_glm_wheeze_chimodel <- confusionMatrix(prediction_glm_wheeze_chimodel, wheezemodel_train$wheeze_30d_f1)

cm_glm_wheeze_chimodel

AUC_glm_wheeze_chimodel <- prediction(as.numeric(prediction_glm_wheeze_chimodel), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_glm_wheeze_chimodel_performance_value <- performance(AUC_glm_wheeze_chimodel, measure = "auc")
AUC_glm_wheeze_chimodel_performance <- performance(AUC_glm_wheeze_chimodel, measure='tpr', x.measure='fpr')
AUC_glm_wheeze_chimodel_value <- unlist(AUC_glm_wheeze_chimodel_performance_value@y.values)
AUC_glm_wheeze_chimodel_value <- as.data.frame(AUC_glm_wheeze_chimodel_value)


#randomforest


set.seed(123)
rfwheeze_chimodel <- randomForest(wheeze_30d_f1 ~ Mask_type +
                                    smkstatus +
                                    dust_cat +
                                    curr_employment +
                                    new_education +
                                    mar_911_new +
                                    mar_new +
                                    occ_trans_e +
                                    income_new +
                                    boroughcode,
                                  data = wheezemodel_train, 
                                 ntree = 500, 
                                 importance=T)

summary(rfwheeze_chimodel)

importance(rfwheeze_chimodel)

varImpPlot(rfwheeze_chimodel)

pred_rf_wheeze_chimodel <- predict(rfwheeze_chimodel, wheezemodel_train)
cm_rf_wheeze_chimodel <- confusionMatrix(pred_rf_wheeze_chimodel, wheezemodel_train$wheeze_30d_f1)
cm_rf_wheeze_chimodel


AUC_rf_wheeze_chimodel <- prediction(as.numeric(pred_rf_wheeze_chimodel), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_rf_wheeze_chimodel_performance_auc <- performance(AUC_rf_wheeze_chimodel, measure = "auc")
AUC_rf_wheeze_chimodel_performance <- performance(AUC_rf_wheeze_chimodel, measure='tpr', x.measure='fpr')
AUC_rf_wheeze_chimodel_value <- unlist(AUC_rf_wheeze_chimodel_performance_auc@y.values)
AUC_rf_wheeze_chimodel_value <- as.data.frame(AUC_rf_wheeze_chimodel_value)



#undersampling
data_under <- ovun.sample(wheeze_30d_f1 ~., 
                          data = wheezemodel_train,
                          method = "under")$data


set.seed(123)
rfwheeze_under <- randomForest(wheeze_30d_f1 ~ Mask_type + 
                                 smkstatus +
                                 dust_cat +
                                 curr_employment +
                                 new_education +
                                 mar_911_new +
                                 mar_new +
                                 occ_trans_e +
                                 income_new +
                                 boroughcode,
                                 data = data_under, 
                              ntree = 500, 
                              importance=T)




pred_rf_wheeze_under <- predict(rfwheeze_under, wheezemodel_train)
cm_rf_wheeze_under <- confusionMatrix(pred_rf_wheeze_under, wheezemodel_train$wheeze_30d_f1)
cm_rf_wheeze_under



AUC_rf_wheeze_under <- prediction(as.numeric(pred_rf_wheeze_under), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_rf_wheeze_under_performance_auc <- performance(AUC_rf_wheeze_under, measure = "auc")
AUC_rf_wheeze_under_performance <- performance(AUC_rf_wheeze_chimodel, measure='tpr', x.measure='fpr')
AUC_rf_wheeze_under_value <- unlist(AUC_rf_wheeze_under_performance_auc@y.values)
AUC_rf_wheeze_under_value <- as.data.frame(AUC_rf_wheeze_under_value )

importance(rfwheeze_under)

varImpPlot(rfwheeze_under)



set.seed(123)
glm_wheeze_under <- glm(relevel(factor(wheeze_30d_f1), ref=2) ~ relevel(factor(Mask_type), ref = "4") + 
                          smkstatus +
                          dust_cat +
                          curr_employment +
                          new_education +
                          mar_911_new +
                          mar_new +
                          occ_trans_e +
                          income_new +
                          boroughcode,
                         data = data_under,
                           family =binomial(link = logit))



pred_glm_wheeze_under <- predict(glm_wheeze_under, wheezemodel_train)
pred_glm_wheeze_under

summary(glm_wheeze_under)

prediction_glm_wheeze_under <- as.factor(ifelse(pred_glm_wheeze_under > 0.5, 1,2))
cm_glm_wheeze_under <- confusionMatrix(prediction_glm_wheeze_under, wheezemodel_train$wheeze_30d_f1)

cm_glm_wheeze_under

AUC_glm_wheeze_under <- prediction(as.numeric(prediction_glm_wheeze_under), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_glm_wheeze_under_performance_value <- performance(AUC_glm_wheeze_under, measure = "auc")
AUC_glm_wheeze_under_performance <- performance(AUC_glm_wheeze_under, measure='tpr', x.measure='fpr')
AUC_glm_wheeze_under_value <- unlist(AUC_glm_wheeze_under_performance_value@y.values)
AUC_glm_wheeze_under_value <- as.data.frame(AUC_glm_wheeze_under_value)




#over
data_over <- ovun.sample(wheeze_30d_f1 ~., 
                          data = wheezemodel_train,
                          method = "over")$data




set.seed(123)
rfwheeze_over <- randomForest(wheeze_30d_f1 ~ Mask_type + 
                                smkstatus +
                                dust_cat +
                                curr_employment +
                                new_education +
                                mar_911_new +
                                mar_new +
                                occ_trans_e +
                                income_new +
                                boroughcode,
                              data = data_over, 
                               ntree = 500, 
                               importance=T)




pred_rf_wheeze_over <- predict(rfwheeze_over, wheezemodel_train)
cm_rf_wheeze_over <- confusionMatrix(pred_rf_wheeze_over, wheezemodel_train$wheeze_30d_f1)
cm_rf_wheeze_over





AUC_rf_wheeze_over <- prediction(as.numeric(pred_rf_wheeze_over), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_rf_wheeze_over_performance_auc <- performance(AUC_rf_wheeze_over, measure = "auc")
AUC_rf_wheeze_over_performance <- performance(AUC_rf_wheeze_chimodel, measure='tpr', x.measure='fpr')
AUC_rf_wheeze_over_value <- unlist(AUC_rf_wheeze_over_performance_auc@y.values)
AUC_rf_wheeze_over_value <- as.data.frame(AUC_rf_wheeze_over_value )



set.seed(123)
glm_wheeze_over <- glm(relevel(factor(wheeze_30d_f1), ref=2) ~ relevel(factor(Mask_type), ref = "4") + 
                          smkstatus +
                         dust_cat +
                         curr_employment +
                         new_education +
                         mar_911_new +
                         mar_new +
                         occ_trans_e +
                         income_new +
                         boroughcode,
                         data = data_over,
                        family =binomial(link = logit))



pred_glm_wheeze_over <- predict(glm_wheeze_over, wheezemodel_train)
pred_glm_wheeze_over

summary(glm_wheeze_over)

prediction_glm_wheeze_over <- as.factor(ifelse(pred_glm_wheeze_over > 0.5, 1,2))
cm_glm_wheeze_over <- confusionMatrix(prediction_glm_wheeze_over, wheezemodel_train$wheeze_30d_f1)

cm_glm_wheeze_over

AUC_glm_wheeze_over <- prediction(as.numeric(prediction_glm_wheeze_over), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_glm_wheeze_over_performance_value <- performance(AUC_glm_wheeze_over, measure = "auc")
AUC_glm_wheeze_over_performance <- performance(AUC_glm_wheeze_over, measure='tpr', x.measure='fpr')
AUC_glm_wheeze_over_value <- unlist(AUC_glm_wheeze_over_performance_value@y.values)
AUC_glm_wheeze_over_value <- as.data.frame(AUC_glm_wheeze_over_value)






#rose

data_rose <- ROSE(wheeze_30d_f1 ~ .,
                       data = wheezemodel_train,
                       N= 600, p=0.5)$data

summary(data_rose)

set.seed(123)
model_glm_rose <- glm(relevel(factor(wheeze_30d_f1), ref=2) ~ relevel(factor(Mask_type), ref = "4") + 
                        smkstatus +
                        dust_cat +
                        curr_employment +
                        new_education +
                        mar_911_new +
                        mar_new +
                        occ_trans_e +
                        income_new +
                        boroughcode,
                       data = data_rose,
                           family =binomial(link = logit))


pred_glm_wheeze_rose <- predict(model_glm_rose, wheezemodel_train)
pred_glm_wheeze_rose

summary(model_glm_rose)

prediction_glm_wheeze_rose <- as.factor(ifelse(pred_glm_wheeze_rose > 0.5, 1, 2))
cm_glm_wheeze_rose <- confusionMatrix(prediction_glm_wheeze_rose, wheezemodel_train$wheeze_30d_f1)
cm_glm_wheeze_rose




AUC_glm_wheeze_rose <- prediction(as.numeric(prediction_glm_wheeze_rose), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_glm_wheeze_rose_performance_value <- performance(AUC_glm_wheeze_rose, measure = "auc")
AUC_glm_wheeze_rose_performance <- performance(AUC_glm_wheeze_rose, measure='tpr', x.measure='fpr')
AUC_glm_wheeze_rose_value <- unlist(AUC_glm_wheeze_rose_performance_value@y.values)
AUC_glm_wheeze_rose_value <- as.data.frame(AUC_glm_wheeze_rose_value)




set.seed(123)
rfwheeze_rose <- randomForest(wheeze_30d_f1 ~ Mask_type + 
                                smkstatus +
                                dust_cat +
                                curr_employment +
                                new_education +
                                mar_911_new +
                                mar_new +
                                occ_trans_e +
                                income_new +
                                boroughcode,
                              data = data_rose, 
                                  ntree = 500, 
                                  importance=T)




pred_rf_wheeze_rose <- predict(rfwheeze_rose, wheezemodel_train)
cm_rf_wheeze_rose <- confusionMatrix(pred_rf_wheeze_rose, wheezemodel_train$wheeze_30d_f1)
cm_rf_wheeze_rose





AUC_rf_wheeze_rose <- prediction(as.numeric(pred_rf_wheeze_rose), as.numeric(wheezemodel_train$wheeze_30d_f1))
AUC_rf_wheeze_rose_performance_auc <- performance(AUC_rf_wheeze_rose, measure = "auc")
AUC_rf_wheeze_rose_performance <- performance(AUC_rf_wheeze_chimodel, measure='tpr', x.measure='fpr')
AUC_rf_wheeze_rose_value <- unlist(AUC_rf_wheeze_rose_performance_auc@y.values)
AUC_rf_wheeze_rose_value <- as.data.frame(AUC_rf_wheeze_rose_value )


wheeze_model_compare <- data.frame(Model = c('RF UNC', 
                                      'RF Under',
                                      'RF Over',
                                      'RF ROSE',
                                      'GLM UNC',
                                      'GLM Under',
                                      'GLM Over',
                                      'GLM ROSE'),
                            Accuracy = c(cm_rf_wheeze_chimodel$overall[1],
                                         cm_rf_wheeze_under$overall[1],
                                         cm_rf_wheeze_over$overall[1],
                                         cm_rf_wheeze_rose$overall[1],
                                         cm_glm_wheeze_chimodel$overall[1],
                                         cm_glm_wheeze_under$overall[1],
                                         cm_glm_wheeze_over$overall[1],
                                         cm_glm_wheeze_rose$overall[1]),
                            Accuracy_min = c(cm_rf_wheeze_chimodel$overall[3],
                                         cm_rf_wheeze_under$overall[3],
                                         cm_rf_wheeze_over$overall[3],
                                         cm_rf_wheeze_rose$overall[3],
                                         cm_glm_wheeze_chimodel$overall[3],
                                         cm_glm_wheeze_under$overall[3],
                                         cm_glm_wheeze_over$overall[3],
                                         cm_glm_wheeze_rose$overall[3]),
                            Accuracy_max = c(cm_rf_wheeze_chimodel$overall[4],
                                             cm_rf_wheeze_under$overall[4],
                                             cm_rf_wheeze_over$overall[4],
                                             cm_rf_wheeze_rose$overall[4],
                                             cm_glm_wheeze_chimodel$overall[4],
                                             cm_glm_wheeze_under$overall[4],
                                             cm_glm_wheeze_over$overall[4],
                                             cm_glm_wheeze_rose$overall[4]),
                          Sensitivity = c(cm_rf_wheeze_chimodel$byClass[1],
                                          cm_rf_wheeze_under$byClass[1],
                                          cm_rf_wheeze_over$byClass[1],
                                          cm_rf_wheeze_rose$byClass[1],
                                          cm_glm_wheeze_chimodel$byClass[1],
                                          cm_glm_wheeze_under$byClass[1],
                                          cm_glm_wheeze_over$byClass[1],
                                          cm_glm_wheeze_rose$byClass[1]),
                          Specificity = c(cm_rf_wheeze_chimodel$byClass[2],
                                          cm_rf_wheeze_under$byClass[2],
                                          cm_rf_wheeze_over$byClass[2],
                                          cm_rf_wheeze_rose$byClass[2],
                                          cm_glm_wheeze_chimodel$byClass[2],
                                          cm_glm_wheeze_under$byClass[2],
                                          cm_glm_wheeze_over$byClass[2],
                                          cm_glm_wheeze_rose$byClass[2]),
                            AUC = c(AUC_rf_wheeze_chimodel_value$AUC_rf_wheeze_chimodel_value, 
                                    AUC_rf_wheeze_under_value$AUC_rf_wheeze_under_value,
                                    AUC_rf_wheeze_over_value$AUC_rf_wheeze_over_value,
                                    AUC_rf_wheeze_rose_value$AUC_rf_wheeze_rose_value,
                                    AUC_glm_wheeze_chimodel_value$AUC_glm_wheeze_chimodel_value, 
                                    AUC_glm_wheeze_under_value$AUC_glm_wheeze_under_value,
                                    AUC_glm_wheeze_over_value$AUC_glm_wheeze_over_value,
                                    AUC_glm_wheeze_rose_value$AUC_glm_wheeze_rose_value))
                                    
                                    
write.csv(wheeze_model_compare, "wheeze_model_compare.csv")

ggplot(aes(x=Model, y=Sensitivity), data=wheeze_model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Sensitivity of Models') +
  xlab('Models') +
  ylab('Sensitivity')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent)
  






ggplot(aes(x=Model, y=Accuracy), data=wheeze_model_compare) +
  geom_point(stat='identity',  color = "red") +
  labs(y= "Overall Accuracy", x= "Models", title = "Comparative Accuracy of Models")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_errorbar(aes(ymin=Accuracy_min, ymax=Accuracy_max), width=.1, color = "blue")+ 
  scale_y_continuous(labels = scales::percent) +
  theme_bw()
  



plot(rfwheeze_under1$err.rate[,1], type = "l", lwd = 3, col = "blue",
     main = "OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")



#enhancing rf model
set.seed(123)
rfwheeze_under1 <- randomForest(wheeze_30d_f1 ~ Mask_type + smkstatus +
                                  dust_cat +
                                  curr_employment +
                                  new_education +
                                  mar_911_new +
                                  mar_new +
                                  occ_trans_e +
                                  income_new +
                                  boroughcode,
                                data = data_under, 
                               ntree = 5000, 
                               importance=T)




pred_rf_wheeze_under1 <- predict(rfwheeze_under1, wheezemodel_train)
cm_rf_wheeze_under1 <- confusionMatrix(pred_rf_wheeze_under1, wheezemodel_train$wheeze_30d_f1)
cm_rf_wheeze_under1


importance(rfwheeze_under1)

varImpPlot(rfwheeze_under1)

###Testing###
set.seed(123)
pred_rf_wheeze1 <- predict(rfwheeze_under1, wheezemodel_testing)
cm_rf_wheeze1 <- confusionMatrix(pred_rf_wheeze1, wheezemodel_testing$wheeze_30d_f1)
cm_rf_wheeze1



