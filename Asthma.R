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


Asthma = maskdata %>%
  select(-worker_e, -type_911, -type_912, -cough_30d_f1, 
         -cough_freq_f1, -cough_cons_f1, -asthma_f1, -sinus_30d_f1,
         -wheeze_30d_f1, -brthless_30d_f1, -X)



Asthma <- Asthma %>%
  mutate_if(is.integer, as.factor)

summary(Asthma)

Asthma <- Asthma %>%
  filter(!is.na(asthma_12m_f1))


Asthma$asthma_12m_f1 <- as.character(Asthma$asthma_12m_f1)

Asthma = Asthma %>%
  filter(asthma_12m_f1 != "8")

Asthma$asthma_12m_f1 <- as.factor(Asthma$asthma_12m_f1)


Asthma = Asthma %>%
  dplyr::mutate(m3_fittest_f1 = replace_na(m3_fittest_f1, 2))
Asthma$m3_fittest_f1[Asthma$m3_fittest_f1==3] <- 2
Asthma$m3_fittest_f1 <- as.character(Asthma$m3_fittest_f1)

Asthma = Asthma %>%
  filter(m3_fittest_f1 != "8")

Asthma = na.omit(Asthma)


Asthma$m3_fittest_f1 <- as.factor(Asthma$m3_fittest_f1)

Asthma$age_911grp <- as.character(Asthma$age_911grp)

Asthma = Asthma %>%
  filter(age_911grp != "<18")

Asthma$age_911grp <- as.factor(Asthma$age_911grp)

Asthma$Mask_type <- as.character(Asthma$Mask_type)

Asthma = Asthma %>%
  filter(Mask_type != "3")

Asthma$Mask_type <- as.factor(Asthma$Mask_type)


Asthma = Asthma %>%
  filter(!is.na(asthma_12m_f1))



summary(Asthma)






maskasthma <- ggplot(Asthma, 
                        aes(x= Mask_type, fill = asthma_12m_f1)) +
  geom_bar(position = "fill") +
  geom_text(aes(label=..count..), stat = "count", position=position_fill(vjust=0.5))+
  labs(y= "Percentage", x= "Mask Type Group", title = "Asthma Attack based on Mask Type Group")

maskasthma


maskasthmaplot <- ggplot(Asthma, 
                         aes(x= Mask_type, fill = asthma_12m_f1)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=..count..), stat = "count", position=position_fill(vjust=0.5))+
  labs(y= "Percentage", x= "Mask Type", title = "Asthma based on Mask Type Group")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "Asthma",
                      breaks =c("2", "1"),
                      labels =c("No", "Yes"))

maskasthmaplot


set.seed(123)

n = nrow(Asthma)
trainindex <- sample(1:n, size = round(0.7*n))
Asthmamodel_train <- Asthma[trainindex, ]
Asthmamodel_testing <- Asthma[-trainindex, ]

table(Asthmamodel_train$asthma_12m_f1)
table(Asthmamodel_testing$asthma_12m_f1)


summary(Asthma_train)
set.seed(123)
Asthmajustmask <- glm(relevel(factor(asthma_12m_f1), ref=2) ~ relevel(factor(Mask_type), ref = "4"),
                          data = Asthmamodel_train, family =binomial(link = logit))

summary(Asthmajustmask)

odd_asthmajustmask <- exp(coef(Asthmajustmask))

predictsAsthmajustmask <- predict(Asthmajustmask, Asthmamodel_train, type = "response")
predictsAsthmajustmask


asthma_outcome_prediction_justmask <- as.factor(ifelse(predictsAsthmajustmask > 0.5, 1,2))


confusionMatrix(asthma_outcome_prediction_justmask, Asthmamodel_train$asthma_12m_f1)




##Feature Selection 
#using Chi-square test to select variables that are statistically significant - p<0.05 for the sinus outcome


rowsdata <- c("Mask_type", "training", "m3_fittest_f1", 
              "dust_cat", "smkstatus", "occ_trans_e", "gender", 
              "age_911grp", "new_education", "census_new", "income_new",
              "boroughcode", "mar_911_new",  "mar_new", 
              "curr_employment", "new_employed_911")

outcome <- c("asthma_12m_f1")


Asthmachifeatureselection <- CreateTableOne(
  factorVars = rowsdata,
  data = Asthma, 
  strata = outcome,
  includeNA = T, 
  smd = T, 
  addOverall = FALSE,
  test = T,
  testApprox = chisq.test,
  testExact = fisher.test)

Asthmachifeatureselection <- print(Asthmachifeatureselection, 
                             catDigits = 1,
                             showAllLevels = T,
                             test = T,
                             missing = T, 
                             quote = F,
                             explain = F)


write.csv(Asthmachifeatureselection, "Asthmachifeatureselection.csv")




asthma_attribute_infogain <-information_gain(formula = asthma_12m_f1 ~., data = Asthma)
attribute_infogain %>%
  arrange(desc(importance))

write.csv(asthma_attribute_infogain, "asthma_attribute_infogain.csv")



glm_Asthma_model <- glm(relevel(factor(asthma_12m_f1), ref=2)  ~ relevel(factor(Mask_type), ref = "4") + 
                      training + m3_fittest_f1 + dust_cat + smkstatus + occ_trans_e + 
                      gender+ age_911grp + new_education +
                      census_new,
                    data = Asthmamodel_train, family =binomial(link = logit))




pred_glm_Asthma_chimodel <- predict(glm_Asthma_model, Asthmamodel_train)
pred_glm_Asthma_chimodel

summary(glm_Asthma_model)

prediction_glm_Asthma_chimodel <- as.factor(ifelse(pred_glm_Asthma_chimodel > 0.5, 1,2))
cm_glm_Asthma_chimodel <- confusionMatrix(prediction_glm_Asthma_chimodel, Asthmamodel_train$asthma_12m_f1)

cm_glm_Asthma_chimodel

AUC_glm_Asthma_chimodel <- prediction(as.numeric(prediction_glm_Asthma_chimodel), as.numeric(Asthmamodel_train$asthma_12m_f1))
AUC_glm_Asthma_chimodel_performance_value <- performance(AUC_glm_Asthma_chimodel, measure = "auc")
AUC_glm_Asthma_chimodel_performance <- performance(AUC_glm_Asthma_chimodel, measure='tpr', x.measure='fpr')
AUC_glm_Asthma_chimodel_value <- unlist(AUC_glm_Asthma_chimodel_performance_value@y.values)
AUC_glm_Asthma_chimodel_value <- as.data.frame(AUC_glm_Asthma_chimodel_value)



#Random Forest to compare outcome with these model1 and model2
#number of trees picked on number of attributes x 10

set.seed(123)
rfsasthma <- randomForest(asthma_12m_f1 ~ Mask_type + 
                                          training + m3_fittest_f1 + 
                            dust_cat + smkstatus + occ_trans_e + 
                            gender+ age_911grp + new_education +
                            census_new, 
                            data = Asthmamodel_train, ntree = 500, importance=T)


summary(rfsasthma)

pred_rf_Asthma_chimodel <- predict(rfsasthma, Asthmamodel_train)
cm_rf_Asthma_chimodel <- confusionMatrix(pred_rf_Asthma_chimodel, Asthmamodel_train$asthma_12m_f1)
cm_rf_Asthma_chimodel


AUC_rf_Asthma_chimodel <- prediction(as.numeric(pred_rf_Asthma_chimodel), as.numeric(Asthmamodel_train$asthma_12m_f1))
AUC_rf_Asthma_chimodel_performance_value <- performance(AUC_rf_Asthma_chimodel, measure = "auc")
AUC_rf_Asthma_chimodel_performance <- performance(AUC_rf_Asthma_chimodel, measure='tpr', x.measure='fpr')
AUC_rf_Asthma_chimodel_value <- unlist(AUC_rf_Asthma_chimodel_performance_value@y.values)
AUC_rf_Asthma_chimodel_value <- as.data.frame(AUC_rf_Asthma_chimodel_value)



importance(rfsasthma)

varImpPlot(rfsasthma)




#naive bayes

set.seed(123)
#naive_Asthma <- naiveBayes(asthma_12m_f1 ~ Mask_type + 
                             #training + m3_fittest_f1 + 
                             #dust_cat + smkstatus + occ_trans_e + 
                             #gender+ age_911grp + new_education +
                             #census_new, 
                                   #data = Asthmamodel_train)



e = Asthmamodel_train %>% 
  select( Mask_type , 
            training , m3_fittest_f1 ,
            dust_cat , smkstatus , occ_trans_e , 
            gender, age_911grp , new_education ,
            census_new)

f= Asthmamodel_train$asthma_12m_f1

set.seed(123)
naive_Asthma <- train(e,f, 'nb', trControl = trainControl(method = 'cv', number=10))

naive_Asthma



pred_naive_Asthma_chimodel <- predict(naive_Asthma, Asthmamodel_train)
cm_naive_Asthma_chimodel <- confusionMatrix(pred_naive_Asthma_chimodel, Asthmamodel_train$asthma_12m_f1)
cm_naive_Asthma_chimodel

asthmanbvarimp <- varImp(naive_Asthma)
plot(asthmanbvarimp)


#AUC of Naive Chi

AUC_naive_Asthma_chimodel <- prediction(as.numeric(pred_naive_Asthma_chimodel), as.numeric(Asthmamodel_train$asthma_12m_f1))
AUC_naive_Asthma_chimodel_performance_value <- performance(AUC_naive_Asthma_chimodel, measure = "auc")
AUC_naive_Asthma_chimodel_performance <- performance(AUC_naive_Asthma_chimodel, measure='tpr', x.measure='fpr')
AUC_naive_Asthma_chimodel_value <- unlist(AUC_naive_Asthma_chimodel_performance_value@y.values)
AUC_naive_Asthma_chimodel_value <- as.data.frame(AUC_naive_Asthma_chimodel_value)



summary(naive_Asthma)





Asthma_model_compare <- data.frame(Model = c('RF', 
                                            'GLM',
                                            'NB'),
                                  Accuracy = c(cm_rf_Asthma_chimodel$overall[1],
                                               cm_glm_Asthma_chimodel$overall[1],
                                               cm_naive_Asthma_chimodel$overall[1]),
                                  Accuracy_min = c(cm_rf_Asthma_chimodel$overall[3],
                                                   cm_glm_Asthma_chimodel$overall[3],
                                                   cm_naive_Asthma_chimodel$overall[4]),
                                  Accuracy_max = c(cm_rf_Asthma_chimodel$overall[4],
                                                   cm_glm_Asthma_chimodel$overall[4],
                                                   cm_naive_Asthma_chimodel$overall[3]),
                                  Sensitivity = c(cm_rf_Asthma_chimodel$byClass[1],
                                                  cm_glm_Asthma_chimodel$byClass[1],
                                                  cm_naive_Asthma_chimodel$byClass[1]),
                                  AUC = c(AUC_rf_Asthma_chimodel_value$AUC_rf_Asthma_chimodel_value, 
                                          AUC_glm_Asthma_chimodel_value$AUC_glm_Asthma_chimodel_value , 
                                          AUC_naive_Asthma_chimodel_value$AUC_naive_Asthma_chimodel_value))
                                          

ggplot(aes(x=Model, y=Sensitivity), data=Asthma_model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Sensitivity of Models') +
  xlab('Models') +
  ylab('Sensitivity')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent)                                                      

ggplot(aes(x=Model, y=Accuracy), data=Asthma_model_compare) +
  geom_point(stat='identity',  color = "red") +
  labs(y= "Overall Accuracy", x= "Models", title = "Comparative Accuracy of Models")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_errorbar(aes(ymin=Accuracy_min, ymax=Accuracy_max), width=.1, color = "blue")+ 
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


roc_rf_ch <- data.frame(fpr = unlist(AUC_rf_Asthma_chimodel_performance@x.values), tpr=unlist(AUC_rf_Asthma_chimodel_performance@y.values))
roc_rf_ch$method <- "RF"
roc_glm_ch <- data.frame(fpr = unlist(AUC_glm_Asthma_chimodel_performance@x.values), tpr=unlist(AUC_glm_Asthma_chimodel_performance@y.values))
roc_glm_ch$method <- "GLM"
roc_nb_ch <- data.frame(fpr = unlist(AUC_naive_Asthma_chimodel_performance@x.values), tpr=unlist(AUC_naive_Asthma_chimodel_performance@y.values))
roc_nb_ch$method <- "NB"



Asthma_AUC <- rbind(roc_rf_ch, roc_glm_ch, roc_nb_ch) %>%
  ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
  geom_line() +
  geom_abline(linetype=2) +
  labs(y= "True Postive Rate", x= "False Positive Rate", title = "Receiver operating characteristic (ROC) curve of Models")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(lim=c(0,1)) +
  scale_y_continuous(lim=c(0,1)) +
  theme(legend.position=c(0.8,0.2), legend.title=element_blank())


#test


set.seed(123)


pred_rf_asthma <- predict(rfsasthma, Asthmamodel_testing)
cm_rf_asthma <- confusionMatrix(pred_rf_asthma, Asthmamodel_testing$asthma_12m_f1)
cm_rf_asthma


plot(rfsasthma$err.rate[,1], type = "l", lwd = 3, col = "blue",
     main = "OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")





