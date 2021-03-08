library(tidyverse)
install.packages("dplyr")
library(dplyr)
library(reshape2)
library(tableone)


wave2 = read_csv("wave2-dataset.csv")

mask <- wave2 %>% select(worker_e, type_911, type_912)

mask <- subset(mask, mask$worker_e == "Yes")

use = mask %>%
  count(type_911,type_912) %>%
  group_by(type_911)%>%
  filter(type_911==type_912)

organizebymasktype = wave2 %>% 
  select(worker_e, type_911, type_912, training, m3_fittest_f1, dust_cat, smkstatus, occ_trans_e, 
         cough_30d_f1, cough_freq_f1, cough_cons_f1, asthma_f1, asthma_12m_f1, sinus_30d_f1, wheeze_30d_f1, brthless_30d_f1,
         gender, age_911grp, new_education, census_new, income_new, boroughcode, mar_911_new, mar_new, curr_employment)%>%
  filter(worker_e == "Yes")%>%
  mutate(Mask_type = case_when(type_911 == "1" & type_912 == "1" ~ 1, 
                           type_911 == "2" & type_912 =="2" ~ 2,
                           type_911 == "3" & type_912 =="3" ~ 3,
                           type_911 == "4" & type_912 =="4" ~ 4))%>%
  filter(!is.na(Mask_type))%>%
  dplyr::relocate(Mask_type)
  


organizebymasktype %>%
  group_by(Mask_type)%>%
  count(dust_cat)%>%
  dcast(., dust_cat ~ Mask_type)

organizebymasktype %>%
  group_by(Mask_type)%>%
  count(smkstatus)%>%
  dcast(., smkstatus ~ Mask_type)

organizebymasktype %>%
  group_by(Mask_type)%>%
  count(occ_trans_e)%>%
  dcast(., occ_trans_e ~ Mask_type)

organizebymasktype %>%
  group_by(Mask_type)%>%
  count(cough_30d_f1)%>%
  dcast(., cough_30d_f1 ~ Mask_type)


organizebymasktype %>%
  group_by(Mask_type)%>%
  count(cough_freq_f1)%>%
  dcast(., cough_freq_f1 ~ Mask_type)


organizebymasktype %>%
  group_by(Mask_type)%>%
  count(cough_cons_f1)%>%
  dcast(., cough_cons_f1 ~ Mask_type)


organizebymasktype %>%
  group_by(Mask_type)%>%
  count(asthma_f1)%>%
  dcast(., asthma_f1 ~ Mask_type)

organizebymasktype %>%
  group_by(Mask_type)%>%
  count(asthma_12m_f1)%>%
  dcast(., asthma_12m_f1 ~ Mask_type)

organizebymasktype %>%
  group_by(Mask_type)%>%
  count(sinus_30d_f1)%>%
  dcast(., sinus_30d_f1 ~ Mask_type)

organizebymasktype %>%
  group_by(Mask_type)%>%
  count(wheeze_30d_f1)%>%
  dcast(., wheeze_30d_f1 ~ Mask_type)

organizebymasktype %>%
  group_by(Mask_type)%>%
  count(brthless_30d_f1)%>%
  dcast(., brthless_30d_f1 ~ Mask_type)


