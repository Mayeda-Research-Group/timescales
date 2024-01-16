#################################
# Author: Eleanor Hayes-Larson
# Purpose: Save table of knot locations
#################################
library(openxlsx)
options(scipen=999)
#Load output
source("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/Timescale project/Harmonized analyses/1. Load results.R")


#Table for attrition over time on study
  for (i in 1:nrow(df)){
  # i<-1
   temp<-get(paste0("output_",df$Cohort[i], "_",df$Group[i]))[[1]]$time_attr
  out<-temp %>% mutate (n = round(n,0),
                        p_apoe=n_APOE/n, 
                                         p_diab=n_DM/n,
                                         Cohort=df$Cohort[i],
                                         Group=df$Group[i]
                                           ) %>%
              select(-n_APOE, -n_DM) %>% pivot_longer(., cols=c(n, p_apoe, p_diab)) 
  
  out[is.na(out)]<-0
  
  out<- out %>% 
              mutate(value=case_when(name=="n" ~ format(round(value,0), nsmall=0),
                                     TRUE ~ format(round(value,2),nsmall=2))) %>%
             pivot_wider(names_from=Time) %>%     mutate_all(as.character) 
  
  if (i==1){ time_attr_tab<-out} else {time_attr_tab<-rbind(time_attr_tab,out)}
  
  }
#Missings come from cohorts that don't have participants with data for a given interval.

#Table for attrition over age
  for (i in 1:nrow(df)){
    
    #i<-3
    temp<-get(paste0("output_",df$Cohort[i], "_",df$Group[i]))[[1]]$age_attr
    
    if(df$Cohort[i]=="ARIC"){temp <- temp %>% rename(Age=Time)}
    out<-temp %>% mutate (p_apoe=n_APOE/n, 
                          p_diab=n_DM/n,
                          Cohort=df$Cohort[i],
                          Group=df$Group[i]
    ) %>%
      select(-n_APOE, -n_DM) %>% pivot_longer(., cols=c(n, p_apoe, p_diab)) %>% 
      mutate(value=case_when(name=="n" ~ format(round(value,0), nsmall=0),
                             TRUE ~ format(round(value,2),nsmall=2))) %>%
      pivot_wider(names_from=Age)
    
    if (i==1){ age_attr_tab<-out} else {age_attr_tab<-rbind(age_attr_tab,out)}
    
  }
#NaNs come from cohorts that don't have participants with data for a given interval.

#Table for baseline age group by age

  for (i in 1:nrow(df)){
    
    #i<-1
    temp<-get(paste0("output_",df$Cohort[i], "_",df$Group[i]))[[1]]$age_attr2
    #if(df$Cohort[i]=="ARIC"){temp <- temp %>% rename(Age=Time)}
    out<-temp %>% mutate (Cohort=df$Cohort[i],
                          Group=df$Group[i]
    ) %>%
      pivot_wider(names_from=Age, values_from=n)
    
    if (i==1){ age_attr_tab2<-out} else {age_attr_tab2<-bind_rows(age_attr_tab2,out)}
    
  }
  
  labelcols<-age_attr_tab2 %>% select(Cohort, Group, age0_group) 
  othercols <- age_attr_tab2 %>% select(-Cohort, -Group, -age0_group) %>% select(sort(colnames(.)))
  age_attr_tab2<-bind_cols(labelcols, othercols)                
#NaNs come from cohorts that don't have participants with data for a give interval.
  
#Split these by race 
  time_attr_Overall<-time_attr_tab %>% filter(Group=="Overall")
  time_attr_White<-time_attr_tab %>% filter(Group=="White")
  time_attr_Black<-time_attr_tab %>% filter(Group=="Black")
  time_attr_Hispanic<-time_attr_tab %>% filter(Group=="Hispanic")

  age_attr_Overall<-age_attr_tab %>% filter(Group=="Overall")
  age_attr_White<-age_attr_tab %>% filter(Group=="White")
  age_attr_Black<-age_attr_tab %>% filter(Group=="Black")
  age_attr_Hispanic<-age_attr_tab %>% filter(Group=="Hispanic")
  
  age_attr2_Overall<-age_attr_tab2 %>% filter(Group=="Overall")
  age_attr2_White<-age_attr_tab2 %>% filter(Group=="White")
  age_attr2_Black<-age_attr_tab2 %>% filter(Group=="Black")
  age_attr2_Hispanic<-age_attr_tab2 %>% filter(Group=="Hispanic")
  
  
#Save in excel output
attr_tabs<-list(time_attr_Overall=time_attr_Overall, 
                time_attr_White=time_attr_White, 
                time_attr_Black=time_attr_Black, 
                time_attr_Hispanic=time_attr_Hispanic,
                age_attr_Overall=age_attr_Overall, 
                age_attr_White=age_attr_White, 
                age_attr_Black=age_attr_Black, 
                age_attr_Hispanic=age_attr_Hispanic,
                age_attr2_Overall=age_attr2_Overall, 
                age_attr2_White=age_attr2_White, 
                age_attr2_Black=age_attr2_Black,
                age_attr2_Hispanic=age_attr2_Hispanic)

write.xlsx(attr_tabs,
           file=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Final figures/attr_tabs.xlsx")
)
