#################################
# Author: Eleanor Hayes-Larson
# Purpose: Create table of package versions
#################################
library(openxlsx)

#Load output
source("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/Timescale project/Harmonized analyses/1. Load results.R")

df_overall<-df %>% filter(Group=="Overall")
cg_names_overall <-grep("Overall",cg_names,value=TRUE)

#Set up table of versions
for (i in 1:nrow(df_overall)){
  #i<-3
  if (i==1){
    temp<-c(df_overall$Cohort[i], get(cg_names_overall[i])[[1]]$versions) %>% t()
    versions<-temp %>% data.frame()
    colnames(versions) <-c("Cohort", "R", colnames(versions)[3:8])
  }   else{
    
    temp<-c(df_overall$Cohort[i], get(cg_names_overall[i])[[1]]$versions) 
  
    versions<-rbind(versions, temp)
}
}
#Warning here comes from ARIC having two additional packages in the versions list. 
#Does not cause errors in table

#Trim R version label
versions$R<-gsub("^.{0,10}", "", versions$R)


# Export table
write.xlsx(versions, paste0(path_prefix, 
                            "/Timescale project/Harmonized analyses/Output/Final figures",
                            "/versions.xlsx"))
