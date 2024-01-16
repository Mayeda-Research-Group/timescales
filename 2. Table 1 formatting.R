#################################
# Author: Eleanor Hayes-Larson
# Purpose: Format and save Table 1's overall and by race
#################################


#################################
# 1. source output and save table 1s by cohort/group
#################################
#Load output
source("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/Timescale project/Harmonized analyses/1. Load results.R")


#Pull together Table 1 summaries
savetabs<-T
for (i in 1:nrow(df)){
 # i<-1
  assign(paste0("tab1_", df$Cohort[i],"_", df$Group[i]),
         get(substr(files[i], 1, nchar(files[i])-6))[[1]]$tab1)
  
  temp<-get(paste0("tab1_", df$Cohort[i],"_", df$Group[i]))$table_body
  
  #Change "n (%)" to just "%"
  temp$stat_label[temp$stat_label=="n (%)"]<-"%"
  temp$stat_0<-ifelse(grepl("%", temp$stat_0, fixed=T)>0,
                         substr(temp$stat_0, 
                                regexpr("(", temp$stat_0, fixed = TRUE)+1, 
                                regexpr("%", temp$stat_0, fixed = TRUE)-1), 
                      temp$stat_0)
  

  temp2<-get(paste0("tab1_", df$Cohort[i],"_", df$Group[i]))
  temp2$table_body<-temp
  assign(paste0("tab1_", df$Cohort[i],"_", df$Group[i]),
         temp2)
  
  if(savetabs){gtsave(as_gt(get(paste0("tab1_", df$Cohort[i],"_", df$Group[i]))), 
         file = paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Table 1s/",
                       "tab1_", df$Cohort[i],"_", df$Group[i],".html"))}
}

#################################
# 2. manual updates to Table 1 objects to aid in combining acros cohorts.  
#################################

#Memento re-label race as unknown
temp<-tab1_Memento_Overall$table_body
temp$label[temp$variable=="race" & temp$row_type=="level"]<-"Unknown"
tab1_Memento_Overall$table_body<-temp

#3City re-label race as unknown, keep only %
temp2<-tab1_3City_Overall$table_body
temp2$row_type[temp2$variable=="race" & temp2$label=="N/A"]<-"level"
temp2$stat_0[temp2$variable=="race" & temp2$label=="N/A"]<-"100.0"
temp2$stat_0[temp2$variable=="race" & temp2$row_type=="label"]<-NA
temp2$label[temp2$variable=="race" & temp2$row_type=="level"]<-"Unknown"
tab1_3City_Overall$table_body<-temp2

#Mayo collapse non-white, relabel race categories
temp3<-tab1_MCSA_Overall$table_body
temp3$label[temp3$variable=="race" & temp3$row_type=="level" & temp3$label=="5=White"]<-"White"
temp3$label[temp3$variable=="race" & temp3$row_type=="level" & temp3$label=="7=Unkown/NotRep"]<-"Unknown"
temprow<-temp3[temp3$variable=="race" & temp3$row_type=="level" & temp3$label=="White",]
  temprow$label<-"Non-White"
  temprow$stat_0<-sum(as.numeric(temp3$stat_0[temp3$variable=="race" & temp3$row_type=="level" & !temp3$label %in% c("White", "Unknown")]))
temp3<-temp3[!(temp3$variable=="race" & temp3$row_type=="level" & !temp3$label %in% c("White", "Unknown")),]
temp3<-rbind(temp3, temprow)

temp3<- temp3 %>% arrange(variable, var_type, var_label)
tab1_MCSA_Overall$table_body<-temp3

#PAC collapse non-white, relabel race categories
temp4<-tab1_PAC_Overall$table_body
temprow<-temp4[temp4$variable=="race" & temp4$row_type=="level" & temp4$label=="White",]
  temprow$label<-"Non-White"
  temprow$stat_0<-sum(as.numeric(temp4$stat_0[temp4$variable=="race" & temp4$row_type=="level" & !temp4$label %in% c("White")]))
temp4<-temp4[!(temp4$variable=="race" & temp4$row_type=="level" & !temp4$label %in% c("White")),]
temp4<-rbind(temp4, temprow)
temp4<- temp4 %>% arrange(variable, var_type, var_label)
tab1_PAC_Overall$table_body<-temp4


#ARIC and FHS, relabel education categories
temp5<-tab1_ARIC_Overall$table_body
temp5$label[temp5$variable=="edu" & temp5$row_type=="level" & temp5$label=="College graduate +"]<-"College degree"
temp5$label[temp5$variable=="edu" & temp5$row_type=="level" & temp5$label=="High school graduate/vocational school"]<-"High school degree/vocational school"
tab1_ARIC_Overall$table_body<-temp5

temp5<-tab1_ARIC_White$table_body
temp5$label[temp5$variable=="edu" & temp5$row_type=="level" & temp5$label=="College graduate +"]<-"College degree"
temp5$label[temp5$variable=="edu" & temp5$row_type=="level" & temp5$label=="High school graduate/vocational school"]<-"High school degree/vocational school"
tab1_ARIC_White$table_body<-temp5

temp5<-tab1_ARIC_Black$table_body
temp5$label[temp5$variable=="edu" & temp5$row_type=="level" & temp5$label=="College graduate +"]<-"College degree"
temp5$label[temp5$variable=="edu" & temp5$row_type=="level" & temp5$label=="High school graduate/vocational school"]<-"High school degree/vocational school"
tab1_ARIC_Black$table_body<-temp5

temp6<-tab1_FHS_Overall$table_body
temp6$label[temp6$variable=="edu" & temp6$row_type=="level" & temp6$label=="College graduate"]<-"College degree"
temp6$label[temp6$variable=="edu" & temp6$row_type=="level" & temp6$label=="High school gradudate or did not graduate"]<-"<= High school degree"
tab1_FHS_Overall$table_body<-temp6


temp7<-tab1_ACT_Overall$table_body
temp7$label[temp7$variable=="race" & temp7$row_type=="level" & temp7$label=="Non-white"]<-"Non-White"
tab1_ACT_Overall$table_body<-temp7




#################################
# 3. Combining acros cohorts, including function to format Table 1s nicely.  
#################################


# #View Table 1
Overall_tab1<-tbl_merge(list(tab1_3City_Overall,
                              tab1_ACT_Overall,
                             tab1_ARIC_Overall,
                              tab1_CHAP_Overall,
                              tab1_FHS_Overall,
                              tab1_HRS_Overall,
                              tab1_MCSA_Overall,
                              tab1_Memento_Overall,
                              tab1_PAC_Overall,
                              tab1_WHICAP_Overall), 
                              tab_spanner=FALSE)
Overall_tab1



formattable1<-function(inputabs){
  
  #testing
#inputabs<-"Hispanic_tab1"
          temp5<-get(inputabs)$table_body
        
          if (inputabs=="Overall_tab1") {
            temp5[which(temp5$variable=="race" & temp5$row_type=="level"),] <-temp5[c(9,11,12,10,8),]}
          
          
        temp5$label[temp5$variable=="age0" & temp5$label=="Mean (SD)"]<-
          paste0(temp5$var_label[temp5$variable=="age0" & temp5$label=="Mean (SD)"], 
                 ", mean (SD)")
        
        temp5$label[temp5$variable=="fu_time" & temp5$label=="Mean (SD)"]<-
          paste0(temp5$var_label[temp5$variable=="fu_time" & temp5$label=="Mean (SD)"], 
                 ", mean (SD)")
        
        temp5$label[temp5$variable=="fu_time" & temp5$label=="Mean (SD)"]<-
          paste0(temp5$var_label[temp5$variable=="fu_time" & temp5$label=="Mean (SD)"], 
                 ", mean (SD)")
        
        temp5$label[temp5$variable=="num_obs" & temp5$label=="Mean (SD)"]<-
          paste0(temp5$var_label[temp5$variable=="num_obs" & temp5$label=="Mean (SD)"], 
                 ", mean (SD)")
        
        temp5$label[temp5$variable=="mem" & temp5$label=="Mean (SD)"]<-"Baseline memory score, mean (SD)"
        
        temp5$label[temp5$variable=="edu" & temp5$label=="Mean (SD)" ]<-"Educational attainment (years), mean (SD)"
        
        temp5$stat_label_1[temp5$variable %in% c("female", "apoe4", "diabetes_bl")
                           & temp5$row_type=="level"]<-"%"
        
        temp5<-temp5[!(temp5$variable %in% c("age0","fu_time", "num_obs","female", "apoe4", "diabetes_bl", "mem") & temp5$row_type=="label"),]
        temp5<-temp5[!(temp5$variable %in% c("edu") & temp5$label=="Years of education"),]
        
        
        temp2<-get(inputabs)
        
        temp2$table_body<-temp5
        
        #Formatting updates
        outtab<-temp2 %>% remove_row_type(.,
                                                             variables = c("female"),
                                                             type = c("level"),
                                                             level_value = c("Male", "0")
        ) %>% remove_row_type(.,
                              variables = c("age0", "edu", "mem", "num_obs", "fu_time"),
                              type = c("level"),
                              level_value = c("Minimum, 10%, 25%, Median, 75%, 90%, Maximum")
        ) %>% remove_row_type(.,
                              variables = c("apoe4"),
                              type = c("level"),
                              level_value = c("No APOE e-4 allele")
        )%>% remove_row_type(.,
                             variables = c("diabetes_bl"),
                             type = c("level"),
                             level_value = c("No diabetes at baseline", "0")
        ) %>% modify_table_body( ~.x %>% dplyr::arrange(factor(variable, 
                                                               levels=c("num_obs", "fu_time", 
                                                                        "age0", "female", "apoe4", 
                                                                        "diabetes_bl","edu", "race", 
                                                                        "mem", "studygrp")),desc(var_label))) 
        if (inputabs!="Hispanic_tab1") {outtab <- outtab %>% remove_row_type(.,
                            variables = c("studygrp"),
                            type = "all")
        }
         

return(outtab)}

Overall_tab1s_fmt<-formattable1("Overall_tab1")
Overall_tab1s_fmt



#Table 1 by race/ethnicity

# #View Table 1
Black_tab1<-tbl_merge(list(   tab1_ARIC_Black,
                              tab1_CHAP_Black,
                              tab1_HRS_Black, 
                              tab1_WHICAP_Black), 
                         tab_spanner=FALSE)
Black_tab1


Black_tab1s_fmt<-formattable1("Black_tab1") %>% remove_row_type(.,
                                                               variables = c("race"),
                                                               type = c("all"))
Black_tab1s_fmt


White_tab1<-tbl_merge(list(  tab1_ARIC_White,
                             tab1_CHAP_White,
                             tab1_HRS_White,
                             tab1_WHICAP_White), 
                      tab_spanner=FALSE)
White_tab1

White_tab1s_fmt<-formattable1("White_tab1") %>% remove_row_type(.,
                                                                variables = c("race"),
                                                                type = c("all"))
White_tab1s_fmt


Hispanic_tab1<-tbl_merge(list(tab1_WHICAP_Hispanic), 
                      tab_spanner=FALSE)
Hispanic_tab1

Hispanic_tab1s_fmt<-formattable1("Hispanic_tab1") %>% remove_row_type(.,
                                                                variables = 'race',
                                                                type = c("all"))
Hispanic_tab1s_fmt


#Exporting tables Note that these save as html, so I need to open in Excel and save as .xlsx. 
#There doesn't seem to be a better way to do this.
gtsave(Overall_tab1s_fmt %>% as_gt, file=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Table1_Overall.html"))
gtsave(Black_tab1s_fmt %>% as_gt, file=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Table1_Black.html"))
gtsave(White_tab1s_fmt %>% as_gt, file=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Table1_White.html"))
gtsave(Hispanic_tab1s_fmt %>% as_gt, file=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Table1_Hispanic.html"))

