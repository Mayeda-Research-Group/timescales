#################################
# Author: Eleanor Hayes-Larson
# Purpose: Plot predicted differences in change 
#################################
library(openxlsx)

#Load output and pull in script that includes co-author choice of final models.
source("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/Timescale project/Harmonized analyses/1.1 Final models.R")

########################################
#Part 1: Generate predicted differences
#######################################
mround<- function(x,base){
  base*round(x/base)
}

mround_down <- function(x,base){
  x-(x %% base)
}


for (i in 1:nrow(pred_input)){
  # #testing
   #i<-16
  #i<-2
  timevar<-ifelse(pred_input$ModelNum[i] %in% c(1:5),"years","age_y_c65")

  maxtime<-ifelse(pred_input$Time_p95[i] %% 5 >=4,mround(pred_input$Time_p95[i],5),mround_down(pred_input$Time_p95[i],5))
  minage<-ifelse(pred_input$Age_p5[i] %% 5 <=1,mround_down(pred_input$Age_p5[i],5),mround(pred_input$Age_p5[i],5))
  maxage<-ifelse(pred_input$Age_p95[i] %% 5 >=4,mround(pred_input$Age_p95[i],5),mround_down(pred_input$Age_p95[i],5))
  
  if(pred_input$Cohort[i]=="MCSA"){maxtime<-15} #Added per Terry request
  
  
  #Need to pull cohort and group to get knot locations
  temp<-eval(parse(text=paste0("alloutput[['",pred_input$CG[i],"']][[1]]$knots_",enexpr(timevar))))
  
  DeltaT<-5
  
  if (timevar=="years") {
    times <- seq(5,maxtime,by=5)
  } else if (timevar=="age_y_c65") {
    times <- seq(minage,maxage,by=5)
  }
  
  model<-eval(parse(text=paste0(pred_input$CG[i],
                                pred_input$bin[i],"$",
                                pred_input$modelname[i])))
  
  if (pred_input$ModelLet[i] != "a"){
    
    #pull location of spline terms in fitted model 
    if (pred_input$Exposure[i]=="apoe"){
      posS<-model$best %>% names() %>% grep("apoe4:ns",.) %>% as.numeric()
      if (length(posS)==0) {posS<-model$best %>% names() %>% grep("apoe4At least one APOE e-4 allele:ns",.) %>% as.numeric()
      }
    } else if (pred_input$Exposure[i]=="diab") {
      posS<-model$best %>% names() %>% grep("diabetes_bl:ns",.) %>% as.numeric()
      if (length(posS)==0) {posS<-model$best %>% names() %>% grep("diabetes_blDiabetes at baseline:ns",.) %>% as.numeric()
      }
    }
    
    
    #Pull if ModelLet == b or ===c to determine if ns1 or ns2 for time 
    if (pred_input$ModelLet[i]=="b") {
      nstime<-ns(times, Boundary.knots =temp[c(1,5)], knots=temp[3]) 
      nstimeT0<-ns((times-DeltaT), Boundary.knots =temp[c(1,5)], knots=temp[3]) 
    } else if (pred_input$ModelLet[i]=="c") {
      nstime<-ns(times, Boundary.knots =temp[c(1,5)], knots=temp[c(2,4)]) 
      nstimeT0<-ns((times-DeltaT), Boundary.knots =temp[c(1,5)], knots=temp[c(2,4)]) 
    }
    
    
    s <- t(sapply(1:length(times), function(k){WaldMult(model,pos=c(posS, posS), contrasts = c( nstime[k,],- (nstimeT0[k,])),value=0)}))
    
    
    tab <- cbind(times,s,s[,1]-qnorm(0.975)*s[,2],s[,1]+qnorm(0.975)*s[,2])
    
  } else if (pred_input$ModelLet[i] == "a"){
    
    #Pull location of exposure*time variable: 
    if (pred_input$Exposure[i]=="apoe"){
      posB<-model$best %>% names() %>% grep(paste0("apoe4:",enexpr(timevar)),.) %>% as.numeric()
      if (length(posB)==0) {posB<-model$best %>% names() %>% grep(paste0("apoe4At least one APOE e-4 allele:",enexpr(timevar)),.) %>% as.numeric()
      }
    } else if (pred_input$Exposure[i]=="diab") {
      posB<-model$best %>% names() %>% grep(paste0("diabetes_bl:",enexpr(timevar)),.) %>% as.numeric()
      if (length(posB)==0) {posB<-model$best %>% names() %>% grep(paste0("diabetes_blDiabetes at baseline:",enexpr(timevar)),.) %>% as.numeric()
      }
    }
    
    time<-times
    time0<-times-DeltaT
    s <- t(sapply(1:length(times), function(k){WaldMult(model,pos=c(posB, posB), contrasts = c( time[k],- time0[k]),value=0)}))
    
    tab <- cbind(times,s,s[,1]-qnorm(0.975)*s[,2],s[,1]+qnorm(0.975)*s[,2])
  }
  
  
  colnames(tab) <- c("time","diff","se","wald","p","binf","bsup")
  tab <- data.frame(tab) %>% mutate(Cohort=pred_input$Cohort[i], 
                                    Group=pred_input$Group[i],
                                    Expvar=as.character(pred_input$Exposure[i]),
                                    ModelNum=pred_input$ModelNum[i],
                                    ModelLet=pred_input$ModelLet[i],
                                    Timevar=timevar)
  
  assign(paste0("preddiffY_",pred_input$Cohort[i],"_",
                pred_input$Group[i],"_",
                pred_input$Exposure[i],"_",
                pred_input$ModelNum[i],"_",
                pred_input$ModelLet[i]),
         tab)
  
}



allpreddiff<-mget(grep("preddiffY", ls(), value=TRUE))
allpreddiffdf<-bind_rows(allpreddiff, .id = 'column label')
#Clean up workspace
rm(list = ls(, pattern = "preddiffY_"))

#Filter this only to relevant time periods for each cohort for final models
allpreddiff_restricted<-inner_join(finalmods,allpreddiffdf, 
                                   by=c("Cohort"="Cohort", "Group"="Group", 
                                        "ModelNum"="ModelNum", "ModelLet"="ModelLet",
                                        "Exposure"="Expvar")) %>% select(-Time_p95) %>%  left_join(.,df, by=c("Cohort", "Group")) %>% 
  # filter((Timevar=="years" & time<=Time_p95) |
  #          (Timevar=="age_y_c65" & 
  #             (Age_p5<=time & time<=Age_p95)), ModelNum %in% c(4,6)) %>%
  select("Cohort", "Group", "time", "diff", "binf", "bsup", "ModelNum", "ModelLet", "Exposure", "Timevar")



########################################
#Part 2: Create plots
#######################################
#Create pred diff plots for age scale

diffpltfxn<-function(grp=NULL){
#Unit testing
  #grp<-"Overall"
  
  if(grp=="Overall"){in_wide<-12} else if(grp %in% c("Black", "White")){
    in_wide<-5} else {in_wide<-3}
  
preds_tos<-allpreddiff_restricted %>% filter(Group==grp, 
                                                    Timevar=="years") %>% 
  mutate(time=factor(time, 
                     levels=c(5,10,15,20), 
                     labels=c("0-5", "5-10", "10-15", "15-20")))


apoe_ddiff_tos<- ggplot(data=preds_tos %>% filter(Exposure=="apoe"), color=Cohort)+
  geom_pointrange(aes(x=time, y=diff, ymin=binf, ymax=bsup, color=Cohort), size=.7, shape=15) +facet_grid(rows=NULL, cols=vars(Cohort))+
  xlab("Period of decline (years since baseline)") +ylab("Difference in cognitive change due to APOE ε4 allele")+ylim(-0.55, 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color="gray")+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   legend.position = "none")
apoe_ddiff_tos


diab_ddiff_tos<-ggplot(data=preds_tos %>% filter(Exposure=="diab"), color=Cohort)+
  geom_pointrange(aes(x=time, y=diff, ymin=binf, ymax=bsup, color=Cohort), size=.7, shape=15) +facet_grid(rows=NULL, cols=vars(Cohort))+
  xlab("Period of decline (years since baseline)") +ylab("Difference in cognitive change due to diabetes")+ylim(-0.55, 0.75)+
  geom_hline(yintercept=0, linetype="dashed", color="gray")+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   legend.position = "none")

diab_ddiff_tos

#Create pred diff plots for age scale
preds_age<-allpreddiff_restricted %>% filter(Group==grp, 
                                                    Timevar=="age_y_c65") %>% mutate(time=time+65) %>%
  mutate(time=factor(time, 
                     levels=c(55,60,65, 70, 75, 80, 85, 90), 
                     labels=c("50-55", "55-60", "60-65", "65-70",
                              "70-75", "75-80", "80-85", "85-90")))

apoe_ddiff_age<-ggplot(data=preds_age %>% filter(Exposure=="apoe"), color=Cohort)+
  geom_pointrange(aes(x=time, y=diff, ymin=binf, ymax=bsup, color=Cohort), size=.7, shape=15) +facet_grid(rows=NULL, cols=vars(Cohort))+
  xlab("Period of decline (age in years)") +ylab("Difference in cognitive change due to APOE ε4 allele")+ylim(-0.55, 0.2)+
  geom_hline(yintercept=0, linetype="dashed", color="gray")+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   legend.position = "none")
apoe_ddiff_age


diab_ddiff_age<-ggplot(data=preds_age %>% filter(Exposure=="diab"), color=Cohort)+
  geom_pointrange(aes(x=time, y=diff, ymin=binf, ymax=bsup, color=Cohort), size=.7, shape=15) +facet_grid(rows=NULL, cols=vars(Cohort))+
  xlab("Period of decline (age in years)") +ylab("Difference in cognitive change due to diabetes")+ylim(-0.55, 0.75)+
  geom_hline(yintercept=0, linetype="dashed", color="gray")+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   legend.position = "none")
diab_ddiff_age

#Combine timescales
p_apoe_ddiff<-plot_grid(apoe_ddiff_tos+theme(axis.title.y = element_blank()), 
                        apoe_ddiff_age+theme(axis.title.y = element_blank()), 
                        labels = c('A', 'B'), ncol=1)
p_apoe_ddiff

#Add space for y axis label
p_apoe_ddiff2<-plot_grid(NULL,p_apoe_ddiff, ncol=2, rel_widths=c(0.02,1))
p_apoe_ddiff2

#Final plot!
p_apoe_ddiff3<- ggdraw(p_apoe_ddiff2) + 
  draw_label("Difference in cognitive change due to APOE ε4 allele", size = 12, angle = 90, x=0.01) 
p_apoe_ddiff3

ggsave(p_apoe_ddiff3,
       filename=paste0("apoe_diffs_",grp,".jpg"),
       path=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Final figures/"),
       width=in_wide,
       height=7,
       units="in")



#Combine timescales
p_diab_ddiff<-plot_grid(diab_ddiff_tos+theme(axis.title.y = element_blank()), 
                        diab_ddiff_age+theme(axis.title.y = element_blank()), 
                        labels = c('A', 'B'), ncol=1)
p_diab_ddiff

#Add space for y axis label
p_diab_ddiff2<-plot_grid(NULL,p_diab_ddiff, ncol=2, rel_widths=c(0.02,1))
p_diab_ddiff2

#Final plot!
p_diab_ddiff3<- ggdraw(p_diab_ddiff2) + 
  draw_label("Difference in cognitive change due to diabetes at baseline", size = 12, angle = 90, x=0.01) 
p_diab_ddiff3

ggsave(p_diab_ddiff3,
       filename=paste0("diab_diffs_",grp,".jpg"),
       path=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Final figures/"),
       width=in_wide,
       height=7,
       units="in")

}

diffpltfxn(grp="Overall")
diffpltfxn(grp="Black")
diffpltfxn(grp="White")
diffpltfxn(grp="Hispanic")



#######################################
#Part 3: Create tables
#######################################

not_all_na <- function(x) any(!is.na(x))


preds_tos_tab<-allpreddiff_restricted %>% filter(Timevar=="years") %>% select(-ModelLet) %>%
  mutate(
    # time=factor(time, 
    #                  levels=c(5,10,15,20), 
    #                  labels=c("0-5", "5-10", "10-15", "15-20")),
         est=format(round(diff,2),nsmall=2),
         CI=paste0("(",
                   format(round(binf,2),nsmall=2),",",
                   format(round(bsup,2),nsmall=2),")")) %>% select(-diff, -bsup, -binf) %>%
  pivot_wider(.,names_from=time, values_from = c(est,CI)) %>%
  arrange(.,Cohort, Exposure, Group) %>% select(Cohort, Group, Exposure, Timevar, ModelNum,
                                                                     est_5, CI_5,
                                                                     est_10, CI_10,
                                                                     est_15, CI_15,
                                                                     est_20, CI_20)

#Separate tables by exposure and group
apoe_overall<-preds_tos_tab %>% filter(Group=="Overall", Exposure=="apoe") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar)

diab_overall<-preds_tos_tab %>% filter(Group=="Overall", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) 

apoe_black<-preds_tos_tab %>% filter(Group=="Black", Exposure=="apoe") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) %>% select(where(not_all_na))

diab_black<-preds_tos_tab %>% filter(Group=="Black", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar)  %>% select(where(not_all_na))

apoe_white<-preds_tos_tab %>% filter(Group=="White", Exposure=="apoe") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) %>% select(where(not_all_na))

diab_white<-preds_tos_tab %>% filter(Group=="White", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar)  %>% select(where(not_all_na))

apoe_hispanic<-preds_tos_tab %>% filter(Group=="Hispanic", Exposure=="apoe") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) %>% select(where(not_all_na))

diab_hispanic<-preds_tos_tab %>% filter(Group=="Hispanic", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar)  %>% select(where(not_all_na))

tos_diffs<-list(apoe_overall=apoe_overall, 
                diab_overall=diab_overall, 
                apoe_black=apoe_black, 
                diab_black=diab_black, 
                apoe_white=apoe_white, 
                diab_white=diab_white,
                apoe_hispanic=apoe_hispanic, 
                diab_hispanic=diab_hispanic)

#Missing values due to cohorts not having individuals with data for a given period.

preds_age_tab<-allpreddiff_restricted %>% filter(Timevar=="age_y_c65") %>% select(-ModelLet) %>% 
  mutate(time=time+65) %>%
  mutate(
    #time=factor(time, 
    #                  levels=c(55,60,65, 70, 75, 80, 85, 90), 
    #                  labels=c("50-55", "55-60", "60-65", "65-70",
    #                           "70-75", "75-80", "80-85", "85-90")),
    est=format(round(diff,2),nsmall=2),
    CI=paste0("(",
              format(round(binf,2),nsmall=2),",",
              format(round(bsup,2),nsmall=2),")")) %>% 
  select(-diff, -bsup, -binf) %>%
  pivot_wider(.,names_from=time, values_from = c(est, CI)) %>%
  arrange(.,Cohort, Exposure, Group) %>% select(Cohort, Group, Exposure, Timevar, ModelNum,
                                        est_55, CI_55,
                                        est_60, CI_60,
                                        est_65, CI_65,
                                        est_70, CI_70,
                                        est_75, CI_75,
                                        est_80, CI_80,
                                        est_85, CI_85,
                                        est_90, CI_90)


#Separate tables by exposure and group
apoe_overall<-preds_age_tab %>% filter(Group=="Overall", Exposure=="apoe") %>% 
                  select(-Exposure, -Group, -ModelNum, -Timevar)

diab_overall<-preds_age_tab %>% filter(Group=="Overall", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) 

apoe_black<-preds_age_tab %>% filter(Group=="Black", Exposure=="apoe") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) %>% select(where(not_all_na))

diab_black<-preds_age_tab %>% filter(Group=="Black", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar)  %>% select(where(not_all_na))

apoe_white<-preds_age_tab %>% filter(Group=="White", Exposure=="apoe") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) %>% select(where(not_all_na))

diab_white<-preds_age_tab %>% filter(Group=="White", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar)  %>% select(where(not_all_na))

apoe_hispanic<-preds_age_tab %>% filter(Group=="Hispanic", Exposure=="apoe") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar) %>% select(where(not_all_na))

diab_hispanic<-preds_age_tab %>% filter(Group=="Hispanic", Exposure=="diab") %>% 
  select(-Exposure, -Group, -ModelNum, -Timevar)  %>% select(where(not_all_na))

age_diffs<-list(apoe_overall=apoe_overall, 
                diab_overall=diab_overall, 
                apoe_black=apoe_black, 
                diab_black=diab_black, 
                apoe_white=apoe_white, 
                diab_white=diab_white,
                apoe_hispanic=apoe_hispanic,
                diab_hispanic=diab_hispanic)

#Missing values due to cohorts not having individuals with data for a given period.

#Export tables
write.xlsx(tos_diffs,
           file=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Final figures/tos_diffs.xlsx")
)

write.xlsx(age_diffs,
           file=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Final figures/age_diffs.xlsx")
)
