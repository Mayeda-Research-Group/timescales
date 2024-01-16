#################################
# Author: Eleanor Hayes-Larson
# Purpose: Plot predicted trajectories 
#################################
library(cowplot)
library(openxlsx)

saveplots<-T #switch to save plots or not. 


#Pull in script that includes co-author choice of final models. May need to authorize account the first time.
#This script also sources the "1. Load results.R script" to load all output.
source("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/Timescale project/Harmonized analyses/1.1 Final models.R")

#export final models table
write.xlsx(finalmods_tab, paste0(path_prefix, 
                            "/Timescale project/Harmonized analyses/Output/Final figures",
                            "/finalmodels.xlsx"))

########################################
#Part 1: Generate predicted trajectories
#######################################

Age0_pctile<-50 #Leave this as 50. Was playing around with other Age0 values.
for (i in 1:nrow(pred_input)){
      #testing
      #i<-1
      Cohort<-pred_input$Cohort[i]
      
      model<-eval(parse(text=paste0(pred_input$CG[i],
                                    pred_input$bin[i],"$",
                                    pred_input$modelname[i])))
      
      #Create dataset of predicted trajectories
      datnew <- data.frame(years=seq(0,pred_input$Time_p95[i] ,length=100),
                           age_y_c65=seq(pred_input$Age_p5[i], pred_input$Age_p95[i],length=100), 
                           age0_c65=eval(parse(text=paste0("pred_input$Age0_p",Age0_pctile,"wide[i]")))-65, 
                           female=1, 
                           first_test=0,
                           edu_c=NA)
      
      if(Cohort=="MCSA"){datnew$years<-seq(0,15 ,length=100)} #Added per Terry request
      
      #Education variable type:
      # cont edu: ACT, CHAP, HRS, MCSA, PAC, WHICAP
      # cat ed: 3C, ARIC, FHS, Memento
      
      #For now include fixes for both edu and edu_c
      datnew$edu_c <- ifelse(Cohort %in% c("ACT", "HRS", "CHAP", "MCSA", "PAC", "WHICAP"), 0,
                                  ifelse(Cohort=="3City",1,
                                         ifelse(Cohort=="ARIC",2,
                                                ifelse(Cohort=="FHS", 0,
                                                       ifelse(Cohort=="Memento",1,NA)))))
      
      
      if (Cohort=="3City"){datnew$edu_c<-factor(datnew$edu_c, levels=c(1,0,2))} else
        if (Cohort=="ARIC"){datnew$edu_c<-factor(datnew$edu_c, levels=c(2,1,3))} else
          if (Cohort=="FHS"){datnew$edu_c<-factor(datnew$edu_c, levels=c(0,1,2))} else
            if (Cohort=="Memento"){datnew$edu_c<-factor(datnew$edu_c, levels=c(1,0,2))} 
      
      
      #Set up spline functions 
      ns1 <- function(x) {
        temp<-eval(parse(text=paste0("alloutput[['",pred_input$CG[i],"']][[1]]$knots_",enexpr(x))))
        ns(x, Boundary.knots =temp[c(1,5)], knots=temp[3])
      }
      
      ns2 <- function(x) {
        temp<-eval(parse(text=paste0("alloutput[['",pred_input$CG[i],"']][[1]]$knots_",enexpr(x))))
        ns(x, Boundary.knots =temp[c(1,5)], knots=temp[c(2,4)])
      }
      
      
      #indicate years on study or age as timescale
      if (pred_input$ModelNum[i] %in% c(seq(1,5),1)){
        timevar<-"years"} else if (pred_input$ModelNum[i] %in% c(seq(6,9),1)) {
          timevar<-"age_y_c65"} 
      
      
      # predictions for exposed = 0 
      datnew$apoe4 <- 0
      datnew$diabetes_bl <- 0
      predtime0 <- predictY(model,datnew,var.time=timevar,draws=T)
      
      # predictions for exposed = 1 
      datnew$apoe4 <- 1
      datnew$diabetes_bl <- 1
      predtime1 <- predictY(model,datnew,var.time = timevar,draws=T)
      
      #save combined plot data
      plotdata<-data.frame(times=predtime0$times, predY=predtime0$pred, Exposed=0) %>% 
        rbind(.,data.frame(times=predtime1$times, predY=predtime1$pred, Exposed=1)) %>%
        mutate(Cohort=Cohort, 
               Group=pred_input$Group[i],
               Expvar=as.character(pred_input$Exposure[i]),
               ModelNum=pred_input$ModelNum[i],
               ModelLet=pred_input$ModelLet[i],
               Exposed=as.factor(Exposed),
               AIC=pred_input$AIC[i],
               BIC=pred_input$BIC[i],
               Age0=eval(parse(text=paste0("pred_input$Age0_p",Age0_pctile,"wide[i]")))-65)
      assign(paste0("plotdata_",pred_input$Cohort[i],"_",
                    pred_input$Group[i],"_",
                    pred_input$Exposure[i],"_",
                    pred_input$ModelNum[i],
                    pred_input$ModelLet[i]),
             plotdata)
      
    }

  #Remove object called "Cohort" so ggplot doesn't get confused later.
    rm(Cohort)
  
  #Merge all predicted trajectory data together
    allplotdata<-mget(grep("plotdata", ls(), value=TRUE))
    allplotdata$plotdata<-NULL 
    allplotdata$allplotdata<-NULL #in case of re-running
    allplotdf<-bind_rows(allplotdata, .id = 'column label')
    allplotdf$age_y<-allplotdf$age_y_c65+65 #un-center age
    
    
    allplotdf$ModelLet2<-factor(allplotdf$ModelLet, levels=c("a", "b", "c"),
                               labels=c("Linear time", "Spline time (p50)",
                                        "Spline time (p33, p67)"))
    
   #Changes 9 levels of modelnum to just 5 different specification of Age0 
    # (1-5 for time on study, 6-9 for age scale, with Age0 specification same in 1,6; 2,7, 3,8; 4,9)
    allplotdf$ModelNum2<-factor(allplotdf$ModelNum, levels=c(1:9),
                               labels=c("No Age0", "Linear Age0", "Spline Age0 (p50)",
                                        "Linear Age0 + intxn", "Spline Age0 (p50) + intxn",
                                        "No Age0", "Linear Age0", "Spline Age0 (p50)","Linear Age0 + intxn"))
                               
    allplotdf$Expvar2 <-factor(allplotdf$Expvar, levels=c("apoe", "diab"),
                               labels=c("APOE e4", "Diabetes"))
    
  
    ########################################
    #Part 2: Plots for manuscript!
    ########################################
    
    for (i in 1:nrow(df)){
      
            
    studyplot<-allplotdf %>% filter(Cohort==df$Cohort[i], Group==df$Group[i], !is.na(years))
      ylower<-min(studyplot$predY.lower.Ypred)
      yupper<-max(studyplot$predY.upper.Ypred)
      text<-studyplot %>% group_by(Cohort, Group, Expvar2, ModelNum2, ModelLet2) %>% 
        select (Cohort, Group, Expvar2, ModelNum2, ModelLet2, AIC, BIC) %>% 
        unique() %>% mutate(AIClab = paste0("AIC=",format(round(AIC,1),nsmall=1)),
                            BIClab = paste0("BIC=",format(round(BIC,1), nsmall=1))) 
      assign(paste0(df$Cohort[i],"_",
                    df$Group[i],"_studytime"),
             ggplot(studyplot)+
               geom_ribbon(aes(x=years, ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.2) +
               geom_line(aes(x=years, y=predY.Ypred, group=Exposed, color=Exposed))+
               coord_cartesian(ylim=c(ylower, yupper))+
               ylab("Memory z-score") + xlab("Years since baseline") + labs(fill="Exposure status", color="Exposure status")+
               facet_grid(rows=vars(ModelNum2), cols=vars(Expvar2, ModelLet2), scales=) +
               geom_text(size=3, data=text, mapping = aes(x=-Inf,y=-Inf, label=AIClab, hjust=-0.1, vjust=-1.25))+
               geom_text(size=3, data=text, mapping = aes(x=-Inf,y=-Inf, label=BIClab, hjust=-0.1, vjust=-2.5))+
               # ggtitle(paste0("Time on study trajectories for ", 
               #                studyplot$Cohort,", ", studyplot$Group)) +
               theme_bw()
      )
      
      if (saveplots){
        ggsave(plot=get(paste0(df$Cohort[i],"_",
                               df$Group[i],"_studytime")),
               filename=paste0(df$Cohort[i],"_",
                               df$Group[i],"_studytime_p",Age0_pctile,".jpg"),
               path=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Trajectories/Age0 p",
                           Age0_pctile,"/"),
               width=12,
               height=9,
               units="in"
        )
      }
      
      #For current age models with interactions with baseline age, plot only for baseline age and later
      studyplot2<-allplotdf %>% filter(Cohort==df$Cohort[i], Group==df$Group[i], is.na(years), !(ModelNum>=6 & age_y_c65<Age0)) %>% mutate(age_y=age_y_c65+65)
      ylower<-min(studyplot2$predY.lower.Ypred)
      yupper<-max(studyplot2$predY.upper.Ypred)
      text2<-studyplot2 %>% group_by(Cohort, Group, Expvar2, ModelNum2, ModelLet2) %>% 
        select (Cohort, Group, Expvar2, ModelNum2, ModelLet2, AIC, BIC) %>% 
        unique() %>% mutate(AIClab = paste0("AIC=",format(round(AIC,1), nsmall=1)),
                            BIClab = paste0("BIC=",format(round(BIC,1), nsmall=1))) 
      assign(paste0(df$Cohort[i],"_",
                    df$Group[i],"_agetime"),
             ggplot(studyplot2)+
               geom_ribbon(aes(x=age_y, ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.2) +
               geom_line(aes(x=age_y, y=predY.Ypred, group=Exposed, color=Exposed))+
               coord_cartesian(ylim=c(ylower, yupper))+
               ylab("Memory z-score") + xlab("Age (years)") + labs(fill="Exposure status", color="Exposure status")+
               facet_grid(rows=vars(ModelNum2), cols=vars(Expvar2, ModelLet2), scales=) +
               geom_text(size=3, data=text2, mapping = aes(x=-Inf,y=-Inf, label=AIClab, hjust=-0.1, vjust=-1.25))+
               geom_text(size=3, data=text2, mapping = aes(x=-Inf,y=-Inf, label=BIClab, hjust=-0.1, vjust=-2.5))+
               # ggtitle(paste0("Current age trajectories for ",
               #                studyplot$Cohort,", ", studyplot$Group)) +
               theme_bw()
      )
      
      if (saveplots){
        ggsave(get(paste0(df$Cohort[i],"_",
                          df$Group[i],"_agetime")),
               filename=paste0(df$Cohort[i],"_",
                               df$Group[i],"_agetime_p",Age0_pctile,".jpg"),
               path=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Trajectories/Age0 p",
                           Age0_pctile,"/"),
               width=12,
               height=9,
               units="in"
        )
      }
    }
    
    

#Trajectories for co-authors to choose from. Will not go in paper and does not need to be checked.
# for (i in 1:nrow(df)){
#   #i<-1
#   studyplot<-allplotdf %>% filter(Cohort==df$Cohort[i], Group==df$Group[i], !is.na(years), ModelNum==4)
#   ylower<-min(studyplot$predY.lower.Ypred)
#   yupper<-max(studyplot$predY.upper.Ypred)
#   text<-studyplot %>% group_by(Cohort, Group, Expvar2, ModelNum2, ModelLet2) %>% 
#     select (Cohort, Group, Expvar2, ModelNum2, ModelLet2, AIC, BIC) %>% 
#     unique() %>% mutate(AIClab = paste0("AIC=",format(round(AIC,1),nsmall=1)),
#                         BIClab = paste0("BIC=",format(round(BIC,1), nsmall=1))) 
#   p1<-  ggplot(studyplot)+
#            geom_ribbon(aes(x=years, ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.2) +
#            geom_line(aes(x=years, y=predY.Ypred, group=Exposed, color=Exposed))+
#            coord_cartesian(ylim=c(ylower, yupper))+
#            ylab("Memory z-score") + xlab("Years since baseline") + labs(fill="Exposure status", color="Exposure status")+
#            facet_grid(rows=vars(ModelNum2), cols=vars(Expvar2, ModelLet2), scales=) +
#            geom_text(size=3, data=text, mapping = aes(x=-Inf,y=-Inf, label=AIClab, hjust=-0.1, vjust=-1.25))+
#            geom_text(size=3, data=text, mapping = aes(x=-Inf,y=-Inf, label=BIClab, hjust=-0.1, vjust=-2.5))+
#            # ggtitle(paste0("Time on study trajectories for ", 
#            #                studyplot$Cohort,", ", studyplot$Group)) +
#            theme_bw()
#   
#   studyplot2<-allplotdf %>% filter(Cohort==df$Cohort[i], Group==df$Group[i], is.na(years), ModelNum==6) %>% mutate(age_y=age_y_c65+65)
#   ylower<-min(studyplot2$predY.lower.Ypred)
#   yupper<-max(studyplot2$predY.upper.Ypred)
#   text2<-studyplot2 %>% group_by(Cohort, Group, Expvar2, ModelNum2, ModelLet2) %>% 
#     select (Cohort, Group, Expvar2, ModelNum2, ModelLet2, AIC, BIC) %>% 
#     unique() %>% mutate(AIClab = paste0("AIC=",format(round(AIC,1), nsmall=1)),
#                         BIClab = paste0("BIC=",format(round(BIC,1), nsmall=1))) 
#       p2<-   ggplot(studyplot2)+
#            geom_ribbon(aes(x=age_y, ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.2) +
#            geom_line(aes(x=age_y, y=predY.Ypred, group=Exposed, color=Exposed))+
#            coord_cartesian(ylim=c(ylower, yupper))+
#            ylab("Memory z-score") + xlab("Age (years)") + labs(fill="Exposure status", color="Exposure status")+
#            facet_grid(rows=vars(ModelNum2), cols=vars(Expvar2, ModelLet2), scales=) +
#            geom_text(size=3, data=text2, mapping = aes(x=-Inf,y=-Inf, label=AIClab, hjust=-0.1, vjust=-1.25))+
#            geom_text(size=3, data=text2, mapping = aes(x=-Inf,y=-Inf, label=BIClab, hjust=-0.1, vjust=-2.5))+
#            # ggtitle(paste0("Current age trajectories for ",
#            #                studyplot$Cohort,", ", studyplot$Group)) +
#            theme_bw()
#   
#   p3<-plot_grid(p1,p2, nrow=2)
#       
#   if (saveplots){
#     ggsave(p3,
#            filename=paste0(df$Cohort[i],"_",
#                            df$Group[i],"_p",Age0_pctile,"_choose.jpg"),
#            path=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Trajectories/Age0 p",
#                        Age0_pctile,"_choose/"),
#            width=12,
#            height=9,
#            units="in"
#     )
#   }
# }
# 
#     
#     
    
#Plots for final models in paper
    
    finalmodslist<-finalmods %>% select(Cohort, Group, modelname)
    
    allplotdf$modelname<-paste0(allplotdf$Expvar,"_m",allplotdf$ModelNum,allplotdf$ModelLet)
    final_plotdata<-left_join(finalmodslist,allplotdf, by=c("Cohort", "Group", "modelname"))
    
    
    plotfxn<-function(grp=NULL){
      #testing
      #grp<-"Overall"
      
      if(grp=="Overall"){in_wide<-12} else if(grp %in% c("Black", "White")){
          in_wide<-5} else {in_wide<-3}
      
      apoe_tos<-ggplot(final_plotdata %>% filter(Group==grp, ModelNum==4,Expvar=="apoe"),
                       aes(x=years, y=predY.Ypred, group=Exposed)) +
        geom_line(aes(color=Exposed))+
        geom_ribbon(aes(ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.1) +
        ylab("Memory z-score") + xlab("Years since baseline") + ylim(-5.1, 1.1) +
        labs(color="APOE ε4 alleles", fill="APOE ε4 alleles")+
        scale_fill_discrete(labels=c("None", "At least one"))+
        scale_color_discrete(labels=c("None", "At least one"))+
        #ggtitle("Cognitive decline for Models using Time on Study as Timescale") +
        facet_grid(cols=vars(Cohort), scales=)+theme_bw()+theme(legend.position = "none")
      
      
      apoe_age<-ggplot(final_plotdata %>% filter(Group==grp, ModelNum==6,Expvar=="apoe"),
                       aes(x=age_y, y=predY.Ypred, group=Exposed)) +
        geom_line(aes(color=Exposed))+
        geom_ribbon(aes(ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.1) +
        ylab("Memory z-score") + xlab("Age") + ylim(-5.1, 1.1) +
        labs(color="Any APOE ε4 allele", fill="Any APOE ε4 allele")+
        #ggtitle("Cognitive decline for Models using Time on Study as Timescale") +
        facet_grid(cols=vars(Cohort), scales=)+theme_bw()+theme(legend.position = "none")
      
      legend_apoe<-get_legend(apoe_tos+theme(legend.position = "bottom"))
      p_apoe<-plot_grid(apoe_tos, apoe_age, labels = c('A', 'B'), ncol=1)
      p_apoe_leg<-plot_grid(p_apoe, legend_apoe, ncol=1, rel_heights=c(1,0.05))
      
      p_apoe_leg
      
      ggsave(p_apoe_leg,
             filename=paste0("apoe_trajs_",grp,"_p",Age0_pctile,".jpg"),
             path=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Final figures/"),
             width=in_wide,
             height=7,
             units="in")
      
      
      diab_tos<-ggplot(final_plotdata %>% filter(Group==grp, ModelNum==4,Expvar=="diab"),
                       aes(x=years, y=predY.Ypred, group=Exposed)) +
        geom_line(aes(color=Exposed))+
        geom_ribbon(aes(ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.1) +
        ylab("Memory z-score") + xlab("Years since baseline") + ylim(-5.1, 1.1) +
        labs(color="Diabetes at baseline", fill="Diabetes at baseline")+
        scale_fill_discrete(labels=c("No", "Yes"))+
        scale_color_discrete(labels=c("No", "Yes"))+
        #ggtitle("Cognitive decline for Models using Time on Study as Timescale") +
        facet_grid(cols=vars(Cohort), scales=)+theme_bw()+theme(legend.position = "none")
      
      
      diab_age<-ggplot(final_plotdata %>% filter(Group==grp, ModelNum==6,Expvar=="diab"),
                       aes(x=age_y, y=predY.Ypred, group=Exposed)) +
        geom_line(aes(color=Exposed))+
        geom_ribbon(aes(ymin=predY.lower.Ypred, ymax=predY.upper.Ypred, fill=Exposed), alpha=0.1) +
        ylab("Memory z-score") + xlab("Age")+ylim(-5.1, 1.1) +
        #ggtitle("Cognitive decline for Models using Time on Study as Timescale") +
        facet_grid(cols=vars(Cohort), scales=)+theme_bw()+theme(legend.position = "none")
      
      legend_diab<-get_legend(diab_tos+theme(legend.position = "bottom"))
      p_diab<-plot_grid(diab_tos, diab_age, labels = c('A', 'B'), ncol=1)
      p_diab_leg<-plot_grid(p_diab, legend_diab, ncol=1, rel_heights=c(1,0.05))
      
      p_diab_leg
      
      ggsave(p_diab_leg,
             filename=paste0("diab_trajs_",grp,"_p",Age0_pctile,".jpg"),
             path=paste0(path_prefix,"Timescale project/Harmonized analyses/Output/Final figures/"),
             width=in_wide,
             height=7,
             units="in")
      
      
      
    }
    plotfxn("Overall") #Warning relates to use of epsilon character but no issues with jpg output.
    plotfxn("Black") #Warning relates to use of epsilon character but no issues with jpg output.
    plotfxn("White") #Warning relates to use of epsilon character but no issues with jpg output.
    plotfxn("Hispanic") #Warning relates to use of epsilon character but no issues with jpg output.

    
