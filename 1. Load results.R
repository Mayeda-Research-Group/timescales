#################################
# Author: Eleanor Hayes-Larson
# Purpose: load all output from cohorts
#################################

##############################
# Part 1. Initializations
##############################

library(lcmm)
library(splines)
library(tidyverse)
library(gtsummary)
library(gt)
library(cowplot)
library(flextable)

#Set up filepaths
  mac<-T
  path_prefix<-ifelse(mac,
                      "/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/", 
                      "C:/Users/ehlarson/Box/")
  path_to_output <- paste0(path_prefix,"Timescale project/Harmonized analyses/Output from co-authors/")

#pre-process MCSA to get matching format
  if (!file.exists(paste0(path_to_output,"output_MCSA_Overall.Rdata"))){
    load(paste0(path_to_output,"output_MCSA_Overall.Rda"))
    output_MCSA_Overall<-list(desc_Overall, apoemods_Overall, diabmods_Overall)
    save(output_MCSA_Overall, file=paste0(path_to_output,"output_MCSA_Overall.Rdata"))
    rm(desc_Overall, apoemods_Overall, diabmods_Overall, output_MCSA_Overall)
  }
  
#Get list of files to load in and create empty dataframe based on names
    files<-list.files(path_to_output, pattern = c(".Rdata"))
    cg_names<-substr(files, 1, nchar(files)-6)
    
    extract_names<-c("Cohort", "Group", "N")
    
    df<-data.frame(matrix(NA, nrow=length(files), ncol=length(extract_names)))
    colnames(df)<-extract_names

#############################
# Part 2. Load data
#############################
   
    #Loop over each cohort and group, creating an object with the output from that cohort group, and
    # a "df" object that contains some basic info about each group (spline knots, etc.)
     for (i in 1:length(cg_names)){
      assign(cg_names[i], get(load(paste0(path_to_output,files[i]))))
      
      #Extract current age and time on study percentiles
      df$Cohort[i] <-  str_split(cg_names[i], pattern="_", n = Inf, simplify = T)[2]
      df$Group[i] <-  str_split(cg_names[i],pattern="_", n = Inf, simplify = T)[3]
      df$N[i] <- get(cg_names[i])[[1]]$tab1["N"]
      df$Age_p5[i] <-get(cg_names[i])[[1]]$knots_age[1]
      df$Age_p33[i] <-get(cg_names[i])[[1]]$knots_age[2]
      df$Age_p50[i] <-get(cg_names[i])[[1]]$knots_age[3]
      df$Age_p67[i] <-get(cg_names[i])[[1]]$knots_age[4]
      df$Age_p95[i] <-get(cg_names[i])[[1]]$knots_age[5]
      df$Time_p5[i] <-get(cg_names[i])[[1]]$knots_time[1]
      df$Time_p33[i] <-get(cg_names[i])[[1]]$knots_time[2]
      df$Time_p50[i] <-get(cg_names[i])[[1]]$knots_time[3]
      df$Time_p67[i] <-get(cg_names[i])[[1]]$knots_time[4]
      df$Time_p95[i] <-get(cg_names[i])[[1]]$knots_time[5]
      df$Age0_p5[i] <-get(cg_names[i])[[1]]$knots_age0[1]
      df$Age0_p33[i] <-get(cg_names[i])[[1]]$knots_age0[2]
      df$Age0_p50[i] <-get(cg_names[i])[[1]]$knots_age0[3]
      df$Age0_p67[i] <-get(cg_names[i])[[1]]$knots_age0[4]
      df$Age0_p95[i] <-get(cg_names[i])[[1]]$knots_age0[5]
      
      df$FUtime_p90[i] <- get(cg_names[i])[[1]]$tab1["table_body"] %>% 
                            as.data.frame() %>% filter(table_body.variable=="fu_time", 
                                                       table_body.label=="Minimum, 10%, 25%, Median, 75%, 90%, Maximum") %>% 
                            select(table_body.stat_0) %>% as.character() %>% str_split(.,",",simplify = T) %>% 
                            as_tibble() %>% select(V6) %>% as.numeric()
      
      
      df$Age0_p25wide[i] <- get(cg_names[i])[[1]]$tab1["table_body"] %>%
        as.data.frame() %>% filter(table_body.variable=="age0",
                                   table_body.label=="Minimum, 10%, 25%, Median, 75%, 90%, Maximum") %>%
        select(table_body.stat_0) %>% as.character() %>% str_split(.,",",simplify = T) %>%
        as_tibble() %>% select(V3) %>% as.numeric()

      df$Age0_p50wide[i] <- get(cg_names[i])[[1]]$tab1["table_body"] %>%
        as.data.frame() %>% filter(table_body.variable=="age0",
                                   table_body.label=="Minimum, 10%, 25%, Median, 75%, 90%, Maximum") %>%
        select(table_body.stat_0) %>% as.character() %>% str_split(.,",",simplify = T) %>%
        as_tibble() %>% select(V4) %>% as.numeric()
      
      df$Age0_p75wide[i] <- get(cg_names[i])[[1]]$tab1["table_body"] %>%
        as.data.frame() %>% filter(table_body.variable=="age0",
                                   table_body.label=="Minimum, 10%, 25%, Median, 75%, 90%, Maximum") %>%
        select(table_body.stat_0) %>% as.character() %>% str_split(.,",",simplify = T) %>%
        as_tibble() %>% select(V5) %>% as.numeric()

    }

  #Remove last set of results loaded (since it was renamed)
    rm(outputlist_Black, outputlist_Overall, outputlist_White)
    

#############################
# Part 3. Extract model fits and compare model choices
#############################
    
    #Pull together all APOE model results including estimates and convergence, and fit
    model_fits<-expand.grid(CG=cg_names, 
                            Exposure=c("apoe", "diab"), 
                            ModelNum=seq(1,9),  #Label for timescale and Age0 spec in model
                            ModelLet=c("a", "b", "c"), #Label for timescale spec in model (linear to splines)
                            conv=NA, #Indicator for whether model converged
                            AIC=NA,
                            BIC=NA)
    
    #colnames(model_fits)<-c("Cohort_Group", "Exposure", "ModelNum", "ModelLet", "conv")
    model_fits$Cohort <- str_split(model_fits$CG, pattern="_", n = Inf, simplify = T)[,2]
    model_fits$Group <- str_split(model_fits$CG, pattern="_", n = Inf, simplify = T)[,3]
    model_fits$bin <- ifelse(model_fits$Exposure=="apoe", "[[2]]", "[[3]]") #used for paste0 to extract statistics from output
    model_fits$modelname <- paste0(model_fits$Exposure,"_m",model_fits$ModelNum, model_fits$ModelLet)
    
    
    #Remove irrelevant rows:
      #ARIC and FHS don't have enough obs for "c" models 
      model_fits <- model_fits %>% filter (((!(Cohort=="ARIC" & ModelLet=="c")) & 
                                        (!(Cohort=="FHS" & ModelLet=="c"))))

    
    for (i in 1:nrow(model_fits)){
      #print(i)
      indexing<-paste0(model_fits$CG[i], model_fits$bin[i],"$", model_fits$modelname[i])
      model_fits$conv[i] <- eval(parse(text=paste0(indexing,"[['conv']]")))
      model_fits$AIC[i] <- eval(parse(text=paste0(indexing,"[['AIC']]")))
      model_fits$BIC[i] <- eval(parse(text=paste0(indexing,"[['BIC']]")))
    }
    
    table(model_fits$CG, model_fits$ModelLet)
    
    #Keep models that converged and order by cohort/group, exposure, model # and letter
     model_fits <- model_fits %>% filter(conv==1) %>% arrange(CG,Exposure,ModelNum, ModelLet)
    
     table(model_fits$CG, model_fits$ModelLet)
     
    #Merge on 95th percentile of ToS
      df2<-df %>% select(Cohort, Group, Time_p95)
      model_fits <-model_fits %>% left_join(.,df2, by=c("Cohort", "Group"))
    
    #Keep different options for "final models"
     #Keep most flexible
      mostflex_mods<-model_fits %>% group_by(CG, Exposure, ModelNum) %>% slice_tail()
     #Keep lowest AIC 
      lowest_AIC_mods<-model_fits %>% group_by(CG, Exposure, ModelNum) %>%
        arrange(CG,Exposure,ModelNum, AIC) %>% slice_head()
     #Keep lowest BIC 
      lowest_BIC_mods<-model_fits %>% group_by(CG, Exposure, ModelNum) %>%
        arrange(CG,Exposure,ModelNum, BIC) %>% slice_head()
      
    
    #Compare final models
    model_comp<-merge(mostflex_mods, lowest_AIC_mods,
                      by=c("CG", "Exposure", "ModelNum"))  %>% 
                rename(mostflex=ModelLet.x,
                       lowestAIC=ModelLet.y,
                       AICflex=AIC.x,
                       BICflex=BIC.x) %>%
                select(CG, Exposure, ModelNum, mostflex, lowestAIC, AICflex, BICflex) %>%
                merge(., lowest_BIC_mods,
                      by=c("CG", "Exposure", "ModelNum"))  %>%
                rename(lowestBIC=ModelLet) %>%
                 select(CG, Exposure, ModelNum, mostflex, lowestAIC, lowestBIC, 
                            AICflex, BICflex, AIC, BIC)
    
    agreementtab<-table(model_comp$mostflex, model_comp$lowestAIC, model_comp$lowestBIC) %>%
      data.frame() %>% rename(mostflex=Var1, lowestAIC=Var2, lowestBIC=Var3) %>%
      filter(Freq!=0)
    agreementtab


      
#############################
# Part 4. Set up inputs for predicted trajectories and diff in diff
#############################
      
pred_input<-merge(df,model_fits, by=c("Cohort", "Group")) %>% 
            rename(Time_p95=Time_p95.x) %>% select(-Time_p95.y)

#Set up output to pull knots in spline functions
alloutput<-mget(grep("output", ls(), value=TRUE))
alloutput$path_to_output<-NULL

#Rename knots to same name as in models (for predictions)
for (j in 1:length(alloutput)){
  alloutput[[j]][[1]]$knots_age_y_c65<-alloutput[[j]][[1]]$knots_age
  alloutput[[j]][[1]]$knots_years<-alloutput[[j]][[1]]$knots_time
  alloutput[[j]][[1]]$knots_age0_c65<-alloutput[[j]][[1]]$knots_age0
}
