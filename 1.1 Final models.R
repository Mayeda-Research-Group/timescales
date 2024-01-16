#################################
# Author: Eleanor Hayes-Larson
# Purpose: keep list of final sets of models
#################################
library(openxlsx)

#Load output:
source("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/Timescale project/Harmonized analyses/1. Load results.R")


#Decisions from coauthors found in:

coauth_resp<-read.xlsx(paste0(path_to_output,'Melodem DWS Timescale model choice (Responses).xlsx'))

coauth_resp2<- coauth_resp %>% rename(CG1=3,
                                     CG_TOS1=4,
                                     CG_AGE1=5,
                                     CG2=7,
                                     CG_TOS2=8,
                                     CG_AGE2=9,
                                     CG3=10,
                                     CG_TOS3=11,
                                     CG_AGE3=12,
                                     CG4=14,
                                     CG_TOS4=15,
                                     CG_AGE4=16) %>% select(contains("CG")) %>%
                mutate(CG1= case_when(CG1=="3C Overall"~"3City Overall",
                                      TRUE ~ CG1))

coauth_resp3<-bind_rows(coauth_resp2 %>% select(contains("1")) %>% 
                          rename(CG=CG1, CG_TOS=CG_TOS1, CG_AGE=CG_AGE1), 
                    coauth_resp2 %>% select(contains("2")) %>% 
                      rename(CG=CG2, CG_TOS=CG_TOS2, CG_AGE=CG_AGE2),
                    coauth_resp2 %>% select(contains("3")) %>% 
                      rename(CG=CG3, CG_TOS=CG_TOS3, CG_AGE=CG_AGE3),
                    coauth_resp2 %>% select(contains("4")) %>% 
                      rename(CG=CG4, CG_TOS=CG_TOS4, CG_AGE=CG_AGE4)) %>%
                pivot_longer(., cols=c(CG_TOS, CG_AGE)) %>% 
                  mutate(ModelNum=case_when(name=="CG_TOS" ~ 4,
                                            name=="CG_AGE" ~ 6),
                         ModelLet=case_when(value=="Linear" ~ "a",
                                            value=="Spline (knot at p50)" ~ "b",
                                            value=="Spline (knots at p33, p67)" ~ "c")) %>% na.omit()
                         
coauth_resp3$Cohort <- str_split(coauth_resp3$CG, 
                                 pattern=" |,", n = Inf, simplify = T)[,1]

temp<-data.frame(str_split(coauth_resp3$CG, 
                                pattern=" |,", n = Inf, simplify = T)) 

temp2 <-cbind(temp[1], mycol = na.omit(unlist(temp[-1]))) %>% filter(mycol != "") %>% data.frame()

coauth_resp3$Group<- temp2$mycol

coauth_resp3$Group<- ifelse(coauth_resp3$Group=="white", "White", coauth_resp3$Group)


finalmods<-inner_join(model_fits, coauth_resp3, by=c("Cohort"="Cohort", 
                                                     "Group"="Group", 
                                                     "ModelNum"="ModelNum", 
                                                     "ModelLet"="ModelLet"), keep=FALSE) %>%
  
  select(-CG.y, -name, -value) %>% rename(CG=CG.x)

finalmods_tab<-finalmods %>% filter(Exposure=="apoe") %>% select(Cohort, Group, ModelNum, ModelLet) %>%
          mutate(ModelLet = case_when(ModelLet=="a" ~ "Linear",
                                      ModelLet=="b" ~ "Spline (knot at p50)",
                                      ModelLet=="c" ~ "Spline (knots at p33, p67)"),
                 Group=factor(Group, levels=c("Overall", "Black", "White", "Hispanic"))) %>%
          pivot_wider(.,names_from=ModelNum, names_prefix="Mod_",values_from = ModelLet) %>%
          arrange(Group, Cohort) %>% rename(`Time on study model`=Mod_4, `Age model`=Mod_6)
