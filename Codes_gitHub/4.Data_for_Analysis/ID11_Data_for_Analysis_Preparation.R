
#####################################################################################
# Script id: 11
# Description: This script creates the cleaned dataset for the statistical analysis
# Author: GIGA team
# Last update: 24.04.2021
#####################################################################################



##-- Libraries

rm(list = ls()) 

library(dplyr)
library(sentimentr)
library(magrittr)
library(tidyr)
library(readxl)



##-- Data for the first panel evaluation 

 # Merge applicants and panel-level dataset
 load("C:/Users/Stefano/Desktop/GIGA_Project/Project_applicants_kernel.RData")
 load("C:/Users/Stefano/Desktop/GIGA_Project/Project_panel_kernel.RData")
 
 xxx <- left_join(Project_applicants_kernel, Project_panel_kernel, by = c("ProjectNumber_OP", "ProjectYear")) %>% 
   distinct(ProjectNumber_OP, ProjectYear, .keep_all = TRUE) %>%
   na.omit()
 
 # Harmonize variable format
 xxx$ProjectYear <- as.factor(xxx$ProjectYear)
 xxx$Scheme <- as.factor(xxx$Scheme)
 
 # Create additional variables on gender (based on Female Ratio)
 xxx <- xxx %>%
   mutate(Female_dominated = ifelse(Female_ratio > .5, 1, 0),
          Female_presence = ifelse(Female_ratio > 0, 1, 0),
          Gender_balanced_team = ifelse(Female_ratio >= 0.4 & Female_ratio <= 0.6, 1, 0))
 
 # Define three macro-period based on project year
 xxx <- xxx %>% 
   mutate(Period = ifelse(ProjectYear %in% 2002:2004, 1,
                          ifelse(ProjectYear %in% 2005:2007, 2, 3)),
          Period_1 = ifelse(Period == 1, 1, 0),
          Period_2 = ifelse(Period == 2, 1, 0))
 

 # Merge info about the project
 Project_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_ProjectLvl")

 # Harmonize variable format
 Project_Kernel$ProjectNumber_FP <- toupper(Project_Kernel$ProjectNumber_FP)

 # Create panel acceptance variable
 Project_Kernel <- Project_Kernel %>%
  dplyr::select(ProjectNumber_FP, ESFDecision) %>%
  mutate(ESFDecision = ifelse(ESFDecision == "Accepted", 1, 0))

 xxx <- left_join(xxx, Project_Kernel)




##-- Data for the external reviewers evaluation

 # Merge project and reviewer-level dataset
 load("C:/Users/Stefano/Desktop/GIGA_Project/Project_applicants_kernel.RData")
 load("C:/Users/Stefano/Desktop/GIGA_Project/Project_ext_reviewers_kernel.RData")
 
 # Harmonize variable format
 Project_applicants_kernel$ProjectNumber_FP <- toupper(Project_applicants_kernel$ProjectNumber_FP)
 Project_Ext_Reviewers_Kernel$ProjectNumber_FP <- toupper(Project_Ext_Reviewers_Kernel$ProjectNumber_FP)
 
  Project_applicants_kernel <- Project_applicants_kernel %>%
   dplyr::select(ProjectNumber_FP, Female_ratio:BudgetR_per_indiv_log, -Cognitive_prox_team) %>%
   filter(ProjectNumber_FP != 0)
 
 dat <- left_join(Project_Ext_Reviewers_Kernel, Project_applicants_kernel, by = c("ProjectNumber_FP")) %>% 
   distinct(AU, ProjectNumber_FP, ProjectYear, .keep_all = TRUE) 


 # Merge with measure of cognitive proximity
 load("C:/Users/Stefano/Desktop/GIGA_Project/Cognitive_dist_team_reviewers.RData")

 Cognitive_prox_team_reviewers <- Cognitive_prox_team_reviewers %>%
   dplyr::select(ProjectNumber_FP, AU, Cognitive_prox_team_reviewers) 
 
 dat <- left_join(dat, Cognitive_prox_team_reviewers, by = c("ProjectNumber_FP", "AU")) %>%
   distinct(AU, ProjectNumber_FP, ProjectYear, .keep_all = TRUE)
 
 # Convert NAs to 0
 dat <- dat %>% 
   mutate(Cognitive_prox_team_reviewers = replace(Cognitive_prox_team_reviewers, is.na(Cognitive_prox_team_reviewers), 0)) 
 

 
#-- Standardize questions Q1 (scientific quality) and Q3 (team merit)
 
 dat$Q1 <- as.numeric(dat$Q1)
 dat$Q3 <- as.numeric(dat$Q3)
 
 dat <- dat %>%
   group_by(Scale) %>%
   mutate(Q1 = scale(Q1),
          Q2 = scale(Q2),
          Q3 = scale(Q3)) %>%
   ungroup()
 
 # Create the average reviewer score between Q1 and Q3
 dat <- dat %>%
   filter(!is.na(Q1) & !is.na(Q2) & !is.na(Q3)) %>%
   rowwise() %>%    
   mutate(Qavg = mean(c(Q1, Q3)))

 # Selected list of variables
 dat <- dat %>%
   dplyr::select(AU, ProjectNumber_FP, Age_reviewers, Age_log_reviewers, Female_reviewers, Publication_stock_reviewers, Publication_stock_log_reviewers, Cognitive_dist_team_reviewers, Shanghai_top100_reviewers, Qavg, Q1, Q3, Q1_Expl, Q3_Expl)


 # Merge with reviewer data
 xxx$ProjectYear <- as.factor(xxx$ProjectYear)
 yyy <- full_join(xxx, dat, by = "ProjectNumber_FP")


 # Double check the sample size 
 # yyy %>% distinct(ProjectNumber_OP, .keep_all = TRUE) %>% nrow()  
 # yyy %>% filter(ProjectNumber_FP != 0) %>% distinct(ProjectNumber_FP, .keep_all = TRUE) %>% nrow() 


##-- Identify problematic enties
 
 # 1.Project with reviewers score but not in first panel evaluation stage
 problems1 <- yyy %>% filter(is.na(Grant) & !is.na(Qavg)) %>% distinct(ProjectNumber_FP, .keep_all = TRUE) %>%
   mutate(problem1 = 1) %>%
   dplyr::select(ProjectNumber_FP, problem1)
 
 # 2.Project appearing in the first panel evaluation stage but no reviewers info
 problems2 <- yyy %>% filter(Grant == 1 & is.na(Qavg)) %>% distinct(ProjectNumber_FP, .keep_all = TRUE) %>%
   mutate(problem2 = 1) %>%
   dplyr::select(ProjectNumber_FP, problem2)
 
 # 3.Project appearing in the first panel evaluation stage but no evaluation decisions
 problems3 <- yyy %>% filter(Grant == 1 & is.na(ESFDecision)) %>% distinct(ProjectNumber_FP, .keep_all = TRUE) %>%
   mutate(problem3 = 1) %>%
   dplyr::select(ProjectNumber_FP, problem3)

 # Keep projects with full information throughout the evaluation process  
 zzz <- left_join(yyy, problems1)
 zzz <- left_join(zzz, problems2)
 zzz <- left_join(zzz, problems3)

 dat <- zzz %>%
   filter(is.na(problem1) & is.na(problem2) & is.na(problem3))

 
 # Double check the sample size on the clean version
 dat %>% distinct(ProjectNumber_OP, .keep_all = TRUE) %>% nrow()                                     # 1347 total projects in the first stage
 dat %>% filter(ProjectNumber_FP != "0") %>% distinct(ProjectNumber_FP, .keep_all = TRUE) %>% nrow() # 511 projects in the second stage



#-- Create sub-sample for different evaluation stages

 dat_1st <- dat %>% distinct(ProjectNumber_OP, .keep_all = TRUE)
 dat_2nd <- dat %>% filter(Grant == 1)
 dat_3rd <- dat %>% 
   group_by(ProjectNumber_FP) %>%
   mutate(Qavg_reviewers_mean = mean(Qavg),      # Variable for average reviewer scores
          Qavg_reviewers_var = var(Qavg)) %>%    # Variabble for variance of reviewer scores 
   ungroup() %>%
   distinct(ProjectNumber_FP, .keep_all = TRUE) %>% 
   filter(!is.na(ESFDecision))
  
 rm(Cognitive_prox_team_reviewers, dat, problems1, problems2, problems3, Project_Kernel, Project_panel_kernel,
    Project_applicants_kernel, Project_Ext_Reviewers_Kernel, xxx, yyy, zzz)



#-- Sentiment analysis

 # Join corpus of Q1 and Q3
 dat_2nd <- dat_2nd %>%
   unite(Q_Corpus, c("Q1_Expl", "Q3_Expl"))

 # Model VADER
 library(vader)
 
 vader_scores <- vader_df(dat_2nd$Q_Corpus)
 vader_scores <- vader_scores %>% dplyr::select(compound, pos, neu, neg) %>%
   rename(vader = compound)
 
 dat_2nd <- cbind(dat_2nd, vader_scores)
 
 # Model SentimentR 
 # Note: not compatible with library(syuzhet)
 xxx <- dat_2nd %>%
   dplyr::mutate(dialogue_split = sentimentr::get_sentences(Q_Corpus)) %$%
   sentiment_by(dialogue_split, list(AU, ProjectNumber_FP))

 dat_2nd <- inner_join(dat_2nd, xxx, by = c("AU", "ProjectNumber_FP"))

 # Model Syuzhet
 library(syuzhet)
 syuzhet <- get_sentiment(dat_2nd$Q_Corpus, method = "nrc")
 
 dat_2nd <- cbind(dat_2nd, syuzhet)
 
 rm(vader_scores, xxx, incl_nt, neu_set, syuzhet)



#-- Create dummy variable for the presence of appreciators

 dat_2nd$word2vec <- as.integer(str_detect(dat_2nd$Q_Corpus, paste(c("connected internationally", "considerable_experience", "excellent_track", "highly qualified", "highly respected",
                                                                     "international connection", "international reputation", "international standing", "internationally competitive", "internationally connected", 
                                                                     "internationally recognized", "internationally renowned", "leading expert", "leading scientists", "numerous cooperations",
                                                                     "significant contribution", "track record", "world leader", "world leading", "world renowned"), collapse = '|')))

##-- Save the final dataset
 
save.image("C:/Users/Stefano/Desktop/GIGA_Project/Data_for_analysis.RData")







