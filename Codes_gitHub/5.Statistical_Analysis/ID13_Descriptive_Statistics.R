
#########################################################################################
# Script id: 13
# Description: This script produces the descriptive statistcs for each evaluation stage
# Author: GIGA team
# Last update: 24.04.2021
#########################################################################################



##-- Libraries

 rm(list = ls())
 
 library(dplyr)
 library(stargazer)
 library(tidyr)
 library(plotly)


 load("C:/Users/Stefano/Desktop/GIGA_Project/Data_for_analysis.RData")


#--- Descriptive statistics first panel evaluation (1st stage)

 xxx <- dat_1st %>% dplyr::select(Grant, Female_ratio, Female_presence, Female_dominated, Gender_balanced_team, Publication_stock_team, Star_scientist_99_team, 
                                  Cognitive_prox_team, Network_prox_team_panel, Blau_index_team, Team_size, Age_team, Shanghai_top100_team, Company_team,
                                  Already_success_past_team, BudgetR_per_indiv, Number_countries)
 
 stargazer(as.data.frame(xxx),
           covariate.labels = c("Grant", "Female Ratio Team", "Female Presence Team", "Female Dominated Team", "Gender Balanced Team",
                                "Productivity Team", "Star Scientist Team",
                                "Cognitive Proximity Team", "Network proximity", "Research Diversity Team", "Team Size", 
                                "Age Team", "Shanghai Ranking Team", "Private Partnership",
                                "Past EUROCORES Grant Team", "Budget Requested", "Number of Countries"),
           summary.stat = c("mean", "sd", "min", "max"))




#--- Descriptive statistics for external reviewers

 xxx <- dat_2nd %>% dplyr::select(Qavg, ave_sentiment, syuzhet, vader, word2vec, Female_reviewers, Publication_stock_reviewers, Age_reviewers, Shanghai_top100_reviewers)
 stargazer(as.data.frame(xxx),
           covariate.labels = c("Reviewer Score", "Reviewer Sentiment Score [SentimentR]", "Reviewer Sentiment Score [Syuzhet]", "Reviewer Sentiment Score [VADER]", "Reviewer Evaluative Terms",
                                "Female Reviewer", "Productivity Reviewer", "Age Reviewer", "Shanghai Ranking Reviewer"),
           summary.stat = c("mean", "sd", "min", "max")) 




#--- Descriptive statistics reviewer evaluation (2nd stage)

 xxx <- dat_2nd %>% 
   distinct(ProjectNumber_FP, .keep_all = TRUE) %>%
   dplyr::select(Female_ratio, Female_presence, Female_dominated, Gender_balanced_team, Publication_stock_team, Star_scientist_99_team, Cognitive_dist_team_reviewers,
                 Blau_index_team, Team_size, Age_team, Shanghai_top100_team, Company_team, 
                 Already_success_past_team, BudgetR_per_indiv, Number_countries, Domain)
 
 stargazer(as.data.frame(xxx),
        covariate.labels = c("Female Ratio Team", "Female Presence Team", "Female Dominated Team", "Gender Balanced Team",
                             "Productivity Team", "Star Scientist Team",
                             "Cognitive Proximity Team", "Research Diversity Team", "Team Size", 
                             "Age Team", "Shanghai Ranking Team", "Private Partnership",
                             "Past EUROCORES Grant Team", "Budget Requested", "Number of Countries",
                             "Physics Engineering", "Life Sciences Biomedicine", "Social Sciences Humanities"),
        summary.stat = c("mean", "sd", "min", "max"))

 xxx <- dat_2nd %>% filter(ProjectNumber_FP != 0) %>% distinct(ProjectNumber_FP, .keep_all = TRUE)
 sum(xxx$Team_size)   
   
 
 
 
#--- Descriptive statistics second panel evaluation (3rd stage)  
   
xxx <- dat_3rd %>% 
  filter(ESFDecision == 1) %>%
  dplyr::select(Female_ratio, Female_presence, Female_dominated, Gender_balanced_team, Publication_stock_team, Star_scientist_99_team, 
                Cognitive_dist_team, Network_team_panel,
                Blau_index_team, Team_size, Age_team, Shanghai_top100_team, Company_team,
                Already_success_past_team, BudgetR_per_indiv, Number_countries, Domain)
     
 stargazer(as.data.frame(xxx),
           covariate.labels = c("Female Ratio Team", "Female Presence Team", "Female Dominated Team", "Gender Balanced Team",
                                "Productivity Team", "Star Scientist Team",
                                "Cognitive Proximity Team", "Network proximity", "Research Diversity Team", "Team Size", 
                                "Age Team", "Shanghai Ranking Team", "Private Partnership", 
                                "Past EUROCORES Grant Team", "Budget Requested", "Number of Countries"),
           summary.stat = c("mean", "sd", "min", "max"))   
 
 xxx <- dat_3rd %>% filter(ProjectNumber_FP != 0) %>% distinct(ProjectNumber_FP, .keep_all = TRUE)
 sum(xxx$Team_size) 
 
 
 
 
#-- Plot on gender figures (Figure 2 in the paper) --
 
 data <- data.frame("x" = c("Applicants: Female Presence", "Applicants: Female Share", "Evaluators: Female Share"), 
                    "y1" = c(0.641, 0.198, 0.203),
                    "y2" = c(0.601, 0.175, 0.170),
                    "y3" = c(0.585, 0.167, 0.203)
 )
 
 data$x <- factor(data$x, levels = data[["x"]])
 
 hline <- function(y = 0, color = "orange") {
   list(
     type = "line", 
     x0 = 0, 
     x1 = 1, 
     xref = "paper",
     y0 = y, 
     y1 = y, 
     line = list(color = color, width = 1, dash = 'dash')
   )
 }
 
 
 p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = '1st Stage (OP submitted)', marker = list(color = 'rgb(49,130,189)')) %>%
   add_trace(y = ~y2, name = '2nd Stage (FP reviewed)', marker = list(color = 'rgb(204,204,204)')) %>%
   add_trace(y = ~y3, name = '3rd Stage (FP recommended for funding)', marker = list(color = 'rgb(229, 231, 233)')) %>%
   layout(
     title = list(text = '<b>Gender statistics per selection stage<b>', x = 0),
     xaxis = list(tickfont = list(size = 20), title = ""),
     yaxis = list(tickfont = list(size = 20), title = ""),
     legend = list(font = list(size = 20)),,
     margin = list(b = 100),
     shapes = list(hline(0.5)),
     barmode = 'group')
 
 p 
   