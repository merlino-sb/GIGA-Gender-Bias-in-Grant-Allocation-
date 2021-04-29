
#####################################################################################
# Script id: 15
# Description: This script creates the coefficient plot of the main results
# Author: GIGA team
# Last update: 24.04.2021
#####################################################################################



##-- Libraries

 rm(list = ls())
 
 library(dplyr)
 library(lmtest)
 library(sandwich)
 library(ggplot2)
 library(tidyr)
 library(mfx)
 library(sampleSelection)
 library(broom)
 library(socviz)


 load("C:/Users/Stefano/Desktop/GIGA_Project/Data_for_analysis.RData")
 
 
##---- PROBIT FIRST PANEL EVALUATION ----
 
 # Estimation
 mod_1st <- probitmfx(Grant ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, data = dat_1st, robust = T, clustervar1 = "Scheme")
 
 # Formating
 out_conf <- tidy(mod_1st, conf.int = TRUE)
 out_conf %>% round_df()
 
 # Keep significant variables
 out_conf <- subset(out_conf, term %in% c("Female_ratio", "Cognitive_dist_team", "Blau_index_team", "Network_team_panel", "Shanghai_top100_team",
                                          "Already_success_past_team", "Number_countries_log", "Panel_size_log", "Project_panel_ratio_log"))

 # Change labels
 out_conf <- out_conf %>% 
   mutate(term = ifelse(term == "Female_ratio", "Female Ratio",
                        ifelse(term == "Cognitive_dist_team", "Cognitive Proximity",
                               ifelse(term == "Blau_index_team", "Research Diversity",
                                      ifelse(term == "Network_team_panel", "Network Proximity",
                                             ifelse(term == "Shanghai_top100_team", "Shanghai Ranking",
                                                    ifelse(term == "Already_success_past_team", "Past EUROCORES Grant",
                                                           ifelse(term == "Number_countries_log", "Number of Countries",
                                                                  ifelse(term == "Panel_size_log", "Panel Size", "Panel Workload")))))))))
 
 # Create the plot
 out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")
  out_conf$title <- "1st Stage"
 
 p_1st <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate),
                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
   geom_pointrange(width=0.2, size=1, color="black", fill="orange", shape=22) + 
   coord_flip() + 
   labs(x="", y= "") + 
   geom_hline(yintercept = 0, linetype ="dashed", color ="gray", size=1) +
   facet_grid(. ~ title) +
   theme_bw() + 
   theme(legend.title=element_blank(),
         legend.text=element_text(size=25),
         text = element_text(size = 25), 
         strip.background = element_rect(fill="azure2")) 



##---- REVIEWERS EVALUATION (SCORES) ----

 # Model 1st stage
 mod2 <- glm(Grant ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, 
             family = binomial( link = "probit" ), data = dat_1st)
 
 dat_1st$IMR1_mod2 <- invMillsRatio(mod2)$IMR1
 
 # Get Inverse Mill's Ratio
 mills_ratio <- dat_1st %>% dplyr::select(ProjectNumber_FP, IMR1_mod2)
 dat_2nd <- left_join(dat_2nd, mills_ratio, by = c("ProjectNumber_FP"))
 
 # Estimation
 mod_2nd <- lm(Qavg ~ IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)
 mod_2nd <- coeftest(mod_2nd, vcov=vcovHC(mod_2nd, type = "HC0", cluster = "Scheme"))
 
 # Formating
 out_conf <- tidy(mod_2nd, conf.int = TRUE)
 out_conf%>% round_df()
 
 # Keep significant variables
 out_conf <- subset(out_conf, term %in% c("Female_ratio", "Publication_stock_log_team", "Star_scientist_99_team", "Cognitive_dist_team_reviewers", "Publication_stock_log_reviewers"))
 
 # Change labels
 out_conf <- out_conf %>% 
   mutate(term = ifelse(term == "Female_ratio", "Female Ratio",
                        ifelse(term == "Star_scientist_99_team", "Star Scientist",
                               ifelse(term == "Publication_stock_log_team", "Productivity",
                                      ifelse(term == "Cognitive_dist_team_reviewers", "Cognitive Proximity", "Productivity Reviewer")))))
 
 # Create the plot
 out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")
 out_conf$title <- "2nd Stage [Scores]"
 
 p_2nd <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate),
                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
   geom_pointrange(width=0.2, size=1, color="black", fill="orange", shape=22) + 
   coord_flip() + 
   labs(x="", y= "") + 
   geom_hline(yintercept = 0, linetype ="dashed", color ="gray", size=1) +
   facet_grid(. ~ title) +
   theme_bw() + 
   theme(legend.title=element_blank(),
         legend.text=element_text(size=25),
         text = element_text(size = 25), 
         strip.background = element_rect(fill="azure2")) 
 
 

##---- REVIEWERS EVALUATION (SENTIMENTS) ----

 # Estimation
 mod_2nd <- lm(vader ~ IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_dist_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)
 mod_2nd <- coeftest(mod_2nd, vcov=vcovHC(mod_2nd, type = "HC0", cluster = "Scheme"))
 
 # Formating
 out_conf <- tidy(mod_2nd, conf.int = TRUE)
 out_conf %>% round_df()
 
 # Keep significant variables
 out_conf <- subset(out_conf, term %in% c("Female_ratio", "Publication_stock_log_reviewers", "Age_log_reviewers"))
 
 # Change labels
 out_conf <- out_conf %>% 
   mutate(term = ifelse(term == "Female_ratio", "Female Ratio",
                               ifelse(term == "Publication_stock_log_reviewers", "Productivity Reviewer", "Age Reviewer")))
 
 # Create the plot
 out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")
 out_conf$title <- "2nd Stage [Sentim.]"
 
 p_2nd_sent <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate),
                                         y = estimate, ymin = conf.low, ymax = conf.high)) +
   geom_pointrange(width=0.2, size=1, color="black", fill="orange", shape=22) + 
   coord_flip() + 
   labs(x="", y= "") + 
   geom_hline(yintercept = 0, linetype ="dashed", color ="gray", size=1) +
   facet_grid(. ~ title) +
   theme_bw() + 
   theme(legend.title=element_blank(),
         legend.text=element_text(size=25),
         text = element_text(size = 25), 
         strip.background = element_rect(fill="azure2")) 
 
 
 

##---- PROBIT SECOND PANEL EVALUATION ----

 # Estimation
 mod_3rd <- probitmfx(ESFDecision ~ Female_ratio + Gender_balanced_team + Qavg_reviewers_mean + Qavg_reviewers_var + Publication_stock_log_team + Star_scientist_99_team + Cognitive_dist_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, data = dat_3rd, robust = T, clustervar1 = "Scheme")
 
 # Formating
 out_conf <- tidy(mod_3rd, conf.int = TRUE)
 out_conf %>% round_df()
 
 # Keep significant variables
 out_conf <- subset(out_conf, term %in% c("Female_ratio", "Qavg_reviewers_mean", "Network_team_panel", "Panel_size_log", "Project_panel_ratio_log"))
 
 # Change labels
 out_conf <- out_conf %>% 
   mutate(term = ifelse(term == "Female_ratio", "Female Ratio",
                        ifelse(term == "Qavg_reviewers_mean", "Reviewers Scores",
                               ifelse(term == "Network_team_panel", "Network Proximity",
                                      ifelse(term == "Panel_size_log", "Panel Size", "Panel Workload")))))
 
 # Create the plot
 out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")
 out_conf$title <- "3rd Stage"
 
 p_3rd <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate),
                                         y = estimate, ymin = conf.low, ymax = conf.high)) +
   geom_pointrange(width=0.2, size=1, color="black", fill="orange", shape=22) + 
   coord_flip() + 
   labs(x="", y= "") + 
   geom_hline(yintercept = 0, linetype ="dashed", color ="gray", size=1) +
   facet_grid(. ~ title) +
   theme_bw() + 
   theme(legend.title=element_blank(),
         legend.text=element_text(size=25),
         text = element_text(size = 25), 
         strip.background = element_rect(fill="azure2")) 
 
 
# Attach the four plots 
 require(gridExtra)
 grid.arrange(p_1st, p_2nd, p_2nd_sent, p_3rd, ncol=4)
 
