

#####################################################################################
# Script id: 14
# Description: This script performs statistical analysis
# Author: GIGA team
# Last update: 24.04.2021
#####################################################################################



##-- Libraries

 rm(list = ls())
 
 library(dplyr)
 library(lmtest)
 library(sandwich)
 library(corrplot)
 library(plm)
 library(stargazer)
 library(tidyr)
 library(mfx)
 library(texreg)
 library(sampleSelection)


 load("C:/Users/Stefano/Desktop/GIGA_Project/Data_for_analysis.RData")


##---- PROBIT FIRST PANEL EVALUATION ----

 mod1 <- probitmfx(Grant ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Domain + ProjectYear + Scheme, data = dat_1st, robust = T, clustervar1 = "Scheme")
 mod2 <- probitmfx(Grant ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Domain + ProjectYear + Scheme, data = dat_1st, robust = T, clustervar1 = "Scheme")
 mod3 <- probitmfx(Grant ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, data = dat_1st, robust = T, clustervar1 = "Scheme")
 
  #Test for possible non-linear effects [NO EFFECTS FOUND]
  #probitmfx(Grant ~ Female_ratio + I(Female_ratio*Female_ratio_panel) + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, data = dat_1st, robust = T, clustervar1 = "Scheme")
 
 mods  = list(mod1$fit, mod2$fit, mod3$fit)
 coefs = list(c(0, mod1$mfxest[, 1]), c(0, mod2$mfxest[, 1]), c(0, mod3$mfxest[, 1]))
 ses   = list(c(0, mod1$mfxest[, 2]), c(0, mod2$mfxest[, 2]), c(0, mod3$mfxest[, 2]))
 pvals = list(c(0, mod1$mfxest[, 4]), c(0, mod2$mfxest[, 4]), c(0, mod3$mfxest[, 4]))
 tr1   = texreg(mods,
                override.coef = coefs,
                override.se = ses,
                override.pval = pvals,
                stars = c(0.01,  0.05, 0.10),
                omit.coef = "(Intercept)|(ProjectYear)|(Scheme)|(Domain)",
                custom.coef.names=c("Female Ratio Team", "Gender Balanced Team", "Productivity Team", "Star Scientist Team",
                                    "Cognitive Proximity Team", "Research Diversity Team", "Network Proximity Team", "Team Size", 
                                    "Age Team", "Shanghai Ranking Team", "Private Partnership", 
                                    "Past EUROCORES Grant Team", "Budget Requested", "Number of Countries", 
                                    "Female Ratio Panel", "Productivity Panel", "Panel Size", "Age Panel", 
                                    "Panel Workload", "Shanghai Ranking Panel"),
                caption.above = TRUE,
                caption = "Results",
                dcolumn = TRUE,
                custom.note = "ciao ciao",
                custom.model.names = c("(1)","(2)","(3)"),
                return.string = TRUE,
                digits = 3)  
 
 
#-- Calculate Inverse Mill's Ratio 
# Note: we run 2 possible specifications of the selection equation as robustness check
 mod1 <- glm(Grant ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Domain + ProjectYear + Scheme, 
             family = binomial( link = "probit" ), data = dat_1st)
 mod2 <- glm(Grant ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, 
             family = binomial( link = "probit" ), data = dat_1st)
 
 dat_1st$IMR1_mod1 <- invMillsRatio(mod1)$IMR1
 dat_1st$IMR1_mod2 <- invMillsRatio(mod2)$IMR1
 
 mills_ratio <- dat_1st %>% dplyr::select(ProjectNumber_FP, IMR1_mod1, IMR1_mod2)




##---- OLS REVIEWERS SCORES ----

# Attach Inverse Mill's Ratio
 dat_2nd <- left_join(dat_2nd, mills_ratio, by = c("ProjectNumber_FP"))

 mod1 <- lm(Qavg ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Domain + ProjectYear + Scheme, data = dat_2nd)
 mod3 <- lm(Qavg ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Domain + ProjectYear + Scheme, data = dat_2nd)
 mod5 <- lm(Qavg ~ Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, data = dat_2nd)
 
 mod2 <- lm(Qavg ~ IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Domain + ProjectYear + Scheme, data = dat_2nd)
 mod4 <- lm(Qavg ~ IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Domain + ProjectYear + Scheme, data = dat_2nd)
 mod6 <- lm(Qavg ~ IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)

   # Test for possible interaction effects with gender reviewers [NO EFFECTS FOUND]
   # mod <- lm(Qavg ~ IMR1_mod2 + Female_ratio + I(Female_ratio*Female_reviewers) + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)
   # coeftest(mod, vcov=vcovHC(mod, type = "HC0", cluster = "group"))

 # Obtain robust standard errors
 mod1 <- coeftest(mod1, vcov=vcovHC(mod1, type = "HC0", cluster = "Scheme"))
 mod2 <- coeftest(mod2, vcov=vcovHC(mod2, type = "HC0", cluster = "Scheme"))
 mod3 <- coeftest(mod3, vcov=vcovHC(mod3, type = "HC0", cluster = "Scheme"))
 mod4 <- coeftest(mod4, vcov=vcovHC(mod4, type = "HC0", cluster = "Scheme"))
 mod5 <- coeftest(mod5, vcov=vcovHC(mod5, type = "HC0", cluster = "Scheme"))
 mod6 <- coeftest(mod6, vcov=vcovHC(mod6, type = "HC0", cluster = "Scheme"))

 stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
           covariate.labels = c("Inverse Mills Ratio", "Female Ratio Team", "Gender Balanced Team", "Productivity Team", "Star Scientist Team",
                                "Cognitive Proximity Team", "Research Diversity Team", "Team Size", 
                                "Age Team", "Shanghai Ranking Team", "Private Partnership",
                                "Past EUROCORES Grant Team", "Budget Requested", "Number of Countries", 
                                "Female Reviewer", "Productivity Reviewer", "Age Reviewer", "Shanghai Ranking Reviewer"),
           omit = c("ProjectYear", "Scheme", "Domain", "Constant"),
           align=TRUE,
           object.names = TRUE, 
           df = FALSE)
 



##---- PROBIT REVIEWERS SENTIMENTS ----

 mod1 <- lm(ave_sentiment ~ IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)
 mod2 <- lm(syuzhet ~       IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)
 mod3 <- lm(vader ~         IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)
 mod4 <- lm(word2vec ~      IMR1_mod2 + Female_ratio + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team_reviewers + Blau_index_team + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_reviewers + Publication_stock_log_reviewers + Age_log_reviewers + Shanghai_top100_reviewers + Domain + ProjectYear + Scheme, dat = dat_2nd)
 
 # Obtain robust standard errors
 mod1 <- coeftest(mod1, vcov=vcovHC(mod1, type = "HC0", cluster = "Scheme"))
 mod2 <- coeftest(mod2, vcov=vcovHC(mod2, type = "HC0", cluster = "Scheme"))
 mod3 <- coeftest(mod3, vcov=vcovHC(mod3, type = "HC0", cluster = "Scheme"))
 mod4 <- coeftest(mod4, vcov=vcovHC(mod4, type = "HC0", cluster = "Scheme"))

 stargazer(mod1, mod2, mod3, mod4,
           covariate.labels = c("Inverse Mills Ratio", "Female Ratio Team", "Gender Balanced Team", "Productivity Team", "Star Scientist Team",
                                "Cognitive Proximity Team", "Research Diversity Team", "Team Size", 
                                "Age Team", "Shanghai Ranking Team", "Private Partnership",
                                "Past EUROCORES Grant Team", "Budget Requested", "Number of Countries", 
                                "Female Reviewer", "Productivity Reviewer", "Age Reviewer", "Shanghai Ranking Reviewer"),
           omit = c("ProjectYear", "Scheme", "Domain", "Constant"),
           align=TRUE,
           object.names = TRUE, 
           df = FALSE)



##---- PROBIT SECOND PANEL EVALUATION ----

 mod1 <- probitmfx(ESFDecision ~ Female_ratio + Gender_balanced_team + Qavg_reviewers_mean + Qavg_reviewers_var + Publication_stock_log_team + Star_scientist_99_team + Cognitive_dist_team + Blau_index_team + Network_team_panel + Domain + ProjectYear + Scheme, data = dat_3rd, robust = T, clustervar1 = "Scheme")
 mod2 <- probitmfx(ESFDecision ~ Female_ratio + Gender_balanced_team + Qavg_reviewers_mean + Qavg_reviewers_var + Publication_stock_log_team + Star_scientist_99_team + Cognitive_dist_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Domain + ProjectYear + Scheme, data = dat_3rd, robust = T, clustervar1 = "Scheme")
 mod3 <- probitmfx(ESFDecision ~ Female_ratio + Gender_balanced_team + Qavg_reviewers_mean + Qavg_reviewers_var + Publication_stock_log_team + Star_scientist_99_team + Cognitive_dist_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, data = dat_3rd, robust = T, clustervar1 = "Scheme")
 
   # Test for possible interaction effects with gender panel [NO EFFECTS FOUND]
   # probitmfx(ESFDecision ~ Female_ratio + I(Female_ratio*Female_ratio_panel) + Qavg_reviewers_mean + Qavg_reviewers_var + Gender_balanced_team + Publication_stock_log_team + Star_scientist_99_team + Cognitive_prox_team + Blau_index_team + Network_team_panel + Team_size_log + Age_log_team + Shanghai_top100_team + Company_team + Already_success_past_team + BudgetR_per_indiv_log + Number_countries_log + Female_ratio_panel + Publication_stock_log_panel + Panel_size_log + Age_log_panel + Project_panel_ratio_log + Shanghai_top100_ratio_panel + Domain + ProjectYear, data = dat_3rd, robust = T, clustervar1 = "Scheme")
      
 mods  = list(mod1$fit, mod2$fit, mod3$fit)
 coefs = list(c(0, mod1$mfxest[, 1]), c(0, mod2$mfxest[, 1]), c(0, mod3$mfxest[, 1]))
 ses   = list(c(0, mod1$mfxest[, 2]), c(0, mod2$mfxest[, 2]), c(0, mod3$mfxest[, 2]))
 pvals = list(c(0, mod1$mfxest[, 4]), c(0, mod2$mfxest[, 4]), c(0, mod3$mfxest[, 4]))
 tr1   = texreg(mods,
                override.coef = coefs,
                override.se = ses,
                override.pval = pvals,
                stars = c(0.01,  0.05, 0.10),
                omit.coef = "(Intercept)|(ProjectYear)|(Scheme)|(Domain)",
                custom.coef.names=c("Female Ratio Team", "Gender Balanced Team", "Reviewers Scores Mean", "Reviewers Scores Variance", 
                                    "Productivity Team", "Star Scientist Team",
                                    "Cognitive Proximity Team", "Research Diversity Team", "Network Proximity Team", "Team Size", 
                                    "Age Team", "Shanghai Ranking Team", "Private Partnership", 
                                    "Past EUROCORES Grant Team", "Budget Requested", "Number of Countries", 
                                    "Female Ratio Panel", "Productivity Panel", "Panel Size", "Age Panel", 
                                    "Panel Workload", "Shanghai Ranking Panel"),
                caption.above = TRUE,
                caption = "Results",
                dcolumn = TRUE,
                custom.note = "ciao ciao",
                custom.model.names = c("(1)","(2)","(3)"),
                return.string = TRUE,
                digits = 3) 
 
 
