
################################################################
# Script id: 8
# Description: This script creates the variables for applicants
# Author: GIGA team
# Last update: 24.04.2021
################################################################



##-- Libraries

 rm(list = ls())
 
 library(dplyr)
 library(data.table)
 library(stringr)
 library(readr)
 library(tidyr)
 library(readxl)



#-- Upload and prepare the list of applicants

 Individ_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_IndvLvl")

 
 
##-- Clean the list of applicants 
 
 # Harmonize variable format
 Individ_Kernel$Surname <- toupper(Individ_Kernel$Surname)
 Individ_Kernel$Surname <- iconv(Individ_Kernel$Surname, "UTF-8", "ASCII//TRANSLIT")
 
 Individ_Kernel$Name <- toupper(Individ_Kernel$Name)
 Individ_Kernel$Name <- iconv(Individ_Kernel$Name, "UTF-8", "ASCII//TRANSLIT")

 Individ_Kernel$ProjectNumber_OP <- toupper(Individ_Kernel$ProjectNumber_OP)
 Individ_Kernel$ProjectNumber_FP <- toupper(Individ_Kernel$ProjectNumber_FP)
 
 Individ_Kernel$DateofBirth <- as.numeric(Individ_Kernel$DateofBirth)
 Individ_Kernel$University <- toupper(Individ_Kernel$University)
 Individ_Kernel$Scheme <- toupper(Individ_Kernel$Scheme)
 
 # Create the variable AU (Surname + First letter) - used as key for merging
 Individ_Kernel$Name_first_let <- substr(Individ_Kernel$Name, 1, 1)
 Individ_Kernel$AU <- paste(Individ_Kernel$Surname, Individ_Kernel$Name_first_let, sep = " ")  
 Individ_Kernel$AU <- as.character(Individ_Kernel$AU)

 # Selected list of variables
 xxx <- Individ_Kernel %>% distinct(AU, ProjectYear, ProjectNumber_OP, .keep_all = TRUE) %>% 
   dplyr::select(AU, DateofBirth, Sex, ProjectNumber_OP, ProjectNumber_FP, Scheme, ProjectYear, Country,
                 University, Organisation, Company, CType, PESC, LESC, EMRC, SCSS, SCH) 
   
 
 
##-- Merge with WoS dataset 
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/Publication_activity_applicants.RData")
 
 yyy <- inner_join(xxx, publication_activity_applicants, by = c("AU", "ProjectYear")) %>% 
   distinct(AU, ProjectYear, ProjectNumber_OP, .keep_all = TRUE)
 
 
 
##-- Merge with Shanghai ranking 2005
 
 yyy$Shanghai_top100 <- as.integer(str_detect(yyy$University, paste(c("HARVARD", "CAMBRIDGE", "STANFORD", "BERKELEY", "MASSACHUSETTS", "CALIFORNIA INSTITUTE", "COLUMBIA",
                                                                      "PRINCETON", "CHICAGO", "OXFORD", "YALE", "CORNELL", "SAN DIEGO", "LOS ANGELES", "PENNSYLVANIA", "MADISON",
                                                                      "WASHINGTON", "SAN FRANCISCO", "HOPKINS", "TOKYO", "ARBOR", "KYOTO", "IMPERIAL COLLEGE", "TORONTO",
                                                                      "ILLINOIS", "COLLEGE LONDON", "SWISS FEDERAL INSTITUTE OF TECHNOLOGY ZURICH", "WASHINGTON", "NEW YORK",
                                                                      "ROCKEFELLER", "NORTHWESTERN", "DUKE", "MINNESOTA", "SANTA BARBARA", "COLORADO", "AUSTIN", "BRITISH COLUMBIA",
                                                                      "SOUTHWESTERN", "PENNSYLVANIA", "VANDERBILT", "DAVIS", "UTRECHT", "RUTGERS", "PITTSBURGH", "KAROLINSKA",
                                                                      "CURIE", "EDINBURGH", "IRVINE", "MARYLAND", "SOUTHERN CALIFORNIA", "MUNICH", "MANCHESTER", "CARNEGIE",
                                                                      "CHAPEL", "AUSTRALIAN", "COPENHAGEN", "FLORIDA", "UNIVERSITY OF ZURICH", "UPPSALA", "PARIS SUD", "OSAKA",
                                                                      "OHIO", "BRISTOL", "SHEFFIELD", "ROCHESTER", "MCGILL", "MOSCOW", "CASE WESTERN", "OSLO", "HEIDELBERG",
                                                                      "LEIDEN", "TOHOKU", "ARIZONA", "PURDUE", "HELSINKI", "MICHIGAN STATE", "RICE", "HEBREW", "BOSTON",
                                                                      "KING'S", "MELBOURNE", "NOTTINGHAM", "GOETTINGEN", "VIENNA", "BROWN", "INDIANA", "BASEL", "A&M", "MCMASTER",
                                                                      "FREIBURG", "STRASBOURG", "ECOLE NORMALE SUPERIEURE", "STOCKHOLM", "TOKYO INSTITUTE", "UTAH", "LA SAPIENZA",
                                                                      "BIRMINGHAM", "LUND", "TUFTS"), collapse = '|')))
 # Convert NAs to 0
 yyy <- yyy %>% mutate(Shanghai_top100 = replace(Shanghai_top100, is.na(Shanghai_top100), 0))
 
 
 
#-- Merge with research diversification (Blau index)

 load("C:/Users/Stefano/Desktop/GIGA_Project/Blau_index_team.RData")
 yyy <- left_join(yyy, blau_index_team, by = c("ProjectNumber_OP")) 

 # Convert NAs to 0
 yyy <- yyy %>% mutate(Blau_index_team = replace(Blau_index_team, is.na(Blau_index_team), 0))
 

 
#-- Merge with cognitive proximity (WoS overalapping categories)
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/Cognitive_prox_team_panel_members.RData")
 yyy <- left_join(yyy, Cognitive_prox_team_panel_members, by = c("ProjectNumber_OP"))
 
 # Convert NAs to 0
 yyy <- yyy %>% mutate(Cognitive_prox_team = replace(Cognitive_prox_team, is.na(Cognitive_prox_team), 0),
                       Nr_overlapping_cat_team = replace(Nr_overlapping_cat_team, is.na(Nr_overlapping_cat_team), 0))
 
 
 
#-- Merge with social proximity (common co-authorship)
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/Social_prox_team_panel_members.RData")
 
 yyy <- left_join(yyy, Social_prox_team_panel_members, by = c("ProjectNumber_OP"))
 
 # Convert NAs to 0
 yyy <- yyy %>% mutate(Coauthors_team_panel = replace(Coauthors_team_panel, is.na(Coauthors_team_panel), 0))
 
 
 
##-- Create a set of applicant-level variables 
  
 yyy <- yyy %>% filter(DateofBirth > 1900 & DateofBirth < 2000) %>% 
   mutate(Age = ProjectYear - DateofBirth,
          Female = ifelse(Sex == "F", 1, 0),
          Grant = ifelse(is.na(ProjectNumber_FP), 0, 1),
          Age = ifelse(Age < 30, 30, Age),
          Age_log = log(Age),
          Publication_stock_log = log(Publication_stock + 1),
          Citation_stock_log = log(Citation_stock + 1),
          Citat_per_paper_log = log(Citat_per_paper + 1))


 
##-- Create a variable to capture past experience and success in Eurocores applications 
 
 past_exp <- yyy %>%
   group_by(AU, DateofBirth, ProjectYear) %>%
   mutate(Already_applied = n(),                                 # Count number of previous applications
          Already_success = sum(!is.na(ProjectNumber_FP))) %>%   # Count number of previous granted projects
   ungroup() %>%
   
   dplyr::select(AU, DateofBirth, ProjectNumber_OP, ProjectNumber_FP, ProjectYear, Already_applied, Already_success) %>%
   distinct(AU, DateofBirth, ProjectYear, .keep_all = TRUE) %>%
   
   group_by(AU, DateofBirth) %>%
   mutate(Already_applied_cum = cumsum(Already_applied),                                        # Cumulative number of previous applications  
          Already_success_cum = cumsum(Already_success),                                        # Cumulative number of previous granted projects
          Already_applied_past = ifelse(Already_applied_cum - Already_applied > 0, 1, 0),       # Binary indicator for past applications
          Already_success_past = ifelse(Already_success_cum - Already_success > 0, 1, 0),       # Binary indicator for past granted projects
          n_obs = n()) %>%

   dplyr::select(AU, DateofBirth, ProjectYear, Already_applied_past, Already_success_past) %>%  # Selected list of variables
   distinct(AU, DateofBirth, ProjectYear, .keep_all = TRUE) %>%
   ungroup()  

 # Merge with the kernel applicants
 yyy <- left_join(yyy, past_exp, by = c("AU", "DateofBirth", "ProjectYear")) 
 
 
 
##-- Assign scientific domain to each scheme
 
 #--- LEEMED = LEE + MED
 #EUROCLIMATE
 #EURODEEP
 #EURODIVERSITY
 #EURODYNA
 #EUROEEFG
 #EUROMARC
 #EUROMARGINS
 #EUROMEMBRANE
 #EUROMINSCI
 #EUROVOL
 #RNAQUALITY
 #TOPO
 
 #EUROCOOLS
 #EUROEPINOMICS
 #EUROGEAR
 #EUROSCOPE
 #EUROSTELLS
 #EUROSTRESS
 
 #--- PEN
 #EUROBIOSAS
 #EUROGENESIS
 #EUROGIGA
 #EUROGRAPHENE
 #EUROSOLARFUELS
 #EUROSYNBIO
 #FANAS
 #FONE
 #S3T
 #SONS
 
 #--- SSH = HUM + SOC
 #BOREAS
 #CNCC
 #EUROBABEL
 #EUROCORECODE
 #EUROHESC
 #EUROUNDERSTANDING
 #HUMVIB
 #INVENTING EUROPE
 #LOGICCC
 #TECT 
 
 yyy <- yyy %>% 
   mutate(Domain = ifelse(Scheme == "EUROCOOLS" | Scheme == "EUROEPINOMICS" | Scheme == "EUROGEAR" | Scheme == "EUROSCOPE" | 
                            Scheme == "EUROSTELLS" | Scheme == "EUROSTRESS" | Scheme == "EUROCLIMATE" | Scheme == "EURODEEP" | 
                            Scheme == "EURODIVERSITY" | Scheme == "EURODYNA" | Scheme == "EUROEEFG" | Scheme == "EUROMARC" | 
                            Scheme == "EUROMARGINS" | Scheme == "EUROMEMBRANE" | Scheme == "EUROMINSCI" | Scheme == "EUROVOL" | 
                            Scheme == "RNAQUALITY" | Scheme == "TOPO", "LEEMED",
                          
                          ifelse(Scheme == "EUROBIOSAS" | Scheme == "EUROGENESIS" | Scheme == "EUROGIGA"| Scheme == "EUROGRAPHENE" | 
                                   Scheme == "EUROSOLARFUELS" | Scheme == "EUROSYNBIO" | Scheme == "FANAS"| Scheme == "FONE" | 
                                   Scheme == "S3T" | Scheme == "SONS", "PEN", "SSH")))
 

 
##-- Create a variable to capture the presence of star scientists
 
 yyy <- yyy %>% 
   group_by(ProjectYear, Domain) %>%
   mutate(Citation_95perc = quantile(Citation_stock, 0.95),                          # 95th percentile of the year-discipline citation distribution
          Citation_99perc = quantile(Citation_stock, 0.99),                          # 99th percentile of the year-discipline citation distribution
          Star_scientist_95 = ifelse(Citation_stock > Citation_95perc, 1, 0),        # Binary indicator to identify a star scientist 
          Star_scientist_99 = ifelse(Citation_stock > Citation_99perc, 1, 0)) %>%    # Idem
   ungroup() 
 
 

##-- Create project-level variables
 
# Note: detailed explanations of variables can be found in SI  
 
 team <- yyy %>%
   group_by(ProjectNumber_OP) %>%
    mutate(Team_size = n(),
           Female_pl = ifelse(CType == "Project Leader" & Female == 1, 1, 0),
           Female_ratio = sum(Female) / Team_size,
           Team_size_log = log(Team_size),
           Number_countries = n_distinct(Country),
           Number_countries_log = log(Number_countries),
           Non_EU_partner = ifelse((Country == "AT" | Country == "AT" | Country == "AT" | Country == "BE" | Country == "BG" | Country == "CY" | Country == "CZ" | Country == "DE" | 
                                    Country == "DK" | Country == "EE" | Country == "ES" | Country == "FI" | Country == "FR" | Country == "GR" | Country == "HR" | Country == "HU" | 
                                    Country == "IE" | Country == "IT" | Country == "LT" | Country == "LU" | Country == "LV" | Country == "MT" | Country == "NL" | Country == "PO" | 
                                    Country == "PT" | Country == "RO" | Country == "SE" | Country == "SI" | Country == "SK" | Country == "UK" | Country == "NO" | Country == "CH" | Country == "PL"), 0, 1),
           Publication_stock_team = mean(Publication_stock),
           Citation_stock_team = mean(Citation_stock),
           Citat_per_paper_team = mean(Citat_per_paper),
           Publication_stock_log_team = mean(Publication_stock_log),
           Citation_stock_log_team = mean(Citation_stock_log),
           Citat_per_paper_log_team = mean(Citat_per_paper_log),
           Age_team = mean(Age),
           Age_log_team = mean(Age_log),
           Shanghai_top100_team = ifelse(sum(Shanghai_top100) / Team_size > 0, 1, 0),
           Company_team = ifelse(sum(Company) > 0, 1, 0),
           Network_team_panel = ifelse(Coauthors_team_panel > 0, 1, 0),
           Company_share_team = ifelse(sum(Company)>0, sum(Company) / Team_size, 0),
           Star_scientist_95_team = ifelse(sum(Star_scientist_95)>0, 1, 0),
           Star_scientist_99_team = ifelse(sum(Star_scientist_99)>0, 1, 0),
           Already_applied_past_team = ifelse(sum(Already_applied_past) / Team_size > 0, 1, 0),
           Already_success_past_team = ifelse(sum(Already_success_past) / Team_size > 0, 1, 0)) %>%
   ungroup() %>%
   
   # Selected list of variables
   dplyr::select(ProjectNumber_OP, Team_size, Female_ratio, Female_pl, Team_size_log, Number_countries, Number_countries_log, Non_EU_partner, Publication_stock_team, Publication_stock_log_team,
                 Citation_stock_team, Citation_stock_log_team, Citat_per_paper_team, Citat_per_paper_log_team, Age_team, Age_log_team, Shanghai_top100_team, 
                 Company_team, Network_team_panel, Company_share_team, Star_scientist_95_team, Star_scientist_99_team, Already_applied_past_team, Already_success_past_team) %>%
   distinct(ProjectNumber_OP, .keep_all = TRUE)
   
 
 # Merge with the kernel applicants
 yyy <- left_join(yyy, team, by = c("ProjectNumber_OP"))
 
 # Omit observations with missing values
 yyy <- yyy %>% mutate(ProjectNumber_FP = replace(ProjectNumber_FP, is.na(ProjectNumber_FP), 0)) %>% # Set 0 to ProjectNumber_FP to run na.omit()
                dplyr::select(ProjectNumber_OP, ProjectNumber_FP, Domain, ProjectYear, Grant, 
                               Female_ratio, Female_pl, Team_size, Team_size_log, Age_team, Age_log_team, 
                               Publication_stock_team, Citation_stock_team, Citat_per_paper_team, 
                               Publication_stock_log_team, Citation_stock_log_team, Citat_per_paper_log_team, 
                               Cognitive_dist_team, Blau_index_team, Number_countries, Number_countries_log, Non_EU_partner,
                               Shanghai_top100_team, Company_team, Company_share_team, Network_team_panel,
                               Already_applied_past_team, Already_success_past_team, Star_scientist_95_team, Star_scientist_99_team) %>%
   na.omit() 
  
 
  
  
##-- Merge with info about budget requsted
  
 # Upload kernel with budget info
 Budget <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_IndvLvl") %>% 
    dplyr::select(ProjectNumber_OP, BudgetR) %>%
    distinct(ProjectNumber_OP, .keep_all = TRUE)

 # Fix problematic entries 1/2
 Budget$ProjectNumber_OP <- toupper(Budget$ProjectNumber_OP)
 Budget$BudgetR <- str_replace_all(Budget$BudgetR, fixed(" "), "")
 Budget$BudgetR <- str_replace_all(Budget$BudgetR, fixed("EUR"), "")
 Budget$BudgetR <- str_replace_all(Budget$BudgetR, fixed(","), "")
 Budget$BudgetR <- str_replace_all(Budget$BudgetR, fixed("K"), "")
 Budget$BudgetR <- as.numeric(Budget$BudgetR)
  
 # Calculate team size excluding associated partners (not receiving money)
 ppp <- xxx %>%
   filter(CType == "Principal Investigator" | CType == "Principal Leader" ) %>% 
   group_by(ProjectNumber_OP) %>%
      mutate(Size_pi_pl = n()) %>%
   ungroup() %>%
   dplyr::select(ProjectNumber_OP, Size_pi_pl) %>%
   distinct(ProjectNumber_OP, .keep_all = TRUE)
 
 # Attach team size to kernel applicants
 zzz <- left_join(yyy, ppp, by = c("ProjectNumber_OP")) %>%
   na.omit()
 
 # Attach budget info to kernal applicants
 zzz <- left_join(zzz, Budget, by = c("ProjectNumber_OP")) %>%
   na.omit()
 
 # Fix problematic entries 2/2
 zzz <- zzz %>%
    mutate(BudgetR = ifelse(BudgetR < 100000 & BudgetR > 10000, BudgetR*10,          
                            ifelse(BudgetR <= 10000 & BudgetR > 1000, BudgetR*100,
                                   ifelse(BudgetR <= 1000, BudgetR*1000, BudgetR))), # Fix budget on the value (x10 or x100 or x1000)
           BudgetR_per_indiv = BudgetR / Size_pi_pl,                                 # Budget requested normalized by team size
           BudgetR_log = log(BudgetR),
           BudgetR_per_indiv_log = log(BudgetR_per_indiv))
  
  
##-- Remove outliers
  
  zzz <- zzz %>% 
    filter(Publication_stock_team < 200)   # Arbitrary cut-off on productivity
 
   
  
##-- Collapse applicant kernel at the project-level 
  
 Project_applicants_kernel <- zzz %>%
   distinct(ProjectNumber_OP, .keep_all = TRUE) %>%
   filter(Team_size >= 3) %>%      # keep a project only if team size > 3 
   filter(Number_countries >=2)    # keep a project only if the number of participating countries > 2
 

 
##-- Clean the working environment  
 
 rm(xxx, yyy, zzz, Individ_Kernel, publication_activity_applicants, Budget, 
    blau_index_team, Cognitive_dist_team_panel_members, Social_dist_team_panel_members, 
    past_exp, team, ppp)

 
 
##-- Save the final dataset
 
 save.image("C:/Users/Stefano/Desktop/GIGA_Project/Project_applicants_kernel.RData")

 