
####################################################################
# Script id: 9
# Description: This script creates the variables for panel experts
# Author: GIGA team
# Last update: 24.04.2021
####################################################################



##-- Libraries

 rm(list = ls())

 library(dplyr)
 library(data.table)
 library(stringr)
 library(readr)
 library(tidyr)
 library(readxl)



#-- Upload and prepare the list of panel members 

 Panel_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_ReviewPanel")


##-- Clean the list of panel members 
 
 # Harmonize variable format
 Panel_Kernel$Surname <- toupper(Panel_Kernel$Surname)
 Panel_Kernel$Surname <- iconv(Panel_Kernel$Surname, "UTF-8", "ASCII//TRANSLIT")

 Panel_Kernel$Name <- toupper(Panel_Kernel$Name)
 Panel_Kernel$Name <- iconv(Panel_Kernel$Name, "UTF-8", "ASCII//TRANSLIT")
 
 Panel_Kernel$Scheme <- toupper(Panel_Kernel$Scheme)

 # Create the variable AU (Surname + First letter) - used as key for merging
 Panel_Kernel$Name_first_let <- substr(Panel_Kernel$Name, 1, 1)
 Panel_Kernel$AU <- paste(Panel_Kernel$Surname, Panel_Kernel$Name_first_let, sep = " ")  

 Panel_Kernel$AU <- as.character(Panel_Kernel$AU)
 Panel_Kernel$ProjectYear <- as.character(Panel_Kernel$ProjectYear)

 # Selected list of variables
 xxx <- Panel_Kernel %>% distinct(AU, Scheme, ProjectYear, .keep_all = TRUE) %>% 
   dplyr::select(AU, DateofBirth, Sex, Scheme, ProjectYear, University)


  
##-- Merge with WoS dataset 
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/Publication_activity_panel_members.RData")
 
 yyy <- inner_join(xxx, publication_activity_panel_members, by = c("AU", "ProjectYear")) %>% 
   distinct(AU, Scheme, ProjectYear, .keep_all = TRUE)
 

 
##-- Merge with Shanghai ranking 2005 
 
 yyy$University <- toupper(yyy$University)
 
 yyy$Shanghai_top100_panel <- as.integer(str_detect(yyy$University, paste(c("HARVARD", "CAMBRIDGE", "STANFORD", "BERKELEY", "MASSACHUSETTS", "CALIFORNIA INSTITUTE", "COLUMBIA",
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
 yyy <- yyy %>% 
   mutate(Shanghai_top100_panel = replace(Shanghai_top100_panel, is.na(Shanghai_top100_panel), 0))  
 
 
 
##-- Create a set of panel member-level variables  
 
 yyy$DateofBirth <- as.numeric(yyy$DateofBirth)
 yyy$ProjectYear <- as.numeric(yyy$ProjectYear)
 
 yyy <- yyy %>% filter(DateofBirth > 1900 & DateofBirth < 2000) %>%
   mutate(Age = ProjectYear - DateofBirth,
          Female = ifelse(Sex == "F", 1, 0),
          Age_log = log(Age),
          Publication_stock_log = log(Publication_stock + 1),
          Citation_stock_log = log(Citation_stock + 1),
          Citat_per_paper_log = log(Citat_per_paper + 1))
 
 

##-- Aggregate the above variables at panel group-level variables  
 # Note: detailed explanations of variables can be found in SI  
 
 yyy <- yyy %>%
 group_by(Scheme, ProjectYear) %>%
   mutate(Panel_size = n(),
          Female_ratio_panel = sum(Female, na.rm=TRUE) / Panel_size,
          Panel_size_log = log(Panel_size),
          Age_panel = mean(Age, na.rm=TRUE),
          Age_log_panel = mean(Age_log, na.rm=TRUE),
          Publication_stock_panel = mean(Publication_stock, na.rm=TRUE),
          Citation_stock_panel = mean(Citation_stock, na.rm=TRUE),
          Citat_per_paper_panel = mean(Citat_per_paper, na.rm=TRUE),
          Publication_stock_log_panel = mean(Publication_stock_log, na.rm=TRUE),
          Citation_stock_log_panel = mean(Citation_stock_log, na.rm=TRUE),
          Citat_per_paper_log_panel = mean(Citat_per_paper_log, na.rm=TRUE),
          Shanghai_top100_ratio_panel = ifelse(sum(Shanghai_top100_panel, na.rm=TRUE) / Panel_size > 0, 1, 0)) %>%
   ungroup()
 
 # Selected list of variables
 www <- yyy %>% dplyr::select(Scheme, ProjectYear, Panel_size, Female_ratio_panel, Panel_size_log, Age_panel, Age_log_panel,
                              Publication_stock_panel, Publication_stock_log_panel, 
                              Citation_stock_panel, Citation_stock_log_panel,
                              Citat_per_paper_panel, Citat_per_paper_log_panel,
                              Shanghai_top100_ratio_panel) %>%
   distinct(Scheme, ProjectYear, .keep_all = TRUE)
 
 
 
 
##-- Remove outliers
 
 www <- www %>% 
   filter(Publication_stock_panel < 200) %>%    # Arbitrary cut-off on productivity
   filter(Panel_size > 3)                       # keep a panel only if its size > 3

 
 
##-- Assign scientific domain to each scheme
 
 www <- www %>% 
   mutate(Domain = ifelse(Scheme == "EUROCOOLS" | Scheme == "EUROEPINOMICS" | Scheme == "EUROGEAR" | Scheme == "EUROSCOPE" | 
                            Scheme == "EUROSTELLS" | Scheme == "EUROSTRESS" | Scheme == "EUROCLIMATE" | Scheme == "EURODEEP" | 
                            Scheme == "EURODIVERSITY" | Scheme == "EURODYNA" | Scheme == "EUROEEFG" | Scheme == "EUROMARC" | 
                            Scheme == "EUROMARGINS" | Scheme == "EUROMEMBRANE" | Scheme == "EUROMINSCI" | Scheme == "EUROVOL" | 
                            Scheme == "RNAQUALITY" | Scheme == "TOPO", "LEEMED",
                          
                          ifelse(Scheme == "EUROBIOSAS" | Scheme == "EUROGENESIS" | Scheme == "EUROGIGA"| Scheme == "EUROGRAPHENE" | 
                                   Scheme == "EUROSOLARFUELS" | Scheme == "EUROSYNBIO" | Scheme == "FANAS"| Scheme == "FONE" | 
                                   Scheme == "S3T" | Scheme == "SONS", "PEN", "SSH"))) 
 


#-- Upload and prepare the list of projects 
 
 Project_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_ProjectLvl")

 # Harmonize variable format
 Project_Kernel$ProjectNumber_OP <- toupper(Project_Kernel$ProjectNumber_OP)
 Project_Kernel$Scheme <- toupper(Project_Kernel$Scheme)
 
 # Selected list of variables
 Project_Kernel <- Project_Kernel %>%
   dplyr::select(ProjectNumber_OP, Scheme, ProjectYear) %>%
   distinct(ProjectNumber_OP, ProjectYear, .keep_all = TRUE)
 

 
#-- Merge panel expert information to each project   

 Project_Kernel <- left_join(Project_Kernel, www, by = c("Scheme", "ProjectYear")) %>% 
   distinct(ProjectNumber_OP, ProjectYear, .keep_all = TRUE)
 
 # Create a panel workload variable
 Project_Kernel <- Project_Kernel %>%
   group_by(Scheme, ProjectYear) %>%
    mutate(Project_panel_ratio = n() / Panel_size,
           Project_panel_ratio_log = log(Project_panel_ratio)) %>%
   ungroup()
 
 
 
##-- Clean the working environment 
 
 rm(Panel_Kernel, publication_activity_panel_members, xxx, yyy, www, stat)
 

 
##-- Save the final dataset
 
 save.image("C:/Users/Stefano/Desktop/GIGA_Project/Project_panel_kernel.RData")
 
 
 