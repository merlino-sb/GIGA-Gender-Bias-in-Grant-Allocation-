
##############################################################################################################
# Script id: 7
# Description: This script creates measures of network proximity between team applicants and expert panels 
# Author: GIGA team
# Last update: 24.04.2021
##############################################################################################################



##-- Libraries

rm(list = ls())

library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyr)
library(readxl)
library(fastDummies)



#-- Upload and prepare the list of applicants for matching with WoS data 

 Individ_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_IndvLvl")
 
 # Harmonize variable format
 Individ_Kernel$Surname <- toupper(Individ_Kernel$Surname)
 Individ_Kernel$Surname <- iconv(Individ_Kernel$Surname, "UTF-8", "ASCII//TRANSLIT")
 
 Individ_Kernel$Name <- toupper(Individ_Kernel$Name)
 Individ_Kernel$Name <- iconv(Individ_Kernel$Name, "UTF-8", "ASCII//TRANSLIT")
 
 Individ_Kernel$ProjectNumber_OP <- toupper(Individ_Kernel$ProjectNumber_OP)
 Individ_Kernel$Scheme <- toupper(Individ_Kernel$Scheme)

 # Create the variable AU (Surname + First letter) - used as key for merging with WoS
 Individ_Kernel$Name_first_let <- substr(Individ_Kernel$Name, 1, 1)
 Individ_Kernel$AU <- paste(Individ_Kernel$Surname, Individ_Kernel$Name_first_let, sep = " ")  
 
 Individ_Kernel$AU <- as.character(Individ_Kernel$AU)
 Individ_Kernel$ProjectYear   <- as.character(Individ_Kernel$ProjectYear)
 
 # Selected list of variables
 Individ_Kernel <- Individ_Kernel %>% distinct(AU, ProjectYear, .keep_all = TRUE) %>% 
   dplyr::select(AU, ProjectNumber_OP, ProjectNumber_FP, Scheme, ProjectYear)

 

#-- Prepare the WoS data for matching with applicants kernel
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/WoS_data_applicant_original.RData")
 
 # Separate rows by authors (and save in a dataframe called 'xxx')
 xxx <- WoS_Data %>%
   separate_rows(AU, sep = "; ")
 
 # Harmonize variable format
 xxx$AU <- toupper(xxx$AU)

 # Create the variable AU (Surname + First letter) with regular expression - used as key for merging 
 xxx$yyy <- str_replace(xxx$AU, ",", "")
 xxx$zzz <- sapply(strsplit(xxx$yyy, "\\s+"), "[", 2)
 xxx$Name_first_let <- substr(xxx$zzz, 1, 1)

 xxx$AU <- gsub("(.*),.*", "\\1", xxx$AU)  
 xxx$AU <- paste(xxx$AU, xxx$Name_first_let, sep = " ")


##-- Merge WoS and applicants dataframes 

 # Attach the variable 'ProjectYear' to WoS data 
 project_year <- Individ_Kernel %>%
   dplyr::select(AU, ProjectNumber_OP, Scheme, ProjectYear)
 
 xxx <- left_join(xxx, project_year, by = "AU")
 
 # Keep only publications associated with 1 unique project
 xxx <- xxx %>% 
   dplyr::select(UT, AU, AF, TI, PY, ProjectNumber_OP, Scheme, ProjectYear) %>%
   group_by(UT) %>% 
   mutate(Applicant = ifelse(!is.na(ProjectNumber_OP) == 1, 1, 0),
          More_project = sum(!is.na(ProjectNumber_OP))) %>% 
   filter(More_project == 1) %>%
   mutate(ProjectNumber_OP = max(ProjectNumber_OP, na.rm = T),
          Scheme = max(Scheme, na.rm = T),
          ProjectYear = max(ProjectYear, na.rm = T)) %>%   
   ungroup() %>%
   na.omit()
   
 # Remove publications before project application year
 xxx <- xxx %>% filter(PY <= ProjectYear)

 # Count the number of co-authors
 xxx <- xxx %>%
   distinct(ProjectNumber_OP, AU, .keep_all = TRUE) %>%
   dplyr::select(AU, ProjectNumber_OP, Scheme, ProjectYear, Applicant) %>%
   group_by(ProjectNumber_OP) %>%
    mutate(Nr_coaut = n()) %>%
   ungroup()
 
 
  
#-- Upload and prepare the list of expert panels for matching with WoS data 
 
 Panel_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_ReviewPanel")
 
 # Harmonize variable format
 Panel_Kernel$Surname <- toupper(Panel_Kernel$Surname)
 Panel_Kernel$Surname <- iconv(Panel_Kernel$Surname, "UTF-8", "ASCII//TRANSLIT")
 
 Panel_Kernel$Name <- toupper(Panel_Kernel$Name)
 Panel_Kernel$Name <- iconv(Panel_Kernel$Name, "UTF-8", "ASCII//TRANSLIT")
 
 # Create the variable AU (Surname + First letter) - used as key for merging with WoS
 Panel_Kernel$Name_first_let <- substr(Panel_Kernel$Name, 1, 1)
 Panel_Kernel$AU <- paste(Panel_Kernel$Surname, Panel_Kernel$Name_first_let, sep = " ")  
 
 Panel_Kernel$AU <- as.character(Panel_Kernel$AU)
 Panel_Kernel$Scheme <- toupper(Panel_Kernel$Scheme)
 
 
#-- Prepare the WoS data for matching with expert panels kernel
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/WoS_data_panel_members_original.RData")
 
 # Separate rows by authors (and save in a dataframe called 'xxx')
 yyy <- WoS_Data %>%
   separate_rows(AU, sep = "; ")
 
 # Harmonize variable format
 yyy$AU <- toupper(yyy$AU)
 
 # Create the variable AU (Surname + First letter) with regular expression - used as key for merging 
 yyy$yyy <- str_replace(yyy$AU, ",", "")
 yyy$zzz <- sapply(strsplit(yyy$yyy, "\\s+"), "[", 2)
 yyy$Name_first_let <- substr(yyy$zzz, 1, 1)
 
 yyy$AU <- gsub("(.*),.*", "\\1", yyy$AU)  
 yyy$AU <- paste(yyy$AU, yyy$Name_first_let, sep = " ")
 
 
 
##-- Merge WoS and panel expert dataframes 
 
 # Attach the variable 'ProjectYear' to WoS data 
 project_year_panel <- Panel_Kernel %>%
   dplyr::select(AU, Scheme, ProjectYear)
 
 yyy <- left_join(yyy, project_year_panel, by = "AU")
 
 # Keep only publications associated with 1 unique project
 yyy <- yyy %>% 
   dplyr::select(UT, AU, AF, TI, PY, Scheme, ProjectYear) %>%
   group_by(UT) %>% 
   mutate(Panel = ifelse(!is.na(Scheme) == 1, 1, 0),
          Scheme = max(Scheme, na.rm = T),
          ProjectYear = max(ProjectYear, na.rm = T)) %>%   
   ungroup() %>%
   distinct(UT, AU, .keep_all = TRUE) %>% 
   na.omit()
 
 # Remove publications before project application year
 yyy <- yyy %>% filter(PY <= ProjectYear)
 
 # Dobule check: WOS:000347500600014
 
 # Count the number of co-authors
 yyy <- yyy %>%
   distinct(Scheme, ProjectYear, AU, .keep_all = TRUE) %>%
   dplyr::select(AU, Scheme, ProjectYear, Panel) %>%
   group_by(Scheme, ProjectYear) %>%
    mutate(Nr_coaut_panel = n()) %>%
   ungroup() 
 
 
 
#-- Create a measure of network proximity between team members and panel experts
 # Note: The variable measures the number of common co-authors 
 
 yyy$ProjectYear <- as.character(yyy$ProjectYear)
 www <- left_join(xxx, yyy, by = c("Scheme", "ProjectYear", "AU")) %>%
   na.omit()
 
 Social_prox_team_panel_members <- www %>%
   group_by(ProjectNumber_OP) %>%
    mutate(Coauthors_team_panel = n()) %>%
   ungroup() %>%
   distinct(ProjectNumber_OP, .keep_all = TRUE) %>%
   dplyr::select(-AU, -Scheme, -ProjectYear, -Applicant, -Panel)
 

  
##-- Clean the working environment 
 rm(Individ_Kernel, Panel_Kernel, WoS_Data, xxx, yyy, www, project_year, project_year_panel)
 
 
 
##-- Save the final dataset  
 save.image("C:/Users/Stefano/Desktop/GIGA_Project/Social_prox_team_panel_members.RData")
 
 