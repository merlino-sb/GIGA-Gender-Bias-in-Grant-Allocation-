
########################################################################
# Script id: 10
# Description: This script creates the variables for external reviewers
# Author: GIGA team
# Last update: 24.04.2021
########################################################################



##-- Libraries

rm(list = ls())

library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyr)
library(readxl)



#-- Upload and prepare the list of external reviewers

 Ext_Reviewers_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_ExternalReviewer")

 # Keep only those reviewers who sent the report
 Ext_Reviewers_Kernel$ReviewStatus <- toupper(Ext_Reviewers_Kernel$ReviewStatus)
 
 Ext_Reviewers_Kernel <- Ext_Reviewers_Kernel %>%
   filter(ReviewStatus == "RECEIVED" | ReviewStatus == "PROMISED" | ReviewStatus == "PENDING") 
 

##-- Clean the list of external reviewers

 # Harmonize variable format
 Ext_Reviewers_Kernel$Surname <- toupper(Ext_Reviewers_Kernel$Surname)
 Ext_Reviewers_Kernel$Surname <- iconv(Ext_Reviewers_Kernel$Surname, "UTF-8", "ASCII//TRANSLIT")

 Ext_Reviewers_Kernel$Name <- toupper(Ext_Reviewers_Kernel$Name)
 Ext_Reviewers_Kernel$Name <- iconv(Ext_Reviewers_Kernel$Name, "UTF-8", "ASCII//TRANSLIT")

 Ext_Reviewers_Kernel$Scheme <- toupper(Ext_Reviewers_Kernel$Scheme)
 Ext_Reviewers_Kernel$ProjectYear <- as.character(Ext_Reviewers_Kernel$ProjectYear)
 
 # Create the variable AU (Surname + First letter) - used as key for merging
 Ext_Reviewers_Kernel$Name_first_let <- substr(Ext_Reviewers_Kernel$Name, 1, 1)
 Ext_Reviewers_Kernel$AU <- paste(Ext_Reviewers_Kernel$Surname, Ext_Reviewers_Kernel$Name_first_let, sep = " ")  

 Ext_Reviewers_Kernel$AU <- as.character(Ext_Reviewers_Kernel$AU)
 
 # Selected list of variables 
 xxx <- Ext_Reviewers_Kernel %>% distinct(AU, ProjectNumber_FP, ProjectYear, .keep_all = TRUE) %>% 
   dplyr::select(AU, DateofBirth, Sex, Scheme, ProjectYear, ProjectNumber_FP, University, DateInvitation, Request, Received, starts_with("Q"), Scale)


  
##-- Merge with WoS dataset 
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/Publication_activity_reviewers.RData")
 
 yyy <- inner_join(xxx, publication_activity_reviewers, by = c("AU", "ProjectYear")) %>% 
   distinct(AU, ProjectNumber_FP, ProjectYear, .keep_all = TRUE)
 

 
##-- Merge with Shanghai ranking 2005 
 
 yyy$University <- toupper(yyy$University)
 
 yyy$Shanghai_top100_reviewers <- as.integer(str_detect(yyy$University, paste(c("HARVARD", "CAMBRIDGE", "STANFORD", "BERKELEY", "MASSACHUSETTS", "CALIFORNIA INSTITUTE", "COLUMBIA",
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
   mutate(Shanghai_top100_reviewers = replace(Shanghai_top100_reviewers, is.na(Shanghai_top100_reviewers), 0))  
 
 
 
 
##-- Create a set of panel reviewer-level variables 
 # Note: detailed explanations of variables can be found in SI
 
 yyy$DateofBirth <- as.numeric(yyy$DateofBirth)
 yyy$ProjectYear <- as.numeric(yyy$ProjectYear)
 
 yyy <- yyy %>% filter(DateofBirth > 1900 & DateofBirth < 2000) %>%
   mutate(Age_reviewers = ProjectYear - DateofBirth,
          Female_reviewers = ifelse(Sex == "F", 1, 0),
          Age_reviewers = ifelse(Age_reviewers < 30, 30, Age_reviewers),
          Age_log_reviewers = log(Age_reviewers),
          Univ_reviewers = ifelse(!is.na(University), 1, 0),
          Publication_stock_reviewers = Publication_stock,
          Publication_stock_log_reviewers = log(Publication_stock + 1),
          Citation_stock_log_reviewers = log(Citation_stock + 1),
          Citat_per_paper_log_reviewers = log(Citat_per_paper + 1))
 
 
 #-- Time to review variable [not used in the analysis]
 # yyy <- yyy %>%
 #   mutate(Date_diff = ifelse(!is.na(DateInvitation), as.Date(Received, format="%Y/%m/%d") - as.Date(DateInvitation, format="%Y/%m/%d"),
 #                             as.Date(Received, format="%Y/%m/%d") - as.Date(Request, format="%Y/%m/%d"))) %>%
 #   mutate(Date_diff = replace(Date_diff, Date_diff < 0, NA),
 #          Date_diff = replace(Date_diff, Date_diff > 150, NA))
   
   
#-- Remove outliers
 
 yyy <- yyy %>% 
   filter(Publication_stock < 200)  # Arbitrary cut-off on productivity


 # Selected list of variables
 yyy <- yyy %>% 
   dplyr::select(AU, ProjectNumber_FP, Scheme, ProjectYear, Age_reviewers, Age_log_reviewers, Female_reviewers, Date_diff,
                 Univ_reviewers, Publication_stock_reviewers, Publication_stock_log_reviewers, 
                 Citation_stock_log_reviewers,
                 Citat_per_paper_log_reviewers, 
                 Shanghai_top100_reviewers, Q1:Q10_Expl, Scale)  
 
 Project_Ext_Reviewers_Kernel <- yyy
 
 
 
##-- Clean the working environment 
 rm(Ext_Reviewers_Kernel, publication_activity_reviewers, xxx, yyy)
 
 
 save.image("C:/Users/Stefano/Desktop/GIGA_Project/Project_ext_reviewers_kernel.RData")
 