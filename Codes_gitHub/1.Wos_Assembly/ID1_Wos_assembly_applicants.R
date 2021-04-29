
#########################################################################
# Script id: 1
# Description: This script aggregates Web of Science data for applicants
# Author: GIGA team
# Last update: 24.04.2021
#########################################################################



##-- Libraries

 rm(list = ls())

 library(dplyr)
 library(data.table)
 library(stringr)
 library(readr)
 library(tidyr)
 library(readxl)



##-- Upload and convert WoS row data to R dataframe

 # Set the working directory (folder with WoS row data)
 getwd()
 setwd("C:/Users/Stefano/Desktop/GIGA_Project/WoS_data_applicants/")

 # Create a vector with the list of WoS row data 
 filelist <- list.files(pattern = ".*.txt")
      
      # Double check
      length(filelist) # Length of the vector
      filelist[123]    # Access an element of the vector
  
 # Convert .txt files to dataframes     
 datalist <- lapply(filelist, FUN=read_delim, "\t", escape_double = FALSE, trim_ws = TRUE) 
  
 # Attach dataframes by row
 data <- do.call("rbind", datalist) 
 rm(datalist, filelist)
  
 
 
#-- Save the clean WoS dataframe for applicants
 
 # Selected list of variables 
 WoS_Data <- data %>%
   dplyr::select(UT, PT, AU, AF, TI, SO, DT, DE, ID, AB, C1, EM, FU, FX, CR, NR, TC, PY, DI, WC, SC)

 # Convert to uppercase  
 WoS_Data$AU <- toupper(WoS_Data$AU)
 WoS_Data$FU <- toupper(WoS_Data$FU) 
 WoS_Data$FX <- toupper(WoS_Data$FX)
 
 # Remove duplicates (by UT)
 WoS_Data <- WoS_Data %>% distinct(UT, .keep_all = TRUE)
 rm(data)
  
 #*** HERE WE SAVED "WoS_data_applicant_original.RData" ***
  
  
  
#-- Upload and prepare the list of applicants for matching with WoS data 
  
 Applicants_names <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_IndvLvl")
  
 # Selected list of variables; remove observations with missing surname
 Applicants_names <- Applicants_names %>% 
   dplyr::select(Surname, Name, ProjectYear) %>% 
   filter(Surname != "")
 
 # Harmonize variable format
 Applicants_names$Surname <- toupper(Applicants_names$Surname)
 Applicants_names$Surname <- iconv(Applicants_names$Surname, "UTF-8", "ASCII//TRANSLIT")
  
 Applicants_names$Name <- toupper(Applicants_names$Name)
 Applicants_names$Name <- iconv(Applicants_names$Name, "UTF-8", "ASCII//TRANSLIT")
  
 # Create the variable AU (Surname + First letter) - used as key for merging
 Applicants_names$Name_first_let <- substr(Applicants_names$Name, 1, 1)
 Applicants_names$AU <- paste(Applicants_names$Surname, Applicants_names$Name_first_let, sep = " ")
 Applicants_names$AU <- as.character(Applicants_names$AU)
  
 

#-- Prepare the WoS data for matching with applicants
 
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
 project_year <- Applicants_names %>%
   dplyr::select(AU, ProjectYear)
 
 xxx <- left_join(xxx, project_year, by = "AU")
 
 # Remove publications before project application year
 xxx <- xxx %>% filter(PY <= ProjectYear)
 
 # Calculate normalized citation count
 xxx <- xxx %>%
   mutate(TCN = (as.numeric(TC) / (2020 - as.numeric(PY))) * (ProjectYear - as.numeric(PY)))
  
 # Create productivity variables 
 xxx <- xxx %>% 
   group_by(AU, ProjectYear) %>%
   mutate(Publication_stock = n(),
          Citation_stock = sum(TCN),
          Citat_per_paper = Citation_stock / (Publication_stock + 1)) %>%
   ungroup()
  
 # Selected list of variables
 xxx <- xxx %>% distinct(AU, ProjectYear, .keep_all = TRUE) %>% 
   dplyr::select(AU, ProjectYear, Publication_stock, Citation_stock, Citat_per_paper)

 # Merge by AU
 matched_data <- left_join(Applicants_names, xxx, by = c("AU", "ProjectYear"))
  
 # Note: the data contain some NA when there are no publications before a given project application year or merging didn't give any result
 # Remove observations with missing values
 matched_data_1 <- matched_data %>% filter(!is.na(Publication_stock))
  


#-- Merging refinement using surnames
 
# Note: some observations didn't match using AU as key; we replicate the merging using the surname as key (instead of Surname + First letter name)

 # Observations that didn't match
 matched_data_NA <- matched_data %>% filter(is.na(Publication_stock)) %>%
   dplyr::select(-Publication_stock, -Citation_stock, -Citat_per_paper)
  
 # Take surname only (and save it as AU) and harmonize format
 matched_data_NA$AU <- gsub("(.*).*", "\\1", matched_data_NA$Surname)
  
 # Separate rows by authors (and save in a dataframe called 'xxx')
 xxx <- WoS_Data %>%
   separate_rows(AU, sep = "; ")
  
 xxx$AU <- toupper(xxx$AU)
 xxx$AU <- gsub("(.*),.*", "\\1", xxx$AU)     

 # Remove publications before project application year
 project_year <- Applicants_names %>%
   dplyr::select(Surname, ProjectYear) %>%
   rename(AU = Surname)
  
 xxx <- left_join(xxx, project_year, by = "AU")
 xxx <- xxx %>% filter(PY <= ProjectYear)
  
 # Calculate normalized citation count
 xxx <- xxx %>%
   mutate(TCN = (as.numeric(TC) / (2020 - as.numeric(PY))) * (ProjectYear - as.numeric(PY)))
  
 # Create productivity variables
 xxx <- xxx %>% 
   group_by(AU, ProjectYear) %>%
   mutate(Publication_stock = n(),
          Citation_stock = sum(TCN),
          Citat_per_paper = Citation_stock / (Publication_stock + 1)) %>%
   ungroup()
  
 # Selected list of variables
 xxx <- xxx %>% distinct(AU, ProjectYear, .keep_all = TRUE) %>% 
   dplyr::select(AU, ProjectYear, Publication_stock, Citation_stock, Citat_per_paper)
 
 # Merge by Surname and keep observations with values
 matched_data <- left_join(matched_data_NA, xxx, by = c("AU", "ProjectYear"))
 matched_data_2 <- matched_data %>% filter(!is.na(Publication_stock))
  
  
  
##-- Attach the two dataframes and create the final dataset
  
 publication_activity_applicants <- Applicants_names

 # Selected list of variables -- first merging (AU)
 matched_data_1 <- matched_data_1 %>% 
   dplyr::select(AU, ProjectYear, Publication_stock, Citation_stock, Citat_per_paper)
 
 # Selected list of variables -- second merging (Surname)
 matched_data_2 <- matched_data_2 %>% 
   dplyr::select(AU, ProjectYear, Publication_stock, Citation_stock, Citat_per_paper) %>%
   rename(Surname = AU)
 
 # Merging and clean data
 publication_activity_applicants <- left_join(Applicants_names, matched_data_1, by = c("AU", "ProjectYear"))
 publication_activity_applicants <- left_join(publication_activity_applicants, matched_data_2, by = c("Surname", "ProjectYear"))
 
 publication_activity_applicants <- publication_activity_applicants %>%
   mutate(Publication_stock = ifelse(!is.na(Publication_stock.x), Publication_stock.x, Publication_stock.y),
          Citation_stock = ifelse(!is.na(Citation_stock.x), Citation_stock.x, Citation_stock.y),
          Citat_per_paper = ifelse(!is.na(Citat_per_paper.x), Citat_per_paper.x, Citat_per_paper.y))
   
 # Convert NAs to 0 
 publication_activity_applicants$Publication_stock[is.na(publication_activity_applicants$Publication_stock)] <- 0 
 publication_activity_applicants$Citation_stock[is.na(publication_activity_applicants$Citation_stock)] <- 0
 publication_activity_applicants$Citat_per_paper[is.na(publication_activity_applicants$Citat_per_paper)] <- 0
 
 # Selected list of variables
 publication_activity_applicants <- publication_activity_applicants %>% 
   distinct(AU, ProjectYear, .keep_all = TRUE) %>%
   dplyr::select(AU, ProjectYear, Publication_stock, Citation_stock, Citat_per_paper)
 
 

##-- Clean the working environment 
 rm(Applicants_names, matched_data, matched_data_1, matched_data_2, matched_data_NA, project_year, WoS_Data, xxx)
  

   
##-- Save the final dataset  
 save.image("C:/Users/Stefano/Desktop/GIGA_Project/publication_activity_applicants.RData")
 
 