
#################################################################################################################
# Script id: 6
# Description: This script creates measures of cognitive proximity between team applicants and external reviewers 
# Author: GIGA team
# Last update: 24.04.2021
#################################################################################################################



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
 
 Individ_Kernel$ProjectNumber_FP <- toupper(Individ_Kernel$ProjectNumber_FP)
 Individ_Kernel$Scheme <- toupper(Individ_Kernel$Scheme)

 # Create the variable AU (Surname + First letter) - used as key for merging with WoS
 Individ_Kernel$Name_first_let <- substr(Individ_Kernel$Name, 1, 1)
 Individ_Kernel$AU <- paste(Individ_Kernel$Surname, Individ_Kernel$Name_first_let, sep = " ")  
 
 Individ_Kernel$AU <- as.character(Individ_Kernel$AU)
 Individ_Kernel$ProjectYear <- as.character(Individ_Kernel$ProjectYear)
 
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
   dplyr::select(AU, ProjectNumber_FP, Scheme, ProjectYear)
 
 xxx <- left_join(xxx, project_year, by = "AU")
 
 # Remove publications before project application year
 xxx <- xxx %>% filter(PY <= ProjectYear)


 
#-- Aggregate WoS categories in scientific macroareas [256 WoS subject categories clustered into 21 macroareas]
 
 xxx <- xxx %>% 
   separate_rows(WC, sep = "; ") %>%
   mutate(WC_aggreg = ifelse(WC == "Anatomy & Morphology"  |  WC == "Andrology"  |  WC == "Biochemical Research Methods"  |  WC == "Biochemistry & Molecular Biology" |
                               WC == "Biology"  | WC == "Biophysics"  | WC == "Biotechnology & Applied Microbiology"  | WC == "Cell Biology" |
                               WC == "Developmental Biology"  | WC == "Endocrinology & Metabolism"  | WC == "Genetics & Heredity"  | WC == "Mathematical & Computational Biology" |
                               WC == "Medical Laboratory Technology"  | WC == "Medicine, Legal"  | WC == "Medicine, Research & Experimental"  | WC == "Microscopy" |
                               WC == "Multidisciplinary Sciences"  | WC == "Nutrition & Dietetics"  | WC == "Obstetrics & Gynecology"  | WC == "Oncology"  |
                               WC == "Pathology"  | WC == "Pharmacology & Pharmacy"  | WC == "Physiology"  | WC == "Reproductive Biology" |   
                               WC == "Toxicology"  | WC == "Urology & Nephrology"  | WC == "Cell & Tissue Engineering", "Biomedical_sciences",
                             
                             ifelse(WC == "Electrochemistry"  | WC == "Energy & Fuels"  | WC == "Instruments & Instrumentation"  | WC == "Materials Science, Ceramics"  |
                                      WC == "Materials Science, Characterization & Testing"  | WC == "Materials Science, Coatings & Films"  | WC == "Materials Science, Multidisciplinary"  | 
                                      WC == "Metallurgy & Metallurgical Engineering" | WC == "Mining & Mineral Processing"  | WC == "Nanoscience & Nanotechnology"  |  
                                      WC == "Physics, Applied"  | WC == "Physics, Condensed Matter", "Material_sciences",
                                    
                                    ifelse(WC == "Automation & Control Systems"  | WC == "Computer Science, Artificial Intelligence"  | WC == "Computer Science, Cybernetics"  | WC == "Computer Science, Hardware & Architecture" | 
                                             WC == "Computer Science, Information Systems"  | WC == "Computer Science, Interdisciplinary Applications"  | WC == "Computer Science, Software Engineering"  | WC == "Computer Science, Theory & Methods" | 
                                             WC == "Engineering, Electrical & Electronic"  | WC == "Engineering, Industrial"  | WC == "Mathematics, Applied"  | WC == "Operations Research & Management Science" |
                                             WC == "Robotics"  | WC == "Telecommunications"  | WC == "Transportation Science & Technology"  | WC == "Quantum Science & Technology", "Computer_sciences" ,
                                           
                                           ifelse(WC == "Anesthesiology"  | WC == "Cardiac & Cardiovascular Systems"  | WC == "Critical Care Medicine"  | WC == "Dentistry, Oral Surgery & Medicine" |
                                                    WC == "Dermatology"  | WC == "Emergency Medicine"  | WC == "Engineering, Biomedical"  | WC == "Gastroenterology & Hepatology" |
                                                    WC == "Hematology"  | WC == "Orthopedics"  | WC == "Otorhinolaryngology"  | WC == "Pediatrics" |
                                                    WC == "Peripheral Vascular Disease"  | WC == "Radiology, Nuclear Medicine & Medical Imaging"  | WC == "Respiratory System" |
                                                    WC == "Rheumatology"  | WC == "Sport Sciences"  | WC == "Surgery"  | WC == "Transplantation", "Clinical_medicine",
                                                  
                                                  ifelse(WC == "Behavioral Sciences"  | WC == "Clinical Neurology"  | WC == "Geriatrics & Gerontology"  | WC == "Neuroimaging" |
                                                           WC == "Neurosciences"  | WC == "Ophthalmology"  | WC == "Psychiatry"  | WC == "Psychology" |
                                                           WC == "Rehabilitation"  | WC == "Substance Abuse"  | WC == "Psychology, Applied"  | WC == "Psychology, Biological" |
                                                           WC == "Psychology, Clinical"  | WC == "Psychology, Developmental"  | WC == "Psychology, Experimental"  | WC == "Psychology, Multidisciplinary" |
                                                           WC == "Psychology, Psychoanalysis"  | WC == "Psychology, Educational"  | WC == "Psychology, Mathematical"  | WC == "Psychology, Social", "Neuro_sciences",
                                                         
                                                         ifelse(WC == "Biodiversity Conservation"  | WC == "Ecology"  | WC == "Entomology"  | WC == "Evolutionary Biology" |
                                                                  WC == "Fisheries"  | WC == "Forestry"  | WC == "Marine & Freshwater Biology"  | WC == "Oceanography" |
                                                                  WC == "Ornithology"  | WC == "Zoology", "Ecology",
                                                                
                                                                ifelse(WC == "Chemistry, Analytical"  | WC == "Chemistry, Applied"  | WC == "Chemistry, Inorganic & Nuclear"  | WC == "Chemistry, Medicinal" |
                                                                         WC == "Chemistry, Multidisciplinary"  | WC == "Chemistry, Organic"  | WC == "Chemistry, Physical"  | WC == "Crystallography" |
                                                                         WC == "Engineering, Chemical"  | WC == "Materials Science, Biomaterials"  | WC == "Materials Science, Textiles" |
                                                                         WC == "Polymer Science"  | WC == "Spectroscopy", "Chemistry", 
                                                                       
                                                                       ifelse(WC == "Engineering, Geological"  | WC == "Engineering, Petroleum"  | WC == "Geochemistry & Geophysics"  | WC == "Geography, Physical" |
                                                                                WC == "Geology"  | WC == "Geosciences, Multidisciplinary"  | WC == "Imaging Science & Photographic Technology"  | WC == "Meteorology & Atmospheric Sciences" |
                                                                                WC == "Mineralogy"  | WC == "Paleontology"  | WC == "Remote Sensing", "Geo_sciences",
                                                                              
                                                                              ifelse(WC == "Acoustics"  | WC == "Construction & Building Technology"  | WC == "Engineering, Aerospace"  | WC == "Engineering, Manufacturing" |
                                                                                       WC == "Engineering, Marine"  | WC == "Engineering, Mechanical"  | WC == "Engineering, Multidisciplinary"  | WC == "Materials Science, Composites" |
                                                                                       WC == "Mathematics"  | WC == "Mathematics, Interdisciplinary Applications"  | WC == "Mechanics"  | WC == "Thermodynamics", "Engineering",
                                                                                     
                                                                                     ifelse(WC == "Allergy"  | WC == "Immunology"  | WC == "Infectious Diseases"  | WC == "Microbiology" |
                                                                                              WC == "Mycology"  | WC == "Parasitology"  | WC == "Tropical Medicine"  | WC == "Veterinary Sciences"  | WC == "Virology", "Infectious_diseases",
                                                                                            
                                                                                            ifelse(WC == "Agricultural Engineering"  | WC == "Engineering, Civil"  | WC == "Engineering, Environmental"  | WC == "Engineering, Ocean" |
                                                                                                     WC == "Environmental Sciences"  | WC == "Limnology"  | WC == "Water Resources", "Environmental_sciences",
                                                                                                   
                                                                                                   ifelse(WC == "Agriculture, Dairy & Animal Science"  | WC == "Agriculture, Multidisciplinary"  | WC == "Agronomy"  | WC == "Food Science & Technology" |
                                                                                                            WC == "Horticulture"  | WC == "Integrative & Complementary Medicine"  | WC == "Materials Science, Paper & Wood"  | WC == "Plant Sciences"  | WC == "Soil Science", "Agriculture",
                                                                                                          
                                                                                                          ifelse(WC == "Astronomy & Astrophysics"  | WC == "Nuclear Science & Technology"  | WC == "Optics"  | WC == "Physics, Atomic, Molecular & Chemical" |
                                                                                                                   WC == "Physics, Fluids & Plasmas"  | WC == "Physics, Mathematical"  | WC == "Physics, Multidisciplinary" |
                                                                                                                   WC == "Physics, Nuclear"  | WC == "Physics, Particles & Fields", "Physics",
                                                                                                                 
                                                                                                                 ifelse(WC == "Education, Scientific Disciplines"  | WC == "Health Care Sciences & Services"  | WC == "History & Philosophy Of Science"  | WC == "Medical Ethics" |
                                                                                                                          WC == "Medical Informatics"  | WC == "Medicine, General & Internal"  | WC == "Nursing"  | WC == "Public, Environmental & Occupational Health"  | WC == "Statistics & Probability" |
                                                                                                                          WC == "Ergonomics"  | WC == "Gerontology"  | WC == "Primary Health Care"  | WC == "Audiology & Speech-Language Pathology"  | WC == "Social Sciences, Biomedical", "General_medicine_health",
                                                                                                                        
                                                                                                                        ifelse(WC == "Education & Educational Research"  | WC == "Education, Special"  | WC == "Information Science & Library Science", "Education_information",
                                                                                                                               
                                                                                                                               ifelse(WC == "Area Studies"  | WC == "Asian Studies"  | WC == "Demography"  | WC == "Development Studies" |
                                                                                                                                        WC == "Environmental Studies"  | WC == "Family Studies"  | WC == "Geography"  | WC == "Green & Sustainable Science & Technology" |
                                                                                                                                        WC == "Health Policy & Services"  | WC == "Humanities, Multidisciplinary"  | WC == "Regional & Urban Planning"  | WC == "Social Sciences, Interdisciplinary"  | WC == "Social Issues" |
                                                                                                                                        WC == "Social Sciences, Mathematical Methods"  | WC == "Social Work"  | WC == "Sociology"  | WC == "Transportation" |
                                                                                                                                        WC == "Urban Studies"  | WC == "Women's Studies"  | WC == "Ethnic Studies", "Regional_community_issues",
                                                                                                                                      
                                                                                                                                      ifelse(WC == "Agricultural Economics & Policy"  | WC == "Business"  | WC == "Business, Finance" |
                                                                                                                                               WC == "Economics"  | WC == "Management"  | WC == "Hospitality, Leisure, Sport & Tourism", "Economics_finance_business_management",
                                                                                                                                             
                                                                                                                                             ifelse(WC == "Criminology & Penology"  | WC == "History"  | WC == "History of Social Sciences"  | WC == "Industrial Relations & Labor" |
                                                                                                                                                      WC == "International Relations"  | WC == "Law"  | WC == "Medieval & Renaissance Studies"  | WC == "Political Science" |
                                                                                                                                                      WC == "Public Administration"  | WC == "Classics", "History_politics_law",
                                                                                                                                                    
                                                                                                                                                    ifelse(WC == "Architecture"  | WC == "Art"  | WC == "Dance"  | WC == "Film, Radio, Television"  | WC == "Literary Reviews" |
                                                                                                                                                             WC == "Literary Theory & Criticism"  | WC == "Literature"  | WC == "Literature, African, Australian, Canadian"  | WC == "Literature, American" | 
                                                                                                                                                             WC == "Literature, British Isles"  | WC == "Literature, German, Dutch, Scandinavian"  | WC == "Literature, Slavic"  | WC == "Literature, Romance"  |  
                                                                                                                                                             WC == "Music"  | WC == "Poetry"  | WC == "Theater", "Arts_literature",
                                                                                                                                                           
                                                                                                                                                           ifelse(WC == "Anthropology"  | WC == "Archaeology"  | WC == "Communication"  | WC == "Cultural Studies" |
                                                                                                                                                                    WC == "Folklore"  | WC == "Language & Linguistics"  | WC == "Linguistics", "Language_culture", "Philosophy_religion"
                                                                                                                                                           )))))))))))))))))))))
 
 
 
 
 
 
 
#-- Identify the different WoS macrocategories for each project-year
 Diversity <- xxx %>%
   separate_rows(WC_aggreg, sep = "; ") %>%
   distinct(ProjectNumber_FP, WC_aggreg, .keep_all = TRUE) %>%
   dplyr::select(WC_aggreg, ProjectNumber_FP, Scheme, ProjectYear) %>%
   group_by(ProjectNumber_FP) %>%
    mutate(Nr_cat = n()) %>%
   ungroup()

 
  
#-- Upload and prepare the list of external reviewers for matching with WoS data 
 
 Reviewers_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_ExternalReviewer")
 
 
 # Harmonize variable format 
 Reviewers_Kernel$Surname <- toupper(Reviewers_Kernel$Surname)
 Reviewers_Kernel$Surname <- iconv(Reviewers_Kernel$Surname, "UTF-8", "ASCII//TRANSLIT")
 
 Reviewers_Kernel$Name <- toupper(Reviewers_Kernel$Name)
 Reviewers_Kernel$Name <- iconv(Reviewers_Kernel$Name, "UTF-8", "ASCII//TRANSLIT")
 
 # Create the variable AU (Surname + First letter) - used as key for merging with WoS
 Reviewers_Kernel$Name_first_let <- substr(Reviewers_Kernel$Name, 1, 1)
 Reviewers_Kernel$AU <- paste(Reviewers_Kernel$Surname, Reviewers_Kernel$Name_first_let, sep = " ")  
 
 Reviewers_Kernel$AU <- as.character(Reviewers_Kernel$AU)
 Reviewers_Kernel$Scheme <- toupper(Reviewers_Kernel$Scheme)
 
 Reviewers_Kernel$ProjectNumber_FP <- toupper(Reviewers_Kernel$ProjectNumber_FP)
 
 
 
#-- Prepare the WoS data for matching with reviewers kernel
 
 load("C:/Users/Stefano/Desktop/GIGA_Project/WoS_data_reviewers_original.RData")
 
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
 

  
##-- Merge WoS and reviwers dataframes   
 
 # Attach the variable 'ProjectYear' to WoS data
 project_year <- Reviewers_Kernel %>%
   dplyr::select(AU, Scheme, ProjectNumber_FP, ProjectYear)
 
 xxx <- left_join(xxx, project_year, by = "AU")
 
 # Remove publications before project application year
 xxx <- xxx %>% filter(PY <= ProjectYear)
 
 
 
#-- Aggregate WoS categories in scientific macroareas [256 WoS subject categories clustered into 21 macroareas]
 
 xxx <- xxx %>% 
   separate_rows(WC, sep = "; ") %>%
   mutate(WC_aggreg = ifelse(WC == "Anatomy & Morphology"  |  WC == "Andrology"  |  WC == "Biochemical Research Methods"  |  WC == "Biochemistry & Molecular Biology" |
                               WC == "Biology"  | WC == "Biophysics"  | WC == "Biotechnology & Applied Microbiology"  | WC == "Cell Biology" |
                               WC == "Developmental Biology"  | WC == "Endocrinology & Metabolism"  | WC == "Genetics & Heredity"  | WC == "Mathematical & Computational Biology" |
                               WC == "Medical Laboratory Technology"  | WC == "Medicine, Legal"  | WC == "Medicine, Research & Experimental"  | WC == "Microscopy" |
                               WC == "Multidisciplinary Sciences"  | WC == "Nutrition & Dietetics"  | WC == "Obstetrics & Gynecology"  | WC == "Oncology"  |
                               WC == "Pathology"  | WC == "Pharmacology & Pharmacy"  | WC == "Physiology"  | WC == "Reproductive Biology" |   
                               WC == "Toxicology"  | WC == "Urology & Nephrology"  | WC == "Cell & Tissue Engineering", "Biomedical_sciences",
                             
                             ifelse(WC == "Electrochemistry"  | WC == "Energy & Fuels"  | WC == "Instruments & Instrumentation"  | WC == "Materials Science, Ceramics"  |
                                      WC == "Materials Science, Characterization & Testing"  | WC == "Materials Science, Coatings & Films"  | WC == "Materials Science, Multidisciplinary"  | 
                                      WC == "Metallurgy & Metallurgical Engineering" | WC == "Mining & Mineral Processing"  | WC == "Nanoscience & Nanotechnology"  |  
                                      WC == "Physics, Applied"  | WC == "Physics, Condensed Matter", "Material_sciences",
                                    
                                    ifelse(WC == "Automation & Control Systems"  | WC == "Computer Science, Artificial Intelligence"  | WC == "Computer Science, Cybernetics"  | WC == "Computer Science, Hardware & Architecture" | 
                                             WC == "Computer Science, Information Systems"  | WC == "Computer Science, Interdisciplinary Applications"  | WC == "Computer Science, Software Engineering"  | WC == "Computer Science, Theory & Methods" | 
                                             WC == "Engineering, Electrical & Electronic"  | WC == "Engineering, Industrial"  | WC == "Mathematics, Applied"  | WC == "Operations Research & Management Science" |
                                             WC == "Robotics"  | WC == "Telecommunications"  | WC == "Transportation Science & Technology"  | WC == "Quantum Science & Technology", "Computer_sciences" ,
                                           
                                           ifelse(WC == "Anesthesiology"  | WC == "Cardiac & Cardiovascular Systems"  | WC == "Critical Care Medicine"  | WC == "Dentistry, Oral Surgery & Medicine" |
                                                    WC == "Dermatology"  | WC == "Emergency Medicine"  | WC == "Engineering, Biomedical"  | WC == "Gastroenterology & Hepatology" |
                                                    WC == "Hematology"  | WC == "Orthopedics"  | WC == "Otorhinolaryngology"  | WC == "Pediatrics" |
                                                    WC == "Peripheral Vascular Disease"  | WC == "Radiology, Nuclear Medicine & Medical Imaging"  | WC == "Respiratory System" |
                                                    WC == "Rheumatology"  | WC == "Sport Sciences"  | WC == "Surgery"  | WC == "Transplantation", "Clinical_medicine",
                                                  
                                                  ifelse(WC == "Behavioral Sciences"  | WC == "Clinical Neurology"  | WC == "Geriatrics & Gerontology"  | WC == "Neuroimaging" |
                                                           WC == "Neurosciences"  | WC == "Ophthalmology"  | WC == "Psychiatry"  | WC == "Psychology" |
                                                           WC == "Rehabilitation"  | WC == "Substance Abuse"  | WC == "Psychology, Applied"  | WC == "Psychology, Biological" |
                                                           WC == "Psychology, Clinical"  | WC == "Psychology, Developmental"  | WC == "Psychology, Experimental"  | WC == "Psychology, Multidisciplinary" |
                                                           WC == "Psychology, Psychoanalysis"  | WC == "Psychology, Educational"  | WC == "Psychology, Mathematical"  | WC == "Psychology, Social", "Neuro_sciences",
                                                         
                                                         ifelse(WC == "Biodiversity Conservation"  | WC == "Ecology"  | WC == "Entomology"  | WC == "Evolutionary Biology" |
                                                                  WC == "Fisheries"  | WC == "Forestry"  | WC == "Marine & Freshwater Biology"  | WC == "Oceanography" |
                                                                  WC == "Ornithology"  | WC == "Zoology", "Ecology",
                                                                
                                                                ifelse(WC == "Chemistry, Analytical"  | WC == "Chemistry, Applied"  | WC == "Chemistry, Inorganic & Nuclear"  | WC == "Chemistry, Medicinal" |
                                                                         WC == "Chemistry, Multidisciplinary"  | WC == "Chemistry, Organic"  | WC == "Chemistry, Physical"  | WC == "Crystallography" |
                                                                         WC == "Engineering, Chemical"  | WC == "Materials Science, Biomaterials"  | WC == "Materials Science, Textiles" |
                                                                         WC == "Polymer Science"  | WC == "Spectroscopy", "Chemistry", 
                                                                       
                                                                       ifelse(WC == "Engineering, Geological"  | WC == "Engineering, Petroleum"  | WC == "Geochemistry & Geophysics"  | WC == "Geography, Physical" |
                                                                                WC == "Geology"  | WC == "Geosciences, Multidisciplinary"  | WC == "Imaging Science & Photographic Technology"  | WC == "Meteorology & Atmospheric Sciences" |
                                                                                WC == "Mineralogy"  | WC == "Paleontology"  | WC == "Remote Sensing", "Geo_sciences",
                                                                              
                                                                              ifelse(WC == "Acoustics"  | WC == "Construction & Building Technology"  | WC == "Engineering, Aerospace"  | WC == "Engineering, Manufacturing" |
                                                                                       WC == "Engineering, Marine"  | WC == "Engineering, Mechanical"  | WC == "Engineering, Multidisciplinary"  | WC == "Materials Science, Composites" |
                                                                                       WC == "Mathematics"  | WC == "Mathematics, Interdisciplinary Applications"  | WC == "Mechanics"  | WC == "Thermodynamics", "Engineering",
                                                                                     
                                                                                     ifelse(WC == "Allergy"  | WC == "Immunology"  | WC == "Infectious Diseases"  | WC == "Microbiology" |
                                                                                              WC == "Mycology"  | WC == "Parasitology"  | WC == "Tropical Medicine"  | WC == "Veterinary Sciences"  | WC == "Virology", "Infectious_diseases",
                                                                                            
                                                                                            ifelse(WC == "Agricultural Engineering"  | WC == "Engineering, Civil"  | WC == "Engineering, Environmental"  | WC == "Engineering, Ocean" |
                                                                                                     WC == "Environmental Sciences"  | WC == "Limnology"  | WC == "Water Resources", "Environmental_sciences",
                                                                                                   
                                                                                                   ifelse(WC == "Agriculture, Dairy & Animal Science"  | WC == "Agriculture, Multidisciplinary"  | WC == "Agronomy"  | WC == "Food Science & Technology" |
                                                                                                            WC == "Horticulture"  | WC == "Integrative & Complementary Medicine"  | WC == "Materials Science, Paper & Wood"  | WC == "Plant Sciences"  | WC == "Soil Science", "Agriculture",
                                                                                                          
                                                                                                          ifelse(WC == "Astronomy & Astrophysics"  | WC == "Nuclear Science & Technology"  | WC == "Optics"  | WC == "Physics, Atomic, Molecular & Chemical" |
                                                                                                                   WC == "Physics, Fluids & Plasmas"  | WC == "Physics, Mathematical"  | WC == "Physics, Multidisciplinary" |
                                                                                                                   WC == "Physics, Nuclear"  | WC == "Physics, Particles & Fields", "Physics",
                                                                                                                 
                                                                                                                 ifelse(WC == "Education, Scientific Disciplines"  | WC == "Health Care Sciences & Services"  | WC == "History & Philosophy Of Science"  | WC == "Medical Ethics" |
                                                                                                                          WC == "Medical Informatics"  | WC == "Medicine, General & Internal"  | WC == "Nursing"  | WC == "Public, Environmental & Occupational Health"  | WC == "Statistics & Probability" |
                                                                                                                          WC == "Ergonomics"  | WC == "Gerontology"  | WC == "Primary Health Care"  | WC == "Audiology & Speech-Language Pathology"  | WC == "Social Sciences, Biomedical", "General_medicine_health",
                                                                                                                        
                                                                                                                        ifelse(WC == "Education & Educational Research"  | WC == "Education, Special"  | WC == "Information Science & Library Science", "Education_information",
                                                                                                                               
                                                                                                                               ifelse(WC == "Area Studies"  | WC == "Asian Studies"  | WC == "Demography"  | WC == "Development Studies" |
                                                                                                                                        WC == "Environmental Studies"  | WC == "Family Studies"  | WC == "Geography"  | WC == "Green & Sustainable Science & Technology" |
                                                                                                                                        WC == "Health Policy & Services"  | WC == "Humanities, Multidisciplinary"  | WC == "Regional & Urban Planning"  | WC == "Social Sciences, Interdisciplinary"  | WC == "Social Issues" |
                                                                                                                                        WC == "Social Sciences, Mathematical Methods"  | WC == "Social Work"  | WC == "Sociology"  | WC == "Transportation" |
                                                                                                                                        WC == "Urban Studies"  | WC == "Women's Studies"  | WC == "Ethnic Studies", "Regional_community_issues",
                                                                                                                                      
                                                                                                                                      ifelse(WC == "Agricultural Economics & Policy"  | WC == "Business"  | WC == "Business, Finance" |
                                                                                                                                               WC == "Economics"  | WC == "Management"  | WC == "Hospitality, Leisure, Sport & Tourism", "Economics_finance_business_management",
                                                                                                                                             
                                                                                                                                             ifelse(WC == "Criminology & Penology"  | WC == "History"  | WC == "History of Social Sciences"  | WC == "Industrial Relations & Labor" |
                                                                                                                                                      WC == "International Relations"  | WC == "Law"  | WC == "Medieval & Renaissance Studies"  | WC == "Political Science" |
                                                                                                                                                      WC == "Public Administration"  | WC == "Classics", "History_politics_law",
                                                                                                                                                    
                                                                                                                                                    ifelse(WC == "Architecture"  | WC == "Art"  | WC == "Dance"  | WC == "Film, Radio, Television"  | WC == "Literary Reviews" |
                                                                                                                                                             WC == "Literary Theory & Criticism"  | WC == "Literature"  | WC == "Literature, African, Australian, Canadian"  | WC == "Literature, American" | 
                                                                                                                                                             WC == "Literature, British Isles"  | WC == "Literature, German, Dutch, Scandinavian"  | WC == "Literature, Slavic"  | WC == "Literature, Romance"  |  
                                                                                                                                                             WC == "Music"  | WC == "Poetry"  | WC == "Theater", "Arts_literature",
                                                                                                                                                           
                                                                                                                                                           ifelse(WC == "Anthropology"  | WC == "Archaeology"  | WC == "Communication"  | WC == "Cultural Studies" |
                                                                                                                                                                    WC == "Folklore"  | WC == "Language & Linguistics"  | WC == "Linguistics", "Language_culture", "Philosophy_religion"
                                                                                                                                                           )))))))))))))))))))))
 
 
 
 
 
 
 
 
#-- Identify different WoS macrocategories for each reviwer-year
 Diversity_reviewers <- xxx %>%
   separate_rows(WC_aggreg, sep = "; ") %>%
   distinct(AU, ProjectYear, WC_aggreg, .keep_all = TRUE) %>%
   dplyr::select(WC_aggreg, AU, ProjectNumber_FP, ProjectYear) %>%
   group_by(AU, ProjectYear) %>%
    mutate(Nr_cat_reviewers = n()) %>%
   ungroup() 
 
 
 
#-- Create a measure of cognitive proximity between team members and panel experts
# Note: The variable measures the share of overlapping WoS macrocategories 

 # The matching determines the overlapping WoS macrocategories
 Diversity_reviewers$ProjectYear <- as.character(Diversity_reviewers$ProjectYear)
 www <- left_join(Diversity, Diversity_reviewers, by = c("ProjectNumber_FP", "ProjectYear", "WC_aggreg")) %>%
   na.omit()

 # Create the dataframe with the variables of interest
 Cognitive_prox_team_reviewers <- www %>%
   group_by(ProjectNumber_FP, AU, ProjectYear) %>%
    mutate(Nr_overlapping_cat_team = n(),
           Cognitive_prox_team_reviewers = n() / Nr_cat_reviewers) %>%
   ungroup() %>%
   distinct(ProjectNumber_FP, AU, ProjectYear, .keep_all = TRUE) %>%
   dplyr::select(-WC_aggreg, -Scheme)
   
 
 
##-- Clean the working environment
 rm(Individ_Kernel, Reviewers_Kernel, WoS_Data, xxx, www, project_year, Diversity, Diversity_reviewers, wins)
 

  
##-- Save the final dataset 
 save.image("C:/Users/Stefano/Desktop/GIGA_Project/Cognitive_prox_team_reviewers.RData")
 
 