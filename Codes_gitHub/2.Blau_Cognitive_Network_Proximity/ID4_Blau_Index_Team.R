
#############################################################################################################
# Script id: 4
# Description: This script creates Blau index for team applicants  
# Author: GIGA team
# Last update: 24.04.2021
#############################################################################################################



##-- Libraries

rm(list = ls())

library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyr)
library(readxl)



#-- Upload and prepare the list of applicants for matching with WoS data

 Individ_Kernel <- read_excel("C:/Users/Stefano/Desktop/GIGA_Project/Eurocores_Kernel_Main.xlsx", sheet = "Kernel_IndvLvl")

 # Harmonize variable format
 Individ_Kernel$Surname <- toupper(Individ_Kernel$Surname)
 Individ_Kernel$Surname <- iconv(Individ_Kernel$Surname, "UTF-8", "ASCII//TRANSLIT")
 
 Individ_Kernel$Name <- toupper(Individ_Kernel$Name)
 Individ_Kernel$Name <- iconv(Individ_Kernel$Name, "UTF-8", "ASCII//TRANSLIT")
 
 # Create the variable AU (Surname + First letter) - used as key for merging with WoS
 Individ_Kernel$Name_first_let <- substr(Individ_Kernel$Name, 1, 1)
 Individ_Kernel$AU <- paste(Individ_Kernel$Surname, Individ_Kernel$Name_first_let, sep = " ")  
 
 Individ_Kernel$AU <- as.character(Individ_Kernel$AU)
 Individ_Kernel$ProjectNumber_OP <- toupper(Individ_Kernel$ProjectNumber_OP)

 # Selected list of variables
 Individ_Kernel <- Individ_Kernel %>% distinct(AU, ProjectYear, .keep_all = TRUE) %>% 
   dplyr::select(AU, DateofBirth, Sex, ProjectNumber_OP, ProjectNumber_FP, ProjectYear, Country,
                 University, Organisation, CType, PESC, LESC, EMRC, SCSS, SCH)



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
   dplyr::select(AU, ProjectYear, ProjectNumber_OP)

 xxx <- left_join(xxx, project_year, by = "AU")

 # Remove publications before project application year
 xxx <- xxx %>% filter(PY <= ProjectYear)



 #-- Aggregate WoS categories in scientific macroareas [256 WoS subject categories clustered into 21 macroareas]
yyy <- yyy %>% 
  separate_rows(WC, sep = "; ") %>%
  mutate(Biomedical_sciences = ifelse(WC == "Anatomy & Morphology"  |  WC == "Andrology"  |  WC == "Biochemical Research Methods"  |  WC == "Biochemistry & Molecular Biology" |
                                        WC == "Biology"  | WC == "Biophysics"  | WC == "Biotechnology & Applied Microbiology"  | WC == "Cell Biology" |
                                        WC == "Developmental Biology"  | WC == "Endocrinology & Metabolism"  | WC == "Genetics & Heredity"  | WC == "Mathematical & Computational Biology" |
                                        WC == "Medical Laboratory Technology"  | WC == "Medicine, Legal"  | WC == "Medicine, Research & Experimental"  | WC == "Microscopy" |
                                        WC == "Multidisciplinary Sciences"  | WC == "Nutrition & Dietetics"  | WC == "Obstetrics & Gynecology"  | WC == "Oncology"  |
                                        WC == "Pathology"  | WC == "Pharmacology & Pharmacy"  | WC == "Physiology"  | WC == "Reproductive Biology" |   
                                        WC == "Toxicology"  | WC == "Urology & Nephrology"  | WC == "Cell & Tissue Engineering", 1, 0),
         
         Material_sciences = ifelse(WC == "Electrochemistry"  | WC == "Energy & Fuels"  | WC == "Instruments & Instrumentation"  | WC == "Materials Science, Ceramics"  |
                                      WC == "Materials Science, Characterization & Testing"  | WC == "Materials Science, Coatings & Films"  | WC == "Materials Science, Multidisciplinary"  | 
                                      WC == "Metallurgy & Metallurgical Engineering" | WC == "Mining & Mineral Processing"  | WC == "Nanoscience & Nanotechnology"  |  
                                      WC == "Physics, Applied"  | WC == "Physics, Condensed Matter", 1 , 0),
         
         Computer_sciences = ifelse(WC == "Automation & Control Systems"  | WC == "Computer Science, Artificial Intelligence"  | WC == "Computer Science, Cybernetics"  | WC == "Computer Science, Hardware & Architecture" | 
                                      WC == "Computer Science, Information Systems"  | WC == "Computer Science, Interdisciplinary Applications"  | WC == "Computer Science, Software Engineering"  | WC == "Computer Science, Theory & Methods" | 
                                      WC == "Engineering, Electrical & Electronic"  | WC == "Engineering, Industrial"  | WC == "Mathematics, Applied"  | WC == "Operations Research & Management Science" |
                                      WC == "Robotics"  | WC == "Telecommunications"  | WC == "Transportation Science & Technology"  | WC == "Quantum Science & Technology", 1 , 0),
         
         Clinical_medicine = ifelse(WC == "Anesthesiology"  | WC == "Cardiac & Cardiovascular Systems"  | WC == "Critical Care Medicine"  | WC == "Dentistry, Oral Surgery & Medicine" |
                                      WC == "Dermatology"  | WC == "Emergency Medicine"  | WC == "Engineering, Biomedical"  | WC == "Gastroenterology & Hepatology" |
                                      WC == "Hematology"  | WC == "Orthopedics"  | WC == "Otorhinolaryngology"  | WC == "Pediatrics" |
                                      WC == "Peripheral Vascular Disease"  | WC == "Radiology, Nuclear Medicine & Medical Imaging"  | WC == "Respiratory System" |
                                      WC == "Rheumatology"  | WC == "Sport Sciences"  | WC == "Surgery"  | WC == "Transplantation", 1, 0),
         
         Neuro_sciences = ifelse(WC == "Behavioral Sciences"  | WC == "Clinical Neurology"  | WC == "Geriatrics & Gerontology"  | WC == "Neuroimaging" |
                                   WC == "Neurosciences"  | WC == "Ophthalmology"  | WC == "Psychiatry"  | WC == "Psychology" |
                                   WC == "Rehabilitation"  | WC == "Substance Abuse"  | WC == "Psychology, Applied"  | WC == "Psychology, Biological" |
                                   WC == "Psychology, Clinical"  | WC == "Psychology, Developmental"  | WC == "Psychology, Experimental"  | WC == "Psychology, Multidisciplinary" |
                                   WC == "Psychology, Psychoanalysis"  | WC == "Psychology, Educational"  | WC == "Psychology, Mathematical"  | WC == "Psychology, Social", 1, 0),
         
         Ecology = ifelse(WC == "Biodiversity Conservation"  | WC == "Ecology"  | WC == "Entomology"  | WC == "Evolutionary Biology" |
                            WC == "Fisheries"  | WC == "Forestry"  | WC == "Marine & Freshwater Biology"  | WC == "Oceanography" |
                            WC == "Ornithology"  | WC == "Zoology", 1, 0),
         
         Chemistry = ifelse(WC == "Chemistry, Analytical"  | WC == "Chemistry, Applied"  | WC == "Chemistry, Inorganic & Nuclear"  | WC == "Chemistry, Medicinal" |
                              WC == "Chemistry, Multidisciplinary"  | WC == "Chemistry, Organic"  | WC == "Chemistry, Physical"  | WC == "Crystallography" |
                              WC == "Engineering, Chemical"  | WC == "Materials Science, Biomaterials"  | WC == "Materials Science, Textiles" |
                              WC == "Polymer Science"  | WC == "Spectroscopy", 1, 0),
         
         Geo_sciences = ifelse(WC == "Engineering, Geological"  | WC == "Engineering, Petroleum"  | WC == "Geochemistry & Geophysics"  | WC == "Geography, Physical" |
                                 WC == "Geology"  | WC == "Geosciences, Multidisciplinary"  | WC == "Imaging Science & Photographic Technology"  | WC == "Meteorology & Atmospheric Sciences" |
                                 WC == "Mineralogy"  | WC == "Paleontology"  | WC == "Remote Sensing", 1, 0),
         
         Engineering = ifelse(WC == "Acoustics"  | WC == "Construction & Building Technology"  | WC == "Engineering, Aerospace"  | WC == "Engineering, Manufacturing" |
                                WC == "Engineering, Marine"  | WC == "Engineering, Mechanical"  | WC == "Engineering, Multidisciplinary"  | WC == "Materials Science, Composites" |
                                WC == "Mathematics"  | WC == "Mathematics, Interdisciplinary Applications"  | WC == "Mechanics"  | WC == "Thermodynamics", 1, 0),
         
         Infectious_diseases = ifelse(WC == "Allergy"  | WC == "Immunology"  | WC == "Infectious Diseases"  | WC == "Microbiology" |
                                        WC == "Mycology"  | WC == "Parasitology"  | WC == "Tropical Medicine"  | WC == "Veterinary Sciences"  | WC == "Virology", 1, 0),
         
         Environmental_sciences = ifelse(WC == "Agricultural Engineering"  | WC == "Engineering, Civil"  | WC == "Engineering, Environmental"  | WC == "Engineering, Ocean" |
                                           WC == "Environmental Sciences"  | WC == "Limnology"  | WC == "Water Resources", 1, 0),
         
         Agriculture = ifelse(WC == "Agriculture, Dairy & Animal Science"  | WC == "Agriculture, Multidisciplinary"  | WC == "Agronomy"  | WC == "Food Science & Technology" |
                                WC == "Horticulture"  | WC == "Integrative & Complementary Medicine"  | WC == "Materials Science, Paper & Wood"  | WC == "Plant Sciences"  | WC == "Soil Science", 1, 0),
         
         Physics = ifelse(WC == "Astronomy & Astrophysics"  | WC == "Nuclear Science & Technology"  | WC == "Optics"  | WC == "Physics, Atomic, Molecular & Chemical" |
                            WC == "Physics, Fluids & Plasmas"  | WC == "Physics, Mathematical"  | WC == "Physics, Multidisciplinary" |
                            WC == "Physics, Nuclear"  | WC == "Physics, Particles & Fields", 1, 0),
         
         General_medicine_health = ifelse(WC == "Education, Scientific Disciplines"  | WC == "Health Care Sciences & Services"  | WC == "History & Philosophy Of Science"  | WC == "Medical Ethics" |
                                            WC == "Medical Informatics"  | WC == "Medicine, General & Internal"  | WC == "Nursing"  | WC == "Public, Environmental & Occupational Health"  | WC == "Statistics & Probability" |
                                            WC == "Ergonomics"  | WC == "Gerontology"  | WC == "Primary Health Care"  | WC == "Audiology & Speech-Language Pathology"  | WC == "Social Sciences, Biomedical", 1, 0),
         
         Education_information = ifelse(WC == "Education & Educational Research"  | WC == "Education, Special"  | WC == "Information Science & Library Science", 1, 0),
         
         Regional_community_issues = ifelse(WC == "Area Studies"  | WC == "Asian Studies"  | WC == "Demography"  | WC == "Development Studies" |
                                              WC == "Environmental Studies"  | WC == "Family Studies"  | WC == "Geography"  | WC == "Green & Sustainable Science & Technology" |
                                              WC == "Health Policy & Services"  | WC == "Humanities, Multidisciplinary"  | WC == "Regional & Urban Planning"  | WC == "Social Sciences, Interdisciplinary"  | WC == "Social Issues" |
                                              WC == "Social Sciences, Mathematical Methods"  | WC == "Social Work"  | WC == "Sociology"  | WC == "Transportation" |
                                              WC == "Urban Studies"  | WC == "Women's Studies"  | WC == "Ethnic Studies", 1, 0),
         
         Economics_finance_business_management = ifelse(WC == "Agricultural Economics & Policy"  | WC == "Business"  | WC == "Business, Finance" |
                                                          WC == "Economics"  | WC == "Management"  | WC == "Hospitality, Leisure, Sport & Tourism", 1, 0),
         
         History_politics_law = ifelse(WC == "Criminology & Penology"  | WC == "History"  | WC == "History of Social Sciences"  | WC == "Industrial Relations & Labor" |
                                         WC == "International Relations"  | WC == "Law"  | WC == "Medieval & Renaissance Studies"  | WC == "Political Science" |
                                         WC == "Public Administration"  | WC == "Classics", 1, 0),
         
         Arts_literature = ifelse(WC == "Architecture"  | WC == "Art"  | WC == "Dance"  | WC == "Film, Radio, Television"  | WC == "Literary Reviews" |
                                    WC == "Literary Theory & Criticism"  | WC == "Literature"  | WC == "Literature, African, Australian, Canadian"  | WC == "Literature, American" | 
                                    WC == "Literature, British Isles"  | WC == "Literature, German, Dutch, Scandinavian"  | WC == "Literature, Slavic"  | WC == "Literature, Romance"  |  
                                    WC == "Music"  | WC == "Poetry"  | WC == "Theater", 1, 0),
         
         Language_culture = ifelse(WC == "Anthropology"  | WC == "Archaeology"  | WC == "Communication"  | WC == "Cultural Studies" |
                                     WC == "Folklore"  | WC == "Language & Linguistics"  | WC == "Linguistics", 1, 0), 
         
         Philosophy_religion = ifelse(WC == "Ethics"  | WC == "Logic"  | WC == "Philosophy"  | WC == "Religion", 1, 0)
         
  )


#-- Create Blau index

 # Count different WoS macrocategories for each team
 zzz <- yyy %>%
   group_by(ProjectNumber_OP) %>%
     mutate(obs = n()) %>%
     mutate_at(vars(`Biomedical_sciences`:`Philosophy_religion`), sum, na.rm = TRUE) %>%
   ungroup()
 
 zzz <- zzz %>% distinct(ProjectNumber_OP, .keep_all = TRUE)

 # Calculate s^2 for each WoS macrocategories-team
 for(i in 27:(ncol(zzz)-1)){
   zzz[,i] <- (zzz[,i] / zzz$obs)^2
 }
 
 # Create the index
 zzz$Blau_index_team <- rep(0, times = nrow(zzz))
 for(i in 1:nrow(zzz)){
   zzz[i,49] <- 1 - sum(zzz[i, 27:(ncol(zzz)-2)])
 }

 # Create the dataframe with the variables of interest
 blau_index_team <- zzz %>%
   dplyr::select(ProjectNumber_OP, Blau_index_team)

 

##-- Clean the working environment 
 rm(Individ_Kernel, project_year, WoS_Data, xxx, yyy, zzz, i)



##-- Save the final dataset  
 save.image("C:/Users/Stefano/Desktop/GIGA_Project/Blau_index_team.RData")  
  
  
  