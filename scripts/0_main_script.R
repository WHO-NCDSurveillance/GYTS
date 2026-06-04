rm(list =ls())
library(tidyverse)
##
##https://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu="certainty")
options(survey.adjust.domain.lonely=TRUE)

###Setting working directory. The directory  should contain analysis inputs: raw data file, mapping matrix, sample and frame Excel files
setwd("C:/Users/cowanm/OneDrive - World Health Organization/R/GYTS")
###Installing devtools package first
if(!('remotes' %in% installed.packages()[,"Package"])) {install.packages('remotes')} else{}
if(!('future.apply' %in% installed.packages()[,"Package"])) {install.packages('future.apply')} else{}

###Installing and loading required libraries
package_and_version = data.frame(list.of.packages = c('tidyverse','zscorer','readxl','Hmisc','survey','flextable','officer','questionr','writexl'),
                      versions = c('2.0.0','0.3.1','1.4.3','5.1-0','4.2-1','0.9.2','0.6.2','0.7.8','1.4.2')) %>%
                      mutate(installation = ifelse(!(list.of.packages %in% installed.packages()[,"Package"]),TRUE, FALSE)) %>%
                      dplyr::filter(installation==TRUE)
##
list.of.packages = c('tidyverse','zscorer','readxl','Hmisc','survey','flextable','officer','questionr','writexl')
#
if(nrow(package_and_version)>0)  ###This requires internet connection
{
  eval(parse(text = paste0('remotes::install_version("',package_and_version$list.of.packages,'"',',','"',package_and_version$versions,'")', sep = '\n')))
} else{}

##Loading all libraries
eval(parse(text=paste0('library(',c(list.of.packages,'parallel','future.apply'),')', sep = '\n')))
#eval(parse(text=paste0('library(',c(list.of.packages,'parallel'),')', sep = '\n')))

###Allocation of cores for analyses
 num_cores = as.numeric(availableCores())
 num_cores = ifelse(num_cores>1, round(num_cores*0.6), num_cores)
# Plan and set up parallel processing using future
plan(multisession, workers = num_cores)
#num_cores = parallel::detectCores()-2
#cl = makeCluster(num_cores)
##Indicating country name and the year of the survey
site_name = "COUNTRY NAME"
survey_year ='2025'
agerange = "13-15"  # 13-15 or 13-17
is_this_census = "No"
language =c('ENGLISH','FRENCH', 'SPANISH','RUSSIAN','OTHER')[1]
if (language =='OTHER'){language = 'SPECIFY'} else{}
post_weight <- "By both sex and grade"
weighted_reporting <- "Yes"

##Clear content of temp_tables, reports, and weighted dataset folders
unlink(paste0(getwd(),"/temp_tables/*"))
unlink(paste0(getwd(),"/reports/*"))
unlink(paste0(getwd(),"/weighted dataset/*"))

###The scripts are structured into sections as indicated below

##Section 1: Edit checks, cleaning, and mapping site specific survey questions to standard format
source(paste0(getwd(),'/scripts/1_cleaning_and_mapping.R'))
##Section 2: Weighting
source(paste0(getwd(),'/scripts/2_weighting.R'))
###Section 3: Preprocessing of datasets
source(paste0(getwd(),'/scripts/3_pre_report_processing.R'))
##Section 4: Primary codebook generation
source(paste0(getwd(),'/scripts/4_primary_codebook.R'))
##Section 5: Demographic Table
source(paste0(getwd(),'/scripts/5_demographic_table.R'))
##Section 7: Binary codebook generation
source(paste0(getwd(),'/scripts/7_binary_codebook.R'))
##Section 9: Fact sheet
source(paste0(getwd(),'/scripts/9_factsheet.R'))
##Section 10: Sample Description
source(paste0(getwd(),'/scripts/10_sample_description.R'))
##Section 8: Summary tables
source(paste0(getwd(),'/scripts/8_summary_tables.R'))
##Section 6: Detailed tables
source(paste0(getwd(),'/scripts/6_detailed_tables.R'))
###Stopping the cores
#stopCluster(cl)
