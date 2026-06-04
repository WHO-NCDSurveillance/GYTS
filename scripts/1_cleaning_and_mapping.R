##Reading raw data
raw_data = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Raw', guess_max = 5000) %>% mutate(record_id = 1:n())
total_recs = nrow(raw_data)
Sys.setenv(total_recs = as.character(total_recs))

##Reading mapping matrix
mapping_matrix = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Matrix')

###Assigning site specific variables to standard variables; and also checking if numeric entries were done and these are converted to letters
##Reorganising the mapping matrix to extract content for automated analysis: clearly defining what the variables are and assigning labels to the various response categories
map_dictionary = mapping_matrix %>% dplyr::filter(!is.na(survey_question)) %>% 
  dplyr::select(all_of(c("site", "survey_question","var_levels"))) %>% 
  distinct() %>% rowwise %>% 
  mutate(standard = gsub('\\s','',strsplit(survey_question,':')[[1]][1]), 
         revised_var_levels = c(gsub('.:','',strsplit(var_levels, "[;]"))), 
         standard = toupper(standard),
         unlabelled_levels = paste0('c(',paste0('"',sapply(strsplit(unlist(strsplit(var_levels, split = ";")), split = ":"), function(x) gsub('\\s','',x[1])),'"', collapse = ','),')'),
         unlabelled_levels = ifelse(is.na(revised_var_levels),NA,unlabelled_levels),site = tolower(site))
#####

original_raw_data = raw_data 

##Assigning site specific variables to standard variable names
eval(parse(text=paste0('raw_data$',map_dictionary$standard,'=NA', sep = '\n')))
eval(parse(text=paste0('raw_data$',map_dictionary$standard,'= raw_data$',map_dictionary$site, sep = '\n')))

####Converting datasets with integers into letters(This is in case the data entries were done as integers):
##For conversions to happen, we first convert all the variables to character type
raw_data = raw_data %>%mutate(across(everything(), as.character))
###

categorical_variables = map_dictionary$standard
##The next code runs through the data and replaces any numeric entry with letters
i=NULL

for(i in 1:length(categorical_variables))
  {
   eval(parse(text=paste0('raw_data$',categorical_variables[i],'[raw_data$',categorical_variables[i],'==', 1:26 ,']="',LETTERS[1:26],'"', sep = '\n')))
}

# Standard GYTS edit checks - these are first defined and data are checked to see if relevant variables are present

# for 2014 questionnaire version - slight tailoring for SLB for e-cigarettes question
 # checks_data = data.frame(var1 = c(rep('CR1',7),rep('CR5',2),'CR7','CR8','CR9','CR13','CHL70'),
 #                          var1_level = c('A','B','C','D','E','F','G',rep('B',2), rep('A',2),rep('B',3)),
 #                          var2 = c(rep('CR6',8),'CR7','CR8','CR7','CR10','CR14','ELR2'),
 #                          var2_level = c('c("E","F","G","H")',rep('c("F","G","H")',2),rep('c("G","H")',2),rep('c("H")',2), 'c("B","C","D","E","F","G","H")', rep('c("B","C","D","E","F","G")',3),rep('c("A")',2),'c("B","C","D","E","F","G","H")'),
 #                          age_logical = c(rep(TRUE,7), rep(FALSE,7))) %>%

# for 2024 questionnaire version
checks_data = data.frame(var1 = c(rep('CR1',7),rep('CR8',2),'CR10','CR22','CR13B','CR14B','CR12C','CR13C','CR14C','CR12D','CR13D','CR14D','CR12E','CR13E','CR14E','CR12F','CR13F','CR14F','CR12G','CR13G','CR14G','CR12H','CR13H','CR14H','CR41', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR41', 'CR43B', 'CR43C', 'CR43D', 'CR43E', 'CR43F', 'CR43G', 'CR43H', 'CR44', 'ELR2B', 'ELR2C', 'ELR2D', 'ELR2E', 'ELR2F', 'ELR2G', 'ELR2H', 'ELR3B', 'ELR3C', 'ELR3D', 'ELR3E', 'ELR3F', 'ELR3G', 'ELR3H', 'ELR4B', 'ELR4C', 'ELR4D', 'ELR4E', 'ELR4F', 'ELR4G', 'ELR4H', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'SR1', 'SR1', 'SR3B', 'SR3C', 'SR3D', 'SR3E', 'SR3F', 'SR3G', 'SR3H', 'SR4', 'SR6B', 'SR6C', 'SR6D', 'SR6E', 'SR6F', 'SR6G', 'SR6H', 'SR7B', 'SR7C', 'SR7D', 'SR7E', 'SR7F', 'SR7G', 'SR7H', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'HR2', 'HR2', 'HR4B', 'HR4C', 'HR4D', 'HR4E', 'HR4F', 'HR4G', 'HR4H', 'HR5', 'HR7B', 'HR7C', 'HR7D', 'HR7E', 'HR7F', 'HR7G', 'HR7H', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'BR1', 'BR1', 'BR3B', 'BR3C', 'BR3D', 'BR3E', 'BR3F', 'BR3G', 'BR4', 'BR6B', 'BR6C', 'BR6D', 'BR6E', 'BR6F', 'BR6G', 'BR6H', 'BR7B', 'BR7C', 'BR7D', 'BR7E', 'BR7F', 'BR7G', 'BR7H', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR38', 'CR38', 'CR38', 'CR39', 'CR39', 'SLR2B', 'SLR2C', 'SLR2D', 'SLR2E', 'SLR2F', 'SLR2G', 'SLR2H', 'CR39', 'SLR3', 'SLR5B', 'SLR5C', 'SLR5D', 'SLR5E', 'SLR5F', 'SLR5G', 'SLR5H', 'SLR5B', 'SLR5C', 'SLR5D', 'SLR5E', 'SLR5F', 'SLR5G', 'SLR5H', 'SLR6B', 'SLR6C', 'SLR6D', 'SLR6E', 'SLR6F', 'SLR6G', 'SLR6H', 'SLR6B', 'SLR6C', 'SLR6D', 'SLR6E', 'SLR6F', 'SLR6G', 'SLR6H', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'CR1', 'NR2', 'NR2', 'NR4B', 'NR4C', 'NR4D', 'NR4E', 'NR4F', 'NR4G', 'NR4H', 'NR5', 'NR7B', 'NR7C', 'NR7D', 'NR7E', 'NR7F', 'NR7G', 'NR7H', 'NR8B', 'NR8C', 'NR8D', 'NR8E', 'NR8F', 'NR8G', 'NR8H', 'CR23', 'CR23', 'CR23', 'CR23'),
                         var1_level = c('A','B','C','D','E','F','G',rep('B',2),'A',rep('B',3),rep('C',3),rep('D',3),rep('E',3),rep('F',3),rep('G',3), rep('H',3),'B', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'B', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'B', 'B', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'B', 'B', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'B', 'B', 'B', 'C', 'D', 'E', 'F', 'G', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'B', 'B', 'B', 'B', 'A' ,'B','C' ,'D', 'E', 'F', 'G', 'H', 'B', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'B', 'B', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'B', 'B', 'B', 'B'),
                         var2 = c(rep('CR9',8),'CR10','CR11','CR23', rep('CR10',17), rep('CR10',3),'CR42', 'CR42', 'CR42', 'CR42', 'CR42', 'CR42', 'CR42', 'CR42', 'CR44', 'CR41', 'CR41', 'CR41', 'CR41', 'CR41', 'CR41', 'CR41', 'ELR1', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'CR44', 'SR2', 'SR2', 'SR2', 'SR2', 'SR2', 'SR2', 'SR2', 'SR2', 'SR4', 'SR1', 'SR1', 'SR1', 'SR1', 'SR1', 'SR1', 'SR1', 'SR5', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'SR4', 'HR3', 'HR3', 'HR3', 'HR3', 'HR3', 'HR3', 'HR3', 'HR3', 'HR5', 'HR2', 'HR2', 'HR2', 'HR2', 'HR2', 'HR2', 'HR2', 'HR6', 'HR5', 'HR5', 'HR5', 'HR5', 'HR5', 'HR5', 'HR5', 'BR2', 'BR2', 'BR2', 'BR2', 'BR2', 'BR2', 'BR2', 'BR2', 'BR4', 'BR1', 'BR1', 'BR1', 'BR1', 'BR1', 'BR1', 'BR5', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'BR4', 'SLR1', 'SLR1', 'SLR1', 'SLR1', 'SLR1', 'SLR1', 'SLR1', 'SLR1', 'CR39', 'SLR3', 'SLR3', 'SLR3', 'CR38', 'CR38', 'CR38', 'CR38', 'CR38', 'CR38', 'CR38', 'SLR4', 'SLR4', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'CR39', 'CR39', 'CR39', 'CR39', 'CR39', 'CR39', 'CR39', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'SLR3', 'CR39', 'CR39', 'CR39', 'CR39', 'CR39', 'CR39', 'CR39', 'NR3', 'NR3', 'NR3', 'NR3', 'NR3', 'NR3', 'NR3', 'NR3', 'NR5', 'NR2', 'NR2', 'NR2', 'NR2', 'NR2', 'NR2', 'NR2', 'NR6', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'NR5', 'OR6', 'OR7', 'OR8', 'OR9'),
                         var2_level = c('c("E","F","G","H")',rep('c("F","G","H")',2),rep('c("G","H")',2),rep('c("H")',2), 'c("B","C","D","E","F","G","H")', rep('c("B","C","D","E","F","G")',2),rep('c("A")',21),'c("B","C","D","E","F","G","H")', 'c("E","F","G","H")', 'c("F","G","H")', 'c("F","G","H")', 'c("G","H")', 'c("G","H")', 'c("H")', 'c("H")', 'c("B","C","D","E","F","G")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B","C","D","E","F","G")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("E","F","G","H")', 'c("F","G","H")', 'c("F","G","H")', 'c("G","H")', 'c("G","H")', 'c("H")', 'c("H")', 'c("B","C","D","E","F","G","H")', 'c("B","C","D","E","F","G")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B","C","D","E")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("E","F","G","H")', 'c("F","G","H")', 'c("F","G","H")', 'c("G","H")', 'c("G","H")', 'c("H")', 'c("H")', 'c("B","C","D","E","F","G","H")', 'c("B","C","D","E","F","G")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B","C","D","E","F","G")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("E","F","G","H")', 'c("F","G","H")', 'c("F","G","H")', 'c("G","H")', 'c("G","H")', 'c("H")', 'c("H")', 'c("B","C","D","E","F","G","H")', 'c("B","C","D","E","F","G")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B","C","D","E","F","G")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("E","F","G","H")', 'c("F","G","H")', 'c("F","G","H")', 'c("G","H")', 'c("G","H")', 'c("H")', 'c("H")', 'c("B","C","D","E","F","G","H")', 'c("A")', 'c("B","C","D","E","F","G")', 'c("B","C","D","E","F","G")',  'c("A")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B","C","D","E","F","G")', 'c("B","C","D","E","F","G")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("E","F","G","H")', 'c("F","G","H")', 'c("F","G","H")', 'c("G","H")', 'c("G","H")', 'c("H")', 'c("H")', 'c("B","C","D","E","F","G","H")', 'c("B","C","D","E","F","G")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B")', 'c("B","C","D","E","F","G")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("A")', 'c("B","C","D","E","F","G")', 'c("B","C","D","E","F","G")', 'c("B","C","D","E","F","G")', 'c("B","C","D","E","F","G")'),
                         age_logical = c(rep(TRUE,7), rep(FALSE,24), FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                         TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
                                         TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                         TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)) %>%
  ###Adding indicators to check if var1 and var2 variables are missing or not
  mutate(var1_check = eval(parse(text = paste0("c(", paste0("'",var1, "' %in% names(raw_data)", collapse = ","), ")"))),
         var2_check = eval(parse(text = paste0("c(", paste0("'",var2, "' %in% names(raw_data)", collapse = ","), ")"))))%>%
  ####Filter the checks based on available variables
  dplyr::filter(var1_check==TRUE & var2_check ==TRUE)

# this loop considers each consistency check and sets vars in each record to missing where logical checks fail 
# (exception is made for CR1 (age) - if a check fails that includes this variable, it is NOT set to missing, only the other variable is set to missing)
 if(nrow(checks_data)>0)
 {
   ###Duplicating  var1 in checks_data matrix
   # dup_var_names = setdiff(unique(checks_data$var1),"DE_AGE")
   dup_var_names = unique(checks_data$var1)
   eval(parse(text = paste0('raw_data$dup_',dup_var_names,' = raw_data$',dup_var_names, sep ='\n')))
   
   i = NULL
   for (i in 1:length(checks_data$var1))
   {
     if (checks_data$age_logical[i] == TRUE)
     {
       eval(parse(text=paste0('raw_data$',checks_data$var2[i],'[raw_data$',checks_data$var1[i],'=="',checks_data$var1_level[i],'" & (', 
                              paste0('raw_data$',checks_data$var2[i],'=="', eval(parse(text=checks_data$var2_level[i])),'"', collapse = '|'),')]=NA')))
       
     }
     else
     {
       eval(parse(text=paste0('raw_data$',checks_data$var1[i],'[raw_data$',checks_data$var1[i],'=="',
                              checks_data$var1_level[i],'" & (', paste0('raw_data$',checks_data$var2[i],'=="', 
                                                                        eval(parse(text=checks_data$var2_level[i])),'"', collapse = '|'),')]=NA')))
       eval(parse(text=paste0('raw_data$',checks_data$var2[i],'[raw_data$dup_',checks_data$var1[i],'=="',
                              checks_data$var1_level[i],'" & (', paste0('raw_data$',checks_data$var2[i],'=="', 
                                                                        eval(parse(text=checks_data$var2_level[i])),'"', collapse = '|'),')]=NA')))
     }
   }
   # remove the duplicates of the variables created just for this loop
   raw_data = raw_data %>% dplyr::select(-all_of(paste0('dup_',dup_var_names)))
 } else{}

 ####Excluding out of range entries
##Set out of range values to missing entries that are out of range
eval(parse(text=paste0('raw_data$',map_dictionary$standard, '[!(raw_data$',map_dictionary$standard,'%in%',map_dictionary$unlabelled_levels,')]=NA', sep='\n')))

#####Cleaned standard variables::::::::::::::::::::::::::::::::::::::::::
eval(parse(text=paste0('raw_data$',map_dictionary$site,'= raw_data$',map_dictionary$standard, sep = '\n')))

# Counting number of non-missing responses per record
excl_variables = setdiff(names(raw_data), grep('q',names(raw_data),v=T))

non_missing_counts_row =  raw_data %>%
  dplyr::select(-all_of(excl_variables)) %>%
  mutate(across(everything(), as.character)) %>%
  rowwise() %>%
  mutate(non_missing_count = sum(!is.na(c_across(everything())))) %>% 
  dplyr::select(non_missing_count)

## Filtering out records with fewer than 10 variables completed 
# (this is a guess as to standard GYTS cleaning - have seen final datasets with recs with as few as 12 complete responses)
original_raw_data = raw_data

raw_data = cbind(raw_data,non_missing_counts_row) %>% 
  dplyr::filter(non_missing_count>=10) %>% 
  dplyr::select(-non_missing_count) %>% as.data.frame()












