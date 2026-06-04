####NOTE: When working with rstudio interface please uncomment lines 3 - 6
##Loading Excel files for frame and sample/selected schools
frame_schools = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Frame', col_names = TRUE, col_types = "text")
sample_schools = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Sample')
colnames(frame_schools) = tolower(colnames(frame_schools))
colnames(sample_schools) = tolower(colnames(sample_schools))

##Setting school_ID and class_ID to lower case to merge with frame and sample in weight computation

school_ID = grep('school_|SCHOOL_|School_', names(raw_data), v=T)
class_ID = grep('class_|CLASS_|Class_', names(raw_data), v=T)
#
raw_data = raw_data %>% dplyr::rename (school_id = school_ID, class_id = class_ID) %>%
  mutate(SAMPL_SEX = CR2, char_SAMPL_SEX = CR2,
         SAMPL_SEX = factor(SAMPL_SEX, levels = names(table(CR2)), labels = names(table(CR2))),
         SAMPL_GRADE = CR3, char_SAMPL_GRADE = CR3,
         SAMPL_GRADE = factor(SAMPL_GRADE, levels = names(table(CR3)), labels = names(table(CR3)))) 


###Imputation of missing sex and grade data: Imputation is conducted while maintaining the distribution ratios for sex and grade
#Defining sex imputation logic for each school
sex_school_lookup = raw_data %>% 
  reframe(tab_names = names(table(SAMPL_SEX)),prop.table(table(SAMPL_SEX)), .by = school_id) %>%
  dplyr::rename(res = `prop.table(table(SAMPL_SEX))`,sex = tab_names) %>% 
  reframe(res = as.numeric(res),
          res = ifelse(is.na(res),0.5, res),
          sexImp = paste0('sample(c("A","B"),1, prob = c(',res[sex=='A'],',',res[sex=='B'],'))'), .by = school_id) %>%
  dplyr::select(school_id, sexImp) %>%unique() %>%as.data.frame()
#Applying the defined logic to impute missing sex
eval(parse(text=paste0('raw_data$char_SAMPL_SEX[is.na(raw_data$char_SAMPL_SEX) & raw_data$school_id ==',sex_school_lookup[,1],"]='",sex_school_lookup[,2],"'",sep='\n')))
raw_data = raw_data %>% rowwise %>% 
  mutate(char_SAMPL_SEX = ifelse(!(char_SAMPL_SEX=='A' | char_SAMPL_SEX=='B'),eval(parse(text=paste0(char_SAMPL_SEX))),CR2)) %>%
  as.data.frame()
##
#Defining grade imputation logic for each school
grade_school_lookup = raw_data %>% 
  reframe(tab_names = names(table(SAMPL_GRADE)),prop.table(table(SAMPL_GRADE)),.by = c(school_id, class_id)) %>%
  dplyr::rename(res = `prop.table(table(SAMPL_GRADE))`, grade = tab_names) %>% 
  reframe(res = as.numeric(as.character(res)), res = ifelse(is.na(res),1/length(grade), res),
          gradeImp = paste0('sample(',paste0('c(',paste0("'",grade,"'", collapse=','),')'),',','1',',', 
                            prob = paste0('c(',paste0(res, collapse = ','),')'),', replace = F)'),
          .by = c(school_id, class_id)) %>%
  dplyr::select(school_id, class_id, gradeImp) %>%unique()%>%as.data.frame()

#Applying the defined logic to impute missing grade
map_categorical = map_dictionary
eval(parse(text=paste0('raw_data$char_SAMPL_GRADE[is.na(raw_data$char_SAMPL_GRADE) & raw_data$school_id ==',grade_school_lookup[,1], 
                       ' & raw_data$class_id ==',grade_school_lookup[,2], ']="',grade_school_lookup[,3],'"',sep='\n')))
raw_data = raw_data %>% rowwise %>% 
  mutate(char_SAMPL_GRADE = ifelse(eval(parse(text=paste0('!(',paste0('char_SAMPL_GRADE == "', 
                                                                      eval(parse(text=map_categorical$unlabelled_levels[map_categorical$standard=='CR3'])),'"', collapse = '|'),')'))),
                                   eval(parse(text=paste0(char_SAMPL_GRADE))),char_SAMPL_GRADE)) %>%as.data.frame()

##Factor  conversions for categorical variables
eval(parse(text=paste0('raw_data$',map_categorical$standard, '= factor(as.character(raw_data$',map_categorical$standard,'), levels = ',map_categorical$unlabelled_levels, ')', sep='\n')))
# eval(parse(text=paste0('raw_data$',map_categorical$standard, '= factor(raw_data$',map_categorical$standard,', levels = ',
#                        map_categorical$unlabelled_levels, ', labels = ',map_categorical$revised_var_levels,')', sep='\n')))
# ##Assigning labels
# eval(parse(text=paste0('label(raw_data$',map_dictionary$standard,') = "',gsub('\\s+',' ',gsub('^\\s*[^:]*:\\s*','',map_dictionary$survey_question)),'"')))

# convert all enrolment columns in sampling frame to numeric
frame_schools[, grepl("boys|girls", names(frame_schools), ignore.case = TRUE)] <- 
  lapply(frame_schools[, grepl("boys|girls", names(frame_schools), ignore.case = TRUE)], as.numeric)

# ensuring all empty enrolment cells are set to 0 and not NA so total enrolment can be calculated
frame_schools[is.na(frame_schools)]=0

# adding total enrolment for each school to frame_schools
frame_schools = frame_schools %>% 
  mutate(enrolment = eval(parse(text=paste0(grep('boys|girls', names(frame_schools), v=T), collapse = '+')))) %>% dplyr::filter(enrolment >0)

###Adding total enrolment for each school to sample_schools
sample_schools = sample_schools %>% #mutate_all(~replace_na(., 0)) %>%
  mutate(quantil_grp = case_when(enrolment < quantile(enrolment, 0.3333, na.rm = T)~1,
                                 enrolment >= quantile(enrolment, 0.3333, na.rm = T)&
                                   enrolment < quantile(enrolment, 0.6666, na.rm = T)~2,
                                 enrolment >= quantile(enrolment, 0.6666, na.rm = T) ~3),
         school_id = as.character(school_id)) 
##Computing school non-response adjustment factor by quantiles
sample_schools = sample_schools %>%
  left_join(sample_schools %>%
              reframe(sch_prt = sum(school_part==1, na.rm = T), sch_sel = sum((school_part==1|school_part==0), na.rm = T),
                      schwgt_adj_factor = round(sch_sel/sch_prt,5),.by=quantil_grp) %>%dplyr::select(quantil_grp, schwgt_adj_factor))

##Applying school non-response factor to obtain adjusted school weight
adj_school_wt = sample_schools %>% mutate(adjusted_scwgt = scwgt*schwgt_adj_factor) %>% mutate(school_id = as.character(school_id))

##Transforming the school sample into long format
long_school_sample = adj_school_wt %>%
  gather(key, value, starts_with("class"), starts_with("cenrol"), starts_with("stpart")) %>%
  separate(key, into = c("var", "index"), sep = "(?<=\\D)(?=\\d)") %>%
  spread(key = var, value = value) %>%
  dplyr::rename(class_id = class) %>%
  dplyr::filter(class_id!=0)%>%mutate(school_id = as.character(school_id))

##Computing number of classes that participated per school
part_classes = long_school_sample %>%  dplyr::filter(stpart>0)%>%
  reframe(part_classes = length(na.omit(class_id)), .by = school_id)%>%
  mutate(school_id = as.character(school_id))

##Computing student prestratification weight = adjusted_scwgt*SCINTV*student_nonresponse_adj_factor*class_adjustment
prestratification_weights =  long_school_sample %>%left_join(part_classes) %>%
  mutate(student_nonresponse_adj_factor = ifelse(stpart>0 & cenrol >0, cenrol/stpart, 1),
         ##Trimming of student_nonresponse_adj_factor weight
         student_nonresponse_adj_factor = ifelse(student_nonresponse_adj_factor>3,3.03030, student_nonresponse_adj_factor),
         class_adjustment = ifelse(selclass>0 & part_classes >0, selclass/part_classes, 1),
         prestrat_wgt = adjusted_scwgt*scintv*student_nonresponse_adj_factor*class_adjustment) %>% 
  dplyr::select(school_id, class_id, prestrat_wgt, scwgt) 

####Adding category to sample_schools
sample_schools = sample_schools %>%
  mutate(num_category = eval(parse(text=paste0('case_when(',
                                               paste0('category =="',unique(category), '"~',1:length(unique(category)), collapse = ','),')'))))


###Assigning strata and psus
#set1 are schools with trim_scwgt!=1
if (nrow(sample_schools %>% dplyr::filter(scwgt!=1))>0)
{
  strata_set1 = sample_schools %>% dplyr::filter(school_part==1)%>% arrange(desc(enrolment)) %>% 
    dplyr::filter(scwgt!=1) %>% group_by(category) %>% dplyr::select(school_id,scwgt,category,num_category) %>%
    mutate(gen_ID = 1:n(), pair_indicator = ifelse(gen_ID %%2 ==1,1,0),pre_stratum = cumsum(pair_indicator)) %>% 
    ungroup %>%
    mutate(stratum = as.numeric(paste0(num_category,pre_stratum)))
  
  ##Determining odd number ID that is not paired
  # max_stratum = strata_set1 %>% reframe(max_st = max(stratum, na.rm = T), max_length=length(max(stratum, na.rm = T)),.by = category)%>%
  #               mutate(recode_max = ifelse(max_st%%2 ==1, max_st-1, max_st))
  max_stratum = strata_set1 %>% reframe(max_st = max(stratum, na.rm = T), max_length=sum(stratum==max(stratum, na.rm = T)),.by = category)%>%
    mutate(recode_max = ifelse(max_length==1, max_st-1, max_st))
  ##
  eval(parse(text=paste0('strata_set1$stratum[strata_set1$stratum == ',max_stratum$max_st,'] = ',max_stratum$recode_max, sep = '\n')))
} else{strata_set1=NA}

#set2 are schools with trim_scwgt==1 or schools selected with certainty
if (nrow(sample_schools %>% dplyr::filter(scwgt==1))>0)
{
  start_num = ifelse(is.null(nrow(strata_set1)),1,1+max(strata_set1$gen_ID, na.rm = T))
  #start_psu = ifelse(is.null(nrow(strata_set1)),1,1+max(as.numeric(strata_set1$school_id), na.rm = T))
  
  strata_set2 = sample_schools %>% dplyr::filter(school_part==1)%>% arrange(desc(enrolment)) %>% 
    dplyr::filter(scwgt==1) %>% group_by(category) %>% dplyr::select(school_id,scwgt,category,num_category) %>%
    mutate(gen_ID = start_num:(n()+start_num-1),pair_indicator=NA,pre_stratum = gen_ID) %>% 
    ungroup()%>% 
    mutate(stratum = as.numeric(paste0(num_category,pre_stratum)))
  #
} else{strata_set2 = NA}

final_set = rbind(strata_set1,strata_set2) %>% dplyr::filter(!is.na(school_id))

###Assigning psus
scwt_not_one_sample_schools = prestratification_weights %>% left_join(final_set) %>%
  dplyr::filter(scwgt!=1) %>%mutate(psu = as.numeric(school_id))
#
psu_start_num = ifelse(nrow(scwt_not_one_sample_schools)>0,1+max(as.numeric(scwt_not_one_sample_schools$psu), na.rm = T),1)
#


scwt_one_sample_schools = prestratification_weights %>% left_join(final_set) %>%
  dplyr::filter(scwgt==1) %>%mutate(psu = ifelse(scwgt==1,psu_start_num:(n()+psu_start_num-1),NA))
####
sample_schools_with_psu_strata = scwt_not_one_sample_schools %>% full_join(scwt_one_sample_schools) %>% 
  mutate(class_id = as.character(class_id), psu = as.numeric(psu))%>% 
  dplyr::select(school_id, class_id, stratum, psu)


##Adding prestratification_wght, psus, and strata to raw_data
raw_data = raw_data %>%left_join(sample_schools_with_psu_strata %>%
                                   dplyr::select(school_id, class_id, stratum, psu) %>%unique()) %>% 
  left_join(prestratification_weights %>%
              dplyr::select(school_id, class_id, prestrat_wgt) %>%
              mutate(class_id = as.character(class_id)))

###Post stratification adjustment
#converting char_SAMPL_GRADE and char_SAMPL_SEX to factor category
raw_data = raw_data %>% mutate(char_SAMPL_GRADE = factor(char_SAMPL_GRADE, levels = LETTERS[1:length(levels(raw_data$CR3))], labels = levels(raw_data$CR3)),
                               char_SAMPL_SEX = factor(char_SAMPL_SEX, levels = LETTERS[1:length(levels(raw_data$CR2))], labels = levels(raw_data$CR2)))

##Computing post stratification factor
#Sample distribution by sex and grade
##Adding category to the dataset frame_schools, sample_schools
raw_data$category = NA
eval(parse(text = paste0('raw_data$category[raw_data$school_id == ',sample_schools$school_id,'] = "', sample_schools$category,'"', sep='\n')))
#################
if(post_weight == 'By grade only')
{
  prewgt_sum_grade = raw_data %>% group_by(category,char_SAMPL_GRADE) %>%reframe( sum(prestrat_wgt, na.rm=T))
  colnames(prewgt_sum_grade) = c('category','grade','sum_wgt')
  prewgt_sum_grade = prewgt_sum_grade %>% mutate(grade = as.character(grade))
  
  #Frame distribution grade
  grade_gender_numbers = grep('boys|girls', names(frame_schools), v=T)
  grade_sustr = toupper(substring(grade_gender_numbers, 1, 1))
  #
  grade_frame = frame_schools %>% dplyr::select(all_of(c(grade_gender_numbers,'category')))
  colnames(grade_frame) = c(grade_sustr,'category')
  #
  class_frame =  grade_frame %>% pivot_longer(cols = all_of(grade_sustr)) %>%
    group_by(category,name)%>% reframe(pop = sum(value, na.rm = T))%>% mutate(category = as.character(category))
  #%>%pivot_wider(names_from = name, values_from = pop)
  colnames(class_frame) = c('category','grade','pop')
  ##
  post_strat_adj =  prewgt_sum_grade %>% left_join(class_frame) %>% 
    reframe(category = category, grade = grade, post_adj = pop/sum_wgt) %>%na.omit()
  
  #Adding postratification factor to the data
  raw_data$post_adj_factor = 1
  eval(parse(text=paste0('raw_data$post_adj_factor[raw_data$char_SAMPL_GRADE=="',post_strat_adj[,2]$grade,'&',
                         '" & raw_data$category =="',post_strat_adj[,1]$category,'"] = ', post_strat_adj[,3]$post_adj, sep= '\n')))
} else if(post_weight == 'By sex only')
{
  prewgt_sum_sex = raw_data %>% group_by(category,char_SAMPL_SEX) %>%reframe( sum(prestrat_wgt, na.rm=T))
  colnames(prewgt_sum_sex) = c('category','sex','sum_wgt')
  prewgt_sum_sex = prewgt_sum_sex %>% mutate(sex = as.character(sex))
  
  #Frame distribution grade
  grade_gender_numbers = grep('boys|girls', names(frame_schools), v=T)
  sex_sustr = substring(grade_gender_numbers, 4, )
  #
  sex_frame = frame_schools %>% dplyr::select(all_of(c(grade_gender_numbers,'category')))
  colnames(sex_frame) = c(sex_sustr,'category')
  #
  sex_frame =  sex_frame %>% pivot_longer(cols = all_of(sex_sustr)) %>%
    group_by(category,name)%>% reframe(pop = sum(value, na.rm = T))%>% mutate(category = as.character(category))
  #%>%pivot_wider(names_from = name, values_from = pop)
  colnames(sex_frame) = c('category','sex','pop')
  ##
  post_strat_adj =  prewgt_sum_sex %>% left_join(sex_frame %>% mutate(sex = case_when(sex =='boys'~'A', sex =='girls'~'B'))) %>% 
    reframe(category = category, sex = sex, post_adj = pop/sum_wgt) %>%na.omit()
  
  #Adding postratification factor to the data
  raw_data$post_adj_factor = 1
  eval(parse(text=paste0('raw_data$post_adj_factor[raw_data$char_SAMPL_SEX=="',post_strat_adj[,2]$sex,'&',
                         '" & raw_data$category =="',post_strat_adj[,1]$category,'"] = ', post_strat_adj[,3]$post_adj, sep= '\n')))
}else if(post_weight == 'None')
{
  raw_data$post_adj_factor = 1
} else{
  prewgt_sum_sex_grade = raw_data %>% group_by(category,char_SAMPL_GRADE, char_SAMPL_SEX) %>%reframe( sum(prestrat_wgt, na.rm=T))
  colnames(prewgt_sum_sex_grade) = c('category','grade','sex','sum_wgt')
  prewgt_sum_sex_grade = prewgt_sum_sex_grade %>% mutate(sex = as.character(sex), grade = as.character(grade))
  
  #Frame distribution by sex and grade
  grade_gender_numbers = grep('boys|girls', names(frame_schools), v=T)
  
  eval(parse(text = paste0('class_sex_frame = frame_schools %>% group_by(category) %>%reframe(',paste0(grade_gender_numbers,'= sum(',grade_gender_numbers,',na.rm=T)', collapse = ','),')')))
  #
  class_sex_frame = class_sex_frame %>% pivot_longer(cols = all_of(grade_gender_numbers)) %>%
    separate(name,c('grade','sex'),'__') %>% 
    mutate(grade = toupper(grade), sex = case_when(sex =='boys'~'A', sex =='girls'~'B'),
           value = ifelse(value ==0, NA, value)) %>%rename(category = category, pop = value) %>%
    mutate(category = as.character(category))
  #
  post_strat_adj =  prewgt_sum_sex_grade %>% left_join(class_sex_frame) %>% 
    reframe(category = category, grade = grade, sex = sex, post_adj = pop/sum_wgt) %>%na.omit()
  
  #Adding postratification factor to the data
  raw_data$post_adj_factor = 1
  eval(parse(text=paste0('raw_data$post_adj_factor[raw_data$char_SAMPL_GRADE=="',post_strat_adj[,2]$grade,'" & raw_data$char_SAMPL_SEX=="',
                         post_strat_adj[,3]$sex,'" & raw_data$category =="',post_strat_adj[,1]$category,'"] = ', post_strat_adj[,4]$post_adj, sep= '\n')))
}
####Post Weighted dataset
if(weighted_reporting=='Yes')
{
  raw_data = raw_data %>%mutate(post_strat_weights = post_adj_factor*prestrat_wgt)%>%
    dplyr::select(-c(SAMPL_SEX,char_SAMPL_SEX,SAMPL_GRADE,char_SAMPL_GRADE))
}else {raw_data = raw_data %>%mutate(post_strat_weights = 1)%>%dplyr::select(-c(SAMPL_SEX,char_SAMPL_SEX,SAMPL_GRADE,char_SAMPL_GRADE))}

####Trimming of poststratification weights
weights_gt_50th_p = quantile(raw_data$post_strat_weights, .50,na.rm = T)
weights_gt_90th_p = quantile(raw_data$post_strat_weights, .90,na.rm = T)
#
raw_data = raw_data %>% mutate(post_strat_weights = ifelse(post_strat_weights>(4*weights_gt_50th_p),weights_gt_90th_p,post_strat_weights))
###Normalising postratification weights
# Calculate the sum of unnormalized weights (this should be approximately Ntotal)
sum_post_strat_weights = sum(raw_data$post_strat_weights, na.rm = T)
Ntotal = nrow(raw_data)
#
raw_data = raw_data %>% mutate(normalised_weights = (post_strat_weights/ sum_post_strat_weights) * Ntotal) 

