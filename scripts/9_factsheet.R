###
# uncomment these lines if you wish to run fact sheet independently after already cleaning and weighting data
# sample_schools = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Sample')
# updated_matrix = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Matrix')
# map_dictionary = updated_matrix %>% dplyr::filter(!is.na(survey_question)) %>% 
#   dplyr::select(all_of(c("site", "survey_question","var_levels"))) %>% 
#   distinct() %>% rowwise %>% 
#   mutate(standard = gsub('\\s','',strsplit(survey_question,':')[[1]][1]), 
#          revised_var_levels = c(gsub('.:','',strsplit(var_levels, "[;]"))), 
#          standard = toupper(standard),
#          unlabelled_levels = paste0('c(',paste0('"',sapply(strsplit(unlist(strsplit(var_levels, split = ";")), split = ":"), function(x) gsub('\\s','',x[1])),'"', collapse = ','),')'),
#          unlabelled_levels = ifelse(is.na(revised_var_levels),NA,unlabelled_levels),site = tolower(site))
# 
# map_categorical = map_dictionary
# data = read_excel(paste0(getwd(),'/weighted_dataset/NAME OF WEIGHTED DATASET.xlsx'),'Data version 1') # put name of weighted dataset here
# eval(parse(text=paste0('data$',map_categorical$standard, '= factor(as.character(data$',map_categorical$standard,'), levels = ',map_categorical$unlabelled_levels, ')', sep='\n')))
# svy_data = svydesign(id=~psu, weights=~weight,strata=~stratum, data=data,nest = T)
# language_matrix = read_excel(paste0(getwd(),'/scripts/LANGUAGES.xlsx')) %>% as.data.frame()
# colnames(language_matrix) = tolower(colnames(language_matrix))
# lang_titles = language_matrix[, tolower(language)]
# matrix_for_summary_tables = updated_matrix %>% dplyr::select(bin_standard, site, numerator, indicator_description)%>% dplyr::filter(!is.na(numerator))
# variable_labels <- matrix_for_summary_tables[,4]$indicator_description ##Indicator description
# new_variables <- gsub('\\s|\t','',matrix_for_summary_tables[,1]$bin_standard) ##binary variables
# eval(parse(text=paste0('Hmisc::label(data$',new_variables,') = "', gsub('\\s+',' ',gsub('^\\s*[^:]*:\\s*','',variable_labels)),'"', sep ='\n')))
###


need_size_footnote <- 0
colnames(sample_schools) = tolower(colnames(sample_schools))
##
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

##########Fact sheet 
if(language =='FRENCH' | language == 'SPANISH')
{
  factsheet_sections = updated_matrix %>% dplyr::filter(!is.na(factsheet_section))%>%
    dplyr::select(bin_standard, factsheet_section,factsheet_subtitle,fact_sheet_order,footnote_number,footnotes) %>% distinct() %>%
    rename(sub_title = factsheet_subtitle) 
} else{
  factsheet_sections = updated_matrix %>% dplyr::filter(!is.na(factsheet_section))%>%
    dplyr::select(bin_standard, factsheet_section,factsheet_subtitle,fact_sheet_order,footnote_number,footnotes) %>% distinct() %>%
    rename(sub_title = factsheet_subtitle) %>%
    mutate(sub_title = gsub('And','and',str_to_sentence(gsub('\\s+',' ',sub_title))))
}

if (agerange =="13-17") {
log_conditions = c("(age_cat=='13 - 15'| age_cat=='16 or 17')",
                   paste0("(age_cat=='13 - 15'| age_cat=='16 or 17') & CR2 =='",levels(data$CR2)[1],"'"),
                   paste0("(age_cat=='13 - 15'| age_cat=='16 or 17') & CR2 =='",levels(data$CR2)[2],"'"))

}else{
log_conditions = c("(age_cat=='13 - 15')",
                   paste0("(age_cat=='13 - 15') & CR2 =='",levels(data$CR2)[1],"'"),
                   paste0("(age_cat=='13 - 15') & CR2 =='",levels(data$CR2)[2],"'"))
}

degrees_freedom = degf(svy_data)-1

fact_sheet_fn = function(section = factsheet_sections$factsheet_section[1])
{
  i = NULL
  #subtitle_results = NULL
  section_matrix = factsheet_sections %>% dplyr::filter(factsheet_section == section)
  all_subtitle_results = NULL
  section_results = NULL
  for(i in unique(section_matrix$sub_title))
  {
    subtitle_results = NULL
    var_group = (factsheet_sections %>% dplyr::filter(sub_title == i))$bin_standard
    #all_var_grp_results = NULL
    j = NULL
    
    for(j in var_group)
    {
      outputs = c()
      k = NULL
      
      for (k in log_conditions)
      {
        num_var = eval(parse(text=lang_titles[1]))[1]
        
        formula = make.formula(paste0(j, '=="',num_var,'"'))
        est_ciprop = svyciprop(formula, design=subset(svy_data,eval(parse(text=k))), method="lo", df = degrees_freedom)
        # the proportion and confidence interval (the latter only if weighted analysis)
        if (weighted_reporting=='Yes'){
          est_ci = paste0(formatC(round(as.vector(est_ciprop)*100,1),format = 'f', digits = 1), '\n(',
                          formatC(round((as.numeric(attr(est_ciprop, "ci")[1]))*100,1),format = 'f', digits = 1),' - ',
                          formatC(round(100*as.numeric(attr(est_ciprop, "ci")[2]),1),format = 'f', digits = 1),')')
        }else{
          est_ci = paste0(formatC(round(as.vector(est_ciprop)*100,1),format = 'f', digits = 1))
        }
        n_participants = data %>% filter(eval(parse(text=paste0('!is.na(',j,') & (',k,')')))) %>% reframe(n()) %>% as.numeric()
        est_ci = ifelse(n_participants>=35,est_ci,'-')
        need_size_footnote <<- ifelse(n_participants<35,1,need_size_footnote)
        outputs =c(outputs,est_ci)
      }
      
      footnote_value <- subset(factsheet_sections, bin_standard == j)$footnote_number
      
      if (!is.na(footnote_value)) {
        #footnote_fpar <- fpar(ftext(footnote_value, prop = fp_text(font.size = 10, vertical.align = "superscript")))
        #var_output <- c(paste0(Hmisc::label(data[,i]), footnote_fpar), outputs)
        var_output <- c(paste0(Hmisc::label(data[,j]), footnote_value), outputs)
      } else {
        var_output = c(Hmisc::label(data[,j]), outputs)
      }
      
      subtitle_results = rbind(subtitle_results,var_output)
      rownames(subtitle_results)=NULL
    }
    
    all_subtitle_results = rbind(i,subtitle_results)
    section_results = rbind(section_results, all_subtitle_results)
    rownames(section_results)=NULL
  }
  
  ##Adding subtitle
  if(length(unique(section_matrix$sub_title))>1)
  {
    final_section_results = rbind(rep(section,3), section_results)
  }else{
    final_section_results = section_results
  }
  return(final_section_results)                        
}


###Calling fact_sheet_fn function
i = NULL
full_fact_sheet = NULL

factsheet_sections <- factsheet_sections[order(factsheet_sections$fact_sheet_order), ]

for(i in unique(factsheet_sections$factsheet_section))
    
{
  fact_subtable = fact_sheet_fn(section = i)
  full_fact_sheet = rbind(full_fact_sheet, fact_subtable)
}

rev_full_fact_sheet = full_fact_sheet%>% as.data.frame()
colnames(rev_full_fact_sheet) = paste0('col',1:4)
#Obtaining row numbers for sections and subtitles
section_names = unique(factsheet_sections$factsheet_section)
sub_title_names = unique(factsheet_sections$sub_title)

section_rows = which(rev_full_fact_sheet$col1 %in% section_names)
subtitle_rows = setdiff(which(rev_full_fact_sheet$col1 %in% sub_title_names),section_rows)

###
if(language =='FRENCH')
{
  rev_full_fact_sheet = eval(parse(text=paste0('cbind(',paste0(gsub('\\.',',',rev_full_fact_sheet), collapse =','),')'))) %>% as.data.frame()
  #
  
}else{}

if (agerange =="13-17") {
  colnames(rev_full_fact_sheet) = eval(parse(text = lang_titles[27]))
}else{
  colnames(rev_full_fact_sheet) = eval(parse(text = lang_titles[19]))
}

#
flex_fact_sheet = rev_full_fact_sheet %>% flextable() %>% autofit() %>%
  flextable::style(pr_t=fp_text(font.size=10,font.family='Source Sans Pro'), part = 'all')%>%
  flextable::bold(i = section_rows)%>%
  flextable::italic(i = subtitle_rows)%>%
  bg(bg="#FCBC71",i=section_rows,part="body")%>%  
  bg(bg="#FFE3C2",i=subtitle_rows,part="body")%>%  
  theme_box()%>% 
  flextable::align(align = "center", j = 2:4, part = "all") %>%
  fontsize(size = 11 ,part = "all")%>%
  merge_h_range(i=c(section_rows,subtitle_rows), j1=1,j2=4)%>%
  flextable::width(j = 2:4, 4.3, unit = "in")%>% 
  bg(bg="#FEECE0",i=1,part="header")%>%
  #color(i = c(section_rows,subtitle_rows), color = "white")%>%
  # bg(bg="#FCBC71",i=subtitle_nrows,part="body")%>%
  padding(padding = 0, part = "all") %>%
  paginate()


##Generating Factsheet::::
if(is_this_census=='No')
{
  if(weighted_reporting=='Yes'){
    doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/fact_sheet_template.docx'))
  }else{
    doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/fact_sheet_template.docx'))
  }
}else{
  if(weighted_reporting=='Yes'){
    doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/census_fact_sheet_template.docx'))
  }else{
    doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/census_fact_sheet_template.docx'))
  }
}

doc = headers_replace_text_at_bkm(doc,"country",site_name)
doc = headers_replace_text_at_bkm(doc,"year",survey_year)

and_text <- gsub("c\\(|\\)", "", lang_titles[26])
and_text <- gsub("'", "", and_text)

if (agerange =="13-17") {
  total_students_in_targeted_agerange <- sum(data$age_cat %in% c("13 - 15", "16 or 17"))
  upper_agerange="17"
}else{
  total_students_in_targeted_agerange <- sum(data$age_cat %in% c("13 - 15"))
  upper_agerange="15"
}

#Adding other text
if(is_this_census == 'No') {
    additional_text = c(survey_year,
                          paste0(levels(data$CR3)[1],'–',levels(data$CR3)[length(levels(data$CR3))]),
                          paste0(levels(data$CR3)[1],'–',levels(data$CR3)[length(levels(data$CR3))]),
                          site_name,
                          sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
                          site_name,
                          paste0(paste0(tolower(unique(factsheet_sections$sub_title)[-length(unique(factsheet_sections$sub_title))]), collapse = ', '),
                                 ', ', and_text, ' ',tolower(unique(factsheet_sections$sub_title)[length(unique(factsheet_sections$sub_title))])),
                          paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),
                          paste0(formatC(round(100*(nrow(data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                          paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T))*(nrow(data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                          nrow(data),
                          site_name,
                          site_name,
                          total_students_in_targeted_agerange,
                          upper_agerange,
                          upper_agerange)
}else{
      additional_text = c(survey_year,
                          paste0(levels(data$CR3)[1],'–',levels(data$CR3)[length(levels(data$CR3))]),
                          paste0(levels(data$CR3)[1],'–',levels(data$CR3)[length(levels(data$CR3))]),
                          site_name,
                          site_name,
                          paste0(paste0(tolower(unique(factsheet_sections$sub_title)[-length(unique(factsheet_sections$sub_title))]), collapse = ', '),
                                 ', ', and_text, ' ',tolower(unique(factsheet_sections$sub_title)[length(unique(factsheet_sections$sub_title))])),
                          paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),
                          paste0(formatC(round(100*(nrow(data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                          paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T))*(nrow(data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                          nrow(data),
                          site_name,
                          site_name,
                          total_students_in_targeted_agerange,
                          upper_agerange,
                          upper_agerange)
}

if (need_size_footnote==1) {
  additional_text <- c(additional_text, lang_titles[30]) 
} else {
  additional_text <- c(additional_text, "") 
}

# generate combo text of all footnotes
# with_footnotes <- factsheet_sections %>%
#   filter(!is.na(footnote_number))
# 
# footnote_string <- paste0(with_footnotes$footnote_number, ". ", with_footnotes$footnotes, collapse = "  ")

#additional_text <- c(additional_text, footnote_string)


bmks = paste0('bmk', 1:length(additional_text))
eval(parse(text=paste0('doc = body_replace_text_at_bkm(doc,"', bmks,'","', additional_text,'")', sep='\n')))

#
doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(flextable::width(flex_fact_sheet, width = dim(flex_fact_sheet)$widths*7.25/(flextable_dim(flex_fact_sheet)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/Batch Reports/',survey_year,' ' ,site_name,' GYTS Fact Sheet.docx')) 
