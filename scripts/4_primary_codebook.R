# keep same weights from initial processing of data (weights may vary each time data is processed from the start due to post-adjustment weighting process)
# previous_weights = read_excel("weighted_dataset/FILE_NAME.xlsx",'Data version 1')
# previous_weights <- previous_weights[c("weight", "record_id")]
# data_v1 = data_v1 %>% dplyr::select(-weight) %>% left_join(previous_weights)

mapping_matrix = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Matrix')
#######################################################################################
####Langauge Matrix
language_matrix = read_excel(paste0(getwd(),'/scripts/LANGUAGES.xlsx')) %>% as.data.frame()
colnames(language_matrix) = tolower(colnames(language_matrix))
lang_titles = language_matrix[, tolower(language)]

######################################################################################################################################################################
######################################################################################################################################################################
###Assigning site specific variables to standard variables; and also checking if numeric entries were done and these are converted to letters
##Reorganising the mapping matrix to extract content for automated analysis: clearly defining what the variables are and assigning labels to the various response categories
map_dictionary = mapping_matrix %>% dplyr::filter(!is.na(survey_question)) %>% 
  dplyr::select(all_of(c("site", "survey_question","var_levels"))) %>% 
  distinct() %>% rowwise %>% 
  mutate(standard = gsub('\\s','',strsplit(survey_question,':')[[1]][1]), 
         revised_var_levels = paste0('c(',paste0('"',sapply(strsplit(unlist(strsplit(var_levels, split = ";")), split = ":"), function(x)  x[2]),'"', collapse = ','),')'),
         standard = toupper(standard),
         unlabelled_levels = paste0('c(',paste0('"',sapply(strsplit(unlist(strsplit(var_levels, split = ";")), split = ":"), function(x) gsub('\\s','',x[1])),'"', collapse = ','),')'),
         unlabelled_levels = ifelse(is.na(revised_var_levels),NA,unlabelled_levels),site = tolower(site)) 

## Factor conversions for categorical variables ("data_v1" contains original variables, "data" contains labelled variables)
eval(parse(text=paste0('data_v1$',map_dictionary$standard, '= factor(as.character(data_v1$',map_dictionary$standard,'), levels = ',
                       map_dictionary$unlabelled_levels, ', labels = ',map_dictionary$unlabelled_levels,')', sep='\n')))

eval(parse(text=paste0('data$',map_dictionary$standard, '= factor(as.character(data$',map_dictionary$standard,'), levels = ',
                       map_dictionary$unlabelled_levels, ', labels = ',map_dictionary$revised_var_levels,')', sep='\n')))

matrix_for_summary_tables = updated_matrix %>% dplyr::select(bin_standard, site, numerator, indicator_description)%>% dplyr::filter(!is.na(numerator))

new_variables <- gsub('\\s|\t','',matrix_for_summary_tables[,1]$bin_standard) ##binary variables
cond_variables <- matrix_for_summary_tables[,2]$site ##site variables
cond_statements <- matrix_for_summary_tables[,3]$numerator ##numerators
variable_labels <- matrix_for_summary_tables[,4]$indicator_description ##Indicator description

eval(parse(text=paste0('data$',new_variables, '=factor(as.character(data$',new_variables,'),levels=',lang_titles[1],', labels =',lang_titles[1],')', sep='\n')))

# Labeling data
eval(parse(text=paste0('Hmisc::label(data$',map_dictionary$standard,') = "',gsub('\\s+',' ',gsub('^\\s*[^:]*:\\s*','',map_dictionary$survey_question)),'"')))
eval(parse(text=paste0('Hmisc::label(data$',new_variables,') = "', gsub('\\s+',' ',gsub('^\\s*[^:]*:\\s*','',variable_labels)),'"', sep ='\n')))

# min n for GYTS is always 35 records
n_cutoff = 35

# ft_text1 = lang_titles[27]
# ft_text2 = paste0(lang_titles[28],' ',n_cutoff)
######################################################################################################################################################################
######################################################################################################################################################################
svy_data = svydesign(id=~psu, weights=~weight,strata=~stratum, data=data,nest = T)


gen_dictionary_fn = function(std_var ='CR1', ctry_var = 'q1')
{ 
  
  if (any(class(data[, std_var]) == 'factor'))
  {
    formula = make.formula(std_var)
    
    output = data %>% 
            dplyr::reframe(label_names1 = names(table(data_v1[,std_var])),
                             label_names2 = names(table(data[,std_var])),
                             unweighted_freq = table(eval(parse(text = std_var))),
                             weighted_perc = formatC(round(prop.table(svytable(formula,design = svy_data))*100,1),format = 'f', digits = 1)
            ) %>% mutate(unweighted_freq = ifelse(is.na(unweighted_freq),0,unweighted_freq),
                         unweighted_freq = as.character(unweighted_freq), weighted_perc = as.character(weighted_perc)) %>%
            dplyr::bind_rows(data %>% reframe(label_names1='',
                                                label_names2 = eval(parse(text=lang_titles[2])),
                                                unweighted_freq = table(is.na(eval(parse(text = std_var))))['TRUE'],weighted_perc = '')%>%
                               mutate(unweighted_freq = ifelse(is.na(unweighted_freq),0,unweighted_freq),
                                      unweighted_freq = as.character(unweighted_freq), weighted_perc = as.character(weighted_perc)))
    ###Formatting output::
    length_output = nrow(output)
    final_output = bind_rows(c(standard_var= toupper(std_var), country_var='\t',label_names1='\t',label_names2='\t',unweighted_freq='\t',weighted_perc='\t'),
                             c(standard_var= toupper(std_var), country_var=toupper(ctry_var),label_names1=Hmisc::label(data[,std_var]),label_names2='',unweighted_freq='',weighted_perc=''),
                             bind_cols(standard_var=rep(toupper(std_var),length_output),country_var=rep('',length_output),output))
    
  }
  
  else {
    final_output = bind_rows(c(standard_var= toupper(std_var), country_var='\t',label_names1='\t',label_names2='\t',unweighted_freq='\t',weighted_perc='\t'),
                             bind_cols(standard_var= toupper(std_var), country_var=toupper(ctry_var),label_names1=Hmisc::label(data[,std_var]),label_names2='',unweighted_freq='',weighted_perc=''))
  }
  
#
  if(weighted_reporting=='Yes'){
    colnames(final_output) = eval(parse(text=lang_titles[3]))
  }else{
    colnames(final_output) = eval(parse(text=lang_titles[21]))  
  }
  rownames(final_output) = NULL
  return(final_output)
}

####Calling gen_dictionary_fn function to generate a dictionary for the selected variables
standard_variables = gsub('\t','',map_dictionary$standard)
country_variables = gsub('\t','',map_dictionary$site)
#
generated_dictionary = list()

i = NULL

for (i in 1:length(standard_variables))
{
  generated_dictionary[[i]] = gen_dictionary_fn(std_var = standard_variables[i], ctry_var = country_variables[i])
}

generated_dictionary = do.call('rbind', generated_dictionary) %>% as.data.frame()

if(language =='FRENCH')
{
  generated_dictionary[,6] = gsub('\\.',',',generated_dictionary[,6])
}else{}
###Flextable generation
cells_to_NA = setdiff(as.numeric(gsub('V','',rownames(generated_dictionary))),
                             as.numeric(rownames(generated_dictionary)[generated_dictionary[,2]!='' & generated_dictionary[,2]!='\t']))


flex_dictionary = generated_dictionary %>% flextable()%>%
                  flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
                  flextable::bold(part = 'header')%>%
                  hline(i = c(as.numeric(gsub('V','',rownames(generated_dictionary)[generated_dictionary[,4]==eval(parse(text=lang_titles[2]))]))), 
                        border=fp_border(color="gray", style="solid", width=1)) %>% 
                  fontsize(size = 9 ,part = "all")%>%autofit()%>% 
                  merge_h_range(i = c(as.numeric(gsub('V','',rownames(generated_dictionary)[generated_dictionary[,2]!='']))), j1 = 3, j2 = 6)%>%
                  merge_h_range(i = 1, j1 = 3, j2 = 4, part = 'header') %>%
                  flextable::width(j = 3, 0.5, unit = "in")%>%
                  flextable::width(j = 4, 3, unit = "in")%>%
                  flextable::align(j = 5:6, align = 'right', part = 'all') %>%
                  flextable::align(j = 3, align = 'center', part = 'header') %>%
                  flextable::valign(j = 1:6, valign = 'top')%>%
                  paginate(group = colnames(generated_dictionary)[1])%>%padding(padding = 0, part = "all")%>%
                  compose(j = 1, i = cells_to_NA, value = as_paragraph(as_chunk(NA)))

## Printing of Codebook::::
doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/codebook_template.docx'))
#
doc = headers_replace_text_at_bkm(doc,"country",site_name)
doc = headers_replace_text_at_bkm(doc,"year",survey_year)

#
doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(flextable::width(flex_dictionary, width = dim(flex_dictionary)$widths*6.5/(flextable_dim(flex_dictionary)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/Batch Reports/',survey_year,' ' ,site_name,' GYTS Codebook.docx')) 






