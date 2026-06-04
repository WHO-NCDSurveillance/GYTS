######Generating codebook for binary variables
binary_dictionary = function(std_var ='CR5', ctry_var = '')
{ 
  
  if (any(class(data[, std_var]) == 'factor'))
  {
    formula = make.formula(std_var)
    
    output = data %>% 
      dplyr::reframe(label_names1 = c("",""),
                       label_names2 = names(table(data[,std_var])),
                       unweighted_freq = table(eval(parse(text = std_var))),
                       weighted_perc = formatC(round(prop.table(svytable(formula,design = svy_data))*100,1),format = 'f', digits = 1)
      ) %>% mutate(unweighted_freq = as.character(unweighted_freq), weighted_perc = as.character(weighted_perc)) %>%
      dplyr::bind_rows(data %>% reframe(label_names1='',
                                          label_names2 = eval(parse(text=lang_titles[2])),
                                          unweighted_freq = table(is.na(eval(parse(text = std_var))))['TRUE'],weighted_perc = '')%>%
                         mutate(unweighted_freq = as.character(unweighted_freq), weighted_perc = as.character(weighted_perc)))
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
  ##
  if(weighted_reporting=='Yes'){
    colnames(final_output) = eval(parse(text=lang_titles[3]))
  }else{
    colnames(final_output) = eval(parse(text=lang_titles[21]))  
  }
  #
  rownames(final_output) = NULL
  return(final_output)
}

#####
generated_binary_dictionary = list()

i = NULL

for (i in 1:length(new_variables))
{
  generated_binary_dictionary[[i]] = binary_dictionary(std_var = gsub('\t| ','',new_variables[i]))
}

generated_binary_dictionary = do.call('rbind', generated_binary_dictionary) %>% as.data.frame()
##Excluding country variable name
generated_binary_dictionary=generated_binary_dictionary[,-2]
###Flextable generation
binary_cells_to_NA = setdiff(as.numeric(gsub('V','',rownames(generated_binary_dictionary))),
                      as.numeric(rownames(generated_binary_dictionary)[generated_binary_dictionary[,3]=='']))
#
if(language =='FRENCH')
{
  generated_binary_dictionary[,5] = gsub('\\.',',',generated_binary_dictionary[,5])
}else{}

##
flex_binary_dictionary = generated_binary_dictionary %>% flextable()%>%
                          flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
                          flextable::bold(part = 'header')%>%
                          hline(i = c(as.numeric(gsub('V','',rownames(generated_binary_dictionary)[generated_binary_dictionary[,3]==eval(parse(text=lang_titles[2]))]))), 
                          border=fp_border(color="gray", style="solid", width=1)) %>% 
                          fontsize(size = 9 ,part = "all")%>%autofit()%>% 
                          merge_h_range(i = c(as.numeric(gsub('V','',rownames(generated_binary_dictionary)[generated_binary_dictionary[,3]=='']))), j1 = 2, j2 = 5)%>%
                          merge_h_range(i = 1, j1 = 2, j2 = 3, part = 'header') %>%
                          flextable::width(j = 2, 0.5, unit = "in")%>%
                          flextable::width(j = 3, 3, unit = "in")%>%
                          flextable::align(j = 4:5, align = 'right', part = 'all') %>%
                          flextable::align(j = 2, align = 'center', part = 'header') %>%
                          flextable::valign(j = 1:5, valign = 'top')%>%
                          paginate(group = colnames(generated_binary_dictionary)[1])%>%padding(padding = 0, part = "all")%>%
                          compose(j = 1, i = binary_cells_to_NA, value = as_paragraph(as_chunk(NA)))


###
## Printing of Codebook::::
doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/codebook_template.docx'))
#
doc = headers_replace_text_at_bkm(doc,"country",site_name)
doc = headers_replace_text_at_bkm(doc,"year",survey_year)

doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(flextable::width(flex_binary_dictionary, width = dim(flex_binary_dictionary)$widths*6.5/(flextable_dim(flex_binary_dictionary)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/Batch Reports/',survey_year,' ' ,site_name,' GYTS Binary_Codebook.docx')) 

