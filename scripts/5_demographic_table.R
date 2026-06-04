demographic_table_fn = function()
{ 
  
  ###Distribution by age category
  unwgt_age_freq = cbind(with(data, table(age_cat, useNA = 'ifany')),
                     with(data, table(age_cat, CR2, useNA = 'ifany')))
  unwgt_age_freq[is.na(unwgt_age_freq)]=0
  #
  unwgt_age_perc = rbind(formatC(round(cbind(with(data, prop.table(table(age_cat))),
                     with(data, prop.table(table(age_cat, CR2),2)))*100,1),digits = 1, 
                     format = 'f'),c('-','-','-'))
  #
  wgt_age_perc = rbind(cbind(formatC(round(prop.table(svytable(~age_cat,design = svy_data))*100,1),
                                     format = 'f', digits = 1),
                   formatC(round(prop.table(svytable(~age_cat+CR2,design = svy_data),2)*100,1),
                           format = 'f', digits = 1)),c('-','-','-'))
 ###Distribution by grade
  unwgt_grade_freq = cbind(with(data, table(CR3, useNA = 'ifany')),
                         with(data, table(CR3, CR2, useNA = 'ifany')))
  #
  unwgt_grade_freq[is.na(unwgt_grade_freq)]=0
  #
  unwgt_grade_perc = rbind(formatC(round(cbind(with(data, prop.table(table(CR3))),
                                             with(data, prop.table(table(CR3, CR2),2)))*100,1), 
                                   digits = 1, format = 'f'),c('-','-','-'))
  #
  wgt_grade_perc = rbind(cbind(formatC(round(prop.table(svytable(~CR3,design = svy_data))*100,1),format = 'f', digits = 1),
                             formatC(round(prop.table(svytable(~CR3+CR2,design = svy_data),2)*100,1),
                                     format = 'f', digits = 1)),c('-','-','-'))
  #####
  if (ncol(unwgt_age_freq)==3)
  {
    unwgt_age_freq = unwgt_age_freq %>% as.data.frame() %>% mutate(Missing = 0)
  }else{}
  #
  if (ncol(unwgt_grade_freq)==3)
  {
    unwgt_grade_freq = unwgt_grade_freq %>% as.data.frame() %>% mutate(Missing = 0)
  }else{}
  
  age_length = length(unique(table(data$age_cat)))
  grade_length = length(unique(table(data$CR3)))
  #
  if(nrow(unwgt_age_freq) == age_length)
  {
    unwgt_age_freq = rbind(unwgt_age_freq,c(0,0,0,0))
  }else{}
  #
  if(nrow(unwgt_grade_freq) == grade_length)
  {
    unwgt_grade_freq = rbind(unwgt_grade_freq,c(0,0,0,0))
  }else{}
  
  #####
  total_n = c(nrow(data), with(data,table(CR2, useNA = 'ifany')))
  total_n[4][is.na(total_n[4])]=0
  
if (agerange =="13-17") {
  combined_table = cbind(c('','',eval(parse(text=lang_titles[4])), eval(parse(text=lang_titles[5])),
                           eval(parse(text=lang_titles[6])),eval(parse(text=lang_titles[7])),eval(parse(text=lang_titles[8])),eval(parse(text=lang_titles[9])),
                           eval(parse(text=lang_titles[2])),eval(parse(text=lang_titles[11])), levels(data$CR3),eval(parse(text=lang_titles[2]))),
                         rbind(c(eval(parse(text=lang_titles[4])),'','',eval(parse(text=lang_titles[12])),'','',eval(parse(text=lang_titles[13])),'','',eval(parse(text=lang_titles[2]))),
                               c('N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),
                                 'N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),
                                 'N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),''),
                               c(total_n[1],'','',total_n[2],'','',total_n[3],'','',total_n[4]),
                               c('','','','','','','','','',''),
                               cbind(unwgt_age_freq[,1],unwgt_age_perc[,1],wgt_age_perc[,1],
                                     unwgt_age_freq[,2],unwgt_age_perc[,2],wgt_age_perc[,2],
                                     unwgt_age_freq[,3],unwgt_age_perc[,3],wgt_age_perc[,3],unwgt_age_freq[,4]),
                               rep('',10),
                               cbind(unwgt_grade_freq[,1],unwgt_grade_perc[,1],wgt_grade_perc[,1],
                                     unwgt_grade_freq[,2],unwgt_grade_perc[,2],wgt_grade_perc[,2],
                                     unwgt_grade_freq[,3],unwgt_grade_perc[,3],wgt_grade_perc[,3],unwgt_grade_freq[,4])))
  
}else{
    combined_table = cbind(c('','',eval(parse(text=lang_titles[4])), eval(parse(text=lang_titles[5])),
                           eval(parse(text=lang_titles[6])),eval(parse(text=lang_titles[7])),eval(parse(text=lang_titles[22])),
                           eval(parse(text=lang_titles[2])),eval(parse(text=lang_titles[11])), levels(data$CR3),eval(parse(text=lang_titles[2]))),
                         rbind(c(eval(parse(text=lang_titles[4])),'','',eval(parse(text=lang_titles[12])),'','',eval(parse(text=lang_titles[13])),'','',eval(parse(text=lang_titles[2]))),
                               c('N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),
                                 'N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),
                                 'N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),''),
                               c(total_n[1],'','',total_n[2],'','',total_n[3],'','',total_n[4]),
                               c('','','','','','','','','',''),
                               cbind(unwgt_age_freq[,1],unwgt_age_perc[,1],wgt_age_perc[,1],
                                     unwgt_age_freq[,2],unwgt_age_perc[,2],wgt_age_perc[,2],
                                     unwgt_age_freq[,3],unwgt_age_perc[,3],wgt_age_perc[,3],unwgt_age_freq[,4]),
                               rep('',10),
                               cbind(unwgt_grade_freq[,1],unwgt_grade_perc[,1],wgt_grade_perc[,1],
                                     unwgt_grade_freq[,2],unwgt_grade_perc[,2],wgt_grade_perc[,2],
                                     unwgt_grade_freq[,3],unwgt_grade_perc[,3],wgt_grade_perc[,3],unwgt_grade_freq[,4])))
  }
  
  colnames(combined_table) =c('',eval(parse(text=lang_titles[4])),'','',eval(parse(text=lang_titles[12])),'','',eval(parse(text=lang_titles[13])),'','',eval(parse(text=lang_titles[2])))
  
  if(weighted_reporting=='No'){
    combined_table <- combined_table[, -c(4, 7, 10)]
  }else{}
  
  ###########
  rownames(combined_table)=NULL
  if (language =='FRENCH')
  {
    combined_table = gsub('\\.',',',combined_table)
  } else{}
 
  if (agerange =="13-17") {
    rows_to_bold <- c(1:2, 4, 10)
  }else{
    rows_to_bold <- c(1:2, 4, 9)
  }
  
  if(weighted_reporting=='No'){
    flex_table_output = combined_table %>% as.data.frame() %>% flextable() %>% autofit() %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>% 
      delete_part(part = "header") %>% flextable::bold(i = rows_to_bold)%>%
      bg(bg="white",i=1,part="header")%>%  
      hline_top(border = fp_border_default(width = 0),part = "header")%>%
      vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
      vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
      flextable::align(align = "center", j = 2:8, part = "all") %>%
      line_spacing(i = 3:nrow(combined_table), space = 1.5, part = "body") %>%
      flextable::width(j = 1:8, 2.8, unit = "in")%>%
      fontsize(size = 8,part = "all")%>%padding(padding = 0, part = "all")
  }else{
    flex_table_output = combined_table %>% as.data.frame() %>% flextable() %>% autofit() %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>% 
      delete_part(part = "header") %>% flextable::bold(i = rows_to_bold)%>%
      bg(bg="white",i=1,part="header")%>%  
      hline_top(border = fp_border_default(width = 0),part = "header")%>%
      vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
      vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
      flextable::align(align = "center", j = 2:11, part = "all") %>%
      line_spacing(i = 3:nrow(combined_table), space = 1.5, part = "body") %>%
      flextable::width(j = 1:11, 2.8, unit = "in")%>%
      fontsize(size = 8,part = "all")%>%padding(padding = 0, part = "all")
  }
  #
  return(flex_table_output)
}

demographic_table = demographic_table_fn()
## Printing of Demographic Table::::
if(weighted_reporting=='Yes'){
  doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/demographic_table_template.docx'))
}else{
  doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/demographic_table_template.docx'))
}
#
doc = headers_replace_text_at_bkm(doc,"country",site_name)
doc = headers_replace_text_at_bkm(doc,"year",survey_year)

#
doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(flextable::width(demographic_table, width = dim(demographic_table)$widths*10/(flextable_dim(demographic_table)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/Batch Reports/',survey_year,' ' ,site_name,' GYTS Demographic Table.docx')) 
