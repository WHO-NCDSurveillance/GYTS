degrees_freedom = degf(svy_data)-1
####
summary_table_fn = function(variable = 'CR5')
{ 
  num_var = eval(parse(text=lang_titles[1]))[1]
  
  eval(parse(text=paste0('formula = ~I(',variable, '=="',num_var,'")')))
  #
  cond1 = paste0('!is.na(',variable,')')
  
  ciprop_function = function(cond_subset = 'CR2==""')
  {
    #Number of participants and estimated ci
    if (cond_subset!='')
    {
      n_participants = data %>% filter(eval(parse(text=paste0(cond_subset,'&', cond1)))) %>% reframe(n()) %>% as.numeric()
      
      if (n_participants>n_cutoff)
      {
        est_ciprop = svyciprop(formula, design=subset(svy_data,eval(parse(text=cond_subset))), method="lo", df = degrees_freedom)
      }
      else
      {
        est_ciprop = '-'
      }
    }
    else
    {
      n_participants = data %>% filter(eval(parse(text=cond1))) %>% reframe(n()) %>% as.numeric()
      if (n_participants>n_cutoff)
      {
        est_ciprop =svyciprop(formula, design=subset(svy_data,eval(parse(text=cond1))), method="lo", df = degrees_freedom)
      }
      else
      {
        est_ciprop = '-'
      }
    }
    
    #the proportion
    total_est = ifelse(n_participants>n_cutoff,formatC(round(as.vector(est_ciprop)*100,1),format = 'f', digits = 1),'-')
    # the confidence interval
    est_ci = ifelse(n_participants>n_cutoff,paste0('(',formatC(round((as.numeric(attr(est_ciprop, "ci")[1]))*100,1),format = 'f', digits = 1),' - ',formatC(round(100*as.numeric(attr(est_ciprop, "ci")[2]),1),format = 'f', digits = 1),')'),'-')
    ###
    if(weighted_reporting=='Yes'){
      result = bind_cols(total_est, est_ci, n_participants)%>%data.frame()
      colnames(result) = c('Percent','CI','N')
    }else{
      result = bind_cols(total_est, n_participants)%>%data.frame()  
      colnames(result) = c('Percent','N')
    }
    
    return(result)
  }
  
  ###Calling ciprop_function
  age_cat_levels = paste0("age_cat","== '",levels(data$age_cat),"'")
  sex_cat_levels = paste0("CR2","== '",levels(data$CR2),"'")##Sex
  class_cat_levels = paste0("CR3","== '",levels(data$CR3),"'")###Class/grade
  ##
  male_age_cat_levels = paste0("CR2","== '",levels(data$CR2)[1],"'",' & ',"age_cat","== '",levels(data$age_cat),"'")
  female_age_cat_levels =  paste0("CR2","== '",levels(data$CR2)[2],"'", ' & ',"age_cat","== '",levels(data$age_cat),"'")
  ##
  male_class_cat_levels = paste0("CR2","== '",levels(data$CR2)[1],"'",' & ',"CR3","== '",levels(data$CR3),"'")
  female_class_cat_levels = paste0("CR2","== '",levels(data$CR2)[2],"'", ' & ',"CR3","== '",levels(data$CR3),"'")
  #####
  #class level conditions
  class_conditions = NULL
  
  k = NULL
  
  for(k in 1:length(levels(data$CR3)))
  {
    
    class_level_cond = c(class_cat_levels[k],male_class_cat_levels[k],female_class_cat_levels[k])
    class_conditions = c(class_conditions,class_level_cond)
    
  }
  
  if (agerange =="13-17") {
    all_conditions = c('',sex_cat_levels,
                       age_cat_levels[1],male_age_cat_levels[1],female_age_cat_levels[1],
                       age_cat_levels[2],male_age_cat_levels[2],female_age_cat_levels[2],
                       age_cat_levels[3],male_age_cat_levels[3],female_age_cat_levels[3],
                       '(age_cat=="13 - 15"|age_cat=="16 or 17")',paste0('CR2 ==','"', levels(data$CR2)[1],'" & (age_cat=="13 - 15"|age_cat=="16 or 17")'),
                       paste0('CR2 ==','"', levels(data$CR2)[2],'" & (age_cat=="13 - 15"|age_cat=="16 or 17")'),
                       age_cat_levels[4],male_age_cat_levels[4],female_age_cat_levels[4],
                       class_conditions)
    list_point_plus_ci_est <- lapply(all_conditions, ciprop_function)
    all_point_plus_ci_est = do.call('cbind',list_point_plus_ci_est)
    #####All possible ranges
    lower_limits = c()
    for(x in 1:length(all_point_plus_ci_est)){
      if(weighted_reporting=='Yes'){
        if(x%%9==0 & (x >45 & x < length(all_point_plus_ci_est))){
          lower_limits =c(lower_limits,x+1)
        }else{}
      }else{
        if(x%%6==0 & (x >30 & x < length(all_point_plus_ci_est))){
          lower_limits =c(lower_limits,x+1)  
        }else{}}
    }
  }else{
    all_conditions = c('',sex_cat_levels,
                       age_cat_levels[1],male_age_cat_levels[1],female_age_cat_levels[1],
                       age_cat_levels[2],male_age_cat_levels[2],female_age_cat_levels[2],
                       age_cat_levels[3],male_age_cat_levels[3],female_age_cat_levels[3],
                       class_conditions)
    list_point_plus_ci_est <- lapply(all_conditions, ciprop_function)
    all_point_plus_ci_est = do.call('cbind',list_point_plus_ci_est)
    #####All possible ranges
    lower_limits = c()
    for(x in 1:length(all_point_plus_ci_est)){
      if(weighted_reporting=='Yes'){
        if(x%%9==0 & (x >27 & x < length(all_point_plus_ci_est))){
          lower_limits =c(lower_limits,x+1)
        }else{}
      }else{
        if(x%%6==0 & (x >18 & x < length(all_point_plus_ci_est))){
          lower_limits =c(lower_limits,x+1)  
        }else{}}
    }
  }
  
  #cl = makeCluster(num_cores)
  #list_point_plus_ci_est <- parallel::mclapply(all_conditions, ciprop_function, mc.cores = num_cores,mc.preschedule = FALSE)
  # stopCluster(cl)
  #list_point_plus_ci_est <- future.apply::future_lapply(all_conditions, ciprop_function,future.seed=TRUE)
  
  ###class labels
  if(weighted_reporting=='Yes'){
    eval(parse(text=paste0('class_tab =rbind(',paste0('c("',levels(data$CR3),
                                                      '",all_point_plus_ci_est[',
                                                      lower_limits,':',lower_limits+8,'])', collapse = ','),')')))
  }else{
    eval(parse(text=paste0('class_tab =rbind(',paste0('c("',levels(data$CR3),
                                                      '",all_point_plus_ci_est[',
                                                      lower_limits,':',lower_limits+5,'])', collapse = ','),')')))
  }
  ####text outputs
    
  if (agerange =="13-17") {
    text_output =eval(parse(text = lang_titles[18]))
    if(weighted_reporting=='Yes'){
      
      output_table = rbind(c(text_output[1], all_point_plus_ci_est[1:9]),
                           c(text_output[2], rep('',9)),
                           c(text_output[3], all_point_plus_ci_est[10:18]),
                           c(text_output[4], all_point_plus_ci_est[19:27]),
                           c(text_output[5], all_point_plus_ci_est[28:36]),
                           c(text_output[6], all_point_plus_ci_est[37:45]),
                           c(text_output[7], all_point_plus_ci_est[46:54]),
                           c(text_output[8], rep('',9)),
                           class_tab
      )
    }else{
      output_table = rbind(c(text_output[1], all_point_plus_ci_est[1:6]),
                           c(text_output[2], rep('',6)),
                           c(text_output[3], all_point_plus_ci_est[7:12]),
                           c(text_output[4], all_point_plus_ci_est[13:18]),
                           c(text_output[5], all_point_plus_ci_est[19:24]),
                           c(text_output[6], all_point_plus_ci_est[25:30]),
                           c(text_output[7], all_point_plus_ci_est[31:36]),
                           c(text_output[8], rep('',6)),
                           class_tab
      )
    }
    
  } else {
    text_output =eval(parse(text = lang_titles[25]))  
    if(weighted_reporting=='Yes'){
      
      output_table = rbind(c(text_output[1], all_point_plus_ci_est[1:9]),
                           c(text_output[2], rep('',9)),
                           c(text_output[3], all_point_plus_ci_est[10:18]),
                           c(text_output[4], all_point_plus_ci_est[19:27]),
                           c(text_output[5], all_point_plus_ci_est[28:36]),
                           c(text_output[6], rep('',9)),
                           class_tab
      )
    }else{
      output_table = rbind(c(text_output[1], all_point_plus_ci_est[1:6]),
                           c(text_output[2], rep('',6)),
                           c(text_output[3], all_point_plus_ci_est[7:12]),
                           c(text_output[4], all_point_plus_ci_est[13:18]),
                           c(text_output[5], all_point_plus_ci_est[19:24]),
                           c(text_output[6], rep('',6)),
                           class_tab
      )  
    }
  }

  #####Creating flextable
  tab_names = colnames(output_table)
  table_tile = paste0(variable,': ',Hmisc::label(eval(parse(text=paste0('data$',variable)))))

  if (agerange =="13-17") {
      if(weighted_reporting=='Yes'){
        res_table_obj = rbind(c('','',text_output[9],''
                                ,'',text_output[10],''
                                ,'',text_output[11],''),
                              c('',text_output[12],text_output[13],'N'
                                ,text_output[12],text_output[13],'N'
                                ,text_output[12],text_output[13],'N'),
                              output_table) %>% as.data.frame()
      }else{
        res_table_obj = rbind(c('','',text_output[9]
                                ,'',text_output[10]
                                ,'',text_output[11]),
                              c('',text_output[12],'N'
                                ,text_output[12],'N'
                                ,text_output[12],'N'),
                              output_table) %>% as.data.frame()
      }
  }else{
      if(weighted_reporting=='Yes'){
      res_table_obj = rbind(c('','',text_output[7],''
                              ,'',text_output[8],''
                              ,'',text_output[9],''),
                            c('',text_output[10],text_output[11],'N'
                              ,text_output[10],text_output[11],'N'
                              ,text_output[10],text_output[11],'N'),
                            output_table) %>% as.data.frame()
    }else{
      res_table_obj = rbind(c('','',text_output[7]
                              ,'',text_output[8]
                              ,'',text_output[9]),
                            c('',text_output[10],'N'
                              ,text_output[10],'N'
                              ,text_output[10],'N'),
                            output_table) %>% as.data.frame()
    }
  }
  
  res_table_obj = res_table_obj %>%mutate(across(everything(), as.character))
  #
  if(language =='FRENCH')
  {
    res_table_obj = eval(parse(text=paste0('cbind(',paste0(gsub('\\.',',',res_table_obj), collapse =','),')'))) %>% as.data.frame()
  }else{}
  #
  
  if (agerange =="13-17") {
    rows_to_bold <- c(1:2, 4, 10)
    }else{
    rows_to_bold <- c(1:2, 4, 8)
  }
  
  
  if(weighted_reporting=='Yes'){
    table_output = res_table_obj %>% flextable() %>% autofit() %>%
      delete_part(part = "header") %>% 
      add_header_lines(table_tile)%>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      flextable::bold(i = rows_to_bold)%>%
      bg(bg="white",i=1,part="header")%>%  
      hline_top(border = fp_border_default(width = 0),part = "header")%>%
      vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
      vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
      flextable::align(align = "center", j = 2:10, part = "all") %>%
      flextable::width(j = 1:10, 3, unit = "in")%>%
      fontsize(size = 9 ,part = "all")
  }else{
    table_output = res_table_obj %>% flextable() %>% autofit() %>%
      delete_part(part = "header") %>% 
      add_header_lines(table_tile)%>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      flextable::bold(i = rows_to_bold)%>%
      bg(bg="white",i=1,part="header")%>%  
      hline_top(border = fp_border_default(width = 0),part = "header")%>%
      vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
      vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
      flextable::align(align = "center", j = 2:7, part = "all") %>%
      flextable::width(j = 1:7, 3, unit = "in")%>%
      fontsize(size = 9 ,part = "all")
  }
  return(table_output)
}

#####Calling summary_table_fn function
# TRUNCATE FOR TESTING PURPOSES
#new_variables <- new_variables[1:5]
all_summary_tables = lapply(new_variables, summary_table_fn)

# Function to add a Flextable with a break at the end
i = NULL
for (i in 1:length(new_variables)) {
  
  if(weighted_reporting=='Yes'){
    my_doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/Table_summary_template.docx'))
  }else{
    my_doc = officer::read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/Table_summary_template.docx'))  
  }
  
  summary_tables = all_summary_tables[[i]]
  
  if (i < length(new_variables))
  {
    if(weighted_reporting=='Yes'){
      my_doc = my_doc %>%
        body_add_flextable(flextable::width(summary_tables, width = dim(summary_tables)$widths*10/(flextable_dim(summary_tables)$widths)),pos = 'on') %>%
        body_add_break()
    }else{
      my_doc = my_doc %>%
        body_add_flextable(flextable::width(summary_tables, width = dim(summary_tables)$widths*7/(flextable_dim(summary_tables)$widths)),pos = 'on') %>%
        body_add_break() 
    }
  }
  else
  {
    if(weighted_reporting=='Yes'){
      my_doc = my_doc %>%
        body_add_flextable(flextable::width(summary_tables, width = dim(summary_tables)$widths*10/(flextable_dim(summary_tables)$widths)),pos = 'on')
    }else{
      my_doc = my_doc %>%
        body_add_flextable(flextable::width(summary_tables, width = dim(summary_tables)$widths*7/(flextable_dim(summary_tables)$widths)),pos = 'on') 
    }
  }
  #
  print(my_doc,target=paste0(getwd(),'/temp_tables/tempsum',i,'.docx'))
}


if(weighted_reporting=='Yes'){
  combined_sum_doc <<- officer::read_docx(paste0(getwd(),'/templates/',language,'/Table_summary_template.docx'))
}else{
  combined_sum_doc <<- officer::read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/Table_summary_template.docx'))
}
combined_sum_doc <<- officer::headers_replace_text_at_bkm(combined_sum_doc,"country",site_name)
combined_sum_doc <<- officer::headers_replace_text_at_bkm(combined_sum_doc,"year",survey_year)

i=NULL
for(i in 1:length(new_variables)){
  path <- paste0(getwd(),'/temp_tables/tempsum',i,'.docx')
  combined_sum_doc <<- body_add_docx(combined_sum_doc, path, pos = "after") 
}

# print combine doc

print(combined_sum_doc,target=paste0(getwd(),'/Batch Reports/',survey_year,' ' ,site_name,' GYTS Summary Tables.docx')) 
