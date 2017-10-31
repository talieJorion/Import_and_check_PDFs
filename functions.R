trim <- function (x) gsub("^\\s+|\\s+$", "", x)

extract_word <- function(text,pattern1,length){
  last <- as.data.frame((str_locate_all(pattern =pattern1,text)[1]))$end
  trim(substr(text, last+1, last+length))
}

find_end_row <- function(search_string,df){
  end_row <- which(grepl(search_string, df))-1
  return(end_row)
}

generate_test_specs <- function(df,end_row){
  test_specs_table <- trim(df[(end_row+9):(end_row+18),])
  test_specs_table <- str_replace_all(test_specs_table, "\\|","")
  test_specs_table <- gsub("\\s+", " ", str_trim(test_specs_table ))
  test_specs_table <- read.table(textConnection(test_specs_table), header = FALSE, sep = "\\", stringsAsFactors = F)
  test_specs_table2 <- separate(test_specs_table, V1, c("number","expected","observed","total",
                                                        "right","wrong","diff","theta","se","total",
                                                        "total response time", "average response time"),
                                sep = " ", remove = TRUE)
  return(test_specs_table2)
}

generate_score_report <- function(df,end_row){
  oldw <- getOption("warn")
  options(warn = -1)
  score_report <- trim(df[12:(end_row),])
  score_report <- gsub("\\s+", " ", str_trim(score_report ))
  score_report <- read.table(textConnection(score_report), header = FALSE, sep = "\\", stringsAsFactors = F)
  score_report <- separate(score_report, V1, c("Event","Item","AccNum","Cont","Diff","T","Resp","Score",
                                               "Time","Spent","Theta","SE"),
                           sep = " ", remove = TRUE)
  return(score_report)
  options(warn = oldw)
}

#If Candidate does not answer last item, Resp will be 0 in EPR.
#However, delimited function will cause time to be pushed over into Score.  
#So if Score has a ":", then the candidate did not respond to the last item.
find_itemN <- function(score_report){
  item <- score_report$Item[nrow(score_report)]
  item <- as.numeric(item)
  is_time <- str_detect(score_report$Score[nrow(score_report)],":")
  return(ifelse(is_time,item-1,item))
}

find_SEM <- function(score_report){
  SEM_test1 <- score_report$SE[nrow(score_report)]
  SEM_test2 <- score_report$Theta[nrow(score_report)]
  SEM_test3 <- score_report$Spent[nrow(score_report)]
  test1_trial <- grepl("^[[:digit:]]",SEM_test1)
  test2_trial <- grepl("^[[:digit:]]",SEM_test2)
  test3_trial <- grepl("^[[:digit:]]",SEM_test3)
  if(test1_trial){
    return(SEM_test1)
  } else if (test2_trial){
    return(SEM_test2)
  } else return(SEM_test3)
}

find_EPR_thet <- function(score_report){
  SEM_test1 <- score_report$SE[nrow(score_report)]
  SEM_test2 <- score_report$Theta[nrow(score_report)]
  SEM_test3 <- score_report$Spent[nrow(score_report)]
  test1_trial <- grepl("^[[:digit:]]",SEM_test1)
  test2_trial <- grepl("^[[:digit:]]",SEM_test2)
  test3_trial <- grepl("^[[:digit:]]",SEM_test3)
  if(test1_trial){
    return(score_report$Theta[nrow(score_report)])
  } else if (test2_trial){
    return(score_report$Spent[nrow(score_report)])
  } else return(score_report$Time[nrow(score_report)])
}

endtoend_check <- function(text,score_report,test_specs,statextract_row){
  EPR_name       <- trim(substr(text, 37,55))
  test           <- extract_word(text,"Test: ",10)
  GUID           <- extract_word(text,"Guid: ",36)
  cut_score      <- extract_word(text,"MPS: ",6)
  pass_fail      <- extract_word(text,"Status: ",6)
  ROOT           <- extract_word(text,"ROOT: ",2)
  Max_item       <- extract_word(text,"Max Item: ",2)
  num_item       <- find_itemN(score_report)
  num_prete      <- length(grep("\\*.", score_report$Theta))
  theta_tb1      <- test_specs[10,8]
  theta_tb2      <- extract_word(text,"Theta: ",7)
  theta_tb3      <- find_EPR_thet(score_report)
  num_items_stat <- statextract_row$NumItem==find_itemN(score_report)
  term_type      <- statextract_row$TermType
  stat_thet      <- statextract_row$FinThetC
  epr_thet       <- extract_word(text,"Theta: ",7)
  SEM_stat       <- statextract_row$FinSEMC
  SEM_EPR        <- find_SEM(score_report)
  pass_stat      <- statextract_row$PassC
  pass_EPR       <- extract_word(text,"Status: ",6)
  n_match        <- all(statextract_row[1,16:23]==as.numeric(test_specs$total[1:8]))
  thet_stat      <- statextract_row[,24:31]
  thet_EPR       <- as.numeric(test_specs$theta[1:8])
  
  checks <- (as.data.frame((c(EPR_name,test,GUID,cut_score,pass_fail,ROOT,Max_item,num_item,num_prete,
                              theta_tb1,theta_tb2,theta_tb3,num_items_stat,term_type,stat_thet,epr_thet,
                              SEM_stat,SEM_EPR,pass_stat,pass_EPR,n_match,thet_stat,thet_EPR ))))
  
  colnames(checks) <- c("EPR_name","test","GUID","cut_score","pass_fail","ROOT","Max_item","num_item","num_prete",
                        "Perform_sum_theta","Header_theta","Score_report_theta","Number_Items_Match_EPR_Stat","term_type","stat_thet","epr_thet","
                        SEM_stat","SEM_EPR","pass_stat","pass_EPR","domains_match",
                        "thet_stat1","thet_stat2","thet_stat3","thet_stat4","thet_stat5","thet_stat6","thet_stat7","thet_stat8",
                        "thet_EPR1","thet_EPR2","thet_EPR3","thet_EPR4","thet_EPR5","thet_EPR6","thet_EPR7","thet_EPR8")
  return(checks)
}
