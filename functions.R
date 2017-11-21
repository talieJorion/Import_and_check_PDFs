#' Trim Function
#'
#' Trims extraneous white space
#' @param x string
#' @keywords
#' @export
#' @examples
#' trim("  so much white space  ")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Extract Word Function
#'
#' Find a string of a specified length following a string
#' @param text long string to search
#' @param pattern1 word contained in text, after which the desired string will follow
#' @param length character length of the specified string
#' @keywords
#' @export
#' @examples

extract_word <- function(text,pattern1,length){
  stringi::last <- as.data.frame((str_locate_all(pattern =pattern1,text)[1]))$end
  trim(substr(text, last+1, last+length))
}

#' Find Quarter Function
#'
#' Given an EPR text, will find the date and return the corresponding quarter.
#' @param search_string long string to search
#' @param full If TRUE, returns full month space year, e.g., October 2017.
#'  If false, returns abb no space year, e.g., Oct17.  Defaults to TRUE.
#' @keywords
#' @export
#' @examples

find_quarter <- function(search_string, full = TRUE){
  month_num <- extract_word(search_string,"Begin: ",2)
  month_num <- as.numeric(month_num)
  year <- str_sub(extract_word(search_string,"Begin: ",10),9,-1)
  if (month_num > 0 & month_num < 4){
    month <- "January" }
  else if (month_num > 3 & month_num < 7) {
    month <- "April" }
  else if (month_num > 6 & month_num < 10) {
    month <- "July" }
  else
    month <- "October"

  month_abb <- str_sub(month,1,3)
  if (!full){
    return(paste0(month_abb,year))
  }
  else return(paste0(month," 20",year))
}

#' Find End Row Function
#'
#' Given an EPR data frame, will return the numeric value of the row above a string.
#' Used to extract tables from EPRs.
#' @param search_string string specified as marker
#' @param df data frame to look through
#' @keywords
#' @export
#' @examples

find_end_row <- function(search_string,df){
  end_row <- which(grepl(search_string, df))-1
  return(end_row)
}


#' Generate Test Specs
#'
#' Given an EPR data frame, will extract the test specs and return a data frame.
#' @param df EPR data frame to look through
#' @param end_row numeric value of the last row
#' @keywords
#' @export
#' @examples
#'
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

#' Generate Score Report
#'
#' Given an EPR data frame, will extract the score report and return a data frame.
#' @param df EPR data frame to look through
#' @param end_row numeric value of the last row
#' @keywords
#' @export
#' @examples
#'
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


#' Find total number of items answered by candidate
#'
#' Given a score report with wonky delimited input, will extract appropriate item N.
#' If Candidate does not answer last item, Resp will be 0 in EPR.
#' However, delimited function will cause time to be pushed over into Score.
#' So if Score has a ":", then the candidate did not respond to the last item.
#' @param score_report score report data frame to fix
#' @keywords
#' @export
#' @examples
#'

find_itemN <- function(score_report){
  item <- score_report$Item[nrow(score_report)]
  item <- as.numeric(item)
  is_time <- str_detect(score_report$Score[nrow(score_report)],":")
  return(ifelse(is_time,item-1,item))
}

#' Find final SEM
#'
#' Given a score report, will return the final SEM.
#' @param score_report score report data frame
#' @keywords
#' @export
#' @examples
#'

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

#' Find final theta
#'
#' Given a score report, will return the final theta.
#' Adjusts in case breaks and alternate items cause input to not be delimited correctly.
#' @param score_report score report data frame
#' @keywords
#' @export
#' @examples
#'

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


#' Is this value consecutive
#'
#' Given a vector (in this case, theta values), will find if
#' there are MORE than two alternate items in a row.
#' RETURNS TRUE if there are NOT more than 2 consecutive *.**

#' @param score_report_theta vector of thetas to search
#' @keywords
#' @export
#' @examples
#' is_conseq("*.**",  "*.**",  "*.**",  "5.00",  "5.00")

is_consec <- function(score_report_theta){
  row_n_pretest <- which(score_report_theta == "*.**")
  result <- rle(diff(row_n_pretest))
  return(!any(result$lengths>=2 & result$values==1))
}


#' Are there alternative items before the 11th item?
#'
#' RETURNS TRUE if no pretest items before 11th item
#' @param score_report_theta vector of thetas to search
#' @keywords
#' @export
#' @examples

is_before_11 <- function(score_report_theta){
  return(!("*.**" %in% score_report_theta[1:10]))
}

#' Are there alternative items after the 85th item?
#'
#' RETURNS TRUE if no pretest items after the 85th item
#' @param score_report_theta vector of thetas to search
#' @keywords
#' @export
#' @examples
#'
is_after_60 <- function(score_report_theta){
  return(!("*.**" %in% score_report_theta[85:length(score_report_theta)]))
}

#' Function to create end to end data frame check
#'
#' Returns a data frame with EPR name, test, GUID, etc.
#' @param text string text to search
#' @param score_report score report data frame
#' @param test_specs data frame of test specs
#' @param statextract_row numeric value of last row in stat extract
#' @keywords
#' @export
#' @examples
#'

endtoend_check <- function(text,score_report,test_specs,statextract_row){
  EPR_name       <- trim(substr(text, 37,55))
  test           <- extract_word(text,"Test: ",10)
  GUID           <- extract_word(text,"Guid: ",36)
  cut_score      <- extract_word(text,"MPS: ",6)
  pass_fail      <- extract_word(text,"Status: ",6)
  ROOT           <- extract_word(text,"ROOT: ",2)
  Max_item       <- extract_word(text,"Max Item: ",2)
  no_before_11   <- is_before_11(score_report$Theta)
  no_after_60    <- is_after_60(score_report$Theta)
  no_more_2_pre  <- is_consec(score_report$Theta)
  num_item       <- find_itemN(score_report)
  num_prete      <- length(grep("99", score_report$Cont))
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

  checks <- (as.data.frame((c(EPR_name,test,GUID,cut_score,pass_fail,ROOT,Max_item,no_before_11,
                              no_after_60,no_more_2_pre,num_item,num_prete,
                              theta_tb1,theta_tb2,theta_tb3,num_items_stat,term_type,stat_thet,epr_thet,
                              SEM_stat,SEM_EPR,pass_stat,pass_EPR,n_match,thet_stat,thet_EPR ))))

  colnames(checks) <- c("EPR_name","test","GUID","cut_score","pass_fail","ROOT","Max_item","no_before_11",
                        "no_after_60","no_more_2_pre","num_item","num_prete","Perform_sum_theta",
                        "Header_theta","Score_report_theta","Number_Items_Match_EPR_Stat",
                        "term_type","stat_thet","epr_thet","SEM_stat","SEM_EPR","pass_stat",
                        "pass_EPR","domains_match","thet_stat1","thet_stat2","thet_stat3",
                        "thet_stat4","thet_stat5","thet_stat6","thet_stat7","thet_stat8",
                        "thet_EPR1","thet_EPR2","thet_EPR3","thet_EPR4","thet_EPR5","thet_EPR6",
                        "thet_EPR7","thet_EPR8")
  return(checks)
}


#' Function to clean score report
#'
#' Takes out alternative items and corrects delimited errors.
#' @param score_report score report data frame h
#' @keywords
#' @export
#' @examples
#'

clean_score_report <- function(score_report){
  #Take out Alternative Items and Breaks
  score_report <- score_report %>%
    filter(Cont!="99") %>%
    filter(Item!="*********************OPTIONAL") %>%
    filter(Item!="******************UNSCHEDULED")

  #Corrects delimited errors
  delim_incor <- which(str_length(score_report$Score)>1)

  for (m in 1:length(delim_incor)){
    score_report$Score[delim_incor] <- str_sub(score_report$T[delim_incor], -1, -1)
    score_report$Theta[delim_incor] <- score_report$Time[delim_incor]
  }
  return(score_report)
}

#' Get Pool Definition File, extract b-values
#'
#' Given a specified quarter, will find the pool def file on the N drive and return the
#' ACC Numbers and the corresponding b-values
#' Does some work arounds
#' @param quarter_pool Quarter in the
#' @keywords
#' @export
#' @examples
#'

get_pooldef_bvals <- function(quarter_pool){
  #Find sheet number of test def file
  pos_pooldef <- pmatch("RNOP_TestDef", excel_sheets(quarter_pool))


  b_vals_qtr <- read_excel(quarter_pool, sheet=pos_pooldef) %>%
    select(test_item_label, bval) %>%
    rename(AccNum = test_item_label)

  b_vals <- left_join(score_report, b_vals_qtr, by="AccNum") %>%
    select(AccNum, bval)
  return(b_vals)
}
