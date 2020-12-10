####################################################################################################
## Project: END TO END CHECKER                                                                    ##
## TO USE:  (1) Upload all EPRS in directory, starting with EPR + 2 digits,                       ## 
##              e.g., "EPR01.pdf"                                                                 ##
##          (2) Upload stat extract (ordered correctly and saved as "statextrac.csv"              ##
##              file in the same directory)                                                       ##
## TODO:    Include CRF pdfs as part of checks                                                    ##
## Date:    10/16/17                                                                              ##
## Author:  Natalie Jorion                                                                        ##
####################################################################################################

# Libraries
library(stringr)
library(tidyr)
library(pdftools)

setwd("./R Projects/End to End")

# Functions
source("functions.R")

#Data
statextract <- read.csv("statextract.csv",header=TRUE)
PNcontent_table1 <- read.csv("PN_table.csv",header=FALSE)
RNcontent_table1 <- read.csv("RN_table.csv",header=FALSE)

#Gather file names
fileNames = list.files(pattern="EPR.*\\.pdf")

datalist = list()
#Perform checks for each EPR
for (fileName in fileNames){
  #read pdf file as text file
  txt1 <- trim(pdf_text(fileName))

  #convert text file into data frame
  df <- read.table(textConnection(txt1), header = TRUE, sep = "\\", stringsAsFactors = F)

  #calculate the last row of the score report
  end_row <- find_end_row("Cont.  Diff T Resp",df$Electronic.Performance.Report)
  
  #extract score report from data frame
  score_report <- generate_score_report(df,end_row)

  #extract performance report from data frame
  test_specs <- generate_test_specs(df,end_row)
  
  #get file number of EPR to locate corresponding line in stat extract
  file_number <- as.numeric(substr(fileName, 4, 5))

  #generate a line of checks
  checks <- endtoend_check(txt1[1],score_report,test_specs,statextract[file_number,])
  
  datalist[[fileName]] <- checks
}

#stack all the checks into a data frame
stacked <- as.data.frame(do.call(rbind, datalist))
stacked  <- tibble::rownames_to_column(stacked)

View(stacked)

write.csv(stacked,"End_to_end_checks.csv", row.names = FALSE)
