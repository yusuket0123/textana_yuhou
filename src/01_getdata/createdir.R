### creating a folder to preserve data extracted from edinet
### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(rvest)
library(lubridate)
library(XBRL)

make_path_filing = function(date_filing, dl_date = today()){
  path = str_c("data/", dl_date, "_dl")
  path_filing = str_c(path, "/", date_filing, "_filing")
  return(path_filing)
}

create_file_structure = function(date_filing, path_filing, path_label, path_tidyup, dl_date = today()) {
  
  path = str_c("data/", dl_date, "_dl")  # use <=
  
  if(!file.exists(path)) {  # create file if NOT exists
    dir.create(path)
  }
  ##
  
  if(!file.exists(path_label)) {
    dir.create(path_label)
  }
  ##

  if(!file.exists(path_filing)) {
    dir.create(path_filing)
  }
  ##
  
  path_zip = str_c(path_filing, "/zip")
  
  if(!file.exists(path_zip)) {
    dir.create(path_zip)
  }
  ##
  
  if(!file.exists(path_tidyup)) {
    dir.create(path_tidyup)
  }
  
  ## return all path
}