### getting docID through edinet API

### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(rvest)
library(lubridate)
library(XBRL)

get_json = function(date_filing, path_filing) {
  
  url_api = "https://api.edinet-fsa.go.jp/api/v2/documents.json?date="
  
  url_type = "&type=2" # 提出書類一覧及びメタデータを取得する。本記事の目的はdocIDの取得であるからこちらを指定する。
  
  key = "&Subscription-Key=eb10a3c6b0a649a4b86980603cd8687e" # subscription key
  
  get_res = httpGET(str_c(url_api, date_filing, url_type, key)) ### get response　# GETメソッドの出力のsubmitDateTimeを指定
  
  data_raw = fromJSON(get_res) # transform json data into list
  
  data_flag = # metadata > resultset > countの情報を取得
    data_raw %>% 
    purrr::pluck("metadata") %>% 
    purrr::pluck("resultset") %>% 
    purrr::pluck("count")
  
  if(is.null(data_flag) == TRUE){
    stop(str_c("Maybe ", date_filing, " is holiday!!"))
  }
  
  data_tidy =       
    data_raw %>% 
    purrr::pluck(2) %>% # extract "results"
    as_tibble() %>% 
    slice(str_which(.$docDescription, "有価証券報告書")) %>% 
    slice(str_which(.$docDescription, "訂正", negate = TRUE)) %>% # negate: 否定条件
    filter(!is.na(secCode)) %>% # 上場企業(seccode)を持っている企業のみ抽出
    select(docID, edinetCode, filerName, secCode, periodEnd) %>% 
    mutate_at("filerName", str_remove, pattern = "株式会社") %>% # remove "株式会社"
    mutate_at("filerName", str_squish) %>% # remove whitespace
    distinct()
  
  data_tidy %>% 
    write_excel_csv(str_c(path_filing, "/docID.csv"))
  
  ## API key word : docID
  
  rm(list = ls(pattern = "^url|get_res|^data"))
}