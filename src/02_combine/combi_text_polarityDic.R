#### edinet からデータ取得
### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(xml2)
library(rvest)
library(lubridate)
library(XBRL)
library(rvest)
library(RMeCab)

### calling functions in other files
source("src/01_getdata/getQualitativedata.R")

### 文章と極性辞書をマッチングする関数
count_match = function(text, word_list){
  kaiseki = RMeCab::RMeCabC(text) # 形態素ごとに文章を区切る
  temp = unlist(kaiseki)
  out = length(intersect(temp, word_list))
  return(out)
}

get_polDic = function(){
  # 日本語極性辞書の読み込み：https://estrellita.hatenablog.com/entry/2018/09/02/004829
  polDic_noun = read_delim("http://www.cl.ecei.tohoku.ac.jp/resources/sent_lex/pn.csv.m3.120408.trim",
                           delim="\t", 
                           col_names = FALSE, 
                           locale = locale(encoding = 'UTF8')
  ) %>%
    dplyr::rename(word = X1, sentiment = X2, vp = X3) %>%
    dplyr::select(word, sentiment) %>%
    dplyr::filter((.$sentiment == "p")|(.$sentiment == "n"))
  
  polDic_verb = read_delim("http://www.cl.ecei.tohoku.ac.jp/resources/sent_lex/wago.121808.pn", 
                           delim="\t", 
                           col_names = FALSE, 
                           locale = locale(encoding = 'UTF8')
  ) %>%
    dplyr::rename(word = X2, sentiment = X1) %>%
    dplyr::select(word, sentiment) %>%
    dplyr::mutate(sentiment = case_when( # sentiment列のユニーク値は"ネガ（経験）" "ネガ（評価）" "ポジ（経験）" "ポジ（評価）"
      stringi::stri_detect_regex(.$sentiment, "^ネガ.*") == TRUE ~ "n",
      stringi::stri_detect_regex(.$sentiment, "^ポジ.*") == TRUE ~ "n"
    )
    )
  
  polDic_maseter = dplyr::bind_rows(polDic_noun, polDic_verb)
  return(polDic_maseter)
}

main = function(start_date, end_date){
  polDic_maseter = get_polDic()
  ### edinetからデータ取り込み
  data_fs_w_text = main(start_date = start_date, end_date = end_date)
  
  polDic_maseter_positive = polDic_maseter %>% dplyr::filter(., sentiment == "p")
  polDic_maseter_negative = polDic_maseter %>% dplyr::filter(., sentiment == "n")
  df_out = data_fs_w_text %>%
    dplyr::mutate(
      count_positive = purrr::map(.$info_ManagementAnalysis_neat, ~ count_match(.x, polDic_maseter_positive$word)),
      count_negative = purrr::map(.$info_ManagementAnalysis_neat, ~ count_match(.x, polDic_maseter_negative$word)),
    )
  return(df_out)
}

main(start_date = "", end_date = "", polDic_maseter)
