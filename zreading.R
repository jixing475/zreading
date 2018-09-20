library(rvest)
library(curl)
library(tidyverse)

library(robotstxt)
# check path allowed
paths_allowed("http://www.zreading.cn/")
# base url
base_url <- "http://www.zreading.cn/page/"
#==== extract paper title and url ====
#==== return table ====
zreading_str <- function(url = "http://www.zreading.cn/page/1"){
  page <- 
    try(read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0"))))
  # extract gene summary infomation
  title <- 
    page%>%
    html_nodes("header > h2 > a") %>% 
    html_text()
  title_url <- 
    page %>% 
    html_nodes("header > h2 > a") %>% # > 代表层级
    html_attr("href")

  table <- 
    data.frame(Title = title,
               url = title_url,
               stringsAsFactors = FALSE)
  return(table)
}
#==== extract all text: return file ====
zreading_text <- function(url, name){
  page <- read_html(curl(as.character(url), handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  p <- page %>%
    html_nodes("div > p") %>% 
    html_text()
  anchor <- which(str_detect(p, "^版权所有"))
  p_filter <- p[1:anchor-1]
  write_lines(p_filter, str_replace_all(name, "\\/", "_") %>% str_c(".txt"))
}
zreading_yulu <- function(url, name){
  page <- read_html(curl(as.character(url), handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  p <- page %>%
    html_nodes("ol >li") %>% 
    html_text()
  write_lines(p, str_replace_all(name, "\\/", "_") %>% str_c(".txt"))
}
#==== extract image file ====
zreading_img <- function(url, name){
  page <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  
  node_img <- 
    page %>%
    html_nodes("p > img") 
  if(length(node_img) != 0){
    img_ur <- node_img %>% 
      html_attr("src") 
    for (i in 1:length(img_ur)) {
      try(download.file(url = img_ur[1],
                        destfile = str_replace_all(name, "\\/", "_") %>% str_c("_", i,".jpg"),
                        mode = "wb"))
    }
  } 
  

}
#==== all page url ====
url <- vector()
for (i in 1:419) {
  url[i] <- str_c(base_url, i)
}

#==== all paper title and url ====
#table_total <- 
#  url %>% 
#  map_df(~ zreading_str(.x) %>% try())
load("table_total.rda")
setwd("~/Desktop/zreading/txt/")
idx <- str_detect(table_total$Title, "语不惊人")
title <- table_total$Title[idx]
paper_url <- table_total$url[idx]
#==== all yulu file ====
for (i in 1:length(paper_url)) {
  zreading_yulu(url = paper_url[i], 
                name = title[i])
  print(i)
}

#==== download images ====
for (i in 1:length(paper_url)) {
  zreading_img(url = paper_url[i], 
                name = title[i])
  print(i)
}
#save(table_total, file = "table_total.rda")

#http://www.zreading.cn/page/101
#system("type -a wget")
