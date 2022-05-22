############################# 
### Naver News Scraping
#############################
library(stringr)

url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EB%B9%85%EB%8D%B0%EC%9D%B4%ED%84%B0&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=42&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start=1"

n_news <- read_html(url) %>% 
          html_nodes(".sc_page_inner ") %>%
          html_text() %>%
          word(3) %>%

  
Sys.setenv("http_proxy"="")
Sys.setenv("no_proxy"=TRUE)
Sys.setenv("no_proxy"=1)  
  
  
########### Assginment 1 ####################  
  
######### 패키지 설치
install.packages("rvest")
install.packages("dplyr")
install.packages("tidyverse")

######### library 불러오기
library(rvest)
library(dplyr)
library(tidyverse)


#### 변하지 않는 url 세팅
based_url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EB%AF%B8%EA%B5%AD%20%EA%B8%B0%EC%A4%80%EA%B8%88%EB%A6%AC&sort=0&photo=0&field=0&pd=1&ds=2021.10.09&de=2021.10.16&cluster_rank=56&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:1w,a:all&start="

#### 변하는 부분 for문 처리로 url 생성
urls <- NULL
for (x in 0:9) {
  urls <- paste0(based_url, x*10+1, seq= '')
}

urls


#### url 가져오기

read_html(urls) %>%
html_nodes('ul.list_news > li > dl > dd > a')





########### re

naver_url <-"https://news.naver.com/main/list.naver?mode=LS2D&sid2=230&sid1=105&mid=shm&date=20211016&page=1"

naver_url_1 <- "https://news.naver.com/main/list.naver?mode=LS2D&sid2=230&sid1=105&mid=shm&date="

date <- 20211001:20211016

naver_url_2 <- '&page='

page <- 1:10

for (dt in date) {
  for (page_num in page) {
    naver_url <- paste0(naver_url_1, dt, naver_url_2, page_num)
    
    print(naver_url)
  }
}

naver_url <-"https://news.naver.com/main/list.naver?mode=LS2D&sid2=230&sid1=105&mid=shm&date=20211016&page=1"

html <- read_html(naver_url)

temp <- unique(html_nodes(html, '#main_content') %>%
                 )



