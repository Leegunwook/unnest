###### Assignment 2 네이버 뉴스데이터 분석 ######
# 빅데이터학과 2020720704 이건욱

# 조건 1: 각자 원하는 주제를 생각해서 고급 검색식을 만들 것
# 조건 2: 조건 1에서 생성한 고급 검색식은 뉴스 개수를 1000개 미만으로 추출할 수 있도록 만들 것
# 조건 3: 수집할 데이터는 (1)뉴스 제목, (2)뉴스 날짜, (3)언론사명, (4)뉴스 URL, (5)뉴스 본문 등으로 구성할 것

# 검색 주제: "삼성" +"야구"
# 정렬: 최신순

####################################################################################
## 1. 라이브러리 로딩

# install.packages("rvest")
# install.packages("stringr")

library(rvest)
library(stringr)

####################################################################################
## 2. 웹 페이지 읽기

# url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%22LG%20%ED%8A%B8%EC%9C%88%EC%8A%A4%22%20%2B%22%EC%8A%B9%EB%A6%AC%22&sort=1&pd=3&ds=2021.10.10&de=2021.10.10&news_office_checked=&nso=so:dd,p:from20211010to20211010,a:all&start="
url <- "https://search.naver.com/search.naver?where=news&query=%22%EC%82%BC%EC%84%B1%22%20%2B%22%EC%95%BC%EA%B5%AC%22&sort=1&news_office_checked=&nso=so%3Add%2Cp%3Aall&start="

# url start=1 ~ start=991 까지 만들어주기 (991 page = 991~1000건까지 조회)
urls <- NULL
for(x in 0:99) {
  urls[x+1] <- paste0(url, x*10+1)
}

####################################################################################
## 3. 뉴스 총 건수 구하기

##### 검색 결과 총 건수는 더이상 네이버에서 제공하지 않음 #####
# all_my span, "건" 등으로 확인 불가

# start = 1, 11, 21, ... , 991까지 = 기사 수가 총 <= 1,000건까지 조회됨
# 최신순 정렬하고 url에 임의숫자로 검색했을 때, 결과가 없는 페이지 숫자를 입력할 경우,
# <div class="api_noresult_wrap"> 이 조회됨
# 따라서 page 1 ~ 991 호출하되, 더이상 기사가 없는 페이지일 경우 break 처리

# "네이버뉴스"로 제공되는 건만 크롤링 (뉴스 기사 본문 tag 통일 관점)

naver_news <- NULL

for (i in 1:991) {
  html_result <- read_html(urls[i])
  # 결과 없을 경우 조회되는 html 태그
  no_result_url <- html_nodes(html_result, '.api_noresult_wrap')
  
  if(length(no_result_url) != 0) {
    # page가 없을 경우, break 처리
    # 없는 page 확인
    cat("no_result url page", (i*10-9))
    break
  } else {
    # "네이버뉴스" URL 크롤링
    naver_news_urls <- html_result %>%
      html_nodes("div.news_area>div.news_info>div.info_group>a.info") %>%
      html_attr("href")
    
    naver_news <- c(naver_news, naver_news_urls)
    # a.info 태그 하위에는 신문사 url이 같은 level로 포함되기 때문에 url 정리
    # "url에 news.naver 포함된 건만 대상으로 정리
    naver_news <- naver_news[grepl("news.naver", unlist(naver_news))]
  }
} # end for


# 네이버뉴스 URL로 본문 크롤링
news_metadata_all <- NULL

for (news in naver_news) {
  
  result <- tryCatch(
    {
      content_html <- read_html(news)
      
      ## 4. 뉴스 제목
      news_title <- content_html %>%
        html_nodes("h4.title") %>%
        html_text()
      
      ## 5-1. 뉴스 날짜
      news_date <- content_html %>%
        html_node("div.news_headline div.info span") %>%
        html_text()
      
      # 5-2. 뉴스 날짜 후처리 (기사입력 2021.10.10. 오후 13:45 -> 날짜만 2021.10.10.)
      news_date <- gsub("기사입력", "", news_date)
      news_date <- gsub("오.*", "", news_date)
      
      ## 6. 뉴스 언론사
      news_press <- content_html %>%
        html_nodes("div.content_area div.news_headline span#pressLogo a.link img") %>%
        html_attr("alt")
      
      ## 7. 뉴스 URL (네이버 뉴스 URL)
      news_url <- news
      
      ## 8. 뉴스 본문 및 본문 후처리
      news_content <- content_html %>%
        html_nodes("div#newsEndContents") %>%
        html_text() %>%
        str_replace_all("<U+00A0>", "") %>%
        str_replace_all("<U.00A0>", "") %>%
        str_replace("\n\t\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t", "")
      
      news_content <- str_trim(news_content)
      news_content <- gsub("\n.*", "", news_content)
      news_content <- gsub("\u00A0", " ", news_content)
      
      ## 9. 뉴스 메타 데이터 프레임 생성
      news_metadata <- data.frame(news_title = news_title, news_date = news_date, news_press = news_press, news_url = news_url, news_content = news_content)
      
      ## 10. 뉴스 메타데이터를 데이터 프레임으로 저장
      news_metadata_all <- rbind(news_metadata_all, news_metadata)
      
    },
    warning = function(e) {
      message("### warning ###")
      news_content <<- "warning"
      return(news_content)
    },
    error = function(e) {
      message("### error ###")
      news_content <<- "error"
      return(news_content)
    },
    finally = {
      # 11. 크롤링한 뉴스 데이터를 CSV 파일로 저장
      write.csv(news_metadata_all, file="naver_article.csv")
      # message("Success")
    }
  ) #tryCatch
  # print(paste("result = ", result))
  
} # end for 뉴스 본문