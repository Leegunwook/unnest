########### PRE-Condition ####################
# 1. 필수 library 설치 및 호출
install.packages("remotes")
library(remotes)
remotes::install_gitlab("mrchypark/multilinguer")
install.packages("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions", "usethis", "RWeka", "multilinguer", "rJava")
Sys.setlocale(locale="ko_KR.utf8")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
Sys.setlocale("LC_CTYPE", "ko_KR.UTF-8")
#install.packages("backports")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("srt", "stringr")

library(KoNLP)
library(srt)
library(tidytext)
library(tidyverse)
library(multilinguer)
library(stringr)
#library(tm)

# 2. srt 자막 파일을 srt 라이브러리 적용하여 tidy형태로 read하여 변수에 저장
s1 <- read_srt("/cloud/project/Squid.Game.S01E01.Red.Light,.Green.Light.WEBRip.Netflix.ko[cc].srt")
s2 <- read_srt("/cloud/project/Squid.Game.S01E02.Hell.WEBRip.Netflix.ko[cc].srt")
s3 <- read_srt("/cloud/project/Squid.Game.S01E03.The.Man.with.the.Umbrella.WEBRip.Netflix.ko[cc].srt")
s4 <- read_srt("/cloud/project/Squid.Game.S01E04.Stick.to.the.Team.WEBRip.Netflix.ko[cc].srt")
s5 <- read_srt("/cloud/project/Squid.Game.S01E05.A.Fair.World.WEBRip.Netflix.ko[cc].srt")
s6 <- read_srt("/cloud/project/Squid.Game.S01E06.Gganbu.WEBRip.Netflix.ko[cc].srt")
s7 <- read_srt("/cloud/project/Squid.Game.S01E07.VIPS.WEBRip.Netflix.ko[cc].srt")
s8 <- read_srt("/cloud/project/Squid.Game.S01E08.Front.Man.WEBRip.Netflix.ko[cc].srt")
s9 <- read_srt("/cloud/project/Squid.Game.S01E09.One.Lucky.Day.WEBRip.Netflix.ko[cc].srt")
##############################################
#에피소드 컬럼 추가
s1 <- mutate(s2, episode = 1) 
s2 <- mutate(s2, episode = 2) 
s3 <- mutate(s3, episode = 3) 
s4 <- mutate(s4, episode = 4) 
s5 <- mutate(s5, episode = 5) 
s6 <- mutate(s6, episode = 6) 
s7 <- mutate(s7, episode = 7) 
s8 <- mutate(s8, episode = 8) 
s9 <- mutate(s9, episode = 9)

# 불용어 사전 구성
st_word <- read.table (file = "/cloud/project/stword.txt", header=T)

# 특수문자를 공백으로 치환 및 애피소드 컬럼 적용
f_s_token <- rbind(s1_token, s2_token, s3_token, s4_token, s5_token, s6_token, s7_token, s8_token, s9_token) %>% anti_join(st_word)


s1_test1 <- str_replace_all(s1$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2)%>%
  count(word, sort = TRUE)  %>%
  anti_join(st_word)

##################################tf idf
s1_token <- str_replace_all(s1$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 1)

s2_token <- str_replace_all(s2$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 2)

s3_token <- str_replace_all(s3$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 3)

s4_token <- str_replace_all(s4$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 4)

s5_token <- str_replace_all(s5$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 5)

s6_token <- str_replace_all(s6$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 6)

s7_token <- str_replace_all(s7$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 7)

s8_token <- str_replace_all(s8$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 8)

s9_token <- str_replace_all(s9$subtitle, "[^가-힣]", " ") %>%
  as_tibble() %>%
  unnest_tokens(morp, value, token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort = TRUE) %>%
  mutate(episode = 9)

f_s_token <- rbind(s1_token, s2_token, s3_token, s4_token, s5_token, s6_token, s7_token, s8_token, s9_token) %>% anti_join(st_word)

##################################
total_words <- 안티조인한거 %>%
  group_by(episode) %>%
  summarize(total = sum(n))