#KoNLP 설치
install.packages("KoNLP")
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions", "usethis", "RWeka", "multilinguer", "rJava"), type = "binary")
install.packages("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions", "usethis", "RWeka", "multilinguer", "rJava")


# 1. 필요한 library 호출
library(srt)
library(tidytext)
library(dplyr)
library(tidyverse)

library(KoNLP)
library(srt)
library(tidytext)
library(tidyverse)
library(multilinguer)
library(stringr)
########################################################
install.packages("remotes")
library(remotes)
remotes::install_gitlab("mrchypark/multilinguer")
install.packages("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions", "usethis", "RWeka", "multilinguer", "rJava")
Sys.setlocale(locale="ko_KR.utf8")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
Sys.setlocale("LC_CTYPE", "ko_KR.UTF-8")

############################################################


# 2. srt 파일을 srt 라이브러리 적용하여 tidy형태로 f_s1 ~ f_s9 변수에 저장
f_s1 <- read_srt("/cloud/project/final/Squid.Game.S01E01.Red.Light,.Green.Light.WEBRip.Netflix.ko[cc].srt")
f_s2 <- read_srt("/cloud/project/final/Squid.Game.S01E02.Hell.WEBRip.Netflix.ko[cc].srt")
f_s3 <- read_srt("/cloud/project/final/Squid.Game.S01E03.The.Man.with.the.Umbrella.WEBRip.Netflix.ko[cc].srt")
f_s4 <- read_srt("/cloud/project/final/Squid.Game.S01E04.Stick.to.the.Team.WEBRip.Netflix.ko[cc].srt")
f_s5 <- read_srt("/cloud/project/final/Squid.Game.S01E05.A.Fair.World.WEBRip.Netflix.ko[cc].srt")
f_s6 <- read_srt("/cloud/project/final/Squid.Game.S01E06.Gganbu.WEBRip.Netflix.ko[cc].srt")
f_s7 <- read_srt("/cloud/project/final/Squid.Game.S01E07.VIPS.WEBRip.Netflix.ko[cc].srt")
f_s8 <- read_srt("/cloud/project/final/Squid.Game.S01E08.Front.Man.WEBRip.Netflix.ko[cc].srt")
f_s9 <- read_srt("/cloud/project/final/Squid.Game.S01E09.One.Lucky.Day.WEBRip.Netflix.ko[cc].srt")
##############################################

# 3. sx_lee변수 내 데이터에 episode 컬럼을 mutate하고, 각 시나리오 번호 부여
f_s1 <- mutate(f_s1, episode = 1) 
f_s2 <- mutate(f_s2, episode = 2) 
f_s3 <- mutate(f_s3, episode = 3) 
f_s4 <- mutate(f_s4, episode = 4) 
f_s5 <- mutate(f_s5, episode = 5) 
f_s6 <- mutate(f_s6, episode = 6) 
f_s7 <- mutate(f_s7, episode = 7) 
f_s8 <- mutate(f_s8, episode = 8) 
f_s9 <- mutate(f_s9, episode = 9) 


s1_test1 <- f_s1 %>%
  unnest_tokens(morp, subtitle, token = SimplePos09)

# 4. gather_episode 변수 생성후 sx_lee의 데이터를 rbind로 병합
f_gather_episode <- rbind(f_s1, f_s2, f_s3, f_s4, f_s5, f_s6, f_s7, f_s8, f_s9)


# 5. 불필요 데이터 제거 (자막 시작과 끝, 번역자 정보 등)
gather_episode <- gather_episode %>% 
  filter(!grepl("EPISODE", subtitle)) %>%
  filter(!grepl("Subtitle translation by", subtitle)) %>%
  filter(!is.na(subtitle))

# 6. 취합된 시나리오를 단어 단위로 unnest처리하고, 불용어 처리
episode_words <- gather_episode %>%
  unnest_tokens(output = word, input = subtitle) %>%
  count(episode,word, sort = TRUE) %>%
  anti_join(stop_words)
#view(episode_words)


# 7. episode_words 데이터 내 에피소드 별 단어 카운트
total_words <- episode_words %>%
  group_by(episode) %>%
  summarize(total = sum(n))
#view(total_words)


# 8. 에피소드 번호 기준 단어의 전체 개수와 total_words를 left_join
episode_words <- left_join(episode_words, total_words)
#view(episode_words)

# 9. df_idf 함수를 사용하여, 각각의 단어가 시나리오(에피소드) 내 tf-idf 산출
episode_words <- episode_words %>% 
  bind_tf_idf(word, episode, n)

# 10. 가중치(tf-idf) 높은 순서로 내림 차순 정렬
episode_words %>% 
  arrange(desc(tf_idf))

# 11. 전체 단어 중 가중치 상위 20개 단어 시각화
episode_words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  top_n(20) %>% 
  ggplot(aes(word, tf_idf, fill = episode)) +
  geom_bar(stat = "identity") +
  coord_flip()

# 12. 시나리오(에피소드)별 가중치 상위 10개 단어에 대한 시각화
episode_words %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(episode) %>% 
  top_n(10) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, tf_idf, fill = episode)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~episode, ncol = 2, scales = "free") +
  coord_flip()


