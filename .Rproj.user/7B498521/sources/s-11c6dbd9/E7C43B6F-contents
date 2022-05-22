$)C############################################################
install.packages("srt")
library(srt)

s1 <- read_srt("/cloud/project/Squid.Game.S01E01.Red.Light,.Green.Light.WEBRip.Netflix.en.srt")
s2 <- read_srt("/cloud/project/Squid.Game.S01E02.Hell.WEBRip.Netflix.en.srt")
s3 <- read_srt("/cloud/project/Squid.Game.S01E03.The.Man.with.the.Umbrella.WEBRip.Netflix.en.srt")
s4 <- read_srt("/cloud/project/Squid.Game.S01E04.Stick.to.the.Team.WEBRip.Netflix.en.srt")
s5 <- read_srt("/cloud/project/Squid.Game.S01E05.A.Fair.World.WEBRip.Netflix.en.srt")
s6 <- read_srt("/cloud/project/Squid.Game.S01E06.Gganbu.WEBRip.Netflix.en.srt")
s7 <- read_srt("/cloud/project/Squid.Game.S01E07.VIPS.WEBRip.Netflix.en.srt")
s8 <- read_srt("/cloud/project/Squid.Game.S01E08.Front.Man.WEBRip.Netflix.en.srt")
s9 <- read_srt("/cloud/project/Squid.Game.S01E09.One.Lucky.Day.WEBRip.Netflix.en.srt")

str(s1)
glimpse(s1)

######### 1번 #########################################
library(dplyr)
eng_text_df1 <- tibble(text = s1)
summary(eng_text_df1)

eng_text_df1 %>% rename("subtitle" = "text.subtitle")

eng_text_df1
class(eng_text_df)


######### 2번 ############################
head(eng_text_df1)
mutate(eng_text_df1, episode = 1)



######## 3번 #############################
eng_text_df1 <- tibble(text = s1)
eng_text_df_1<- mutate(eng_text_df1, episode = 1)
eng_text_df_re_1 <- mutate (s1, episode = 1)

eng_text_df2 <- tibble(text = s2)
eng_text_df_2 <- mutate(eng_text_df2, episode = 2)
eng_text_df_re_2 <- mutate (s2, episode = 2)

eng_text_df3 <- tibble(text = s3)
eng_text_df_3 <-mutate(eng_text_df3, episode = 3)
eng_text_df_re_3 <- mutate (s3, episode = 3)

eng_text_df4 <- tibble(text = s4)
eng_text_df_4 <- mutate(eng_text_df4, episode = 4)
eng_text_df_re_4 <- mutate (s4, episode = 4)

eng_text_df5 <- tibble(text = s5)
eng_text_df_5 <- mutate(eng_text_df5, episode = 5)
eng_text_df_re_5 <- mutate (s5, episode = 5)

eng_text_df6 <- tibble(text = s6)
eng_text_df_6 <- mutate(eng_text_df6, episode = 6)
eng_text_df_re_6 <- mutate (s6, episode = 6)


eng_text_df7 <- tibble(text = s7)
eng_text_df_7 <- mutate(eng_text_df7, episode = 7)
eng_text_df_re_7 <- mutate (s7, episode = 7)

eng_text_df8 <- tibble(text = s8)
eng_text_df_8 <- mutate(eng_text_df8, episode = 8)
eng_text_df_re_8 <- mutate (s8, episode = 8)

eng_text_df9 <- tibble(text = s9)
eng_text_df_9 <- mutate(eng_text_df9, episode = 9)
eng_text_df_re_9 <- mutate (s9, episode = 9)

eng_text_df_9

############ 4번 ############################

eng_text_df <- rbind(eng_text_df_1, eng_text_df_2, eng_text_df_3, eng_text_df_4, eng_text_df_5, eng_text_df_6, eng_text_df_7, eng_text_df_8, eng_text_df_9)
eng_text_df_re <- rbind(eng_text_df_re_1, eng_text_df_re_2, eng_text_df_re_3, eng_text_df_re_4, eng_text_df_re_5, eng_text_df_re_6, eng_text_df_re_7, eng_text_df_re_8, eng_text_df_re_9)

############ 5번 ############################
2. dplyr 패키지를 통해, 에피소드, 제작자 정보가 있는 로우를 삭제 해 준다.

########### 6번 #############################
eng_text_df_re <- eng_text_df_re %>% 
  filter(!grepl("EPISODE", subtitle)) %>%
  filter(!grepl("Subtitle translation by", subtitle)) %>%
  filter(!is.na(subtitle))

eng_text_df_re %>% gsub("[[:punct:]]", "", data)

######### 7번 #############################
install.packages("tidytext")
library(tidytext)

eng_text_df_tokens <- eng_text_df_re %>%
  unnest_tokens(word, subtitle)
  

eng_text_df_tokens <- eng_text_df_re %>%
  unnest_tokens(word, subtitle) %>%
  anti_join(stop_words)

######### 8번 ############################################
df_re_1_count <- filter(eng_text_df_tokens, episode == 1) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

######### 9번 ############################################
install.packages("ggplot2")
library(ggplot2)

df_re_1_count %>%
  ggplot(aes(x= reorder(word, n),  y=n)) + 
  geom_bar(stat="identity", width=0.8, fill="#CC3399") +
  ggtitle("epi1 단어 빈도수") +
  labs(x="단어", y="빈도 수") + 
  coord_flip()

######## 10번 ############################################
df_re_2_count <- filter(eng_text_df_tokens, episode == 2) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

df_re_3_count <- filter(eng_text_df_tokens, episode == 3) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

df_re_4_count <- filter(eng_text_df_tokens, episode == 4) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

df_re_5_count <- filter(eng_text_df_tokens, episode == 5) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

df_re_6_count <- filter(eng_text_df_tokens, episode == 6) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

df_re_7_count <- filter(eng_text_df_tokens, episode == 7) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

df_re_8_count <- filter(eng_text_df_tokens, episode == 8) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

df_re_9_count <- filter(eng_text_df_tokens, episode == 9) %>% 
  count(word, sort = TRUE) %>% 
  head(20)



ggplot(df_re_1_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi1 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_2_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi2 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_3_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi3 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_4_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi4 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_5_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi5 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_6_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi6 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_7_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi7 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_8_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi8 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()

ggplot(df_re_9_count, aes(x= reorder(word, n),  y=n)) + geom_bar(stat="identity", width=0.8, fill="#FFCC00") +
  ggtitle("Epi9 단어 빈도수") + labs(x="단어", y="빈도 수") + coord_flip()
