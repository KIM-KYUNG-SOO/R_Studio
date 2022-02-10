library(KoNLP)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

# presidant_1 노무현
txt <- readLines("./Data/presidant_1.txt",encoding = "UTF-8")
txt
head(txt)

library(stringr)
nouns <- extractNoun(txt)

wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word <- filter(df_word, nchar(word) >= 2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8개 색상을 선택

set.seed(1234)                  ## 난수 고정, 써도되고 안써도됌
wordcloud(words = df_word$word, ## 단어
          freq = df_word$freq,  ## 빈도
          min.freq = 2,        ## 최소 단어 빈도
          max.words = 200,      ## 표현 단어 수
          random.order = F,     ## 고빈도 단어 중앙 배치
          rot.per = .1,         ## 회전 단어 비율
          scale = c(6, 0.3),    ## 단어 크기 범위
          colors = pal)         ## 색깔 목록


# presidant_2 박근혜
txt <- readLines("./Data/presidant_2.txt",encoding = "UTF-8")
txt
head(txt)

library(stringr)
nouns <- extractNoun(txt)

wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word <- filter(df_word, nchar(word) >= 2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8개 색상을 선택

set.seed(1234)                  ## 난수 고정, 써도되고 안써도됌
wordcloud(words = df_word$word, ## 단어
          freq = df_word$freq,  ## 빈도
          min.freq = 2,        ## 최소 단어 빈도
          max.words = 200,      ## 표현 단어 수
          random.order = F,     ## 고빈도 단어 중앙 배치
          rot.per = .1,         ## 회전 단어 비율
          scale = c(8, 0.3),    ## 단어 크기 범위
          colors = pal)         ## 색깔 목록

# presidant_3 이명박
txt <- readLines("./Data/presidant_3.txt",encoding = "UTF-8")
txt
head(txt)

library(stringr)
nouns <- extractNoun(txt)

wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word <- filter(df_word, nchar(word) >= 2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8개 색상을 선택

set.seed(1234)                  ## 난수 고정, 써도되고 안써도됌
wordcloud(words = df_word$word, ## 단어
          freq = df_word$freq,  ## 빈도
          min.freq = 2,        ## 최소 단어 빈도
          max.words = 200,      ## 표현 단어 수
          random.order = F,     ## 고빈도 단어 중앙 배치
          rot.per = .1,         ## 회전 단어 비율
          scale = c(4, 0.3),    ## 단어 크기 범위
          colors = pal)         ## 색깔 목록

# presidant_4 김영삼
txt <- readLines("./Data/presidant_4.txt",encoding = "UTF-8")
txt
head(txt)

library(stringr)
nouns <- extractNoun(txt)

wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word <- filter(df_word, nchar(word) >= 2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8개 색상을 선택

set.seed(1234)                  ## 난수 고정, 써도되고 안써도됌
wordcloud(words = df_word$word, ## 단어
          freq = df_word$freq,  ## 빈도
          min.freq = 2,         ## 최소 단어 빈도
          max.words = 200,      ## 표현 단어 수
          random.order = F,     ## 고빈도 단어 중앙 배치
          rot.per = .1,         ## 회전 단어 비율
          scale = c(8, 0.3),    ## 단어 크기 범위
          colors = pal)         ## 색깔 목록

## 지도 시각화
install.packages("ggiraphExtra")
library(ggiraphExtra)

# 미국 주별 범죄데이터 준비하기

str(USArrests)
head(USArrests)

library(tibble)

# 행 이름을 state 변수로 바꿔 데이터 프레임 생성
crime <- rownames_to_column(USArrests, var = "state")
crime

# 지도 데이터와 동일하게 맞추기 위해 state의 값을 소문자로 수정
crime$state <- tolower(crime$state) # 대문자는 toupper
str(crime)

library(ggplot2)
states_map <- map_data("state")
str(states_map)

# 단계 구분도 만들기
ggChoropleth(data = crime,        # 지도에 표현할 데이터
             aes(fill = Murder,   # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
             map = states_map)    # 지도 데이터

# 인터랙티브 단계 구분도 만들기
ggChoropleth(data = crime,        # 지도에 표현할 데이터
             aes(fill = Murder,   # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
             map = states_map,    # 지도 데이터
             interactive = T)     # 인터랙티브  

# 대한민국 시도별 단계 구분도 만들기
install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)

str(changeCode(korpop1))
# kormaps2014는 utf-8인코딩으로 그대로 사용하면 오류남...
# changeCode() 함수를 이용하여 cp949로 인코딩해줘야함

library(dplyr)
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
str(changeCode(kormap1))

# 단계 구분도 만들기
ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

# 대한민국 시도별 결핵 환자 수 단계 구분도 만들기
#str(changeCode(tbc))
#ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

#str(changeCode(tbc))
#str(changeCode(korpop1))
#head(changeCode(korpop1))
#head(changeCode(tbc))

#test <- left_join(changeCode(korpop1), changeCode(tbc), by = "code")
#head(test)
#str(test)

#tbr_ratio
#str(korpop1)
#summary(changeCode(korpop1$시점))
