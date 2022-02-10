## 텍스트 마이닝
# 문자로 된 데이터에서 가치 있는 정보를 얻어내는 방법
library(KoNLP)
library(dplyr)
useNIADic()

# 데이터 불러오기
txt <- readLines("./Data/hiphop.txt",encoding = "UTF-8")
txt
head(txt)

# 특수문자 제거하기
# install.packages("stringer")
library(stringr)

txt <- str_replace_all(txt, "\\W"," ") # 나머지는 공백처리하겠다.
# \W : 한글, 영문, 숫자를 찾는다
# \\W : 한글, 영문, 숫자를 제외한 나머지를 처리한다.

# 가장 많이 사용된 단어 알아보기
# 명사 추출하기
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")
# 데이터가 리스트 형태로 나온다

# 가사에서 명사추출
nouns <- extractNoun(txt)
nouns

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount

# 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

# 두 글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >= 2)
# nchar 글자수, word = 칼럼명

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

# 워드 클라우드 만들기
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
# R에서 사용할 수 있는 색상 라이브러리

# 단어 색상 목록 만들기
pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8개 색상을 선택

# 워드 클라우드 생성
set.seed(1234)                  ## 난수 고정, 써도되고 안써도됌
wordcloud(words = df_word$word, ## 단어
          freq = df_word$freq,  ## 빈도
          min.freq = 2,         ## 최소 단어 빈도
          max.words = 200,      ## 표현 단어 수
          random.order = F,     ## 고빈도 단어 중앙 배치
          rot.per = .1,         ## 회전 단어 비율
          scale = c(4, 0.3),    ## 단어 크기 범위
          colors = pal)         ## 색깔 목록

# 단어 색상 바꾸기
pal <- brewer.pal(9,"Blues")[5:9] # Dark2 색상 목록에서 8개 색상을 선택

set.seed(1234)                  ## 난수 고정, 써도되고 안써도됌
wordcloud(words = df_word$word, ## 단어
          freq = df_word$freq,  ## 빈도
          min.freq = 2,         ## 최소 단어 빈도
          max.words = 200,      ## 표현 단어 수
          random.order = F,     ## 고빈도 단어 중앙 배치
          rot.per = .1,         ## 회전 단어 비율
          scale = c(4, 0.3),    ## 단어 크기 범위
          colors = pal)         ## 색깔 목록

# 국정원 트윗 텍스트 마이닝
# 국정원 계정 트윗 데이터

# 데이터 준비
# 데이터 로드
twitter <- read.csv("./Data/twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
head(twitter)

# 변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)
str(twitter)

# 특수문자 제거
#twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")

# 명사추출
nouns <- extractNoun(twitter$tw)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount

df_word <- as.data.frame(wordcount, stringsAsFactors = F)

# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

head(df_word)

df_word <- filter(df_word, nchar(word) >= 2)

# 상위 20개 추출
top20 <- df_word %>%
  arrange(desc(freq)) %>% 
  head(20)

library(ggplot2)
order <- arrange(top20, freq)$word               # 빈도 순서 변수 생성

ggplot(data = top20, aes(x = word, y = freq)) +  
  ylim(0, 2500) +
  geom_col() + 
  coord_flip() +
  scale_x_discrete(limit = order) +              # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label = freq), hjust = -0.3)     # 빈도 표시

# 단어 색상 목록 만들기
pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8개 색상을 선택

# 워드 클라우드 생성
set.seed(1234)                  ## 난수 고정, 써도되고 안써도됌
wordcloud(words = df_word$word, ## 단어
          freq = df_word$freq,  ## 빈도
          min.freq = 10,        ## 최소 단어 빈도
          max.words = 200,      ## 표현 단어 수
          random.order = F,     ## 고빈도 단어 중앙 배치
          rot.per = .1,         ## 회전 단어 비율
          scale = c(6, 0.3),    ## 단어 크기 범위
          colors = pal)         ## 색깔 목록

