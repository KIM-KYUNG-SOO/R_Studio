getwd()
library(readxl)

exam <- read.csv("./Data/csv_exam.csv")
exam
## 데이터 가공
## 불러온 데이터를 확인하기

head(exam)
tail(exam)
# 기본 상위, 하위 6개를 보여줌
head(exam, 10)
tail(exam, 10)

View(exam)
# V는 대문자로 써야함

dim(exam) #데이터구조, 차원을 알려줌
str(exam)
summary(exam)
# 문자값이 있는 컬럼은 배제한다


mpg <- as.data.frame(ggplot2::mpg)
# ggplot2의 mpg 데이터를 데이터 프레임 형태로 불러오기
# 내장되어있는 파일의 경우 위와같이 데이터프레임형태로만든다
mpg
head(mpg,10)
str(mpg)
summary(mpg)

install.packages("dplyr")
library(dplyr)
# 데이터 전처리(Preprocessing)- dplyr패키지

df_raw <- data.frame(var1 = c(1,2,1), var2 = c(2,3,2))
df_raw
df_new <- df_raw

df_new <- rename(df_new, v2 = var2)
# 컬럼명의 변경
df_new

df <- data.frame(var1 = c(4,3,8), var2 = c(2,6,1))
df

# var_sum 파생변수 생성
df$var_sum <- df$var1 + df$var2
df$var_meam <- (df$var1 + df$var2)/2
# df$칼럼명
df

mpg
mpg$total <- (mpg$cty + mpg$hwy)/2
# 통합 연비 변수 생성
head(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total) # 히스토그램 생성

mpg$test <- ifelse(mpg$total >= 20, "pass","fail")
head(mpg,20)

table(mpg$test)

library(ggplot2)
qplot(mpg$test)
qplot(mpg$test)
# 연비 합격 빈도 막대 그래프 생성

mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total > 20,"B","C"))
head(mpg,10)
qplot(mpg$grade)
table(mpg$grade)

midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)

# 2,3번문제
head(midwest)
midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)
midwest$per <- (midwest$asian / midwest$total)*100

# 4번문제
mean(midwest$per)
midwest$x1 <- ifelse(midwest$per > mean(midwest$per),"large","small")

# 5번문제
table(midwest$x1)
qplot(midwest$x1)

# 자유자재로 데이터 가공하기(filtering)
exam
exam %>% filter(class == 1)
# %>% 단축키 Ctrl+Shift+M
# 해석은 ~중에서로 한다
# exam변수중에서~

exam %>% filter(class != 1)
# 1반이 아닌 나머지반

exam %>% filter(class == 1 & math >= 50)
# 여러 조건을 충족하는 행 추출하기 'AND'

exam %>% filter(math >= 90 | english >= 50)
# 논리연산자 'OR'

exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(class %in% c(1,3,5))
# 같은 결과값이다.

class1 <- exam %>% filter(class == 1)
mean(class1$math)


# 1번문제: 배기량이 4이하인 제품이 연비가 높다
mpg
mpg1 <- mpg %>% filter(displ <= 4)
mpg1
mean(mpg1$hwy)
mpg2 <- mpg %>% filter(displ >= 5)
mean(mpg2$hwy)

# 2번문제: toyota가 도시연비가 높다
mpg3 <- mpg %>% filter(manufacturer == "audi")
mpg4 <- mpg %>% filter(manufacturer == "toyota")
mean(mpg3$cty)
mean(mpg4$cty)

# 3번문제
mpg5 <- mpg %>% filter(manufacturer == "chevrolet" 
                       | manufacturer == "ford" 
                       | manufacturer == "honda")
mean(mpg5$hwy)

exam %>% select(math)
# 특정칼럼(math)만 추출

exam %>% select(class, math,english)
# 여러 칼럼 추출하기

exam %>%  select(-math)
# 특정칼람만 제외하기

exam %>%  select(-math, -english)
# 여러개의 칼럼 제외하기

# class가 1인행만 추출한 다음 english 추출
exam %>% 
  filter(class == 1) %>% 
  select(english)

exam %>% 
  select(id, math) %>% 
  head

# 1번문제
mpg6 <- mpg %>% select(class,cty)

# 2번문제
df_suv <- mpg6 %>% filter(class == "suv")
df_compact <- mpg6 %>% filter(class == "compact")
mean(df_suv$cty)
mean(df_compact$cty)

# 오름차순으로 정렬하기
exam %>% arrange(math)
exam %>% arrange(desc(math)) # 내림차순

exam %>% arrange(class, math)

# filter, arrange, head의 연계
mpg
str(mpg)
mpg %>% filter(manufacturer == "audi") %>% 
  arrange(hwy) %>% head(5)

# 파생변수 만들기 mutate는 조회기능
# 실제로 만들어지는 것은 아니다
exam %>% 
  mutate(total = math + english, science) %>% 
  head

# 여러 파생변수를 한번에 만들기
exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>% 
  head

exam %>% mutate(test = ifelse(science >= 60, "pass","fail")) %>% 
  head

exam

# 1~3번문제
mpg_copy <- mpg %>% mutate(total_hwy = cty + hwy)
mpg_copy %>% mutate(mean_hwy = total_hwy/2) %>% 
  arrange(desc(mean_hwy)) %>% head(3) 

# 4번문제
mpg %>% mutate(total_hwy = cty + hwy, 
               mean_hwy = (total_hwy)/2) %>% 
  arrange(desc(mean_hwy)) %>% head(3)

# 집단별로 요약하기
# 요약하기
exam %>% summarise(mean_math = mean(math))

# 집단별로 요약하기
exam %>% 
  group_by(class) %>% # 클래스별로 분리
  summarise(mean_math = mean(math)) # math 평균 산출
# 각 학급별 수학점수 평균

# 여러 통계량 한번에 요약하기
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math), 
            sum_math = sum(math), 
            madian_math = median(math)
            , n = n()) # n(): 빈도

mpg %>% 
  group_by(manufacturer, drv) %>% # 회사별, 구동방식별로 분리
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

# 문제
str(mpg)
mpg %>% group_by(manufacturer, class) %>% 
  filter(class == "suv") %>% 
  mutate(total_sum = cty + hwy) %>% 
  summarise(total_mean = mean(total_sum)) %>% 
  arrange(desc(total_mean)) %>% 
  head(5)

# 문제
# 1번
mpg %>% group_by(class) %>% 
  summarise(mean = mean(cty))

# 2번
mpg %>% group_by(class) %>% 
  summarise(mean = mean(cty)) %>% 
  arrange(desc(mean))

# 3번
mpg %>% group_by(manufacturer) %>% 
  summarise(mean = mean(hwy)) %>% 
  arrange(desc(mean)) %>% head(3)

# 4번
mpg %>% group_by(manufacturer,class) %>% 
  filter(class == "compact") %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# 데이터 합치기
test1 <- data.frame(id = c(1,2,3,4,5), 
                    midterm = c(60,80,70,90,85))
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,80))
test1
test2

total <- left_join(test1, test2, by = "id")
total

# by 뒤에 칼럼명은 ""를 붙여야한다
# 합치려는 칼럼에 by뒤에 칼럼이 공통으로 있어야한다

# 다른 데이터를 활용해 변수 추가하기
name <- data.frame(class = c(1,2,3,4,5),
                    teacher = c("kim","lee","park","choi","jung"))
name

exam_new <- left_join(exam, name, by = "class")
exam_new

# 새로로 붙이기
group_a <- data.frame(id = c(1,2,3,4,5), 
                      test=c(60,80,70,90,85))
group_b <- data.frame(id = c(6,7,8,9,10), 
                      test = c(70,83,65,95,80))

group_all <- bind_rows(group_a, group_b)
group_all

# 문제
fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = (c(2.35,2.38,2.11,2.76,2.22)),
                   stringsAsFactors = F)
fuel
str(mpg)
mpg_col <- left_join(mpg, fuel ,by = "fl")
mpg_col %>% select(model,fl,price_fl) %>% head(5)

## 데이터 정제
## 빠진 데이터, 이상한 데이터의 처리
## 결측치(Missing Value): 누락, 함수적용 불가, 분석결과 왜곡값

df <- data.frame(sex = c("M","F",NA,"M","F"),
                 score = c(5,4,3,4,NA))
df

# 결측치 확인
is.na(df) #전체행을 체크함
table(is.na(df)) # 결측치 빈도 출력
# T -> 결측치

table(is.na(df$sex)) # 칼럼별로 결측치 확인

mean(df$score)
sum(df$score)
# 데이터에 결측치가 하나라도 있으면 계산이 안됨
# 결측치가 우선

df %>% filter(is.na(score)) # na가 있는 값만 필터링

df %>% 
  filter(!is.na(score)) #score에 na 제외한 값들만 필터링


df_nomiss=df %>% filter (!is.na(score))
mean(df_nomiss$score)

sum(df_nomiss$score)

# filter select 헷갈릴 때:
# 조건이 있으면 filter, 없으면 select

#score , sex 결측치 제외(행단위)
df_nomiss=df %>% filter (!is.na(score) & !is.na(sex)) # &: 그리고 or
df_nomiss

# 결측치 하나라도 있으면 제거(행 자체 없앰)
df_nomiss2= na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2


# 일반적으로 컬럼 한개씩 결측치 있는지 파악

# 함수의 결측치 제외 기능 이용하기 - na.rm=T
mean(df$score, na.rm=T) # 결측치 제외하고 평균 산출

sum(df$score, na.rm=T) # 결측치 제외하고 합계 산출

exam
exam[c(3,8,15),"math"]=NA # math의 3,8,15행 요소 NA
exam

# 결측지 제외하고난 뒤 평균산출
exam %>% 
  summarise(mean_math= mean(math,na.rm=T)) # 혹시가 결측치 있다면 제외해줘!

# NA 값 대체하기
# 평균으로 대체
exam %>% 
  summarise(mean_math= mean(math,na.rm=T)) # 평균 55점임을 확인

# 중요!!!!!!!
exam$math=ifelse(is.na(exam$math),55,exam$math) # NA를 평균점수 55로대체
exam

head(mpg)
mpg[c(65,124,131,153,212),"hwy"]=NA

mpg %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T)) # 평균 23.5

mpg %>% filter(is.na(hwy))

table(is.na(mpg$hwy))
table(is.na(mpg$drv))

mpg %>% 
  filter(!is.na(hwy))

mpg %>% filter(is.na(drv))

mpg %>% 
  filter(!is.na(hwy)) %>% # hwy 중 na 아닌 것만 걸러준다음
  group_by(drv) %>% # 그룹화하고
  summarise(mean_hwy = mean(hwy)) # 평균 나타냄

##############이상치 제거 - 정상범주에서 크게 벗어난 값

# 이상치 확인하기


# 결측 처리하기 - sex
# sex가 3이면 NA 할당

outlier = data.frame(sex = c(1,2,1,3,2,1),
                     score = c(5,4,3,4,2,6))

outlier$sex = ifelse(outlier$sex ==3,NA, outlier$sex)

# 결측 처리하기- score
# sex가 1~5 아니면 NA 할당
outlier$score = ifelse(outlier$score >5,NA, outlier$score)

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

# 이상치 제거하기-극단적인 값
# 정상범위 기준 정해서 벗어나면 결측 처리
# 논리적 판단 or 통계적 판단

# 문제 1번
is.na(mpg$drv)

# boxplot으로 극단치 기준 정해서 제거하기

mpg=as.data.frame(ggplot2::mpg)
mpg
boxplot(mpg$hwy)$stats # $stats: boxplot 통계치 출력력
# 극단치 최상위값, 최하위값 알 수있다. # 12,37

# 12~37 벗어나면 NA할당
mpg$hwy= ifelse(mpg$hwy <12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

# 결측치 제외하고 분석하기
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

# 문제
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),"drv"] <-"k"
mpg[c(29,43,129,203), "cty"] <- c(3,4,39,42)
mpg

str(mpg$drv)
table(mpg$drv)
# 1번
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"),mpg$drv,NA)

# 2번
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
summary(mpg$cty)

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
table(is.na(mpg$hwy))

mpg %>% filter(!is.na(cty) & !is.na(drv)) %>%
  group_by(drv) %>%
  summarise(mean_cty = mean(cty))