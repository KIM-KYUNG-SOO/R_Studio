## 데이터 분석프로젝트
## 한국인의 삶을 파악하라
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
getwd()
raw_welfare <- read.spss(file ="./Data/Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
# spss파일을 불러들일때 데이터가 많은경우 오류가 나올수 있다
# warning 오류는 무시하고 가는 경향이 많음
welfare <- raw_welfare
welfare
head(welfare)
tail(welfare)
View(welfare) # 시간이 많이걸림, 많은데이터를 처리하는 경우 쓰지말것
dim(welfare)
str(welfare)
summary(welfare)

# 변수명 바꾸기
welfare <- rename(welfare,
                  sex = h10_g3,           #성별
                  birth = h10_g4,         #태어난 연도
                  marriage = h10_g10,     #혼인상태
                  religion = h10_g11,     #종교
                  income = p1002_8aq1,    #월급
                  code_job = h10_eco9,    #직종코드
                  code_region = h10_reg7) #지역코드

# 성별에 따른 월급차이(성별에 따라 다를까?)
class(welfare$sex)
table(welfare$sex)

# 이상치 결측 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

# 결측치 확인
table(is.na(welfare$sex))

# 성별 항목 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, "male","female")
table(welfare$sex)

qplot(welfare$sex)

# 월급 변수 검토 및 전처리
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)

# 전처리, 이상치 확인
summary(welfare$income)

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% 
                           c(0,9999), NA, welfare$income)

# 결측치 확인
table(is.na(welfare$income))

# 성별에 따른 월급차이 분석하기
# 성별 월급 평균표 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% # 결측치는 제외하도록 필터링 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col()

## 나이와 월급의 관계
## 몇 살 때 월급을 가장 많이 받알까?
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

# 이상치 확인, 결측치 확인
# 파생변수 만들기 - 나이
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$birth)

# 나이에 따른 월급 평균표 만들기
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) + 
  geom_line()

## 연령대에 따른 월급차이
## 어떤 연령대의 월급이 가장 많을까?
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle","old")))
table(welfare$ageg)
qplot(welfare$ageg)

# 연령대에 따른 월급 분석하기
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income
ggplot(data = ageg_income , aes(x = ageg, y = mean_income)) +
  geom_col()

# 막대 정렬: 초년, 중년, 노년 순
ggplot(data = ageg_income , aes(x = ageg, y = mean_income)) +
  geom_col() + 
  scale_x_discrete(limits = c("young","middle","old"))

## 연령대 및 성별 월급 차이
## 성별 월급 차이는 연령대별로 다를까?

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))

sex_income


ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() + 
  scale_x_discrete(limits = c("young","middle","old"))

# 성별 막대분리
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") + 
  scale_x_discrete(limits = c("young","middle","old"))

# 나이대별 연봉차이
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income)) 

sex_age

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + 
  geom_line()

# 직업별로 연봉도 다를까?

library(readxl)
list_job <- read_excel("./Data/Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)

welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>% 
  filter(!is.na(income)) %>%
  select(code_job, job) %>% 
  head(10)

# 직업별 월급 차이 분석하기
# 직업별 월급 평균
job_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
job_income

# 평균월급 top 10추출
top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

# 막대그래프 옆으로 그리기
library(ggplot2)
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() + 
  coord_flip() # 막대그래프 옆으로 그리기기

# 평균 월급 하위 10개 추출
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income)) +
  geom_col() + 
  coord_flip()

## 성별별 직업빈도
# 성별 직업 빈도표 만들기
job_male <- welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male

job_female <- welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

# 그래프 그리기
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

## 종교 변수 검토 및 전처리하기
## 변수 검토하기
class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1,"yes","no")
class(welfare$religion)
table(welfare$religion)

# 결혼여부 변수 검토
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

# 종교 유무에 따른 이혼율 표 만들기
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))
religion_marriage

# count() 활용
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  # count 자체가 그룹핑을하고 카운터를 함
  # group_by위에서 씀
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100/1))

# 이혼율 추출하기, 다시보기
divorce <- religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

divorce

ggplot(data = divorce, aes(x = religion, y = pct)) +
  geom_col()

# 연령대별 이혼율 표 만들기
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

ageg_marriage

# count 활용
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100,1))

ageg_marriage

# 초년 제외, 이혼율 추출, 다시보기
ageg_divorce <- ageg_marriage %>% 
  filter(ageg != "young" & group_marriage == "divorce") %>% 
  select(ageg, pct)

ageg_divorce
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) +
  geom_col()

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

ageg_religion_marriage

df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(ageg, religion, pct)

df_divorce

ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) + 
  geom_col(position = "dodge")

## 지역별 연령대 비율
## 노년층이 많은 지역은 어디일까?
class(welfare$code_region)
table(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
# 원시데이터에 조인할만한 변수가 없기 때문에
# 데이터 프레임으로 만듬

welfare <- left_join(welfare,list_region, id = "code_region")
welfare %>% 
  select(code_region, region) %>% 
  head

region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

head(region_group)


ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()


list_order_old <- region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)
list_order_old

# 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old","middle","young"))
##-> 그래프 빠트림....

## 텍스트 마이닝
## 문자로 된 데이터에서 가치 있는 정보를 얻어 내는 분석 기법
## 분석절차
## - 형태소 분석
## - 명사, 동사 형용사 등 의미를 지닌 품사 단어 추출
## - 빈도표 만들기
## - 시각화

## KONLP 패키지를 사용하기 위한 사전 준비사항
## 1. JDK 1.8설치 > 설치 후 시스템환경에 JAVA_HOME 등록 > path=에 bin 설정

## 2. RTools4.0 패스 설정 > 아래 순서대로 진행
## - 실행 : writeLines('PATH='${RTOOL})
## 
install.packages("usethis")
usethis::edit_r_environ()
## PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"을 써준다
Sys.which("make")

install.packages("rJava")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
text <- "R은 통계 계산과 그래픽을 위한 프로그래밍 언어이자 소프트웨어 환경이자 프리웨어이다.[2] 뉴질랜드 오클랜드 대학의 로버트 젠틀맨(Robert Gentleman)과 로스 이하카(Ross Ihaka)에 의해 시작되어 현재는 R 코어 팀이 개발하고 있다. R는 GPL 하에 배포되는 S 프로그래밍 언어의 구현으로 GNU S라고도 한다. R는 통계 소프트웨어 개발과 자료 분석에 널리 사용되고 있으며, 패키지 개발이 용이해 통계 소프트웨어 개발에 많이 쓰이고 있다."
extractNoun(text)
library(dplyr)
# useNIADic()
getwd()
txt <- readLines("./Data/hiphop.txt",encoding = "UTF-8")
txt
head(txt)
