library(kormaps2014)
library(dplyr)
library(ggplot2)
library(ggiraphExtra)

# 2015년 시도별 인구 및 가구 현황
korpop1_copy <- changeCode(korpop1)
korpop1_copy <- rename(korpop1_copy,
                       family_tot = 가구_계_가구)

# 대한민국 시도별 결핵 환자 수 데이터(2001~2015년)
tbc_copy <- changeCode(tbc)
str(korpop1_copy)
str(tbc_copy)
table(korpop1_copy$시점)
table(tbc$year)

# 2015년 결핵 환자 수 데이터 추출
tbc_2015 <- tbc_copy %>% 
  filter(!is.na(NewPts) & year == 2015)
tbc_2015

# 2015년 총인구수, 결핵환자 데이터(korpop1_copy, tbc_copy 조인)
test1 <- left_join(korpop1_copy, tbc_2015, by = "code")
test1
str(test1)
table(test1$NewPts)

# 1. 2015년 지역별 결액 환자 비율 구하기(1/10000)
test2 <- test1 %>%
  filter(!is.na(NewPts)) %>%
  select(code,name.x, pop, NewPts) %>% 
  group_by(name.x) %>%
  mutate(ratio = as.numeric(NewPts)/as.numeric(pop)*10000)
test2

# 지도에 표시하기
ggChoropleth(data = test2,
             aes(fill = ratio,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

# 2. 2015년 지역별 한가구당 인원수
test3 <- korpop1_copy %>%
  select(code, name, pop, family_tot) %>% 
  mutate(memb = as.numeric(pop)/as.numeric(family_tot))
test3

# 지도에 표시하기
ggChoropleth(data = test3,
             aes(fill = memb,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

# 3. 지역별 결액 환자 평균(2001~2015)
test4 <- tbc_copy %>% 
  filter(!is.na(NewPts)) %>%
  select(code, name, NewPts) %>% 
  group_by(name) %>% 
  mutate(mean_NewPts = mean(as.numeric(NewPts)))
test4
table(test4$mean_NewPts)

# 지도에 표시하기
ggChoropleth(data = test4,
             aes(fill = mean_NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)


# 4. 연도별 결액 환자 평균
test5 <- tbc_copy %>% 
  filter(!is.na(NewPts)) %>% 
  group_by(year) %>%
  summarize(mean_NewPts = mean(as.numeric(NewPts)))
test5

# 차트그리기
ggplot(data = test5, aes(x = mean_NewPts , y = year )) + 
  geom_col()
