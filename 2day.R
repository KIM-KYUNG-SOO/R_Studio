## 그래프 만들기
library(ggplot2)

# 배경설정하기
ggplot(data = mpg, aes(x = displ, y = hwy))
# mpg data를 사용하며, x축 displ은 y축 hwy를 사용

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
# geom_point() -> 산점도를 찍겠다

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + xlim(3,6)
# x축의 범위를 조정 y축은 ylim()

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + xlim(3,6) + ylim(10,30)
# 현재까지 layer 2개

# 문제
# 1번
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

# 2번
midwest <- as.data.frame(ggplot2::midwest)
str(midwest)

ggplot(data = midwest, aes(x = poptotal, y = popasian)) + 
  geom_point() + xlim(0,500000) + ylim(0,10000)

# 막대그래프 - 집단 간 차이 표현하기
# 하나의 컬럼을 통해 두개의 데이터를 비교가능
library(dplyr)

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_mpg
df_mpg <- na.omit(df_mpg) # 모든 변수에 결측치 없는 데이터 추출
# 그래프 생성하기
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + 
  geom_col()

# 크기순으로 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + 
  geom_col()

# 빈도 막대그래프 그리기
ggplot(data = mpg, aes(x = drv)) + geom_bar()
# 하나의 컬럼에서 그 범주의 빈도수를 나타냄
# 칼럼내 여러 데이터 범주의 빈도수를 알고싶을때 사용

# 문제
# 1번
str(mpg)
mpg_suv <- mpg %>% filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)
mpg_suv

ggplot(data= mpg_suv, aes(x = reorder(manufacturer, -mean_cty), 
                          y = mean_cty))+
  geom_col()

@ 2번
ggplot(data = mpg, aes(x = class)) + geom_bar()

# 시계열 그래프 그리기 - 선그래프
# 연속성이 있는 그래프

library(ggplot2)
library(dplyr)
ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line()
# 그래프의 패턴을 파악하고 그에 따른 원인까지 찾는것이 중요!

# 문제
str(economics)
ggplot(data = economics, aes(x = date, y = psavert)) +
  geom_line()

# 상자 그림 그리기
ggplot(data = mpg, aes(x=drv, y=hwy)) +
  geom_boxplot()
mpg <- data.frame(ggplot2::mpg)
mpg1 <- mpg %>% 
  filter(class %in% c("compact","subcompact","suv"))
ggplot(data = mpg1, aes(x = class, y = cty)) +
  geom_boxplot()