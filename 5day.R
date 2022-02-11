###### 인터랙티브 그래프
install.packages("plotly")
library(plotly) # 인터렉티브를 위한 패키지
library(ggplot2)

p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) +
  geom_point()
p
# 인터랙티브 그래프 만들기
ggplotly(p)

# 인터렉티브 막대그래프 만들기
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")
p
ggplotly(p)

# dygraphs 패키지로 시계열 그래프 만들기
install.packages("dygraphs")
library(dygraphs)

library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)

dygraph(eco)

# 날짜 범위 선택기능
dygraph(eco) %>% dyRangeSelector()

# 여러값 표현하기
# 저축률
eco_a <- xts(economics$psavert, order.by = economics$date)

# 실업자 수
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)

# 합치기
eco2 <- cbind(eco_a, eco_b)                # 데이터 결합
colnames(eco2) <- c("psavert", "unemploy") # 변수명 바꾸기
head(eco2)

dygraph(eco2) %>% dyRangeSelector()

###### 오라클 접속하기 ######
install.packages("RJDBC")
library(RJDBC)

# Data Base
# Server: 요청에 대한 응답, Client: 요청
# 오라클 드라이버 연결 경로 설정
driver <- JDBC("oracle.jdbc.OracleDriver",
               classPath = "C:/DEV/SERVER/Oracle/product/12.2.0/dbhome_1/jdbc/lib/ojdbc8.jar")
driver

# 오라클 접속하기
conn <- dbConnect(driver,
                  "jdbc:oracle:thin:@//localhost:1521/orcl",
                  "busan", "dbdb")
conn

sql_in <- paste("Insert into test",
                "(AA, BB, CC)",
                "values('a1','b1','c1')")
sql_in

in_stat = dbSendQuery(conn, sql_in)
in_stat

dbClearResult((in_stat))

sql_sel <- "Select*From test Where AA = 'a1'"
sql_sel

getData <- dbGetQuery(conn, sql_sel)
getData

getData$AA
str(getData)
# 중요, 필수!! 무조건 오라클 접속해제하기
dbDisconnect(conn)

## 빅데이터 자료 수집
# 1. 파일 데이터셋 자료수집: 가장 많이 씀
# 2. 웹 스크래핑(웹크롤링)
# 3. 오픈 API기반 자료 수집

# 파일 데이터셋은 다양한 기관이 공익적인 목적에서 제공하는
# 파일데이터 셋을 통하여 자료를 수집

# 일반적으로 파일 데이터셋은 정형데이터와 비정형데이터로 구분되며,
# 정형데이터는 csv나 엑셀파일 형식으로 제공, 비정형데이터는 텍스트파일로 제공
# 파일데이터 셋
getwd()
library(readxl)
data <- read.csv("./Data/전라남도_목포시_장애인_복지시설_20210802.csv",
                 header = T,
                 fileEncoding = "EUC-KR")
data
# 엑셀 파일의 경우 엑셀에서 csv파일로 변경하여 사용

###### 웹스크래핑(웹크롤링) ######
install.packages("rvest") # 웹스크래핑 패키지
install.packages("stringr") # 문자열 처리 패키지

# 순서
# 1. 웹스크래핑 대상 URL할당
# 2. 웹 문서 가져오기
# 3. 특정 태그의 데이터 추출
# 4. 데이터 정제
# 5. 데이터 프래임 만들기

library(rvest)
library(stringr)

###### 웹스크래핑 ######
url <- "http://www.bobaedream.co.kr/cyber/CyberCar.php?gubun=K&page=1"
url

# 웹 문서 가져오기
usedCar <- read_html(url)
usedCar

# 특정 태그의 데이터 추출
# 가져운 usedCar에서 css가 product-item을 찾음
carInfos <- html_nodes(usedCar, css=".product-item")
head(carInfos)

# 차량 명칭 추출
title_tmp <- html_nodes(carInfos, css=".tit.ellipsis")
title_tmp

title <- html_text(title_tmp)
title

# 데이터 정제
title <- str_trim(title) # 문자열에서 공백 제거
title

# 차량 연식 추출
year_tmp <- html_nodes(carInfos, css=".mode-cell.year")
year_tmp

year <- html_text(year_tmp)
year

year <- str_trim(year)
year

# 연료구분
fuel_tmp <- html_nodes(carInfos, css=".mode-cell.fuel")
fuel_tmp

fuel <- html_text(fuel_tmp)
fuel

fuel <- str_trim(fuel)
fuel

# 주행거리 추출
km_tmp <- html_nodes(carInfos, css=".mode-cell.km")
km_tmp

km <- html_text(km_tmp)
km

km <- str_trim(km)
km

# 판매가격 추출
price_tmp <- html_nodes(carInfos, css=".mode-cell.price")
price_tmp

price <- html_text(price_tmp)
price

price <- str_trim(price)
price

price <- str_replace(price, '\n','') #문자열 변경(\n을 스페이스로 변경)
price

# 차량 명칭으로부터 제조사 추출
maker = c() # 임의의 빈공간을 만들어놓음
maker

for(i in 1:length(title)){
  maker <- c(maker, unlist(str_split(title[i],' '))[1])}
# str_split 문자열 분리
# 공백을 기준으로 잘라내라
# split은 자른데이터를 리스트 구조로 만든다
# unlist: 리스트가 아닌 문자열로 넣겠다.
maker
str(maker)

# 데이터 프레임 만들기
usedcars <- data.frame(title,year,fuel,km,price,maker)
View(usedcars)

# 데이터 정제
# km 자료 숫자로 변경
usedcars$km

usedcars$km <- gsub("만km","0000",usedcars$km) #문자열 변환("만km"->"000")
usedcars$km <- gsub("천km","000",usedcars$km)
usedcars$km <- gsub("km","",usedcars$km)
usedcars$km <- gsub("미등록","",usedcars$km)
usedcars$km <- as.numeric(usedcars$km) # 숫자형으로 타입변경

usedcars$km

# price 자료 숫자로 변경
usedcars$price <- gsub("만원","",usedcars$price)
usedcars$price <- gsub("계약","",usedcars$price)
usedcars$price <- gsub("팔림","",usedcars$price)
usedcars$price <- gsub("금융리스","",usedcars$price)
usedcars$price <- gsub(",","",usedcars$price)
usedcars$price <- as.numeric(usedcars$price)
usedcars$price

View(usedcars)

# 웹 스크래핑 자료 파일로 저장하기
# 디렉터리 설정
write.csv(usedcars, "./Data/usedcars_new.csv")

###### 오픈 API기반 자료 수집하기 ######
# 오픈API를 XML 형식으로 변경하는 함수를 지원하는 패키지
install.packages("XML")
library(XML)

# API 자료수집순서
# 1. 오픈API 제공 웹사이트 접속 및 로그인
# 2. 오픈API 자료 검색
# 3. 활용신청 및 개발계정 API키 신청
# 4. 승인 받은 개발계정 API키 확인
# 5. 오픈 API 접속을 위한 웹 URL 및 요청변수 확인
# 6. R에서 오픈 API를 이용한 자료요청
# 7. 데이터프레임 만들기

# 웹사이트 URL 설정
api_url <- "http://apis.data.go.kr/B552584/ArpltnInforInqireSvc/getCtprvnRltmMesureDnsty"

# 승인 받은 KEY 등록
service_key <- "wdfwEVqMaPXmUTDHwlPclF0uSl1hgiIOafNaejxtlutPb0A3aDEKita1JdL1WB0NThxH1mHsr8k8UBBBk1EFyQ%3D%3D"

# 요청변수 등록
numOfRows <- "30"
sidoName <- "경기"

sidoName <- URLencode(iconv(sidoName, to ="UTF-8")) # 한글을 웹 URL코드화
sidoName

searchCondition <- "DAILY"
paste("a","b","c") # 공백을 구분자로 묶기
paste0("a","b","c") # 구분자 공백없이 모두 묶기

# URL 주소를 공백없이 모두 묶기
open_api_url <- paste0(api_url,"?serviceKey=",service_key,
                       "&numOfRows=",numOfRows,
                       "&sidoName=",sidoName,
                       "&searchCondition=",searchCondition)
open_api_url

raw.data <- xmlTreeParse(open_api_url,
                             useInternalNodes = TRUE,
                             encoding = "utf-8")
raw.data

# response 뿌리
# body 파생
# items
# item : 우리가 가져와야할 내용들이 들어있는곳

# XML 형식의 자료를 데이터프레임으로 변경하기
# </item> 태그(노드) 별로 데이터 구분하기
# 테그 하나하나를 node라고 한다
air_pollution <- xmlToDataFrame(getNodeSet(raw.data," //item"))
air_pollution

View(air_pollution)

# subset(): 데이터프레임 내에서 검색 조건(select)에 맞는
# 항목(컬럼)들만 가지고 오기
air_pollution <- subset(air_pollution,
                        select = c(dataTime,
                                   stationName,
                                   so2Value,
                                   coValue,
                                   o3Value,
                                   no2Value,
                                   pm10Value))
View(air_pollution)
write.csv(air_pollution, "./Data/air_pollution_new.csv") #csv로 저장

###### 실습 ######