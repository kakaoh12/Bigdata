library(rJava)      # 자바 지원
library(xlsx)       # 엑셀 파일 읽고 쓰고 저장하기
library(readxl)     # 엑셀 파일 불러오기
library(ggplot2)    # 시각화
library(dplyr)      # 전처리
library(foreign)    # SPSS 파일 불러오기


exam1 <- read.csv("sample/시군구 지역코드.csv",  stringsAsFactors = FALSE)
exam2 <- read.csv("sample/시도 지역코드.csv")
exam3 <- read.csv("sample/실제진료정보_감기_시군구.csv")
exam4 <- read.csv("sample/실제진료정보_감기_시도.csv")
exam5 <- read.csv("sample/실제진료정보_눈병_시군구.csv")
exam6 <- read.csv("sample/실제진료정보_눈병_시도.csv")
exam7 <- read.csv("sample/실제진료정보_천식_시군구.csv")
exam8 <- read.csv("sample/실제진료정보_천식_시도.csv")
exam9 <- read.csv("sample/실제진료정보_피부염_시군구.csv")
exam10 <- read.csv("sample/실제진료정보_피부염_시도.csv")

감기1 <- exam3
감기2 <- exam4

천식1 <- exam7
천식2 <- exam8


# 감기1$날짜 <- ifelse(감기1$날짜 < 20150101, "2014년", 
#                   ifelse(감기1$날짜 < 20160101, "2015년",
#                            ifelse(감기1$날짜 < 20170101, "2016년", 
#                                   ifelse(감기1$날짜 < 20180101, "2017년",
#                                            ifelse(감기1$날짜 < 20190101, "2018년", "2019년")))))

str(감기1)
str(exam1)

cold1 <- left_join(감기1, exam1, by = "시군구지역코드")


# 연도별 빈도수
# 
# test1 <- cold1  %>% 
#     select(날짜, 발생건수.건., 시군구명) %>% 
#     group_by(날짜) %>% 
#     summarise(mean_cold1 = mean(발생건수.건.))
# 
# test2 <- cold1  %>% 
#     select(날짜, 발생건수.건., 시군구명) %>% 
#     group_by(시군구명) %>% 
#     summarise(mean_cold1 = mean(발생건수.건.))
# 
# 
# 
# ggplot(data = test1, aes(x = 날짜, y = mean_cold1)) + geom_col()
# ggplot(data = test2, aes(x = 시군구명, y = mean_cold1)) + geom_col()


# 
# test <- cold1 %>% 
#     select(날짜, 발생건수.건., 시군구명)%>% 
#     count(날짜, 발생건수.건.) %>% 
#     group_by(날짜) %>% 
#     mutate(pct = round(n/sum(n)*100, 2))
# 
# ggplot(data = test, aes(x= 날짜, y = pct, fill = 발생건수.건.))+
#     geom_col() +
#     coord_flip() 


# 
install.packages("lubridate")

library(lubridate)
library(reshape2)

cold1$날짜 <- ymd(cold1$날짜)

month()

cold_sample <- cbind(cold1, month = month(cold1$날짜))
month <- as.character(cold_sample$month)

cold1 <- cbind(cold1, month, stringsAsFactors = FALSE)

str(cold1)

cold_t$month <- cold_t
# 피벗 테이블 만들기
# 
# cold_month <- dcast(y, month ~ . , value.var = "발생건수.건.", mean)

bin <- cold1 %>% 
    select(month, 발생건수.건.) %>% 
    group_by(month) %>% 
    summarise(발생건수 = mean(발생건수.건.))

ggplot(data = bin, aes(x = month, y = 발생건수)) + 
    geom_col() +
    scale_x_discrete(limits = c(1:12))

           