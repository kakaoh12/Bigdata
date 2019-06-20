library(rJava)      # 자바 지원
library(xlsx)       # 엑셀 파일 읽고 쓰고 저장하기
library(readxl)     # 엑셀 파일 불러오기
library(ggplot2)    # 시각화
library(dplyr)      # 전처리
library(foreign)    # SPSS 파일 불러오기
library(lubridate)  # 시간 date 사용.


# 2014년 ~ 2018년 월별 감기  평균 빈도수 보기

# 데이터 불러오기
exam1 <- read.csv("sample/시군구 지역코드.csv",  stringsAsFactors = FALSE)
exam3 <- read.csv("sample/실제진료정보_감기_시군구.csv")

# 새로운 변수명  
감기1 <- exam3


# 지역코드와 진료정보 합치기(감기)
cold1 <- left_join(감기1, exam1, by = "시군구지역코드")


# 진료정보에 있는 날짜 값은 date 값으로 변경하기
cold1$날짜 <- ymd(cold1$날짜)

# 월만 추출하기

cold_sample <- cbind(cold1, month = month(cold1$날짜))      # 날짜에서 월을 추출하여 sample에 저장
month <- as.character(cold_sample$month)                    # 추출된 정보가 date 포맷이기 때문에 charatcter로 변경하기

cold1 <- cbind(cold1, month, stringsAsFactors = FALSE)      # 따로 만들어진 변수 month를 원본 cold1에 결합하기.

str(cold1)      # 처음에 date format인 month가 chr로 변경된 것을 확인/                                              


# 월별 감기의 평균 빈도수 확인하기

bin <- cold1 %>% 
    select(month, 발생건수.건.) %>% 
    group_by(month) %>% 
    summarise(발생건수 = mean(발생건수.건.))

# chr로 되어 있어서 숫자가 크기별로 정렬이 아닌 1, 10, 11, 12, 2, 3, ... 이런식으로 그래프가 만들어 진다.
ggplot(data = bin, aes(x = month, y = 발생건수)) + 
    geom_col() +
    scale_x_discrete(limits = c(1:12))      # scale_x_discrete를 통해 문자열을 1:12까지 강제로 정렬시키면 원하는 월별 그래프가 만들어 진다.

