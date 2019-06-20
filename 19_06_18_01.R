library(rJava)      # 자바 지원
library(xlsx)       # 엑셀 파일 읽고 쓰고 저장하기
library(readxl)     # 엑셀 파일 불러오기
library(ggplot2)    # 시각화
library(dplyr)      # 전처리

# 데이터 과학 프로세스
# 1. 문제 정의(problem definition): 현실의 구체적인 문제를 명확하게 표현. 통계적, 수리적 언어로 번역하는 작업
# 2. 데이터 정의(data definition): 변수(variable), 지표(metric) 등을 정의
# 3. 실험 계획(design of experiment): 데이터 수집하는 2가지 목적은 어떤 처리 효과를 알아내기 위한 통제실험, 
#    표본화(sampling): 모집단을 대표하는 표본을 얻기 위한 표본화
# 4. 데이터 취득(data acquisition): 원데이터를 분석 시스템으로 가져오는 활동
# 5. 데이터 가공(data processing, data wrangling): 분석하기 적당한 표 형태로 가공하는 작업, 데이터 변환
# 6. 탐색적 분석과 데이터 시각화(exploratory data analysis, data visualization): 시각화와 간단한 통계량 -> 데이터 패턴 발견, 이상치 점검
# 7. 모형화(modeling): 모수 추정, 가설검정 등의 활동과 모형분석, 예측분석 등을 포괄
# 8. 분석결과 정리(reporting): 분석 결과를 현실적인 언어로 이해하기 쉽도록 번역해내는 작업


# 1. 한국 복지패널 데이터 분석

install.packages("foreign")
library(foreign)    # SPSS 파일 불러오기

# 데이터 불러오기
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)   
# to.data.frame = T : SPSS파일을 데이터프레임 형태로 변환, 이 옵션을 하지 않으면 데이터를 리스트 형태로 불러옴.

# 복사본 만들기
welfare <- raw_welfare

head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 변수명 바꾸기

welfare <- rename(welfare,
                  sex = h10_g3,               # 성별
                  birth = h10_g4,             # 태어난 연도
                  marriage = h10_g10,         # 혼인 상태
                  religion = h10_g11,         # 종교
                  income = p1002_8aq1,        # 월급
                  coode_job = h10_eco9,       # 직업 코드
                  code_region = h10_reg7)     # 지역 코드

# 1-2 성별에 따른 월급 차이 분석

# 성별 변수 검토 및 전처리
class(welfare$sex)      # class는 변수 타입을 확인 하는 함수.
table(welfare$sex)

# 이상치 확인
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)    # 엑셀에 전제조건에 대한 값을 보면 모름/무응답인 경우 9를 할당함.
table(is.na(welfare$sex))       # 이상치 없음

# 성별에 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)

qplot(welfare$sex)

# 월급 변수 검토 및 전처리
class(welfare$income)
summary(welfare$income)
# 최소값: 0 / 1사분위: 122 / 중앙값: 192.5 / 평균: 241.6 / 3사분위: 316.6 / 최대값: 2400 / NA: 12030
qplot(welfare$income)

qplot(welfare$income) + xlim(0,1000)    # 대다수를 차지하는 0~1000 사이를 자세히 보기 위해 설정

# 전처리
# 직업이 없어 수입이 없는 응답자가 있기 때문에 결측치 존재, 그런데 0원이 존재하면 안됨(백수 = 결측치인데 0이 포함되면 이상치)
# 값의 범위가 1 ~ 9998까지이니 0과 9999를 가진 사람은 결측처리를 해야한다.

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)  # 0, 9999값을 지닌 이상치를 NA하고 그렇지 않으면 그대로 값을 지님.
table(is.na(welfare$income))    # 12030 -> 12044로 증가함

# 성별에 따른 월급 차이 분석하기(결측치는 제외한다.)
sex_income <- welfare %>% 
    filter(!is.na(income)) %>%  # 결측치가 아닌 값만 추출(결측치 제외)
    group_by(sex) %>% 
    summarise(mean_income = mean(income))
sex_income    

ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# 1-2 나이에 따른 월급 차이 분석

# 변수 검토
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

# 이상치 확인
summary(welfare$birth)
table(is.na(welfare$birth)) # 이상치와 결측치는 존재하지 않음

# 이상치 결측 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# 태어난 년도를 나이로 변환하기 (2015년 기준 2015 - 태어난 년도 + 1)
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)
# 최소값: 2 / 1사분위: 28 / 중앙값: 50 / 평균: 48.43 / 3사분위: 70 / 최대값: 109

# 나이에 따른 월급 평균표
age_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(age) %>% 
    summarise(mean_income = mean(income))
age_income

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()
summary(age_income)
