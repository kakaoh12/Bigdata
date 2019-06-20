# month 데이터를 chr가 아니 num으로 받은 소스코드

exam1 <- read.csv("sample/시군구 지역코드.csv",  stringsAsFactors = FALSE)
exam3 <- read.csv("sample/실제진료정보_감기_시군구.csv")
감기1 <- exam3

cold1 <- left_join(감기1, exam1, by = "시군구지역코드")

cold1$날짜 <- ymd(cold1$날짜)



cold_sample <- cbind(cold1, month = month(cold1$날짜))


bin <- cold_sample %>% 
    select(month, 발생건수.건.) %>% 
    group_by(month) %>% 
    summarise(발생건수 = mean(발생건수.건.))

ggplot(data = bin, aes(x = month, y = 발생건수)) + 
    geom_col() +
    scale_x_discrete(limits = c(1:12))
str(bin)

# month 데이터를 chr가 아니 num으로 받은 소스코드

exam7 <- read.csv("sample/실제진료정보_천식_시군구.csv")
천식1 <- exam7

asthma1 <- left_join(천식1, exam1, by = "시군구지역코드")

asthma1$날짜 <- ymd(asthma1$날짜)
asthma_sample <- cbind(asthma1, month = month(asthma1$날짜))      # 날짜에서 월을 추출하여 sample에 저장

ain <- asthma_sample %>% 
    select(month, 발생건수.건.) %>% 
    group_by(month) %>% 
    summarise(발생건수 = mean(발생건수.건.))

ggplot(data = ain, aes(x = month, y = 발생건수)) + 
    geom_col() +
    scale_x_discrete(limits = c(1:12))
str(ain)

# 감기와 천식의 빈도를 질병으로 묶어서 서로의 차이를 보는 그래프

cold <- bin
asthma <- ain

table(cold)

cold <- rename(cold, 
               cold_1 = 발생건수)

asthma <- rename(asthma,
                 asthma_1 = 발생건수)

disease <- cbind(cold, asthma$asthma_1)

disease <- rename(disease,
                  asthma_1 = "asthma$asthma_1")

summary(disease$cold_1)
str(disease)

ggplot(data = disease, aes(x=month, y=cold_1))+
    geom_line()+
    geom_line(aes(x=month,y=asthma_1 * 10),colour="red")+
    geom_point()+
    geom_point(aes(x=month,y=asthma_1 * 10),colour="red") +
    scale_x_discrete(limits = c(1:12))
