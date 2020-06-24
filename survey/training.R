### training analysis ###
# 2018, 2019

getwd()
setwd("D://02.온평원/08. 선행연구/03. 수요조사/2020년/06. 데이터요청(심평원)")

# load data
# null, " "(공백)을 NA로 입력, na.string=c("", NA)
mydata <- read.csv("training.csv", header=FALSE,
                   stringsAsFactors=TRUE,
                   na.strings=c("", NA))

# load packages
library(dplyr)
library(ggplot2)

# summary()
names(mydata)
dim(mydata)
class(mydata)
str(mydata)
summary(mydata)

# 수료인원 histogram()
barplot(mydata[["V28"]]) #Rplot01
plot(table(mydata$V28))
hist(mydata[["V28"]], breaks=100)

mydata %>% ggplot(aes(x=V28)) + geom_histogram()  #Rplot02
mydata %>% ggplot(aes(x=V28)) + geom_freqpoly()

# 상관관계행렬 그래프
#install.packages("corrplot")
library(corrplot)
pcor <- round(cor(mydata[,c(3, 22, 27:28)]), 2)
corrplot(pcor,
         method="shade",
         addshade="all",
         tl.srt=30,
         diag=FALSE,
         addCoef.col="black") #order="FPC" #Rplot03

# 상관행렬
pairs(mydata %>% dplyr::select(V27, V28) %>% sample_n(1000))

# 수강인원, 수료인원 correlation coefficient
cor(mydata$V27, mydata$V28)
with(mydata, cor(V27, V28))

# 수강인원, 수료인원 linear regression
ggplot(mydata, aes(V27, V28)) + 
  geom_jitter(aes(alpha=0.5)) + 
  geom_smooth(method="lm") #Rplot04

# hexbin graph
install.packages("hexbin")
library(hexbin)

ggplot(mydata, aes(V27, V28)) +
  stat_binhex(colour="grey") +
  theme_bw()

# 투명도
ggplot(mydata, aes(V27, V28)) +
  theme_bw() +
  geom_point(alpha=0.1)

#geom_density2d(colour="white")
#scale_fill_gradient(low="maroon", high="blue")
#stat_binhex(colour="grey") + # border colour

# lm
ComRatelm <- lm(V28 ~ V27, data=mydata)
summary(ComRatelm)
# V28=0.007+0.867*V27

#훈련비(총지원금), 수료인원
CMcor <- lm(V28 ~ V25, data=mydata)
summary(CMcor)
# V28=9.840+1.285*V25

### 훈련과정명, 훈련과정명, 재직여부, 훈련유형, 훈련방법, 실시인원, 수료인원 ###
aggregate(cbind(V27, V28) ~ V4 + V17, mydata, mean)
#aggregate로는 피벗이 어려움

#dplyr, group_by, summarise를 활용
library(tidyr)
library(dplyr)

### 2019년 ###
# 2019년, 재직여부 기준, 실시인원, 수료인원, 수료율 
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  mutate(per=paste0(round(V28/sum(V28)*100, 2))) %>%
  data.frame() %>%
  select(V17, V28) %>%
  ggplot(aes(x=factor(reorder(V17, -V28)), y=V28)) +
  geom_col(fill="steelblue", color="grey50") #Rplot05

# 2019년, 재직여부, 훈련유형, 실시인원, 수료인원, 수료율 
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V18) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  mutate(per=paste0(round(V28/sum(V28)*100, 2))) %>%
  data.frame() %>%
  select(V17, V18, V28) %>%
  ggplot(aes(x=factor(reorder(V17, -V28)), y=V28, fill=V18)) +
  geom_col(position="dodge2", color="grey50") #Rplot06

# 2019년, 재직여부 기준, 운영과정 수
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17) %>%
  summarise(n=n()) %>%
  data.frame() %>%
  select(V17, n) %>%
  ggplot(aes(x=factor(reorder(V17, -n)), y=n)) +
  geom_col(fill="steelblue", color="grey50") #Rplot07

# 지수표시 X
options(scipen=999)

# 2019년, 재직여부, 훈련유형, 운영과정 수
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V18) %>%
  summarise(n=n()) %>%
  data.frame() %>%
  select(V17, V18, n) %>%
  ggplot(aes(x=factor(reorder(V17, -n)), y=n, fill=V18)) +
  geom_col(position="dodge2", color="grey50") #Rplot08

# 2019년, 재직여부, 훈련방법, 운영과정 수
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V19) %>%
  summarise(n=n()) %>%
  data.frame() %>%
  select(V17, V19, n) %>%
  ggplot(aes(x=factor(reorder(V17, -n)), y=n, fill=V19)) +
  geom_col(position="dodge2", color="grey50") #Rplot09

# 2019년, 재직여부, 훈련방법, 수료인원
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V19) %>%
  summarise(n=n(),
            V28=sum(V28)) %>%
  data.frame() %>%
  select(V17, V19, V28) %>%
  ggplot(aes(x=factor(reorder(V17, -V28)), y=V28, fill=V19)) +
  geom_col(position="dodge2", color="grey50") #Rplot10

# 2019년, 재직여부, 훈련유형, 훈련방법, 실시인원, 수료인원, 수료율 
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V18, V19) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2))

# 2019년, 재직여부, 훈련유형, 훈련방법, 운영과정 수
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V18, V19) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=factor(reorder(V17, -n)), y=n, fill=V18)) +
  geom_col(position="dodge2", color="grey50") +
  facet_wrap(~V19) #Rplot11

# 2019년, 재직여부, 훈련유형, 훈련방법, 수료인원
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V18, V19) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  ggplot(aes(x=factor(reorder(V17, -V28)), y=V28, fill=V18)) +
  geom_col(position="dodge2", color="grey50") +
  facet_wrap(~V19) #Rplot12

### 훈련과정명 기준 ###
# 2019년, 훈련과정명 기준, 실시인원, 수료인원, 수료율
# 훈련과정별, 수료인원 순, 재직여부
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V4, V5, V17, V18, V19) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  arrange(-V28) %>%
  head(50) %>%
  ggplot(aes(x=V4, y=V28, fill=V17)) +
  geom_bar(stat="identity") #Rplot13

### 대분류기준 ###
# 2019년, 대분류코드, 훈련과정 수
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V5) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  mutate(per=paste0(round(V28/sum(V28)*100, 2))) %>%
  data.frame() %>%
  ggplot(aes(x=factor(V5), y=n)) +
  geom_bar(stat="identity") #Rplot14

# 2019년, 대분류코드, 수료인원
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V5) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  mutate(per=paste0(round(V28/sum(V28)*100, 2))) %>%
  data.frame() %>%
  ggplot(aes(x=factor(V5), y=V28)) +
  geom_bar(stat="identity") #Rplot15

# 2019년, 재직여부, 대분류코드, 운영과정 수
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V5) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  ggplot(aes(x=factor(V5), y=n)) +
  geom_bar(stat="identity") +
  facet_wrap(~V17) #Rplot16

# 2019년, 재직여부, 대분류코드, 수료인원
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V17, V5) %>%
  summarise(n=n(),
            V27=sum(V27),
            V28=sum(V28),
            V28Rate=round(sum(V28)/sum(V27), 2)) %>%
  ggplot(aes(x=factor(V5), y=V28)) +
  geom_bar(stat="identity") +
  facet_wrap(~V17) #Rplot17

## 참고사항 ##
# 특정 칼럼 데이터를 wide로 정렬
pivot_V19_wide <- mydata %>%
  filter(V5 %in% c(2)) %>%
  group_by(V4, V17, V18, V19) %>%
  summarise(CmpSum=sum(V28)) %>%
  ungroup() %>%
  spread(V19, CmpSum)

### 2019년, 재직자 ###
# 2019년, 재직자, 대분류, 훈련유형별
mydata %>%
  filter(V1 == 2019) %>%
  filter(V17 == "재직자") %>%
  group_by(V4, V5, V18) %>%
  summarise(V28=sum(V28)) %>%
  ungroup() %>%
  spread(V18, V28)

# 2019년, 재직자, 대분류, 훈련방법별
mydata %>%
  filter(V1 == 2019) %>%
  filter(V17 == "재직자") %>%
  group_by(V4, V5, V19) %>%
  summarise(V28=sum(V28)) %>%
  ungroup() %>%
  spread(V19, V28)

# 2019년, 실업자, 대분류, 훈련유형별
mydata %>%
  filter(V1 == 2019) %>%
  filter(V17 == "실업자") %>%
  group_by(V4, V5, V18) %>%
  summarise(V28=sum(V28)) %>%
  ungroup() %>%
  spread(V18, V28)

# 2019년, 실업자, 대분류, 훈련방법별
mydata %>%
  filter(V1 == 2019) %>%
  filter(V17 == "실업자") %>%
  group_by(V4, V5, V19) %>%
  summarise(V28=sum(V28)) %>%
  ungroup() %>%
  spread(V19, V28)


# 전체(2018, 2019), 실시인원, 수료인원, 재직여부(color), 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V17)) + 
  geom_point(size=1, alpha=0.1) +
  facet_wrap(~V5) #Rplot18

# 전체(2018, 2019), 실시인원, 수료인원, 훈련유형(color), 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V18)) + 
  geom_point(size=1, alpha=0.5) +
  facet_wrap(~V5) #Rplot19

# 전체(2018, 2019), 실시인원, 수료인원, 훈련방법(color), 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V19)) + 
  geom_point(size=1, alpha=0.5) +
  facet_wrap(~V5) #Rplot20

### 2019년, 훈련과정 ###
# 2019년, 대분류수
mydata %>%
  filter(V1 == 2019) %>%
  ggplot(aes(factor(V5))) +
  geom_bar() #Rplot

# 2019년, 훈련과정(중복제거), 대분류수
mydata %>%
  filter(V1 == 2019) %>%
  distinct(V4, .keep_all=TRUE) %>%
  ggplot(aes(factor(V5))) +
  geom_bar() #Rplot

# 2019년, 중분류수
mydata %>%
  filter(V1 == 2019) %>%
  ggplot(aes(factor(V7))) +
  geom_bar()

# 2019년, 훈련과정(중복제거), 중분류수
mydata %>%
  filter(V1 == 2019) %>%
  distinct(V4, .keep_all=TRUE) %>%
  ggplot(aes(factor(V7))) +
  geom_bar()

# 2019년, 소분류수
mydata %>%
  filter(V1 == 2019) %>%
  ggplot(aes(factor(V9))) +
  geom_bar()

# 2019년, 훈련과정(중복제거), 소분류수
mydata %>%
  filter(V1 == 2019) %>%
  distinct(V4, .keep_all=TRUE) %>%
  ggplot(aes(factor(V7))) +
  geom_bar()

# 2019년, 세분류수
mydata %>%
  filter(V1 == 2019) %>%
  ggplot(aes(factor(V11))) +
  geom_bar()

# 2019년, 훈련과정(중복제거), 세분류수
mydata %>%
  filter(V1 == 2019) %>%
  distinct(V4, .keep_all=TRUE) %>%
  ggplot(aes(factor(V11))) +
  geom_bar()

# 2019년, 대분류별 그룹, 대분류그룹수(훈련과정수) > 2000
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V5) %>%
  tally() %>%
  filter(n >= 2000) %>%
  ggplot(aes(factor(V5), n)) +
  geom_col()

# 2019년, 중분류별 그룹, 중분류그룹수(훈련과정수) > 2000
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V7) %>%
  tally() %>%
  filter(n >= 2000) %>%
  ggplot(aes(factor(V7), n)) +
  geom_col()

# 2019년, 소분류별 그룹, 소분류그룹수(훈련과정수) > 2000
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V9) %>%
  tally() %>%
  filter(n >= 2000) %>%
  ggplot(aes(factor(V9), n)) +
  geom_col()

# 2019년, 세분류별 그룹, 세분류그룹수(훈련과정수) > 2000
mydata %>%
  filter(V1 == 2019) %>%
  group_by(V11) %>%
  tally() %>%
  filter(n >= 2000) %>%
  ggplot(aes(factor(V11), n)) +
  geom_col()

### 2019년, 수료인원 ###
# 2019년, 재직자, 실시인원, 수료인원, 훈련방법
mydata %>%
  filter(V1 == 2019) %>%
  filter(V17 == "재직자") %>%
  ggplot(aes(V27, V28, colour=V19)) +
  geom_point(size=1, alpha=0.8)

# 2019년, 실업자, 실시인원, 수료인원, 훈련방법
mydata %>%
  filter(V1 == 2019) %>%
  filter(V17 == "실업자") %>%
  ggplot(aes(V27, V28, colour=V19)) +
  geom_point(size=1, alpha=0.8)


