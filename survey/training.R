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
barplot(mydata[["V28"]]) #Rplot
plot(table(mydata$V28)) #Rplot
hist(mydata[["V28"]], breaks=100) #Rplot

mydata %>% ggplot(aes(x=V28)) + geom_histogram()  #Rplot
mydata %>% ggplot(aes(x=V28)) + geom_freqpoly()  #Rplot

# 상관관계행렬 그래프
library(corrplot)
pcor <- round(cor(mydata[,c(3, 22, 27:28)]), 2)
corrplot(pcor,
         method="shade",
         addshade="all",
         tl.srt=30,
         diag=FALSE,
         addCoef.col="black") #order="FPC" #Rplot

# 상관행렬
pairs(mydata %>% dplyr::select(V27, V28) %>% sample_n(1000))

# 수강인원 수료인원 correlation coefficient
cor(mydata$V27, mydata$V28)
with(mydata, cor(V27, V28))

# linear regression
ggplot(mydata, aes(V27, V28)) + geom_jitter() + geom_smooth(method="lm")

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

# 전체(2018, 2019), 실시인원, 수료인원, 재직여부(colour), 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V17)) + 
  geom_point(size=1, alpha=0.1) +
  facet_wrap(~V5) #Rplot

# 전체(2018, 2019), 실시인원, 수료인원, 훈련유형(colour), 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V18)) + 
  geom_point(size=1, alpha=0.5) +
  facet_wrap(~V5) #Rplot 

# 전체(2018, 2019), 실시인원, 수료인원, 훈련방법(colour), 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V19)) + 
  geom_point(size=1, alpha=0.5) +
  facet_wrap(~V5) #Rplot

# 전체(2018, 2019), 실시인원, 수료인원, 재직여부별
mydata %>% 
  ggplot(aes(V27, V28, colour=V19)) + 
  geom_point(size=1, alpha=0.5) +
  facet_wrap(~V17) #Rplot

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
