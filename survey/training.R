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
table(mydata$V28)
plot(table(mydata$V28))

mydata %>% ggplot(aes(x=V28)) + geom_histogram()
mydata %>% ggplot(aes(x=V28)) + geom_freqpoly()

# 산점도 행렬
pairs(mydata %>% dplyr::select(V27, V28) %>% sample_n(1000))

# correlation coefficient
cor(mydata$V27, mydata$V28)
with(mydata, cor(V27, V28))

# linear regression
ggplot(mydata, aes(V27, V28)) + geom_jitter() + geom_smooth(method="lm")

# lm
ComRatelm <- lm(V28 ~ V27, data=mydata)
summary(ComRatelm)
# V28=0.007+0.867*V27

# 전체(2018, 2019), 실시인원, 수료인원, 재직여부, 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V17)) + 
  geom_point() +
  facet_wrap(~V6)

# 전체(2018, 2019), 실시인원, 수료인원, 훈련유형, 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V18)) + 
  geom_point() +
  facet_wrap(~V6)

# 전체(2018, 2019), 실시인원, 수료인원, 훈련방법, 대분류별
mydata %>% 
  ggplot(aes(V27, V28, colour=V19)) + 
  geom_point() +
  facet_wrap(~V6)
