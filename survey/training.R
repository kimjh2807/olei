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

# histogram()
table(mydata$V27)
plot(table(mydata$V27))

mydata %>% ggplot(aes(x=V27)) + geom_histogram()
mydata %>% ggplot(aes(x=V27)) + geom_freqpoly()

# 산점도 행렬
pairs(mydata %>% dplyr::select(V3, V16, V22, V27, V28) %>% sample_n(1000))

# correlation coefficient
cor(mydata$V27, mydata$V28)
with(mydata, cor(V27, V28))

# linear regression
ggplot(mydata, aes(V27, V28)) + geom_jitter() + geom_smooth(method="lm")

# lm
ComRatelm <- lm(V28 ~ V27, data=mydata)
summary(ComRatelm)
# V28=0.007+0.867*V27

# 대분류별 수료인원
mydata %>% ggplot(aes(V17, V28)) + geom_boxplot()





