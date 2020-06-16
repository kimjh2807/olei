# 2020 수요조사
# 온평원 2014-2019 운영실적 분석
# 만족도가 높은 콘텐츠

# set working directory
getwd()
setwd("D:/02.온평원/03. 이러닝운영/03. 운영실적/운영실적/수요조사/2020년")

# all packages installed(설치된 패키지 확인)
library()

# packages currently loaded(현재 로드된 패키지 확인)
search()

### load refinded data ###
mydata_sat <- read.csv("mydata_sat.csv", header = FALSE,
                   stringsAsFactors = TRUE,
                   na.strings = c("", " ", NA))
str(mydata_sat)

# change data type
# package(dplyr)
# package(hablar)
library(dplyr)
library(hablar)
mydata_sat <- mydata_sat %>% convert(int(V1,V5,V8,V9,V13,V19:V48, V53, V54))
mydata_sat <- mydata_sat %>% convert(num(V49:V52, V55))
str(mydata_sat)
#write.csv(mydata_sat, file="mydata_sat.csv", row.names = FALSE)

# load library
satis <- mydata_sat %>% select(V6, V49:V55)
str(satis)

# 결측값이 들어 있는 행 전체를 데이터 셋에서 제거
# na.omit()
satis <- na.omit(satis)
str(satis)

# 소수점 자리 정리
satis$V55 <- round(satis$V55, 2)
str(satis)

# 콘텐츠명 중복 확인
table(satis$V6)
satis %>% distinct(V6)
#-> 중복 없음

# 만족도 평균 추가
satis <- satis %>% mutate(V56=(V49+V50+V51+V52)/4)
satis$V56 <- round(satis$V56, 2)
summary(satis)

# 만족도 평균 분포도
plot(satis$V56)
hist(satis$V56)
hist(log10(satis$V56))
boxplot(satis$V56)
qqline(satis$V56)
barplot(table(satis$V56))

### 상관관계(correlation) ###
# 만족도평균 - 교육인원
cor(satis$V56, satis$V53)

# 만족도평균 - 수료인원
cor(satis$V56, satis$V54)

# 만족도평균 - 수료율
cor(satis$V56, satis$V55)

# 산점도 행렬
pairs(satis)

# 만족도 구간나누기 (0~4.0, 4.1, 4.2, 4.3, 4.4, 4.5~5.0)
satis <- transform(satis, V57=ifelse(V56 < 4.0, 4.0,
                              ifelse(V56 >= 4.0 & V56 < 4.1, 4.1,
                              ifelse(V56 >= 4.1 & V56 < 4.2, 4.2,
                              ifelse(V56 >= 4.2 & V56 < 4.3, 4.3,
                              ifelse(V56 >= 4.3 & V56 < 4.4, 4.4,
                              ifelse(V56 >= 4.4 & V56 < 4.5, 4.5, 5.0)))))))

satis$V57
plot(satis$V57)

# 만족도 구간별로 수료율과의 상관관계
cor(satis$V55, satis$V57)

# 만족도와 수료율과의 선형 회귀분석
CSlm <- lm(V56 ~ V55, data=satis)
summary(CSlm)
#V56(만족도)=4.172+0.194*V55(수료율)

# 만족도 구간별, 수료율과의 선형 회귀분석
CSClm <- lm(V56 ~ V57, data=satis)
summary(CSClm)
#V56(만족도)=0.912+0.773*V55(만족도구간별)

# 만족도 구간별, 수료율 박스상자 그래프(geom_boxplot())
library(ggplot2)
satis %>% ggplot(aes(factor(V57), V55)) + geom_boxplot()
satis %>% ggplot(aes(factor(V57), V55)) + geom_point()
satis %>% ggplot(aes(factor(V57), V55)) + geom_jitter()

# 만족도 구간별, 교육인원 박스상자 그래프(geom_boxplot())
library(ggplot2)
satis %>% ggplot(aes(factor(V57), V53)) + geom_boxplot()
satis %>% ggplot(aes(factor(V57), V54)) + geom_boxplot()

# 만족도와 수료율이 높은 콘텐츠는?
# 수료율 > 0.43 (수료율 평균) & 만족도 >= 4.25
satis %>% filter(V55 >= 0.43 & V56 >= 4.25) %>% select(c(V6, V53, V54))

# 수료율 > 0.43 (수료율 평균) & 만족도 >= 4.5
satis %>% filter(V55 >= 0.43 & V56 >= 4.5) %>% select(c(V6, V53, V54))

# 수료율 > 0.43 (수료율 평균) & 만족도 >= 4.5
satis %>% filter(V56 >= 4.25 & V56 <= 4.35) %>% select(c(V6, V53, V54))

# aggregate
round(aggregate(V53~V57, satis, mean), 1)

### wordcloud ###
ContentName <- satis %>% filter(V56 >= 4.25 & V56 <= 4.35) %>% select(V6)

# txt 파일로 저장
write.csv(ContentName, file="ContentName.txt", row.names="FALSE")

# 텍스트 마이닝
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")

# load packages
library(KoNLP)
library(dplyr)

# useNIADic()
useNIADic()
# 'C:/Users/user/AppData/Local/Temp/RtmpiOFTyv/remotes133ec7609268e/NIADic/inst/doc'

# load text
txt <- readLines("ContentName.txt")
head(txt)
txt

# 특수문자 제거하기
install.packages("stringr")
library(stringr)

# str_replace_all(대상, 변경할 패턴, 변경될 패턴) 
# sub(), gsub()과 같은 기능
# regex [:punct:] ~!@#$%^&*(){}_+:"<>?,./;'[]-=
txt <- str_replace_all(txt, "!", "")
txt <- str_replace_all(txt, ",", "")
txt <- str_replace_all(txt, "/", "")
txt <- str_replace_all(txt, ":", "")

# 확인
txt

# 파일에서 명사 추출
nouns <- extractNoun(txt)
nouns
# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))

# 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringAsFactors = FALSE)

# 변수명 수정
#library(dplyr)
#df_word <- rename(df_word, word=Var1, freq=Freq)

# data type change
df_word$Var1 <- as.character(df_word$Var1)
str(df_word)

# 두 글자 이상 단어 추출
df_word <- filter(df_word, nchar(Var1) >= 2)

top20 <- df_word %>% arrange(desc(Freq)) %>% head(20)
top20

### 워드 클라우드 만들기 ###
# install package (wordcloud)
install.packages("wordcloud")

# load packages
library(wordcloud)
library(RColorBrewer)

# Dark2 색상 목록에서 8개 색상 추출
pal <- brewer.pal(8, "Dark2")
pal2 <- brewer.pal(9, "Blues")[5:9]

wordcloud(words = df_word$Var1, #단어
          freq = df_word$Freq, #빈도
          min.freq = 2, #최소 단어 빈도
          max.words = Inf, #표현 단어 수
          random.order = FALSE, #고빈도 단어 중앙 배치 yes
          rot.per = .1, #회전 단어 비율
          scale = c(4, 0.5), #단어 크기 범위
          colors = pal) #색깔 목록

# Rplot_wc_07
