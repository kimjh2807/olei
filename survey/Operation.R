# 온평원 2014-2019 운영실적 분석

# set working directory
getwd()
setwd("D:/02.온평원/03. 이러닝운영/03. 운영실적/운영실적/수요조사/2020년")

# all packages installed(설치된 패키지 확인)
library()

# packages currently loaded(현재 로드된 패키지 확인)
search()

# load data
# not load column name (create new column name in order), header = FALSE
# null, " "을 NA로 입력, na.strings = c("", " ", NA)
mydata <- read.csv("Operation.csv", header = FALSE,
                   stringsAsFactors = TRUE,
                   na.strings = c("", " ", NA))

ls()
names(mydata)
dim(mydata)
class(mydata)
head(mydata, n=5)
str(mydata)
sapply(mydata, class)
summary(mydata)

# 데이터 확인하기(dplyr)
library(dplyr)
dplyr::glimpse(mydata)
# Error: 'vec_dim' is not an exported object from 'namespace:vctrs'
# 'pillar' packages를 업데이트해 줘야 함

# convert columns data type(여러 컬럼 데이터 타입을 한 번에 변환)
# package 'hablar'
install.packages('hablar')
library(hablar)

# change data type
#mydata <- mydata %>% convert(int(V1,V2,V6,V9,V10,V14,V20:V56))
#str(mydata)

# 결측치와 유일값 진단(package 'dlookr')
install.packages('dlookr')
library(dlookr)
dlookr::diagnose(mydata)
#Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
#‘latticeExtra’이라고 불리는 패키지가 없습니다

# 변수별(컬럼별) 결측값 확인 및 결측값 개수 합계
is.na(mydata)
is.na(mydata$V8)
sum(is.na(mydata$V8)) #211
colSums(is.na(mydata)) #V8: 211
table(is.na(mydata))

# 행별(row별) 결측값 확인 및 결측값 개수 합계
# NA 있으면 FALSE, NA 없을 경우 TRUE로 표시됨
complete.cases(mydata)
sum(complete.cases(mydata)) #1, 컬럼명에는 NA가 없음
sum(!complete.cases(mydata)) #1011, 모든 행에 NA가 있음

# 특정 컬럼에 대해
complete.cases(mydata$V8)
sum(complete.cases(mydata$V8)) #801
sum(!complete.cases(mydata$V8)) #211

# 컬럼의 특정 값을 NA로 처리하기
# outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
# outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
# outlier %>% filter(!is.na(sex) & !is.na(score)) %>% group_by(sex) %>% summarise(mean_score = mean(score))

# 결측값 요약(package 'naniar')
install.packages('naniar')
library(naniar)

# 결측값 요약: 행 기준
naniar::miss_case_summary(mydata)

# 결측값 요약: 컬럼 기준
naniar::miss_var_summary(mydata)

# 결측값 시각화(package 'naniar')
naniar::vis_miss(mydata) # Rplot01

# 결측값 시각화(package 'VIM')
install.packages('VIM')
library(VIM)

VIM::aggr(mydata) # Rplot02

# 연차보고서에 있는 콘텐츠명만 추출
# 특정 변수(컬럼)의 결측값을 제거하고 나머지 컬럼 가져오기 등
# yearbook criteria
mydata_sub <- mydata[complete.cases(mydata$V8), ]
VIM::aggr(mydata_subset) # Rplot03

# save as other file name
write.csv(mydata_sub, file="mydata_sub.csv", row.names = FALSE)

### load refinded data ###
mydata_sub <- read.csv("mydata_sub.csv", header = FALSE,
                   stringsAsFactors = TRUE,
                   na.strings = c("", " ", NA))
str(mydata_sub)

# change data type
# package(dplyr)
# package(hablar)
library(dplyr)
library(hablar)
mydata_sub <- mydata_sub %>% convert(int(V1,V5,V8,V9,V13,V19:V54))
str(mydata_sub)
#write.csv(mydata_sub, file="mydata_sub.csv", row.names = FALSE)

# 기술통계(descriptive statistics)
# R Friend 블로그 참조
# https://rfriend.tistory.com/119?category=605867

### mydata_sub.csv ###

# 도수분포표(frequency distribution table)
# table(), xtabs()
tab_V8 <- table(mydata_sub$V8)
xtabs(~V8, data=mydata_sub)

# 상대도수분포표(relative frequency distribution table): prop.table()
options("digits"=2) # 소수점 자리수 설정
prop.table(tab_V8)

### 막대 그래프(bar plot) ###

# 개발연도별 콘텐츠 수
ContentDevYear <- data.frame(table(mydata_sub$V1))
barplot(table(mydata_sub$V1),
        main = "개발연도별 콘텐츠 수",
        xlab = "연도",
        ylab = "개수",
        ylim = c(0, 150),
        col = "steelblue",
        border = "black") # Rplot04

# 개발구분
ContentDevType <- data.frame(table(mydata_sub$V2))
barplot(table(mydata_sub$V2),
        main = "개발구분",
        xlab = "유형",
        ylab = "개수",
        ylim = c(0, 400),
        col = "steelblue",
        border = "black") # Rplot05

# 분야
ContentField <- data.frame(table(mydata_sub$V3))
barplot(table(mydata_sub$V3),
        main = "분야",
        xlab = "분야",
        ylab = "개수",
        ylim = c(0, 300),
        col = "steelblue",
        border = "black") # Rplot06

# 운영회차
ContentSub <- data.frame(table(mydata_sub$V5))
barplot(table(mydata_sub$V5),
        main = "운영회차",
        xlab = "운영회차",
        ylab = "개수",
        ylim = c(0, 150),
        col = "steelblue",
        border = "black") # Rplot07

# 연차보고서 기준 콘텐츠명
ContentName <- data.frame(table(mydata_sub$V7))
summary(ContentName)

# 연차보고서 기준 개발년도
ContentDevYearReport <- data.frame(table(mydata_sub$V9))
barplot(table(mydata_sub$V9),
        main = "연차보고서 기준 개발연도별 콘텐츠 수",
        xlab = "연도",
        ylab = "개수",
        ylim = c(0, 250),
        col = "steelblue",
        border = "black") # Rplot08

# 연차보고서 기준 확보구분
ContentGet <- data.frame(table(mydata_sub$V10))
barplot(table(mydata_sub$V10),
        main = "연차보고서 기준 확보구분",
        xlab = "확보구분",
        ylab = "개수",
        ylim = c(0, 600),
        col = "steelblue",
        border = "black") # Rplot09

# 연차보고서 기준 사업구분
ContentDevObj <- data.frame(table(mydata_sub$V11))
barplot(table(mydata_sub$V11),
        main = "연차보고서 기준 사업구분",
        xlab = "사업구분",
        ylab = "개수",
        ylim = c(0, 600),
        col = "steelblue",
        border = "black") # Rplot10

# 연차보고서 기준 NCS 대분류
ContentNCSHigh <- data.frame(table(mydata_sub$V12))
barplot(table(mydata_sub$V12),
        main = "연차보고서 기준 NCS 대분류",
        xlab = "NCS 대분류",
        ylab = "개수",
        ylim = c(0, 300),
        col = "steelblue",
        border = "black") # Rplot11

# 연차보고서 기준 콘텐츠 회차
ContentSubYB <- data.frame(table(mydata_sub$V13))
barplot(table(mydata_sub$V13),
        main = "연차보고서 기준 콘텐츠 회차",
        xlab = "콘텐츠 회차",
        ylab = "개수",
        ylim = c(0, 160),
        col = "steelblue",
        border = "black") # Rplot12

# 연차보고서 기준 SME 소속
ContentSMEInOut <- data.frame(table(mydata_sub$V14))
barplot(table(mydata_sub$V14),
        main = "연차보고서 기준 SME 소속(내부/외부)",
        xlab = "내부/외부",
        ylab = "개수",
        ylim = c(0, 500),
        col = "steelblue",
        border = "black") # Rplot13

# 연차보고서 기준 SME 성명
ContentSMEName <- data.frame(table(mydata_sub$V15))
barplot(table(mydata_sub$V15),
        main = "연차보고서 기준 SME 성명",
        xlab = "SME 성명",
        ylab = "개수",
        ylim = c(0, 25),
        col = "steelblue",
        border = "black") # Rplot14

# 연차보고서 기준 강의유형
ContentLectureType <- data.frame(table(mydata_sub$V16))
barplot(table(mydata_sub$V16),
        main = "연차보고서 기준 강의유형",
        xlab = "강의유형",
        ylab = "개수",
        ylim = c(0, 350),
        col = "steelblue",
        border = "black") # Rplot15

# 연차보고서 기준 개발유형
ContentDevMethod <- data.frame(table(mydata_sub$V17))
barplot(table(mydata_sub$V17),
        main = "연차보고서 기준 개발유형",
        xlab = "개발유형",
        ylab = "개수",
        ylim = c(0, 600),
        col = "steelblue",
        border = "black") # Rplot16

# 연차보고서 기준 상세유형
ContentLectureDiff <- data.frame(table(mydata_sub$V18))
barplot(table(mydata_sub$V18),
        main = "연차보고서 기준 상세유형",
        xlab = "상세유형",
        ylab = "개수",
        ylim = c(0, 200),
        col = "steelblue",
        border = "black") # Rplot17

# 20200425
# https://rfriend.tistory.com/120?category=605867

# 이변량 분할표(contingency table) 
# 연차보고서 기준 개발연도별/확보구분
CTDevYearGet <- table(mydata_sub$V9, mydata_sub$V10)
CTDevYearGet

# 연차보고서 기준 개발연도별/사업구분
CTDevYearBiz <- table(mydata_sub$V9, mydata_sub$V11)
CTDevYearBiz

# 연차보고서 기준 NCS 대분류/개발연도별
CTDevYearNCSHC <- table(mydata_sub$V9, mydata_sub$V12)
CTDevYearNCSHC

# 연차보고서 기준 개발연도별/회차
CTDevYearSub <- table(mydata_sub$V9, mydata_sub$V13)
CTDevYearSub

# 연차보고서 기준 개발연도별/강의유형
CTDevYearLectureType <- table(mydata_sub$V9, mydata_sub$V16)
CTDevYearLectureType

# 연차보고서 기준 개발연도별/개발유형
CTDevYearDevType <- table(mydata_sub$V9, mydata_sub$V17)
CTDevYearDevType

# 연차보고서 기준 개발연도별/상세유형
CTDevYearLectureDiff <- table(mydata_sub$V9, mydata_sub$V18)
CTDevYearLectureDiff

#addmargins(CTDevYearType, margin=1) # row sum이 결과에 추가되어 나옴
#addmargins(CTDevYearType, margin=2) # col sum이 결과에 추가되어 나옴

# 카이제곱 검정
# package ("gmodels")
install.packages("gmodels")
library(gmodels)

CrossTable(mydata_sub$V9, mydata_sub$V16, expected=TRUE, chisq=TRUE)

# mosaic plot: vcd package, mosaic()
library(vcd)
mosaic(CTDevYearLectureType,
       gp=gpar(fill=c("red", "blue")),
       direction="v")

# 산(기)술통계량(descriptive statistics)
# https://rfriend.tistory.com/121?category=605867
# package: MASS
library(MASS)
str(mydata_sub)
hist(mydata_sub$V53, freq=FALSE, breaks=100)
mean(mydata_sub$V53, na.rm=TRUE)
table(mydata_sub$V53)

# 퍼짐정도(분산, 표준편차, 변이계수, 범위, IQR, 백분위수)
# https://rfriend.tistory.com/122?category=605867
# package: ggplot2

# 개발연도별 교육인원(수강신청인원)
DevYearEdu <- tapply(mydata_sub$V53, mydata_sub$V9, sum, na.rm=TRUE)
barplot(DevYearEdu,
        main="개발연도별 교육인원(수강신청인원)",
        xlab="연도",
        ylab="인원(명)",
        ylim=c(0, 230000),
        col="steelblue",
        border="black") # Rplot18

# 변이계수(coefficient of variation)
# 표준편차의 절대크기가 현저하게 달라서, 평균이 서로 매우 다른 두 집단 간 비교,
# 측정 단위가 다른 두 변수간 비교에는 부적합하다.
# 이럴 때 퍼짐 정도를 비교 가능하도록 표준화해준 통계량이 변이계수임
# 변이계수는 표준편차를 평균으로 나눈 다음에 100을 곱해서 계산함

# 최소값: min()
# 최대값: max()
# 범위: diff(range())
# 백분위수: quantile(x, probs=c())
# IQR: IQR()

# boxplot totalEdu by YBDevyear
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V9)) # Rplot19

# boxplot totalEdu by YBDevyear, DeyType
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V2)) # Rplot20

# boxplot totalEdu by YBDevYear, ContentGet
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V10)) # Rplot21

# boxplot totalEdu by YBDevYear, ContentBiz
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V11)) # Rplot22

# boxplot totalEdu by YBDevYear, NCSHC
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V12)) # Rplot23

# boxplot totalEdu by YBDevYear, ContentSub
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V13)) # Rplot24

# boxplot totalEdu by YBDevYear, SMEIO
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V14)) # Rplot25

# boxplot totalEdu by YBDevYear, ContentLectureType
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V16)) # Rplot26

# boxplot totalEdu by YBDevYear, ContentDevType
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V17)) # Rplot27

# boxplot totalEdu by YBDevYear, ContentSpecType
ggplot(mydata_sub, aes(V9, V53)) +
  geom_boxplot() +
  facet_wrap(vars(V18)) # Rplot28

### 연속형 변수 요약통계 한번에 보기 ###
# https://rfriend.tistory.com/124?category=605867

# base
summary(mydata_sub[c("V19", "V25", "V37", "V40", "V46", "V53")])

# package(pastecs)
install.packages("pastecs")
library(pastecs)
round(stat.desc(mydata_sub[c("V19", "V25", "V37", "V40", "V46", "V53")],
                basic = TRUE,
                desc = TRUE,
                norm = TRUE,
                p = 0.90), 1)

# basic = TRUE : 관측치 개수, null 개수, NA 개수, 최소값, 최대값, 범위, 합
# desc = TRUE : 중앙값, 평균, 분산, 표준편차, 변이계수
# norm = TRUE : 왜도, 첨도, 정규성 검정통계량, 정규성 검정 P-value
# p = 0.90 :  신뢰계수 90% (유의수준 10%) 값 => 90% 신뢰구간은 평균 ± CI.mean.0.9 값
# (위의 예 Price의 90% 신뢰구간은 19.51 ± 1.66)

# Check the scipen option
options("scipen")

# Set scipen
options(scipen=1)

# psych package: describe()
# psych package의 describe() 함수는 summary()보다는 많고 stat.desc()보다는
# 적은 기술통계량을 보여줍니다.
# describe(): 관측값 개수(n), 평균(mean), 표준편차(sd), 중앙값(median),
# 절삭평균(10% 절삭평균), 중위값절대편차(from 중위값), 최소값(min), 최대값(max)
# 범위(range), 왜도(skew), 첨도(kurtosis), 표준오차(SE, standard error)
install.packages("psych")
library(psych)

describe(mydata_sub[c("V19", "V25", "V37", "V40", "V46", "V53")],
         na.rm = TRUE, # not include missing value
         interp = TRUE, # method of median calculation
         skew = TRUE, # skewness, kurtosis
         ranges = TRUE, # range
         trim = 0.1) # trimmed mean

# 연속형 변수 그룹별(요인별) 요약통계 비교하기
# tapply(var, factor, summary)  # base
# by()                          # base
# aggregate()                   # stats
# summarBy()                    # doBy (설치 안됨)
# describe.by()                 # psych

# tapply()
# 개발연도별 교육인원
tapply(mydata_sub$V53, mydata_sub$V9, summary)
tapply(mydata_sub$V53, mydata_sub$V9, sum, na.rm=TRUE)

# 기술통계(요약)
# create function(){} for descriptive statistics(summary)
fun_summary <- function(x, ...) {
  c(na.rm=TRUE,
    n=sum(!is.na(x)), # length
    min=min(x, ...),
    median=median(x, ...),
    mean=mean(x, ...),
    max=max(x, ...),
    sum=sum(x, ...),
    var=var(x, ...),
    sd=sd(x, ...))
}

options(digits=1)
options(scipen=1)

# 개발연도별, 총 교육인원, 요약통계
# by()
by(mydata_sub[c("V53")],
   mydata_sub$V9,
   function(x) sapply(x, fun_summary, na.rm=TRUE))

# 연차보고서기준 콘텐츠명별 총 교육인원(수강신청인원)
# aggregate()
# by DevYear, ContentGet etc
aggregate(V53 ~ V7, data=mydata_sub, fun_summary)
ContentNameEdu <- format(aggregate(V53 ~ V7, data=mydata_sub, sum), scientific=FALSE)
ContentNameEdu
str(ContentNameEdu)

# chr -> int
library(dplyr)
library(hablar)
ContentNameEdu <- ContentNameEdu %>% convert(int(V53))

# order
ContentNameEdu[order(-rank(ContentNameEdu$V53)), ] #descending

# save file
write.csv(ContentNameEdu, file="ContentNameEdu.csv", row.names=FALSE)

# 총 교육인원 분포도 (plot, hist)
plot(ContentNameEdu$V53)
hist(ContentNameEdu$V53, breaks="Sturges", col="magenta")
hist(ContentNameEdu$V53, breaks="FD", col="magenta")

#콘솔에 출력되는 행의 수가 21부터 줄여서 안 보임 -> options(), max.print 
options(max.print=1000)
options(digits=2)
options(scipen=0)

# 각 변수에 따른 전체 수강신청 인원
fun_sum_mean <- function(x, ...) {
  c(sum=sum(x, ...),
    mean=mean(x, ...))
}

# 집계
aggregate(cbind(V53) ~ V9, mydata_sub, FUN=fun_sum_mean) # 연도별
aggregate(cbind(V53) ~ V10, mydata_sub, FUN=fun_sum_mean) # 개발(확보)유형별
aggregate(cbind(V53) ~ V11, mydata_sub, FUN=fun_sum_mean) # 개발유형별
aggregate(cbind(V53) ~ V12, mydata_sub, FUN=fun_sum_mean) # NCS분류별
aggregate(cbind(V53) ~ V13, mydata_sub, FUN=fun_sum_mean) # 회차별
aggregate(cbind(V53) ~ V16, mydata_sub, FUN=fun_sum_mean) # 내용유형별
aggregate(cbind(V53) ~ V17, mydata_sub, FUN=fun_sum_mean) # 강의개발유형별
aggregate(cbind(V53) ~ V18, mydata_sub, FUN=fun_sum_mean) # 강사강의유형별

# 변수 조합 집계
arrange(aggregate(cbind(V53) ~ V9 + V16, mydata_sub, FUN=fun_sum_mean), V9, V16)
arrange(aggregate(cbind(V53) ~ V9 + V18, mydata_sub, FUN=fun_sum_mean), V9, V18)
arrange(aggregate(cbind(V53) ~ V16 + V18, mydata_sub, FUN=fun_sum_mean), V16, V18)
arrange(aggregate(cbind(V53) ~ V9 + V16 + V18, mydata_sub, FUN=fun_sum_mean), V9, V16, V18)

# summaryBy()
install.packages("doBy") # 설치 안됨

# describeBy()
# 요약통계량(n, mean, sd, median, trimmed, mean, mad, min, max, range, skewness, kurtosis, se)
# 임의로 요약통계량을 지정할 수 없음
describeBy(mydata_sub[c("V53")], mydata_sub$V9, mat=TRUE)

# aggregate()
# 연차보고서 기준, 개발연도, 강의유형, 개발유형, 상세유형, 전체 교육인원(수강신청인원)
aggregate(V53 ~ V9 + V16 + V17 + V18, mydata_sub, mean)

# 연차보고서 기준, 개발연도, NCS 대분류, 전체 교육인원(수강신청인원)
aggregate(cbind(V53) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggregate(V53 ~ V9 + V12, mydata_sub, mean)
aggregate(cbind(V53, V13) ~ V9, mydata_sub, mean)
aggregate(cbind(V53, V13) ~ V9 + V18, mydata_sub, mean)

# plyr()
library(plyr)
aggregate(cbind(V53) ~ V9, mydata_sub, each(sum, mean, max)) # 합계, 평균, 최대값

# filter()
library(dplyr)
mydata_sub %>% filter(V9 == 2014, V10 == "개발") %>% select(V7, V53)

# 특정 행을 지정
mydata_sub %>% filter(V9 == 2014, V10 == "개발") %>% select(V7, V53) %>% slice(c(1:10, 100))

# 정렬
mydata_sub %>% filter(V9 == 2014, V10 == "개발") %>% select(V7, V53) %>% arrange(desc(V53))

### 함수 ###
# do()
# function() {}
# 개발년도: 입력 받음
# 개발주체: 입력 받음
# 콘텐츠명, 교육인원(수강신청인원)
# 개수는 10개로 지정

# 콘텐츠명(연차보고서) 기준, 개발연도(V7), 교육인원(수강신청인원) 합계
aggreFUN <- function(x, N) {
  x %>% aggregate(cbind(V53) ~ V7, ., each(sum)) %>% arrange(desc(V53)) %>% head(N)
}

# 개발연도, 개발구분 입력하는 함수
topN <- function(x, N) {
  x %>% filter(V9 == readline('DevYear: ')) %>% filter(V10 == readline('DevSubject: ')) %>%
    select(V7, V53) %>% arrange(desc(V53)) %>% head(N)
}

# 개발연도 입력하고, 합계 구함
mydata_sub %>% do(topN(., N=31)) %>% do(aggreFUN(., N=31)) -> Dev2014
mydata_sub %>% do(topN(., N=31)) %>% do(aggreFUN(., N=31)) -> Dev2015
mydata_sub %>% do(topN(., N=31)) %>% do(aggreFUN(., N=31)) -> Dev2016
mydata_sub %>% do(topN(., N=31)) %>% do(aggreFUN(., N=31)) -> Dev2017
mydata_sub %>% do(topN(., N=31)) %>% do(aggreFUN(., N=31)) -> Dev2018
mydata_sub %>% do(topN(., N=31)) %>% do(aggreFUN(., N=31)) -> Dev2019

# add DevYear column
Dev2014$DevYear <- 2014
Dev2015$DevYear <- 2015
Dev2016$DevYear <- 2016
Dev2017$DevYear <- 2017
Dev2018$DevYear <- 2018

# rbind
DevYearEdu <- rbind(Dev2014, Dev2015, Dev2016, Dev2017, Dev2018)

# save file
write.csv(DevYearEdu, file = "DevYearEdu.csv", row.names = FALSE)

# 콘텐츠명 기준, 연도별 교육인원, 전체 합계
# 엑셀 피벗 기능임 (pivotpal)
# attach libraries
library(tidyverse)
library(readxl)

# install.packages('here')
install.packages("here")
library(here)

# install.packages('skimr')
install.packages("skimr")
library(skimr)

# install.packages('kableExtra')
install.packages("kableExtra")
library(kableExtra)

# summary()는 수치데이터만 제공, skim()은 non-numeric 데이터에 대한 정보 제공
skimr::skim(mydata_sub)

# select(), 필요한 변수만 선택
mydata_sub_pivot <- mydata_sub %>% select(V7, V8, V19, V25, V31, V37, V40, V53)
skimr::skim(mydata_sub_pivot)
write.csv(mydata_sub_pivot, file = "mydata_sub_pivot.csv", row.names = FALSE)

# group_by %>% summarize()
mydata_sub_pivot %>% group_by(V7)

### 피벗테이블 ###
# 콘텐츠명 기준, 연도별 교육인원(수강신청인원) 합계 만들기
# 콘텐츠명 기준, 연도별 교육인원 합계
# 콘텐츠명 기준, 연도별 교육인원 합계, 피벗테이블
# melt -> aggregate
aggreEdu2014 <- aggregate(cbind(V19) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEdu2014$year <- 2014

aggreEdu2015 <- aggregate(cbind(V25) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEdu2015$year <- 2015

aggreEdu2016 <- aggregate(cbind(V31) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEdu2016$year <- 2016

aggreEdu2017 <- aggregate(cbind(V37) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEdu2017$year <- 2017

aggreEdu2018 <- aggregate(cbind(V40) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEdu2018$year <- 2018

aggreEdu2019 <- aggregate(cbind(V46) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEdu2019$year <- 2019

# renaming columns with dplyr
library(dplyr)
aggreEdu2014$V19
aggreEdu2015 <- aggreEdu2015 %>% rename(V19=V25)
aggreEdu2016 <- aggreEdu2016 %>% rename(V19=V31)
aggreEdu2017 <- aggreEdu2017 %>% rename(V19=V37)
aggreEdu2018 <- aggreEdu2018 %>% rename(V19=V40)
aggreEdu2019 <- aggreEdu2019 %>% rename(V19=V46)
aggreEdu <- rbind(aggreEdu2014, aggreEdu2015, aggreEdu2016, aggreEdu2017, aggreEdu2018, aggreEdu2019)
aggreEdu

# melt
library(MASS)
library(reshape2)
library(reshape)
print(head(aggreEdu, n=10))

aggreEdu.melt <- melt(aggreEdu, id=c("V7", "year"))
aggreEduYear <- cast(aggreEdu.melt, V7~year, sum)

# save csv file
write.csv(aggreEduYear, file = "aggreEduYear.csv", row.names = FALSE)

# 콘텐츠명 기준, 연도별 수료인원 합계 
# 콘텐츠명 기준, 연도별 수료인원 합계, 피벗테이블
# melt -> aggregate
aggreCom2014 <- aggregate(cbind(V20) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreCom2014$year <- 2014

aggreCom2015 <- aggregate(cbind(V26) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreCom2015$year <- 2015

aggreCom2016 <- aggregate(cbind(V32) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreCom2016$year <- 2016

aggreCom2017 <- aggregate(cbind(V38) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreCom2017$year <- 2017

aggreCom2018 <- aggregate(cbind(V41) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreCom2018$year <- 2018

aggreCom2019 <- aggregate(cbind(V47) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreCom2019$year <- 2019

# renaming columns with dplyr
library(dplyr)
aggreCom2014$V20
aggreCom2015 <- aggreCom2015 %>% rename(V20=V26)
aggreCom2016 <- aggreCom2016 %>% rename(V20=V32)
aggreCom2017 <- aggreCom2017 %>% rename(V20=V38)
aggreCom2018 <- aggreCom2018 %>% rename(V20=V41)
aggreCom2019 <- aggreCom2019 %>% rename(V20=V47)
aggreCom <- rbind(aggreCom2014, aggreCom2015, aggreCom2016, aggreCom2017, aggreCom2018, aggreCom2019)
aggreCom

# melt
library(MASS)
library(reshape2)
library(reshape)
print(head(aggreCom, n=10))

aggreCom.melt <- melt(aggreCom, id=c("V7", "year"))
aggreComYear <- cast(aggreCom.melt, V7~year, sum)

# save csv file
write.csv(aggreComYear, file = "aggreComYear.csv", row.names = FALSE)

# 교육인원(수강신청인원) 대비 수료인원
# 엑셀에서 두 개 파일(aggreEdu.year, aggreCom.year)을 vlookup으로 작업해서 가져오기
# 다음 이름으로 저장, aggreEduComYear.csv
#rm(list=ls(pattern="aggre"))

# 콘텐츠별 재직자, 구직자, 기타별로 교육인원(수강신청인원)
# 재직자

# 2014은 재직자, 구직자, 기타가 구분되어 있지 않음

aggreEmp2015 <- aggregate(cbind(V22) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEmp2015$year <- 2015

aggreEmp2016 <- aggregate(cbind(V28) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEmp2016$year <- 2016

aggreEmp2017 <- aggregate(cbind(V34) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEmp2017$year <- 2017

# 2018은 재직자, 구직자, 기타가 구분되어 있지 않음

aggreEmp2019 <- aggregate(cbind(V43) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEmp2019$year <- 2019

# renaming columns with dplyr
library(dplyr)
aggreEmp2015$V22
aggreEmp2016 <- aggreEmp2016 %>% rename(V22=V28)
aggreEmp2017 <- aggreEmp2017 %>% rename(V22=V34)
#aggreEmp2018 <- aggreEmp2018 %>% rename(V22=V41)
aggreEmp2019 <- aggreEmp2019 %>% rename(V22=V43)

aggreEmp <- rbind(aggreEmp2015, aggreEmp2016, aggreEmp2017, aggreEmp2019)
aggreEmp

# melt
library(MASS)
library(reshape2)
library(reshape)

aggreEmp.melt <- melt(aggreEmp, id=c("V7", "year"))
aggreEmpYear <- cast(aggreEmp.melt, V7~year, sum)

# save csv file
write.csv(aggreEmpYear, file = "aggreEmpYear.csv", row.names = FALSE)

# 구직자
# 2014은 재직자, 구직자, 기타가 구분되어 있지 않음
# 2018은 재직자, 구직자, 기타가 구분되어 있지 않음

aggreSeek2015 <- aggregate(cbind(V23) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreSeek2015$year <- 2015

aggreSeek2016 <- aggregate(cbind(V29) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreSeek2016$year <- 2016

aggreSeek2017 <- aggregate(cbind(V35) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreSeek2017$year <- 2017

aggreSeek2019 <- aggregate(cbind(V44) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreSeek2019$year <- 2019

# renaming columns with dplyr
library(dplyr)
aggreSeek2015$V23
aggreSeek2016 <- aggreSeek2016 %>% rename(V23=V29)
aggreSeek2017 <- aggreSeek2017 %>% rename(V23=V35)
aggreSeek2019 <- aggreSeek2019 %>% rename(V23=V44)

aggreSeek <- rbind(aggreSeek2015, aggreSeek2016, aggreSeek2017, aggreSeek2019)
aggreSeek

# melt
library(MASS)
library(reshape2)
library(reshape)

aggreSeek.melt <- melt(aggreSeek, id=c("V7", "year"))
aggreSeekYear <- cast(aggreSeek.melt, V7~year, sum)

# save csv file
write.csv(aggreSeekYear, file = "aggreSeekYear.csv", row.names = FALSE)

# 기타
# 2014은 재직자, 구직자, 기타가 구분되어 있지 않음
# 2018은 재직자, 구직자, 기타가 구분되어 있지 않음

aggreEtc2015 <- aggregate(cbind(V24) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEtc2015$year <- 2015

aggreEtc2016 <- aggregate(cbind(V30) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEtc2016$year <- 2016

aggreEtc2017 <- aggregate(cbind(V36) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEtc2017$year <- 2017

aggreEtc2019 <- aggregate(cbind(V45) ~ V7, mydata_sub, sum, na.rm=TRUE)
aggreEtc2019$year <- 2019

# renaming columns with dplyr
library(dplyr)
aggreEtc2015$V24
aggreEtc2016 <- aggreEtc2016 %>% rename(V24=V30)
aggreEtc2017 <- aggreEtc2017 %>% rename(V24=V36)
aggreEtc2019 <- aggreEtc2019 %>% rename(V24=V45)

aggreEtc <- rbind(aggreEtc2015, aggreEtc2016, aggreEtc2017, aggreEtc2019)
aggreEtc

# melt
library(MASS)
library(reshape2)
library(reshape)

aggreEtc.melt <- melt(aggreEtc, id=c("V7", "year"))
aggreEtcYear <- cast(aggreEtc.melt, V7~year, sum)

# save csv file
write.csv(aggreEtcYear, file = "aggreEtcYear.csv", row.names = FALSE)

################ 만족도 ####################

# 만족도
mydata_sat <- mydata[complete.cases(mydata$V8), ]
write.csv(mydata_sat, file="mydata_sat.csv", row.names = FALSE)


# load refinded data #
mydata_sat <- read.csv("mydata_sat.csv", header = FALSE,
                       stringsAsFactors = TRUE,
                       na.strings = c("", " ", NA))

mydata_sat <- mydata_sat[c("V7", "V49", "V50", "V51", "V52")]

# change data type to numeric
# package(dplyr)
# package(hablar)
library(dplyr)
library(hablar)
mydata_sat <- mydata_sat %>% convert(num(V49,V50,V51,V52))

# average (소수점 1자리)
mydata_sat$V53 <- round(rowMeans(mydata_sat[, c(2:5)], na.rm=TRUE), 1)

# 콘텐츠별로 집계
aggreSat <- aggregate(cbind(V52) ~ V7, mydata_sat, mean, na.rm=TRUE, 1)
aggreSat$V52 <- round(aggreSat$V52, 1)

# save file
write.csv(aggreSat, file="aggreSat.csv", row.names=FALSE)

# 만족도가 높은 콘텐츠
# desceding, dplyr, arrange
table(aggreSat$V52)
plot(aggreSat$V52)
hist(aggreSat$V52)
aggreSat %>% arrange(desc(V52)) %>% head(20)
aggreSat %>% filter(V52 > 4.6)


# data.frame merge
#DataMerge <- rbind(dataframe1, dataframe2)

# save file
# write.csv(mydata_01, file = "mydata.csv", row.names = FALSE)

