
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

# convert columns data type(여러 컬럼의 데이터 타입을 변환)
# package 'hablar'
install.packages('hablar')
library(hablar)

mydata <- mydata %>% convert(int(V1,V2,V6,V9,V10,V14,V20:V56))

# 데이터 확인하기(dplyr)
library(dplyr)
dplyr::glimpse(mydata)
# Error: 'vec_dim' is not an exported object from 'namespace:vctrs'
# 'pillar' packages를 업데이트해 줘야 함

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
naniar::vis_miss(mydata)

# 결측값 시각화(package 'VIM')
install.packages('VIM')
library(VIM)

VIM::aggr(mydata)

# 연차보고서에 있는 콘텐츠명만 추출
# 특정 변수(컬럼)의 결측값을 제거하고 나머지 컬럼 가져오기 등
mydata_subset <- mydata[complete.cases(mydata$V8), ] # yearbook

# save file
write.csv(mydata, file = "mydata.csv", row.names = FALSE)
write.csv(mydata_subset, file = "mydata_subset.csv", row.names = FALSE)

