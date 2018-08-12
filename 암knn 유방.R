#knn 유방암 진
setwd("C:/Users/HS/Documents/GitHub/Wisconsin Breast Cancer Diagnostic")

wbcd <- read.csv("wbcd.csv")
summary(wbcd)
dim(wbcd)
names(wbcd)

wbcd<- wbcd[-1]  #id 제외

table(wbcd$diagnosis)  #양성 357개, 음성 212개

wbcd$diagnosis <- factor(wbcd$diagnosis, levels=c('B','M'),
                         labels=c('Benign','Malignant'))
#분류기 목표 특징이 팩터로 코딩되어야 하기 때문

round(prop.table(table(wbcd$diagnosis))*100, 1)


#데이터변환
summary(wbcd[, c(2,5,6)])
#범위 차이가 너무 많이 난다
#표준 범위로 재조정
normalize <- function(x){
  return ((x-min(x)) / (max(x)- min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n)
dim(wbcd_n)



#데이터준비
#train, test dataset
smp_size <- floor(0.75 * nrow(wbcd_n))

## set the seed to make your partition reproducible
set.seed(123)
idx <- sample(seq_len(nrow(wbcd_n)), size = smp_size)

train <- wbcd_n[idx, ]
test <- wbcd_n[-idx, ]

train_label <- wbcd[idx, 1]
test_label<- wbcd[-idx, 1]



#데이터로 모델 훈련
install.packages("class")
library(class)

test_pred <- knn(train= train, test=test, 
                      cl= train_label, k=21)
#클래스에 대한 예측값


#모델 성능 평가
install.packages("gmodels")
library(gmodels)
CrossTable(x= test_label, y=test_pred, prop.chisq = FALSE)


#모델 성능 개선
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

train<- wbcd_z[idx,-31]
test<- wbcd_z[-idx,-31]
train_label<- wbcd[idx,1]
test_label<- wbcd[-idx,1]
test_pred <- knn(train= train, test= test, cl= train_label, k=21)
CrossTable(x=test_label, y=test_pred, prop.chisq = FALSE)

