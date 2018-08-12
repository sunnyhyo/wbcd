#tree
setwd("C:/Users/HS/Documents/GitHub/Wisconsin Breast Cancer Diagnostic")

wbcd <- read.csv("wbcd.csv")
summary(wbcd)
dim(wbcd)
names(wbcd)
prop.table(table(wbcd$diagnosis))

#데이터 준비
set.seed(123)
tr_idx <- sample(nrow(wbcd) , nrow(wbcd)*0.9)
str(tr_idx)

train <- wbcd[tr_idx,-c(1,2,33)]
test <- wbcd[-tr_idx,-c(1,2,33)]
train_label<- wbcd[tr_idx, 1]
test_label<- wbcd[-tr_idx, 1]


#균등하게 분할됐음


#모델 훈련
install.packages("C50")
library(C50)

diag_model <- c5.0(train, train_label)
diag_pred <- predict(diag_model, test)
