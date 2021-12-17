# 1. 
install.packages("scatterplot3d")
library(scatterplot3d)

x = c(3.0, 6.0, 3.0, 6.0, 7.5, 7.5, 15.0)       
u = c(10.0, 10.0, 20.0, 20.0, 5.0, 10.0, 12.0)   
y = c(4.65, 5.9, 6.7, 8.02, 7.7, 8.1, 6.1)     

m = lm(y ~ x+u)

nx = c(7.5, 5.0)
nu = c(15.0, 12.0)
new_data = data.frame(x = nx, u = nu)
ny = predict(m, new_data)
ny

s = scatterplot3d(nx, nu, ny, 
                  xlim = 2:7, ylim = 7:23, zlim = 0:10,
                  pch = 20, type = 'h', color = 'red')
s$plane3d(m)

# 5.
library(rpart)
library(randomForest)
library(class)
library(e1071)
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit)

# 학습데이터, 테스트데이터 분리
n_ucla = nrow(ucla)

i = 1:n_ucla
train_list = sample(i, n_ucla*0.6)
test_list = setdiff(i, train_list)

ucla_train = ucla[train_list, ]
ucla_test = ucla[test_list, ]

ucla_train

# 결정트리 모델
r = rpart(admit~., data = ucla_train)
par(mfcol = c(1, 1), xpd = NA)

# 결정트리 모델 결과
p = predict(r, newdata = ucla_test, type = 'class')
table(p, ucla_test$admit)

# RandomForest 트리개수 50개
forest50 = randomForest(admit~., data = ucla_train, ntree = 50)

# RandomForest ntree 50개 결과
p = predict(forest50, newdata = ucla_test)
table(p, ucla_test$admit)

# RandomForest 트리개수 1,000개
forest1000 = randomForest(admit~., data = ucla_train, ntree = 1000)

# RandomForest ntree 1,000개 결과
p = predict(forest1000, newdata = ucla_test)
table(p, ucla_test$admit)

# knn
k = knn(ucla_train, ucla_test, ucla_train$admit, k=15)

# knn 결과
table(k, ucla_test$admit)

# svm
s_radial = svm(admit~., data = ucla_train, kernel = 'radial')
s_poly = svm(admit~., data = ucla_train, kernel = 'polynomial')

# svm radial 결과
p = predict(s_radial, newdata = ucla_test)
table(p, ucla_test$admit)

# svm poly 결과
p = predict(s_poly, newdata = ucla_test)
table(p, ucla_test$admit)
