# Question 5

# part a
set.seed(421)

x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1 * (x1^2 - x2^2 > 0)

# part b
plot(x1[y == 0], x2[y == 0], col = "black", xlab = "x1", ylab = "x2", pch = 1)
points(x1[y == 1], x2[y == 1], col = "purple", pch = 1)

# part c
lm.fit = glm(y ~ x1 + x2, family = binomial)
summary(lm.fit)

# part d
data = data.frame(x1 = x1, x2 = x2, y = y)
lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.53, 1, 0)
data.pos = data[lm.pred == 1, ]
data.neg = data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "purple", xlab = "x1", ylab = "x2", pch = 1)
points(data.neg$x1, data.neg$x2, col = "black", pch = 1)

# part e
lm.fit = glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2) + log(x2), data = data, family = binomial)

# part f
lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.5, 1, 0)
data.pos = data[lm.pred == 1, ]
data.neg = data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "purple", xlab = "x1", ylab = "x2", pch = 1)
points(data.neg$x1, data.neg$x2, col = "black", pch = 1)

# part g
library(e1071)

svm.fit = svm(as.factor(y) ~ x1 + x2, data, kernel = "linear", cost = 0.1)
svm.pred = predict(svm.fit, data)
data.pos = data[svm.pred == 1, ]
data.neg = data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "purple", xlab = "x1", ylab = "x2", pch = 1)
points(data.neg$x1, data.neg$x2, col = "black", pch = 1)

# part h
svm.fit = svm(as.factor(y) ~ x1 + x2, data, gamma = 1)
svm.pred = predict(svm.fit, data)
data.pos = data[svm.pred == 1, ]
data.neg = data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "x1", ylab = "x2", pch = 1)
points(data.neg$x1, data.neg$x2, col = "black", pch = 1)

# Question 6

set.seed(927)

# parts a & b
n = 50
x = matrix(runif(n*2), ncol=2)
y = factor(x[,2] > x[,1])
df = data.frame(x, y)
cost = 10^(seq(-3,3,by=.5))
r = tune(svm, y ~ X1 + X2, data = df, kernel="linear", ranges = list(cost = cost))
summary(r) #best cost=3.162278

# part c
set.seed(1)

best_cost = r$best.parameters$cost
test_x = matrix(runif(10*n*2), ncol=2)
test_y = factor(test_x[,2] > test_x[,1])
test_df = data.frame(test_x, y=test_y)
cost_perform = double(length(cost))
for(n in seq_along(cost))
{
  c = cost[n]
  svm.mod = svm(y ~ X1 + X2, data = df, kernel="linear", cost = c)
  pred_y = predict(svm.mod, test_df)
  cost_perform[n] = mean(pred_y == test_y)
}
cbind(cost, cost_perform)

# Question 7

auto = read.csv("Documents/SMU/2019-20/Spring/STAT 6309/Homework/Homework Chapter 9/Auto.csv")

# part a
med = median(auto$mpg)
x1 = ifelse(auto$mpg > med, 1, 0)
auto$x1 = as.factor(x1)

# part b
library(e1071)
set.seed(3255)

cv = tune(svm, x1 ~ ., 
                data = auto, kernel = "linear", 
                ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000)))
summary(cv)

# part c
set.seed(21)

cv2 = tune(svm, x1 ~ ., 
           data = auto, 
           kernel = "polynomial", 
           ranges = list(cost = c(0.1, 1, 10, 100), degree = c(2, 3, 4, 5)))
summary(cv2)

# part d
linear = svm(x1 ~ ., data = auto, kernel = "linear", cost = 1)
poly = svm(x1 ~ ., data = auto, kernel = "polynomial", cost = 10, degree = 2)
radial = svm(x1 ~ ., data = auto, kernel = "radial", cost = 10, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(auto)[!(names(auto) %in% c("MPG", "x1", "name"))]) {
    plot(fit, auto, as.formula(paste("MPG~", name, sep = "")))
  }
}
plotpairs(linear)
plotpairs(poly)
plotpairs(radial)


# Question 8
# Entire is from online

# part a
data(OJ,package="ISLR")
set.seed(927)
train <- sample.int(nrow(OJ), size=800)

# part b
library(e1071)
svm.class <- svm(Purchase ~ ., data=OJ[train,], cost=0.01, kernel="linear")
summary(svm.class)

# part c
#Training error
class.train.pred <- predict(svm.class)
pur.train <- OJ[train,"Purchase"]
(class.train.err <- mean(pur.train != class.train.pred))
## [1] 0.16875
#Test error
class.test.pred <- predict(svm.class, OJ[-train,])
pur.test <- OJ[-train,"Purchase"]
(class.test.err <- mean(pur.test != class.test.pred))
## [1] 0.1407407

# part d
set.seed(927)
costs<-10^seq(-2,2,by=.25)
class.res <- tune(svm, Purchase ~ ., data=OJ[train,], ranges=list(cost=costs), kernel="linear")
summary(class.res)

# part e
#Training error
svm.best.class <- class.res$best.model
svm.best.class.pred <- predict(svm.best.class)
(best.class.train.err <- mean(pur.train != svm.best.class.pred)) #0.1675

#Test error
svm.best.class.test <- predict(svm.best.class, OJ[-train,])
(best.class.test.err <- mean(pur.test != svm.best.class.test)) #0.162963

