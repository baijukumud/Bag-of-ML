install.packages("https://cran.r-project.org/src/contrib/Archive/ElemStatLearn/ElemStatLearn_2015.6.26.2.tar.gz", repos = NULL, type = "source")
install.packages('e1071')

library(caTools)
library(ElemStatLearn)
library(e1071)

dataset = read.csv('./Social_Network_Ads.csv')
dataset = dataset[3:5]
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set[-3] = scale(training_set[-3]) 
test_set[-3] = scale(test_set[-3]) 
classifier = naiveBayes(x = training_set[-3], y = training_set$Purchased)
y_pred = predict(classifier, newdata = test_set[-3])
cm = table(test_set[, 3], y_pred) 
print(cm)
set = training_set
print(set)

X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(
  set[, -3],
  main = 'Naive Bayes (Training set)',
  xlab = 'Age', ylab = 'Estimated Salary', 
  xlim = range(X1), 
  ylim = range(X2)
)
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(
  set[, -3],
  main = 'NaiveBayes (Test set)',
  xlab = 'Age', ylab = 'Estimated Salary', 
  xlim = range(X1), ylim = range(X2)
)

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
