# STATS 503
#
# KNN
# load packages
library(class)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(gridExtra)

# data manipulation
## upload training data and check data
dat = read.csv("C:/Users/wenji/Downloads/STATS 503/HW1/diabetes_train.csv")
dat$Outcome = factor(dat$Outcome > 1/2)
levels(dat$Outcome) = c("not having diabetes", "having diabetes")
summary(dat)

## upload test data and check data
dat_test = read.csv("C:/Users/wenji/Downloads/STATS 503/HW1/diabetes_test.csv")
dat_test$Outcome = factor(dat_test$Outcome > 1/2)
levels(dat_test$Outcome) = c("not having diabetes", "having diabetes")
summary(dat_test)

### Training data set:
## There is no missing data. However, some data in the data set is impossible.
## Like no people could live with no glucose or insulin in his or her body. 
## Also, BMI and diastolic blood pressure are impossible to be 0.
## Based on above, I omit all the false data, in case those data could make the model become less accurate.
dat_clean = dat %>%
  filter(Glucose != 0 & BloodPressure != 0 & Insulin != 0 & BMI != 0)
summary(dat_clean)
## Although it also looks very rare that people can get pregnant in 17 times, we don't can enough clue to delete these data.

### Test data set
## For the test data set, we use the same process to manipulate data.
dat_test_clean = dat_test %>%
  filter(Glucose != 0 & BloodPressure != 0 & Insulin != 0 & BMI != 0)
summary(dat_test_clean)

## generate plot
box1 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = Pregnancies)) 
box2 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = Glucose)) 
box3 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = BloodPressure)) 
box4 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = SkinThickness)) 
box5 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = Insulin)) 
box6 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = BMI)) 
box7 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = DiabetesPedigreeFunction)) 
box8 = ggplot(dat_clean) + 
  geom_boxplot(aes(x = Outcome, y = Age)) 
grid.arrange(box1, box2, box3, box4, box5, box6, box7, box8, 
             widths = c(4, 6))
## From the box plots above, we can conclude that having diabetes or not highly related to the glucose concentration.

# KNN model
## select data from data set dat_clean and dat_test_clean
train_label = dat_clean %>% .$Outcome
test_label = dat_test_clean %>% .$Outcome
train_x = dat_clean %>%
  select('Pregnancies', 'Glucose', 'BloodPressure', 'SkinThickness', 'Insulin', 'BMI', 
         'DiabetesPedigreeFunction', 'Age')
test_x = dat_test_clean %>%
  select('Pregnancies', 'Glucose', 'BloodPressure', 'SkinThickness', 'Insulin', 'BMI', 
         'DiabetesPedigreeFunction', 'Age')

## scale training and test data in the same way
mean_train = colMeans(train_x)
std_train = sqrt(diag(var(train_x)))
train_x = scale(train_x, center = mean_train, scale = std_train)
test_x = scale(test_x, center = mean_train, scale = std_train)

## KNN model & k = 1
set.seed(1)
pred_train_1 = knn(train_x, train_x, train_label, k = 1)
pred_test_1 = knn(train_x, test_x, train_label, k = 1)
table(pred_train_1, train_label)  # all the data has been classified correctly
mean(pred_train_1 == train_label) # training error is 1 - 1 = 0
table(pred_test_1, test_label)
mean(pred_test_1 == test_label)   # test error is 1 - 0.80 = 0.20

## compute KNN model when k = 1, 2, 3, ..., 20 and get training error and test error respectively.
k_range = c(1:20)
train_error = c()
test_error = c()
for(i in 1:length(k_range)){
  pred_train <- knn(train_x, train_x, train_label, k = k_range[i])
  train_error[i] = mean(pred_train != train_label)
  pred_test = knn(train_x, test_x, train_label, k = k_range[i])
  test_error[i] = mean(pred_test != test_label)
}
## generate plot
errors = data.frame(train_error, test_error, k_range)
ggplot(errors, aes(x = k_range)) + 
  geom_line(aes(y = train_error), col = "red") + geom_point(aes(y = train_error), col = "red") +
  geom_line(aes(y = test_error), col = "blue") + geom_point(aes(y = test_error), col = "blue") +
  ylab("Error Rate") + xlab("K") + ggtitle("Training and test error rate for KNN") + theme_minimal()