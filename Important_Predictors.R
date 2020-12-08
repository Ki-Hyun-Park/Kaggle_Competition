```{r}
library(data.table)
library(dplyr)
library(lubridate)

#### Imported train.csv and exclude non-needed variables


alias.mat <- as.data.frame(alias(glm(HTWins ~., data = train[, -c(1,2,4:8)], family = "binomial"))$Complete)[, -1]

t <- as.data.frame(which(alias.mat > 0, arr.ind=T))
t$colname <- colnames(alias.mat)[t$col]


colli.vars <- colnames(alias.mat)[t[, 2]]


train2 <- train %>% mutate(date2 = ymd(date)) %>%  
  mutate(year = lubridate::year(date2)) %>% 
  dplyr::select(-id, -gameID, -date2, -colli.vars) 



train2$HTWins01 <- as.numeric(train2$HTWins == "Yes")


train2$HTWins.MA <- HT.MA(train2$HTWins01)



## Cummulative average and Moving Average 

HT.CA <- function(x){               # cummulative mean for each HT
  ave(x, train2$HT, FUN=cummean)
}

VT.CA <- function(x){               # cummulative mean for each VT
  ave(x, train2$VT, FUN=cummean)
}


library(zoo)
HT.MA <- function(x){               # Moving average for each HT
  ave(x, train2$HT, FUN= function(z) rollmean(z, 5, fill = NA, align = "right"))
}

VT.MA <- function(x){               # Moving average for each VT
  ave(x, train2$VT, FUN= function(z) rollmean(z, 5, fill = NA, align = "right"))
}

grep("VT.S1.pts", colnames(train2))     # 9
grep("VT.pmxW", colnames(train2))       # 50
grep("HT.TS.fgm", colnames(train2))     # 51
grep("HT.pmxW", colnames(train2))       # 154


# Apply CA for each variable by HT and VT
train2.HT.CA <- apply(train2[, -c(1:8, 9:50, 155)], 2, HT.CA)
colnames(train2.HT.CA) <- paste0(colnames(train2.HT.CA), rep(".CA", ncol(train2.HT.CA))) 

train2.VT.CA <- apply(train2[, -c(1:8, 51:154, 155)], 2, VT.CA)
colnames(train2.VT.CA) <- paste0(colnames(train2.VT.CA), rep(".CA", ncol(train2.VT.CA))) 


# Apply 5 game MA for each variable by HT and VT
train2.HT.MA <- apply(train2[, -c(1:8, 9:50, 155)], 2, HT.MA)
colnames(train2.HT.MA) <- paste0(colnames(train2.HT.MA), rep(".MA", ncol(train2.HT.MA))) 

train2.VT.MA <- apply(train2[, -c(1:8, 51:154, 155)], 2, VT.MA)
colnames(train2.VT.MA) <- paste0(colnames(train2.VT.MA), rep(".MA", ncol(train2.VT.MA))) 

# Combine old and new variables
train3 <- as.data.frame(cbind(train2, train2.HT.CA, train2.VT.CA, 
                              train2.HT.MA, train2.VT.MA))

# checked CA and MA for each team (just to make sure it works for each team)

train3 %>% dplyr::select(HT, HT.TS.pts, HT.TS.pts.CA) %>% filter(HT == "AJAX")
train3 %>% dplyr::select(VT, VT.TS.pts, VT.TS.pts.CA) %>% filter(VT == "AJAX")

train3 %>% select(date, HT, HT.TS.pts, HT.TS.pts.MA) %>% filter(HT == "AJAX")
train3 %>% select(date, VT, VT.TS.pts, VT.TS.pts.MA) %>% filter(VT == "AJAX")


test <- fread("test.csv", header = TRUE, stringsAsFactors = TRUE)
test %>% group_by(HT) %>% summarise(count = n())
```



```{r}
training <- train3[train3$year >= 2001 & train3$year <= 2010, ]
testing <- train3[train3$year >= 2011 & train3$year <= 2012, ]


grep("year", colnames(train3))
# train.ht <- train3 %>% group_by(HT) %>% summarise(c = n()) %>% dplyr::select(HT) %>% unlist() %>% as.character()
# 
# train3 <- subset(train3, train3$HT == train.ht[2])
# 
# training <- train3[train3$year >= 2001 & train3$year <= 2010, ]
# testing <- train3[train3$year >= 2011 & train3$year <= 2012, ]



training.log <- glm(HTWins ~ ., data = training[, -c(2,3,6,155)], family = "binomial")
sum.log <- summary(training.log)
sum.log
log.pred <- predict(training.log, newdata = testing[, -c(2,3,6,155)], type = "response")
log.predYN <- ifelse(log.pred > 0.5, "Yes", "No")
table(log.predYN, testing$HTWins)
mean(log.predYN == testing$HTWins)




i <- seq(10, -2, length = 100)    
lambda.v <- 10^i    # create a sequence of lambda from 10^-2 to 10^10

## Train train.csv by Ridge 
library(glmnet)
set.seed(123)

training.sc <- model.matrix(HTWins ~., data = training[, -c(2,3,6,155)])

train.ridge <- glmnet(training.sc, training$HTWins, alpha = 1, lambda = lambda.v, family = "binomial")
ridge_coef <- coef(train.ridge)

cv.ridge <- cv.glmnet(training.sc,  training$HTWins, alpha = 1, family = "binomial") # cross validations for ridge
cv.ridge$lambda.min      # best lambda that minimizes the MSE
predict(train.ridge, s = cv.ridge$lambda.min, type = "coefficients")


ridge_pred <- predict(train.ridge, s = cv.ridge$lambda.min, newx = as.matrix(testing[- c(1:5)]), 
                      type = "class")
table(ridge_pred, testing$HTWins)
mean(ridge_pred == testing$HTWins)


```







```{r}

tr <- train %>% mutate(pairs = paste(train$HT, "-", train$VT)) %>% 
  group_by(pairs) %>% summarise(htwins = sum(HTWins == "Yes"))

t <- test %>% mutate(pairs = paste(test$HT, "-", test$VT)) %>% 
  group_by(pairs) %>% summarise(pts = mean(HT.TS.pts))

train$pairs <- paste(train$HT, "-", train$VT)
test$pairs <-  paste(test$HT, "-", test$VT)


test_w_pairs <- left_join(test, tr, by = "pairs")

plot(test_w_pairs$htwins, test_w_pairs$HT.TS.pts)
cbind(tr[-650, ], t)


# Removed collinearity
# alias.mat <- as.data.frame(alias(glm(HTWins ~., data = train[, -c(1,2,4:8)], family = "binomial"))$Complete)[, -1]
# 
# t <- as.data.frame(which(alias.mat > 0, arr.ind=T))
# t$colname <- colnames(alias.mat)[t$col]
# t
# colli.vars <- rownames(alias.mat)[t[, 1]]


# train$pairs <- as.factor(paste(train$HT, "-", train$VT))  # pair HT-VT

# 
# library(lubridate)
train.sub <- train %>% dplyr::mutate(date2 = ymd(date)) %>%
  dplyr::mutate(year = lubridate::year(date2), #day = lubridate::day(date2))
                month = lubridate::month(date2)) %>%
  dplyr:: select(-c(colli.vars, date2), -c(1,2,4,5,8:10))

names(train.sub)

```



```{r}
## Imported test.csv and excluded non-needed variables
test <- fread("test.csv", header = TRUE, stringsAsFactors = TRUE)
test <- fread("test.csv", stringsAsFactors = TRUE)
test2 <- train[, -c(1,2,4:8)]

alias.mat <- as.data.frame(alias(glm(HTWins ~., data = test[, -c(1,2,4:8)], family = "binomial"))$Complete)[, -1]

t <- as.data.frame(which(alias.mat > 0, arr.ind=T))
t$colname <- colnames(alias.mat)[t$col]
t
colli.vars <- rownames(alias.mat)[t[, 1]]


test.sub <- test %>% dplyr::mutate(date2 = ymd(date)) %>% 
  dplyr::mutate(year = lubridate::year(date2), #day = lubridate::day(date2)) 
                month = lubridate::month(date2)) %>% 
  dplyr:: select(-c(colli.vars, date2), -c(1,2,3,4,7,8,9))

```                              
  
#### Boxplot 

```{r}
## Overall boxplot of HTWins 
vars <- colnames(train)[9:ncol(train)]
for (i in 9:ncol(train)){
  plot(x = train$HTWins, y = train[[i]], ylab = vars[i-8])
  points(tapply(train[[i]], train$HTWins, mean), pch = 19, col = "red")
}


## Correlation plot

# install.packages("corrplot")
library(corrplot)
res <- cor(train_non_colli[, -(1:11)])
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

```



#### Find colinearity variables


```{r}

alias.mat <- as.data.frame(alias(glm(HTWins ~., data = train2[, -(2:5)], family = "binomial"))$Complete)

t <- as.data.frame(which(alias.mat > 0, arr.ind=T))
t$colname <- colnames(alias.mat)[t$col]
t
colli.vars <- colnames(alias.mat)[t[, 2]]


library(dplyr)
train3 <- train2 %>% dplyr::select(-colli.vars)
test3 <- test2 %>% dplyr::select(-colli.vars)



```
  
