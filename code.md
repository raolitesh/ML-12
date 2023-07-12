# codes are written in R studio
```r
# installing and loading packages
install.packages("forecast")
install.packages("xlsx")
install.packages("tseries")
library("forecast")
library("xlsx")
library("tseries")
```
```r
# reading the data
coca <- read.xlsx(file.choose(),1)
attach(coca)
View(coca)
summary(coca)
sd(Sales) 
var(Sales)
# line plot of sales
plot(Sales, type = "l") 
```
```r
# creation of dummies for four quarters
coca["Qs"] <- rep(1:4, len=42)
coca[paste0('Q', 1:4)] <- model.matrix(~ factor(coca$Qs)-1)
```
```r
# creating a new variable t taking values from 1 to 42 and corresponding $t^2$ variable
coca["t"]<- 1:42
coca["t_square"] <- coca["t"] * coca["t"]
# log of sales
coca["log_sales"] <- log(coca["Sales"])
```
```r
# training and testing data
attach(coca)
train <- coca[1:28,]
test <- coca[29:42,]
```
```r
# executing different models and collecting RMSE
# buidling the model on training data and prediction is done for testing data

# model 1: Linear Regression Model
lin_mod <- lm(Sales~t , data = train) 
summary(lin_mod)
lin_pred <- data.frame(predict(lin_mod, interval = "predict", newdata = test)) 
View(lin_pred)
rmse_lin_model <- sqrt(mean((test$Sales - lin_pred$fit)^2, na.rm = T))
rmse_lin_model

# model 2: Exponential Model
exp_mod <- lm(log_sales~t, data = train) 
summary(exp_mod)
exp_pred <- data.frame(predict(exp_mod, interval = "predict", newdata = test))
View(exp_pred)
rmse_exp_mod <- sqrt(mean((test$Sales - exp(exp_pred$fit))^2, na.rm = T))
rmse_exp_mod

# model 3: Quadratic Model
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

# model 4: Additive Seasonality model
sea_add_model<-lm(Sales~Q1+Q2+Q3,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

# model 5: Additive Seasonality with Linear Regression model
Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

# model 6: Additive Seasonality with Quadratic model
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

# model 7: Multiplicative Seasonality Model
multi_sea_model<-lm(log_sales~Q1+Q2+Q3,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

# model 8: Multiplicative Seasonality with Linear regression Model
multi_sea_lin_model<-lm(log_sales~t+Q1+Q2+Q3,data = train)
summary(multi_sea_lin_model) 
multi_sea_lin_pred<-data.frame(predict(multi_sea_lin_model,newdata=test,interval='predict'))
rmse_multi_sea_lin<-sqrt(mean((test$Sales-exp(multi_sea_lin_pred$fit))^2,na.rm = T))
rmse_multi_sea_lin
```
```r
collecting all RMSE in a table
table_rmse<-data.frame('Model'=c("Linear Regression Model","Exponential Model","Quadratic Model","Additive Seasonality model","Additive Seasonality with Linear Regression model","Additive Seasonality with Quadratic model","Multiplicative Seasonality Model","Multiplicative Seasonality with Linear regression Model"),'RMSE'=c(rmse_lin_model,rmse_exp_mod,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_sea_lin))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
```
## choose which gives the least RMSE for forecasting. In this example, Multiplicative Seasonality with Linear Regression Model gives the least RMSE
## we are using this to build forecast
```r
# buidling the prediction
multi_sea_lin_model_whole <- lm(log_sales~t+Q1+Q2+Q3, data = coca)
# residuals
resid <- residuals(multi_sea_lin_model_whole)
# histogram of residuals
hist(resid)
```

