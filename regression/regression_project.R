  library(ggplot2)
  library(stringr)
  library(Amelia)
  library(corrplot)
  library(mlbench)
  library(caret)
  library(glmnet)
  source("outliers.R")
  source("reg_perf.R")
  #source("plot_config.R")
  car <- read.csv('dataset/data.csv')
  #Dataset cleaning
  for(i in 1:2)
    car <- remove_outliers(car, "km_driven")
  crop <- ggplot(data=car, mapping=aes(x="km_driven", y=km_driven))+geom_boxplot() +
    scale_color_manual(values = c("#0099f8", "#e74c3c", "#2ecc71"))
  crop
  car$name <- word(car$name,1)
  #There are 32 unique values, so I manually assign a number to each name
  car$name <- str_replace(car$name, 'Maruti', '0')
  car$name <- str_replace(car$name, 'Skoda', '1')
  car$name <- str_replace(car$name, 'Honda', '2')
  car$name <- str_replace(car$name, 'Hyundai', '3')
  car$name <- str_replace(car$name, 'Toyota', '4')
  car$name <- str_replace(car$name, 'Ford', '5')
  car$name <- str_replace(car$name, 'Renault', '6')
  car$name <- str_replace(car$name, 'Mahindra', '7')
  car$name <- str_replace(car$name, 'Tata', '8')
  car$name <- str_replace(car$name, 'Chevrolet', '9')
  car$name <- str_replace(car$name, 'Fiat', '10')
  car$name <- str_replace(car$name, 'Datsun', '11')
  car$name <- str_replace(car$name, 'Jeep', '12')
  car$name <- str_replace(car$name, 'Mercedes-Benz', '13')
  car$name <- str_replace(car$name, 'Mitsubishi', '14')
  car$name <- str_replace(car$name, 'Audi', '15')
  car$name <- str_replace(car$name, 'Volkswagen', '16')
  car$name <- str_replace(car$name, 'BMW', '17')
  car$name <- str_replace(car$name, 'Nissan', '18')
  car$name <- str_replace(car$name, 'Lexus', '19')
  car$name <- str_replace(car$name, 'Jaguar', '20')
  car$name <- str_replace(car$name, 'Land', '21')
  car$name <- str_replace(car$name, 'MG', '22')
  car$name <- str_replace(car$name, 'Volvo', '23')
  car$name <- str_replace(car$name, 'Daewoo', '24')
  car$name <- str_replace(car$name, 'Kia', '25')
  car$name <- str_replace(car$name, 'Force', '26')
  car$name <- str_replace(car$name, 'Ambassador', '27')
  car$name <- str_replace(car$name, 'Ashok', '28')
  car$name <- str_replace(car$name, 'Isuzu', '29')
  car$name <- str_replace(car$name, 'Opel', '30')
  car$name <- str_replace(car$name, 'Peugeot', '31')
  car$name <- as.numeric(car$name)
  table(car$name)
  head(car, n=10)
  car <- subset (car, select = -torque)
  head(car, n=10)
  #Removing unit from mileage, converting it to numeric value and replacing the missing values
  car$mileage <- str_replace(car$mileage, 'kmpl', '')
  car$mileage <- str_replace(car$mileage, 'km/kg', '')
  car$mileage <- as.numeric(car$mileage)
  car$mileage[is.na(car$mileage)]<-mean(car$mileage,na.rm=TRUE)
  #Removing unit from engine, converting it to numeric value and replacing the missing values
  car$engine <- str_replace(car$engine, 'CC', '')
  car$engine <- as.numeric(car$engine)
  car$engine[is.na(car$engine)]<-mean(car$engine,na.rm=TRUE)
  #Removing unit from max_power, converting it to numeric value and replacing the missing values
  car$max_power <- str_replace(car$max_power, 'bhp', '')
  car$max_power <- as.numeric(car$max_power)
  car$max_power[is.na(car$max_power)]<-mean(car$max_power,na.rm=TRUE)
  #Converting seats to numeric value and replacing the missing values
  car$seats <- as.numeric(car$seats)
  car$seats[is.na(car$seats)]<-median(car$seats,na.rm=TRUE)
  car$mileage[car$mileage == ""] <- NA
  car$engine[car$engine == ""] <- NA
  car$max_power[car$max_power == ""] <- NA
  # Checking for missing values
  sapply(car, function(x) sum(is.na(x)))
  missmap(car, legend = TRUE, col = c("red", "blue"))
  ggplot(data = car, aes(x=reorder(fuel, fuel, function(x)-length(x)), fill = fuel)) +
    geom_bar() + labs(x='Fuel') + labs(title = "Bar Graph of Fuel")
  ggplot(data = car, aes(x=reorder(owner, owner, function(x)-length(x)), fill = owner)) +
    geom_bar() + labs(x='Owner') + labs(title = "Bar Graph of Owner") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data = car, aes(x=reorder(seats, seats, function(x)-length(x)), fill = seats)) +
    geom_bar() + labs(x='Seats') + labs(title = "Bar Graph of Seats") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
  car$transmission <- str_replace(car$transmission, 'Manual', "0")
  car$transmission <- str_replace(car$transmission, 'Automatic', "1")
  car$transmission <- as.numeric(car$transmission)
  car$owner <- str_replace(car$owner, 'First Owner', "0")
  car$owner <- str_replace(car$owner, 'Second Owner', "1")
  car$owner <- str_replace(car$owner, 'Third Owner', "2")
  car$owner <- str_replace(car$owner, 'Fourth & Above Owner', "3")
  car$owner <- str_replace(car$owner, 'Test Drive Car', "4")
  car$owner <- as.numeric(car$owner)
  car$seller_type <- str_replace(car$seller_type, "Trustmark Dealer", "0")
  car$seller_type <- str_replace(car$seller_type, "Dealer", "1")
  car$seller_type <- str_replace(car$seller_type, "Individual", "2")
  car$seller_type <- as.numeric(car$seller_type)
  car$fuel <- str_replace(car$fuel, 'Diesel', "0")
  car$fuel <- str_replace(car$fuel, 'Petrol', "1")
  car$fuel <- str_replace(car$fuel, 'CNG', "2")
  car$fuel <- str_replace(car$fuel, 'LPG', "3")
  car$fuel <- as.numeric(car$fuel)
  #Histogram of Selling price
  ggplot(car, aes(x=selling_price)) + 
    geom_histogram(bins = 200, color="black", fill="blue") + 
    labs(x='Selling Price ') + labs(title = "Histogram of Selling Price") +
    scale_x_continuous(labels = scales::comma,trans='log10') 
  
  #Histogram of Km driven
  ggplot(car, aes(x=km_driven)) + 
    geom_histogram(bins = 200, color="black", fill="blue") + 
    labs(x='Km driven ') + labs(title = "Histogram of Km driven") +
    scale_x_continuous(labels = scales::comma,trans='log10') 
  
  #Primo test 
  set.seed(5)
  options(scipen=999)
  ggplot(stack(car), aes(x = ind, y = values, color=ind)) + 
    geom_boxplot(alpha=0.3)
  correlationMatrix <- cor(car)
  #corrplot(correlationMatrix, type="full", 
  #         method ="color", title = "Correlation Plot", 
  #         mar=c(0,0,1,0), tl.cex= 0.8, outline= T, tl.col="indianred4")
  
  trainIndex <- createDataPartition(car$selling_price, p = .7,
                                    list = FALSE,
                                    times = 1)
  train <- car[ trainIndex,]
  test <- car[-trainIndex,]
  m1_lr <- lm(selling_price ~ ., data = train)
  summary(m1_lr)
  m2_lr <- lm(selling_price ~ name + year + km_driven + seller_type + 
                mileage + transmission + max_power, data = train)
  m2_lr
  model_summ <- summary(m2_lr)
  model_summ
  pred_lr <- m2_lr %>% predict(test)
  rmse_orm <- RMSE(pred_lr, test$selling_price)
  rsquare_lrm <- R2(pred_lr, test$selling_price)
  rmse_orm
  rsquare_lrm
  #Ridge regression
  #define response variable
  y <- test$selling_price
  
  #define matrix of predictor variables
  x <- data.matrix(test[, c('name', 'year', 'km_driven', 'seller_type',"mileage","transmission","max_power")])
  ridge_md <- glmnet(x, y, alpha = 0)
  
  #view summary of model
  summary(ridge_md)
  cv_model <- cv.glmnet(x, y, alpha = 0)
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  best_lambda
  plot(cv_model)
  #find coefficients of best model
  best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  coef(best_model)  
  #produce Ridge trace plot
  plot(ridge_md, xvar = "lambda")
  #use fitted best model to make predictions
  y_predicted <- predict(cv_model, s = best_lambda, newx = x)
  
  #find SST and SSE
  sst <- sum((y - mean(y))^2)
  sse <- sum((y_predicted - y)^2)
  
  #find R-Squared
  rsq_ridge <- 1 - sse/sst
  rsq_ridge  
  rmse_ridge <- sqrt(sse/nrow(test))
  rmse_ridge
  
  #Lasso regression model
  #define response variable
  y <- test$selling_price
  
  #define matrix of predictor variables
  x <- data.matrix(test[, c('name', 'year', 'km_driven', 'seller_type',"mileage","transmission","max_power")])
  #perform k-fold cross-validation to find optimal lambda value
  cv_model <- cv.glmnet(x, y, alpha = 1)
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  best_lambda

  #produce plot of test MSE by lambda value
  plot(cv_model) 
  
  #find coefficients of best model
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  coef(best_model)
  
  #use lasso regression model to predict response value
  y_predicted <- predict(best_model, s = best_lambda, newx = x)
  
  #find SST and SSE
  sst <- sum((y - mean(y))^2)
  sse <- sum((y_predicted - y)^2)
  
  #find R-Squared
  rsq_lasso <- 1 - sse/sst
  rsq_lasso
  rmse_lasso <- sqrt(sse/nrow(test))
  rmse_lasso
  
  print("--------- LRM ---------")
  print("R-SQUARED: ")
  print(rsquare_lrm)
  print("RMSE: ")
  print(rmse_orm)
  print("---------RIDGE---------")
  print("R-SQUARED: ")
  print(rsq_ridge)
  print("RMSE: ")
  print(rmse_ridge)
  print("---------LASSO---------")
  print("R-SQUARED: ")
  print(rsq_lasso)
  print("RMSE: ")
  print(rmse_lasso)
  
  