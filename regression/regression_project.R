library(ggplot2)
library(stringr)
library(Amelia)
library(corrplot)
library(mlbench)
library(caret)
car <- read.csv('dataset/data.csv')
#Dataset cleaning
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
correlationMatrix <- cor(car)
#corrplot(correlationMatrix, type="full", 
#         method ="color", title = "Correlation Plot", 
#         mar=c(0,0,1,0), tl.cex= 0.8, outline= T, tl.col="indianred4")
#Finding and removing higly correlated features with 0.5 cutoff
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
highlyCorrelated <- sort(highlyCorrelated)
highlyCorrelated <- highlyCorrelated[highlyCorrelated != 3]
reduced_car = car[, -highlyCorrelated]
trainIndex <- createDataPartition(reduced_car$selling_price, p = .7,
                                  list = FALSE,
                                  times = 1)
train <- reduced_car[ trainIndex,]
test <- reduced_car[-trainIndex,]
m1_lr <- lm(selling_price ~ ., data = train)
#R Squared di merda, p value del cazzo 
summary(m1_lr)

m2_lr <- lm(selling_price ~ name + km_driven + fuel + seller_type + 
              transmission + owner, data = train)
summary(m2_lr)

pred_lr <- m2_lr %>% predict(test)
RMSE(pred_lr, test$selling_price)
# (b) R-square
R2(pred_lr, test$selling_price)