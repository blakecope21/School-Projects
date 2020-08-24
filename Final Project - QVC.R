df <- read.csv("QVC_Data_Complete.csv")

summary(df)

df$Delivery_Confirmation_Dt <- as.POSIXct(df$Delivery_Confirmation_Dt, format = 
                                             "%m/%d/%Y %H:%M")
df$Shipped_Dt <- as.POSIXct(df$Shipped_Dt, format = "%m/%d/%Y %H:%M")

dftime <- difftime(df$Delivery_Confirmation_Dt, df$Shipped_Dt, units = 'days')
df$Delivery_Time_Days <- dftime

summary(df)

summary(df$Delivery_Time_Days)

head(df$Delivery_Time_Days)

df <- subset(df, select=-c(Cancelled_Qty))

df2 <- df

summary(df$Party_Id)
head(df$Party_Id)
summary*df2$Delivery_Time_Days

# Create order Frequency variable
Freq_Var = ave(as.numeric(df$Party_Id), df$Party_Id, FUN=length)

df$Order_Freq = Freq_Var

summary(df$Order_Freq)

#drop NA
df2$Delivery_Time_Days = with(df2,impute(Delivery_Time_Days, 0))
df2 = df2[!(df2$Delivery_Time_Days <= 0),]


#Normalize Order_Freq
df2 <- subset(df, Order_Freq <= 70)
summary(df2)

# Imputations
# Changed class to numeric imputed with median and changed to digits
df2$Delivery_Time_Days = as.numeric(df2$Delivery_Time_Days)
df2 = df2[!(df2$Delivery_Time_Days < 0),]
df2$Delivery_Time_Days <- with(df2, impute(Delivery_Time_Days, median))


df3 <- subset(df2, Delivery_Time_Days <=9)
df3$Delivery_Time_Days = trunc(df3$Delivery_Time_Days)

# Group by partyid and order freq
group_by(df3$Party_Id)

# Box Plot to find Outliers
boxplot(df$Order_Freq, main="", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))
# Linear Regression using seed 101
# Also using training, valid, test (40,20,20)
set.seed(101)
sample <- sample.split(df2$Order_Freq, SplitRatio = 0.7)
df.train <- subset(df2, sample==TRUE)
df.test = subset(df2, sample ==FALSE)
sample2 <- sample.split(df2$Order_Freq, SplitRatio = 0.5)
df.t <- subset(df2, sample==TRUE)
df.valid <- subset(df2, sample==FALSE)

model_Linear_Regression_1 <- lm(formula = Order_Freq ~ Delivery_Time_Days, data = df.train)
summary(model_Linear_Regression_1)






pred.probabilities <- predict(model_Linear_Regression_1, newdata=df.valid,type='response')
pred.results <- ifelse(pred.probabilities > 0.5,1,0)
print(table(pred.results, df.valid$Order_Freq))
pred <- prediction(pred.results,df.valid$Order_Freq)
perf <- performance(pred, "tpr", "fpr")


# Neural Network
library(neuralnet)
nn <- neuralnet(Order_Freq~Delivery_Time_Days, data=df.train,hidden=c(5,3),linear.output=TRUE)
plot(nn)
pred.probabilities <- compute(nn,df.valid[1:35])
pred.probabilities <- as.numeric(pred.probabilities$net.result)
pred.Result <- ifelse(pred.probabilities>0.5,1,0)
print(table(pred.Result,df.valid$Order_Freq))
pred <- prediction(pred.probabilities,df.valid$Order_Freq)
pr.nn <- compute(nn,df.test[,1:35])



