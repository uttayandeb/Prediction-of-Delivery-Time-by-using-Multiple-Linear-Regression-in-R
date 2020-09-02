######## choosing  the Delivery_Time.csv dataset#######
#####****** Understanding and reading the data****######

dl.tm <- read.csv(file.choose()) 

dt.st <- dl.tm
View(dt.st)
names(dt.st)#[1] "Delivery.Time" "Sorting.Time"
# 21 Observations of 2 variables




#############**** Scatter Diagram (Plot x,y) between 2 continuous variables*****#############

plot(dt.st$Sorting.Time,dt.st$Delivery.Time)

###### Other Exploratory data analysis and Plots ########

boxplot(dt.st)

hist(dt.st$Sorting.Time)

hist(dt.st$Delivery.Time)

summary(dt.st)

#*** Correlation coefficient value for Delivery Time and Sorting Time ***##

dt<- dt.st$Delivery.Time
View(dt)
st <- dt.st$Sorting.Time
View(st)
cor(st,dt)#[1] 0.8259973,correlation value
## If corelation valueis greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.8259973). 
## This it has a moderate Correlation 

#so the transformation of variables is needed


##### 1st Simple model  without any transformation  #####
##### deploying the liear model ####

reg<-lm(dt~st)
View(reg)
reg$residuals
mean(reg$residuals)#[1] 4.758099e-17

sqrt(sum(reg$residuals^2)/nrow(dt.st))  # root mean square eoor(RMSE)
#[1] 2.79165
sqrt(mean(reg$residuals^2))
#[1] 2.79165
summary(reg)

###the probability value should be less the 0.05(4.54e-05) and the P- value is  3.983e-06
### therefore the p value is overall less
### teh multiple R squared value is 0.6823 which is very less as compared to 0.8259973




#######***** confidence interval ***#######

confint(reg,level = 0.95)
#so we have two range upper and lower
#2.5% is the lower and 97.5% is the upper
# so we will get two equations from here
# st=2.979134+1.108673*dt
# st=10.186334+2.189367*dt
# to calculate the lower range and the upper range

# Function to Predict the above model 
predict(reg,interval="predict")


#ggplot(dt.st,aes(dt,st))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')


# we have to do transformation of variables for better R-squared value
# Applying transformations

# LOGARTHMIC TRANSFORMATION
plot(log(dt), st)


cor(log(dt), st)#[1] 0.8431773,the R squared value is increased now little bit

reg_log<-lm(st~log(dt))  # Regression using logarthmic transformation
View(reg_log)
summary(reg_log)#miltiple R square valur is  0.7109,


confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")



###******** Exponential Model or Transformation ************###

plot(dt,log(st))

cor(dt,log(st))#[1] 0.8339325

reg_exp<-lm(log(st)~dt)
View(reg_exp)

summary(reg_exp)# R square value is 0.69

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))#[1] 0.2520376

pred_2 <- predict(reg_exp)
View(pred_2)
st_1 <- exp(pred_2)
View(st_1)



sqrt(sum(error^2)/nrow(dt.st))  #####RMSE####
#[1] 0

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")



################ Quadratic model #################

dt.st[,"st_sq"] = st*st

# Quadratic model 1
quad_mod <- lm(dt~st+I(st^2),data=dt.st)
summary(quad_mod)


confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Quadratic model 2
qd_model <- lm(dt~st+st_sq,data=dt.st)
summary(qd_model)


confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")



# Cubic model
poly_mod <- lm(dt~st+I(st^2)+I(st^3),data=dt.st)
summary(poly_mod) #  0.7034,
confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")

logpol3 <- predict(poly_mod)
expy3 <- exp(logpol3)
View(expy3)

###################### visualization ##########
ggplot(data = dt.st, aes(x = dt + I(dt^2) + I(dt^3), y = st)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt.st, aes(x=dt+I(dt^2)+I(dt^3), y=expy3))





model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c( 0.6823, 0.7109,0.6954, 0.6934, 0.7034)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# LOGARTHMIC  model gives the best Adjusted R-Squared value
predicted_Value <- exp(predict(reg_log))
predicted_Value


Final <- cbind(Sorting_Time=dt.st$Sorting.Time ,Delivery_Time = dt.st$Delivery.Time,Predicted_Delivery_time=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-dt)^2))
rmse#[1] 6121.54





plot(reg_exp)
hist(residuals(reg_log)) # close to normal distribution
###### therefore Logarathmic model gives the best R squared value which is equal to 0.7109,
