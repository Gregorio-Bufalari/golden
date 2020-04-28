#First part:


#The dataset we worked on is a time series of Tesla stock prices in the last three years, from 01-02-2017 to 01-02-2020.
# We chose to work on Tesla because of the recent crazy trend in its stock price.
# We first performed a simple linear regression to study the correlation between the closing price at time t-1 (dependent variable)
# and the closing price at time t (independent variable).
# Then we performed a Durbin-Watson test to check whether there is autocorrelation (correlation between residuals) or not.

# Importing the packages needed.
library(readr)
ds <- read_csv("Desktop/durbin wtason test/tsla.csv")
View(tsla)
library(lmtest)

# Loading and adjusting the dataset.
# Loading the dataset
ds <- ds[, c(1,6)]    # Adjusting the dataset according to our needs
'Adj.Close.t-1' <- ds$Adj.Close     # Creating and adjusting the column for prices at time t-1
'Adj.Close.t-1' <- append('Adj.Close.t-1', 251.93, after=0)   # Price at 31-01-2017 manually retrieved and added
len <- length('Adj.Close.t-1')
'Adj.Close.t-1' <- 'Adj.Close.t-1'[-len]    # Removing the last price (we don't need it)
ds$'Adj.Close.t-1' <- 'Adj.Close.t-1'    # Adding the column to our dataset


# Performing a simple linear regression.
x <- ds$'Adj.Close.t-1'   # Storing the dependent variable in an extern variable for simplicity
y <- ds$`Adj.Close`        # Storing the independent variable in an extern variable for simplicity
plot(x,y)   # Plotting the points to have an initial view of how data are distributed
slrmodel <- lm(y~x)   # Initializing and fitting the model
abline(slrmodel, col="red")   # Plotting the model adding the regression line
segments(x, y, x, slrmodel$fitted.values)   # Adding segments to better visualize the residuals
summary(slrmodel)

# As we can see from the summary, the resulting model is:
# price(t) = -1.955627 + 1.008035 * price(t-1) + eps.

# The model seems to work pretty well in predicting the price:
# An R^2 value of more than 97% and p-value close to 0 for the regression slope let us conclude that most of the
# model is explained by the dependent variable, therefore a linear relationship does exist.
# However, we want to study the error component of the model, therefore we perform the Durbin-Watson test.

# Performing the Durbin-Watson test
dwmodel <- dwtest(slrmodel)
res <- residuals(slrmodel)
plot(head(res,n=50),type='b',main='Residuals')
plot(predict(slrmodel), residuals(slrmodel))
residuals.df <- data.frame(res)

# We previously said that the Durbin Watson test reports a test statistic, with a value from 0 to 4, where:
# 2 is no autocorrelation; 0 to 2 is positive autocorrelation, and 2 to 4 is negative autocorrelation.
# Given the DW test value of 1.9766 and the p-value of 0.3601, we can conclude that there is no autocorrelation in residuals.

##Second Part:

#Now we analyze a dataset about House prices: 

library(readr)
ds <- read_csv("salespred.csv")
View(ds)


#Adjusting dataset: remove first column and select the needed columns

ds <- ds[,-1]
View(ds)
ds1 <- ds[,c(1,2,3,9,10)]
View(ds1)

#Split the dataset into training and test

ind <- sample(2,nrow(ds1),replace=T,prob=c(0.8,0.2))
tdata <- ds1[ind==1,]
head(tdata)
vdata <- ds1[ind==2,]

#perfmorming the linear regression model with 4 indipendent variables

lm.fit <- lm(Price_house~prediction+Taxi_dist+Market_dist+Hospital_dist
             ,tdata)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

library(ggplot2)
db1 %>% 
  ggplot(aes(x,y))+
  geom_point()+
  stat_smooth(method='lm')+
  ggtitle('Linear Regression')

#Residuals Analysis:

res <- residuals(lm.fit)
data.frame(res)

plot(head(res,n=70),type='b',main='Residuals')

#Performing DW test:

library(lmtest)
dwtest <- dwtest(lm.fit) #durbin-watson test 
dwtest

#Performing Cochrane-Orcutt test
library(orcutt)
orcutlm.fit <- cochrane.orcutt(lm.fit) #cochrane orcutt test
summary(orcutlm.fit)
dwtest(orcutlm.fit)


##Third part:

#Breusch Godfrey Test 
install.packages("dynlm", repos = "https://cran.r-project.org/")
library(dynlm)
library(AER)

data('USMacroG')
consump.1 <- dynlm(consumption ~ dpi + L(dpi), data = USMacroG) #with L() we obtained lagging component 
?dynlm
res <- residuals(consump.1)
data.frame(res)

library(car)
durbinWatsonTest(consump.1)
durbinWatsonTest(consump1, max.lag = 4)
dwtest(consump.1)

bgtest(consump.1)

#To see the difference between DW and BG tests we can construct an other model.

consump2 <- dynlm(consumption ~ dpi + L(dpi) + L(consumption, 2)+ L(consumption), data = USMacroG)
durbinWatsonTest(consump2, max.lag = 4)
bgtest(consump2)


##White HC Robust Standard Errors
library(sandwich)
coeftest(consump.1, vcov = vcovHC(consump.1, type = "HC1"))
summary(consump.1)

##Newey West HAC Robust Standard Errors
coeftest(consump.1, vcov = NeweyWest(consump.1))
coeftest(consump.1)


