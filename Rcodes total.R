################################################################################
# Updated version of the code for the analysis in:
#
#   "Interrupted time series regression for the evaluation of public health 
#     interventions: a tutorial"
#   J. Lopez Bernal, S. Cummins, A. Gasparrini 
#   International Journal of Epidemiology - 2017
#   http://www.ag-myresearch.com/2017_lopezbernal_ije.html
#
# Update: 10 October 2018
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2016_lopezbernal_IJE_codedata
################################################################################

# Install packages required for the analysis (uncomment if needed)
install.packages("lmtest") ; install.packages("Epi")
install.packages("tsModel"); install.packages("vcd")
install.packages("data.table")
# load the packages
library(foreign) ; library(tsModel) ; library("lmtest") ; 
library("Epi");
library("splines") ; library("vcd") ; library(grid)


data <- Data_hamedan
head(data)
View(data)
################################################################################
#Step 3: Descriptive analyses
#######################################
# Examining the data is an important first step
# Looking at the pre-intervention trend can give an indication of how stable the
#   trend is over time, whether a linear model is likely to be appropriate, and
#   whether there appears to be a seasonal trend

## Scatter plot
data$cspercent <- with(data, CSP/pop*100)
# start the plot, excluding the points and the x-axis
plot(data$cspercent,type="n",ylim=c(00,100),xlab="Year", ylab="percent of CS",
  bty="l",xaxt="n")
rect(111,0,156,100,col=grey(0.9),border=F)
points(data$cspercent[data$g==0],cex=0.7)
axis(1,at=0:10*12,labels=F)
axis(1,at=0:10*12,tick=F,labels=1383:1393)
title("Percent of CS, 1384-1393")
summary(data)
summary(data$cspercent[data$g==0])
summary(data$cspercent[data$g==1])
################################################################################
#Step 5: methodological issues
##################################################################

#a) Overdispersion: Quasi-Poisson model 
# In the model above we have not allowed for overdispersion - in order to do
#   this we can use a quasipoisson model, which allows the variance to be
#   proportional rather than equal to the mean
# add a change-in-slope
# we parameterize it as an interaction between time and the ban indicator
model <- glm(CSP ~ offset(log(pop)) + g*time ,family=quasipoisson, data)
summary(model)
summary(model)$dispersion
round(ci.lin(model,Exp=T),3)
# create a new dataframe with 0.1 time units to improve the graph
datanew <- data.frame(pop=mean(data$pop),g=rep(c(0,1),c(1110,450)),time= 1:1560/10,month=rep(1:120/10,13))

# We generate predicted values based on the model in order to create a plot
pred1 <- predict(model,type="response",datanew)/mean(data$pop)*100

#This can then be plotted along with a scatter graph (see above)

plot(data$cspercent,type="n",ylim=c(0,100),xlab="Year",ylab="Percent of CS",bty="l",xaxt="n")
rect(111,0,156,100,col=grey(0.9),border=F)
points(data$cspercent,cex=0.7)
axis(1,at=0:14*12,labels=F)
axis(1,at=0:14*12,tick=F,labels=1383:1397)
lines((1:1560/10),pred1,col=2)
title("Percent of CS, 1384-1396")

# to plot the counterfactual scenario we create a data frame as if smokban
#   (the intervention) was never being implemented
datanew <- data.frame(pop=mean(data$pop),g=0,time=1:1560/10,month=rep(1:120/10,13))

# generate predictions under the counterfactual scenario and add it to the plot
pred1b <- predict(model1,datanew,type="response")/mean(data$pop)*100
lines(datanew$time,pred1b,col=2,lty=2)

# return the data frame to the scenario including the intervention
datanew <- data.frame(pop=mean(data$pop),g=rep(c(0,1),c(1110,450)), time= 1:1560/10,month=rep(1:120/10,13))

#b) Model checking and autocorrelation

# Check the residuals by plotting against time
res1 <- residuals(model1,type="deviance")
plot(data$time,res1,ylim=c(-10,15),pch=19,cex=0.7,col=grey(0.6),main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)

# Further check for autocorrelation by examining the autocorrelation and
#   partial autocorrelation functions
acf(res1)
pacf(res1)

#additional material
##################################################################
ts (frequency = 12, start = 1384) # freq 12 => Monthly data. 
model <- glm(cscount ~ offset(log(pop)),family=quasipoisson, data)
summary(model)
summary(model)$dispersion
round(ci.lin(model1,Exp=T),3)





#for comparison 
data$cscount <- with(data, cscount)
sd(data$cscount)
mean(data$cscount)
shapiro.test(data$cscount)
shapiro.test(data$cspercent)
