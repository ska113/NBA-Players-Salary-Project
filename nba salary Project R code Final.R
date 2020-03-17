#NBA players stat and salary in 2016-2017 season
setwd("C:\Users\cchris86\STAT\350\Report")
salaryo = read.csv('NBA_2015.csv',header=T)
salaryo = na.omit(salaryo)
colnames(salaryo)[21] = 'SALARY'
colnames(salaryo)[12] = 'AGE'



#Objective
#Determining the most important factors that affects the players' salary 
#in National Bastetketball Association

#Data description:
#The data is acquired from
#Source: Kaggle.com
#523 entries with 25 variables
#They are All Numeric Variables 

#Response: 
#Salary (in Million USD)

#Predictors: 
#FGM : Field Goals Made
#...
#DRB : Defensive Rebounds 



#Clearly seen from the boxplot, observations with salaries larger than 13 were outliers.
#To make the model more accurate, the outliers and the observations with missing data
#were removed.

boxplot(salaryo$SALARY,ylab='Salary')
salary = read.csv('NBA_2015_nooutliers.csv',header=T)
salary = na.omit(salary)
colnames(salary)[21] = 'SALARY'
colnames(salary)[12] = 'AGE'




#SALARY ~ FGM+FGA+FG_+FTM+FTA+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB+TRB

#Initially selected 19 predictors

#then scatter plot was used to check linear relationship between each predictor.

#Among the those plots, 6 plots were showing the most strong linear relationships.

#Therefore, one of each pairs of variables in the plots were removed:
# 5 predictors were removed: FGA, FGM, FTM, FTA, TRB

par(mfrow = c(2, 3))
plot(salary$DRB, salary$TRB, xlab='Offensive Rebounds', ylab='Total Rebounds',main='Total Rebounds vs. Defensive Rebounds',cex.main=1.5)
plot(salary$DRB, salary$TRB, xlab='Defensive Rebounds', ylab='Total Rebounds',main='Total Rebounds vs. Defensive Rebounds',cex.main=1.5)
plot(salary$POINTS, salary$FTA,xlab='Points', ylab='Free Throw Attempts',main='Free Throws Attempted vs. Points',cex.main=1.5)
plot(salary$POINTS, salary$FTM,xlab='Points', ylab='Free Throws Made',main='Free Throws Made vs. Points',cex.main=1.5)
plot(salary$POINTS, salary$FGA ,xlab='Points', ylab='Field Goals Attempted',main='Field Goals Attempted vs. Points',cex.main=1.5)
plot(salary$POINTS, salary$FGM ,xlab='Points', ylab='Field Goals Made',main='Field Goals Made vs. Points',cex.main=1.5)


#Scatter plots of The response vs predictors:
#All plots show linear relationship,
#therefore the response variable is a linear function of predictors (1)

par(mfrow = c(2,3),oma=c(0,0,0,0))
plot(salary$AST, salary$SALARY,xlab='Assists', ylab='Salary',main='Salary vs. Assists',cex.main=1.5)
plot(salary$STL, salary$SALARY,xlab='Steals', ylab='Salary',main='Salary vs. Steals',cex.main=1.5)
plot(salary$POINTS, salary$SALARY,xlab='Points', ylab='Salary',main='Salary vs. Points',cex.main=1.5)
plot(salary$MPG, salary$SALARY,xlab='Minutes Played', ylab='Salary',main='Salary vs. Minutes Played',cex.main=1.5)
plot(salary$ORB, salary$SALARY,xlab='Offensive Rebounds', ylab='Salary',main='Salary vs. Offensive Rebounds',cex.main=1.5)
plot(salary$DRB, salary$SALARY,xlab='Defensive Rebounds', ylab='Salary',main='Salary vs. Defensive Rebounds',cex.main=1.5)
par(mfrow = c(2,4))
plot(salary$BLK, salary$SALARY,xlim=c(0,60),xlab='Blocks', ylab='Salary',main='Salary vs. Blocks',cex.main=1.5)
plot(salary$AGE, salary$SALARY,xlab='Age', ylab='Salary',main='Salary vs. Age',cex.main=1.5)
plot(salary$FT_, salary$SALARY,xlim=c(40,90),xlab='Free Throw Percentage', ylab='Salary',main='Salary vs. Free Throw Percentage',cex.main=1.5)
plot(salary$TOV, salary$SALARY,xlab='Turnovers', ylab='Salary',main='Salary vs. Turnovers',cex.main=1.5)
plot(salary$PF, salary$SALARY,xlab='Personal Fouls', ylab='Salary',main='Salary vs. Personal Fouls',cex.main=1.5)
plot(salary$Height, salary$SALARY,xlab='Height', ylab='Salary',main='Salary vs. Height',cex.main=1.5)
plot(salary$GP, salary$SALARY,xlab='Games Played', ylab='Salary',main='Salary vs. Games Played',cex.main=1.5)
plot(salary$FG_, salary$SALARY,xlim=c(30,60),xlab='Field Goal Percentage', ylab='Salary',main='Salary vs. Field Goal Percentage',cex.main=1.5)



# Then,
# 4 interaction terms of my interest were added to make the Full Model: 
# TOV:AST, TOV:POINTS, DRB:ORB FG_:POINTS

#hence, full model has 18 predictors including interaction terms:

#Full Model:
#SALARY ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB+
TOV:AST+TOV:POINTS+ DRB:ORB +FG_:POINTS



# Residual vs Fitted plot shows a funnel shape, which means that residual variance is increasing with fitted value
# Normal QQ plot shows large departure from the straight line at the topright tail,
#And Scale-Location plot shows a non-linear trend,
#while Residuals vs Leverage plot does not any influential cases.
par(mfrow = c(2, 2))
mf = lm(SALARY ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
        +TOV:AST+ TOV:POINTS+ DRB:ORB +FG_:POINTS,data=salary)
plot(mf)

#In order to improve the model, the sqrt transformation was done on the response: sqrt(SALARY)

#The Residual vs Fitted plot now shows a linear trend, and
# the residuals are randomly spreaded out from the center line,
#which shows that residual variance is constant. (2)
#Also, Normal Q-Q plot fairly fits the straight line,
#and Scale-Location plot also shows a linear trend, 
#while Residual vs Leverage plot does not shows any influential cases.

sqrtsal = sqrt(salary$SALARY)
sqrtlm = lm(sqrtsal ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
            +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS,data=salary)
plot(sqrtlm)

#Clearly, the tranformation improved the model and
#the plot shows that the residuals follow approximately normal distributtion and have a zero mean. (3) (4)
par(mfrow=c(1,2))
yhat = mf$fitted.values
rhat = rstudent(mf) # studentized residuals
d <- density(rhat) # returns the density data 
plot(d, main='Before Transformation',ylim=c(0,0.48),lwd=2) # plots the results
xvec = seq(-4,4,length.out = 801)
yvec = dnorm(xvec)
lines(xvec,yvec,col="red",lwd=3)
yhat = sqrtlm$fitted.values
rhat = rstudent(sqrtlm) # studentized residuals
d <- density(rhat) # returns the density data 
plot(d,main='After Square-Root Tranformation',ylim=c(0,0.48),lwd=2) # plots the results
xvec = seq(-4,4,length.out = 801)
yvec = dnorm(xvec)
lines(xvec,yvec,col="red",lwd=3)



#Model Selection!

# Forward Selection for AIC
m0 = lm(sqrtsal ~ 1, salary)
m1 = lm(sqrtsal ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
        +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS
        ,data=salary)
forModel = step(m0,scope=list(lower=~1, 
                              upper=  ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                              +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS
), direction="forward",data=salary)
backModel = step(m1,scope=list(lower=~1, 
                               upper= ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                               +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS),
                 direction="backward",data=salary)
AICstepfor = step(m0,scope=list(lower=~1, 
                                upper=  ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                                +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS
), direction="both",data=salary)
AICstepback = step(m1,scope=list(lower=~1, 
                                 upper= ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                                 +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS),
                   direction="both",data=salary)

#AIC best model is:
#AIC = -378.72
#sqrtsal ~ POINTS + AGE + Height + AST + MPG + STL

# Forward and back Selection for BIC
n=nrow(salary)
forModel = step(m0,scope=list(lower=~1, 
                              upper=  ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                              +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS
), direction="forward",k=log(n),data=salary)
backModel = step(m1,scope=list(lower=~1, 
                               upper= ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                               +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS),
                 direction="backward",k=log(n),data=salary)
AICstepfor = step(m0,scope=list(lower=~1, 
                                upper=  ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                                +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS
), direction="both",k=log(n),data=salary)
AICstepback = step(m1,scope=list(lower=~1, 
                                 upper= ~ FG_+FT_+AST+STL+BLK+TOV+PF+AGE+Height+GP+MPG+POINTS+ORB+DRB
                                 +TOV:AST+ TOV:POINTS+ DRB:ORB+ FG_:POINTS),
                   direction="both",k=log(n),data=salary)

#BIC best model is:
#BIC=-351.3
#sqrtsal ~ AST + AGE + Height + MPG + POINTS

#ANOVA on AIC best model and BIC best model is:
AICmodel = lm(sqrtsal ~ POINTS + AGE + Height + AST + MPG + STL,data=salary)
summary(AICmodel)
BICmodel = lm(sqrtsal ~ AST + AGE + Height + MPG + POINTS,data=salary)
summary(BICmodel)
anova(BICmodel, AICmodel)

#AIC model is chosen as a final model, because the p-value of F-stat
#in ANOVA table suggested that AIC model is significantly better than BIC model

#Hence, after the model selection, and ANOVA test, the final model was:
#sqrtsal ~ POINTS + AGE + Height + AST + MPG + STL


#Final Model summary 
par(mfrow=c(2,2))
after_ms = lm(sqrtsal ~ POINTS + AGE + Height + AST + MPG + STL,data=salary)
summary(after_ms)
plot(after_ms)
round(vif(after_ms),1)

#All predictors have The Variance Inflation Factor smaller than 5
#therefore there is no multi-colinearity among the predictors (5)
#The p-values of coefficients of all predictors and the F-statistics are very small < 0.01, < 2.2e-16
#therefore, all estimated coefficients and the final model are significant.




#Conclusion
#The six most important factors that affect the NBA players' salary are:
#Age
#Minutes Played
#Height
#Assists
#Points
#Assists
#Steals


#Suggestions
#There exist uncertain factors that affect players' salaries:

#Situation of Free Agent Market
#Injuries 
#Timing of the contracts
#Work ethic

#In the future ,
#I would like to find a way to quantify these uncertainties
#to build a better model.

#=============================================================================================


