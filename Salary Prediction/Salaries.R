library(dplyr)
library(ggcorrplot)
library(car)

setwd("/Users/yahia/Desktop/Files")
sal = read.csv("Salaries.csv")
dim(sal); head(sal)
sum(is.na(sal))

#Updating to Column names to be understandable
new_names <- c("Ln_Salary","Experience","Education","Bonus","Supervised", 
        "Assets","Board_Member","Age","Profits","International","Sales")

for(i in 2:length(colnames(sal))){
  colnames(sal)[i] <- new_names[i-1]
}

#Adding levels to each categorical variable and viewing
sal$Bonus <- factor(sal$Bonus)
sal$Board_Member <- factor(sal$Board_Member)
sal$International <- factor(sal$International)
levels(sal$Bonus); levels(sal$Board_Member); levels(sal$Internationa)

#Counting categorical occurrences 
length(which(sal$Board_Member== 1));length(which(sal$Board_Member== 0))
length(which(sal$International== 1));length(which(sal$International== 0))
length(which(sal$Board_Member== 1));length(which(sal$Board_Member== 0))

#Correlation ignoring Qualitative variables 
numeric_data <- sal[sapply(sal, is.numeric)]
correlation <- cor(numeric_data)
ggcorrplot(correlation,hc.order = TRUE,type = "lower",lab = TRUE)

#Box plot Ln_Salary vs Qualitative variables 
ip=par(mfrow=c(1,3))
boxplot(sal$Ln_Salary~sal$Bonus,xlab = "Bonus Eligibility",ylab = "Log(Salary)",col = "blue")
boxplot(sal$Ln_Salary~sal$International,xlab = "International Responsibility",ylab = "Log(Salary)",col = "blue")
boxplot(sal$Ln_Salary~sal$Board_Member, xlab = "Board Member",ylab = "Log(Salary)",col = "blue")
par(ip)

#Building models
#Full model
m0 <- lm(Ln_Salary~., data = sal[,-1])
summary(m0)

#Null model
m1<-lm(Ln_Salary~1, data = sal[,-1])
summary(m1)

#Model Selection
#Backward
m.backward<-step(m0,direction = "backward")

#Forward
m.foward <- step(m1,scope=formula(m0), direction = "forward")

#Step wise 
m.step <- step(m1,scope=formula(m0))

#Model Summaries
summary(m.backward)
summary(m.backward)
summary(m.step)

#All selection processes yielded the same model with same summary statistics. 
fit_model <- m.step
summary(fit_model)

#Subset data based on model coefficients
sub_sal <- select(sal,Ln_Salary, Experience, Bonus,Supervised,Education,Assets)
sub_sal$Ln_Salary <- exp(sub_sal$Ln_Salary) #Undo natural log for better visualization 
colnames(sub_sal)[1] <- "Salaries"
sub_sal

#Pairwise scatter plot
pairs(sub_sal, main = "Pairwise Scatterplots of Salaries(non ln) based on Variables")

#Model assumptions 
op=par(mfrow=c(2,2))
plot(fit_model, which = 1) #Residuals vs Fitted (Linearity)
plot(fit_model, which = 2) #Normal Q-Q Plot (Normality)
plot(fit_model, which = 3) #Scale-Location Plot (Homoscedasticity)
plot(fit_model, which = 5) #Residuals vs Leverage (Influential Points)
par(op)

which.max(cooks.distance(fit_model))  # Index of most influential point
sal[which.max(cooks.distance(fit_model)), ]  # Viewing observation

vif_values <- vif(fit_model) #Detecting multicollinearity
par(mfrow = c(1,1)); barplot(vif_values, col = "blue", main = "Variance Inflation Factor (VIF)")
