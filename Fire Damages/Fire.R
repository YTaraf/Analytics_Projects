library(ggcorrplot)
library(dplyr)
setwd("/Users/yahia/Desktop/Files")
fire = read.csv("Fires.csv")

#Exploratory Data Analysis 
head(fire); dim(fire); names(fire)

# Check for missing values
sum(is.na(fire))

# Summary statistics for DAMAGE and DISTANCE
mean(fire$DAMAGE); mean(fire$DISTANCE)
sd(fire$DAMAGE); sd(fire$DISTANCE)

# Sort data by DISTANCE and DAMAGE for a quick look at distribution
fire %>% arrange(DISTANCE)
fire %>% arrange(DAMAGE)

# Visualize correlation matrix
fire_correlation <- cor(fire)
ggcorrplot(fire_correlation, hc.order = TRUE, lab = TRUE)

# Histogram to explore the distribution of fire damage and distance
lp=par(mfrow=c(1,2))
hist(fire$DAMAGE, col="red", xlab="Damage in thousands", main="Frequency of Damages", breaks=10)
hist(fire$DISTANCE, col="red", xlab="Distance in miles", main="Frequency of Distance", breaks=10)
par(lp)

#Simple Linear Regression Model
#Predict amount of Damages(thousands) in major residential fires to the Distance(miles) between the burning house and nearest fire station

fire_lm <- lm(DAMAGE~DISTANCE, data = fire)
summary(fire_lm)

op=par(mfrow=c(2,2))

#Scatter Plot for linearity
plot(DAMAGE~DISTANCE,data=fire, xlab = "Distance in miles from fire station", ylab = "Damage in thousands",main = "Damage against Distance(Linearity)")
abline(fire_lm, col = "red")

#Residual Plot for equal variance
plot(fire_lm$residuals~fire_lm$fitted.values,xlab="Predicted values",ylab="Residuals", main = "Residuals against Predicited(Equal Variance)") 
abline(h=0,col="red")

#Independence plot
plot(fire_lm$residuals,type="l",xlab="Index",ylab="Residuals", main = "Residuals against Index(Independence)")
abline(h=0,col = "red")

#Normal population plot
qqnorm(fire_lm$residuals,main="Normal probability plot of residuals(Normaility)")
qqline(fire_lm$residuals, col = "red")

par(op)

# Check for influential points using Cook's Distance
plot(cooks.distance(fire_lm), main = "Cook's Distance (Original Model)")
which.max(cooks.distance(fire_lm))  # Index of most influential point
fire[which.max(cooks.distance(fire_lm)), ]  # Viewing observation


# Fit a new model excluding the most influential point (row 13)
fire_reduced <- fire %>% slice(-13)

fire_reduced_lm <- lm(DAMAGE~DISTANCE, data = fire_reduced)
summary(fire_reduced_lm)

ip=par(mfrow=c(2,2))

#Scatter Plot for linearity
plot(DAMAGE~DISTANCE,data=fire_reduced, xlab = "Distance in miles from fire station", ylab = "Damage in thousands",main = "Damage against Distance(Linearity)")
abline(fire_lm, col = "blue")

#Residual Plot for equal variance
plot(fire_reduced_lm$residuals~fire_reduced_lm$fitted.values,xlab="Predicted values",ylab="Residuals", main = "Residuals against Predicited(Equal Variance)") 
abline(h=0,col="blue")

#Independence plot
plot(fire_reduced_lm$residuals,type="l",xlab="Index",ylab="Residuals", main = "Residuals against Index(Independence)")
abline(h=0,,col = "blue")

#Normal population plot
qqnorm(fire_reduced_lm$residuals,main="Normal probability plot of residuals(Normaility)")
qqline(fire_reduced_lm$residuals,col = "blue")

par(ip)

#Checking for outlier 
plot(cooks.distance(fire_reduced_lm))
which.max(cooks.distance(fire_reduced_lm))  
fire_reduced[which.max(cooks.distance(fire_reduced_lm)), ]

# Check for influential points using Cook's Distance
plot(cooks.distance(fire_reduced_lm), main = "Cook's Distance (Reduced Model)")
which.max(cooks.distance(fire_lm))  # Index of most influential point
fire[which.max(cooks.distance(fire_lm)), ]  # Viewing observation

# Visual comparison of both regression lines
plot(DAMAGE~DISTANCE,data=fire, xlab = "Distance in miles from fire station", ylab = "Damage in thousands",main = "Regression Lines With and Without Point 13")
abline(fire_lm, col = "red",lwd = 1.5)              # Full model
abline(fire_reduced_lm, col = "blue", lty = 2, lwd = 2)  # Without point 13
legend("topleft",legend = c("Original Model", "Without Row 13"),col = c("red", "blue"),lty = c(1, 2))

# Summary: Removing the 13th observation slightly lowered R² but improved residual fit and reduced influence from an outlier.