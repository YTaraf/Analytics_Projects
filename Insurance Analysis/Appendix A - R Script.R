library(ggplot2)
library(ggcorrplot) 
library(dplyr)
library(dlookr)
library(tidyverse)
library(maps)
library(patchwork)
library(car)
library(randomForest)

#Opening and Read csv from directory 
raw_data <- read.csv("insurance.csv") #Ensure to specific path
df <- raw_data
#Data Prep and Cleaning 
str(df)
head(df)
diagnose(df)

#Bullion values of each sex and smoker for numeric anaylsis 
df <- df %>% mutate(is_male = if_else(sex == "male", 1, 0))
df <- df %>% mutate(is_female = if_else(sex == "female", 1, 0))
df <- df %>% mutate(is_smoker = if_else(smoker == "yes", 1, 0))
#drop mutated values 
drops <- c("smoker","sex")
df<-df[ , !(names(df) %in% drops)]

#Split into 4 different sets based on region
regions <- unique(df$region)
region_dfs <- split(df, df$region)
southwest_df <-region_dfs$southwest
southeast_df <-region_dfs$southeast
northeast_df <-region_dfs$northeast
northwest_df <-region_dfs$northwest
total_rows <- sum(nrow(southwest_df),nrow(southeast_df),nrow(northeast_df),nrow(northwest_df))

#Ensuring no data lost 
#Col count
ncol(df) == ncol(southwest_df)
ncol(df) == ncol(southeast_df)
ncol(df) == ncol(northeast_df)
ncol(df) == ncol(northwest_df)
#Row count
nrow(df) == total_rows

# Create a list of data frames
region_dfs <- list(southwest = southwest_df, southeast = southeast_df, northeast = northeast_df, northwest = northwest_df)

# Function to drop the region column
region_drop <- function(df) {
  df %>% select(-region) # Return the modified data frame
}
region_dfs <- lapply(region_dfs, region_drop)

#Mean of non-smoker charges
non_smoker_mean_charges <- lapply(region_dfs, function(df) {
  mean(df$charges[df$is_smoker == 0], na.rm = TRUE) # Calculate mean ignoring NA values
})
non_smoker_mean_charges
#Mean of smoker charges
smoker_mean_charges <- lapply(region_dfs, function(df) {
  mean(df$charges[df$is_smoker == 1], na.rm = TRUE) # Calculate mean ignoring NA values
})
smoker_mean_charges

#Smoker df with region
smoker_region_data <- data.frame(
  region = c("southwest", "southeast", "northeast", "northwest"),
  mean_charges = unlist(smoker_mean_charges))
#Non Smoker df with region
non_smoker_region_data <- data.frame(
  region = c("southwest", "southeast", "northeast", "northwest"),
  mean_charges = unlist(non_smoker_mean_charges))

# Assign states to regions
region_states <- data.frame(
  state = tolower(c(
    "Arizona", "New Mexico", "Texas", "Oklahoma", # Southwest
    "Florida", "Georgia", "Alabama", "South Carolina", "Tennessee", # Southeast
    "New York", "Massachusetts", "Pennsylvania", "New Jersey", "Connecticut", # Northeast
    "Washington", "Oregon", "Idaho", "Montana", "Wyoming" # Northwest
  )),
  region = rep(c("southwest", "southeast", "northeast", "northwest"), 
               times = c(4, 5, 5, 5))
)

# Merge with region data
smoker_region_states <- region_states %>%
  left_join(smoker_region_data, by = "region")
# Merge with region data
non_smoker_region_states <- region_states %>%
  left_join(non_smoker_region_data, by = "region")
# Get U.S. state map data
us_map <- map_data("state")
# Merge map data with region_states
non_smoker_map_data <- us_map %>%
  left_join(non_smoker_region_states, by = c("region" = "state"))
# Merge map data with region_states
smoker_map_data <- us_map %>%
  left_join(smoker_region_states, by = c("region" = "state"))

# Plot heatmap for non_smoker
non_smoker_plot <- ggplot(non_smoker_map_data, aes(long, lat, group = group, fill = mean_charges)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Average Charges by Non-Smokers",
    fill = "Mean Charges"
  ) +
  coord_fixed(1.3)
# Plot heatmap for smoker
smoker_plot <- ggplot(smoker_map_data, aes(long, lat, group = group, fill = mean_charges)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "orange", high = "darkred", na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Average Charges For Smokers",
    fill = "Mean Charges"
  ) +
  coord_fixed(1.3)

# Combine the two plots side by side
non_smoker_plot + smoker_plot

#Correlation info
corr <- cor(subset(df, select = -c(region)))
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

# Fit a linear regression model (mod1) with charges as the dependent variable
#and age, bmi, and children as the independent variables.
mod1 = lm(charges ~ age + bmi + children, data= df)
par(mfrow=c(2,2))
# Generate diagnostic plots for mod1 
plot(mod1)

# Fit a second linear regression model (mod2) with additional predictors: is_male and is_smoker.
mod2 = lm(charges ~ age + bmi + children + is_male + is_smoker, data= df)
par(mfrow=c(2,2))

# Generate diagnostic plots for mod2 
plot(mod2)

#Summary of mod2,coefficients, p-values, R-squared
summary(mod2)

#Check for multicollinearity among predictors using Variance Inflation Factor
vif(mod2)

#Compare the two models using ANOVA test to determine if the additional predictors 
#in mod2 significantly improve the model fit.
anova(mod1, mod2)
set.seed(123)

# Generate a random sample of row indices for the training set (80% of the data).
train_index <- sample(1:nrow(df), size = 0.8 * nrow(df))
train = df[train_index, ]
test = df[-train_index, ]

# Fit a Random Forest model to predict charges using all predictors in the training dataset.
# The model also calculates proximity measures between data points.
rf_mod = randomForest(charges ~ age + bmi + children + is_male + is_smoker, data= train, proximity = TRUE)

#Generate predictions for the test dataset using the trained Random Forest model.
predictions = predict(rf_mod, newdata=test)
