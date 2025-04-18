# Leonard Okhani
# LSE Data Analytics Career Accelerator
# DA301: Advanced Analytics for Organisational Impact

##### Scenario #####

# You are a data analyst working for Turtle Games, 
# a game manufacturer and retailer. 
# They manufacture and sell their own products, 
# along with sourcing and selling products manufactured by other companies. 
# Their product range includes books, board games, video games and toys. 
# They have a global customer base and have a business objective of improving 
# overall sales performance by utilising customer trends. In particular, 
# Turtle Games wants to understand: 
  # how customers accumulate loyalty points (Week 1)
  # exploring the structure using decision trees (Week 2)
  # exploring clusters in customer behaviour (Week 3)
  # can social data (e.g. customer reviews) be used in marketing campaigns 
  # (Week 4)
  # loading, transforming and visualising data in R (Week 5)
  # statistical analysis and modelling in R (Week 6)

################################################################################

##### Week 5: Cleaning and wrangling data and performing EDA in R #####

##### Scenario 5 #####
# Turtle Games’s sales department has historically preferred to use R when 
# performing sales analyses due to its existing workflow systems. 
# As you’re able to perform data analysis in R, 
# you will perform exploratory data analysis and present your findings by 
# utilising basic statistics and plots. 
# You'll explore and prepare the data set to analyse sales per product. 
# The sales department is hoping to use the findings of this exploratory 
# analysis to inform changes and improvements in the team. 
# (Note that you will use basic summary statistics in Module 5 and dive into 
# more detail with descriptive statistics in Module 6.)

##### Objective 5 #####
# Load and wrangle the data. 
# Use summary statistics and groupings if required to sense-check and gain 
# insights into the data. 
# Use different visualisations such as scatterplots, histograms, boxplots and 
# barplots to learn more about the data set. 
# Explore the data and comment on the insights you gain from your exploratory 
# data analysis; 
# for example, outliers, missing values, and distribution of data. 
# Also comment on initial patterns and distributions or behaviour that may be of 
# interest to the business.

# Load the necessary libraries.
library(tidyverse)
library(skimr)
library(DataExplorer)
library(moments)

# Import the dataset.
sales <- read.csv('turtle_reviews_clean.csv', header = T)

# Viewing the data.
head(sales)
str(sales)

# Determining the number of missing values.
colSums(is.na(sales))

##### SCATTER PLOTS #####

# Specify the ggplot function. 
ggplot(sales, mapping=aes(x=age, y=loyalty_points)) +
  geom_point(color = 'red', alpha = 0.7, size = 2.5) +
  labs(x = "Age (Years)", y = "Loyalty Points", 
       title = "Age vs Loyalty Points (Education)") +
  facet_wrap(~education) 

# Specify the ggplot function. 
ggplot(sales, mapping=aes(x=age, y=loyalty_points)) +
  geom_point(color = 'black', alpha = 0.7, size = 2.5) +
  labs(x = "Age (Years)", y = "Loyalty Points", 
       title = "Age vs Loyalty Points (Gender)") +
 facet_wrap(~gender)  

# Specify the ggplot function. 
ggplot(sales, mapping=aes(x=remuneration, y=loyalty_points)) +
  geom_point(color = 'blue', alpha = 0.7, size = 2.5) +
  labs(x = "Remuneration (k£)", y = "Loyalty Points", 
       title = "Remuneration vs Loyalty Points (Education)") +
  facet_wrap(~education)    

# Specify the ggplot function.   
ggplot(sales, mapping=aes(x=remuneration, y=loyalty_points)) +
  geom_point(color = 'green', alpha = 0.7, size = 2.5) +
  labs(x = "Remuneration (k£)", y = "Loyalty Points", 
       title = "Remuneration vs Loyalty Points (Gender)") +
  facet_wrap(~gender)  

# Specify the ggplot function.   
ggplot(sales, mapping=aes(x=spending_score, y=loyalty_points)) +
  geom_point(color = 'yellow', alpha = 0.7, size = 2.5) +
  labs(x = "Spending Score", y = "Loyalty Points", 
       title = "Spending Score vs Loyalty Points (Education)") +
  facet_wrap(~education)    

# Specify the ggplot function.   
ggplot(sales, mapping=aes(x=spending_score, y=loyalty_points)) +
  geom_point(color = 'pink', alpha = 0.7, size = 2.5) +
  labs(x = "Spending Score", y = "Loyalty Points", 
       title = "Spending Score vs Loyalty Points (Gender)") +
  facet_wrap(~gender)  

# Insights
# There was no clear relationship between age and loyalty points. 
# The loyalty points were randomly distributed across all ages.
# There was a weak but positive relationship between remuneration and loyalty 
# points. Higher remuneration often lead to higher loyalty points, 
# particularly for the higher education levels. 
# There were however customers with low earnings but high points.
# There was a much stronger positive relationship between spending score and 
# loyalty points. This was apparent in all the education levels.
# There wasn't much difference in terms of gender - 
# males and females showed to have similar distribution patterns across all the 
# variables.
# The higher education levels (graduate, postgraduate, PhD) showed much stronger
# linear patterns, particularly in the remuneration vs loyalty points plot.

##### HISTOGRAMS #####

# Specify the ggplot function. 
ggplot(sales, aes(x = age)) +
  # Add fill, colour, and a statistic.
  geom_histogram(fill = 'red', color = 'black', binwidth = 10) + 
  # Add the labs function for labels.
  labs(x = "Age (Years)", y = "No. of customers", title = "Age Distribution")

# Specify the ggplot function. 
ggplot(sales, aes(x = remuneration)) +
  # Add fill, colour, and a statistic.
  geom_histogram(fill = 'green', color = 'black', binwidth = 10) + 
  # Add the labs function for labels.
  labs(x = "Remuneration (k£)", y = "No. of customers", 
       title = "Remuneration Distribution")

# Specify the ggplot function. 
ggplot(sales, aes(x = spending_score)) +
  # Add fill, colour, and a statistic.
  geom_histogram(fill = 'blue', color = 'black', binwidth = 10) + 
  # Add the labs function for labels.
  labs(x = "Spending Score", y = "No. of customers", 
       title = "Spending Score Distribution")

# Insights
# Age Distribution - Most of the customers were aged 25-45.
# Remuneration Distribution - Most of the customers earned between 
# £15,000-£65,000.
# Spending Score Distribution - There was a cluster around the mid spending 
# scores.

##### BOXPLOTS #####

# Specify the ggplot function.
ggplot(sales, aes(y = age)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = "red", color = "black") +
  # Specify the titles.
  labs(title = "Age Boxplot", y = "Age (Years)")

# Specify the ggplot function.
ggplot(sales, aes(x = age, y = loyalty_points)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'brown', notch = TRUE, outlier.color = 'black') + 
  # Specify the titles.
  labs(title = "Age by Loyalty Points", x = "Age (Years)", y = "Loyalty Points")

# Specify the ggplot function.
ggplot(sales, aes(y = remuneration)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = "blue", color = "black") +
  # Specify the titles.
  labs(title = "Remuneration Boxplot", y = "Remuneration (k£)")

# Specify the ggplot function.
ggplot(sales, aes(x = remuneration, y = loyalty_points)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'navy', notch = TRUE, outlier.color = 'black') + 
  # Specify the titles.
  labs(title = "Remuneration by Loyalty Points", x = "Remuneration (k£)", 
       y = "Loyalty Points")

# Specify the ggplot function.
ggplot(sales, aes(x = "",y = spending_score)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = "yellow", color = "black") +
  # Specify the titles.
  labs(title = "Spending Score Boxplot", y = "Spending Score")

# Specify the ggplot function.
ggplot(sales, aes(x = spending_score, y = loyalty_points)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'green', notch = TRUE, outlier.color = 'black') + 
  # Specify the titles.
  labs(title = "Spending Score by Loyalty Points", x = "Spending Score", 
       y = "Loyalty Points")

# Insights
# Compared to other visualisations, in the scatter plots, 
# loyalty points barely proved to have a relationship with either age, 
# remuneration or spending score.
# There were outliers detected within each independent variable.
# Age Outliers - 
# There were very young and old customers with high loyalty points. 
# For the younger customers, 
# they were probably using their parents' accounts and money. 
# As for the older customers, 
# it is possible they are retired with a very high level of income.
# Remuneration Outliers - 
# There were customers with high incomes but low loyalty points. 
# These are probably customers who prefer to save or not motivated by the 
# loyalty program.
# Spending Score Outliers - 
# There were customers who spent way above the average compared to the others. 
# This could be down to the company rewarding customers who spend more with VIP 
# treatment. 

##### BAR CHARTS #####

# Specify the ggplot function.
ggplot(sales, aes(x = education, fill = gender)) +
  # Specify the geom_bar function and add position.
  geom_bar(position = 'dodge') +
  # Add fill colours. 
  scale_fill_manual(values=c('pink', 'blue')) +
  # Specify the titles.
  labs(title = "Education vs Gender", x = "Education", y = "No. of customers")

sales %>%
  # Group by education level.
  group_by(education) %>%
  # Calculated the avergae of loyalty points for each group.
  summarise(loyalty_avg = mean(loyalty_points)) %>%
  # Specify the ggplot function.
  ggplot(aes(x=education, y=loyalty_avg)) +
  # Add fill colours.
  geom_col(fill="green") +
  # Specify the titles.
  labs(title="Average Loyalty Points by Education Level", x = "Education", 
       y = "Average Loyalty Points")

sales %>%
  # Group by gender.
  group_by(gender) %>%
  # Calculated the avergae of loyalty points for each group.
  summarise(loyalty_avg = mean(loyalty_points)) %>%
  # Specify the ggplot function.
  ggplot(aes(x=gender, y=loyalty_avg)) +
  # Add fill colours.
  geom_col(fill="yellow") +
  # Specify the titles.
  labs(title="Average Loyalty Points by Education Level", x = "Gender", 
       y = "Average Loyalty Points")

# Insights
# Almost half of the total customers had a bachelor's degree, 
# although many also had a PhD or master's.
# There were more female customers than male overall.
# Although there were more female customers than males, 
# more male customers had basic education and PhD.
# Although basic was the education level with the least number of customers, 
# it had the highest average of loyalty points out of all the education levels.
# Females had a higher average of loyalty points than males but not by a large 
# extent, even though females populated over 50% of the total customers.

################################################################################

##### Week 6: Making recommendations to the business #####

##### Scenario 6 #####

# In Module 5, 
# you revisited components of the analysis using Turtle Games’s preferred 
# language, R, 
# in order to make it easier for the sales team to implement your analysis 
# internally. As a final task, 
# the team asked you to perform a statistical analysis and create a multiple 
# linear regression model using R to predict the accumulation of customers’ 
# loyalty points using the available features in a multiple linear model. 
# They did not prescribe which features to use; therefore, 
# you can use insights from previous modules as well as your statistical 
# analysis to make recommendations regarding the suitability of this model type, 
# the specifics of the model you created, and alternative solutions. 
# As part of this final task, 
# they also requested your observations and recommendations regarding the 
# current loyalty programme and how it can be improved. 

##### Objective 6 #####

# You need to investigate customer behaviour and the effectiveness of the 
# current loyalty programme based on the work you completed in Modules 1–5, 
# as well as your statistical analysis and modelling efforts in Module 6. 
# Turtle Games has requested that you answer the following questions:
  # Can we predict the accumulation of loyalty points given the existing 
  # features using a relatively simple MLR model?
  # Do you have confidence in the model results (goodness of fit evaluation)? 
  # Where should the business focus its marketing efforts?
  # How could the loyalty programme be improved?
  # How could the analysis be improved?

##### Statistical Analysis #####

# View the descriptive statistics of the data.
summary(sales)
summary(sales$loyalty_points)

# Measuring the central tendencies of loyalty points with mean and median.
mean(sales$loyalty_points)
median(sales$loyalty_points)

# Calculating the maximum, minimum and range of loyalty points.
max(sales$loyalty_points)
min(sales$loyalty_points)
max(sales$loyalty_points) - min(sales$loyalty_points)

# Calculating the lower and higher quartiles of loyalty points.
quantile(sales$loyalty_points, 0.25)
quantile(sales$loyalty_points, 0.75)

# Calculate the IRQ, variance and standard deviation.
IQR(sales$loyalty_points)
var(sales$loyalty_points)
sd(sales$loyalty_points)

# To determine if the data is normally distributed, 
# a q-q plot was created to measure the normality in the loyalty points.
qqnorm(sales$loyalty_points)
# Adding a reference line.
qqline(sales$loyalty_points, col='red')

# Shapiro-Wilk test.
shapiro.test((sales$loyalty_points))

# Check for skewness and kurtosis for loyalty points.
skewness(sales$loyalty_points)
kurtosis(sales$loyalty_points)

# Checking normality in the independent variables.
shapiro.test(sales$age)
shapiro.test(sales$remuneration)
shapiro.test(sales$spending_score)

# Checking for correlation between loyalty points and the independent variables 
# with Pearson's correlation.
cor(sales$loyalty_points, sales$age)
cor(sales$loyalty_points, sales$remuneration)
cor(sales$loyalty_points, sales$spending_score)

# Insights

# With the W value below 1 and a very small p-value from the Shapiro-Wilk test, 
# this confirmed that the data was definitely not normally distributed.
# Skewness was above 0, which means the data had a positive skewness.
# Kurtosis was above 3, so the data had a leptokurtic distribution.
# Age, remuneration and spending score had also proven to not be normally 
# distributed through the Shapiro-Wilk test.
# Based on the correlation tests:
  # Age doesn't have a significant relationship with loyalty points.
  # Remuneration has a fairly positive relationship with loyalty points.
  # Spending Score has quite a strong positive relationship with loyalty points.

##### Multiple Linear Regression Models #####

# Creating multiple linear regression models.
# Create new objects.
# Specify the lm function and the variables.
# Keep loyalty points as the dependent variables.
# Change the combination of the independent variables with each model.
# Change the name of each model.
# View the summary of each model.

modela = lm(loyalty_points~age, data=sales)
summary(modela)

modelb = lm(loyalty_points~remuneration, data=sales)
summary(modelb)

modelc = lm(loyalty_points~spending_score, data=sales)
summary(modelc)

modeld = lm(loyalty_points~age+remuneration, data=sales)
summary(modeld)

modele = lm(loyalty_points~age+spending_score, data=sales)
summary(modele)

modelf = lm(loyalty_points~remuneration+spending_score, data=sales)
summary(modelf)

modelg = lm(loyalty_points~age+remuneration+spending_score, data=sales)
summary(modelg)

# Q1. Can we predict the accumulation of loyalty points given the existing 
#     features using a relatively simple MLR model?

# A1. Yes, we can. If you look at modelg, which contained all 3 variables, 
#     it led to a very high adjusted R-squared of 0.8397 - 
#     approximately 84% of the variannce of loyalty points. 
#     This shows that a relatively simple MLR model is effective for predictons.

# Q2. Do you have confidence in the model results (goodness of fit evaluation)?

# A2. Yes, I am confident with the model results. 
#     Like mentioned in the previous question, 
#     the fact that the final model covered over 80% of the loyalty points 
#     variance showed that it is a really good fit. In addition, 
#     there was a very small difference between the Multiple R-squared and 
#     Adjusted R-squared, which further compliments the fitting of the model. 

# Q3. Where should the business focus its marketing efforts?

# A3. Modelf and Modelg were the models with the highest R-squared values and 
#     they both contained remuneration and spending score as predictors. 
#     Therefore, the business should be targeting high spending customers, 
#     especially those who with high incomes. 
#     It would be good if the company created offers to reward the customers who 
#     spend a lot.

# Q4. How could the loyalty programme be improved?

# A4. A way to improve the loyalty programme is to send income-based rewards. 
#     For those who earn a lot, they should send luxury perks, 
#     similar to as mentioned in the previous question. 
#     Regarding those who earn less, 
#     discount offers so their low earnings don't affect their loyalty.

# Q5. How could the analysis be improved?

# A5. During the analysis, 
#     one discovery that was made was that the models with more than one 
#     variable were the ones with the higher R-squared values. Therefore, 
#     adding more variables (preferences of products, website visits frequency, 
#     etc), could improve the model and lead to more accurate results.

# Insights 

# Out of the seven models, modelg was the best.
# It's all worth noting that each model which included spending score had higher
# adjusted R-squared values - 
# showcasing how spending score is the best predictor for loyalty points.