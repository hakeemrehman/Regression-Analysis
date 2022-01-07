'Simple Regression Analysis'

# Album1 data
'-------------'
# Access the album1 data
album1<-read.delim("Album Sales 1.dat", header = TRUE)
View(album1)

# Fit the simple linear regression model
'---------------------------------------'
options(scipen=999)
albumSales.1 <- lm(sales ~ adverts, data = album1)
summary(albumSales.1)

# Model Parameters - Confidence Intervals 
confint(albumSales.1) # By Default at 95%
confint(albumSales.1, level = 0.99)

# Testing the Accuracy of the Regression Model
'----------------------------------------------'
'1) Obtain casewise diagnostics: Regression Deletion Diagnostics '
'Cook distance is a measure of how much influence a independent variable has on
the predicted value of the outcome (dependent) variable'
# Specifically, 
'it refers to how far an average predicted (Y-Values) will change; if the 
  particular recode(case) is deleted from the data set'
plot(albumSales.1, pch=18, col="red", which = c(4))

album1$standardized.residuals <- rstandard(albumSales.1)
album1$cooks.distance<-cooks.distance(albumSales.1)
album1$leverage <- hatvalues(albumSales.1)
album1$covariance.ratios <- covratio(albumSales.1)

# Save file
write.table(album1, "Album Sales-1 With Diagnostics.dat", sep = "\t", row.names = FALSE)
# Look at the data (and round the values)
round(album1, digits = 3)

# List of standardized residuals greater than 2
album1$standardized.residuals>2| album1$standardized.residuals < -2
# Create a variable called large.residual, which is TRUE (or 1) if the residual is >2, or <-2
album1$large.residual <- album1$standardized.residuals > 2 | album1$standardized.residuals < -2
# Count the number of large residuals
sum(album1$large.residual)

# Display the value of sales, airplay, attract, adverts, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------
album1[album1$large.residual,c("sales", "adverts", "standardized.residuals")]

# Cook's distance, leverage and covariance ratio for cases with large residuals
album1[album1$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]

'2) Assessing the assumption of independence of errors'
#---------------------------------------------------
# The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()
library(car)
durbinWatsonTest(albumSales.1)
dwt(albumSales.1)

'3) Checking assumptions about the residuals (Normality & Homogeneousness Variances)'
#--------------------------------------------------------------------------------
plot(albumSales.1, pch=18, col="red", which = c(1)) # Residuals Vs Fitted
plot(albumSales.1, pch=18, col="red", which = c(2)) # Normal Q-Q Plot
plot(albumSales.1, pch=18, col="red", which = c(3)) # Scale-Location Plot

'Scatter Plot & Simple Regression Analysis'
# Exam Anxiety data
'-------------------'
# Read/Load the data 
examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
View(examData)

# Simple Linear Regression
examData_lm<-lm(Anxiety ~ Exam, data = examData)
summary(examData_lm)

library(ggplot2)
#Simple scatter
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") 

#Simple scatter with regression line
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se=F) + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 

#Simple scatter with regression line + CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Red") + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 

# Simple scatter with regression line + coloured CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 

#Grouped scatter with regression line + CI
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + 
    geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + 
    labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 

