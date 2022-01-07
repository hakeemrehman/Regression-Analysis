'Multiple Linear (Multivariate) Regression'
# -----------------------------------------

# Access the album2 data
album2<-read.delim("Album Sales 2.dat", header = TRUE)


# Run the multiple regression model
'lm function: lm(outcome ~ predictor(s))'
albumSales.2<-lm(sales ~ adverts, data = album2)
albumSales.3<-lm(sales ~ adverts + airplay + attract, data = album2)
summary(albumSales.2)
summary(albumSales.3)

# Confidence intervals for each Predictors
confint(albumSales.3)

# Standardized parameter estimates using lm.beta() function
library(QuantPsyc)
lm.beta(albumSales.3)
sd(album2$sales) # Standard deviation 
sd(album2$adverts)
sd(album2$airplay)

# For comparing the R-Square in two models, use the ANOVA command
anova(albumSales.2, albumSales.3)

# Obtain casewise diagnostics and add them to the original data file
album2$standardized.residuals <- rstandard(albumSales.3)
album2$cooks.distance<-cooks.distance(albumSales.3)
album2$leverage <- hatvalues(albumSales.3)
album2$covariance.ratios <- covratio(albumSales.3)

# Save file
write.table(album2, "Album Sales With Diagnostics.dat", sep = "\t", row.names = FALSE)
# Look at the data (and round the values)
round(album2, digits = 3)

'Testing the Accuracy of the Regression Model'
#---------------------------------------------

'Case wise Diagnostics: Outliers & influential cases'
#----------------------------------------------------
# List of standardized residuals greater than 2
album2$standardized.residuals>2| album2$standardized.residuals < -2

# Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2
album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2

# Count the number of large residuals
sum(album2$large.residual)

# Display the value of sales, airplay, attract, adverts, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------
album2[album2$large.residual,c("sales", "airplay", "attract", "adverts", "standardized.residuals")]

# Cook's distance, leverage and covariance ratio for cases with large residuals
album2[album2$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]

'Assessing the assumption of independence of errors'
#---------------------------------------------------
# The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()
library(car)
durbinWatsonTest(albumSales.3)
dwt(albumSales.3)

'Assessing the assumption of no multicollinearity'
#-------------------------------------------------
# Obtaining the VIF
vif(albumSales.3)

# The tolerance is 1/VIF
1/vif(albumSales.3)

# The mean VIF
mean(vif(albumSales.3))


'Checking assumptions about the residuals'
#---------------------------------------
plot(albumSales.3)

'Robust regression: Bootstrapping'
#--------------------------------
#---Write a bootstrap function.
bootReg<-function(formula, data, i)
{
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

# Bootstrapping our regression model, with 2000 replications
bootResults<-boot(statistic = bootReg, formula = sales ~ adverts + airplay + 
                    attract, data = album2, R = 2000,)

# We can then obtaine the bootstrap confidence intervals for the intercept
boot.ci(bootResults, type = "bca", index = 1)

# And the three slope estimates
boot.ci(bootResults, type = "bca", index = 2)
boot.ci(bootResults, type = "bca", index = 3)
boot.ci(bootResults, type = "bca", index = 4)

# Regression Analysis With Dummy Variables
'-----------------------------------------'
# View the Built-in datasets 
data()

library(carData)

# Required data set
View(Salaries)

# Inspect the data set
head(Salaries)

'Categorical variables with two levels'
#--------------------------------------
# Returns the coding that R have used to create the dummy variables
contrasts(Salaries$sex)

# Compute the model
model <- lm(salary ~ sex, data = Salaries)
summary(model)
summary(model)$coef


'Categorical variables with more than two levels'
#------------------------------------------------
# Returns the coding that R have used to create the dummy variables
res <- model.matrix(~rank, data = Salaries)
head(res)
head(res[, -1])

# Compute the model
library(car)
model2 <- lm(salary ~ yrs.service + rank + discipline + sex, data = Salaries)
Anova(model2)

#To get the contrasts of the categorical variable
summary(model2)


