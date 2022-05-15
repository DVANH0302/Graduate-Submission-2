setwd("C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project")





### Understanding and EDA 
install.packages("psych", dependencies = TRUE)
library(tidyverse)
library(psych)

# Stat for the data attribute 
describe(grad)

# Check for na values 


# Frequency distribution of Chance of Admit (response variable)
ggplot(grad, aes(x=Chance.of.Admit)) + 
  geom_histogram(aes(y=..density..),      
                 bins = 20,
                 colour="black", fill="skyblue") +
  geom_density(alpha=.2, fill="#FF6666") + 
  ggtitle('Frequency distribution of Chance of Admit')



## Step 1: Read the data and its variables 
grad = read.csv('Admission_Predict.csv')

# Remove the first column
grad = grad[, -1]

# Attach data 
attach(grad)

## Step 2:  Compute the correlation matrix 
cor(grad)

pairs(grad)

## Step 3:  Regression output - Identify the model 
model_1  = lm(formula = Chance.of.Admit ~  GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research)
summary(model_1)
    # Criteria:  p value < 0.05
      # University.Rating and SOP have p-value > 0.05 
    # Criteria r-square > 0.6 (Satisfied)

## Step 4 Regression Output 
anova(model_1)
    # Criteria: p value <0.05

## Step 5 Modified Regression Output - Identify the model 
model_2 = lm(formula = Chance.of.Admit ~ GRE.Score + TOEFL.Score + LOR + CGPA + Research)

## Step 6 Regression output 
summary(model_2)
anova(model_2)

## Step 7 Residual Analysis 

pred = fitted(model_2)
Res = residuals(model_2)
plot(Res)
abline(0,0)
qqnorm(Res)
qqline(Res)
write.csv(pred,"C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project\\pred_m.csv") 
write.csv(Res,"C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project\\Res_m.csv")

## Step 8 Standardizing Residuals using Scale function 
Std_Res = scale(Res, center = T, scale = T)
write.csv(Std_Res,"C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project\\Std_Res_m.csv")

## Step 9 Normality Check on residuals 
qqnorm(Std_Res)
qqline(Std_Res)

## Step 10 Normality test using Shapiro Wilk Normality Test
  # Note Ho: Data are normal
  #     H1 : Data are not normal 
shapiro.test(Res)


## Step 11 Checking Multi-collinearity using Variance Inflation Factor
# Criteria: VIF > 5 indicates multicolinearity 
install.packages("car")
library(car)
vif(model_1)
vif(model_2)


## Step 12 Remove multicolinearity using Stepwise Regression 
install.packages ("MASS")
library(MASS) 
model_3 = lm(formula = Chance.of.Admit ~  GRE.Score + TOEFL.Score + SOP + LOR + CGPA + Research) 
step = stepAIC(model_3, direction = "both")
summary(step)
vif(step)




## Extra - Improving the model 
boxcox(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP + LOR + CGPA + Research)
transformed_y = (Chance.of.Admit^2 - 1) / 2 
extra_model = lm(formula = transformed_y ~  GRE.Score + TOEFL.Score + SOP + LOR + CGPA + Research) 
summary(extra_model)

pred = fitted(extra_model)
Res = residuals(extra_model)
plot(Res)
abline(0,0)
qqnorm(Res)
qqline(Res)

install.packages ("MASS")
library(MASS) 
model_final = lm(formula = transformed_y~  GRE.Score + TOEFL.Score + SOP + LOR + CGPA + Research) 
step2 = stepAIC(model_final, direction = "both")
summary(step2)
vif(step2)

