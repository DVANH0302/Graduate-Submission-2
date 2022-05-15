# Set working directory 
setwd("C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project")

### Import and attach data 
grad = read.csv('Admission_Predict.csv')

# Remove the first column
grad = grad[, -1]

# Attach data 
attach(grad)



### Understanding and EDA 
install.packages("psych", dependencies = TRUE)
library(tidyverse)
library(psych)

# Overview of data
str(grad)
head(grad)

# Stat for the data attribute 
describe(grad)
summary(grad)

# Check for NA values 
any(is.na(grad))

# Frequency distribution of Chance of Admit (response variable)
ggplot(grad, aes(x=Chance.of.Admit)) + 
  geom_histogram(aes(y=..density..),      
                 bins = 20,
                 colour="black", fill="skyblue") +
  geom_density(alpha=.2, fill="#FF6666") + 
  ggtitle('Frequency distribution of Chance of Admit')


# Compute the correlation matrix 
cor(grad)

# Correlation plot 
pairs(grad)



## Model_1 
model_1  = lm(formula = Chance.of.Admit ~  GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research)

summary(model_1)
    # Criteria:  p value < 0.05
      # University.Rating and SOP have p-value > 0.05 
    # Criteria r-square > 0.6 (Satisfied)


## model_2 (dropping University.Rating and SOP)
model_2 = lm(formula = Chance.of.Admit ~ GRE.Score + TOEFL.Score + LOR + CGPA + Research)

summary(model_2)


# model_2 : Residual Analysis 


plot(model_2)
qqnorm(Res)
qqline(Res)

      #write.csv(pred,"C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project\\pred_m.csv") 
      #write.csv(Res,"C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project\\Res_m.csv")

# model_2: Standardizing Residuals using Scale function 
Std_Res = scale(Res, center = T, scale = T)
      #write.csv(Std_Res,"C:\\Users\\Computer\\OneDrive\\Desktop\\Statistic Project\\Std_Res_m.csv")

# model_2:  Normality Check on residuals 
qqnorm(Std_Res)
qqline(Std_Res)

# model_2 Normality test using Shapiro Wilk Normality Test
  # Note Ho: Data are normal
  #      H1 : Data are not normal 
shapiro.test(Res)


## model_2: Checking Multi-collinearity using Variance Inflation Factor
# Criteria: VIF > 5 indicates multicolinearity 
install.packages("car")
library(car)



## model_3: Remove multicolinearity using Stepwise Regression 
install.packages ("MASS")
library(MASS) 
model_3 = lm(formula = Chance.of.Admit ~  GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research) 
step = stepAIC(model_3, direction = "both")
summary(step)
vif(step)




## Extra - Improving the model using Box-Cox Transformation 

boxcox(Chance.of.Admit ~ GRE.Score + TOEFL.Score + LOR + CGPA + Research)

transformed_y = (Chance.of.Admit^2 - 1) / 2 


final_model = lm(formula = transformed_y ~  GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research) 
step = stepAIC(final_model, direction = "both")
summary(step)
vif(step)
plot(step)
anova(step)


