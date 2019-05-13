#"Is an automatic or manual transmission better for MPG"
#"Quantify the MPG difference between automatic and manual transmissions"

# Did the student interpret the coefficients correctly?
# Did the student do some exploratory data analyses?
# Did the student fit multiple models and detail their strategy for model selection?
# Did the student answer the questions of interest or detail why the question(s) is (are) not answerable?
# Did the student do a residual plot and some diagnostics?
# Did the student quantify the uncertainty in their conclusions and/or perform an inference correctly?
# Was the report brief (about 2 pages long) for the main body of the report and no longer than 5 with supporting appendix of figures?
# Did the report include an executive summary?
# Was the report done in Rmd (knitr)? 
data(mtcars)
names(mtcars)

library(dplyr)
mtcars1 <- mtcars %>% mutate(Transmission = as.factor(ifelse(am == 0, "Automatic", "Manual")))

library(ggplot2)
g <- ggplot(mtcars1, aes(x = Transmission, y = mpg, fill = Transmission))
g <- g + geom_boxplot()
g
#That suggests that a manual transmission may be better for mpg.

g1 <- ggplot(mtcars1, aes(x = Transmission, y = mpg, fill = Transmission))
g1 <- g1 + geom_point(aes(colour = Transmission))
g1

g2 <- ggplot(mtcars, aes(x = am, y = mpg, fill = am))
g2 <- g2 + geom_point(aes(colour = am))
g2 <- g2 + geom_smooth(method = lm)
g2
#That sort of graph is difficult to interpret because there's not really any 
#such thing as a transmission that is 30% manual and 70% automatic.

fit1 <- lm(mpg~Transmission, data = mtcars1)
summary(fit1)
# Call:
#         lm(formula = mpg ~ Transmission, data = mtcars1)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
# -9.3923 -3.0923 -0.2974  3.2439  9.5077 
# 
# Coefficients:
#                       Estimate        Std.    Error t value   Pr(>|t|)    
# (Intercept)           17.147          1.125   15.247          1.13e-15 ***
# TransmissionManual    7.245           1.764   4.106           0.000285 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.902 on 30 degrees of freedom
# Multiple R-squared:  0.3598,	Adjusted R-squared:  0.3385 
# F-statistic: 16.86 on 1 and 30 DF,  p-value: 0.000285
table(fit1$fitted.values) #Note that there are only two predicted values.
plot(
        fit1$residuals ~ fit1$fitted.values,
        data = mtcars1,
        xlab = 'Predicted Values',
        ylab = 'Residual values',
        main = 'Predicted v. Residuals For mpg As Function \n of Transmission Type Only'
)
#There is slightly more variation in the manual transmission data, but we don't 
        #learn much from the residuals.

#That regression summary indicates
        #For an automatic transmission the expected mpg would be about 17.147
        #For a manual transmission it would be 17.147 + 1*7.245 = 24.392
        #Both coefficients are statistically significant
        #The F statistic says that it's a good model
        #The R^2 values indicate that the linear model doesn't explain much of
                #the variance in the data.
        #There are other variables that may help explain the results. It's 
                #likely that the transmission type variable is picking up on 
                #variation due to something else like one of the following.
                        #displacement
                        #weight
                        #Engine (type)
                        #Number of forward gears (which is a part of the transmission)
                        #Number of carburetors (which measure the engine's 
                                #capacity to mix fuel with air in order to facilitate 
                                #combustion)

#Perhaps weight?
fit2 <- lm(mpg~Transmission+wt, data = mtcars1)
summary(fit2)
# Call: lm(formula = mpg ~ Transmission + wt, data = mtcars1)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
# -4.5295 -2.3619 -0.1317  1.4025  6.8782 
# 
# Coefficients:
#                       Estimate        Std. Error      t value Pr(>|t|)    
# (Intercept)           37.32155        3.05464         12.218  5.84e-13 ***
# TransmissionManual    -0.02362        1.54565         -0.015  0.988    
# wt                    -5.35281        0.78824         -6.791  1.87e-07 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.098 on 29 degrees of freedom
# Multiple R-squared:  0.7528,	Adjusted R-squared:  0.7358 
# F-statistic: 44.17 on 2 and 29 DF,  p-value: 1.579e-09
#R squared is a lot better, but now the transmission type is nearly completely 
        #inconsequential (manual went from +7 to -.02)
plot(
        fit2$residuals ~ fit2$fitted.values,
        data = mtcars1,
        xlab = 'Predicted Values',
        ylab = 'Residuals',
        main = 'Predicted vs. Residual For Model Including \n Transmission And Weight'
)

par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))

#Does ANOVA tell us it's a better model?
anova(fit1,fit2)
# Analysis of Variance Table
# 
# Model 1: mpg ~ Transmission
# Model 2: mpg ~ Transmission + wt
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     30 720.90                                  
# 2     29 278.32  1    442.58 46.115 1.867e-07 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#So weight is good to add to the linear model

#Perhaps the number of carburetors is also significant. Being able to burn more 
        #fuel at one time would increase performance, and that usually 
        #decreases efficiency.
fit3 <- lm(mpg~Transmission+wt+carb, data = mtcars1)
summary(fit3)
# Call: lm(formula = mpg ~ Transmission + wt + carb, data = mtcars1)
# 
# Residuals:
#       Min     1Q       Median 3Q      Max 
#       -4.5856 -2.1105  0.1393 1.5248  5.1851 
# 
# Coefficients:
#               Estimate        Std. Error t value      Pr(>|t|)    
# (Intercept)         34.0163     2.9713  11.448        4.49e-12 ***
# TransmissionManual   2.5263     1.6479   1.533        0.136490    
# wt                  -3.6340     0.9281  -3.915        0.000527 ***
# carb                -1.1593     0.4063  -2.853        0.008046 ** 
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.775 on 28 degrees of freedom
# Multiple R-squared:  0.8085,	Adjusted R-squared:  0.788 
# F-statistic: 39.41 on 3 and 28 DF,  p-value: 3.5e-10

#That regression has a lower F statistic but also a lower p value. Its R 
#squared is higher. Transmission type is still statistically 
#insignificant.
#Effect of a manual transmission goes from slightly negative to a bit more positive again

#Examine the residuals
plot(
        fit3$residuals ~ fit3$fitted.values,
        data = mtcars1,
        ylab = 'Residuals',
        xlab = 'Predicted Values',
        main = 'Residuals and Predicted Values for The Model Including Transmission, \n Weight, and Number of Carburetors'
)
#More uncorrelated

par(mfrow = c(2,2))
plot(fit3)
par(mfrow = c(1,1))

#What does ANOVA think?
anova(fit1,fit2, fit3)
# Analysis of Variance Table
# 
# Model 1: mpg ~ Transmission
# Model 2: mpg ~ Transmission + wt
# Model 3: mpg ~ Transmission + wt + carb
# Res.Df    RSS Df Sum of Sq       F    Pr(>F)    
# 1     30 720.90                                   
# 2     29 278.32  1    442.58 57.4719 2.941e-08 ***
# 3     28 215.62  1     62.70  8.1418  0.008046 ** 
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#That says that adding weight in model 2 improves the model significantly, and
        #adding number of carburetors in model 3 significantly improves the model.

#########Summary
#########Summary
#########Summary
#########Summary
# We see that modelling mpg based only on transmission type yields a positive and 
# statistically significant result that a manual transmission has a higher mpg 
# than an automatic one by about `r fit1$coefficients[2]` miles per gallon. 
# However, upon adding the car's weight to the model we see that the transmission 
# coefficient becomes negative and statistically insignificant (with a very high 
# p value). ANOVA analysis says that adding the weight variable does a better job 
# at explaining the analysis than using the transmission type alone. Furthermore 
# we see that adding the carburetor variable to the model returns the 
# transmission variable to having a positive effect, but the transmission type is 
# still not statistically significant. ANOVA indicates that adding the carburetor 
# to the transmission + weight model is a significant improvement over modeling 
# mpg based on transmission and weight alone.
Overall we conclude that the transmission type alone does not have a significant effect, and including other variables in the model can affect the way that the transmission interacts with the efficiency (as measured in mpg).