# Library Input
library(dplyr)
library(GGally)
library(ggcorrplot)
library(MLmetrics)
library(performance)
library(ggplot2)
library(lmtest)
library(car)
library(RColorBrewer)
library(DT)

# data preparation

# data Input
ins <- read.csv("C:/Users/JAMALUDDIN/OneDrive - Asia Pacific University/Desktop/assignment/insurance.csv")

# view content of datasets
View(ins)

# read the heading of the data
head(ins)

# Data Coercion
ins[ , c("sex", "smoker", "region")] <- 
  lapply(ins[ , c("sex", "smoker", "region")], as.factor)

str(ins)

# summary of charges
summary(ins$charges)

# visualize using histogram
hist(ins$charges)

# distribution of the data
table(ins$sex)
table(ins$smoker)
table(ins$children)


# To find any missing values
colSums(is.na(ins))


# Exploratory data analysis

# correlation matrix
cor(ins[c("age","bmi","children","charges")])

# scatterplot matrix
pairs(ins[c("age","bmi","children","charges")])



# install packages
install.packages("magrittr")
library(magrittr)


#install packages
install.packages("ggcorrplot")
library(ggcorrplot)

#install packages
install.packages("ggplot2")
library(ggplot2)



# Correlation
model.matrix(~0+., data=ins) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2, 
             colors = c("#1B9E77", "white", "#D95F02")) 



# Scatter Plot 2
ggplot(ins, aes(x=bmi, y=charges, shape=smoker, color=smoker)) +
  geom_point() +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")


#to fit the linear regression model to data

ins_model <- lm(charges ~age + children + bmi + sex + region,
                data = ins)

ins_model <- lm(charges ~ ., data = ins)

ins_model

#Building the model

# Splitting the data set
RNGkind(sample.kind = "Rounding")


set.seed(100) 

index_ins <- sample(x = nrow(ins) , size = nrow(ins)*0.8) 
ins_train <- ins[index_ins , ]
ins_test <- ins[-index_ins, ]

# Building Model (all variables)
model_all <- lm(formula = charges ~ . , data = ins_train)
summary(model_all)


# Feature Selection (backward)
model_backward <- step(object = model_all, direction = "backward", trace = 0)
summary(model_backward)




#install packages
install.packages("MLmetrics")
library(MLmetrics)

# Prediction of train data set
pred <- predict(model_backward, newdata = ins_train)
ins_train$prediction <- pred


# Error on the train data set prediction
MAPE(y_pred = ins_train$prediction, y_true = ins_train$charges)*100

# Prediction of test data set
prediction <- predict(model_backward, newdata = ins_test)
ins_test$prediction <- prediction

# Error on the train data set prediction
MAPE(y_pred = ins_test$prediction, y_true = ins_test$charges)*100




#install library
install.packages("ggplot2")
library(ggplot2)

#model assumption

# Linearity Scatter Plot
resact <- data.frame(residual = model_backward$residuals, fitted = model_backward$fitted.values)

ggplot(resact, aes(fitted, residual)) + 
  geom_point(aes(col = "#D95F02")) + 
  geom_smooth(aes(col = "#1B9E77")) + 
  geom_hline(aes(yintercept = 0)) + 
  theme(panel.grid = element_blank(), panel.background = element_blank()) 

#normality of residual
#histogram
hist(model_backward$residuals, col = "#1B9E77")


# Shapiro Test
shapiro.test(model_backward$residuals)


#no multicolinearity
library(car)
vif(model_backward)

#No autocorrelation
durbinWatsonTest(model_backward)

#homoscedasticity of residual
plot(x = model_backward$fitted.values, y = model_backward$residuals, col = "#1B9E77")
abline(h = 0, col = "#D95F02", lty = 2)





#Model recommendations

# Library Input

install.packages("randomForest")
install.packages("caret")
install.packages("lattice")
library(randomForest)
library(caret)

# Building Model
model_rf <- randomForest(formula = charges ~ . , data = ins_train, ntree = 1000, mtry = 3, 
                         keep.forest = FALSE, importance = TRUE) 
model_rf




# Prediction 
pred_rf <- model_rf$predicted
insurance_rf <- ins_train
insurance_rf$prediction <- pred_rf



datatable(insurance_rf)