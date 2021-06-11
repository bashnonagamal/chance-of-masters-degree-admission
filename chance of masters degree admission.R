# Read CSV file
grades <- read.csv("D:/BIG DATA/Admission_Predict_Ver1.1.csv")

# Dataset Summary
summary(grades)

# Correlation Matrix 
corr <- round(cor(grades), 1)
corr

# Data Population
library(e1071)
plot(density(grades$Chance.of.Admit), main="Density Plot: Chance of Admit", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(grades$Chance.of.Admit), 2)))  

# Box Plot for CGPA
boxplot(grades$CGPA, main="CGPA", sub=paste("Outlier rows: ", boxplot.stats(grades$CGPA)$out))

# All Relationships
pairs(grades)

# Important Relationships
scatter.smooth(x=grades$CGPA, y=grades$Chance.of.Admit, xlab="CGPA", ylab="Chance of admit", main="CGPA and Chance of Admit")
par(mfrow=c(1,2))
scatter.smooth(x=grades$TOEFL.Score, y=grades$Chance.of.Admit, xlab="TOEFL Score", ylab="Chance of admit", main="TOEFL and Chance of Admit")
scatter.smooth(x=grades$GRE.Score, y=grades$Chance.of.Admit, xlab="GRE Score", ylab="Chance of admit", main="GRE and Chance of Admit")
scatter.smooth(x=grades$University.Rating, y=grades$Chance.of.Admit, xlab="University Rating", ylab="Chance of admit", main="University Rating and Chance of Admit")
scatter.smooth(x=grades$Research, y=grades$Chance.of.Admit, xlab="Research", ylab="Chance of admit", main="Research and Chance of Admit")

# Create Training and Test data
set.seed(100)
trainingRowIndex <- sample(1:nrow(grades), 0.8*nrow(grades))
train <- grades[trainingRowIndex, ]
test  <- grades[-trainingRowIndex, ]

# Create The model
model <- lm(Chance.of.Admit ~ GRE.Score+TOEFL.Score+University.Rating+SOP+LOR+CGPA+Research, data = train)
summary(model)

# Get Predictions
predicted = predict(model, newdata=test)

# Actual prediction data frame
actuals_preds <- data.frame(cbind(actuals=test$Chance.of.Admit, predicteds=predicted))

# Multiple Linear Regression plotting
par(mfrow=c(1,1))
plot(actuals_preds$actuals, xlab="",ylab="", col="red")
points(actuals_preds$predicteds , col="blue")
legend(89, 0.45, legend=c("actuals", "predicted"),
       col=c("red", "blue"), lty=3:3, cex=0.8)

# Model Accurecy Using Correlation
correlation_accuracy <- cor(actuals_preds) * 100
correlation_accuracy

# Model Accurecy Using Min Max Accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) * 100
min_max_accuracy