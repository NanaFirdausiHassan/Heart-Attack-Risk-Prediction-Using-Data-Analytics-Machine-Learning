# {r}
install.packages("rsdmx")
install.packages("dplyr")
install.packages("ROSE")
install.packages("leaps")
install.packages("glmnet")
library(tidyverse)
library(ggplot2)
library(rsdmx)
library(dplyr)
library(ROSE)
library(leaps)
library(glmnet)
library(tidyr)
library(janitor)



df <-read.csv(file.choose(),header=T)
head(df)



is.data.frame(df)
dim(df)

str(df)

colnames(df)

# Data Cleaning
df <- clean_names(df)

df$blood_pressure <- sub("/", " ", df$blood_pressure)
df <- separate(df,blood_pressure, into = c("systolic", "diastolic"), sep = " ")
df$systolic <- as.numeric(df$systolic)
df$diastolic <- as.numeric(df$diastolic)

df$blood_pressure <- ifelse(df$systolic >= 140 | df$diastolic >= 90, "Arterial Hypertension",
                      ifelse((df$systolic >= 130 & df$systolic <= 139) | 
                               (df$diastolic >= 85 & df$diastolic <= 89), "High",
                             ifelse((df$systolic >= 120 & df$systolic <= 129) | 
                                      (df$diastolic >= 80 & df$diastolic <= 84), "Normal",
                                    ifelse(df$systolic < 120 & df$diastolic < 80, "Optimal", NA))))

# creating a new variable "age_group" based on the "age" variable
df$age_group <- ifelse(df$age < 30, "Young",
                       ifelse(df$age >= 30 & df$age < 50, "Middle Aged", "Senior"))

# Convert character columns to factors
df <- df %>%
  mutate(across(where(is.character), as.factor))

df$diabetes <- as.factor(df$diabetes)
df$family_history <- as.factor(df$family_history)
df$smoking <- as.factor(df$smoking)
df$obesity <- as.factor(df$obesity)
df$alcohol_consumption <- as.factor(df$alcohol_consumption)
df$previous_heart_problems <- as.factor(df$previous_heart_problems)
df$medication_use <- as.factor(df$medication_use)
df$heart_attack_risk <- as.factor(df$heart_attack_risk)
df$blood_pressure <- as.factor(df$blood_pressure)
df$age_group <- as.factor(df$age_group)

# remove some variables
df <- df %>% select(-patient_id,-country, -continent, -hemisphere)

# Handling missing values
df <- df[complete.cases(df), ]
df[is.na(df)] <- sapply(df, function(x) ifelse (is.numeric(x), median(x, na.rm = TRUE), x))

# Cleaned Data set
colSums(sapply(df, is.na))

str(df)

head(df)

#Data Analysis
summary(select(df, age, cholesterol, heart_rate, diet))

# plot for categorical variables
for (var_name in names(df)) {
  if(is.factor(df[[var_name]])) {
    #create a bar plot
    p <- ggplot(df, aes_string(x = var_name)) +
      geom_bar(fill = "magenta", color = "black") +
      theme_minimal() +
      labs(title = paste("Bar plot of", var_name), x =var_name, y ="Frequency")
    
    print(p)
  }
  
}

ggplot(df, aes(x=sex)) + ggtitle("Sex Percentage of Patients") + xlab("Sex") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.25, fill="cyan") + ylab("Percentage") + theme_minimal()

ggplot(df, mapping = aes(x=age, fill = sex)) + geom_density(position = "stack")

ggplot(df, aes(x=diet)) + ggtitle("Percentage of Patient's Diet") + xlab("Diet") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.25, fill= "skyblue") + ylab("Percentage") + theme_minimal()

ggplot(df, mapping = aes(x=diet, fill = age_group)) + geom_bar(color="gray", position = "dodge")

ggplot(df, aes(x=age_group)) + ggtitle("Percentage patient's Age Group") + xlab("Age Group") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.25, fill="steelblue") + ylab("Percentage") + theme_minimal()

ggplot(df, aes(x=blood_pressure)) + ggtitle("Percentage of Patient's Blood Pressure") + xlab("Blood Pressure") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.4, fill="lightblue") + ylab("Percentage") + coord_flip() +theme_minimal()

ggplot(df, mapping = aes(x=sex, fill = blood_pressure)) + geom_bar(color="grey2", position = "dodge")

ggplot(df, mapping = aes(x=age_group, fill = sex)) + geom_bar(color="white", position = "dodge") + theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5), aspect.ratio = 1) + labs(title = "Count of Patients by Age Group") + scale_fill_manual(values = c("#009999", "#D6604D", "#0000FF"))

diet.cho.df <- xtabs(~diet+cholesterol+heart_attack_risk, data = df)
diet.cho.df

margin.table(diet.cho.df,1)

diet.cho.heart.df <- as.data.frame(margin.table(diet.cho.df,1))
diet.cho.heart.df

ggplot(df, mapping = aes(x=heart_attack_risk, y=triglycerides, fill = heart_attack_risk)) +geom_boxplot() +geom_boxplot(color="gray14") + theme_minimal() + scale_fill_manual(values = c("steelblue", "cadetblue")) + labs(title = "Relationship Between Heart Attack Risk and Triglycerides")

ggplot(df, mapping = aes(x=heart_attack_risk, y=cholesterol, fill = heart_attack_risk)) +geom_boxplot()+geom_boxplot(color="black") +facet_wrap(~sex)+ theme_minimal() + scale_fill_manual(values = c("steelblue", "cadetblue")) + labs(title = "Heart Attack Risk and Cholesterol with Sex")

ggplot(df, aes(x=heart_attack_risk, y=diabetes, fill = heart_attack_risk)) +
  geom_violin(trim = FALSE, color="black") +
  facet_wrap(~sex) +
  scale_fill_manual(values = c("steelblue", "cadetblue")) +
  labs(title = "Heart Attack Risk based on Diabetes and Sex") + 
  theme_minimal()

table(df$heart_attack_risk)

library(plotly)

fig <- plot_ly(df, labels = ~heart_attack_risk, type = "pie")
fig %>% layout(title = "Percentage of Heart Attack Risk Among Patients")

summary(df)

numeric1 <- select_if(df, is.numeric)
cor(numeric1)

library(ggcorrplot)
ab <- cor(numeric1)
ggcorrplot(ab, hc.order = TRUE,
           type = "lower", lab = TRUE)

cor(df$systolic, df$diastolic)
ggplot(df, mapping = aes(x=systolic, y=diastolic)) +geom_point(color="brown2", shape=1) + theme_minimal()

cor(df$exercise_hours_per_week, df$physical_activity_days_per_week)
ggplot(df, mapping = aes(x=physical_activity_days_per_week, y=exercise_hours_per_week)) + geom_point(color="navy", shape=1) + theme_minimal()

cor(df$bmi, df$income)
ggplot(df, mapping = aes(x=bmi, y=income, color=heart_attack_risk)) + geom_point() + theme_minimal()

cor(df$stress_level, df$sleep_hours_per_day)
ggplot(df, mapping = aes(x=stress_level, y=sleep_hours_per_day, color=heart_attack_risk)) +geom_point() + theme_minimal()

cor(df$sedentary_hours_per_day,df$physical_activity_days_per_week)
ggplot(df, mapping = aes(x=physical_activity_days_per_week, y=sedentary_hours_per_day)) +geom_point(color="orangered4", shape=1) + theme_minimal()

cor(df$sleep_hours_per_day, df$triglycerides)
ggplot(df, mapping = aes(x=sleep_hours_per_day, y=triglycerides, color= heart_attack_risk)) + geom_point() + theme_minimal()

new.df <- ovun.sample(heart_attack_risk ~ ., data = df, method = "over", N=nrow(df), seed = 1234)$data
View(new.df)
class_count <- table(new.df$heart_attack_risk)
class_count

set.seed(1234)
new.df_split <- sample(x=nrow(new.df), size = .70*nrow(new.df))
train <- new.df[new.df_split,]
test<- new.df[-new.df_split,]

glm.fit <- glm(heart_attack_risk~., data = train, family = "binomial")
summary(glm.fit)

# capture the summary
model_summary <- summary(glm.fit)
# Print deviance residuals manually (mimicking what summary usually shows)
cat("Deviance Residuals:\n")
print(summary(residuals(glm.fit, type = "deviance")))

probs <- predict(glm.fit, test, type = "response")
predict <- rep("No", length(probs))
predict[probs > 0.5] <- "Yes"
table(predict, test$heart_attack_risk)
mean(predict != test$heart_attack_risk)

fwd.set = regsubsets(heart_attack_risk~., data = new.df, nvmax = 10, method = "forward")
summary(fwd.set)

bkd.set <- regsubsets(heart_attack_risk~., data = new.df, nvmax = 10, method = "backward")
summary(bkd.set)

coef(fwd.set ,7)
coef(bkd.set ,7)

fwd.set <- regsubsets(heart_attack_risk~., data = train, nvmax = 10, method = "forward")
bkd.set <-regsubsets(heart_attack_risk~., data = train, nvmax = 10, method = "backward")

coef(fwd.set ,7)
coef(bkd.set ,7)

par(mfrow=c(2,2))
plot(summary(fwd.set)$rss ,xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(summary(fwd.set)$rsq ,xlab = "Number of Variables", ylab = "Rsq", type = "l")
plot(summary(fwd.set)$adjr2 ,xlab = "Number of Variables", ylab = "adjr2", type = "l")
plot(summary(fwd.set)$cp ,xlab = "Number of Variables", ylab = "cp", type = "l")
plot(summary(fwd.set)$bic ,xlab = "Number of Variables", ylab = "bic", type = "l")

glm.fit2<- glm(heart_attack_risk~diet + cholesterol + bmi + alcohol_consumption + age_group + family_history + age, data = train, family = "binomial" )
summary(glm.fit2)

model_summary <- summary(glm.fit2)
# Print deviance residuals manually (mimicking what summary usually shows)
cat("Deviance Residuals:\n")
print(summary(residuals(glm.fit2, type = "deviance")))

probs2 <- predict(glm.fit2, test, type = "response")
predict2 <- rep("No", length(probs2))
predict2[probs2 > 0.5] <- "Yes"
table(predict2, test$heart_attack_risk)
mean(predict2 != test$heart_attack_risk)

glm.fit3 <- glm(heart_attack_risk~diet + cholesterol + bmi + alcohol_consumption + age_group + family_history + exercise_hours_per_week, data = train, family = "binomial")
summary(glm.fit3)

model_summary <- summary(glm.fit3)
cat("Deviance Residuals:\n")
print(summary(residuals(glm.fit3, type = "deviance")))

probs3 <- predict(glm.fit3, test, type = "response")
predict3 <- rep("No", length(probs3))
predict3[probs3 > 0.5] <- "Yes"
table(predict3, test$heart_attack_risk)

mean(predict3 != test$heart_attack_risk)


lda.fit<- lda(heart_attack_risk~diet + cholesterol + bmi + alcohol_consumption + age_group + family_history + age, data=train, family="binomial")
lda.fit

pred.lda<- predict(lda.fit, test)
table(pred.lda$class, test$heart_attack_risk)

mean(pred.lda$class != test$heart_attack_risk)

# fit QDA model
qda.fit <- qda(heart_attack_risk ~ diet + cholesterol + bmi + alcohol_consumption + age_group + family_history + age, data = train)
qda.fit

pred.qda<- predict(qda.fit, test)
table(pred.qda$class, test$heart_attack_risk)

mean(pred.qda$class != test$heart_attack_risk)

install.packages("e1071")
library(e1071)
nb_model <- naiveBayes(heart_attack_risk~ cholesterol + bmi + alcohol_consumption + blood_pressure, data = df)
print(nb_model)


fwd.set2 = regsubsets(cholesterol~., data = new.df, nvmax = 10, method = "forward")
bkd.set2 = regsubsets(cholesterol~., data = new.df, nvmax = 10, method = "backward")
coef(fwd.set2 ,7)
coef(bkd.set2 ,7)

split<- sample(x=nrow(new.df), size = .70*nrow(new.df))
train2 <- new.df[split,]
test2 <- new.df[-split,]
fwd.set2 = regsubsets(cholesterol~., data = train2, nvmax = 10, method = "forward")
bkd.set2 = regsubsets(cholesterol~., data = train2, nvmax = 10, method = "backward")

coef(fwd.set2 ,7)
coef(bkd.set2 ,7)

lm.fit<- lm(cholesterol~., data = train2)
summary(lm.fit)

pred.lm<- predict(lm.fit, test2)
mean((pred.lm - test2$cholesterol)^2)

lm.fit2 <- lm(cholesterol~exercise_hours_per_week + stress_level + heart_attack_risk, data = train2)
summary(lm.fit2)

pred.lm2<- predict(lm.fit2, test2)
mean((pred.lm2 - test2$cholesterol)^2)

train.mat<- model.matrix(cholesterol~exercise_hours_per_week + stress_level + heart_attack_risk, data = train2)
test.mat<- model.matrix(cholesterol~exercise_hours_per_week + stress_level + heart_attack_risk, data = test2)
grid <- 10 ^ seq(4, -2, length = 100)
fit.ridge <- glmnet(train.mat, train2$cholesterol, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train.mat, train2$cholesterol, alpha = 0, lambda = grid, thresh = 1e-12)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
mean((pred.ridge - test2$cholesterol)^2)

fit.lasso<- glmnet(train.mat, train2$cholesterol, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso<- cv.glmnet(train.mat, train2$cholesterol, alpha = 1, lambda = grid, thresh = 1e-12)
bestlam.lasso<- cv.lasso$lambda.min
bestlam.lasso
pred.lasso<- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - test2$cholesterol)^2)

test.avg<- mean(test2$cholesterol)
lm.r2 <- 1 - mean((pred.lm - test2$cholesterol)^2) / mean((test.avg - test2$cholesterol)^2)
ridge.r2<- 1 - mean((pred.ridge - test2$cholesterol)^2) / mean((test.avg - test2$cholesterol)^2)
lasso.r2<- 1 - mean((pred.lasso - test2$cholesterol)^2) / mean((test.avg - test2$cholesterol)^2)
lm.r2
ridge.r2
lasso.r2

# PCA
pca_result <- prcomp(numeric1)
# plot the variance explained by each principal component
plot(pca_result, type = "l", main = "scree plot")
biplot(pca_result, main = "Biplot of PCA")


install.packages("class")

# KNN
library(class)

numeric_columns <- sapply(df, is.numeric)
df[numeric_columns] <- scale(df[numeric_columns])

set.seed(1)
indices <- sample(1:nrow(df), size = 0.7*nrow(df))
train_data <- df[indices, ]
test_data <- df[-indices, ]

predicted_heartattackrisk <- knn(train = train_data[numeric_columns],
                                 test = test_data[numeric_columns],
                                 cl = train_data$heart_attack_risk,
                                 k = 5)
table(predicted = predicted_heartattackrisk, actual = test_data$heart_attack_risk)

my_data<- new.df[,c(3,23)]
my_data <- as.data.frame(my_data)
ggplot(data = my_data, mapping = aes(x=cholesterol, y=heart_attack_risk)) + geom_point()

set.seed(1)
fit <- kmeans(x=my_data, centers = 5)
my_data$cluster <- factor(fit$cluster)
ggplot() + geom_point(data = my_data, mapping = aes(x=cholesterol, y =heart_attack_risk, color=cluster)) + geom_point(data = data.frame(fit$centers, centroid=as.factor(1:nrow(fit$centers))), mapping = aes(x=cholesterol, y=heart_attack_risk, fill=centroid), shape=23, color="black")
my_data1<- as.data.frame(scale(my_data[,1:2]))
wss <- (nrow(my_data1)-1)* sum(apply(X=my_data1, MARGIN = 2, FUN = var))

set.seed(1)
for (i in 2:15) wss [i] <- sum(kmeans(my_data1, centers = i)$withinss)
wss.df<- data.frame(cluster = 1:15, wss = wss)
ggplot(data = wss.df, mapping = aes(x=cluster,y=wss)) + geom_line() + scale_x_continuous(breaks = seq(from=1, to=15, by=1)) + labs(x="Number of Clusters", y="Within-Clusters Sum of squares")
km.out=kmeans (my_data1, 6, nstart=20)
km.out$tot.withinss
plot(my_data1, col=(km.out$cluster +1), main= "K-Means Clustering Results with K=6", xlab="", ylab="", pch=20, cex=1)