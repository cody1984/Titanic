
# Initialise Project ------------------------------------------------------

library(ggplot2)
library(ggExtra)
library(caret)
library(mice)
library(VIM)
library(lattice)
library(DMwR)
library(rpart)
library(MASS)
library(randomForest)


library(rattle)

train <- read.csv("train.csv", header = TRUE, stringsAsFactors=FALSE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors=FALSE)

# Summary Statistics ------------------------------------------------------

summary(train)
str(train)


table(train$Survived)
round(prop.table(table(train$Survived)), 3)

table(train$Survived, train$Sex)
round(prop.table(table(train$Sex, train$Survived),1), 3) # row proportions
round(prop.table(table(train$Sex, train$Survived),2), 3) # column proportions


table(train$Survived, train$Pclass)
round(prop.table(table(train$Pclass, train$Survived),1), 3) # row proportions
round(prop.table(table(train$Pclass, train$Survived),2), 3) # column proportions


table(train$Pclass)
round(prop.table(table(train$Pclass)), 3)

table(train$Sex)
round(prop.table(table(train$Sex)), 3)

table(train$SibSp)
round(prop.table(table(train$SibSp)), 3)

table(train$Parch)
round(prop.table(table(train$Parch)), 3)

table(train$SibSp)
round(prop.table(table(train$SibSp)), 3)

table(train$Embarked)
round(prop.table(table(train$Embarked)), 3)


summary(train$Age) # 177 NA's

summary(train$Fare)



# Summary Plots -----------------------------------------------------------

# https://www.r-graph-gallery.com/portfolio/ggplot2-package/

# basic chart
basic <- ggplot(train, aes(Fare, Age)) +
   geom_point() +
   facet_wrap(~ Pclass)

basic

# Change background
basic + theme(strip.background = element_rect(colour = "red", fill = alpha("blue", 0.2) ))

# Change the text 
basic + theme(strip.text.x = element_text(colour = "red", face = "bold", size=10, angle=30))

# Change the space between parts:
basic + theme(panel.spacing = unit(4, "lines"))


basic <- ggplot(train, aes(Fare, Age, colour = factor(Pclass), shape = factor(Sex) )) +
   geom_point()

basic

basic + labs(colour = "Passenger Class", shape = "Gender")

basic + guides(shape = FALSE)


# classic plot :
p <- ggplot(train, aes(x=Fare, y=Age, color=Pclass, size=Pclass)) +
   geom_point() +
   theme(legend.position="none")

# with marginal histogram
ggMarginal(p, type="histogram")

# marginal density
ggMarginal(p, type="density")

# marginal boxplot
ggMarginal(p, type="boxplot")



# Modelling Code ----------------------------------------------------------

set.seed(42)
train$Survived <- as.factor(train$Survived)

imp <- mice(train)
train2 <- complete(imp)


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train2,
             method = "class")

summary(fit)
Prediction <- predict(fit, test, type = "class")


fit <- glm(Survived ~ Pclass + Sex + Age + SibSp,
           family = binomial(logit),
           data=train2)

summary(fit)
Prediction <- predict(fit, test, type = "response")
Prediction <- ifelse(Prediction > 0.5,1,0)




# Submission Output -------------------------------------------------------

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "rpart_mice.csv", row.names = FALSE)
write.csv(submit, file = "logit_mice.csv", row.names = FALSE)


# Throwaway code ----------------------------------------------------------

p.age <- lm(Age ~ . - Name - Ticket - Cabin - PassengerId - Embarked - Sex - Parch - Fare, data = train)
summary(p.age)