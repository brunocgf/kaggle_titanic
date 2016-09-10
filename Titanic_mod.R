# The sinking of the RMS Titanic is one of the most infamous shipwrecks in history.  On April 15, 1912, during her maiden voyage, the
# Titanic sank after colliding with an iceberg, killing 1502 out of 2224 passengers and crew. This sensational tragedy shocked the
# international community and led to better safety regulations for ships.

# One of the reasons that the shipwreck led to such loss of life was that there were not enough lifeboats for the passengers and crew.
# Although there was some element of luck involved in surviving the sinking, some groups of people were more likely to survive than others,
# such as women, children, and the upper-class.

# In this challenge, we ask you to complete the analysis of what sorts of people were likely to survive. In particular, we ask you to apply
# the tools of machine learning to predict which passengers survived the tragedy.

# VARIABLE DESCRIPTIONS:
#  survival        Survival (0 = No; 1 = Yes)
# pclass          Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)

# SPECIAL NOTES:
# Pclass is a proxy for socio-economic status (SES) 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# Age is in Years; Fractional if Age less than One (1) If the Age is Estimated, it is in the form xx.5
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic



# I start setting the work directory and calling the libraries I'll use in the model.

setwd("C:/Users/Bruno Gonzalez/Documents/Titatic ML")
library(ggplot2)
library(caret)

# After that y download the data and read it

url_train <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/train.csv?sv=2012-02-12&se=2016-09-09T18%3A52%3A08Z&sr=b&sp=r&sig=TbOt%2BBSzGCLd8u3zU22bELJEWOTYBGfg93OgfXT83t4%3D"
url_test <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/test.csv?sv=2012-02-12&se=2016-09-09T18%3A46%3A25Z&sr=b&sp=r&sig=DuEtRppBztpjPcyX2TR85q0F1rNcI%2F60L2QGNjlQZa4%3D"

download.file(url_train, destfile = "./train.csv")
download.file(url_test, destfile = "./test.csv")

test <- read.csv("test.csv")
train <- read.csv("train.csv")

# Quick visualization of the dataset

str(train)
summary(train)

#It can be noticed that not all variables has relevance on the prediction. However, I'll make some analysis to pick those ones.

pairs(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Cabin+Embarked, data=train)


#I think Pclass can detarminate more variables.
pairs(Pclass~SibSp+Parch+Fare+Cabin+Embarked, data=train)

#I started checking the the familiy numbers
plot(train$SibSp+rnorm(891, sd=0.1), train$Parch+rnorm(891, sd=0.1), col=train$Pclass, pch=4)

#It can be seen that thera are not visible clusters so is a good idea to sum both variables.

train_mod <- train[c(1,2,3,4,5,6,9,10,11,12)]
test_mod <- test[c(1,2,3,4,5,8,9,10,11)]
train_mod$Fam <- train$SibSp+train$Parch
test_mod$Fam <- test$SibSp+test$Parch

#I'll create a subtrain and a subtest patition to evaluate the models.

part <- createDataPartition(train_mod$Survived, p=0.85, list=FALSE)

sub_train <- train_mod[part,]
sub_test <- train_mod[-part,]


mod_1 <- train(as.factor(Survived)~Pclass+Sex+Fam, method="gbm", data=sub_train)
mod_2 <- train(as.factor(Survived)~Pclass+Sex+Fam, method="glm", data=sub_train)

sub_train2 <- sub_train[complete.cases(sub_train),]

mod_3 <- train(as.factor(Survived)~Pclass+Sex+Fam, method="rf", data=sub_train2)

mod_4 <- train(as.factor(Survived)~Pclass+Sex+Fam+Fare, method="rf", data=sub_train2)

pred <- predict(mod_3, newdata = test_mod)
my_solution <- data.frame(PassengerId = test_mod$PassengerId, Survived = pred)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names=FALSE)



