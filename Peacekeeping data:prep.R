##Questions 2
install.packages("Matching")
install.packages("rgenoud")
install.packages("dplyr")
require(dplyr)
require(ggplot2)
require(tidyr)
library(MASS)
library(tree)
library(arm)
library(reshape2)
library(Matching)

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns ADD pbs
foo <- foo[, c(6:8, 11:16, 18, 19, 99, 50, 102, 114, 49, 55, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)


### codebook is here: http://www.nyu.edu/gsas/dept/politics/faculty/cohen/codebook.pdf

#Question 2
orig <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + 
              factnum2 + trnsfcap + untype4 + treaty + develop + exp 
            + decade, data = foo, family = binomial())
summary(orig)

mod <- glm(pbs2s3 ~ wartype + logcost + wardur*untype4 + factnum + 
         factnum2 + trnsfcap + treaty + develop + exp + 
          decade, data = foo, family = binomial())
summary(mod)


use_col <- c('wartype', 'logcost' ,  'untype4', 'factnum',  
  'factnum2' , 'trnsfcap' , 'treaty' , 'develop' , 'exp', 'decade')

newdata <- data.frame(wardur= 5:315)
newdata[,use_col] <- matrix(rep(apply(foo[,use_col], 2, mean), nrow(newdata)), nrow=nrow(newdata), byrow=T)

#Compare the predicted outcomes for treatment v. control, as predicted by the original regression model 
x0 <- newdata
x0$untype4 <- 0

x1 <- newdata
x1$untype4 <- 1

##This is the marginal treatment effect
peace_original <- predict(orig, newdata = x1, type='response') - predict(orig, newdata = x0, type='response')

#Creat plot with original regression model graphed
plot(newdata$wardur, peace_original, type='l',ylim=c(0,1), xlab = 'Duration of War (in months)', 
     ylab = 'Marginal effects of UN peacekeeping operations', main = "Replication of King & Zeng's Figure 8")

#Compare the predicted outcomes for treatment v. control, as predicted by the second regression model (with interaction)
##This is the marginal treatement effect
peace_modified <- predict(mod, newdata = x1, type='response') - predict(mod, newdata = x0, type='response')

#Add second model to the plot for visual comparison of implications of model dependence
lines(newdata$wardur, peace_modified, type='l', ylim=c(0,1), col='red')





