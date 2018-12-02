library("dplyr")
library("Matching")


foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns 
foo <- foo[, c(18, 19, 34, 35, 114, 49, 50, 55, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

which(is.na(foo) == TRUE)

##Question 3
#This creates a vector the length of the foo data set(122) and defines treatment as any U.N intervention, 
##by using the conditional (like a double negative): treatment is any country that did not have "None" , in other 
##words that did not have NO U.N. intervention = any country that had any type of U.N. intervention.
Tr <- rep(0, length(foo$untype))
Tr[which(foo$untype != "None")] <- 1

##Question 4

#logistic regression model
##first need to add Tr variable to the foo data frame 

foo <- mutate(foo, Tr = ifelse(foo$untype != "None",1,0))

#Recode as binary variable; not required, just want the practice
foo$pbs2l <- ifelse(foo$pbs2l == "Success", 1, ifelse(foo$pbs2l == "Failure", 0, 0))

#Logistic regression 
## First running raw data in match balance for variables considered in class (as if those are the only ones)
mb <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                     factnum2 + trnsfcap + treaty + develop + exp 
                   + decade, data = foo, nboots = 500)

log.model.2yr <- glm(pbs2l ~ Tr, data = foo, family = binomial)

#Originally thought the log odds equation(below) was the treatment effect... but Vinnie's note on Piazza said using the coefficient of Tr in reg. was suffience
##to put in the table below, so thats what I did... 

prob.success.no.treat <- (exp(log.model.2yr$coefficients[1])/(1 + exp(log.model.2yr$coefficients[1])))
prob.success.with.treat <- (exp(log.model.2yr$coefficients[1] + log.model.2yr$coefficients[2])/ 
                              (1 + exp(log.model.2yr$coefficients[1] + log.model.2yr$coefficients[2])))

log.treat.2yr<- prob.success.with.treat - prob.success.no.treat
 

#Need to specify which values of 5 yr to use (i.e. not the NAs) Y5 = 5 year lenient, but only some are valid
valid_Y5 <-!is.na(foo$pbs5l) 

log.model.5yr <- glm(pbs5l[valid_Y5] ~ Tr[valid_Y5], data = foo, family = binomial) 

##Again calulated treatment effect as such, but changed answer in table based on Vinnie's suggestion

prob.success.no.treat.5 <- (exp(log.model.5yr$coefficients[1])/(1 + exp(log.model.5yr$coefficients[1])))
prob.success.with.treat.5 <- (exp(log.model.5yr$coefficients[1] + log.model.5yr$coefficients[2])/ 
                              (1 + exp(log.model.5yr$coefficients[1] + log.model.5yr$coefficients[2])))
log.treat.5yr<- prob.success.with.treat.5 - prob.success.no.treat.5


##For subsequent models decided to simplying lenient peacebuilding success for 2 years 
Y2 <- foo$pbs2l

##Propentsity score model: run via Match & then put into MatchBalance
glm1 <- glm(Tr ~ wartype + logcost + wardur + factnum + 
            factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, family = binomial)

X <- glm1$fitted.values

mout <- Match(Tr = foo$Tr, X = X, M = 1, estimand = "ATT", replace = TRUE, ties = TRUE)

##nboots should be set to 500 for publication-level p  value
p.mb <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + 
                      foo$factnum2 + foo$trnsfcap + foo$treaty + foo$develop + foo$exp 
                    + foo$decade, match.out = mout, nboots = 500)


#Rerun Match, this time including outcome variables (Y's defined above) and add balance adjustment argument
##However, we won't include these results in the table anyways as leximan p value > 0.1 (from MatchBalance above)
propmod.2yroutcomes <- Match(Y = Y2, Tr = foo$Tr, X = X, M = 1, estimand = "ATT", replace = TRUE, ties = TRUE, BiasAdjust = TRUE)

##Need to drop NAs from the 5 year
propmod.5yroutcomes <- Match(Y = foo$pbs5l[valid_Y5], Tr = foo$Tr[valid_Y5], X = X[valid_Y5], M = 1, estimand = "ATT", replace = TRUE, ties = TRUE, BiasAdjust = TRUE)
summary(propmod.5yroutcomes)


##Genetic Matching 
X1 <- cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, 
            foo$factnum2, foo$trnsfcap, foo$treaty, foo$develop, foo$exp, foo$decade)

genout <- GenMatch(Tr = foo$Tr, X = X1, estimand = "ATT", M = 1, pop.size = 300, max.generations = 30, wait.generations = 3)


#exclude the Y (outcome) b/c don't want to look at until we've achieved decent balance
gen.mout <- Match(Tr = foo$Tr, X = X1, estimand = "ATT", M = 1, Weight.matrix = genout, replace = TRUE, ties = TRUE)

#Look at the before/after balance of variables before deciding whether need to try to find better balance, or 
##ready to look at treatement effect by looking at outcome variables
gen.mb <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + 
                         foo$factnum2 + foo$trnsfcap + foo$treaty + foo$develop + foo$exp 
                       + foo$decade, match.out = gen.mout, nboots = 500)

#Rerun Match, this time including outcome variables (Y's defined above)
genmatch.2yroutcomes <- Match(Y = Y2, Tr = foo$Tr, X = X1, M = 1, estimand = "ATT", Weight.matrix = genout, replace = TRUE, ties = TRUE, BiasAdjust = TRUE)
summary(genmatch.2yroutcomes)

genmatch.5yroutcomes <- Match(Y = foo$pbs5l[valid_Y5], Tr = foo$Tr[valid_Y5], X = X1[valid_Y5,], M = 1, estimand = "ATT", Weight.matrix = genout, replace = TRUE, ties = TRUE, BiasAdjust = TRUE)
summary(genmatch.5yroutcomes)

##Combine all meaningful results into a table 

#First the logistic regression results..
results <- c(NA, log.model.2yr$coefficients[2], 3.2767e-05)
results <- rbind(results, c(NA, log.model.5yr$coefficients[2], 3.2767e-05))


## Then the  Propensity score results...
##The leximan p value < 0.1, so include neither treatment affects per HW instructions 
results <- rbind(results, c(NA, NA, 2.22e-16))
results <- rbind(results, c(NA, NA, 2.22e-16))

## And then the GenMatch score results..                        
results <- rbind(results, c(genmatch.2yroutcomes$est, genmatch.2yroutcomes$est.noadj, 0.250))
results <- rbind(results, c(genmatch.5yroutcomes$est, genmatch.5yroutcomes$est.noadj, 0.250))

#Round results because the numbers for these columns were long/cluttered the table
results[,1:2] <- round(results[,1:2], 3)

#Put all results into data frame + add rows
results <- data.frame(results, row.names =c('Logistic 2Y', 'Logistic 5Y','Propensity 2Y', 'Propensity 5Y',
                                            'GenMatch 2Y', 'GenMatch 5Y'))
#Label colums
colnames(results) <- c('tmt effect (bias adj)' ,	'tmt effect (no bias adj)',	'p-value') 

#Check that it's pretty!
results

