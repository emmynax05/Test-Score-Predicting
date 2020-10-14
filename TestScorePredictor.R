############################################################
#Predicting student performance in math, reading and writing
############################################################

########reading the data and cleasing/preparation ##########

#read the excel file
library(readxl)

setwd("./School/Year 2/Fall 1/Big Data/R Files/")
performance = read.csv("StudentsPerformance.csv", header = TRUE)
View(performance)

#check for any missing data
sapply(performance, function(x){sum(is.na(x))})

#check the structure of each variable
str(performance)

#we need to change the categorical variables to factors
qual_vars = c(1:5)
performance[,qual_vars] = lapply(performance[,qual_vars], factor)
str(performance)

#some visualization of correlation between the test scores
dev.new()
pairs(performance[,-qual_vars])
cor(performance[,-qual_vars])
#this shows that a student who performs well in the writing test is highly likely to also perform well in the reading test and vice versa


#split dataset into training and validation data in ratio 60:40
set.seed(10)
rownum = nrow(performance)
trainindex = sample(rownum, 0.6*rownum)

train = performance[trainindex,]
validation = performance[-trainindex,]

########linear regression to predict test scores for math, reading and writing############
#first we perform the regression for math scores
math_reg = lm (formula = math.score ~ . -reading.score - writing.score, data = train)
summary(math_reg)
#R2 is extremely low: 0.267!!

#next, we perform the regression for reading scores
reading_reg = lm (formula = reading.score ~ . -math.score - writing.score, data = train)
summary(reading_reg)
#R2 is extremely low: 0.228!!

#finally, we perform the regression for writing scores
writing_reg = lm (formula = writing.score ~ . -math.score - reading.score, data = train)
summary(writing_reg)
#R2 is extremely low: 0.332!!

#across the board the relationship of independent variables with test scores appear to be directional consistent, except for the gender variable!
#it appears that males are more likely to do well on the math test and more likely to perform poorly on writing and reading tests (which are very correlated with one another, as we previously saw)
#given this, we will also try to see if we can predict a student's gender simply by looking at the test scores
performance$gender2 = (performance$gender == "male")*1
train = performance[trainindex,]
validation = performance[-trainindex,]

gender_reg = lm (formula = gender2 ~ math.score + reading.score + writing.score, data = train)
summary(gender_reg)
#R2 is still low but, better than other models

########check accuracy of linear regression predictions################################
#starting with gender prediction
gendervalid_pred = predict(gender_reg, new = validation)
gendervalidRSS = sum((validation$gender2 - gendervalid_pred)^2)
gendervalidRMS = sqrt(gendervalidRSS/nrow(validation))
gendervalidRMS #Answer 0.32 -> very high degree of accuracy!

#math test score prediction accuracy
mathvalid_pred = predict(math_reg, new = validation)
mathvalidRSS = sum((validation$math.score - mathvalid_pred)^2)
mathvalidRMS = sqrt(mathvalidRSS/nrow(validation))
mathvalidRMS #Answer 13.19

#writing test score prediction accuracy
writingvalid_pred = predict(writing_reg, new = validation)
writingvalidRSS = sum((validation$writing.score - writingvalid_pred)^2)
writingvalidRMS = sqrt(writingvalidRSS/nrow(validation))
writingvalidRMS #Answer 12.33

#reading test score prediction accuracy
readingvalid_pred = predict(reading_reg, new = validation)
readingvalidRSS = sum((validation$reading.score - readingvalid_pred)^2)
readingvalidRMS = sqrt(readingvalidRSS/nrow(validation))
readingvalidRMS #Answer 13.03


# library(leaps)
# reg_bestset = regsubsets(math.score ~ . -reading.score - writing.score, data = train, nvmax = 12, method = "exhaustive" )
# bestset = summary(reg_bestset)
# dev.new()
# bestset

#trying an alternative model with trees
library(tree)
train = subset(train,select = -c(gender2))
names(train)

########regression tree to predict math, reading and writing scores##################
########regression tree for math score prediction
mathtree = tree(math.score ~ . -reading.score - writing.score, data = train)
dev.new()
plot(mathtree, type = "uniform")
text(mathtree, pretty = 0)

#Using the cv.tree() function to decide how many terminal leaf nodes to prune off.
cv = cv.tree(mathtree)
num_nodes = cv$size
RMS_error = cv$dev
dev.new()
plot(num_nodes, RMS_error)
title('Cross validated RMS error as function of #terminal nodes')
#optimal number of nodes = 6

prunedmathtree = prune.tree(mathtree, best = 6)
dev.new()
plot(prunedmathtree, type = "uniform")
text(prunedmathtree, pretty = 0)

#found this rpart & rpart.plot packages. They plot fancy trees. Same results as the normal pruned tree
#install using install.packages("rpart") and install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
mathtree2 = rpart(math.score ~ . -reading.score - writing.score, data = train, cp=.02)
dev.new()
rpart.plot(mathtree2,  box.palette = "RdBu", shadow.col = "gray", nn=TRUE)


########regression tree for reading score prediction###########################

readingtree = tree(reading.score ~ . -math.score - writing.score, data = train)
dev.new()
plot(readingtree, type = "uniform")
text(readingtree, pretty = 0)

#Using the cv.tree() function to decide how many terminal leaf nodes to prune off.
cv = cv.tree(readingtree)
num_nodes = cv$size
RMS_error = cv$dev
dev.new()
plot(num_nodes, RMS_error)
title('Cross validated RMS error as function of #terminal nodes')
#optimal number of nodes = 4

prunedreadingtree = prune.tree(readingtree, best = 4)
dev.new()
plot(prunedreadingtree, type = "uniform")
text(prunedreadingtree, pretty = 0)

#using the rpart package with fancy tree plot. Same results as the pruned tree
readingtree2 = rpart(reading.score ~ . -math.score - writing.score, data = train, cp=.02)
dev.new()
rpart.plot(readingtree2,  box.palette = "RdBu", shadow.col = "gray", nn=TRUE)


########regression tree for writing score prediction###########################
writingtree = tree(writing.score ~ . -math.score - reading.score, data = train)
dev.new()
plot(writingtree, type = "uniform")
text(writingtree, pretty = 0)

#Using the cv.tree() function to decide how many terminal leaf nodes to prune off.
cv = cv.tree(writingtree)
num_nodes = cv$size
RMS_error = cv$dev
dev.new()
plot(num_nodes, RMS_error)
title('Cross validated RMS error as function of #terminal nodes')
#optimal number of nodes = 7

prunedwritingtree = prune.tree(writingtree, best = 7)
dev.new()
plot(prunedwritingtree, type = "uniform")
text(prunedwritingtree, pretty = 0)

#using the rpart package with fancy tree plot. One node less than the pruned tree
writingtree2 = rpart(writing.score ~ . -math.score - reading.score, data = train, cp=.02)
dev.new()
rpart.plot(writingtree2,  box.palette = "RdBu", shadow.col = "gray", nn=TRUE)

########regression tree for gender prediction###########################
# gendertree = tree(gender ~ math.score + reading.score + writing.score, data = train)
# dev.new()
# plot(gendertree, type = "uniform")
# text(gendertree, pretty = 0)
#
# cv = cv.tree(gendertree)
# num_nodes = cv$size
# RMS_error = cv$dev
# dev.new()
# plot(num_nodes,RMS_error)
# best number of nodes is what?
# prunedgendertree = prune.tree(gendertree, best = 6)
#
# dev.new()
# plot(prunedgendertree, type = "uniform")
# text(prunedgendertree, pretty = 0)
#
# gendertree2 = rpart(gender ~ math.score + reading.score + writing.score, data = train, cp=.02)
# dev.new()
# rpart.plot(gendertree2,  box.palette = "RdBu", shadow.col = "gray", nn=TRUE)

########check accuracy of regression tree predictions###########################
#starting with the regression tree for math scores
mathtree_pred = predict(prunedmathtree, new = validation)
mathtree_RSS = sum( (validation$math.score - mathtree_pred)^2 )
mathtree_RMS = sqrt( mathRSS/nrow(validation) )
mathtree_RMS #Answer 14.267

#reading scores prediction accuracy using regression tree
readingtree_pred = predict(prunedreadingtree, new = validation)
readingtree_RSS = sum( (validation$reading.score - readingtree_pred)^2 )
readingtree_RMS = sqrt( readingtree_RSS/nrow(validation) )
readingtree_RMS #Answer 14.018

#writing scores prediction accuracy using regression tree
writingtree_pred = predict(prunedwritingtree, new = validation)
writingtree_RSS = sum( (validation$writing.score - writingtree_pred)^2 )
writingtree_RMS = sqrt( writingtree_RSS/nrow(validation) )
writingtree_RMS #Answer 13.101

# #gender prediction accuracy using regression tree
# gendertree_pred = predict(prunedgendertree, new = validation)
# gendertree_RSS = sum( (validation$gender2 - gendertree_pred)^2 )
# gendertree_RMS = sqrt( gendertree_RSS/nrow(validation) )
# gendertree_RMS #Answer 0.856

########comparison of RMS of linear regression with regression tree################
row_names = c("Math RMS", "Reading RMS", "Writing RMS")
reg_results = c(mathvalidRMS,readingvalidRMS,writingvalidRMS)
tree_results = c(mathtree_RMS,readingtree_RMS,writingtree_RMS)
RMSComparison = data.frame("Linear Reg" = c(reg_results), "Regression Tree" = c(tree_results), row.names = row_names)
RMSComparison
#Linear regression performs better than Regression Tree in all cases.

########interpretation of regression results############################
#math scores
summary(math_reg)
dev.new()
rpart.plot(mathtree2,  box.palette = "RdBu", shadow.col = "gray", nn=TRUE)
title("Regression Tree for Math Scores", cex.main = 1, line = 3)

#reading scores
summary(reading_reg)
dev.new()
rpart.plot(readingtree2,  box.palette = "RdBu", shadow.col = "gray", nn=TRUE)
title("Regression Tree for Reading Scores", cex.main = 1, line = 3)

#writing scores
summary(writing_reg)
dev.new()
rpart.plot(writingtree2,  box.palette = "RdBu", shadow.col = "gray", nn=TRUE)
title("Regression Tree for Writing Scores", cex.main = 1, line = 3)

# lunch is the most important independent variable for all test scores
# for all tests, students whose parents do not have complete college education are most likely to score the least
# females are more likely to do well reading and writing tests (given that they have received standard lunch)
# being from ethnic group D or E generally results in better performance than being from ethnic groups A, B, and C
# test scores can be used to predict the sex of a student to a degree of certainty