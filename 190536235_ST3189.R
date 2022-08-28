# Data pre-processing
ewcs <- read.csv('D:/SIM courses/ML/Project/EWCS_2016.csv', na.strings = -999) # convert outliers to NA values.
ewcs <- na.omit(ewcs) 
sum(is.na(ewcs))

library(skimr)
library(gridExtra)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(dplyr)
library(caret)
skim(ewcs)

# Question 1: Data visualization==========================
ewcs$Q2a <- as.factor(ewcs$Q2a)
levels(ewcs$Q2a)
levels(ewcs$Q2a) <- c('Male','Female') 
colnames(ewcs)[which(names(ewcs) == "Q2b")] <- "Age"  
colnames(ewcs)[which(names(ewcs) == "Q2a")] <- "Gender"

Mode <- function(Age) {
  x <- table(Age)
  as.numeric(names(x)[x == max(x)])
}

Age <- ewcs$Age
mode <- Mode(Age)

# Plot the distribution of Age and Gender.
hist_stats <- ewcs %>% 
  group_by(Gender) %>%
  summarize(whichstat = c("mean",
                          "mode", 
                          "median"),
            value = c(median = median(Age),
                      mean = mean(Age),
                      mode = Mode(Age))) 
hist_stats$value <- round(hist_stats$value,0)

my_hist <- ggplot(ewcs, aes(x = Age, fill = Gender, colour = Gender)) + 
  geom_histogram(aes(y=..density..),binwidth = 1,alpha=0.2, position="identity") + geom_density(alpha= 0.3) + 
  geom_vline(data=hist_stats,aes(xintercept = value,
                                 linetype = whichstat,
                                 color = Gender),size=1, alpha = 2) +
  labs(title='Density plot of Age among Gender', x='Age', y='Density')

my_hist

# Plot the distribution of overall questions from Q80 and Q90.
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"

ewcs <- ewcs %>%
  mutate_at(vars(Q87a, Q87b, Q87c, Q87d, Q87e, Q90a, Q90b, Q90c, Q90f), factor)

Q87 <- melt(ewcs, id.vars = c('Age', 'Gender', 'Q90a', 'Q90b','Q90c','Q90f'))
Q87 <- Q87 %>% select(variable, value) %>% group_by(variable, value) %>% summarize(count = n()) %>% mutate(pct = round(count/sum(count),2))  
colnames(Q87)[which(names(Q87) == "value")] <- "responses"

Q87_plt <- ggplot(Q87) +
  aes(x=variable, y = pct, fill = responses, width = 0.7) +
  geom_col(alpha = 0.7) +
  geom_text(aes(label = scales::percent(pct)), position= position_stack(0.8)) + coord_flip() +
  ggtitle("% of Responses among Question 87") + 
  xlab("Q87") + 
  ylab("% of responses")
Q87_plt

Q90 <-  melt(ewcs, id.vars = c('Age', 'Gender', 'Q87a', 'Q87b','Q87c','Q87d','Q87e'))
Q90 <- Q90 %>% select(variable, value) %>% group_by(variable, value) %>% summarize(count = n()) %>% mutate(pct = round(count/sum(count),2))  
colnames(Q90)[which(names(Q90) == "value")] <- "responses"

Q90_plt <- ggplot(Q90) +
  aes(x=variable, y = pct, fill = responses, width = 0.7) +
  geom_col(alpha = 0.7) +
  geom_text(aes(label = scales::percent(pct)), position= position_stack(0.8), hjust=0.6) + coord_flip() +
  ggtitle("% of Responses among Question 90") + 
  xlab("Q90") + 
  ylab("% of responses")
Q90_plt


grid.arrange(Q87_plt, Q90_plt,nrow = 2)


# PCA1 analysis:
library(ISLR)
library(tidyverse)  
library(cluster)    
library(factoextra) 
set.seed(2)
str(ewcs)

for (i in 1:ncol(ewcs)){
  if(is.factor(ewcs[,i])){
    ewcs[,i]=as.numeric(ewcs[,i])
  }
}

pca.out = prcomp(ewcs, scale=TRUE)
pca.out
summary(pca.out)
# First 2 principal components captures at least 0.5285 = 52.8% of variance.
# First 3 principal components captures at least 0.6213 - 62.13% of variance
# First 4 principal components captures at least 0.7068 - 70.68% of variance

pca.out$rotation 
pca.out$x=-pca.out$x 
pca.out
biplot(pca.out, scale=0)

pca.out$sdev 
pca.var=pca.out$sdev^2
pca.var
pca.var.df <- get_pca_var(pca.out)

pve=pca.var/sum(pca.var) 
pve
pve_df <- data.frame(pve, row.names = colnames(pca.out$rotation[,1:11]))
pve_df <- round(pve_df,3)*100
pve_df <- transform(pve_df, cumFreq = cumsum(pve))
pve_df 

# Plot PVE and Cumulative PVE.
par(mfrow=c(1,2)) 
plot(pve2, xlab="Principal Component", ylab="Proportion of Variance SExplained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
par(mfrow=c(1,1))

pca_plot <- fviz_eig(pca.out, addlabels = TRUE, main = "Scree plot of PCA1", ylim=c(0,60), alpha = 0.3, barfill = '#00AFBB', linecolor ='#FC4E07', barcolor = '#2E9FDF')
pca_plot


# PCA2 analysis 
# Dimension reduction on less impact contribution variables: age and gender, that is not highly correlated in PC1 and PC2.
ewcs2 <- ewcs[,3:11] 
set.seed(2)
str(ewcs2)

for (i in 1:ncol(ewcs2)){
  if(is.factor(ewcs2[,i])){
    ewcs2[,i]=as.numeric(ewcs2[,i])
  }
}


pca.out2 = prcomp(ewcs2, scale=TRUE)
summary(pca.out2)
# First 2 principal components captures at least 0.6397 = 63.97% of variance.
# First 3 principal components captures at least 0.7266 = 72.66% of variance
# First 4 principal components captures at least 0.7891 - 78.91% of variance

pca.out2$rotation 
pca.out2$rotation=-pca.out2$rotation 
pca.out2$x=-pca.out2$x
pca.out2
biplot(pca.out2, scale=0)
fviz_pca_biplot(pca.out2, linecolor = 'RED')
fviz_pca_var(pca.out2, color.var='black')

pca.out2$sdev 
pca.var2=pca.out2$sdev^2
pca.var2
pca.var2.df <- get_pca_var(pca.out2)

# Correlation of PCA clusters, its PCA value and its variables individual variables plot.
library("corrplot")
par(mfrow=c(1,2))
corrplot(pca.out$rotation, is.corr=FALSE)
corrplot(pca.out2$rotation, is.corr=FALSE)
par(mfrow = c(1, 1))

pve2=pca.var2/sum(pca.var2) 
pve2
pve_df2 <- data.frame(pve2, row.names = colnames(pca.out2$rotation[,1:9]))
pve_df2 <- round(pve_df2,3)*100
pve_df2 <- transform(pve_df2, cumFreq = cumsum(pve2))
pve_df2 

# Plot proportion variance explained (PVE) explained by each component as well as cumulative PVE.
par(mfrow=c(1,2)) 
plot(pve2, xlab="Principal Component", ylab="Proportion of Variance SExplained", ylim=c(0,1),type='b')
plot(cumsum(pve2), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
par(mfrow=c(1,1))

pca2_plot <- fviz_eig(pca.out2, addlabels = TRUE, main = "Scree plot of PCA2", ylim=c(0,60), alpha = 0.3, barfill = '#00AFBB', linecolor ='#FC4E07', barcolor = '#2E9FDF')
pca2_plot

library(gridExtra)
grid.arrange(pca_plot, pca2_plot, nrow = 2)

# Based according to graph, cumulative PVE till PC2 explains at least 64% of the data.
# After PC2 explains further slight increase which is not as important in terms of overall impact as it doesnt contribute much to the explanation of data.


# K-means clustering
set.seed(2020)

cluster_scree <- fviz_nbclust(scale(ewcs2), kmeans, nstart=50, method = "wss") +
  geom_vline(xintercept = 2, linetype = 1)
cluster_silhouette <- fviz_nbclust(scale(ewcs2), kmeans, nstart=100, method = "silhouette")

grid.arrange(cluster_scree, cluster_silhouette,nrow = 2)
# Optimal k=2 means clustering.

x <- scale(ewcs2) 
k2 <- kmeans(x, centers=2)
summary(k2)
k2$tot.withinss
k2$size
k2
k2$cluster

pca2_biplot_cluster <- plot(fviz_pca_biplot(pca.out2, scale = TRUE, linecolor = 'RED', habillage=as.factor(k2$cluster), addEllipses = TRUE, ellipse.type='norm', repel = TRUE)) + 
  ggtitle("PCA2 where k = 2 Biplot")
pca2_biplot_cluster

library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(pca.out2$x, col=k2$cluster, pch=16)

k2means_df <- data.frame(cluster = k2$cluster, ewcs2)

str(k2means_df)

k2means_df <- melt(k2means_df, id.vars = 'cluster')

for (i in 1:ncol(k2means_df)){
  if(is.numeric(k2means_df[,i])){
    k2means_df[,i]=as.factor(k2means_df[,i])
  }
}

k2means_df <- k2means_df %>%  select(cluster, variable, value) %>% group_by(cluster, variable,value) %>% summarize(count = n()) %>% mutate(pct = round(count/sum(count),2))
k2means_df <- data.frame(k2means_df)

k2means_plot <- ggplot(k2means_df) +
  aes(x= variable, y=pct, fill=value, width=0.7) + 
  geom_col(alpha=0.7) +
  ggtitle('Count of Clusters 1 and 2 among variables and its values') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = scales::percent(pct)), position= position_stack(0.8), vjust=0.6) + coord_flip() +
  facet_grid(~cluster)
k2means_plot


# Hierarchical Clustering 
ewcs3 <- scale(ewcs2)

par(mfrow=c(1,2))
hc.average = hclust(dist(ewcs3), method ="average")
plot(hc.average , main ="Average Linkage", xlab="", sub ="", cex =.9)
abline (h = 8, col = " red")# K=2 clusters cut-off height
abline (h = 7.05, col = " blue") # K=3 clusters cut-off height
sum(cutree(hc.average , 2)==2) 
## Average linkage fails to provide sufficient sample size (29 cases) for one cluster as can't analyze 2 cases.

hc.complete = hclust(dist(ewcs3), method ="complete")
plot(hc.complete , main ="Complete Linkage", xlab="", sub ="", cex =.9)
abline (h = 13, col = " red")# K=2 clusters cut-off height
abline (h = 11.8, col = " blue") # K=3 clusters cut-off height
sum(cutree(hc.complete , 2)==2) 

## Complete linkage able to provide sufficient sample size (760 cases) for one cluster as can't analyze 2 cases.
par(mfrow=c(1,1))


hc.cluster_df <- data.frame(ewcs2, cutree(hc.complete, 2))
colnames(hc.cluster_df)[which(names(hc.cluster_df) == 'cutree.hc.complete..2.' )] <- 'cluster'
#hc.cluster2 <- subset(k2means_df2, cutree(hc.complete, 2)==2)

hc.cluster_df <- melt(hc.cluster_df, id.vars = 'cluster')

for (i in 1:ncol(hc.cluster_df)){
  if(is.numeric(hc.cluster_df[,i])){
    hc.cluster_df[,i]=as.factor(hc.cluster_df[,i])
  }
}

hc.cluster_df <- hc.cluster_df  %>%  select(cluster, variable, value) %>%  group_by(cluster,variable,value) %>% summarize(count = n())  %>% mutate(pct = round(count/sum(count),2))

hc_plot <- ggplot(hc.cluster_df) +
  aes(x= variable, y=pct, fill=value, width=0.7) + 
  geom_col(alpha=0.7) +
  ggtitle('Count of Complete linkage of Hierarchy Clusters 1 and 2 among variables and its values') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = scales::percent(pct)), position= position_stack(0.8), vjust=0.6) + coord_flip() +
  facet_grid(~cluster)
hc_plot
# Hierachy complete linkage generate similar cluster to k-means clustering, except for Q90f and Q90e.
# Cluster 1 has more observations in responses 1-2 while Cluster2 has more responses in 3-5 except Q90f and Q90c.



# Question 2 Regression predicting G3 (final grade) in absence of G1 and G2 for both math and portuguese.=============================
library(car)
math_grade <- read.csv("D:/SIM courses/ML/Project/student-mat.csv", sep=";", header=TRUE, row.name=NULL)
portu_grade <- read.csv("D:/SIM courses/ML/Project/student-por.csv", sep=";", header=TRUE, row.name=NULL)
sum(is.na(portu_grade))

drop_cols <- c('G1','G2')
math_grade <- math_grade[,!(names(math_grade) %in% drop_cols)] # scale if needed.
portu_grade <- portu_grade[,!(names(portu_grade) %in% drop_cols)]

# Convert character data type to factors.
for (i in 1:ncol(math_grade)){
  if(is.character(math_grade[,i])){
    math_grade[,i]=factor(math_grade[,i])
  }
}

for (i in 1:ncol(portu_grade)){
  if(is.character(portu_grade[,i])){
    portu_grade[,i]=factor(portu_grade[,i])
  }
}

# Observing the full model without alteration.
set.seed(2020)
m1_math <- lm(G3 ~ . , data = math_grade)
summary(m1_math) 

m1_portu <- lm(G3 ~ . , data = portu_grade)
summary(m1_portu)

vif(m1_portu)# No multi-collinearity detected as GIF<5 or <10, and concludes that all are independent variables.
vif(m1_math)

# Model selection
m.math.null <- lm(G3 ~ 1, data = math_grade)
m.math.null

m.portu.null <- lm(G3 ~ 1, data = portu_grade)
m.portu.null


# backward elimination
m.math.full <- lm(G3 ~ . , data = math_grade)
m2.math <- step(m.math.full)
summary(m2.math) 
# lowest AIC = 1,127, inclusive of sex + age + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + romantic + freetime + goout + absences

m.portu.full <- lm(G3 ~ . , data = portu_grade)
m2.portu <- step(m.portu.full)
summary(m2.portu) 
# lowest AIC = 1,287, inclusive of school + sex + age + Medu + guardian + studytime + failures + schoolsup + higher + romantic + Dalc + health + absences

m3.math <- step(m.math.full, direction = 'both') 
# lowest AIC = 1,127, inclusive of sex + age + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + romantic + freetime + goout + absences
summary(m3.math)

m3.portu<- step(m.portu.full, direction = 'both') 
# lowest AIC = 1,287, inclusive of school + sex + age + Medu + guardian + studytime + failures + schoolsup + higher + romantic + Dalc + health + absences
summary(m3.portu)


# Final Model selection
library(leaps)
set.seed(123)

m4.math <- lm(G3 ~ sex + age + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + romantic + freetime + goout + absences, math_grade)
summary(m4.math)
vif(m4.math)

m4.portu <- lm(G3 ~ school + sex + age + Medu + guardian + studytime + failures + schoolsup + higher + romantic + Dalc + health + absences, portu_grade)
summary(m4.portu)
vif(m4.portu)

# Plot distribution for math and portuguese linear model.
par(mfrow = c(2,4))
plot(m4.math , main = 'Math regression model 4')
plot(m4.portu, main = 'Portuguese Regression model 4')
par(mfrow = c(1,1)) # 


# Train-Test split
library(glmnet)
library(caTools)
set.seed(200)
train.math <- sample.split(Y = math_grade[,31], SplitRatio = 0.7)
trainset.math <- subset(math_grade, train.math ==T)
testset.math <- subset(math_grade, train.math == F)

train.portu <- sample.split(Y = portu_grade[,31], SplitRatio = 0.7)
trainset.portu <- subset(portu_grade, train.portu ==T)
testset.portu <- subset(portu_grade, train.portu == F)

m4.math.train <- lm(G3 ~ sex + age + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + romantic + freetime + goout + absences, data = trainset.math)
summary(m4.math.train)
RMSE.m4.math.train <- round(sqrt(mean(residuals(m4.math.train)^2)),2) #RMSE.train = 3.91
RMSE.m4.math.train

predict.m4.math.test <- predict(m4.math.train, newdata = testset.math) # predict on train set model using 30% test set.
m4.math.testset.error <- testset.math$G3 - predict.m4.math.test 
RMSE.m4.math.test <- round(sqrt(mean(m4.math.testset.error^2)),2) # RMSE.test = 4.28
RMSE.m4.math.test

m4.portu.train <- lm(G3 ~ school + sex + age + Medu + guardian + studytime + failures + schoolsup + higher + romantic + Dalc + health + absences, data = trainset.portu)
summary(m4.portu.train)
RMSE.m4.portu.train <- round(sqrt(mean(residuals(m4.portu.train)^2)),2) #RMSE.train = 2.7
RMSE.m4.portu.train

predict.m4.portu.test <- predict(m4.portu.train, newdata = testset.portu) # predict on train set model using 30% test set.
m4.portu.testset.error <- testset.portu$G3 - predict.m4.portu.test 
RMSE.m4.portu.test <- round(sqrt(mean(m4.portu.testset.error^2)),2) # RMSE.test = 2.52
RMSE.m4.portu.test


# Ridge regression.
set.seed(400)
math_grade2 <- math_grade
portu_grade2 <- portu_grade



for (i in 1:ncol(math_grade2)){
  if(is.factor(math_grade2[,i])){
    math_grade2[,i]=as.numeric(math_grade2[,i])
  }
}


for (i in 1:ncol(portu_grade2)){
  if(is.factor(portu_grade2[,i])){
    portu_grade2[,i]=as.numeric(portu_grade2[,i])
  }
}

math_grade3 <- scale(as.matrix(math_grade2))
x.math <- math_grade3[, (1:30)]
y.math <- math_grade3[,31]

portu_grade3 <- scale(as.matrix(portu_grade2))
x.portu <- math_grade3[, (1:30)]
y.portu <- math_grade3[,31]


train.math <- sample.split(Y = math_grade3[,31], SplitRatio = 0.7)
trainset.math <- subset(math_grade3, train.math ==T)
testset.math <- subset(math_grade3, train.math == F)

train.portu <- sample.split(Y = portu_grade3[,31], SplitRatio = 0.7)
trainset.portu <- subset(portu_grade3, train.portu ==T)
testset.portu <- subset(portu_grade3, train.portu == F)

grid <- 10^seq(10,-2,length=100)
math.ridge <- glmnet(x = trainset.math[,c(1:30)], y = trainset.math[,31], alpha = 0, lambda = grid)

cv.out.math <- cv.glmnet(x = trainset.math[,c(1:30)], y = trainset.math[,31], alpha = 0)
plot(cv.out.math)
bestlam.math <- cv.out.math$lambda.min
bestlam.math # 0.659

math.ridge.best <- glmnet(x = trainset.math[,c(1:30)], y = trainset.math[,31], alpha = 0, lambda = bestlam.math ) 

math.ridge.pred.best <- predict(math.ridge.best, s = bestlam.math, newx = testset.math[,c(1:30)])
RMSE.math.ridge.test.best <- sqrt(mean((math.ridge.pred.best-testset.math[,31])^2)) # RMSE.test = 0.931
RMSE.math.ridge.test.best
math.ridge.coeff <- predict(math.ridge.best,type="coefficients",s=bestlam.math)
math.ridge.coeff


portu.ridge <- glmnet(x = trainset.portu[,c(1:30)], y = trainset.portu[,31], alpha = 0, lambda = grid)

cv.out.portu <- cv.glmnet(x = trainset.portu[,c(1:30)], y = trainset.portu[,31], alpha = 0)
plot(cv.out.portu)
bestlam.portu <- cv.out.portu$lambda.min
bestlam.portu # 0.385

portu.ridge.best <- glmnet(x = trainset.portu[,c(1:30)], y = trainset.portu[,31], alpha = 0, lambda = bestlam.portu) 

portu.ridge.pred.best <- predict(portu.ridge.best, s = bestlam.portu, newx = testset.portu[,c(1:30)])
RMSE.portu.ridge.test.best <- sqrt(mean((portu.ridge.pred.best-testset.portu[,31])^2)) # RMSE.test = 0.856
RMSE.portu.ridge.test.best
portu.ridge.coeff <- predict(portu.ridge.best,type="coefficients",s=bestlam.portu)
portu.ridge.coeff

levels(portu_grade$school)


# Lasso regression
set.seed(90)
math.lasso <- glmnet(x = trainset.math[, c(1:30)], y = trainset.math[,31], alpha = 1, lambda = grid) 
cv.out2.math <- cv.glmnet(x = trainset.math[, c(1:30)], y = trainset.math[,31], alpha = 1)
plot(cv.out2.math)
bestlam2.math <- cv.out2.math$lambda.min
bestlam2.math # 0.0328

math.lasso.best <- glmnet(x = trainset.math[, c(1:30)], y = trainset.math[,31], alpha = 1, lambda = bestlam2.math) 
math.lasso.pred.best <- predict(math.lasso.best, s = bestlam2.math, newx = testset.math[,c(1:30)])
RMSE.math.lasso.test.best <- sqrt(mean((math.lasso.pred.best-testset.math[,31])^2))# RMSE = 0.945

math.lasso.coef <- predict(math.lasso.best,type="coefficients",s=bestlam2.math)
math.lasso.coef
math.lasso.coef[math.lasso.coef!=0]


portu.lasso <- glmnet(x = trainset.portu[, c(1:30)], y = trainset.portu[,31], alpha = 1, lambda = grid) 
cv.out2.portu <- cv.glmnet(x = trainset.portu[, c(1:30)], y = trainset.portu[,31], alpha = 1)
plot(cv.out2.portu)
bestlam2.portu <- cv.out2.portu$lambda.min
bestlam2.portu # 0.0305

portu.lasso.best <- glmnet(x = trainset.portu[, c(1:30)], y = trainset.portu[,31], alpha = 1, lambda = bestlam2.portu) 
portu.lasso.pred.best <- predict(portu.lasso.best, s = bestlam2.portu, newx = testset.portu[,c(1:30)])
RMSE.portu.lasso.test.best <- sqrt(mean((portu.lasso.pred.best-testset.portu[,31])^2))# RMSE = 0.862

portu.lasso.coef <- predict(portu.lasso.best,type="coefficients",s=bestlam2.portu)
portu.lasso.coef
portu.lasso.coef[portu.lasso.coef!=0]


# Classification tree on regression.
library(rpart)
library(rpart.plot)			

set.seed(900)
options(digits = 3)

math_grade4 <- math_grade
portu_grade4 <- portu_grade

# Convert character data type to factors.
for (i in 1:ncol(math_grade4)){
  if(is.character(math_grade4[,i])){
    math_grade4[,i]=factor(math_grade4[,i])
  }
}

for (i in 1:ncol(portu_grade4)){
  if(is.character(portu_grade4[,i])){
    portu_grade4[,i]=factor(portu_grade4[,i])
  }
}

train.math2 <- sample.split(Y=math_grade4$G3, SplitRatio = 0.7)
trainset.math2 <- subset(math_grade4, train.math2==T)
testset.math2 <- subset(math_grade4, train.math2==F)

train.portu2 <- sample.split(Y=portu_grade4$G3, SplitRatio = 0.7)
trainset.portu2 <- subset(portu_grade4, train.portu2==T)
testset.portu2 <- subset(portu_grade4, train.portu2==F)

# CART optimal using 1SE rule
m4.math.cart <- rpart(G3 ~ ., method = "anova", cp = 0, data = trainset.math2)
m4.portu.cart <- rpart(G3 ~ ., method = "anova", cp = 0, data = trainset.portu2)
RMSE.math.train2 <- round(sqrt(mean((trainset.math2$G3 - predict(m4.math.cart))^2)))

RMSE.math.test2 <- round(sqrt(mean((testset.math2$G3 - predict(m4.math.cart, newdata = testset.math2))^2)))
RMSE.portu.train2 <- round(sqrt(mean((trainset.portu2$G3 - predict(m4.portu.cart))^2)))
RMSE.portu.test2 <- round(sqrt(mean((testset.portu2$G3 - predict(m4.portu.cart, newdata = testset.portu2))^2)))

# Compute min CVerror + 1SE in maximal CART.
cverror_math.cap <- m4.math.cart$cptable[which.min(m4.math.cart$cptable[,"xerror"]), "xerror"] + m4.math.cart$cptable[which.min(m4.math.cart$cptable[,"xerror"]), "xstd"]
cverror_portu.cap <- m4.portu.cart$cptable[which.min(m4.portu.cart$cptable[,"xerror"]), "xerror"] + m4.portu.cart$cptable[which.min(m4.portu.cart$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (m4.math.cart$cptable[i,j] > cverror_math.cap) {
  i <- i + 1
}

i <- 1; j<- 4
while (m4.portu.cart$cptable[i,j] > cverror_portu.cap) {
  i <- i + 1
}

cp.opt.math = ifelse(i > 1, sqrt(m4.math.cart$cptable[i,1] * m4.math.cart$cptable[i-1,1]), 1)
cp.opt.portu = ifelse(i > 1, sqrt(m4.portu.cart$cptable[i,1] * m4.portu.cart$cptable[i-1,1]), 1)

opt.math.cart<- prune(m4.math.cart, cp = cp.opt.math)
opt.portu.cart <- prune(m4.portu.cart, cp = cp.opt.portu)
opt.math.cart
opt.portu.cart

RMSE.math.cart.train <- c(RMSE.math.train2, round(sqrt(mean((trainset.math2$G3 - predict(opt.math.cart))^2)),2))
RMSE.math.cart.test <- c(RMSE.math.test2, round(sqrt(mean((testset.math2$G3 - predict(opt.math.cart, newdata = testset.math2))^2)),2))
round(mean((testset.math2$G3 - predict(opt.math.cart, newdata = testset.math2))^2),2)

RMSE.portu.cart.train <- c(RMSE.portu.train2, round(sqrt(mean((trainset.portu2$G3 - predict(opt.portu.cart))^2)),2))
RMSE.portu.cart.test <- c(RMSE.portu.test2, round(sqrt(mean((testset.portu2$G3 - predict(opt.portu.cart, newdata = testset.portu2))^2)),2))

par(mfrow=c(1,2))
rpart.plot(opt.math.cart, nn = T, main = "Optimal Tree in Math G3 (Final Grade)",type=4, extra = 101, digits=-1)
rpart.plot(opt.portu.cart, nn = T, main = "Optimal Tree in Portuguese G3 (Final Grade)",type=4, extra = 101, digits=-1)
par(mfrow=c(1,1))

opt.math.cart$variable.importance
opt.portu.cart$variable.importance


# Random Forest
library(randomForest)
library(tree)
library(MASS)
set.seed(50)
sum(is.na(math_grade4))
sum(is.na(portu_grade4))

math.test=math_grade4[-train.math,"G3"]
portu.test=portu_grade4[-train.portu,"G3"]

rf.math=randomForest(G3~.,data=math_grade4,subset=train.math,importance=TRUE) 

# OOB (test set) RMSE = 4.02
sqrt(rf.math$mse[rf.math$ntree])
plot(rf.math)

predict.rf.math = predict(rf.math,newdata=math_grade4[-train.math,])
RMSE_rf_math <- sqrt(mean((predict.rf.math-math.test)^2)) # RMSE RF math = 2.6
importance(rf.math)
varImpPlot(rf.math) 

rf.portu=randomForest(G3~.,data=portu_grade4,subset=train.portu,importance=TRUE) #

# OOB (test set) RMSE
sqrt(rf.portu$mse[rf.portu$ntree])
plot(rf.portu)

predict.rf.portu = predict(rf.portu,newdata=portu_grade4[-train.portu,])
RMSE_rf_portu <- sqrt(mean((predict.rf.portu -portu.test)^2)) # RMSE RF portu = 2.71
importance(rf.portu) 
varImpPlot(rf.portu) 


# Boosting
library(gbm)
set.seed(700)
sum(is.na(math_grade$G3))
sum(is.na(portu_grade$G3))
math_grade5 <- math_grade
portu_grade5 <- portu_grade

train.math <- sample.split(Y = math_grade5[,31], SplitRatio = 0.7)
trainset.math <- subset(math_grade5, train.math ==T)
testset.math <- subset(math_grade5, train.math == F)

train.portu <- sample.split(Y = portu_grade5[,31], SplitRatio = 0.7)
trainset.portu <- subset(portu_grade5, train.portu ==T)
testset.portu <- subset(portu_grade5, train.portu == F)

mgrade.test=math_grade5[-train.math,"G3"]
mgrade.train=math_grade5[train.math,"G3"]

pgrade.test=portu_grade5[-train.portu,"G3"]
pgrade.train=portu_grade5[train.portu,"G3"]

powers = seq(-10, -0.2, by = 0.1)
lambdas = 10^powers
length.lambdas = length(lambdas)

mgrade.train.errors = rep(NA, length.lambdas)
mgrade.test.errors = rep(NA, length.lambdas)
pgrade.train.errors = rep(NA, length.lambdas)
pgrade.test.errors = rep(NA, length.lambdas)


for (i in 1:length.lambdas) {
  boost.mgrade = gbm(G3 ~ ., data = trainset.math, distribution = "gaussian", 
                     n.trees = 1000, shrinkage = lambdas[i])
  mgrade.train.pred = predict(boost.mgrade, trainset.math, n.trees = 1000)
  mgrade.test.pred = predict(boost.mgrade, testset.math, n.trees = 1000)
  mgrade.train.errors[i] = sqrt(mean((mgrade.train.pred - mgrade.train)^2))
  mgrade.test.errors[i] = sqrt(mean((mgrade.test.pred - mgrade.test)^2))
}

for (i in 1:length.lambdas) {
  boost.pgrade = gbm(G3 ~ ., data = trainset.portu, distribution = "gaussian", 
                     n.trees = 1000, shrinkage = lambdas[i])
  pgrade.train.pred = predict(boost.pgrade, trainset.portu, n.trees = 1000)
  pgrade.test.pred = predict(boost.pgrade, testset.portu, n.trees = 1000)
  pgrade.train.errors[i] = sqrt(mean((pgrade.train.pred - pgrade.train)^2))
  pgrade.test.errors[i] = sqrt(mean((pgrade.test.pred - pgrade.test)^2))
}

min(mgrade.test.errors) # RMSE math boost train = 4.58
min(mgrade.train.errors) # RMSE math boost test = 3.36
lambdas[which.min(mgrade.test.errors)] # lambda = 0.0000000001

min(pgrade.test.errors) # RMSE portu boost train = 3.23
min(pgrade.train.errors) # RMSE portu boost test = 2.46
lambdas[which.min(pgrade.test.errors)] # lambda = 0.000126

# Plot train and test MSE against its penalty imposed.
par(mfrow=c(2,2))
plot(lambdas, mgrade.train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "blue", pch = 20)
plot(lambdas, mgrade.test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "red", pch = 20)

plot(lambdas, pgrade.train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "blue", pch = 20)
plot(lambdas, pgrade.test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "red", pch = 20)
par(mfrow=c(1,1))


boost.mgrade.best = gbm(G3 ~ ., data = trainset.math, distribution = "gaussian", 
                        n.trees = 1000, shrinkage = lambdas[which.min(mgrade.test.errors)])
summary(boost.mgrade.best)
mgrade.test.pred.best = predict(boost.mgrade.best, testset.math, n.trees = 1000)
boost.mgrade.MSE <- mean((mgrade.test.pred.best - mgrade.test)^2)
boost.mgrade.RMSE <- sqrt(mean((mgrade.test.pred.best - mgrade.test)^2)) # RMSE math boost = 4.58
boost.mgrade.RMSE

boost.pgrade.best = gbm(G3 ~ ., data = trainset.portu, distribution = "gaussian", 
                        n.trees = 1000, shrinkage = lambdas[which.min(pgrade.test.errors)])
summary(boost.pgrade.best)
pgrade.test.pred.best = predict(boost.pgrade.best, testset.portu, n.trees = 1000)
boost.pgrade.MSE <- mean((pgrade.test.pred.best - pgrade.test)^2)
boost.pgrade.RMSE <- sqrt(mean((pgrade.test.pred.best - pgrade.test)^2)) # RMSE portu boost = 3.23
boost.pgrade.RMSE


# Combine regression and classification RMSE plot:
math_reg_class <- data.frame(cbind(RMSE.m4.math.test,RMSE.math.ridge.test.best,RMSE.math.lasso.test.best,RMSE.math.cart.test[2] , RMSE_rf_math,boost.mgrade.RMSE))
names(math_reg_class)[4] <- 'RMSE.math.cart.test'
names(math_reg_class)[6] <- 'boost.math.grade.RMSE'
math_reg_class <- melt(math_reg_class)                        
math_reg_class <- math_reg_class %>% mutate(method = c("LinearRegression","RidgeRegression",'LassoRegression',"CART", 'RandomForest','Boosting'))


portu_reg_class <- data.frame(cbind(RMSE.m4.portu.test,RMSE.portu.ridge.test.best,RMSE.portu.lasso.test.best,RMSE.portu.cart.test[2] , RMSE_rf_portu,boost.pgrade.RMSE))                          
names(portu_reg_class)[4] <- 'RMSE.portu.cart.test'
names(portu_reg_class)[6] <- 'boost.portu.grade.RMSE'
portu_reg_class <- melt(portu_reg_class)     
portu_reg_class <- portu_reg_class %>% mutate(method = c("LinearRegression","RidgeRegression",'LassoRegression',"CART", 'RandomForest','Boosting'))

                                                                       
combined_reg_class <- data.frame(rbind(portu_reg_class, math_reg_class))
combined_reg_class <- combined_reg_class %>% mutate(module = case_when(grepl('math',variable) ~ 'G3 Math',
                                                                       grepl('portu',variable, ignore.case=TRUE) ~'G3 Portuguese'))
                                                                       

combined_reg_class_plot <- ggplot(combined_reg_class) + 
  geom_bar(aes(x=method, y=value, fill=module), 
           stat="identity",
           position = "dodge",
           alpha = 0.8) +
  geom_text_repel(aes(x=method, y=value, label=round(value,2)),nudge_y=0.5,
                  nudge_x=0.5, vjust=0.5,hjust=0.5) +
  ggtitle("Testset RMSE of ML methods for G3 Math and Portuguese Final Grade ") + coord_flip() +
  xlab("ML methods") + 
  ylab("Testset RMSE") +
  scale_y_continuous(breaks = seq(0,6, len=5)) +
  theme_light()
combined_reg_class_plot

portu.ridge.coeff_df <- data.frame(portu.ridge.coeff[,1])
colnames(portu.ridge.coeff_df)[which(names(portu.ridge.coeff_df) == "portu.ridge.coeff...1.")] <- "portu.ridge.coeff"

math.ridge.coeff_df <- data.frame(math.ridge.coeff[,1])
colnames(math.ridge.coeff_df)[which(names(math.ridge.coeff_df) == "math.ridge.coeff...1.")] <- "math.ridge.coeff"

par(mfrow=c(2,1))
plot(math.ridge.coeff,xlab = 'Variables' , ylab = 'Coefficients')
text(math.ridge.coeff,rownames(math.ridge.coeff),cex=1, pos=4, col="red")
title(main = 'Coefficient of Ridge Regression for G3 final grade Math')
plot(portu.ridge.coeff,xlab = 'Variables' , ylab = 'Coefficients')
text(portu.ridge.coeff,rownames(portu.ridge.coeff),cex=1, pos=4, col="red")
title(main = 'Coefficient of Ridge Regression for G3 final grade Portuguese')
par(mfrow=c(1,1)) 

# Final Ridge regression model highest coefficient variables
levels(portu_grade$school)
cor(math_grade2$failures, math_grade2$goout) # numeric values
cor(portu_grade2$failures, portu_grade2$school)

#Question 3: Classification task for Bank==============
# Logistic regression with stepwise elimination------------------ 
library(nnet)
library(ISLR)
library(dplyr)
library(car)
library(glmnet)
library(caTools)
library(tidyverse)
library(corrplot)
library(skimr)
library(caret)
set.seed(8000)
bank <- read.csv('D:/SIM courses/ML/Project/bank.csv',sep=";", header=TRUE, row.name=NULL) # convert outliers to NA values.
bank <- na.omit(bank) 
sum(is.na(bank))

skim(bank)

# Convert data type from character to factor.
for (i in 1:ncol(bank)){
  if(is.character(bank[,i])){
    bank[,i]=factor(bank[,i])
  }
}

bank$month <- factor(bank$month, levels=c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))
#bank$poutcome <-factor(bank$poutcome, levels=c('failure','unknown','other','success'))
#bank$y <-factor(bank$y, levels=c('yes','no'))
levels(bank$month)
levels(bank$poutcome)
levels(bank$y)

# Check its correlation
bank_corr <- bank

  
for (i in 1:ncol(bank_corr)){
  if(is.factor(bank_corr[,i])){
    bank_corr[,i]=as.numeric(bank_corr[,i])
  }
}

cor(bank_corr)
corrplot(cor(bank_corr),is.corr=TRUE, method="color")

levels(bank$y) # Baseline reference = No
summary(bank)

m1.bank <- glm(y~. , family = binomial,data = bank) # AIC = 2260 for full model.
summary(m1.bank) 

# Backward elimination
m.full.bank2 <- glm(y ~ .,family = binomial, data = bank)
m3.bank <- step(m.full.bank2)
summary(m3.bank) # lowest AIC = 2,250

# Stepwise elimination
m4.bank <- step(m.full.bank2, direction = 'both') # lowest AIC = 2,249.5
summary(m4.bank)

# Final logisteic regression model:
m5.bank <- glm(y ~ marital + education + housing + loan + contact + day + month + duration + campaign + poutcome, family = binomial,data = bank)
summary(m5.bank) # AIC = 2,249.5. 
vif(m5.bank) # No multi-collinearity detected.

# Validation train-test split.
set.seed(2)
train.bank <- sample.split(Y = bank$y, SplitRatio = 0.7)
trainset.bank <- subset(bank, train.bank == T)
testset.bank <- subset(bank, train.bank == F)

m6.bank <- glm(y ~ marital + education + housing + loan + contact + day + month + duration + campaign + poutcome, family = 'binomial',data = trainset.bank)
summary(m6.bank)

# Confusion Matrix on Trainset
threshold1 <- 0.5 # default 50/50 
threshold2 <- sum(bank$y == "yes")/length(bank$y) # emperical threshold 

prob.train.bank <- predict(m6.bank, type = 'response')
predict.train.bank <- ifelse(prob.train.bank > threshold1, "yes", "no")

# Overall Accuracy
confusionMatrix(as.factor(predict.train.bank), trainset.bank$y) # 90.58% overall accuracy if default threshold 0.5 is used. 84.6% overall accuracy if empirical threshold is used.

# Confusion Matrix on Testset
prob.test.bank <- predict(m6.bank, newdata = testset.bank, type = 'response')
predict.test.bank <- ifelse(prob.test.bank > threshold1, "yes", "no")

# Overall Accuracy
logistic_cm <-confusionMatrix(as.factor(predict.test.bank), testset.bank$y) # overall accuracy 90% if threshold 50% is used, 84.14% overall accuracy if empirical threshold is used.
logistic_cm
logistic_oacc <- c(logistic_cm$overall[1], logistic_cm$byClass[1:4])


library(ROCR)
roc.logistic.pred <- prediction(prob.test.bank, testset.bank$y)
roc.logistic.perf <- performance(roc.logistic.pred, 'tpr','fpr')
plot(roc.logistic.perf, colorize=TRUE, lwd=2)
auc = performance(roc.logistic.pred, 'auc')
slot(auc, 'y.values') # 88.5% AUC



# Discriminant Analysis with stepwise elimination
library(MASS)
library(caret)
set.seed(800)
lda.bank = lda(y ~ marital + education + housing + loan + contact + day + month + duration + campaign + poutcome,data = trainset.bank)
lda.train.pred = predict(lda.bank, trainset.bank)
lda.test.pred = predict(lda.bank, testset.bank[,-17])
lda.bank
levels(trainset.bank$month)
levels(trainset.bank$poutcome)
levels(trainset.bank$y)

#https://stackoverflow.com/questions/40087417/lda-interpretation
#http://www.talkstats.com/threads/discriminant-linear-analysis-coefficient-interpretation.39958/ 
# https://www.r-bloggers.com/2021/05/linear-discriminant-analysis-in-r/


plot(lda.bank$scaling)
text(lda.bank$scaling,rownames(lda.bank$scaling),cex=0.9, pos=3, col="red", offset=0.2)
title(main = 'Standardized Coefficient of LDA for Bank Subscription')

# Confusion matrix
confusionMatrix(lda.train.pred$class, trainset.bank$y)  # test set 90.1% of overall correct prediction.
lda_cm <- confusionMatrix(lda.test.pred$class, testset.bank$y)  # test set 90.4% of overall correct prediction.
lda_cm
lda_oacc <- c(lda_cm$overall[1], lda_cm$byClass[1:4])

# Plot ROC/AUC curve
roc.lda.pred <- prediction(lda.test.pred[3], testset.bank$y)
roc.lda.perf <- performance(roc.lda.pred, 'tpr','fpr')
plot(roc.lda.perf, colorize=TRUE, lwd=2)
auc = performance(roc.lda.pred, 'auc')
slot(auc, 'y.values') # 88.3% AUC.


# Ridge logistic regression
set.seed(300)
bank2 <- bank

# Convert factor data type to numeric values for scaling and matrix purposes.
for (i in 1:ncol(bank2)){
  if(is.factor(bank2[,i])){
    bank2[,i]=as.numeric(bank2[,i])
  }
}

bank2 <- scale(as.matrix(bank2))

train.bank2 <- sample.split(Y = bank2[,17], SplitRatio = 0.7)
trainset.bank2 <- subset(bank2, train.bank2 ==T)
testset.bank2 <- subset(bank2, train.bank2 == F)


grid <- 10^seq(10,-2,length=100)
bank.ridge <- glmnet(x = trainset.bank2[,c(1:16)], y = trainset.bank2[,17], alpha = 0,family = 'binomial', lambda = grid)

cv.ridge.bank <- cv.glmnet(x = trainset.bank2[,c(1:16)], y = trainset.bank2[,17], alpha = 0, family = 'binomial')
plot(cv.ridge.bank)
bestlam.bank <- cv.ridge.bank$lambda.min
bestlam.bank # 0.013
coef(cv.ridge.bank, cv.ridge.bank$lambda.min)

bank.ridge.best <- glmnet(x = trainset.bank2[,c(1:16)], y = trainset.bank2[,17], alpha = 0,family = "binomial", lambda = bestlam.bank) 

# Confusion Matrix on Trainset
threshold1 <- 0.5 # default 50/50 
threshold2 <- sum(bank[,17] == "yes")/length(bank[,17]) # emperical threshold 

prob.train.ridge.bank <- predict(bank.ridge.best, newx=trainset.bank2[,c(1:16)], type = 'response')
predict.train.ridge.bank <- ifelse(prob.train.ridge.bank > threshold1, "yes", "no")

# Overall Accuracy 
confusionMatrix(as.factor(predict.train.ridge.bank), trainset.bank$y) # 89.6% overall accuracy if default threshold 0.5 is used. 80.3% overall accuracy if empirical threshold is used.

# Confusion Matrix on Testset
prob.test.ridge.bank <- predict(bank.ridge.best, newx = testset.bank2[,c(1:16)], type = 'response')
predict.test.ridge.bank <- ifelse(prob.test.ridge.bank > threshold1, "yes", "no")

# Overall Accuracy
ridge_cm <- confusionMatrix(as.factor(predict.test.ridge.bank), testset.bank$y) # 86.21% overall accuracy if default threshold 0.5 is used,  79.8% overall accuracy if empirical threshold is used.
ridge_cm
ridge_oacc <- c(ridge_cm$overall[1],ridge_cm$byClass[1:4])


# Plot ROC/AUC curve
roc.ridge.pred <- prediction(prob.test.ridge.bank, testset.bank$y)
roc.ridge.perf <- performance(roc.ridge.pred, 'tpr','fpr')
plot(roc.ridge.perf, colorize=TRUE, lwd=2)
auc = performance(roc.ridge.pred, 'auc')
slot(auc, 'y.values') # 49.63% AUC


# Lasso logistic regression
set.seed(80)
grid <- 10^seq(10,-2,length=100)
bank.lasso <- glmnet(x = trainset.bank2[,c(1:16)], y = trainset.bank2[,17], alpha = 1,family = 'binomial', lambda = grid)

cv.lasso.bank2 <- cv.glmnet(x = trainset.bank2[,c(1:16)], y = trainset.bank2[,17], alpha = 1, family = 'binomial')
plot(cv.lasso.bank2)
bestlam.bank2 <- cv.lasso.bank2$lambda.min
bestlam.bank2 # 0.00314
coef(cv.lasso.bank2, cv.lasso.bank2$lambda.min)

bank.lasso.best <- glmnet(x = trainset.bank2[,c(1:16)], y = trainset.bank2[,17], alpha = 1,family = "binomial", lambda = bestlam.bank2) 

# Confusion Matrix on Trainset
threshold1 <- 0.5 # default 50/50 
threshold2 <- sum(bank[,17] == "yes")/length(bank[,17]) # empirical threshold 

prob.train.lasso.bank <- predict(bank.lasso.best, newx=trainset.bank2[,c(1:16)], type = 'response')
predict.train.lasso.bank <- ifelse(prob.train.lasso.bank > threshold1, "yes", "no")

# Overall Accuracy 
confusionMatrix(as.factor(predict.train.lasso.bank), trainset.bank$y) # 85.81% overall accuracy if default threshold 0.5 is used. 81.1% overall accuracy if empirical threshold is used.

# Confusion Matrix on Testset
prob.test.lasso.bank <- predict(bank.lasso.best, newx = testset.bank2[,c(1:16)], type = 'response')
predict.test.lasso.bank <- ifelse(prob.test.lasso.bank > threshold1, "yes", "no")
predict.test.lasso.bank
testset.bank2[,17]
# Overall Accuracy
lasso_cm <- confusionMatrix(as.factor(predict.test.lasso.bank), testset.bank$y) # 86% overall accuracy if default threshold 0.5 is used,  80.4% overall accuracy if empirical threshold is used.
lasso_cm
lasso_oacc <- c(lasso_cm$overall[1],lasso_cm$byClass[1:4])

# Plot ROC/AUC
roc.lasso.pred <- prediction(prob.test.lasso.bank, testset.bank$y)
roc.lasso.perf <- performance(roc.lasso.pred, 'tpr','fpr')
plot(roc.lasso.perf, colorize=TRUE, lwd=2)
auc = performance(roc.lasso.pred, 'auc')
slot(auc, 'y.values') # 49.74% AUC


# KNN analysis
library(class)
set.seed(90)

bank4 <- bank

for (i in 1:ncol(bank4)){
  if(is.factor(bank4[,i])){
    bank4 [,i]=as.numeric(bank4[,i])
  }
}

bank4$y <- factor(bank4$y)

train.bank4 <- sample.split(Y = bank4$y, SplitRatio = 0.7)
trainset.bank4 <- subset(bank4, train.bank4 ==T)
testset.bank4 <- subset(bank4, train.bank4 == F)

trainset.bank4[-17] = scale(trainset.bank4[-17])
testset.bank4[-17] = scale(testset.bank4[-17])
train.class = as.matrix(trainset.bank4$y) #class labels for the training observations
test.class = as.matrix(testset.bank4$y)

knn_classes <-numeric() #Holding variable
for(i in 1:200){
  #Apply knn with k = i
  knn.pred<-knn(trainset.bank4[-17], testset.bank4[-17],
                train.class,k=i)
  knn_classes <-c(knn_classes,
                  mean(knn.pred != test.class)) # test error rate is 10.7% where k=5 or k=16, best overall accuracy = 89.3%% 
}

plot(knn_classes,type="l",ylab="Error Rate",
     xlab="K",main="Error Rate for bank")

min(knn_classes)
knn_classes 

knn.pred.best<-knn(trainset.bank4[-17], testset.bank4[-17],
                   train.class,k=5, prob=TRUE)
prob.knn <- attr(knn.pred.best, "prob")
prob.knn

knn_cm <- confusionMatrix(knn.pred.best, testset.bank4$y) # overall accuracy 88.4%
knn_cm
knn_oacc <- c(knn_cm$overall[1], knn_cm$byClass[1:4])

# Plot ROC/AUC curve
roc.knn.pred <- prediction((1-prob.knn), testset.bank4$y)
roc.knn.perf <- performance(roc.knn.pred, 'tpr','fpr')
plot(roc.knn.perf, colorize=TRUE, lwd=2, main="ROC curve of KNN algorithm")
auc = performance(roc.knn.pred, 'auc')
slot(auc, 'y.values') # AUC for knn is 75.6%. 


# CART 
library(rpart)
library(rpart.plot)
set.seed(250) 
options(digits = 3) 

# Train-Test split on CART 
cart.bank <- rpart(y ~., data = trainset.bank, method = 'class', cp=0)
print(cart.bank)
printcp(cart.bank, digits = 3)
plotcp(cart.bank)

rpart.plot(cart.bank, nn = T, main = "Maximal Tree in m1")
printcp(cart.bank, digits = 3) 

cverror.bank.cap <- cart.bank$cptable[which.min(cart.bank$cptable[,"xerror"]), "xerror"] + cart.bank$cptable[which.min(cart.bank$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (cart.bank$cptable[i,j] > cverror.bank.cap) {
  i <- i + 1
}

cp.opt.bank = ifelse(i > 1, sqrt(cart.bank$cptable[i,1] * cart.bank$cptable[i-1,1]), 1)

# Prune to get 1SE optimal CART 
cart.bank.1SE <- prune(cart.bank, cp = cp.opt.bank)
cart.bank.1SE$variable.importance
# duration and outcome is the most important variable..

rpart.plot(cart.bank.1SE, nn = T, main = "Maximal Tree in optimal CART for bank",cex=0.7, fallen.leaves=T,tweak=0.9, digits=-2, Margin=-0.01,type=4, extra = 101) #, 
printcp(cart.bank.1SE, digits = 3)

cart.predicted.train.bank <- predict(cart.bank.1SE, newdata = trainset.bank, type='class')
cart.predicted.test.bank <- predict(cart.bank.1SE, newdata = testset.bank, type='class')
cart.predicted.test.bank2 <- predict(cart.bank.1SE, newdata = testset.bank[,-17], type='prob')

# Confusion Matrix for train and test set.
confusionMatrix(cart.predicted.train.bank, trainset.bank$y) # overall accuracy on trainset 88.5%.
cart_cm <- confusionMatrix(cart.predicted.test.bank, testset.bank$y)  # overall accuracy on testset 88.5%.
cart_cm
cart_oacc <- c(cart_cm$overall[1], cart_cm$byClass[1:4])

# Plot ROC/AUC curve
roc.cart.pred <- prediction(cart.predicted.test.bank2[,2], testset.bank$y)
roc.cart.perf <- performance(roc.cart.pred, 'tpr','fpr')
plot(roc.cart.perf, colorize=TRUE, lwd=2)
auc = performance(roc.cart.pred, 'auc')
slot(auc, 'y.values') # AUC = 50%


# Random Forest
library(randomForest)
library(magrittr)
library(tree)
set.seed(99)

bank.RF <- randomForest(y ~ . , data=trainset.bank, importance=TRUE, cv=10) 
bank.RF$err.rate 
levels(trainset.bank$month)

bank.RF # OOB (test error) is 9.95% 
bank.RF$predicted

# OOB (test set) RMSE = 31.5%
sqrt(bank.RF$err.rate[bank.RF$ntree])
plot(bank.RF) # stabilized ntree = 500


RF.bank.train.predict <- predict(bank.RF, newdata = trainset.bank)
RF.bank.test.predict <- predict(bank.RF, newdata = testset.bank)
RF.bank.test.predict2 <- predict(bank.RF, newdata = testset.bank[,-17], type='prob')
plot(RF.bank.test.predict)
# Confusion Matrix for train set.
confusionMatrix(RF.bank.train.predict, trainset.bank[,17]) # train data is 100% accurate, indicating that all values are classified correctly.

# Confusion Matrix for test set.
rf_cm <- confusionMatrix(RF.bank.test.predict, testset.bank[,17]) # test data accuracy is 90.4%, indicating majority of values are correctly classified.
rf_cm
rf_oacc <- c(rf_cm$overall[1],rf_cm$byClass[1:4])

# Gets Classification RF tree and variance importance variables.
getTree(bank.RF, 1, labelVar=TRUE)
importance(bank.RF)
varImpPlot(bank.RF, main = "Variance Importance of Random Forest model for Bank")

  
# Plot ROC/AUC
colors <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(testset.bank$y)

# For each class
for (i in 1:2) {
  # Define which observations belong to class[i]
  true_values <- ifelse(testset.bank[,17]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(RF.bank.test.predict2[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=colours(i)) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=colours(i),add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values) # AUC = 90.5%
}


roc_RF_pred <- prediction(RF.bank.test.predict2[,2], testset.bank$y)
roc_RF_perf <- performance(pred, 'tpr','fpr')
plot(roc_RF_perf, colorize=TRUE, lwd=2)
auc = performance(pred, 'auc')
slot(auc, 'y.values') # Balanced accuracy at 92.1%


# Boosting
library(gbm) 
set.seed(55)
bank4 <- bank
sum(is.na(bank))


train.bank3 <- sample.split(Y = bank[,17], SplitRatio = 0.7)
trainset.bank3 <- subset(bank, train.bank3 ==T)
testset.bank3 <- subset(bank, train.bank3 == F)

boost.bank <- gbm((unclass(y)-1)~., data = trainset.bank3, distribution = "bernoulli", n.trees = 200, interaction.depth = 4)
boost.predict.test.bank <- predict(boost.bank, newdata=testset.bank3, type='response')
boost.predict.test.bank

# Confusion Matrix on Trainset
threshold1 <- 0.5 # default 50/50 
threshold2 <- sum(bank[,17] == "yes")/length(bank[,17]) # empirical threshold 

# Confusion Matrix on Testset
boost.predict.test.bank2 <- ifelse(boost.predict.test.bank > threshold1, "yes", "no")

# Overall Accuracy
mean(boost.predict.test.bank2 == testset.bank3[,17]) # 90% overall accuracy if default threshold 0.5 is used,  84.6% overall accuracy if empirical threshold is used.

boost_cm <- confusionMatrix(as.factor(boost.predict.test.bank2), testset.bank3$y) # 90% accuracy that classifies correctly.
boost_cm
boost_oacc <-  c(boost_cm$overall[1],boost_cm$byClass[1:4])

roc_boost_pred <- prediction(boost.predict.test.bank, testset.bank3$y)
roc_boost_perf <- performance(roc_boost_pred, 'tpr','fpr')
plot(roc_boost_perf, colorize=TRUE, lwd=2)
auc = performance(roc_boost_pred, 'auc')
slot(auc, 'y.values') #  90.7% AUC


# Sector Vector Machine
library(ISLR)
library(e1071)
library(caret)
library(caTools)
set.seed(2)

bank4 <- bank

for (i in 1:ncol(bank4)){
  if(is.factor(bank4[,i])){
    bank4 [,i]=as.numeric(bank4[,i])
  }
}

bank4$y <- factor(bank4$y)

train.bank4 <- sample.split(Y = bank4$y, SplitRatio = 0.7)
trainset.bank4 <- subset(bank4, train.bank4 ==T)
testset.bank4 <- subset(bank4, train.bank4 == F)

trainset.bank4[-17] = scale(trainset.bank4[-17])
testset.bank4[-17] = scale(testset.bank4[-17])

svm.bank=svm(y~., data=trainset.bank4, type = 'C-classification',
             kernel = 'linear', cost=1, gamma=1)
plot(data=svm.bank, trainset.bank4)
summary(svm.bank)

svm.bank2=svm(y~., data=trainset.bank4, kernel="radial",gamma=1, cost=1e5)
summary(svm.bank2)
#plot(data=svm.bank,col=trainset.bank4)

set.seed(10)
tune.out <- tune(svm, y~., data=trainset.bank4, kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.5,1,2,3,4))) # lowesterror = 0.114068 where cost = 1 and gamma = 0.5

table(true=trainset.bank4[,17], pred=predict(tune.out$best.model, 
                                             newx=trainset.bank4))
summary(tune.out)

svm.bank3=svm(y~., data=trainset.bank4, kernel="radial",gamma=0.5, cost=0.1,  probability = TRUE)
summary(svm.bank3)
plot(data=svm.bank2,trainset.bank4[,17])

bank.predict.train.svm <- predict(svm.bank3, newdata = trainset.bank4[-17])
bank.predict.test.svm <- predict(svm.bank3, newdata = testset.bank4[-17]) 
bank.predict.test.svm2 <- predict(svm.bank3, newdata = testset.bank4[-17], probability = TRUE)
prob <- attr(bank.predict.test.svm2, 'probabilities')

roc_svm_pred <- prediction(prob[,2], testset.bank4[,17])
roc_svm_perf <- performance(roc_svm_pred, 'tpr','fpr')
plot(roc_svm_perf, colorize=TRUE, lwd=2)
auc = performance(roc_svm_pred, 'auc')
slot(auc, 'y.values') #  81.5% AUC


# Confusion matrix on test set
svm_cm <- confusionMatrix(testset.bank4[,17], bank.predict.test.svm2) # overall accuracy 89.5%
svm_cm
svm_oacc <- c(svm_cm$overall[1],svm_cm$byClass[1:4])

# Plot ROC/AUC curve
library(ROCR)
rocplot=function(pred, truth, ...){
  predob =prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

svmfit.opt=svm(y~., data=trainset.bank4, kernel="radial",gamma=0.5, cost=0.1,decision.values=T)
fitted=attributes(predict(svmfit.opt,trainset.bank4,
                          decision.values=TRUE))$decision.values
fitted.inverse = 1-fitted
par(mfrow=c(1,2))
rocplot(fitted.inverse,trainset.bank4[,17],main="Training Data")

svmfit.flex=svm(y~., data=trainset.bank4, kernel="radial",gamma=4, cost=0.1,decision.values=T)

fitted2=attributes(predict(svmfit.flex,trainset.bank4,
                           decision.values=TRUE))$decision.values
fitted.inverse2 = 1-fitted2
rocplot(fitted.inverse2,trainset.bank4[,17],add=T,col="red")

fitted3=attributes(predict(svmfit.opt,testset.bank4,
                           decision.values=TRUE))$decision.values
fitted.inverse3 = 1-fitted3
rocplot(fitted.inverse3,testset.bank4[,17],main="Test Data")

fitted4=attributes(predict(svmfit.flex,testset.bank4,
                           decision.values=TRUE))$decision.values
fitted.inverse4 = 1-fitted4
rocplot(fitted.inverse4,testset.bank4[,17],add=T,col="red")
par(mfrow=c(1,1))
fitted.inverse4


# Plot multiple ROC curve in one graph:
rocplot(fitted.inverse4,testset.bank4[,17], main='ROC/AUC curve of multiple ML algorithms',col="red",lty=1, lwd = 2)
rocplot(boost.predict.test.bank, testset.bank3$y, add=T,col="blue", lty=2, lwd = 2)
rocplot(RF.bank.test.predict2[,2], testset.bank$y, add=T,col="green", lty=4, lwd = 2)
rocplot(cart.predicted.test.bank2[,2], testset.bank$y, add=T,col="orange",lty=5, lwd = 2)
rocplot((1-prob.knn), testset.bank4$y, add=T,col="grey", lty=6, lwd = 2)
rocplot(prob.test.lasso.bank, testset.bank$y, add=T,col="purple", lty=3, lwd = 2)
rocplot(prob.test.ridge.bank, testset.bank$y, add=T,col="brown", lty=9, lwd = 2)
rocplot(lda.test.pred[3], testset.bank$y, add=T,col="black", lty=8, lwd = 2)
rocplot(prob.test.bank, testset.bank$y, add=T,col="gold", lty=7, lwd = 2)
legend("bottomright", c("SVM", "Boost", "RF","CART","KNN","Lasso","Ridge","LDA","Logistic Reg"), 
       col = c("red", "blue",'green','orange','grey','purple','brown','black','gold'), bty="n", lty=c(1,2,4,5,2,3,9,8,7), lwd=c(2,2,2,2,2,2,2,2))


# Plot Overall accuracy, TP (PPV), TN (NPV), Sensitivity and Specificity.
library(reshape2)
overall_class_acc <- cbind(logistic_oacc, lda_oacc,ridge_oacc,lasso_oacc,cart_oacc, knn_oacc,rf_oacc,boost_oacc,svm_oacc)
overall_class_acc
overall_class_acc_melt <- reshape2::melt(as.matrix(overall_class_acc))
overall_class_acc_melt <- data.frame(overall_class_acc_melt)

colnames(overall_class_acc_melt)[which(names(overall_class_acc_melt) == "Var1")] <- "Types_Accuracy"  
colnames(overall_class_acc_melt)[which(names(overall_class_acc_melt) == "Var2")] <- "ML"
colnames(overall_class_acc_melt)[which(names(overall_class_acc_melt) == "value")] <- "Accuracy"

overall_class_acc_plot <- ggplot(overall_class_acc_melt) + 
  geom_bar(aes(x=ML, y=Accuracy , fill=Types_Accuracy),
           stat="identity",
           position = "dodge",
           alpha = 0.8,
           width = 0.7) +
  geom_text(aes(x=ML, y=Accuracy, label=round(Accuracy,3)), position=position_dodge(width=0.8), vjust=0.1, hjust=0.6) + 
# geom_text_repel(aes(x=ML, y=Accuracy, label=round(Accuracy,4)),nudge_y=0.5,
#                  nudge_x=0.5, vjust=0.5,hjust=0.5) +
  ggtitle("Overall Accuracy of ML methods for modelling Bank subscription") +
  xlab("ML methods") + 
  ylab("Accuracy") +
  scale_y_continuous(breaks = seq(0,1, len=5)) +
  theme_light() + 
  facet_wrap(~Types_Accuracy) + coord_flip()
overall_class_acc_plot


# Final model comparison of varImplot between logistic and discriminant analysis.
#anova(m5.bank)
#varImp_lda = anova(m5.bank)[-1,2,drop=FALSE]
#varImp_lda = varImp_lda[order(varImp_lda[,1]),drop=FALSE,]
#varImp_lda[,1] = 100*varImp_lda[,1]/max(varImp_lda[,1])
#varImp_lda

#lda.bank$scaling
#lda.bank <- class(lda.bank)
#lda_coeff <- as.data.frame(lda.bank$scaling)
#anova(lda_coeff)
#varImp_da = anova(lda.coeff)[-1,2,drop=FALSE]
#varImp_da = varImp_da[order(varImp_da[,1]),drop=FALSE,]
#varImp_da[,1] = 100*varImp_da[,1]/max(varImp_da[,1])

# https://stats.stackexchange.com/questions/140055/using-results-of-canonical-discriminant-analysis-to-get-overall-variable-importa