# === AM project, Group 2 ===






# ========================
# === Libraries ===
# ========================

# main
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(corrplot)
library(stringr)
library(EnvStats)

# one hot
library(onehot)
library(e1071)

# pca
library(MASS)
library(rrcov)
library(DescTools)
library(quantable)

# outliers
library(chemometrics)

# cluster
library(cluster)
library(NbClust)
library(fpc)
library(jpeg)

# classification
library(caret)
library(MLmetrics)







# ========================
# === Functions ===
# ========================

# utility function
print.var <- function(var.name, var.val) {
  print(sprintf("%s = %g", var.name, var.val), quote=F)
}

plot.jpg <- function(path, add=FALSE) {
  jpg <- readJPEG(path, native=T) # read the file
  res <- dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg, 1, 1, res[1], res[2])
}






# ========================
# === Pre-processing ===
# ========================


df<-read.csv("AppleStore.csv")



# === Column pre-processing ==================================================

df$currency<-as.factor(df$currency)

# convert content rating to numerical
df$cont_rating <- as.character(df$cont_rating)
df$cont_rating <- as.numeric(substr(df$cont_rating, 1, nchar(df$cont_rating) - 1))

df$prime_genre<-as.factor(df$prime_genre)
df$vpp_lic<-as.factor(df$vpp_lic)
df$size_mb <- df$size_bytes/1024/1024
df$size_mb<-round(df$size_mb,digits = 0)

# replacing rating_count_ver with name_len
names(df)[names(df) == 'rating_count_ver'] <- 'name_len'
df$name_len <- str_length(df$track_name)

#drop irrelevant columns
df<-subset(df,select=-c(`X`,`id`,`size_bytes`,`currency`,
                        `track_name`,`ver`,`user_rating`,`rating_count_tot`))

head(df)

summary(df)

#number of variables
ncol(df)
#number of categorical variables
ncol(df %>% select_if(is.factor))
#number of numerical variables
ncol(select_if(df,is.numeric))

#column_na_count <-function (x) sapply(x, function(y) sum(is.na(y)))
#column_na_count(df)

#divide into categorical and continuous columns and rearrange columns
df <- df[, c(1,2,4,6:8,10,5,9,3)]

#changing user_rating column to binary (>4 good <4 bad)
df$user_rating_is_good <- as.factor(ifelse(df$user_rating_ver >=4, 1,0))

head(df)

# === Row pre-processing

row_na_count<-function (x) nrow(x)-nrow(x[complete.cases(x), ])
row_na_count(df)

# removing rows with less than 10 ratings
#df <- df[-which(df$rating_count_ver < 10),]
#df_removed<-df[which(df$rating_count_ver < 10),]
#table(df_removed$user_rating_ver)
#df_not_removed <- df[-which(df$rating_count_ver < 10),]
#table(df_not_removed$user_rating_is_good)
#table(df_not_removed$user_rating_ver)


# === Derived variables for faster coding =====================================

num.col.count <- 7
df_numeric <- df[,1:7]
df_numeric.w.rating <- df[,c(1:7,10)]
df_categorical <- df[,8:9]



# === Mean, sd, cov, cor, etc ================================================

mean<- colMeans(df_numeric.w.rating,na.rm=TRUE)
median <- apply(df_numeric.w.rating,2, median,na.rm=TRUE)
sd<-apply(df_numeric.w.rating,2,sd,na.rm=TRUE)

var<-var(df_numeric.w.rating, y = NULL, na.rm = FALSE, use="all.obs")

cov<-cov(df_numeric.w.rating, y = NULL, use = "all.obs",
         method = c("pearson", "kendall", "spearman"))

cor<-cor(df_numeric.w.rating, y = NULL, use = "all.obs",
         method = c("pearson", "kendall", "spearman"))

mean
median
sd

var
cov
cor

corrplot(cor,method="circle")







# ========================
# === Exploratory graphs ===
# ========================

#App category
qplot(x=fct_infreq(df$prime_genre), data = df) +
  labs (title="Frequncy of Apps based on the category",
        x="Categories",y="Count") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

#App content rating
qplot(df$cont_rating, data = df) +
  labs (title="Frequncy of Apps based on the content type",
        x="Content Type",y="Count") 

#User rating
qplot(x=user_rating_ver, data = subset(df,df$user_rating_ver>0),binwidth = 1) + 
  labs (title="Frequency of Apps based on the user rating",
        x="User Rating Version",y="Count")

#Other Correlation Plot GGPairs
ggpairs(df[,c('price','cont_rating',
              'size_mb','lang.num','sup_devices.num','user_rating_ver')]) + theme(axis.text.x=element_text(angle=90,hjust=1))

#Relation between the price and user rating
ggplot(aes(x=user_rating_ver,y=price), data = subset(df,price<50)) +
  geom_point() + xlim(c(1,5)) +
  labs (x="Average User Rating", y="Price",
        title="Not strong relation between price of an app and it's mean user rating.")

#Relation between the user rating and number of languages
ggplot(aes(x=user_rating_ver,y=lang.num), data = df) +
  stat_summary(geom = 'line', fun = "mean") +
  labs (x="Average User Rating", y="Number of language supported",
        title="The relation between number of languages supported 
        by an app and it's quality/user rating")


#relation between user rating and number of devices supported by an app
ggplot(aes(y=user_rating_ver,x=df$sup_devices.num), data = df) +
  scale_x_continuous(limits=c(36,47),breaks = seq(36,47,1)) +
  stat_summary(geom = 'line', fun = "mean") + 
  labs (y="Average User Rating", x="Number of devices supported",
        title="The relation between number of devices supported by an app and 
        it's quality/user rating")

#Other relations (relation between size and rating)
# The relation between app size and price or user_rating
#ggplot(aes(x=user_rating_ver,y=size_mb), data = subset(df,df$user_rating>0)) +
#  stat_bin(geom = 'bar') 
ggplot(df, aes(x = user_rating_ver, y = size_mb)) +
  geom_col()+
  labs(x="user_rating", y="size_mb")+xlim(-0.5,5.5)

#Bivariate Price/Category/User Rating
ggplot(aes(x=prime_genre,y=price), data = df) +
  geom_line(aes(color = factor(user_rating_ver))) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(y="Price",x="Categories")

#Bivariate size/price/user rating
ggplot(aes(x=size_mb,y=price), data = subset(df,df$price<25)) +
  geom_point(aes(color = factor(user_rating_ver))) + coord_flip() + labs (y="Price", x="Size (MB)" )






# ========================
# === Box-Cox transformation ===
# ========================




# === Testing for normality ============================================

par(mfrow=c(2,4))
par(oma=rep(0,4))
par(mar=rep(1,4))
for (i in 1:length(df_numeric)) {
  boxplot(df_numeric[,i], main=names(df_numeric[i]), type="l")
}
par(mfrow=c(1,1))



par(mfrow=c(3,3))
print("Shapiro-Wilk P-values (before transformation):")
for (i in 1:num.col.count) {
  sample <- df_numeric[, i]
  if (5000 < length(sample)) sample <- sample(df_numeric[, i], 5000)
  curr.p.val <- shapiro.test(sample)$p.value
  # none of the columns are normal
  print.var(colnames(df_numeric)[i], curr.p.val)
  hist(df_numeric[,i], main=names(df_numeric[i]))
}
par(mfrow=c(1,1))



# function to find best Box-Cox params
# returns different values due to randomness of sampling
find.box.cox.params <- function (x) {
  best.p <- 0
  best.lambda.1 <- 0
  best.lambda.2 <- 0
  best.t <- NULL
  x <- sample(x, 5000)
  for (lambda.1 in seq(-2, 2, .1)) {
    for (lambda.2 in seq(.1, 2, .1)) {
      t <- boxcoxTransform(x+lambda.2, lambda.1)
      p <- shapiro.test(t)$p.value
      if (best.p < p) {
        best.p <- p
        best.lambda.1 <- lambda.1
        best.lambda.2 <- lambda.2
        best.t <- t
      }
    }
  }
  hist(best.t)
  print(summary(best.t))
  print.var("best.p", best.p)
  print.var("best.lambda.1", best.lambda.1)
  print.var("best.lambda.2", best.lambda.2)
}



df.untransformed <- df
df_numeric.untransformed <- df_numeric

# based on several runs of the function, we apply the following transform:
df$price <- log(df$price + 1)
df$name_len <- log(df$name_len)
# content_rating won't change shape
df$sup_devices.num <- boxcoxTransform(df$sup_devices.num, 2)
# ipadSc won't change shape
df$lang.num <- log(df$lang.num + 1)
df$size_mb <- log(df$size_mb)



# resetting derived vars
df_numeric <- df[,1:7]
df_numeric.w.rating <- df[,c(1:7,10)]



par(mfrow=c(3,3))
print("Shapiro-Wilk P-values (after transformation):")
for (i in 1:num.col.count) {
  sample <- df_numeric[, i]
  if (5000 < length(sample)) sample <- sample(df_numeric[, i], 5000)
  curr.p.val <- shapiro.test(sample)$p.value
  # none of the columns are normal
  print.var(colnames(df_numeric)[i], curr.p.val)
  hist(df_numeric[,i], main=names(df_numeric[i]))
}
par(mfrow=c(1,1))






# ========================
# === PCA ===
# ========================


# === Classic PCA

pca.info <- princomp(df_numeric, cor=T)
round(pca.info$loadings, 3)
evs <- pca.info$sdev^2

# 3 pc's
sum(evs > 1)
# 6 pc's explain 90% of variability
cumsum(evs / sum(evs))
# 1 or 6 PCs
screeplot(pca.info, type="line")

# so let it be 6?
loadings.pca.classic <- pca.info$loadings[,1:6]
scores.pca.classic <- as.data.frame(pca.info$scores[, 1:6])



# === Robust PCA

# robust covariance will give error on these datsets, "system is computationally singular"
# df.and.mds <- as.data.frame(cbind(df_numeric, scores.mds))
# df.and.mds.scaled <- scale(df.and.mds)

# thus using only numeric variables

hubert.info <- PcaHubert(df_numeric, alpha = .75, scale = T, k=7)

round(hubert.info$loadings, 3)
evs <- hubert.info$eigenvalues
avg <- sum(evs) / length(evs)
# 4 pc's
sum(evs > avg)
# 5 pc's explain 80% of the robust variance
cumsum(evs / sum(evs))
# knee at 1 or 6
screeplot(hubert.info, type="line", ylim=c(0, max(hubert.info$eigenvalues)))

# keep 5?
# calling again but without the K param. The results are 99% same, but now with OD calculated
# due to randomness of sampling, let's save
fname <- 'computed_objs/hubert.info.rds'
if (file.exists(fname)) {
  hubert.info <- readRDS(fname)
} else {
  # it suggests 5 PCs
  hubert.info <- PcaHubert(df_numeric, alpha = .75, scale = T)
  saveRDS(hubert.info, fname)
}
round(hubert.info$loadings, 3)
loadings.pca.hubert <- hubert.info$loadings
scores.pca.hubert <- as.data.frame(hubert.info$scores)



# === Hubert with MAD scaling
# MAD is zero for 4 vars out of 7, so it's rather useless
robustscale(df_numeric)$data[1,]
unused.mad.pca <- function() {
  # it suggests 2 PCs
  mad.info <- PcaHubert(df_numeric, alpha = .75, scale = mad)
  evs <- mad.info$eigenvalues
  evs.avg <- mean(evs)
  # just one PC is higher than average
  sum(evs > evs.avg)
  loadings.pca.mad <- mad.info$loadings
  scores.pca.mad <- mad.info$scores
}



# ===  Manual scaling by the quantile
# winsorizing is preferrable
unused.trimmed.pca <- function() {
  outliers.manual <- df_numeric$price > quantile(df_numeric$price, .95) | # 124
                     df_numeric$rating_count_ver > quantile(df_numeric$rating_count_ver, .95) | # 217
                     df_numeric$size_mb > quantile(df_numeric$size_mb, .95) # 217
  # 518
  sum(outliers.manual)
  df_numeric.wo.manual.outliers <- df_numeric[!outliers.manual, ]
  df.wo.manual.outliers <- df[!outliers.manual, ]
  col.sds <- sapply(df_numeric.wo.manual.outliers, sd)
  col.means <- sapply(df_numeric.wo.manual.outliers, mean)
  df_numeric.ms <- scale(df_numeric, center = col.means, scale=col.sds)

  # suggests 4 PCs
  hubert.info.ms <- PcaHubert(df_numeric.ms, alpha = .75, scale = F)
  evs <- hubert.info.ms$eigenvalues
  # no knee
  screeplot(hubert.info.ms, type="line", ylim=c(0, max(evs)))
  loadings.pca.ms <- hubert.info.ms$loadings
  scores.pca.ms <- hubert.info.ms$scores
}


# another old method that we don't use no more
unused.rob.pca <- function() {
  df_numeric.scaled <- scale(df_numeric)
  cov.robust.info <- cov.mcd(df_numeric.scaled, cor=T, quantile.used=nrow(df_numeric) * .75)

  cov.robust <- cov.robust.info$cov
  cov.robust.eigen <- eigen(cov.robust)
  evs <- cov.robust.eigen$values

  # 1 eigenvalue is above average
  above.avg.count <- sum(evs > 1)
  # 3 pc's explain 87% of variance
  repr.80.count <- sum(cumsum(evs / sum(evs)) < .80) + 1
  # knee at 3
  plot(evs,
       type="line", lwd=5, las=1,
       xlab="Number of dimensions",
       ylab="Eigenvalues")

  # so let it be 3?
  loadings.pca.rob <- cov.robust.eigen$vectors[,1:3]
  center.mtx <- rep(1, nrow(df_numeric.scaled)) %*% t(cov.robust.info$center)
  scores.pca.rob <- as.data.frame(as.matrix(df_numeric.scaled - center.mtx) %*% loadings.pca.rob)
  colnames(scores.pca.rob) <- paste("pca.rob", colnames(scores.pca.rob), sep = "_")
}






# ========================
# === One-hot encoding ===
# ========================



#using multidimensional scaling (MDS) for categorical variables
#one hot encoding for Hamming distance
start.time1 <- Sys.time()
encoder <- onehot(df[c(8:9)], max_levels = 60)
df.onehot <- predict(encoder,df[c(8:9)])
end.time1 <- Sys.time()
#dist <- dist(data.onehot, method="hamming")

fname <- 'computed_objs/hamming.dist.8.9.rds'
if (file.exists(fname)) {
  dist <- readRDS(fname)
} else {
  dist <- hamming.distance(df.onehot)
  saveRDS(dist, fname)
}

fname <- 'computed_objs/mds.8.9.rds'
if (file.exists(fname)) {
  mds <- readRDS(fname)
} else {
  mds <- cmdscale(dist, k=20,eig=TRUE)
  saveRDS(mds, fname)
}

#==== dimensionality reduction

mds.eig.avg <- sum(mds$eig) / length(mds$eig)

# number of above-average eigenvalues: 23
above.avg.count <- sum(mds$eig > mds.eig.avg)

# number of eigenvalues that represent 80% of variability: 9
repr.80.count <- sum(cumsum(mds$eig / sum(mds$eig)) < .8) + 1

plot(mds$eig[1:30],
     type="line", lwd=5, las=1,
     xlab="Number of dimensions", 
     ylab="Eigenvalues")
# visually the knee is at 4
visual.knee.count <- 4
abline(v=c(repr.80.count, above.avg.count, visual.knee.count), untf = FALSE, col="blue")

# let's take 9 then
scores.mds <- as.data.frame(mds$points[,1:repr.80.count])
colnames(scores.mds) <- paste("mds", colnames(scores.mds), sep = "_")






# ========================
# === Outlier detection ===
# ========================


# classic PCA
pca.dist.info <- pcaDiagplot(df_numeric, pca.info, scale=T, a=ncol(scores.pca.classic), plot=F )
plot(pca.dist.info$SDist, pca.dist.info$ODist, pch=19)
abline(v=pca.dist.info$critSD, h=pca.dist.info$critOD)
# df_numeric[pca.dist.info$ODist > 8, ]
outliers.pca.classic <- pca.dist.info$SDist > pca.dist.info$critSD | pca.dist.info$ODist > pca.dist.info$critOD
# 741
sum(outliers.pca.classic)
# apps with few supported devices are outliers
table(df$sup_devices.num, outliers.pca.classic)

# robust PCA
# notice the horizontal line - those are apps with the low number of supported devices
plot(hubert.info, pch=19)
outliers.pca <- hubert.info$sd > hubert.info$cutoff.sd | hubert.info$od > hubert.info$cutoff.od
# 726 outliers with the default cutoff of .975
sum(outliers.pca)
# apps with few supported devices are outliers
table(df$sup_devices.num, outliers.pca)
# 552 in common with classic
sum(outliers.pca.classic & outliers.pca)

# MAD version
unused.mad.outliers <- function (){
  plot(mad.info, pch=19, label='none')
  outliers.pca.mad <- mad.info$sd > mad.info$cutoff.sd | mad.info$od > mad.info$cutoff.od
  # 1185
  sum(outliers.pca.mad)
  sum(outliers.pca & outliers.pca.mad)
}

# ms version
unused.trimmed.outliers <- function () {
  # notice a diagonal lines -- those are apps with high rating_count
  plot(hubert.info.ms, pch=19, label='none')
  df_numeric[hubert.info.ms$sd > 20 & hubert.info.ms$od > 140, ]
  df_numeric[hubert.info.ms$sd > 120, ]
  outliers.pca.ms <- hubert.info.ms$sd > hubert.info.ms$cutoff.sd | hubert.info.ms$od > hubert.info.ms$cutoff.od
  # 1227
  sum(outliers.pca.ms)
  # 480 - classic outliers are mostly contained within manually scaled
  sum(outliers.pca.classic & outliers.pca.ms)
}

# === univariate outliers

par(mfrow=c(3,3))
print('univariate outlier counts per column:')
outlier.counts.per.row <- rep(0, nrow(df_numeric))
for (i in 1:length(df_numeric)) {
  col_name <- names(df_numeric[i])
  curr_out <- boxplot(df_numeric[,i], main=col_name, type="l")$out
  print.var(col_name, length(curr_out))
  curr_out <- which(df_numeric[,i] %in% curr_out)
  outlier.counts.per.row[curr_out] <- outlier.counts.per.row[curr_out] + 1
}
par(mfrow=c(1,1))
table(outlier.counts.per.row)
outliers.univ <- outlier.counts.per.row >= 2
# 278 rows that are univariate outliers in 2 columns or more
sum(outliers.univ)



# === Removing outliers
# classic
df.wo.outliers.classic <- df[!outliers.pca.classic,]
df_numeric.wo.outliers.classic <- df_numeric[!outliers.pca.classic,]
scores.pca.classic.wo.outliers.classic <- scores.pca.classic[!outliers.pca.classic, ]

# robust
df.wo.outliers <- df[!outliers.pca,]
df.untransformed.wo.outliers <- df.untransformed[!outliers.pca,]
df_numeric.wo.outliers <- df_numeric[!outliers.pca,]
df_numeric.untransformed.wo.outliers <- df_numeric.untransformed[!outliers.pca,]
scores.pca.hubert.wo.outliers <- scores.pca.hubert[!outliers.pca,]
scores.mds.wo.outliers <- scores.mds[!outliers.pca,]

# univariate
df.wo.outliers.univ <- df[!outliers.univ,]
df_numeric.wo.outliers.univ <- df_numeric[!outliers.univ,]






# ========================
# === Variables for classification ===
# ========================

#No PCA and OneHot
df_noPCA_onehot<-as.data.frame(cbind(df[c(1:7)],df.onehot,df[11]))

#Classic PCA and OneHot
df_classicPCA_onehot<-as.data.frame(cbind(scores.pca.classic,df.onehot,df[11]))

#Classic PCA and MDS
df_classicPCA_mds<-as.data.frame(cbind(scores.pca.classic,scores.mds,df[11]))

#Robust PCA and OneHot
df_robustPCA_onehot<-as.data.frame(cbind(scores.pca.hubert,df.onehot,df[11]))

#Robust PCA and MDS
df_robustPCA_mds<-as.data.frame(cbind(scores.pca.hubert,scores.mds,df[11]))






# ========================
# === Classification ===
# ========================

#Choose one Dataset:
#df_used<-df_noPCA_onehot
df_used<-df_robustPCA_mds

## set the seed to make your partition reproducible
set.seed(123)  
#Cross-validation
## 70% of the sample size
smp_size <- floor(0.70 * nrow(df_used))
train_ind <- sample(seq_len(nrow(df_used)), size = smp_size)

train <- df_used[train_ind, ]
test <- df_used[-train_ind, ]
train.classes <- train$user_rating_is_good
test.classes <-test$user_rating_is_good

numb_class_0<-table(test.classes)[1]
numb_class_1<-table(test.classes)[2]

#Training control
train.control <- trainControl(method = "cv", number=10, savePredictions = "final")

#Dataframe to save classifiers predictions
classifier_scores<-data.frame()[1:dim(test)[1],]

#Classifiers

###Linear Discriminant Analysis
model_lda <- train(user_rating_is_good ~., 
                   data=train, method = "lda", trControl = train.control)

lda_pred <- predict(model_lda, test)
classifier_scores["lda"]<-predict(model_lda, test,type="prob")[,2]
#confusion matrix LDA
confusionMatrix(lda_pred, test.classes, positive='1')
avgF1score<-(F1_Score(lda_pred, test.classes, positive = '1')+F1_Score(lda_pred, test.classes, positive = '0'))/2
print(avgF1score)

###Regularized Discriminant Analysis 
model_rda <- train(user_rating_is_good ~., data=train, method = "rda", trControl = train.control)
rda_pred <- predict(model_rda, test)
classifier_scores["rda"]<-predict(model_rda, test,type="prob")[,2]
confusionMatrix(rda_pred, test.classes,positive = '1')
avgF1score<-(F1_Score(rda_pred, test.classes, positive = '1')+F1_Score(rda_pred, test.classes, positive = '0'))/2
print(avgF1score)


###Support Vector Machines
model_svm <- train(user_rating_is_good ~., data=train, method = "svmRadial", trControl = train.control)
svm_pred <- predict(model_svm, test)
#confusion matrix SVM
confusionMatrix(svm_pred, test.classes,positive = '1')
avgF1score<-(F1_Score(svm_pred, test.classes, positive = '1')+F1_Score(svm_pred, test.classes, positive = '0'))/2
print(avgF1score)

###Neural Networks
model_nn <- train(user_rating_is_good ~., data=train, method = "nnet"
                    , tuneGrid=expand.grid(size = 6,
                      decay = 0.1), trControl = train.control)
nn_pred <- predict(model_nn, test)
classifier_scores["nnet"]<-predict(model_nn, test,type="prob")[,2]
#confusion matrix NN
confusionMatrix(nn_pred, test.classes,positive='1')
avgF1score<-(F1_Score(nn_pred, test.classes, positive = '1')+F1_Score(nn_pred, test.classes, positive = '0'))/2
print(avgF1score)

###Random Forest
model_rf <- train(user_rating_is_good ~., data=train, method = "rf", trControl = train.control)
rf_pred <- predict(model_rf, test)
classifier_scores["rf"]<-predict(model_rf, test,type="prob")[,2]
#confusion matrix RF
confusionMatrix(rf_pred, test.classes,positive='1')
avgF1score<-(F1_Score(rf_pred, test.classes, positive = '1')+F1_Score(rf_pred, test.classes, positive = '0'))/2
print(avgF1score)


###K-Nearest Neighbour 
knnFit <- train(user_rating_is_good ~., data = train, method = "knn", trControl = train.control, preProcess = c("center","scale"),tuneLength = 20)
knnFit
plot(knnFit)
model_knn <- train(user_rating_is_good ~., data=train, method = "knn",tuneGrid=expand.grid(.k=11), trControl = train.control)
knn_pred <- predict(model_knn, test)
classifier_scores["knn"]<-predict(model_knn, test,type="prob")[,2]
#confusion matrix KNN
confusionMatrix(knn_pred, test.classes,positive='1')
avgF1score<-(F1_Score(knn_pred, test.classes, positive = '1')+F1_Score(knn_pred, test.classes, positive = '0'))/2
print(avgF1score)

#-------------------------------------------------------
#-------------Ensemble decisions------------------------
#-------------------------------------------------------
#-------------------------------------------------------

ensemble_decisions<-data.frame()[1:dim(classifier_scores)[1],]
#average
ensemble_decisions$avg<-rowSums(classifier_scores)/dim(classifier_scores)[2]
ensemble_decisions$avg<-as.factor(ifelse(ensemble_decisions$avg>0.5,'1','0'))
print(confusionMatrix(ensemble_decisions$avg, test.classes,positive = '1'))
print((F1_Score(ensemble_decisions$avg, test.classes, positive = '1')+F1_Score(ensemble_decisions$avg, test.classes, positive = '0'))/2)
#weighted average
ensemble_decisions$weighted_avg<-0.3*classifier_scores$nnet+0.3*classifier_scores$rf+0.13*classifier_scores$lda+0.13*classifier_scores$rda+0.13*classifier_scores$knn
ensemble_decisions$weighted_avg<-as.factor(ifelse(ensemble_decisions$weighted_avg>0.5,'1','0'))
print(confusionMatrix(ensemble_decisions$weighted_avg, test.classes,positive = '1'))
print((F1_Score(ensemble_decisions$weighted, test.classes, positive = '1')+F1_Score(ensemble_decisions$weighted, test.classes, positive = '0'))/2)
#majority voting
classifier_scores[]<-lapply(round(classifier_scores),factor)
ensemble_decisions$majority<-apply(classifier_scores,1,function(x) names(which.max(table(x))))
ensemble_decisions$majority<-as.factor(ensemble_decisions$majority)
print(confusionMatrix(ensemble_decisions$majority, test.classes,positive = '1'))
print((F1_Score(ensemble_decisions$majority, test.classes, positive = '1')+F1_Score(ensemble_decisions$majority, test.classes, positive = '0'))/2)








# ========================
# === Cluster analysis ===
# ========================




clust.to.pred <- function (clust, labels) {
  pred <- rep(-1, length(labels))
  for (k in unique(clust)) {
    good.count <- sum(clust == k & labels == 1)
    bad.count <- sum(clust == k & labels == 0)
    clust.label <- if (good.count < bad.count) 0 else 1
    pred[clust == k] <- clust.label
  }
  pred
}

calc.balanced.acc <- function (pred, labels) {
  conf.mtx <- confusionMatrix(factor(pred, c(0, 1)), factor(labels, c(0, 1)))
  print.var("balanced accuracy", conf.mtx$byClass[['Balanced Accuracy']])
}



# === Options without outliers
curr.df.descr <- 'Numerical and categorical vars, without robust outliers'
curr.df.name <- 'num.wc.wo'
curr.df <- df.wo.outliers[,1:9]
curr.df.for.pairs <- df_numeric.untransformed.wo.outliers
curr.labels <- df.wo.outliers$user_rating_is_good
curr.metric <- "gower"
# pairwise distnces dominated by prime_genre and ipadSc
curr.linkage <- "single"
# ba .5, k 2, silh .33, ch 3.2, one-vs-all
# plot dominated by outliers and prime_genre
curr.linkage <- "complete"
# ba .5, k 2, silh .32, ch 58, 29-vs-all
# with k = 3: ba .62, silh .29, ch 58
# PAMK: ba .6, k 2, silh .34, ch 2524


unused.options <- function() {
  curr.df.descr <- 'Scaled numerical vars, without robust outliers'
  curr.df.name <- 'num.wo'
  curr.df <- as.data.frame(scale(df_numeric.wo.outliers))
  curr.df.for.pairs <- df_numeric.untransformed.wo.outliers
  curr.labels <- df.wo.outliers$user_rating_is_good
  curr.metric <- "euclidean"
  curr.linkage <- "complete"
  # ba .5, k 3, silh .7, ch 562
  # PAMK: ba .6, k 8, silh .17, ch 977



  curr.df.descr <- 'Robust PCA with categorical vars, without outliers'
  curr.df.name <- 'pca.rob.wc.wo'
  curr.df <- cbind(scores.pca.hubert.wo.outliers, df.wo.outliers$prime_genre, df.wo.outliers$vpp_lic)
  curr.df.for.pairs <- df_numeric.untransformed.wo.outliers
  curr.labels <- df.wo.outliers$user_rating_is_good
  curr.metric <- "gower"
  # pairwise distances dominated by prime_genre
  curr.linkage <- "complete"
  # same as numerical
  curr.linkage <- "average"
  # same as numerical
  # plot: dominated by outliers and prime_genre
  curr.linkage <- "ward"
  # ba .5, k 2, silh .06, ch 393
  # plot: 2 large groups
  # PAMK: ba .59, k 4, silh .03, ch 353



  curr.df.descr <- 'Robust PCA with MDS, without outliers'
  curr.df.name <- 'pca.rob.wmds.wo'
  curr.df <- cbind(scores.pca.hubert.wo.outliers, scores.mds.wo.outliers)
  curr.df.for.pairs <- df_numeric.untransformed.wo.outliers
  curr.labels <- df.wo.outliers$user_rating_is_good
  curr.metric <- "euclidean"
  # pairwise distances ok
  curr.linkage <- 'complete'
  # ba .59, k 10, silh .1, ch 488
  # PAMK: ba .63, k 4, silh .18, ch 1132



  curr.df.descr <- 'Robust PCA without categorical vars, without outliers'
  curr.df.name <- 'pca.rob.wo'
  curr.df <- scores.pca.hubert.wo.outliers
  curr.df.for.pairs <- df_numeric.untransformed.wo.outliers
  curr.labels <- df.wo.outliers$user_rating_is_good
  curr.metric <- "euclidean"
  # pairwise distances ok
  curr.linkage <- 'complete'
  # ba .6, k 4, silh .13, ch 969
  # PAMK: ba .61, k 10, silh .24, ch 1354



  # === Options with outliers
  curr.df.descr <- 'Robust PCA with categorical vars, with outliers'
  curr.df.name <- 'pca.rob.wc'
  curr.df <- cbind(scores.pca.hubert, df$prime_genre, df$vpp_lic)
  curr.df.for.pairs <- df_numeric.untransformed
  curr.labels <- df$user_rating_is_good
  curr.metric <- "gower"
  # pairwise distances dominated by prime_genre
  curr.linkage <- 'complete'
  # ba .59, k 10, silh .41, ch 794
  # PAMK: ba .58, k 4, silh .33, ch 1344



  curr.df.descr <- 'Robust PCA with MDS, with outliers'
  curr.df.name <- 'pca.rob.wmds'
  curr.df <- cbind(scores.pca.hubert, scores.mds)
  curr.df.for.pairs <- df_numeric.untransformed
  curr.labels <- df$user_rating_is_good
  curr.metric <- "euclidean"
  # pairwise distances ok
  curr.linkage <- 'complete'
  # ba .5, k 2, silh .19, ch 140, 100-vs-all
  # PAMK: ba .62, k 6, silh .17, ch 1130



  curr.df.descr <- 'Robust PCA without categorical vars, with outliers'
  curr.df.name <- 'pca.rob'
  curr.df <- scores.pca.hubert
  curr.df.for.pairs <- df_numeric.untransformed
  curr.labels <- df$user_rating_is_good
  curr.metric <- "euclidean"
  # pairwise distances ok
  curr.linkage <- 'complete'
  # ba .5, k 2, silh .32, ch 94, 33-vs-all
  # PAMK: ba .6, k 9, silh .23, ch 1497
}



# === Preparatory Computations
curr.op.descr <- sprintf('Agnes: %s, %s linkage', curr.df.descr, curr.linkage)
curr.op.descr.pamk <- sprintf('PAM: %s', curr.df.descr)
curr.fname.base <- sprintf('agnes.new.%s.%s', curr.df.name, curr.linkage)
curr.fname.nbclust <- sprintf('computed_objs/%s.nbclust.rds', curr.fname.base)
curr.fname.tree <- sprintf('computed_objs/%s.tree.rds', curr.fname.base)
curr.fname.plot <- sprintf('plots/%s.jpg', curr.fname.base)
curr.fname.pamk <- sprintf('computed_objs/pamk.new.%s.rds', curr.df.name)
curr.fname.tree.pairs <- sprintf('plots/%s.pairs.jpg', curr.fname.base)
curr.fname.pamk.pairs <- sprintf('plots/pamk.new.%s.pairs.jpg', curr.df.name)

# distances
fun.pairwise.distances <- function () {
  for (i in 1:ncol(curr.df)) {
    print.var(names(curr.df)[i], mean(daisy(as.data.frame(curr.df[,i]), metric=curr.metric)))
  }
}
fun.calc.dist <- function () {
  curr.dist <- daisy(curr.df, metric=curr.metric)
}



fun.nbclust <- function () {
  # === Nbclust computations
  curr.linkage.nbclust <- if (curr.linkage == "ward") "ward.D" else curr.linkage
  if (file.exists(curr.fname.nbclust)) {
    curr.nbclust.info <- readRDS(curr.fname.nbclust)
  } else {
    curr.nbclust.info <- NbClust(diss=curr.dist, distance=NULL, min.nc=2, max.nc=10, method=curr.linkage.nbclust, index="silhouette")
    saveRDS(curr.nbclust.info, curr.fname.nbclust)
  }
  print.var('nbclust best k', curr.nbclust.info$Best.nc[[1]])
  print.var('nbclust best silh', curr.nbclust.info$Best.nc[[2]])
  table(curr.nbclust.info$Best.partition, curr.labels)
}


fun.agnes <- function() {
  # === agnes computations
  if (file.exists(curr.fname.tree)) {
    curr.tree <- readRDS(curr.fname.tree)
  } else {
    curr.tree <- agnes(curr.dist, diss=TRUE, method=curr.linkage, keep.diss=FALSE)
    saveRDS(curr.tree, curr.fname.tree)
  }

  if (!file.exists(curr.fname.plot)) {
    jpeg(curr.fname.plot, width = 1024, height = 1024)
    pltree(curr.tree, main=curr.op.descr, cex=0.83, xlab="Data", ylab="Height", )
    dev.off()
  }
  plot.jpg(curr.fname.plot)

  curr.best.asw <- 0
  curr.best.k <- 0
  curr.best.cut <- NULL
  for (k in 2:10) {
    curr.cut <- cutree(curr.tree, k)
    curr.asw <- summary(silhouette(curr.cut, curr.dist))$avg.width
    if (curr.best.asw < curr.asw) {
      curr.best.asw <- curr.asw
      curr.best.k <- k
      curr.best.cut <- curr.cut
    }
  }
  table(curr.best.cut, curr.labels)
  curr.tree.pred <- clust.to.pred(curr.best.cut, curr.labels)
  calc.balanced.acc(curr.tree.pred, curr.labels)
  print.var('agnes best k', curr.best.k)
  print.var('agnes best silh', curr.best.asw)
  print.var('agnes best CH', cluster.stats(curr.dist, curr.best.cut)$ch)

  if (!file.exists(curr.fname.tree.pairs)) {
    jpeg(curr.fname.tree.pairs, width = 1024, height = 1024)
    pairs(curr.df.for.pairs, main=curr.op.descr, col=curr.best.cut, pch=19)
    dev.off()
  }
  plot.jpg(curr.fname.tree.pairs)
  pairs(curr.df.for.pairs, main=curr.op.descr, col=curr.tree.pred+1, pch=19)


  # custom stuff
  curr.cut <- cutree(curr.tree, 3)
  table(curr.cut, curr.labels)
  curr.tree.pred <- clust.to.pred(curr.cut, curr.labels)
  calc.balanced.acc(curr.tree.pred, curr.labels)
  summary(silhouette(curr.cut, curr.dist))$avg.width
  cluster.stats(curr.dist, curr.best.cut)$ch
  pairs(df_numeric.wo.outliers, col=curr.cut, pch=19)
}



# === PAMK computations
if (file.exists(curr.fname.pamk)) {
  curr.pam.info <- readRDS(curr.fname.pamk)
} else {
  curr.pam.info <- pamk(curr.dist, k=2:10, diss=TRUE, usepam=TRUE)
  saveRDS(curr.pam.info, curr.fname.pamk)
}
curr.pam.clust <- curr.pam.info$pamobject$clustering
table(curr.pam.clust, curr.labels)
curr.pam.pred <- clust.to.pred(curr.pam.clust, curr.labels)

fun.calc.pamk.metrics <- function() {
  calc.balanced.acc(curr.pam.pred, curr.labels)
  print.var('pam k', curr.pam.info$nc)
  print.var('pam silh', summary(silhouette(curr.pam.clust, curr.dist))$avg.width)
  print.var('pam ch', cluster.stats(curr.dist, curr.pam.clust)$ch)
}

if (!file.exists(curr.fname.pamk.pairs)) {
  jpeg(curr.fname.pamk.pairs, width = 1024, height = 1024)
  pairs(curr.df.for.pairs, main=curr.op.descr.pamk, col=curr.pam.clust, pch=19)
  dev.off()
}
plot.jpg(curr.fname.pamk.pairs)

fun.pred.pairs <- function() {
  pairs(curr.df.for.pairs, main=curr.op.descr.pamk, col=curr.pam.pred+1, pch=19)
}






# ========================
# === Classification into clusters ===
# ========================

# _____________________
#|Cluster intepretation|
# _____________________
cluster_1<-df_numeric.wo.outliers[which(curr.pam.clust=='1'),]
cluster_2<-df_numeric.wo.outliers[which(curr.pam.clust=='2'),]
#Clusters Contigency table
table(curr.pam.clust, df.wo.outliers$user_rating_is_good)
pairs(df_numeric.wo.outliers, col=curr.pam.clust)
#summary()
summary(cluster_1)
summary(cluster_2)
#medoids
df_numeric[curr.pam.info$pamobject$medoids[1],]
df_numeric[curr.pam.info$pamobject$medoids[2],]
#----------END----------------------

#Choose clusters objects type to be used as the new class:
cluster_object<-curr.pam.clust#Choose partition around medoids
clusters<-data.frame(as.factor(cluster_object))
#Choose the dataset to be used
df_used<-df_robustPCA_mds[!outliers.pca,]#robust pca and mds without outliers
  
## set the seed to make your partition reproducible
set.seed(123)
#Cross-validation
## 70% of the sample size
smp_size <- floor(0.70 * nrow(df_used))
train_ind <- sample(seq_len(nrow(df_used)), size = smp_size)
train <- df_used[train_ind, ]
test <- df_used[-train_ind, ]
#Change the class (user_rating_is_good) to the cluster type
train.classes <- clusters[train_ind,]
test.classes <-clusters[-train_ind,]
train$user_rating_is_good<-train.classes
test$user_rating_is_good<-test.classes
#Control
train.control <- trainControl(method = "cv", number=10, savePredictions = "final")
#Dataframe to save classifiers predictions
classifier_scores<-data.frame()[1:dim(test)[1],]

#Classifiers

###Linear Discriminant Analysis
model_lda <- train(user_rating_is_good ~., 
                   data=train, method = "lda", trControl = train.control)

lda_pred <- predict(model_lda, test)
classifier_scores["lda"]<-predict(model_lda, test,type="prob")[,2]
#confusion matrix LDA
confusionMatrix(lda_pred, test.classes, positive='2')
avgF1score<-(F1_Score(lda_pred, test.classes, positive = '1')+F1_Score(lda_pred, test.classes, positive = '2'))/2
print(avgF1score)

###Regularized Discriminant Analysis 
model_rda <- train(user_rating_is_good ~., data=train, method = "rda", trControl = train.control)
rda_pred <- predict(model_rda, test)
classifier_scores["rda"]<-predict(model_rda, test,type="prob")[,2]
confusionMatrix(rda_pred, test.classes,positive = '2')
avgF1score<-(F1_Score(rda_pred, test.classes, positive = '1')+F1_Score(rda_pred, test.classes, positive = '2'))/2
print(avgF1score)


###Support Vector Machines
model_svm <- train(user_rating_is_good ~., data=train, method = "svmRadial", trControl = train.control)
svm_pred <- predict(model_svm, test)
#confusion matrix SVM
confusionMatrix(svm_pred, test.classes,positive = '2')
avgF1score<-(F1_Score(svm_pred, test.classes, positive = '1')+F1_Score(svm_pred, test.classes, positive = '2'))/2
print(avgF1score)

###Neural Networks
model_nn <- train(user_rating_is_good ~., data=train, method = "nnet", tuneGrid=expand.grid(.size=c(10), .decay=c(0,0)), trControl = train.control)
nn_pred <- predict(model_nn, test)
classifier_scores["nnet"]<-predict(model_nn, test,type="prob")[,2]
#confusion matrix NN
confusionMatrix(nn_pred, test.classes,positive='2')
avgF1score<-(F1_Score(nn_pred, test.classes, positive = '1')+F1_Score(nn_pred, test.classes, positive = '2'))/2
print(avgF1score)

###Random Forest
model_rf <- train(user_rating_is_good ~., data=train, method = "rf", trControl = train.control)
rf_pred <- predict(model_rf, test)
classifier_scores["rf"]<-predict(model_rf, test,type="prob")[,2]
#confusion matrix RF
confusionMatrix(rf_pred, test.classes,positive='2')
avgF1score<-(F1_Score(rf_pred, test.classes, positive = '1')+F1_Score(rf_pred, test.classes, positive = '2'))/2
print(avgF1score)


###K-Nearest Neighbour 
knnFit <- train(user_rating_is_good ~., data = train, method = "knn", trControl = train.control, preProcess = c("center","scale"),tuneLength = 20)
knnFit
plot(knnFit)
model_knn <- train(user_rating_is_good ~., data=train, method = "knn",tuneGrid=expand.grid(.k=9), trControl = train.control)
knn_pred <- predict(model_knn, test)
classifier_scores["knn"]<-predict(model_knn, test,type="prob")[,2]
#confusion matrix KNN
confusionMatrix(knn_pred, test.classes,positive='2')
avgF1score<-(F1_Score(knn_pred, test.classes, positive = '1')+F1_Score(knn_pred, test.classes, positive = '2'))/2
print(avgF1score)

#-------------------------------------------------------
#-------------Ensemble decisions------------------------
#-------------------------------------------------------
#-------------------------------------------------------

ensemble_decisions<-data.frame()[1:dim(classifier_scores)[1],]
#average
ensemble_decisions$avg<-rowSums(classifier_scores)/dim(classifier_scores)[2]
ensemble_decisions$avg<-as.factor(ifelse(ensemble_decisions$avg>0.5,'2','1'))
print(confusionMatrix(ensemble_decisions$avg, test.classes,positive = '2'))
print((F1_Score(ensemble_decisions$avg, test.classes, positive = '1')+F1_Score(ensemble_decisions$avg, test.classes, positive = '2'))/2)
#weighted average
ensemble_decisions$weighted_avg<-0.3*classifier_scores$nnet+0.3*classifier_scores$rf+0.13*classifier_scores$lda+0.13*classifier_scores$rda+0.13*classifier_scores$knn
ensemble_decisions$weighted_avg<-as.factor(ifelse(ensemble_decisions$weighted_avg>0.5,'2','1'))
print(confusionMatrix(ensemble_decisions$weighted_avg, test.classes,positive = '2'))
print((F1_Score(ensemble_decisions$weighted, test.classes, positive = '1')+F1_Score(ensemble_decisions$weighted, test.classes, positive = '2'))/2)
#majority voting
classifier_scores[]<-lapply(round(classifier_scores),factor)
ensemble_decisions$majority<-apply(classifier_scores,1,function(x) names(which.max(table(x))))
ensemble_decisions$majority<-as.factor(ifelse(ensemble_decisions$majority=='1','2','1'))
print(confusionMatrix(ensemble_decisions$majority, test.classes,positive = '2'))
print((F1_Score(ensemble_decisions$majority, test.classes, positive = '1')+F1_Score(ensemble_decisions$majority, test.classes, positive = '2'))/2)
