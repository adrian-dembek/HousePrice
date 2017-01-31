options(scipen=999)
library(caret)
library(dplyr)

setwd("C:\\Learn\\House")
list.files()

#################################################################
## Load data
train <- read.csv2("train.csv", sep=",")
test <- read.csv2("test.csv", sep=",")

train$set <- "train"
test$set <- "test"

all <- merge(train, test, all=TRUE)
all$set <- as.factor(all$set)

all %>% filter(set=="train") %>% 
  dplyr:: select(Exterior1st, SalePrice) %>%
  group_by(Exterior1st) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())

#################################################################
## Condition1 and Condition2 joined; created dummy variables
Cond1 <- all %>% dplyr:: select(Id, Condition1) %>% mutate(cnt=1) %>% dcast(Id ~ Condition1)
names(Cond1) <- paste0(names(Cond1),"1")
Cond2 <- all %>% dplyr:: select(Id, Condition2) %>% mutate(cnt=1) %>% dcast(Id ~ Condition2)
names(Cond2) <- paste0(names(Cond2),"2")
Cond <- merge(Cond1, Cond2, by.x="Id1", by.y="Id2", all=TRUE)
Cond[is.na(Cond)] <- 0

Cond <- Cond %>% rowwise() %>% mutate(CondArtery=max(Artery1, Artery2),
                                      CondFeedr=max(Feedr1, Feedr2),
                                      CondNorm=max(Norm1, Norm2),
                                      CondPosA=max(PosA1, PosA2),
                                      CondPosN=max(PosN1, PosN2),
                                      CondRRAe=max(RRAe1, RRAe2),
                                      CondRRAn=max(RRAn1, RRAn2),
                                      CondRRNe=max(RRNe1),
                                      CondRRNn=max(RRNn1, RRNn2)) %>% dplyr:: select(Id=Id1, 19:27)

all <- all %>% dplyr:: select(-Condition1, -Condition2)
all <- merge(all,Cond,by="Id", all=TRUE)
#################################################################

#################################################################
## Exterior
(p <- ggplot(train, aes(as.factor(Exterior1st), as.factor(Exterior2nd))) + geom_tile(aes(fill = SalePrice), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue"))

# case <- train %>% dplyr:: select(Exterior1st, Exterior2nd, SalePrice) %>% group_by(Exterior1st, Exterior2nd) %>% summarise(avg=mean(SalePrice),cnt=n())
# # really low counts

# replace NA's with most common value
all$MSSubClass[all$MSSubClass %in% c("150" )]  <- "160"

all$Exterior1st[is.na(all$Exterior1st)]  <- "VinylSd"
all$Exterior2nd[is.na(all$Exterior2nd)]  <- "VinylSd"

## Exterior1st and Exterior2nd joined; created dummy variables
Ext1 <- all %>% dplyr:: select(Id, Exterior1st) %>% mutate(cnt=1) %>% dcast(Id ~ Exterior1st)
names(Ext1) <- paste0(names(Ext1),"1")
names(Ext1)[10] <- "WdSdng1"
Ext2 <- all %>% dplyr:: select(Id, Exterior2nd) %>% mutate(cnt=1) %>% dcast(Id ~ Exterior2nd)
names(Ext2) <- paste0(names(Ext2),"2")
names(Ext2)[4] <- "BrkCmn2"
names(Ext2)[16] <- "WdSdng2"
names(Ext2)[17] <- "WdShing2"

Ext <- merge(Ext1, Ext2, by.x="Id1", by.y="Id2", all=TRUE)
Ext[is.na(Ext)] <- 0

Ext <- Ext %>% rowwise() %>% mutate(  ExtAsbShng=max(AsbShng1, AsbShng2),
                                      ExtAsphShn=max(AsphShn2),
                                      ExtBrkCmn=max(BrkCmn2),
                                      ExtBrkFace=max(BrkFace1, BrkFace2),
                                      ExtCBlock=max(CBlock2),
                                      ExtCemntBd=max(CemntBd1, CmentBd2),
                                      ExtHdBoard=max(HdBoard1, HdBoard2),
                                      ExtImStucc=max(ImStucc2),
                                      ExtMetalSd=max(MetalSd2),
                                      ExtOther=max(Other2),
                                      ExtPlywood=max(Plywood1, Plywood2),
                                      ExtStone=max(Stone2),
                                      ExtStucco=max(Stucco1, Stucco2),
                                      ExtVinylSd=max(VinylSd1, VinylSd2),
                                      ExtWdSdng=max(WdSdng1, WdSdng2),
                                      ExtWdShing=max(WdShing1, WdShing2)) %>% dplyr:: select(Id=Id1, 33:48) %>% dplyr:: select(-ExtOther)

all <- all %>% dplyr:: select(-Exterior1st, -Exterior2nd)
all <- merge(all,Ext,by="Id", all=TRUE)
#################################################################

#################################################################
# Heating
(p <- ggplot(train, aes(as.factor(Heating), as.factor(HeatingQC))) + geom_tile(aes(fill = SalePrice), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue"))
# remove due to low predictive power and highly unbalanced data
all <- all %>% dplyr:: select(-Heating)
#################################################################

#################################################################
# MSSubClass
all$MSSubClass[all$MSSubClass %in% c("150" )]  <- "160"
all$MSSubClass[all$MSSubClass %in% c("40" )]  <- "50"
all$MSSubClass <- factor(all$MSSubClass)
#################################################################

#################################################################
# RoofMatl
all <- all %>% dplyr:: select(-RoofMatl)
#################################################################

#################################################################
# Electrical
all$Electrical[all$Electrical %in% c("FuseP")]  <- "FuseF"
all$Electrical[all$Electrical %in% c("Mix")]  <- "SBrkr"
all$Electrical[is.na(all$Electrical)] <- "SBrkr"
all$Electrical <- factor(all$Electrical)
#################################################################

#################################################################
## MiscFeature
all <- all %>% dplyr:: select(-MiscFeature)
#################################################################

#################################################################
## PoolQC
all <- all %>% dplyr:: select(-PoolQC)
#################################################################

#################################################################
## GarageCond
all <- all %>% dplyr:: select(-GarageCond)
#################################################################

#################################################################
## Utilities
all <- all %>% dplyr:: select(-Utilities)
#################################################################



# all %>% filter(set=="train") %>% 
#   dplyr:: select(GarageCond, SalePrice) %>%
#   group_by(GarageCond) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
# 
# 
# summary(stepwise.log) ## Multiple R-squared:  0.946,	Adjusted R-squared:  0.9401  Residual standard error: 0.09693



# library(memisc)
# test.var <- cases(
#   "BestQualCond"=(train$OverallCond>7 & train$OverallQual>6),
#   "BestQual"=(train$OverallCond>7 & train$OverallQual>0),# only applies if x >= 0
#   "AvgQual"=(train$OverallCond>2 & train$OverallQual>4),
#   "LowQualCond"=TRUE
# )
# View(as.data.frame(test.var))
# table(test.var)


## define function processing the data
process.house <- function(df) {
  
  # assign row.names based on Id and exclude Id variable
  # row.names(df) <- df$Id
  # df <- df %>% dplyr:: select(-Id)
  
  # change types
  df$MSSubClass <- as.factor(df$MSSubClass)
  df$MasVnrArea <- as.integer(df$MasVnrArea)
  
  df$MSZoning[is.na(df$MSZoning)] <- "RL"  
  
  levels(df$Fence) <- c(levels(df$Fence), "None")
  df$Fence[is.na(df$Fence)] <- "None"

  levels(df$FireplaceQu) <- c(levels(df$FireplaceQu), "None")
  df$FireplaceQu[is.na(df$FireplaceQu)] <- "None"
  
  levels(df$Alley) <- c(levels(df$Alley), "None")
  df$Alley[is.na(df$Alley)] <- "None"
  
  levels(df$BsmtQual) <- c(levels(df$BsmtQual), "None")
  df$BsmtQual[is.na(df$BsmtQual)] <- "None"
  
  levels(df$BsmtCond) <- c(levels(df$BsmtCond), "None")
  df$BsmtCond[is.na(df$BsmtCond)] <- "None"
  
  levels(df$BsmtExposure) <- c(levels(df$BsmtExposure), "None")
  df$BsmtExposure[is.na(df$BsmtExposure)] <- "None"
  
  levels(df$BsmtFinType1) <- c(levels(df$BsmtFinType1), "None")
  df$BsmtFinType1[is.na(df$BsmtFinType1)] <- "None"
  
  levels(df$BsmtFinType2) <- c(levels(df$BsmtFinType2), "None")
  df$BsmtFinType2[is.na(df$BsmtFinType2)] <- "None"
  
  df$MasVnrType[is.na(df$MasVnrType)] <- "None"
  
  levels(df$GarageQual) <- c(levels(df$GarageQual), "None")
  df$GarageQual[is.na(df$GarageQual)] <- "None"
  
  levels(df$GarageType) <- c(levels(df$GarageType), "None")
  df$GarageType[is.na(df$GarageType)] <- "None"
  
  levels(df$GarageFinish) <- c(levels(df$GarageFinish), "None")
  df$GarageFinish[is.na(df$GarageFinish)] <- "None"
  

  # GarageYrBlt - replace NAs with weighted average
  a <- df %>% 
    dplyr:: select(GarageYrBlt) %>% 
    filter(!is.na(GarageYrBlt)) %>%
    group_by(GarageYrBlt) %>% 
    summarise(cnt=n())
  
  avg_GarageYrBlt <- round(sum((a$GarageYrBlt*a$cnt))/sum(a$cnt),0)
  df$GarageYrBlt[is.na(df$GarageYrBlt)] <- avg_GarageYrBlt
  
  # Exterior
  # df$Exterior1st <- ifelse(is.na(df$Exterior1st),df$Exterior1st=levels(df$Exterior1st)
  
  ## change NAs to most common level
  df$SaleType[is.na(df$SaleType)] <- "Oth"
  df$Functional[is.na(df$Functional)]  <- "Typ"
  df$KitchenQual[is.na(df$KitchenQual)]  <- "TA"
  
  # change NAs to 0
  df$MasVnrArea[is.na(df$MasVnrArea)] <- 0
  df$BsmtFinSF1[is.na(df$BsmtFinSF1)] <- 0
  df$BsmtFinSF2[is.na(df$BsmtFinSF2)] <- 0  
  df$BsmtUnfSF[is.na(df$BsmtUnfSF)] <- 0  
  df$TotalBsmtSF[is.na(df$TotalBsmtSF)] <- 0  
  df$BsmtFullBath[is.na(df$BsmtFullBath)] <- 0 
  df$BsmtHalfBath[is.na(df$BsmtHalfBath)] <- 0
  df$GarageCars[is.na(df$GarageCars)] <- 0
  df$GarageArea[is.na(df$GarageArea)] <- 0
  
  # Predict LotFrontage based on LotArea and LotConfig
  df$SqrtLotArea <- sqrt(df$LotArea)
  model.LotFrontage <- lm(LotFrontage ~ SqrtLotArea + LotConfig, data=df)

  df$LotFrontagePredict <- round(predict(model.LotFrontage,df, interval = "prediction"),2)
  df$LotFrontage[is.na(df$LotFrontage)] <- df$LotFrontagePredict[is.na(df$LotFrontage)]
  df <- df %>% dplyr:: select(-SqrtLotArea,-LotFrontagePredict)
  
  # Create new variables
  
  df$AgeSold <- df$YrSold - df$YearBuilt
  df$AgeRemod <- df$YrSold - df$YearRemodAdd
  
  df$AgeSold <- ifelse(df$AgeSold<0,0,df$AgeSold)
  df$AgeRemod <- ifelse(df$AgeRemod<0,0,df$AgeRemod)
  
  
  library(memisc)
  df$QualCond <- as.factor(cases(
    "BestQualCond"=(df$OverallCond>7 & df$OverallQual>6),
    "BestQual"=(df$OverallCond>7 & df$OverallQual>0),# only applies if x >= 0
    "AvgQual"=(df$OverallCond>2 & df$OverallQual>4),
    "LowQualCond"=TRUE
  ))

  df$AgeGroup <- as.factor(cases(
    "Young"=(df$AgeSold <16),
    "Average"=(df$AgeSold <26),# only applies if x >= 0
    "Old"=(df$AgeSold <61),
    "VeryOld"=(df$AgeSold <100),
    "Above100"=TRUE
  ))
  
  # df$QualCondAge <- paste0(df$QualCond,df$AgeGroup)
  
  df$logAgeSold <- log(df$AgeSold+1)  
  df$logAgeRemod <- log(df$AgeRemod+1)
  df$logLotFrontage <- log(df$LotFrontage+1)
  df$logLotArea <- log(df$LotArea+1)
  df$logMasVnrArea <- log(df$MasVnrArea+1)
  df$logBsmtFinSF1 <- log(df$BsmtFinSF1+1)
  df$logBsmtFinSF2 <- log(df$BsmtFinSF2+1)  
  df$logX1stFlrSF <- log(df$X1stFlrSF+1)  
  df$logX2ndFlrSF <- log(df$X2ndFlrSF+1)  
  df$logGrLivArea <- log(df$GrLivArea+1)  
  df$logGarageArea <- log(df$GarageArea+1)    
  df$logWoodDeckSF <- log(df$WoodDeckSF+1) 
  df$logOpenPorchSF <- log(df$OpenPorchSF+1)   
  
  df$AgeSold.p2 <- (df$AgeSold)^2  
  df$AgeRemod.p2 <- (df$AgeRemod)^2
  df$LotFrontage.p2 <- (df$LotFrontage)^2
  df$LotArea.p2 <- (df$LotArea)^2
  df$MasVnrArea.p2 <- (df$MasVnrArea)^2
  df$BsmtFinSF1.p2 <- (df$BsmtFinSF1)^2
  df$BsmtFinSF2.p2 <- (df$BsmtFinSF2)^2  
  df$X1stFlrSF.p2 <- (df$X1stFlrSF)^2  
  df$X2ndFlrSF.p2 <- (df$X2ndFlrSF)^2  
  df$GrLivArea.p2 <- (df$GrLivArea)^2  
  df$GarageArea.p2 <- (df$GarageArea)^2    
  df$WoodDeckSF.p2 <- (df$WoodDeckSF)^2 
  df$OpenPorchSF.p2 <- (df$OpenPorchSF)^2   
  
  df$AgeSold.p3 <- (df$AgeSold)^3  
  df$AgeRemod.p3 <- (df$AgeRemod)^3
  df$LotFrontage.p3 <- (df$LotFrontage)^3
  df$LotArea.p3 <- (df$LotArea)^3
  df$MasVnrArea.p3 <- (df$MasVnrArea)^3
  df$BsmtFinSF1.p3 <- (df$BsmtFinSF1)^3
  df$BsmtFinSF2.p3 <- (df$BsmtFinSF2)^3  
  df$X1stFlrSF.p3 <- (df$X1stFlrSF)^3  
  df$X2ndFlrSF.p3 <- (df$X2ndFlrSF)^3  
  df$GrLivArea.p3 <- (df$GrLivArea)^3  
  df$GarageArea.p3 <- (df$GarageArea)^3    
  df$WoodDeckSF.p3 <- (df$WoodDeckSF)^3 
  df$OpenPorchSF.p3 <- (df$OpenPorchSF)^3   
  
  
  return(df)
}

all.processed <- process.house(all)

# summary(lm(log(SalePrice)~QualCondAge, all.processed))
# cor(all.processed, use="complete.obs", method="kendall") 

factors <- sapply(all.processed, is.factor)
factor.vars <- all.processed[ , factors]

## Check availability of factors in both sets
library(reshape2)

variables <- names(factor.vars)

level.report <- data.frame(variable=as.character(), 
                              level=as.character(), 
                              train=as.integer(), 
                              test=as.integer())

for (i in 1:length(factor.vars)){
  
  v <- variables[i]
  
  attach(factor.vars)
  var1 <- factor.vars %>% dplyr:: select(i, set) %>% group_by(get(v),set) %>% summarise(cnt=n()) %>% dcast(get(v) ~ set)
  var1$variable <- v
  names(var1)[1] <- "level"
  var1 <- var1 %>% dplyr:: select(variable, level, train, test)
  var1$level <- as.character(var1$level)
  detach(factor.vars)

  level.report <- merge(level.report,var1,all=TRUE)
}

train.processed <- all.processed %>% filter(set=="train")
row.names(train.processed) <- train.processed$Id
train.processed <- train.processed %>% dplyr:: select(-Id, -set)

test.processed <- all.processed %>% filter(set=="test")
row.names(test.processed) <- test.processed$Id
test.processed <- test.processed %>% dplyr:: select(-Id, -set)

## Remove outliers
library(ggplot2)
ggplot(train.processed, aes(SalePrice,GrLivArea)) + geom_point()

train.processed <- train.processed %>% filter(GrLivArea<4000)

#################################################################


#################################################################
#################################################################

# ## simple model
# simple = lm(log(SalePrice) ~ logGrLivArea + OverallQual + Neighborhood + logAgeSold + logLotArea + SaleType + X1stFlrSF + CentralAir + BsmtCond + FireplaceQu + KitchenQual, data = train.processed, na.action=na.exclude)
# summary(simple)
# 
# ## FIRST BASIC STEPWISE MODEL
# 
# summary(train)
# 
# ## FULL MODEL
# fullmod = lm(SalePrice ~ ., data = train.processed, na.action=na.exclude) 
# summary(fullmod)
# 
# ## Only intercept model
# nothing <- lm(SalePrice ~ 1,data = train.processed, na.action=na.exclude)
# summary(nothing)
# 
# ## Stepwise
# m.lm.base.stepwise = step(nothing, 
#                           list(lower=formula(nothing),
#                                upper=formula(fullmod)),
#                               direction="both")
# summary(m.lm.base.stepwise)
# plot(m.lm.base.stepwise)


#####################################################################################
## Create log of response variable
hist(train$SalePrice)

## FULL MODEL
fullmod.log = lm(log(SalePrice) ~ ., data = train.processed, na.action=na.exclude) 
summary(fullmod.log)

nothing.log <- lm(log(SalePrice) ~ 1,data = train.processed, na.action=na.exclude)
summary(nothing.log)

stepwise.log = step(nothing.log, 
                    list(lower=formula(nothing.log),
                         upper=formula(fullmod.log)),
                    direction="both")

summary(stepwise.log)
plot(stepwise.log)


  #####################
  ## Dummied variables
  train.dummy <- dummy(train.processed)
  fullmod.dummy.log <- lm(log(SalePrice) ~ ., data = train.dummy, na.action=na.exclude) 

  nothing.dummy.log <- lm(log(SalePrice) ~ 1,data = train.dummy, na.action=na.exclude)
  summary(nothing.log)
  
  stepwise.dummy.log = step(nothing.dummy.log, 
                      list(lower=formula(nothing.dummy.log),
                           upper=formula(fullmod.dummy.log)),
                      direction="both")
  
  summary(stepwise.dummy.log)
  # Multiple R-squared:  0.9463,	Adjusted R-squared:  0.9425 
  # F-statistic: 244.2 on 98 and 1357 DF,  p-value: < 0.00000000000000022
  
  # Remove outliers
  outlierTest(stepwise.dummy.log) # all outliers should be removed
  train.dummy.out <- train.dummy %>% dplyr:: slice(-c(632,463,1321,967,31,588,969,89,1450,1429))
  
  fullmod.dummy.log <- lm(log(SalePrice) ~ ., data = train.dummy.out, na.action=na.exclude) 
  summary(fullmod.dummy.log)
  
  nothing.dummy.log <- lm(log(SalePrice) ~ 1,data = train.dummy.out, na.action=na.exclude)
  summary(nothing.log)
  
  stepwise.dummy.log = step(nothing.dummy.log, 
                            list(lower=formula(nothing.dummy.log),
                                 upper=formula(fullmod.dummy.log)),
                            direction="both")
  summary(stepwise.dummy.log)
  
  ## Predict on test
  test.dummy <- dummy(test.processed)
  pred.step.dummy <- exp(predict(stepwise.dummy.log, test.dummy, interval = "prediction"))
  pred.step.dummy <- as.data.frame(pred.step.dummy)
  View(pred.step.dummy)
  pred.step.dummy$Id <- as.integer(row.names(pred.step.dummy))
  pred.step.dummy <- pred.step.dummy %>% dplyr:: select(Id, SalePrice=fit)
  write.table(pred.step.dummy, file="step_dummy.csv", dec=".", sep=",", row.names=FALSE)
  
  ## save the model
  save(stepwise.dummy.log, file="stepwise_dummy_log.rda")
  save(train.dummy.out, file="train_dummy_out.RData")
  
  #####################


## save the model
save(stepwise.log, file="stepwise_log.rda")

## check residuals
train.stepwise.log <- cbind(train.processed[,"SalePrice"],exp(predict(stepwise.log, train.processed, interval = "prediction")))
train.stepwise.log <- as.data.frame(train.stepwise.log)
train.stepwise.log$error <- (train.stepwise.log$fit- train.stepwise.log$V1)/train.stepwise.log$V1

ggplot(aes(V1,fit), data=train.stepwise.log) + geom_point()
plot(train.stepwise.log$error)

## predict on test set
predict.stepwise.log <- exp(predict(stepwise.log, test.processed, interval = "prediction"))
View(predict.stepwise.log)
predict.stepwise.log <- as.data.frame(predict.stepwise.log)
predict.stepwise.log <- predict.stepwise.log %>% dplyr:: select(fit)
options(OutDec= ".")
write.csv2(predict.stepwise.log, file="result_stepwise.csv")


#####################################################################################
## Continue with stepwise model
library(car)
outlierTest(stepwise.log) # all outliers should be removed

train.processed.out <- train.processed %>% dplyr:: slice(-c(632,463,1321,967,31,588,969,89,1450,1429))

## FULL MODEL
fullmod.log.out = lm(log(SalePrice) ~ ., data = train.processed.out, na.action=na.exclude) 
summary(fullmod.log.out)

nothing.log.out <- lm(log(SalePrice) ~ 1,data = train.processed.out, na.action=na.exclude)
summary(nothing.log.out)

stepwise.log.out = step(nothing.log.out, 
                        list(lower=formula(nothing.log.out),
                             upper=formula(fullmod.log.out)),
                        direction="both")

summary(stepwise.log.out)
plot(stepwise.log.out)

# outlierTest(stepwise.log.out) # Two more are left
train.processed.out <- train.processed.out %>% slice(-c(463,683))

## check residuals
train.stepwise.log.out <- cbind(train.processed.out[,"SalePrice"],exp(predict(stepwise.log.out, train.processed.out, interval = "prediction")))
train.stepwise.log.out <- as.data.frame(train.stepwise.log.out)
train.stepwise.log.out$error <- (train.stepwise.log.out$fit- train.stepwise.log.out$V1)/train.stepwise.log.out$V1

ggplot(aes(V1,fit), data=train.stepwise.log.out) + geom_point()
plot(train.stepwise.log.out$error)

table(test.processed$QualCondAge)
levels(test.processed$QualCondAge)[test.processed$QualCondAge=="BestQualCondAverage"] <- "BestQualCondOld"
levels(test.processed$QualCondAge)[test.processed$QualCondAge%in%c("BestQualYoung")] <- "AvgQualYoung"

## predict on test set
predict.stepwise.log.out <- exp(predict(stepwise.log.out, test.processed, interval = "prediction"))
View(predict.stepwise.log.out)
predict.stepwise.log.out <- as.data.frame(predict.stepwise.log.out)
predict.stepwise.log.out <- predict.stepwise.log.out %>% dplyr:: select(fit)
options(OutDec= ".")
# write.csv2(predict.stepwise.log.out, file="result_stepwise_out2.csv")
write.csv2(predict.stepwise.log.out, file="result_stepwise_out3.csv")

#####################################################################################
# Lasso regression

dummy <- function(df) {  
  
  NUM <- function(dataframe)dataframe[,sapply(dataframe,is.numeric)]
  FAC <- function(dataframe)dataframe[,sapply(dataframe,is.factor)]
  
  require(ade4)
  if (is.null(ncol(NUM(df)))) {
    DF <- data.frame(NUM(df), acm.disjonctif(FAC(df)))
    names(DF)[1] <- colnames(df)[which(sapply(df, is.numeric))]
  } else {
    DF <- data.frame(NUM(df), acm.disjonctif(FAC(df)))
  }
  return(DF)
} 

train.dummy <- dummy(train.processed.out)
train.dummy.x <- train.dummy %>% dplyr:: select(-SalePrice)
train.dummy.y <- train.dummy %>% dplyr:: select(SalePrice)

train.dummy.x.m <- as.matrix(train.dummy.x)
train.dummy.y.m <- as.matrix(train.dummy.y)

library(glmnet)
fit <- glmnet(train.dummy.x.m, train.dummy.y.m, family="gaussian", alpha=0, lambda=0.001)

fit

test.dummy <- dummy(test.processed)
test.dummy.m <- as.matrix(test.dummy)
View(test.dummy.m)
results <- predict(fit, s=0.01, newx=test.dummy.m, type="response")

#####################################################################################
# XGboost model
library(xgboost)

train.xg <- train.processed.out %>% dplyr:: select(-SalePrice) %>% dplyr:: select(-Condition2, -YearBuilt, -YearRemodAdd) %>% dummy()
  dplyr:: select(LotArea, LotFrontage, YearBuilt)
  dplyr:: select(-SalePrice)

train.xg <- data.matrix(train.xg)
train.Sale.xg <- train.processed.out %>% dplyr:: select(SalePrice)
train.Sale.xg$SalePrice <- log(train.Sale.xg$SalePrice)
train.Sale.xg <- data.matrix(train.Sale.xg)

dim(train.xg)
dim(train.Sale.xg)

bstSparse <- xgboost(data = train.xg, label = train.Sale.xg, seed=10, booster="gblinear", max_depth=10, nround = 10000, alpha=0.1)

summary(bstSparse)

names <- dimnames(data.matrix(train.xg[,-1]))[[2]]
importance_matrix <- xgb.importance(names, model = bstSparse)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])


test.xg <- test.processed %>%  dplyr:: select(-Condition2, -YearBuilt, -YearRemodAdd) %>% dummy() %>% data.matrix() 

pred <- exp(predict(bstSparse, test.xg))

train2 <- cbind(train.processed.out$SalePrice,exp(predict(bstSparse, train.xg)))
train2 <- as.data.frame(train2)
train2$error <- (train2$V2-train2$V1)/train2$V1

pred <- as.data.frame(pred)
View(pred)
pred$Id <- as.integer(row.names(pred)) + 1460
pred <- pred %>% dplyr:: select(Id, SalePrice=pred)
write.table(pred, file="xgboost2.csv", dec=".", sep=",", row.names=FALSE)

#####################################################################################

#####################################################################################
## Change levels to the most common level
train.processed.out.rel <- train.processed.out

factors <- sapply(train.processed.out.rel, is.factor)
factor.vars <- train.processed.out.rel[factors]

names(factor.vars[44])
train.processed.out.rel %>%
  dplyr:: select(Heating, SalePrice) %>%
  group_by(Heating) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.processed.out.rel$Heating)

train.processed.out.rel$MSZoning <- relevel(train.processed.out.rel$MSZoning, ref = "RL")
train.processed.out.rel <- train.processed.out.rel %>% dplyr:: select(-Street)
test.processed <- test.processed %>% dplyr:: select(-Street)
train.processed.out.rel <- train.processed.out.rel %>% dplyr:: select(-Condition2)
test.processed <- test.processed %>% dplyr:: select(-Condition2)
train.processed.out.rel$Alley <- relevel(train.processed.out.rel$Alley, ref = "None")
train.processed.out.rel$LotShape <- relevel(train.processed.out.rel$LotShape, ref = "Reg")
train.processed.out.rel$LandContour <- relevel(train.processed.out.rel$LandContour, ref = "Lvl")
train.processed.out.rel$LotConfig <- relevel(train.processed.out.rel$LotConfig, ref = "Inside")
train.processed.out.rel$RoofStyle <- relevel(train.processed.out.rel$RoofStyle, ref = "Gable")
train.processed.out.rel$RoofMatl <- relevel(train.processed.out.rel$RoofMatl, ref = "CompShg")  ## USUNAC???
train.processed.out.rel$Exterior1st <- relevel(train.processed.out.rel$Exterior1st, ref = "VinylSd")
train.processed.out.rel$Exterior2nd <- relevel(train.processed.out.rel$Exterior2nd, ref = "VinylSd")
train.processed.out.rel$MasVnrType <- relevel(train.processed.out.rel$MasVnrType, ref = "None")
train.processed.out.rel$ExterQual <- relevel(train.processed.out.rel$ExterQual, ref = "TA")
train.processed.out.rel$ExterCond <- relevel(train.processed.out.rel$ExterCond, ref = "TA")
train.processed.out.rel$Foundation <- relevel(train.processed.out.rel$Foundation, ref = "PConc")
train.processed.out.rel$BsmtQual <- relevel(train.processed.out.rel$BsmtQual, ref = "TA")
train.processed.out.rel$BsmtCond <- relevel(train.processed.out.rel$BsmtCond, ref = "TA")
train.processed.out.rel$BsmtExposure <- relevel(train.processed.out.rel$BsmtExposure, ref = "No")
train.processed.out.rel$BsmtFinType1 <- relevel(train.processed.out.rel$BsmtFinType1, ref = "Unf")
train.processed.out.rel$BsmtFinType2 <- relevel(train.processed.out.rel$BsmtFinType2, ref = "Unf")
train.processed.out.rel$Heating <- relevel(train.processed.out.rel$Heating, ref = "GasA")
train.processed.out.rel$HeatingQC <- relevel(train.processed.out.rel$HeatingQC, ref = "TA")
train.processed.out.rel$CentralAir <- relevel(train.processed.out.rel$CentralAir, ref = "Y")
train.processed.out.rel$Electrical <- relevel(train.processed.out.rel$Electrical, ref = "SBrkr")
train.processed.out.rel$KitchenQual <- relevel(train.processed.out.rel$KitchenQual, ref = "TA")
train.processed.out.rel$Functional <- relevel(train.processed.out.rel$Functional, ref = "Typ")
train.processed.out.rel$FireplaceQu <- relevel(train.processed.out.rel$FireplaceQu, ref = "None")
train.processed.out.rel$GarageType <- relevel(train.processed.out.rel$GarageType, ref = "Attchd")
train.processed.out.rel$GarageFinish <- relevel(train.processed.out.rel$GarageFinish, ref = "Unf")
train.processed.out.rel$GarageQual <- relevel(train.processed.out.rel$GarageQual, ref = "TA")
train.processed.out.rel$GarageCond <- relevel(train.processed.out.rel$GarageCond, ref = "TA")
train.processed.out.rel$PavedDrive <- relevel(train.processed.out.rel$PavedDrive, ref = "Y")
train.processed.out.rel$PoolQC <- relevel(train.processed.out.rel$PoolQC, ref = "None")
train.processed.out.rel$Fence <- relevel(train.processed.out.rel$Fence, ref = "None")
train.processed.out.rel$MiscFeature <- relevel(train.processed.out.rel$MiscFeature, ref = "None")
train.processed.out.rel$SaleType <- relevel(train.processed.out.rel$SaleType, ref = "WD")
train.processed.out.rel$SaleCondition <- relevel(train.processed.out.rel$SaleCondition, ref = "Normal")

train.processed.out.rel$Neighborhood <- relevel(train.processed.out.rel$Neighborhood, ref = "NAmes")
train.processed.out.rel$Condition1 <- relevel(train.processed.out.rel$Condition1, ref = "Norm")
train.processed.out.rel$HouseStyle <- relevel(train.processed.out.rel$HouseStyle, ref = "1Story")

train.processed.out.rel$OverallCond <- as.factor(train.processed.out.rel$OverallCond)
train.processed.out.rel$OverallCond <- relevel(train.processed.out.rel$OverallCond, ref = "5")
test.processed$OverallCond <- as.factor(test.processed$OverallCond)

#####################################################################################
## Model after releveling

## FULL MODEL

fullmod.log.out.rel = lm(log(SalePrice) ~ . + .*., data = train.processed.out.rel, na.action=na.exclude) 
summary(fullmod.log.out.rel)

nothing.log.out.rel <- lm(log(SalePrice) ~ 1,data = train.processed.out.rel, na.action=na.exclude)
summary(nothing.log.out.rel)

stepwise.log.out.rel = step(nothing.log.out.rel, 
                            list(lower=formula(nothing.log.out.rel),
                                 upper=formula(fullmod.log.out.rel)),
                            direction="both")
summary(stepwise.log.out.rel)

predict.stepwise.log.out.rel <- exp(predict(stepwise.log.out.rel, test.processed, interval = "prediction"))
View(predict.stepwise.log.out.rel)
predict.stepwise.log.out.rel <- as.data.frame(predict.stepwise.log.out.rel)
predict.stepwise.log.out.rel <- predict.stepwise.log.out.rel %>% dplyr:: select(fit)
options(OutDec= ".")
# write.csv2(predict.stepwise.log.out.rel, file="result_stepwise_out_rel.csv")
write.csv2(predict.stepwise.log.out.rel, file="result_stepwise_out_rel2.csv")

outlierTest(stepwise.log.out.rel)
train.processed.out.rel <- train.processed.out.rel %>% slice(-c(492))


#####################################################################################
## Cross validation
library(DAAG)
train.processed.out.cv <- train.processed.out %>% dplyr:: select(-Condition1, -Condition2)

CV.model <- cv.lm(train.processed.out.cv, form.lm = formula(log(SalePrice) ~ .), m=2, dots =
       FALSE, seed=9, plotit=TRUE, printit=TRUE)


#####################################################################################
## Continue lm model 

## Binning continous variables




# NOT WORKING
# result=smbinning(df=train.processed.out,y="SalePrice",x="OverallCond",p=0.01) 
# 
# result <- smbinning.factor()
# 
# a <- smbinning.factor.gen(df=train.processed.out,y="SalePrice",x="OverallCond")

## Bining factor categories
train.binning <- train.processed.out
test.binning <- test.processed

report <- train.processed.out %>% dplyr:: select(MSSubClass, SalePrice) %>% group_by(MSSubClass) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.binning$MSSubClass)[levels(train.binning$MSSubClass)%in%c("45","180")] <- "30"
levels(test.binning$MSSubClass)[levels(test.binning$MSSubClass)%in%c("45","180")] <- "30"
levels(train.binning$MSSubClass)[levels(train.binning$MSSubClass)%in%c("40")] <- "50"
levels(test.binning$MSSubClass)[levels(test.binning$MSSubClass)%in%c("40")] <- "50"
report <- train.binning %>% dplyr:: select(MSSubClass, SalePrice) %>% group_by(MSSubClass) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.binning$MSSubClass) # OK


table(train.binning$MSSubClass)

summary(stepwise.log.out)

report <- train.processed.out %>% dplyr:: select(Street, SalePrice) %>% group_by(Street) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
train.binning <- train.binning %>% dplyr:: select(-Street)
test.binning <- test.binning %>% dplyr:: select(-Street)

report <- train.processed.out %>% dplyr:: select(Alley, SalePrice) %>% group_by(Alley) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.binning$Alley) <- c("None","Grvl","Pave")
train.binning$Alley <- relevel(train.binning$Alley, ref = "None")

report <- train.processed.out %>% dplyr:: select(Neighborhood, SalePrice) %>% group_by(Neighborhood) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.binning$Neighborhood)[levels(train.binning$Neighborhood)%in%c("Blueste")] <- "Sawyer"
levels(test.binning$Neighborhood)[levels(test.binning$Neighborhood)%in%c("Blueste")] <- "Sawyer"
levels(train.binning$Neighborhood)[levels(train.binning$Neighborhood)%in%c("NPkVill")] <- "NAmes"
levels(test.binning$Neighborhood)[levels(test.binning$Neighborhood)%in%c("NPkVill")] <- "NAmes"
levels(train.binning$Neighborhood)[levels(train.binning$Neighborhood)%in%c("Veenker")] <- "Timber"
levels(test.binning$Neighborhood)[levels(test.binning$Neighborhood)%in%c("Veenker")] <- "Timber"
report <- train.binning %>% dplyr:: select(Neighborhood, SalePrice) %>% group_by(Neighborhood) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
train.binning$Neighborhood <- relevel(train.binning$Neighborhood, ref = "NAmes")

report <- train.processed.out %>% dplyr:: select(Condition1, SalePrice) %>% group_by(Condition1) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.binning$Condition1)[levels(train.binning$Condition1)%in%c("RRNe","RRAn")] <- "Norm"
levels(test.binning$Condition1)[levels(test.binning$Condition1)%in%c("RRNe","RRAn")] <- "Norm"
levels(train.binning$Condition1)[levels(train.binning$Condition1)%in%c("PosA","RRNn")] <- "PosN"
levels(test.binning$Condition1)[levels(test.binning$Condition1)%in%c("PosA","RRNn")] <- "PosN"
train.binning$Condition1 <- relevel(train.binning$Condition1, ref = "Norm")
report <- train.binning %>% dplyr:: select(Condition1, SalePrice) %>% group_by(Condition1) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())

report <- train.processed.out %>% dplyr:: select(Condition2, SalePrice) %>% group_by(Condition2) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
train.binning <- train.binning %>% dplyr:: select(-Condition2)
test.binning <- test.binning %>% dplyr:: select(-Condition2)

report <- train.processed.out %>% dplyr:: select(BldgType, SalePrice) %>% group_by(BldgType) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.binning$BldgType)

report <- train.processed.out %>% dplyr:: select(HouseStyle, SalePrice) %>% group_by(HouseStyle) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
levels(train.binning$HouseStyle)[levels(train.binning$HouseStyle)%in%c("2.5Fin")] <- "2Story"
levels(test.binning$HouseStyle)[levels(test.binning$HouseStyle)%in%c("2.5Fin")] <- "2Story"
train.binning$HouseStyle <- relevel(train.binning$HouseStyle, ref = "1Story")
levels(train.binning$HouseStyle)

report <- train.processed.out %>% dplyr:: select(OverallQual, SalePrice) %>% group_by(OverallQual) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
ggplot(data=train.processed.out, aes(OverallQual,SalePrice)) + geom_point()
train.binning$OverallQual <- ifelse(train.binning$OverallQual==1,2,train.binning$OverallQual)
test.binning$OverallQual <- ifelse(test.binning$OverallQual==1,2,test.binning$OverallQual)
train.binning$OverallQual <- ifelse(train.binning$OverallQual==10,9,train.binning$OverallQual)
test.binning$OverallQual <- ifelse(test.binning$OverallQual==10,9,test.binning$OverallQual)
ggplot(data=train.binning, aes(OverallQual,SalePrice)) + geom_point()
table(train.binning$OverallQual)

report <- train.processed.out %>% dplyr:: select(OverallCond, SalePrice) %>% group_by(OverallCond) %>% summarise(avg=mean(SalePrice), std=sd(SalePrice), stdp=std/avg,cnt=n())
train.binning$OverallCond <- ifelse(train.binning$OverallCond==2 & train.binning$SalePrice>300000,5,test$OverallCond)
ggplot(data=train.binning, aes(OverallCond,SalePrice)) + geom_point()

train.binning$OverallCond <- ifelse(train.binning$OverallCond==1 | train.binning$OverallCond==2, 1, train.binning$OverallCond)
train.binning$OverallCond <- ifelse(train.binning$OverallCond==3 | train.binning$OverallCond==4, 3, train.binning$OverallCond)
train.binning$OverallCond <- ifelse(train.binning$OverallCond==6 | train.binning$OverallCond==7, 7, train.binning$OverallCond)
train.binning$OverallCond <- ifelse(train.binning$OverallCond==8 | train.binning$OverallCond==9, 9, train.binning$OverallCond)
train.binning$OverallCond <- as.factor(train.binning$OverallCond)
train.binning$OverallCond <- relevel(train.binning$OverallCond, ref = "5")
levels(train.binning$OverallCond)

summary(lm(data=train.binning,log(SalePrice)~OverallCond))

test.binning$OverallCond <- ifelse(test.binning$OverallCond==1 | test.binning$OverallCond==2, 1, test.binning$OverallCond)
test.binning$OverallCond <- ifelse(test.binning$OverallCond==3 | test.binning$OverallCond==4, 3, test.binning$OverallCond)
test.binning$OverallCond <- ifelse(test.binning$OverallCond==6 | test.binning$OverallCond==7, 7, test.binning$OverallCond)
test.binning$OverallCond <- ifelse(test.binning$OverallCond==8 | test.binning$OverallCond==9, 9, test.binning$OverallCond)
test.binning$OverallCond <- as.factor(test.binning$OverallCond)
test.binning$OverallCond <- relevel(test.binning$OverallCond, ref = "5")



#####################################################################################
## stepwise model after binning
## FULL MODEL
fullmod.log.out = lm(log(SalePrice) ~ ., data = train.binning, na.action=na.exclude) 
summary(fullmod.log.out)

nothing.log.out <- lm(log(SalePrice) ~ 1,data = train.binning, na.action=na.exclude)
summary(nothing.log.out)

stepwise.log.out = step(nothing.log.out, 
                        list(lower=formula(nothing.log.out),
                             upper=formula(fullmod.log.out)),
                        direction="both")

summary(stepwise.log.out)
plot(stepwise.log.out)

influencePlot(stepwise.log.out,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#####################################################################################

#####################################################################################
## Neural networks
library(h2o)
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

dat <- train.processed.out[,-(82:94)]  # remove the ID column
dat_h2o <- as.h2o(dat)
dat_h2o_test <- as.h2o(test.processed[,-(81:93)])

prob=0.3
n <- round(nrow(train.processed.out) * prob,0)

set.seed(1)
valid <- dat[sample(nrow(dat), n, replace = FALSE, prob=NULL),]
training <- dat[-sample(nrow(dat), n, replace = FALSE, prob=NULL),]

h2o.valid <- as.h2o(valid)
h2o.training <- as.h2o(training)

model.h2o <- 
  h2o.deeplearning(x = c(1:78,80,81),  # column numbers for predictors
                   y = "SalePrice",   # column number for label
                   training_frame = h2o.training, # data in H2O format
                   validation_frame = h2o.valid,
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   hidden = c(50,50,50), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs

h2o.train <- h2o.predict(model.h2o, dat_h2o)
result.h2o <- cbind(as.data.frame(dat_h2o), as.data.frame(h2o.train))
result.h2o$error <- (result.h2o$predict-result.h2o$SalePrice)/result.h2o$SalePrice

a <- cbind(result.h2o,train.stepwise.log.out)
a$er.h2o <- abs((a$predict-a$SalePrice)/a$SalePrice)
a$er.step <- abs((a$fit-a$SalePrice)/a$SalePrice)
mean(a$er.h2o)
mean(a$er.step)

a$both <- (a$predict+a$fit)/2
a$er.both <- abs((a$both-a$SalePrice)/a$SalePrice)
mean(a$er.both)

h2o.test <- h2o.predict(model.h2o, dat_h2o_test)
View(h2o.test)

h2o.test <- as.data.frame(h2o.test)
write.csv2(h2o.test,"h2o_test.csv")


#####################################################################################
## Decision Tree
# ctree
library(party)
m.tree.base <-  ctree(
  SalePrice ~ ., 
  data = train.processed)

summary(m.tree.base)
plot(m.tree.base, type="simple")
######################################################################################

######################################################################################
# rpart
library(rpart)
m.tree.rpart <- rpart(SalePrice ~ ., method="anova", data=train.processed, control=rpart.control(minsplit=30, cp=0.001))
printcp(m.tree.rpart)
plotcp(m.tree.rpart)
plot(m.tree.rpart, uniform=TRUE, 
     main="Classification Tree for Train")
text(m.tree.rpart, use.n=TRUE, all=TRUE, cex=.8)

m.tree.rpart.pr <- prune(m.tree.rpart, cp=m.tree.rpart$cptable[which.min(m.tree.rpart$cptable[,"xerror"]),"CP"])
plot(m.tree.rpart.pr, uniform=TRUE, 
     main="Classification Tree for Train")
text(m.tree.rpart.pr, use.n=TRUE, all=TRUE, cex=.8)
######################################################################################



result <- exp(predict(simple, test.processed, interval = "prediction"))
View(result)

result <- as.data.frame(result)
submission <- result %>% dplyr:: select(fit)

options(OutDec= ".")
write.csv2(submission, file="result.csv")


table(test$MSSubClass)
summary(stepwise.log)


result.stepwise <- exp(predict(stepwise.log, test.processed, interval = "prediction"))
View(result.stepwise)
result.stepwise <- as.data.frame(result.stepwise)
submission2 <- result.stepwise %>% dplyr:: select(fit)
options(OutDec= ".")
write.csv2(submission2, file="result_stepwise2.csv")

#####################################################################################
## random forest

# library(randomForest)
# train.processed <- train.processed %>% dplyr:: select(-Utilities)
# forest <- randomForest(SalePrice ~ ., data=train.processed)
# summary(forest)
# plot(randomForest(SalePrice ~ ., train.processed, keep.forest=FALSE, ntree=500))
# 
# prediction <- predict(forest, train.processed, predict.all=TRUE)
# View(prediction[1])
# 
# train.processed2 <- cbind(train.processed,prediction[1])
# train.processed2$error <- (train.processed2$aggregate - train.processed2$SalePrice)/train.processed2$SalePrice
# 
# ## for random forest purpuses
# levels(test.processed$Condition2) <- levels(train.processed$Condition2)
# levels(test.processed$HouseStyle) <- levels(train.processed$HouseStyle)
# levels(test.processed$RoofMatl) <- levels(train.processed$RoofMatl)
# levels(test.processed$Exterior1st) <- levels(train.processed$Exterior1st)
# levels(test.processed$Exterior2nd) <- levels(train.processed$Exterior2nd)
# levels(test.processed$Heating) <- levels(train.processed$Heating)
# levels(test.processed$Electrical) <- levels(train.processed$Electrical)
# levels(test.processed$GarageQual) <- levels(train.processed$GarageQual)
# levels(test.processed$PoolQC) <- levels(train.processed$PoolQC)
# levels(test.processed$MiscFeature) <- levels(train.processed$MiscFeature)
# 
# prediction <- predict(forest, test.processed, predict.all=TRUE)
# View(prediction[1])
# prediction <- as.data.frame(prediction[1])
# write.csv2(prediction, file="rf1.csv")

#################
## Continue with stepwise model
library(car)
outlierTest(stepwise.log)

train.processed.out <- train.processed %>% slice(-c(632,463,1321,967,31,588,969,89,1450,1429))

#####################################################################################
## Create log of response variable

## FULL MODEL
fullmod.log = lm(log(SalePrice) ~ ., data = train.processed.out, na.action=na.exclude) 
summary(fullmod.log)

nothing.log <- lm(log(SalePrice) ~ 1,data = train.processed.out, na.action=na.exclude)
summary(nothing.log)

stepwise.log = step(nothing.log, 
                    list(lower=formula(nothing.log),
                         upper=formula(fullmod.log)),
                    direction="both")
summary(stepwise.log)
plot(stepwise.log)

nums <- sapply(train.processed, is.numeric)
numeric.vars <- train.processed[ , nums]
findLinearCombos(numeric.vars)

summary(stepwise.log)
#####################################################################################