
library(glmnet)
library(ggplot2)
#library(corrplot)
library(ROCR)
library(openxlsx)


DATa <- read.xlsx("telecom.xlsx")

str(DATa)
dim(DATa)
###################################################################
######################### Scalling data ###########################
###################################################################

DATa$`Int'l.Plan` <- as.numeric(DATa$`Int'l.Plan`)
DATa$VMail.Plan <- as.numeric(DATa$VMail.Plan)
DATa$Eve.Mins <- as.numeric(DATa$Eve.Mins)
DATa$Eve.Charge <- as.numeric(DATa$Eve.Charge)
DATa$Night.Mins <- as.numeric(DATa$Night.Mins)
DATa$Night.Charge <- as.numeric(DATa$Night.Charge)
DATa$Intl.Mins <- as.numeric(DATa$Intl.Mins)
DATa$Night.Charge <- as.numeric(DATa$Night.Charge)
DATa$Intl.Charge <- as.numeric(DATa$Intl.Charge)

DATa$`Churn?` <- as.factor(DATa$`Churn?`)

## COR <- cor(DATa)
## corrplot(COR, method="circle")

ScDAT <- scale(DATa[,2:14], center = TRUE , scale = TRUE)

###############################################################################
############################# #cross-validation ###############################
################################################################################


CrVa <- cv.glmnet(ScDAT, DATa$`Churn?`, type.measure="class", family="binomial")

LMDmin <- CrVa$lambda.min

plot(CrVa)

# #fit the model

RidPen <- glmnet(ScDAT, DATa$`Churn?`, family="binomial", alpha=0)
LassPen <- glmnet(ScDAT, DATa$`Churn?`, family="binomial", alpha=1)

## #predicting

Pred_RID <- predict(RidPen, ScDAT, type="class", s=LMDmin)

Pred_Lass <- predict(LassPen, ScDAT, type="class", s=LMDmin)

## Confusion matrix

M.Con.Rid <- table(DATa$`Churn?`,Pred_RID)
Err.Rid <- (8+468)/3333

####
M.Con.Lass <- table(DATa$`Churn?`,Pred_Lass)
Err.Lass <- (6+469)/3333

################################################################################
############################## Roc Curve #######################################
################################################################################


St.Pr.Rid <- predict(RidPen, ScDAT, type="response", s=LMDmin)
St.Pr.Las <- predict(LassPen, ScDAT, type="response", s=LMDmin)

Prd.St.Rid <- prediction(St.Pr.Rid, DATa$`Churn?`)
Prd.St.Las <- prediction(St.Pr.Las, DATa$`Churn?`)

Per.Rid <- performance(Prd.St.Rid, measure = "tpr", x.measure = "fpr")
Per.Lass <- performance(Prd.St.Las, measure = "tpr", x.measure = "fpr")


################## T.P.R
#Rid
TPR.One.Rid <- attr(Per.Rid, "y.values")[[1]]

#Lass
TPR.one.Lass <- attr(Per.Lass, "y.values")[[1]]



################## F.P.R
# Rid
FPR.One.Rid <- attr(Per.Rid, "x.values")[[1]]

#Lass
FPR.one.Lass <- attr(Per.Lass, "x.values")[[1]]


##############  AUC

## Rid
AUC.Rid <- attr(performance(Prd.St.Rid, "auc"), "y.values")[[1]]
For.AUC.Rid <- signif(AUC.Rid,digits = 3)

ROC.Rid <- data.frame(fpr=FPR.One.Rid , tpr=TPR.One.Rid,model="GLM")

ggplot(ROC.Rid, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve STD Rid = ", For.AUC.Rid))



## Lasso

AUC.Lass <- attr(performance(Prd.St.Las, "auc"), "y.values")[[1]]
For.AUC.Lass <- signif(AUC.Lass,digits = 3)

ROC.Lass <- data.frame(fpr=FPR.one.Lass , tpr=TPR.one.Lass,model="GLM")

ggplot(ROC.Lass, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve STD Lass = ", For.AUC.Lass))





