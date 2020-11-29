# ========================================================================================================
# Purpose:      Rscript for Application of LOG REG and CART (with sampling) on Full Loan Dataset
# Notes:        Add clustering?
#=========================================================================================================

setwd('C:/Users/woony/Desktop/BC2406/Project')
library(data.table)
library(caTools)
clean_data <- fread("clean_data.csv", stringsAsFactors = TRUE)
names(clean_data)
dim(clean_data)
# check that there are no 'character' columns
sapply(clean_data, class)

# reorder data from earliest to latest
clean_data <- clean_data[order(issue_year, issue_month)] 


# train-test split according to data
split = round(clean_data[,.N]*0.7)
train  = clean_data[1:split,]
test  = clean_data[(split+1):.N,]


#=========================================================================================================
# ===================== SMOTE:========================================================================

library(DMwR)

# Check initial count of unique value in loan_status
as.data.frame(table(train$loan_status))
#          Var1 Freq
# 1 Charged Off 157425
# 2  Fully Paid 674069

# Fix problem of memory alloc
memory.limit()
memory.limit(size = 32000)

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(loan_status ~., train, perc.over = 300, k = 5, perc.under = 150)

as.data.frame(table(balanced.data$loan_status))
#         Var1  Freq
#1 Charged Off 5632
#2  Fully Paid 6336



#=========================================================================================================
# ===================== Down Sampling:========================================================================


# Check initial count of unique value in loan_status
as.data.frame(table(train$loan_status))
#          Var1 Freq
# 1 Charged Off 157425
# 2  Fully Paid 674069

# Sample the majority to address imbalanced data & use same testset to test ----
# Random sample from majority class loan_status = Fully Paid and combine with loan_status = Charged Off to form new trainset -----
majority <- train[loan_status == "Fully Paid"]

minority <- train[loan_status == "Charged Off"]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases. 
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen <- majority[chosen]

# Combine two data tables by appending the rows
balanced.data <- rbind(majority.chosen, minority)
summary(balanced.data$loan_status)
## Check trainset is balanced.
# Charged Off  Fully Paid 
# 157425      157425

#=========================================================================================================
# ==================================== Apply Logistic Regression on sampled data =========================================

library(dplyr)
library(ggplot2)

# Develop model on Train Set
m1 <- glm(loan_status ~ ., family = binomial, data = balanced.data)
summary(m1)

toselect.variables <- summary(m1)$coeff[-1,4] < 0.05
sig.variables <- names(toselect.variables)[toselect.variables == TRUE] 
sig.variables

# Remove insignificant variables
m2.formula <- as.formula(loan_status ~ addr_state + avg_cur_bal
                         + avg_fico_range_high + avg_fico_range_low
                         + avg_inq_last_6mths + avg_mths_since_last_major_derog
                         + avg_revol_bal + avg_revol_util
                         + combined_dti + delinq_2yrs
                         + earliest_cr_line_year+ emp_length
                         + issue_month + issue_year
                         + loan_amnt + mo_sin_rcnt_rev_tl_op
                         + mo_sin_rcnt_tl + mths_since_last_delinq
                         + mths_since_last_record + mths_since_recent_revol_delinq
                         + num_actv_bc_tl + num_actv_rev_tl
                         + num_bc_tl + num_tl_op_past_12m
                         + pub_rec_bankruptcies + term
                         + tot_cur_bal + total_bal_ex_mort
                         + total_il_high_credit_limit + total_rev_hi_lim)

m2 <- glm(m2.formula, family = binomial, data = balanced.data)
summary(m2)


# 2nd round of removing insignificant variables
toselect.variables <- summary(m2)$coeff[-1,4] < 0.05
sig.variables <- names(toselect.variables)[toselect.variables == TRUE] 
sig.variables

m3.formula <- as.formula(loan_status ~ avg_cur_bal
                         + avg_fico_range_high + avg_fico_range_low
                         + avg_inq_last_6mths 
                         + avg_revol_bal + avg_revol_util
                         + combined_dti + delinq_2yrs
                         + earliest_cr_line_year+ emp_length
                         + issue_month + issue_year
                         + loan_amnt + mo_sin_rcnt_rev_tl_op
                         + mo_sin_rcnt_tl + mths_since_last_delinq
                         + mths_since_last_record 
                         + num_actv_bc_tl + num_actv_rev_tl
                         + num_bc_tl + num_tl_op_past_12m
                         + pub_rec_bankruptcies + term
                         + tot_cur_bal + total_bal_ex_mort
                         + total_il_high_credit_limit + total_rev_hi_lim)

# remove: addr_state + avg_mths_since_last_major_derog + mths_since_recent_revol_delinq
# + 

m3 <- glm(m3.formula, family = binomial, data = balanced.data )
summary(m3)

# VIF
vif(m3) # some variables have large GVIF > 5
# avg_cur_bal, avg_revol_bal, tot_cur_bal, total_bal_ex_mort, total_il_high_credit_limit
# + total_rev_hi_lim

# Remove variables with GVIF values over 5
m4.formula <- as.formula(loan_status ~ avg_fico_range_high + avg_fico_range_low
                         + avg_inq_last_6mths + avg_revol_util
                         + combined_dti + delinq_2yrs
                         + earliest_cr_line_year+ emp_length
                         + issue_month + issue_year
                         + loan_amnt + mo_sin_rcnt_rev_tl_op
                         + mo_sin_rcnt_tl + mths_since_last_delinq
                         + mths_since_last_record 
                         + num_actv_bc_tl + num_actv_rev_tl
                         + num_bc_tl + num_tl_op_past_12m
                         + pub_rec_bankruptcies + term)

m4 <- glm(m4.formula, family = binomial, data = balanced.data)
summary(m4)

OR <- exp(coef(m4))
OR

OR.CI <- exp(confint(m4))
OR.CI

# VIF
vif(m4) # highest GVIF is ~3.39


# Apply model on Test set
# Confusion matrix
prob_test_logreg <- predict(m4, newdata = test, type = "response")
threshold = 0.5
y.hat.test.logreg <- ifelse(prob_test_logreg > threshold, 1, 0)
y.hat.test.logreg <- factor(y.hat.test.logreg, levels = c(0,1), labels = c("Charged Off", "Fully Paid"))
table_logreg_balanced <- table(test$loan_status, y.hat.test.logreg, deparse.level = 2)
table_logreg_balanced

#                       y.hat.test.logreg
# test$loan_status      Charged Off  Fully Paid
# Charged Off             71162       6187
# Fully Paid              15735     263270

mean(y.hat.test.logreg == test$loan_status) 
# 0.9384825 (LOGREG on downsampled)

# FN rates: aka predict as fully paid when it is charged off = FN / (TP+FP)
FN <- 6187
TP <- 263270
FP <- 15735
print(FN/(TP+FP))
# FNR = 0.02217523

#=========================================================================================================
# ==================================== Apply CART on sampled data =========================================

library(rpart)
library(rpart.plot)
cart1 <- rpart(loan_status ~ ., data = balanced.data, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

printcp(cart1)

plotcp(cart1)

print(cart1)


# Extract the Optimal Tree via code
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
## i = 6 shows that the 2nd tree is optimal based on 1 SE rule.


# Prune the max tree using a particular CP value
cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)

## --- Trainset Error & CV Error --------------------------
## Root node error: 157425/314850 = 0.5
##       CP     nsplit    rel error   xerror   xstd
## 6 0.00095      8     0.211  0.211 0.00109

print(cart2)
rpart.plot(cart2, nn = T, main = "Optimal Tree in loan_status with sampling")

cart2$variable.importance
## avg_fico_range_high, avg_fico_range_low, max_fico_range_high has the top 3 highest importance.


# Model applied on Test-set
# Test set confusion matrix
predictions.smote.cart <- predict(cart2, newdata = test, type = 'class')
table_cart_balanced <- table(test$loan_status, predictions.smote.cart, deparse.level = 2)
table_cart_balanced
#                       predictions
# test$loan_status Charged Off Fully Paid
# Charged Off           72526       4823
# Fully Paid            17030     261975

round(prop.table(table1), 3)

# Overall Accuracy
mean(predictions.smote.cart == test$loan_status) 
# 0.9386761 (CART with smote)

# FN rates: aka predict as fully paid when it is charged off = FN / (TP+FP)
FN <- 4823
TP <- 261975
FP <- 17030
print(FN/(TP+FP))
# FNR = 0.01728643

