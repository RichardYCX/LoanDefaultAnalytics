# ========================================================================================================
# Purpose:      Rscript for Application of CART and LR (with different sampling methods) on a portion of Loan Dataset
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

# randomly sample 30% of train set for modeling
set.seed(2000)
train_sample = sample.split(train$loan_status, SplitRatio = .01) 
train_01 = subset(train, train_sample == TRUE)

# test data
remaining_train <- subset(train, train_sample == FALSE)
remaining_train_sample <- sample.split(remaining_train$loan_status, SplitRatio = .1) 
train_02 <- subset(remaining_train, remaining_train_sample == TRUE)

#=========================================================================================================
# ===================== SMOTE:========================================================================

library(DMwR)

# Check initial count of unique value in loan_status
as.data.frame(table(train_01$loan_status))

#          Var1 Freq
# 1 Charged Off 1408
# 2  Fully Paid 5932

# Fix problem of memory alloc
memory.limit()
memory.limit(size = 32000)

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(loan_status ~., train_01, perc.over = 300, k = 5, perc.under = 150)

as.data.frame(table(balanced.data$loan_status))
#         Var1  Freq
#1 Charged Off 5632
#2  Fully Paid 6336


#=========================================================================================================

# ==================================== Apply CART on SMOTE data =========================================
library(rpart)
library(rpart.plot)
cart1 <- rpart(loan_status ~ ., data = balanced.data, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

printcp(cart1)

plotcp(cart1)

print(cart1)


# Extract the Optimal Tree via code instead of eye power ------------
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
## i = 85 shows that the 2nd tree is optimal based on 1 SE rule.

# Prune the max tree using a particular CP value
cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)

## --- Trainset Error & CV Error --------------------------
## Root node error: 70400/142208 = 0.495
##       CP     nsplit    rel error   xerror   xstd
## 85 1.49e-05    429   0.00243 0.00847 0.000346

print(cart2)

rpart.plot(cart2, nn = T, main = "Optimal Tree in loan_status")


cart2$variable.importance
## avg_fico_range_low has the highest importance.


# Model applied on Test-set
# Test set confusion matrix
predictions.smote.cart <- predict(cart2, newdata = train_02, type = 'class')
table1 <- table(train_02$loan_status, predictions.smote.cart, deparse.level = 2)
table1
#                       predictions
# testset2$loan_status Charged Off Fully Paid
# Charged Off           9291       4650
# Fully Paid            5466       53260

round(prop.table(table1), 3)

# Overall Accuracy
mean(predictions.smote.cart == train_02$loan_status) 

# 0.8607896 (CART with smote) 

# ================================= # CART without SMOTE =============================================

cart3 <- rpart(loan_status ~ ., data = train_01, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

printcp(cart3)

plotcp(cart3)

print(cart3)


# Extract the Optimal Tree via code instead of eye power ------------
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart3$cptable[which.min(cart3$cptable[,"xerror"]), "xerror"] + cart3$cptable[which.min(cart3$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart3$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart3$cptable[i,1] * cart3$cptable[i-1,1]), 1)
## i = 5 shows that the 2nd tree is optimal based on 1 SE rule.

# Prune the max tree using a particular CP value
cart4 <- prune(cart3, cp = cp.opt)
printcp(cart4, digits = 3)

## --- Trainset Error & CV Error --------------------------
## Root node error: 1408/7340 = 0.192
##       CP     nsplit    rel error   xerror   xstd
## 5 0.0191      5     0.500  0.518 0.0182

print(cart4)

rpart.plot(cart4, nn = T, main = "Optimal Tree in loan_status (without sampling)")


cart4$variable.importance
## avg_fico_range_high has the highest importance.


# Model applied on Test-set
# Test set confusion matrix
predictions.cart <- predict(cart4, newdata = train_02, type = 'class')
table2 <- table(train_02$loan_status, predictions.cart, deparse.level = 2)
table2
#                       predictions
# testset2$loan_status Charged Off Fully Paid
# Charged Off           10815       3126
# Fully Paid            3830       54896

round(prop.table(table2), 3)

# Overall Accuracy
mean(predictions.cart == train_02$loan_status) 

# 0.9042757 (CART without smote) 



# ==============================================================================
library(dplyr)
library(ggplot2)

unique(train_01$hardship_flag)
train_01 <- train_01[, hardship_flag := NULL]

# ==============================================================================
# ======================= Using glm without SMOTE ==============================

# Develop model on Train Set
m1 <- glm(loan_status ~ ., family = binomial, data = train_01)
summary(m1)

toselect.variables <- summary(m1)$coeff[-1,4] < 0.05
sig.variables <- names(toselect.variables)[toselect.variables == TRUE] 
sig.variables

# Remove insignificant variables
m2.formula <- as.formula(loan_status ~ avg_fico_range_high + avg_fico_range_low 
                         + avg_inq_last_6mths + emp_length 
                         + loan_amnt + num_actv_rev_tl 
                         + term + tot_coll_amt)

m2 <- glm(m2.formula, family = binomial, data = train_01)
summary(m2)


# 2nd round of removing insignificant variables
m3.formula <- as.formula(loan_status ~ avg_fico_range_high + avg_fico_range_low 
                         + avg_inq_last_6mths + emp_length 
                         + loan_amnt + term)
m3 <- glm(m3.formula, family = binomial, data = train_01 )
summary(m3)

OR <- exp(coef(m3))
OR

OR.CI <- exp(confint(m3))
OR.CI

# VIF
vif(m3) # many variables have adjusted GVIF above 2, that for cyl is way above 2
vif(m2)     # adjusted GVIF around or less than 2, coefficients are much more stable
vif(m1) # Error

# Apply model on test set

# Train set confusion matrix
prob_test <- predict(m3, newdata = train_02, type = "response")
threshold = 0.5
y.hat.test <- ifelse(prob_test > threshold, 1, 0)
y.hat.test <- factor(y.hat.test, levels = c(0,1), labels = c("Charged Off", "Fully Paid"))
table_logreg <- table(train_02$loan_status, y.hat.test, deparse.level = 2)
table_logreg

#                       predictions
# testset$loan_status Charged Off Fully Paid
# Charged Off             9506        4435
# Fully Paid              3466       55260

mean(y.hat.test == train_02$loan_status) 
# 0.8912711

# ======================= Using glm with SMOTE ==============================

balanced.data <- balanced.data[, hardship_flag := NULL]

# Develop model on Train Set
m4 <- glm(loan_status ~ ., family = binomial, data = balanced.data)
summary(m4)

toselect.variables <- summary(m4)$coeff[-1,4] < 0.05
sig.variables <- names(toselect.variables)[toselect.variables == TRUE] 
sig.variables

# Remove insignificant variables
m5.formula <- as.formula(loan_status ~ addr_state + avg_fico_range_high 
                         + avg_fico_range_low + avg_inq_last_6mths
                         + avg_mort_acc + avg_num_rev_accts
                         + delinq_2yrs + earliest_cr_line_year
                         + emp_length + issue_month
                         + loan_amnt + mo_sin_old_rev_tl_op
                         + mths_since_last_record
                         + mths_since_recent_bc + num_accts_ever_120_pd
                         + num_actv_rev_tl + num_il_tl 
                         + num_sats + num_tl_op_past_12m
                         + pub_rec + pub_rec_bankruptcies + tax_liens
                         + term + tot_coll_amt
                         + tot_hi_cred_lim + total_acc)

m5 <- glm(m5.formula, family = binomial, data = balanced.data)
summary(m5)


# 2nd round of removing insignificant variables

toselect.variables <- summary(m5)$coeff[-1,4] < 0.05
sig.variables <- names(toselect.variables)[toselect.variables == TRUE] 
sig.variables

m6.formula <- as.formula(loan_status ~ addr_state + avg_fico_range_high 
                         + avg_fico_range_low + avg_inq_last_6mths
                         + avg_mort_acc + avg_num_rev_accts
                         + delinq_2yrs + earliest_cr_line_year
                         + emp_length
                         + loan_amnt + mo_sin_old_rev_tl_op
                         + mths_since_last_record
                         + mths_since_recent_bc + num_accts_ever_120_pd
                         + num_actv_rev_tl + num_il_tl 
                         + num_sats + num_tl_op_past_12m
                         + pub_rec + pub_rec_bankruptcies 
                         + tax_liens + tot_coll_amt
                         + tot_hi_cred_lim + total_acc)

m6 <- glm(m6.formula, family = binomial, data = balanced.data )
summary(m6)

OR <- exp(coef(m6))
OR

OR.CI <- exp(confint(m6))
OR.CI

# VIF
vif(m6) # some variables have large GVIF > 100
# avg_num_rev_accts, num_il_tl, total_acc

# Remove variables with GVIF values over 100.
m7.formula <- as.formula(loan_status ~ addr_state + avg_fico_range_high 
                         + avg_fico_range_low + avg_inq_last_6mths
                         + avg_mort_acc 
                         + delinq_2yrs + earliest_cr_line_year
                         + emp_length
                         + loan_amnt + mo_sin_old_rev_tl_op
                         + mths_since_last_record
                         + mths_since_recent_bc + num_accts_ever_120_pd
                         + num_actv_rev_tl  
                         + num_sats + num_tl_op_past_12m
                         + pub_rec + pub_rec_bankruptcies 
                         + tax_liens + tot_coll_amt
                         + tot_hi_cred_lim)
m7 <- glm(m7.formula, family = binomial, data = balanced.data)
summary(m7)

OR <- exp(coef(m7))
OR

OR.CI <- exp(confint(m7))
OR.CI

# VIF
vif(m7) # highest GVIF is 4


# Apply model on test set

# Train set confusion matrix
prob_test_balanced <- predict(m7, newdata = train_02, type = "response")
threshold = 0.5
y.hat.test.balanced <- ifelse(prob_test_balanced > threshold, 1, 0)
y.hat.test.balanced <- factor(y.hat.test.balanced, levels = c(0,1), labels = c("Charged Off", "Fully Paid"))
table_logreg_balanced <- table(train_02$loan_status, y.hat.test.balanced, deparse.level = 2)
table_logreg_balanced

#                       y.hat.test.balanced
# train_02$loan_status Charged Off Fully Paid
# Charged Off             68084       3724
# Fully Paid              5057        65343

mean(y.hat.test.balanced == train_02$loan_status) 
# 0.8786657


# ======================================= DOWN SAMPLING ======================================================================

# Sample the majority to address imbalanced data & use same testset to test ----
# Random sample from majority class loan_status = Fully Paid and combine with loan_status = Charged Off to form new trainset -----
majority <- train_01[loan_status == "Fully Paid"]

minority <- train_01[loan_status == "Charged Off"]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases. 
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen <- majority[chosen]

# Combine two data tables by appending the rows
train_01.bal <- rbind(majority.chosen, minority)
summary(train_01.bal$loan_status)
## Check trainset is balanced.


# ========================= Run LOGREG with downsampling ======================================================


# Develop model on Train Set
m8 <- glm(loan_status ~ ., family = binomial, data = train_01.bal)
summary(m8)

toselect.variables <- summary(m8)$coeff[-1,4] < 0.05
sig.variables <- names(toselect.variables)[toselect.variables == TRUE] 
sig.variables

# Remove insignificant variables
m9.formula <- as.formula(loan_status ~ avg_fico_range_high + emp_length 
                         + issue_year + num_op_rev_tl
                         + pub_rec_bankruptcies
                         + term + tot_coll_amt)

m9 <- glm(m9.formula, family = binomial, data = train_01.bal)
summary(m9)


# 2nd round of removing insignificant variables
m10.formula <- as.formula(loan_status ~ avg_fico_range_high + emp_length 
                         + num_op_rev_tl
                         + pub_rec_bankruptcies
                         + term + tot_coll_amt)
m10 <- glm(m10.formula, family = binomial, data = train_01.bal )
summary(m10)

OR <- exp(coef(m10))
OR

OR.CI <- exp(confint(m10))
OR.CI

# VIF
vif(m10) # all variables have adjusted GVIF around 1
vif(m9)  # all variables have adjusted GVIF around 1

# Apply model on test set

# Train set confusion matrix
prob_test <- predict(m10, newdata = train_02, type = "response")
threshold = 0.5
y.hat.test <- ifelse(prob_test > threshold, 1, 0)
y.hat.test <- factor(y.hat.test, levels = c(0,1), labels = c("Charged Off", "Fully Paid"))
table_logreg_down <- table(train_02$loan_status, y.hat.test, deparse.level = 2)
table_logreg_down

#                       predictions
# train_02$loan_status Charged Off Fully Paid
# Charged Off             12399       1542
# Fully Paid              7839       50887

mean(y.hat.test == train_02$loan_status) 
# 0.8709043


# =========================== RUN CART with downsampling ===================================================
cart1 <- rpart(loan_status ~ ., data = train_01.bal, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

printcp(cart1)

plotcp(cart1)

print(cart1)


# Extract the Optimal Tree via code instead of eye power ------------
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
## i = 4 shows that the 2nd tree is optimal based on 1 SE rule.

# Prune the max tree using a particular CP value
cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)

## --- Trainset Error & CV Error --------------------------
## Root node error: 70400/142208 = 0.495
##       CP     nsplit    rel error   xerror   xstd
## 4 0.00556      6     0.209  0.241 0.0116

print(cart2)

rpart.plot(cart2, nn = T, main = "Optimal Tree in loan_status")


cart2$variable.importance
## avg_fico_range_high has the highest importance.


# Model applied on Test-set
# Test set confusion matrix
predictions.down.cart <- predict(cart2, newdata = train_02, type = 'class')
table1 <- table(train_02$loan_status, predictions.down.cart, deparse.level = 2)
table1
#                       predictions
# testset2$loan_status Charged Off Fully Paid
# Charged Off           14369       1216
# Fully Paid            10566       56167

round(prop.table(table1), 3)

# Overall Accuracy
mean(predictions.down.cart == train_02$loan_status) 

# 0.8568721 (CART with downsampling) 


# ==================================== END ==========================================

