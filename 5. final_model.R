# ========================================================================================================
# Purpose:      Rscript for Application of LOG REG and CART (with sampling) on Full Loan Dataset
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

# Remove variables with avg, min, max, total of the same kind to keep only the average
clean_data <- clean_data[, avg_fico_range_low:= NULL]
clean_data <- clean_data[, max_fico_range_low:= NULL]
clean_data <- clean_data[, max_fico_range_high:= NULL]
clean_data <- clean_data[, min_fico_range_high:= NULL]
clean_data <- clean_data[, min_fico_range_low:= NULL]
clean_data <- clean_data[, debt_settlement_flag:= NULL]
clean_data <- clean_data[, max_revol_bal:= NULL]
clean_data <- clean_data[, issue_year := NULL]
clean_data <- clean_data[, max_open_acc := NULL]
clean_data <- clean_data[, min_open_acc := NULL]
clean_data <- clean_data[, max_mort_acc := NULL]
clean_data <- clean_data[, max_mths_since_last_major_derog := NULL]
clean_data <- clean_data[, min_mths_since_last_major_derog := NULL]
clean_data <- clean_data[, max_revol_util := NULL]
clean_data <- clean_data[, min_revol_util := NULL]
clean_data <- clean_data[, total_revol_util := NULL]
clean_data <- clean_data[, max_num_rev_accts := NULL]
clean_data <- clean_data[, min_num_rev_accts := NULL]
clean_data <- clean_data[, total_annual_inc := NULL]
clean_data <- clean_data[, min_mort_acc := NULL]
clean_data <- clean_data[, total_mort_acc := NULL]


# train-test split according to data
split = round(clean_data[,.N]*0.7)
train  = clean_data[1:split,]
test  = clean_data[(split+1):.N,]



#=============================================================================================================
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


#==========================================================================================================
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
## i = 7 shows that the 2nd tree is optimal based on 1 SE rule.


# Prune the max tree using a particular CP value
cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)

## --- Trainset Error & CV Error --------------------------
## Root node error: 157425/314850 = 0.5
##       CP     nsplit    rel error   xerror   xstd
## 7  0.000442     15       0.229     0.232   0.00114

## --- Variables actually used in tree construction: --------------------------
# [1] avg_fico_range_high avg_num_rev_accts   bc_open_to_buy     
# [4] combined_dti        loan_amnt           term               
# [7] total_acc 

print(cart2)
rpart.plot(cart2, nn = T, main = "Optimal Tree in loan_status with sampling", cex = 0.07, tweak = 10, border.col = 0, box.palette = 0, shadow.col = 0)
prp(cart2, type = 4, extra = 101, leaf.round = 1, fallen.leaves = TRUE,
    varlen = 0, cex = 0.4, tweak = 2, box.palette=rgb(0,0,0,0), border.col = rgb(0,0,0,0))

cart2$variable.importance
variable.impt <- cart2$variable.importance

sum <- sum(variable.impt)
variable.impt <- variable.impt/sum*100
variable.impt <- round(variable.impt,0)
variable.impt
## avg_fico_range_high, bc_open_to_buy, total_bc_limit, total_rev_hi_lim, tot_hi_cred_lim, avg_cur_bal has the top 6 highest importance.

# Model applied on Test-set
# Test set confusion matrix
predictions.cart <- predict(cart2, newdata = test, type = 'class')
table_cart_balanced <- table(test$loan_status, predictions.cart, deparse.level = 2)
table_cart_balanced

#                       predictions
# test$loan_status Charged Off Fully Paid
# Charged Off           72295       5054
# Fully Paid            20159     258846

round(prop.table(table_cart_balanced), 3)

# using type = 'prob'
predictions.prob.cart <- predict(cart2, newdata = test, type = 'prob')
class(predictions.prob.cart)
threshold = 0.5
predictions.cart <- ifelse(predictions.prob.cart[,2] > threshold, 1, 0)
predictions.cart <- factor(predictions.cart, levels = c(0,1), labels = c("Charged Off", "Fully Paid"))
table_results <- table(test$loan_status, predictions.cart, deparse.level = 2)
table_results

# Overall Accuracy
mean(predictions.cart == test$loan_status) 
# 0.9292473 (CART with sampled data)

# FN rates: aka predict as fully paid when it is charged off = FN / (TP+FP)
FN <- 5054
TP <- 258846
FP <- 20159
print(FN/(TP+FP))
# FNR = 0.01811437

# Read RDS file
saveRDS(cart2, "cart2.RDS")
