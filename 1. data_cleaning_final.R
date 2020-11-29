require(lubridate)
setwd("C:/Users/yangc/OneDrive - Nanyang Technological University/Uni/BC2406/Project")
library(data.table)
library(ggplot2)
original_data <- fread("accepted_2007_to_2018Q4.csv")

data <- original_data
# summary(data)
dim(original_data)

## drop var that are either too specific to lending club/ will not know before loan
data[,":="(desc=NULL, emp_title=NULL, funded_amnt = NULL, funded_amnt_inv = NULL,
           grade = NULL, hardship_flag = NULL, id = NULL, initial_list_status = NULL, inq_fi = NULL,
           inq_last_12m = NULL, installment = NULL, int_rate = NULL,
           last_credit_pull_d = NULL, last_pymnt_amnt = NULL, last_pymnt_d = NULL, 
           member_id = NULL, mths_since_recent_inq = NULL, next_pymnt_d = NULL, num_bc_sats = NULL, 
           num_rev_tl_bal_gt_0 = NULL, num_tl_120dpd_2m = NULL, num_tl_30dpd = NULL,
           num_tl_90g_dpd_24m = NULL, out_prncp = NULL, out_prncp_inv = NULL, 
           payment_plan_start_date = NULL, pymnt_plan = NULL, recoveries = NULL,
           settlement_date = NULL, settlement_status = NULL, sub_grade = NULL, 
           total_pymnt_inv = NULL, verification_status = NULL, verification_status_joint = NULL)]
data[,":="(acc_open_past_24mths=NULL, collection_recovery_fee=NULL, collections_12_mths_ex_med=NULL,
                 fico_range_high=NULL, fico_range_low=NULL, pct_tl_nvr_dlq=NULL, policy_code=NULL, 
                 total_pymnt=NULL, total_rec_int=NULL, total_rec_late_fee=NULL, total_rec_prncp=NULL)]
# keep only interested rows
data[loan_status == "Does not meet the credit policy. Status:Fully Paid",
     loan_status := "Fully Paid"]
data[loan_status == "Does not meet the credit policy. Status:Charged Off",
     loan_status := "Charged Off"]
unique(data$loan_status)
data <- data[loan_status == "Charged Off" | loan_status == "Fully Paid"]

# drop var with too many missing values/ unimportant
data[,":="(debt_settlement_flag_date = NULL, hardship_last_payment_amount = NULL,
           hardship_payoff_balance_amount = NULL, orig_projected_additional_accrued_interest = NULL,
           hardship_loan_status = NULL, hardship_dpd = NULL, hardship_length = NULL, hardship_end_date = NULL, 
           hardship_start_date = NULL, hardship_amount = NULL, deferral_term = NULL, total_cu_tl = NULL,  
           hardship_status = NULL, hardship_reason = NULL, hardship_type = NULL, zip_code = NULL,
           title = NULL, purpose = NULL, url = NULL, all_util = NULL, mths_since_rcnt_il = NULL, total_bal_il = NULL,
           il_util = NULL, max_bal_bc = NULL, open_acc_6m = NULL, open_act_il= NULL, open_il_12m = NULL,
           open_il_24m = NULL, open_rv_12m = NULL, open_rv_24m = NULL, sec_app_chargeoff_within_12_mths = NULL, 
           sec_app_collections_12_mths_ex_med = NULL, sec_app_open_act_il = NULL, settlement_amount = NULL,
           settlement_percentage = NULL, settlement_term = NULL, mo_sin_old_il_acct = NULL)]
summary(data)
names(data)
setcolorder(data, order(names(data)))

# richard
data[,issue_d := dmy(paste("01-",issue_d,sep=""))]
data[,issue_year := year(issue_d)]
data[,issue_month := month(issue_d, label=T, abbr=T)]
data[,issue_quarter := quarter(issue_d)]
data[,issue_d := NULL]

data[,sec_app_earliest_cr_line := dmy(paste("01-",sec_app_earliest_cr_line,sep=""))]
data[,earliest_cr_line := dmy(paste("01-",earliest_cr_line,sep=""))]
data[,earliest_cr_line_year := year(pmin(earliest_cr_line, sec_app_earliest_cr_line, na.rm = T))]
data[,earliest_cr_line_month := month(pmin(earliest_cr_line, sec_app_earliest_cr_line, na.rm = T), 
                                      label=T, abbr=T)]
data[,earliest_cr_line_quarter := quarter(pmin(earliest_cr_line, sec_app_earliest_cr_line, na.rm = T))]
data[,":="(earliest_cr_line = NULL, sec_app_earliest_cr_line = NULL)]


data[, coborrower_inc := ifelse(!is.na(annual_inc_joint), annual_inc_joint - annual_inc, 0)]
data[, max_ind_annual_inc := pmax(annual_inc, coborrower_inc)]
data[, min_ind_annual_inc := ifelse(coborrower_inc == 0, annual_inc, pmin(annual_inc, coborrower_inc))]
data[, avg_annual_inc := ifelse(coborrower_inc == 0, annual_inc, (annual_inc+coborrower_inc)/2)]
data[, total_annual_inc := ifelse(coborrower_inc == 0, annual_inc, annual_inc_joint)]
data[, ":="(annual_inc = NULL, annual_inc_joint = NULL, coborrower_inc = NULL)]

data[,max_dti := pmax(dti, dti_joint, na.rm = T)]
data[,combined_dti := ifelse(is.na(dti_joint), dti, dti_joint)]
data[, ":="(dti = NULL, dti_joint = NULL)]

# okka
data[is.na(mths_since_last_delinq), mths_since_last_delinq := 999]
data[,mths_since_last_delinq := cut(mths_since_last_delinq, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)]
data$mths_since_last_delinq <- as.factor(data$mths_since_last_delinq)

data$mths_since_last_record[is.na(data$mths_since_last_record)] <- 999
data$mths_since_last_record <- cut(data$mths_since_last_record, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)
data$mths_since_last_record <- as.factor(data$mths_since_last_record)

data$mths_since_recent_bc_dlq[is.na(data$mths_since_recent_bc_dlq)] <- 999
data$mths_since_recent_bc_dlq <- cut(data$mths_since_recent_bc_dlq, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)
data$mths_since_recent_bc_dlq <- as.factor(data$mths_since_recent_bc_dlq)

data$mths_since_recent_revol_delinq[is.na(data$mths_since_recent_revol_delinq)] <- 999
data$mths_since_recent_revol_delinq <- cut(data$mths_since_recent_revol_delinq, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)
data$mths_since_recent_revol_delinq <- as.factor(data$mths_since_recent_revol_delinq)

# mindy
data[, avg_revol_bal := ifelse(is.na(revol_bal_joint), revol_bal, (revol_bal + revol_bal_joint)/2)]
data[, max_revol_bal := pmax(revol_bal, revol_bal_joint, na.rm = T)]
data[, ":=" (revol_bal = NULL, revol_bal_joint = NULL)]

# dennis
## last_fico_range_high, sec_app_fico_range_high
data[, max_fico_range_high := ifelse(is.na(sec_app_fico_range_high), last_fico_range_high, pmax(last_fico_range_high, sec_app_fico_range_high))]
data[, min_fico_range_high := ifelse(is.na(sec_app_fico_range_high), last_fico_range_high, pmin(last_fico_range_high, sec_app_fico_range_high))]
data[, avg_fico_range_high := ifelse(is.na(sec_app_fico_range_high), last_fico_range_high, (last_fico_range_high + sec_app_fico_range_high)/2)]
data[, ":="(last_fico_range_high = NULL, sec_app_fico_range_high = NULL)]

## last_fico_range_low, sec_app_fico_range_low
data[, max_fico_range_low := ifelse(is.na(sec_app_fico_range_low), last_fico_range_low, pmax(last_fico_range_low, sec_app_fico_range_low))]
data[, min_fico_range_low := ifelse(is.na(sec_app_fico_range_low), last_fico_range_low, pmin(last_fico_range_low, sec_app_fico_range_low))]
data[, avg_fico_range_low := ifelse(is.na(sec_app_fico_range_low), last_fico_range_low, (last_fico_range_low + sec_app_fico_range_low)/2)]
data[, ":="(last_fico_range_low = NULL, sec_app_fico_range_low = NULL)]

##  inq_last_6mths, sec_app_inq_last_6mths
data[, max_inq_last_6mths := ifelse(is.na(sec_app_inq_last_6mths), inq_last_6mths, pmax(inq_last_6mths, sec_app_inq_last_6mths))]
data[, min_inq_last_6mths := ifelse(is.na(sec_app_inq_last_6mths), inq_last_6mths, pmin(inq_last_6mths, sec_app_inq_last_6mths))]
data[, avg_inq_last_6mths := ifelse(is.na(sec_app_inq_last_6mths), inq_last_6mths, (inq_last_6mths + sec_app_inq_last_6mths)/2)]
data[, total_inq_last_6mths := ifelse(is.na(sec_app_inq_last_6mths), inq_last_6mths, (inq_last_6mths + sec_app_inq_last_6mths))]
data[, ":="(inq_last_6mths = NULL, sec_app_inq_last_6mths = NULL)]

## mort_acc ,sec_app_mort_acc
data[, max_mort_acc  := ifelse(is.na(sec_app_mort_acc), mort_acc, pmax(mort_acc, sec_app_mort_acc))]
data[, min_mort_acc := ifelse(is.na(sec_app_mort_acc), mort_acc, pmin(mort_acc, sec_app_mort_acc))]
data[, avg_mort_acc := ifelse(is.na(sec_app_mort_acc), mort_acc, (mort_acc + sec_app_mort_acc)/2)]
data[, total_mort_acc := ifelse(is.na(sec_app_mort_acc), mort_acc, (mort_acc + sec_app_mort_acc))]
data[, ":="(mort_acc = NULL, sec_app_mort_acc = NULL)]

## mths_since_last_major_derog, sec_app_mths_since_last_major_derog 
data[, max_mths_since_last_major_derog  := ifelse(is.na(sec_app_mths_since_last_major_derog), mths_since_last_major_derog, pmax(mths_since_last_major_derog, sec_app_mths_since_last_major_derog))]
data[, min_mths_since_last_major_derog := ifelse(is.na(sec_app_mths_since_last_major_derog), mths_since_last_major_derog, pmin(mths_since_last_major_derog, sec_app_mths_since_last_major_derog))]
data[, avg_mths_since_last_major_derog := ifelse(is.na(sec_app_mths_since_last_major_derog), mths_since_last_major_derog, (mths_since_last_major_derog + sec_app_mths_since_last_major_derog)/2)]
data[, total_mths_since_last_major_derog := ifelse(is.na(sec_app_mths_since_last_major_derog), mths_since_last_major_derog, (mths_since_last_major_derog + sec_app_mths_since_last_major_derog))]
data[, ":="(mths_since_last_major_derog = NULL, sec_app_mths_since_last_major_derog = NULL)]
data$max_mths_since_last_major_derog[is.na(data$max_mths_since_last_major_derog)] <- 999
data$max_mths_since_last_major_derog <- cut(data$max_mths_since_last_major_derog, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)
data$max_mths_since_last_major_derog <- as.factor(data$max_mths_since_last_major_derog)
data$min_mths_since_last_major_derog[is.na(data$min_mths_since_last_major_derog)] <- 999
data$min_mths_since_last_major_derog <- cut(data$min_mths_since_last_major_derog, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)
data$min_mths_since_last_major_derog <- as.factor(data$min_mths_since_last_major_derog)
data$avg_mths_since_last_major_derog[is.na(data$avg_mths_since_last_major_derog)] <- 999
data$avg_mths_since_last_major_derog <- cut(data$avg_mths_since_last_major_derog, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)
data$avg_mths_since_last_major_derog <- as.factor(data$avg_mths_since_last_major_derog)
data$total_mths_since_last_major_derog[is.na(data$total_mths_since_last_major_derog)] <- 999
data$total_mths_since_last_major_derog <- cut(data$total_mths_since_last_major_derog, breaks = c(0,60,120,180,240,10000),labels = c("0-5 yrs", "5-10 yrs", "10-15 yrs","15-20 yrs", "nil"), include.lowest = TRUE)
data$total_mths_since_last_major_derog <- as.factor(data$total_mths_since_last_major_derog)

## num_rev_accts, sec_app_num_rev_accts
data[, max_num_rev_accts  := ifelse(is.na(sec_app_num_rev_accts), num_rev_accts, pmax(num_rev_accts, sec_app_num_rev_accts))]
data[, min_num_rev_accts := ifelse(is.na(sec_app_num_rev_accts), num_rev_accts, pmin(num_rev_accts, sec_app_num_rev_accts))]
data[, avg_num_rev_accts := ifelse(is.na(sec_app_num_rev_accts), num_rev_accts, (num_rev_accts+ sec_app_num_rev_accts)/2)]
data[, total_num_rev_accts := ifelse(is.na(sec_app_num_rev_accts), num_rev_accts, (num_rev_accts + sec_app_num_rev_accts))]
data[, ":="( num_rev_accts= NULL, sec_app_num_rev_accts = NULL)]

## open_acc, sec_app_open_acc
data[, max_open_acc  := ifelse(is.na(sec_app_open_acc), open_acc, pmax(open_acc, sec_app_open_acc))]
data[, min_open_acc := ifelse(is.na(sec_app_open_acc), open_acc, pmin(open_acc, sec_app_open_acc))]
data[, avg_open_acc := ifelse(is.na(sec_app_open_acc), open_acc, (open_acc + sec_app_open_acc)/2)]
data[, total_open_acc := ifelse(is.na(sec_app_open_acc), open_acc, (open_acc + sec_app_open_acc))]
data[, ":="(open_acc= NULL, sec_app_open_acc = NULL)]

# revol_util, sec_app_revol_util
data[, max_revol_util  := ifelse(is.na(sec_app_revol_util), revol_util , pmax(revol_util, sec_app_revol_util))]
data[, min_revol_util := ifelse(is.na(sec_app_revol_util), revol_util , pmin(revol_util, sec_app_revol_util))]
data[, avg_revol_util  := ifelse(is.na(sec_app_revol_util), revol_util , (revol_util + sec_app_revol_util)/2)]
data[, total_revol_util  := ifelse(is.na(sec_app_revol_util), revol_util , (revol_util + sec_app_revol_util))]
data[, ":="(revol_util = NULL, sec_app_revol_util = NULL)]

# remove rows where emp_length have no values, but null either
data <- data[emp_length != ""]

# # convert string to factors
# data$loan_status <- as.factor(data$loan_status)
# data$home_ownership <- as.factor(data$home_ownership)
# data$hardship_flag <- as.factor(data$hardship_flag)
# data$emp_length <- as.factor(data$emp_length)
# data$disbursement_method <- as.factor(data$disbursement_method)
# data$application_type <- as.factor(data$application_type)
# data$addr_state <- as.factor(data$addr_state)

# check to make sure no more NA
clean_data <- na.omit(data)
colSums(is.na(clean_data))
dim(clean_data)
# still a good amount of data left 
# 1,187,848 out of 1,348,059 remaining

setcolorder(clean_data, order(names(clean_data))) # order variables alphabetically

fwrite(clean_data, "clean_data.csv")

x<-fread("clean_data.csv")
dim(x)
