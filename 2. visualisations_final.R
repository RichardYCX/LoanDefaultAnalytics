# ========================================================================================================
# Purpose:      Rscript for Data Visualisation on Full Loan Dataset
#=========================================================================================================

setwd("C:/Users/yangc/OneDrive - Nanyang Technological University/Uni/BC2406/Project/Data Visualisations")
library(data.table)
library(caTools)
library(ggplot2)
options(scipen = 10000000)

clean_data <- fread("clean_data.csv", stringsAsFactors = TRUE)
names(clean_data)

#=========================================================================================================
# Plot 1: Percentage of Loans by FICO Score
avg_fico_range <- cut(clean_data$avg_fico_range_high, breaks=c(min(clean_data$avg_fico_range_high), 500, 600, 660, 780, max(clean_data$avg_fico_range_high)),
                      include.lowest = T, right = T,
                      labels = c("very poor", "poor", "fair", "good", "excellent"))
clean_data[,avg_fico_range := avg_fico_range]

loan_by_plot <- ggplot(data = clean_data, aes(x = avg_fico_range)) +
  theme(text = element_text(size=20)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=scales::percent) +
  labs(title="percentage of loans by fico score",
       x="fico range",y="percentage of loans")
loan_by_plot
## high fico loaners makes up a majority of all approved loans, but fico score 500-600 users
## also takes up around 20%

#=========================================================================================================
# Plot 2: Default Rate against FICO Score
loan_by_fico_dt <- clean_data[, .(avg_fico_range, loan_status)]
loan_by_fico_dt <- loan_by_fico_dt[,{totwt = .N
.SD[,.(frac=.N/totwt),by=loan_status]
},by=avg_fico_range]
loan_by_fico_dt <- loan_by_fico_dt[loan_status == "Charged Off"]
loan_by_fico_plot <- ggplot(data = loan_by_fico_dt, aes(x = factor(avg_fico_range), y = frac, group = 1)) +
  geom_col() +
  geom_smooth(se=F, colour = "red", span = 1.5)+
  coord_cartesian(ylim = c(0, 1)) + 
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "None", axis.text.x = element_text())+
  labs(title="loan default rate by FICO score",
       x="FICO score",y="default rate")
loan_by_fico_plot
## Fico score 500-600 is actually a dangerous group to loan to. around 70% of all loans given 
## are defaulted from that group. 
## Should be careful about given out loans to that group

#=========================================================================================================
# Plot 3: Percentage of Loans by Employment Length
clean_data$emp_length <- factor(clean_data$emp_length, levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years",
                                                                  "5 years", "6 years", "7 years", "8 years", "9 years",
                                                                  "10+ years"))
loan_by_emplen_plot <- ggplot(data = clean_data, aes(x = emp_length)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title="percentage of loans by employment length",
       x="employment length",y="percentage of loans")
loan_by_emplen_plot
## Borrowers with more than 10 years of working experience make up 30% of all loans given out,
## it is the largest population by employment length. Borrowers of other employment lengths 
## make up about 5 to 10% each.

#=========================================================================================================
# Plot 4: Default Rate against Employment Length
loan_by_emplen_dt <- clean_data[, .(emp_length, loan_status)]
loan_by_emplen_dt <- loan_by_emplen_dt[,{totwt = .N
.SD[,.(frac=.N/totwt),by=loan_status]
},by=emp_length]
loan_by_emplen_dt <- loan_by_emplen_dt[loan_status == "Charged Off"]
loan_by_emplen_plot <- ggplot(data = loan_by_emplen_dt, aes(x = factor(emp_length), y = frac, group = 1)) +
  geom_col() +
  geom_smooth(se=F, colour = "red", span = 2)+
  coord_cartesian(ylim = c(0.18, 0.22)) + 
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "None", axis.text.x = element_text(angle=-40))+
  labs(title="loan default rate by emp length",
       x="employment length",y="default rate")
loan_by_emplen_plot
## number of loans is highly skewed towards >10 years employment 
## There is a slight decrease in default rate as lender's employment lengths increase.
## Lenders are right in giving out more loans to experienced workers, although the percentage
## of loans can be reshuffled, since the default rate drop is not skyhigh.

#=========================================================================================================
# Plot 5: Percentage of Loans by Number of Total Accounts
total_acc_range <- cut(clean_data$total_acc, breaks=c(0, 10, 20, 30, 40, 50, 60, max(clean_data$total_acc)),
                       include.lowest = T, right = T)
clean_data[,total_acc_range := total_acc_range]

loan_by_total_acc_plot <- ggplot(data = clean_data, aes(x = total_acc_range)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title="percentage of loans by number of total accounts",
       x="total accounts",y="percentage of loans")
loan_by_total_acc_plot
## most loans are given out to the 10-30 total accounts range, around 85%.

#=========================================================================================================
# Plot 6: Default Rate against Number of Total Accounts
loan_by_total_acc_dt <- clean_data[, .(total_acc_range, loan_status)]
loan_by_total_acc_dt <- loan_by_total_acc_dt[,{totwt = .N
.SD[,.(frac=.N/totwt),by=loan_status]
},by=total_acc_range]
loan_by_total_acc_dt <- loan_by_total_acc_dt[loan_status == "Charged Off"]
loan_by_total_acc_plot <- ggplot(data = loan_by_total_acc_dt, aes(x = factor(total_acc_range), y = frac, group = 1)) +
  geom_col() +
  geom_smooth(se=F, colour = "red", span = 2)+
  coord_cartesian(ylim = c(0.18, 0.22)) +   
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "None", axis.text.x = element_text(angle=-40))+
  labs(title="loan default rate by total number of accounts",
       x="total number of total accounts",y="default rate")
loan_by_total_acc_plot
## But the groups with the lowerest default rates are actually lenders with 30-40 accounts.
## Loan-approvers may be under the assumption that the more accounts the loan applicant have,
## the more likely he is to be able to replay the loans.
## Loan-approvers could assume that the number of accounts signify the borrower's financial 
## stablity. That's why he or she can have so many credit lines. 
## But that is actually not the case. That hypothesis is true from 0 to 40 accounts. But beyond that, the default rate actually
## peaks again.
## Beyond 40 accounts, there is an actually an increase in default rate. This could be because 
## these borrowers are frequent borrowers, thats why they require so many credit lines. 
## Which also implies higher risk since they are likely to borrow from many loans, which
## increases their chance of not paying back.
## Thus, higher number of total accounts does not always mean lower default rates.
## Loan-approvers should reevaluate their assumptions, an give out to loans
## to the group of people with 30-40 accounts instead. 

#=========================================================================================================
# Plot 7: Percentage of Loans by Percentage of Bank Accounts With > 75% of Limit
percent_bc_gt_75_range <- cut(clean_data$percent_bc_gt_75, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                              include.lowest = T, right = T)
clean_data[,percent_bc_gt_75_range := percent_bc_gt_75_range]

loan_by_acct_75_range_plot <- ggplot(data = clean_data, aes(x = percent_bc_gt_75_range)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title="percentage of loans by percentage of bank accounts with > 75% of limit",
       x="percentage of bank accounts with > 75% of limit",y="percentage of loans")
loan_by_acct_75_range_plot
## 25% of borrowers are without any bank accounts close to its limit. The other major group
## is people with 100% of bank accounts over the 75% credit limit, about 18%. Most likely borrowing for
## debt consolidation.

#=========================================================================================================
# Plot 8: Default Rate against Percentage of Bank Accounts With > 75% of Limit
loan_by_acct_75_range_dt <- clean_data[, .(percent_bc_gt_75_range, loan_status)]
loan_by_acct_75_range_dt <- loan_by_acct_75_range_dt[,{totwt = .N
.SD[,.(frac=.N/totwt),by=loan_status]
},by=percent_bc_gt_75_range]
loan_by_acct_75_range_dt <- loan_by_acct_75_range_dt[loan_status == "Charged Off"]
loan_by_acct_75_range_plot <- ggplot(data = loan_by_acct_75_range_dt, aes(x = factor(percent_bc_gt_75_range), y = frac, group = 1)) +
  geom_col() +
  geom_smooth(se=F, colour = "red", span = 2)+
  coord_cartesian(ylim = c(0.15, 0.28)) +  
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "None", axis.text.x = element_text(angle=-40))+
  labs(title="loan default rate by percentage of accounts over 75% credit limit",
       x="percentage of accounts over 75% credit limit",y="default rate")
loan_by_acct_75_range_plot
## there is a clear trend on how increasing percentage of bank accounts close to limit will 
## result in higher rate of default, but despite that, loans are still given out to a wide range 
## of people, with a good amount being given to people with > 50% of accounts close to that cap
## this could severely increase the number of defaults since we are giving loans to high risk 
## groups. Might want to rethink our selection criterion

#=========================================================================================================
# Plot 9: Percentage of Loans by Loan Amounts
loan_amount_range <- cut(clean_data$loan_amnt, breaks=c(0, 10000, 20000, 30000, 40000, 50000), right = F, dig.lab = 10)
clean_data[,loan_amount_range := loan_amount_range]

loan_by_loan_amount_range_plot <- ggplot(data = clean_data, aes(x = loan_amount_range)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title="percentage of loans by loan amounts",
       x="loan amounts",y="percentage of loans")
loan_by_loan_amount_range_plot
## majority of the loans are from 10,000 dollars to 20,000 dollars, making up about 40% of 
## all loans given out.

#=========================================================================================================
# Plot 10: Default Rate against Loan Amounts
loan_by_loan_amount_range_dt <- clean_data[, .(loan_amount_range, loan_status)]
loan_by_loan_amount_range_dt <- loan_by_loan_amount_range_dt[,{totwt = .N
.SD[,.(frac=.N/totwt),by=loan_status]
},by=loan_amount_range]
loan_by_loan_amount_range_dt <- loan_by_loan_amount_range_dt[loan_status == "Charged Off"]
loan_by_loan_amount_range_plot <- ggplot(data = loan_by_loan_amount_range_dt, aes(x = loan_amount_range, y = frac, group = 1)) +
  geom_col() +
  geom_smooth(se=F, colour = "red", span = 2)+
  coord_cartesian(ylim = c(0.15, 0.28)) +
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "None", axis.text.x = element_text(angle=-40))+
  labs(title="loan default rate by loan amount",
       x="loan amount",y="default rate")
loan_by_loan_amount_range_plot

## With higher loan amounts, there is larger sums of money to be gained from interests 
## collected. But at the same time, there is also more loans that go default. It is a delicate
## game of which one outweights the other. 
## but given the data, there is too significant a difference in default rates between 
## 10,000 - 20,000 loans and 20-30k loans. Hence, if the company wants to continue to reap 
## rewards from this group of borrowers, we could consider lending more money to that group.
## It is more difficult to make the same judgement call for the 40-50k loans since there is 
## too little samples to make an informed decision desite the data showing lower rate of 
## default than the other groups

#=========================================================================================================
# Plot 11: Percentage of Loans by combined DTI
clean_data <- clean_data[combined_dti>=0]
combined_dti_range <- cut(clean_data$combined_dti, breaks=c(-Inf, 0, 10, 20, 30, 40, 70), right = F)
clean_data[,combined_dti_range := combined_dti_range]

loan_by_combined_dti_range_plot <- ggplot(data = clean_data, aes(x = combined_dti_range)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title="percentage of loans by debt to income ratio of lender",
       x="debt to income ratio",y="percentage of loans")
loan_by_combined_dti_range_plot
## majority of borrowers have low debt to income ratio of 0-30%, making up around 90% of all
## loans.

#=========================================================================================================
# Plot 12: Default Rate against Debt To Income Ratio
loan_by_combined_dti_range_dt <- clean_data[, .(combined_dti_range, loan_status)]
loan_by_combined_dti_range_dt <- loan_by_combined_dti_range_dt[,{totwt = .N
.SD[,.(frac=.N/totwt),by=loan_status]
},by=combined_dti_range]
loan_by_combined_dti_range_dt <- loan_by_combined_dti_range_dt[loan_status == "Charged Off"]
loan_by_combined_dti_range_plot <- ggplot(data = loan_by_combined_dti_range_dt, aes(x = combined_dti_range, y = frac, group = 1)) +
  geom_col() +
  geom_smooth(se=F, colour = "red", span = 1)+
  coord_cartesian(ylim = c(0.15, 0.70)) +
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "None", axis.text.x = element_text(angle=-40))+
  labs(title="loan default rate by debt to income ratio of lender",
       x="debt to income ratio",y="default rate")
loan_by_combined_dti_range_plot
## There is a strong correlation between dti ratio to default rate. Lenders should be wary of
## lending to anyone with > 40% dti ratio because there is a high chance of default beyond 
## this percentage

#=========================================================================================================
# Plot 13: Loan Amounts by Combined DTI
combined_dt_to_loan_amount_plot <- ggplot(data = clean_data, aes(x = combined_dti_range, y = loan_amount_range)) +
  geom_count(aes(color = ..n..)) +
  scale_size_area(max_size = 10) +
  labs(title="loan amounts by combined dti",
       x="combined dti",y="loan amounts")
combined_dt_to_loan_amount_plot

# There is a fair share of people in every category. Some have high dti but
# low loan amounts, some are he opposite. So which factor is more important? It is difficult
# to determine which factor is more important when everything is compared in a vacuum. 
# That's where our statistical model comes into play. It is able to compare all variables at
# once, and determine which is the most important factor amongst all other factors. This way,
# you do not need to weigh the variables yourself. This is especially helpful when there are
# multiple variables with oppositing recommendations. For example, someone with high dti but
# borrowing a small amount of money. It is difficult to judge, but with our tool, you will be
# able to simply key in the information on hand, and get the best recommendation instantly.
# Best thing is, it not only take into account the two factors just mentioned, but 80 more 
# factors that are commonly gathered from borrowers. This helps you make a holistic judgement.

#=================== Text Mining =================================================================================
# Text Mining on emp_title
library(readtext)
library(quanteda)
data <- readtext("emp_title.csv")
colnames(data)[2] <- "loan_status"
data$loan_status[data$loan_status == "Does not meet the credit policy. Status:Charged Off"] <- "Charged Off"
data$loan_status[data$loan_status == "Does not meet the credit policy. Status:Fully Paid"] <- "Fully Paid"

unique(data$loan_status)
names(data)
table(data$emp_title)
data <- subset(data, loan_status == "Charged Off" | loan_status == "Fully Paid",
               select = c(loan_status, emp_title))
doc.corpus <- corpus(data, text_field = "emp_title")
summary(doc.corpus)
doc.dfm <- dfm(doc.corpus, remove_numbers = TRUE, 
               remove_punct = TRUE,
               tolower = TRUE,
               remove = stopwords("english"))
topfeatures(doc.dfm, 20)
set.seed(2)
textplot_wordcloud(doc.dfm, max_words = 20, max_size = 9, min_size = 1, rotation = 0)

charged_off_data <- subset(data, loan_status == "Charged Off",
                           select = emp_title)
charged.off.doc.corpus <- corpus(charged_off_data, text_field = "emp_title")
charged.off.doc.dfm <- dfm(charged.off.doc.corpus, remove_numbers = TRUE, 
                           remove_punct = TRUE,
                           tolower = TRUE,
                           remove = stopwords("english"))
topfeatures(charged.off.doc.dfm, 20)
set.seed(2)
textplot_wordcloud(charged.off.doc.dfm, max_words = 20, max_size = 9, min_size = 1, rotation = 0)

fully_paid_data <- subset(data, loan_status == "Fully Paid",
                          select = emp_title)
fully.paid.doc.corpus <- corpus(fully_paid_data, text_field = "emp_title")
fully.paid.doc.dfm <- dfm(fully.paid.doc.corpus, remove_numbers = TRUE, 
                          remove_punct = TRUE,
                          tolower = TRUE,
                          remove = stopwords("english"))

topfeatures(fully.paid.doc.dfm, 20)
set.seed(2)
textplot_wordcloud(fully.paid.doc.dfm, max_words = 20, max_size = 9, min_size = 1, rotation = 0)

## The conclusion here is: there is no correlation in the type of occupations and default 
## rates
#=================== END =================================================================================
