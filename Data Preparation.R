######################################################
#################### by Shu Liu ######################
############ shutel at hotmail dot com ###############
################### 07/22/2016 #######################
################# DataVisual project #################
######################################################

# Preparartion of valid data
loandf_12_13 <- read.csv('./data/LoanStats12-13.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_12_13 <- loandf_12_13[-c(188126, 188125, 188124), ] # remove the last three useless rowa
loandf_14 <- read.csv('./data/LoanStats14.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_14 <- loandf_14[-c(235632, 235631, 235630), ]
loandf_16Q1 <- read.csv('./data/LoanStats16Q1.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_16Q1 <- loandf_16Q1[-c(133890, 133889, 133888), ]
loandf_org1 <- rbind(loandf_12_13, loandf_14, loandf_16Q1)
loandf_org1$issue_d <- as.Date(gsub("^", "01-", loandf_org1$issue_d), format="%d-%b-%y")

## The data 2007-2011 and 2015 are disordered, tidy it separately. (That's because LC is a child at that time)
loandf_07_11 <- read.csv('./data/LoanStats07-11.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_07_11 <- loandf_07_11[-c(42540, 42539, 42538), ]
loandf_15 <- read.csv('./data/LoanStats15.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_15 <- loandf_15[-c(421098, 421097, 421096), ]

loandf_org2 <- rbind(loandf_07_11, loandf_15)
loandf_org2$issue_d <- as.Date(gsub("^", "01-", loandf_org2$issue_d), format="%d-%y-%b") # fomat the date


loandf_org <- rbind(loandf_org1, loandf_org2)

# 41 variables, selection criteria: limited missingness, understandable, meaningful 
var_name = c('id', 'member_id', 'loan_amnt', 'term', 'int_rate', 'installment', 'grade', 'sub_grade', 'emp_title', 
             'emp_length', 'home_ownership',	'annual_inc',	'verification_status', 'issue_d', 'loan_status', 'purpose', 
             'title', 'addr_state', 'dti', 'earliest_cr_line', 'open_acc', 'total_acc', 'total_pymnt', 'total_rec_prncp', 
             'total_rec_int', 'open_il_6m', 'open_il_12m', 'open_il_24m', 'mths_since_rcnt_il', 'total_bal_il', 'il_util', 
             'all_util', 'total_rev_hi_lim', 'mort_acc',	'mths_since_recent_bc', 'mths_since_recent_bc_dlq', 'num_actv_bc_tl', 
             'num_op_rev_tl', 'tot_hi_cred_lim', 'total_bal_ex_mort', 'total_bc_limit')
## Notice: ('open_il_6m', 'open_il_12m', 'open_il_24m', 'mths_since_rcnt_il', 'total_bal_il', 'il_util', 'all_util') 
## ('total_rev_hi_lim', 'mort_acc',	'mths_since_recent_bc', 'mths_since_recent_bc_dlq', 'num_actv_bc_tl', 'num_op_rev_tl', 
## 'tot_hi_cred_lim', 'total_bal_ex_mort', 'total_bc_limit')
## have a large percetage of missingness


loandf <- loandf_org %>% select(one_of(var_name))

# Save the pre-selected dataset
save(loandf, file = './Lending_Club/loandf.RData')


# growth/ geograhical / purpose/ term, grade, interest/ grade_sub_grade/  *
# proportion of different grades in one grade * bar chart
# emply_length & amount & grade (interest rate, around fixed)
# home_ownship & amount & grade
# annual income & amount & veridication (income source)
# loan status percentage for different grade
### purpose/title are the same thing
# dti: & grade = excluding the Lc and morgage debt payment/income
# earliest_crm_line: have to format the date/ => years
# open_acc number(currently open) / total_acc number(total, open and close)
# total_pymnt = total_rec_principle + total_rec_interest: interest/principal
# mort_acc: number of mortgages accounts

# mths_since_rcnt_il	Months since most recent installment accounts opened
# mths_since_recent_bc	Months since most recent bankcard account opened.
# num_op_rev_tl	Number of open revolving accounts

# num_actv_bc_tl	Number of currently active bankcard accounts
# tot_hi_cred_lim	Total high credit/credit limit
# total_bal_ex_mort	Total credit balance excluding mortgage
# total_bc_limit	Total bankcard high credit/credit limit


##############################
# correlation map: employelength - annual income - higher amount? (- dti - ... -)
# missingness map (for left variables)
# higher income - verification 
# homeownership - employelength - mortgage amount

# feature engineering: issue_date - earliest_crm_line: (check the format of date for crrm_line)

