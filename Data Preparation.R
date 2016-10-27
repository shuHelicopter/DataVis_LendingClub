library(dplyr)
# Preparartion of valid data
loandf_07_11 <- read.csv('data/LoanStats07-11.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_12_13 <- read.csv('./data/LoanStats12-13.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_14 <- read.csv('./data/LoanStats14.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_15 <- read.csv('./data/LoanStats15.csv', stringsAsFactors = FALSE, header = TRUE)
loandf_16Q1 <- read.csv('./data/LoanStats16Q1.csv', stringsAsFactors = FALSE, header = TRUE)
loandf <- rbind(loandf_07_11, loandf_12_13, loandf_14, loandf_15, loandf_16Q1)

# 41 variables
var_name = c('id', 'member_id', 'loan_amnt', 'term', 'int_rate', 'installment', 'grade', 'sub_grade', 'emp_title', 'emp_length', 'home_ownership',	'annual_inc',	'verification_status', 'issue_d', 'loan_status', 'purpose', 'title', 
             'addr_state', 'dti', 'earliest_cr_line', 'open_acc', 'total_acc', 'total_pymnt', 'total_rec_prncp', 'total_rec_int', 'open_il_6m', 'open_il_12m', 'open_il_24m', 'mths_since_rcnt_il', 'total_bal_il', 'il_util', 'all_util',
             'total_rev_hi_lim', 'mort_acc',	'mths_since_recent_bc', 'mths_since_recent_bc_dlq', 'num_actv_bc_tl', 'num_op_rev_tl', 'tot_hi_cred_lim', 'total_bal_ex_mort', 'total_bc_limit')

loandf <- loandf %>% select(one_of(var_name))

# Preprocessing -- standardize date and term format
loandf$issue_d <- as.Date(gsub("^", "15-", loandf$issue_d), format="%d-%b-%y")
loandf <- mutate(loandf, term = ifelse(term == ' 36 months', 36, 60))

save(loandf, file = 'loandf.RData')
