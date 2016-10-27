library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(treemap)


# Download data
load("loandf.RData")

str(loandf)
# Preprocessing -- standardize date format
loandf$issue_d <- as.Date(gsub("^", "01-", loandf$issue_d), format="%d-%b-%y")

# Loan Amount and volume changs from 2007 to 2015
amt_df <- loandf %>%
  select(issue_d, loan_amnt) %>%
  group_by(issue_d) %>%
  summarise(amount = sum(loan_amnt), volume = n(), avgAmt = amount/volume) 
  
## changes of amount
g_amt <- ggplot(amt_df, aes(x = issue_d))
g_amt + geom_line(aes(y = amount), color = 'red') + labs(title = 'Loan amount by month', x = 'Date Issued', y = 'Amount($)') 

## changes of volume
g_vol <- ggplot(amt_df, aes(x = issue_d))
g_vol + geom_line(aes(y = volume), color = 'red') + labs(title = 'Loan volume by month', x = 'Date Issued', y = 'Volume')

## changes of average amount per loan
g_avgAmt <- ggplot(amt_df, aes(x = issue_d, y = avgAmt))
g_avgAmt + geom_point(color = 'cadetblue4', size = 0.5) + geom_smooth(color = 'red', linetype = 'dashed', method = lm, size = 0.7, se = FALSE) + labs(title = 'Average loan amount by month', x = 'Date Issued', y = 'avgAmount')

#### Problems here!

#loan issued locations by volume or amount
locVol_df <- select(loandf, addr_state)
locVol_df <- locVol_df %>%
  group_by(addr_state) %>%
  dplyr::summarise(value = n())

locAmt_df <- select(loandf, addr_state, loan_amnt)
locAmt_df$loan_amnt <- as.numeric(locAmt_df$loan_amnt)
locAmt_df <- locAmt_df %>%
  group_by(addr_state) %>%
  dplyr::summarise(value = sum(loan_amnt, na.rm = TRUE))

addr_state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

region =  c("alabama",        "alaska",         "arizona",        "arkansas",       "california",     "colorado",      
                "connecticut",    "delaware",       "florida",        "georgia",        "hawaii",         "idaho",
                "illinois",       "indiana",        "iowa",           "kansas",         "kentucky",       "louisiana",     
                "maine",          "maryland",       "massachusetts",  "michigan",       "minnesota",      "mississippi",   
                "missouri",       "montana",        "nebraska",       "nevada",         "new hampshire",  "new jersey",    
                "new mexico",     "new york",       "north carolina", "north dakota",   "ohio",           "oklahoma",      
                "oregon",         "pennsylvania",   "rhode island",   "south carolina", "south dakota",   "tennessee",     
                "texas",          "utah",           "vermont",        "virginia",       "washington",     "west virginia", 
                "wisconsin",      "wyoming")
nameTrans <-  data.frame(addr_state, region)
locVol_df <- inner_join(locVol_df, nameTrans, by = 'addr_state')
locAmt_df <- inner_join(locAmt_df, nameTrans, by = 'addr_state')

state_choropleth(locVol_df, title = 'Loan volume by state', num_colors = 9)
state_choropleth(locAmt_df, title = 'Loan amount by state', num_colors = 9) 
## how to draw a map
 

# Relation between return rate and default rate and grade and term
## Get numeric value in term columns
deft_df <- loandf %>% 
  select(grade, loan_status, term) %>%
  mutate(term = ifelse(term == ' 36 months', 36, 60))

deft_df <- deft_df[!deft_df$loan_status %in% c('', 'Does not meet the credit policy. Status:Fully Paid', 'Does not meet the credit policy. Status:Charged Off'), ]
ggplot(data = deft_df, aes(x = grade)) + geom_bar(fill = 'dodgerblue')
ggplot(data = deft_df, aes(x = loan_status)) + geom_bar(fill = 'dodgerblue3') + facet_wrap( ~ grade)


## Subgroup 1: Current & Fully paid
deft_df1 <- filter(deft_df, loan_status %in% c('Current', 'Fully Paid'))
ggplot(data = deft_df1, aes(x = grade)) + geom_bar(fill = 'chartreuse3') + facet_wrap( ~ loan_status)

## Subgroup 2: In Grace Period & Late(16-30 days) & Late(31-120 days)
deft_df2 <- filter(deft_df, loan_status %in% c('In Grace Period', 'Late (16-30 days)', 'Late (31-120 days)'))
ggplot(data = deft_df2, aes(x = grade)) + geom_bar(fill = 'darkgoldenrod1') + facet_wrap( ~ loan_status)

## Subgroup 3: Default & Charged off
deft_df3 <- filter(deft_df, loan_status %in% c('Charged Off', 'Default'))
ggplot(data = deft_df3, aes(x = grade)) + geom_bar(fill = 'firebrick2')



# Grade with factors including Homwowner, Income percent, ...... etc. fico;



# Purposes Analysis:
prp_df <- loandf %>% select(purpose, loan_amnt, term, emp_length) 
prp_df <- prp_df %>%
  group_by(purpose) %>%
  dplyr::summarize(loan_volume = n(), average_amnt = sum(as.numeric(loan_amnt)/n()))
prp_df <- prp_df[!prp_df$purpose %in% c(''), ]
  
treemap(prp_df, 
       index = 'purpose', 
       vSize = 'loan_volume',
       vColor = 'average_amnt',
       type = 'value',
       palette= 'Oranges',
       algorithm="pivotSize",
       sortID="-size")


   