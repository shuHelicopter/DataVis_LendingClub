######################################################
#################### by Shu Liu ######################
############ shutel at hotmail dot com ###############
################### 07/22/2016 #######################
### DataVisual project @ NYC Data Science Academy ####
######################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(treemap)
library(ggthemes)


# Download data
load("loandf.RData")

# Loan Amount and volume changs from 2007 to 2016
amt_df <- loandf %>%
  select(issue_d, loan_amnt) %>%
  group_by(issue_d) %>%
  dplyr::summarise(amount = sum(loan_amnt), volume = n(), avgAmt = amount/volume) 
  
# Changes of amount
g_amt <- ggplot(amt_df, aes(x = issue_d))
g_amt + geom_line(aes(y = amount), color = 'red') + 
        labs(title = 'Loan Amount by Month', x = 'Date Issued', y = 'Amount($)') + 
        theme_linedraw()

# Changes of volume
g_vol <- ggplot(amt_df, aes(x = issue_d))
g_vol + geom_line(aes(y = volume), color = 'red') + 
        labs(title = 'Loan Volume by Month', x = 'Date Issued', y = 'Volume') + 
        theme_linedraw()

# Grade, term, volume, year
## Extract 'year' from 'date'
loandf$issue_yr <- format(loandf$issue_d, '20%y')

gtvy_df <- loandf %>% select(issue_yr, grade, term)
gtvy_df <- gtvy_df[complete.cases(gtvy_df),]
gtvy_df <- gtvy_df[!gtvy_df$issue_yr == 2016,]
gtvy_df$term <- as.character(gtvy_df$term)

g_gtvy <- ggplot(gtvy_df, aes(x = issue_yr))
g_gtvy + geom_bar(aes(fill = grade)) + 
         facet_grid( ~ term) + 
         labs(title = 'Loan Volume by Year', x = 'Issued Year', y = 'Volume') + 
         theme_bw()



## Ghanges of average amount per loan
g_avgAmt <- ggplot(amt_df, aes(x = issue_d, y = avgAmt))
g_avgAmt + geom_point(color = 'cadetblue4', size = 0.5) + 
           geom_smooth(color = 'red', linetype = 'dashed', size = 0.7, se = FALSE) +
           labs(title = 'Average Loan Amount by Month', x = 'Date Issued', y = 'avgAmount') + 
           theme_bw() 



# Loan issued locations by volume or amount
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

state_choropleth(locVol_df, title = 'Loan Volume by State', num_colors = 9) 
state_choropleth(locAmt_df, title = 'Loan Amount by State', num_colors = 9)
 

# Relation between return rate and default rate and grade and term
## Get numeric value in term columns
deft_df <- loandf %>% select(grade, loan_status, term)
deft_df <- deft_df[!deft_df$loan_status %in% c('', 'Does not meet the credit policy. Status:Fully Paid', 'Does not meet the credit policy. Status:Charged Off'), ]

g_deft = ggplot(data = deft_df, aes(x = grade))
g_deft + geom_bar(fill = 'dodgerblue')

g_deft2 = ggplot(data = deft_df, aes(x = loan_status))
g_deft2 + ggeom_bar(fill = 'dodgerblue3') + facet_wrap( ~ grade)


## Subgroup 1: Current & Fully paid
deft_df1 <- filter(deft_df, loan_status %in% c('Current', 'Fully Paid'))

g_deftSb1 = ggplot(data = deft_df1, aes(x = grade)) 
g_deftSb1 + geom_bar(fill = 'chartreuse3') + 
            facet_wrap( ~ loan_status)

## Subgroup 2: In Grace Period & Late(16-30 days) & Late(31-120 days)
deft_df2 <- filter(deft_df, loan_status %in% c('In Grace Period', 'Late (16-30 days)', 'Late (31-120 days)'))

g_deftSb2 = ggplot(data = deft_df2, aes(x = grade)) 
g_deftSb2 + geom_bar(fill = 'darkgoldenrod1') + 
            facet_wrap( ~ loan_status)

## Subgroup 3: Default & Charged off
deft_df3 <- filter(deft_df, loan_status %in% c('Charged Off', 'Default'))
g_deftSb3 = ggplot(data = deft_df3, aes(x = grade))
g_deftSb3 + geom_bar(fill = 'firebrick2')

# Purposes of loans
prp_df <- select(loandf, purpose, loan_amnt)
prp_df <- prp_df %>%
  group_by(purpose) %>%
  dplyr::summarise(volume = n(), average_amnt = sum(as.numeric(loan_amnt), rm.na = TRUE)/n())

prp_df <- prp_df[!prp_df$purpose == '',]

treemap(prp_df, 
       index = 'purpose', 
       vSize = 'volume', 
       vColor = 'average_amnt',
       range = c(6000, 16000),
       type = 'manual',
       palette = c('yellow', 'green', 'orange', 'orange2', 'firebrick'),
       algorithm = 'pivotSize',
       sortID = '-size',
       title = 'Purposes of Loans',
       title.legend = 'Avg_Amnt',
       fontsize.labels = 12,
       fontsize.legend = 8,
       fontface.labels = 1,
       position.legend = 'right',
       force.print.labels = TRUE,
       border.col = 'white')

# Term, interest rate and grade
loandf$int_rate = as.numeric(sub("%", "", loandf$int_rate))
tig_df <- select(loandf, int_rate, grade, term, issue_yr)

# Term, interest rate and grade
tig_df <- tig_df[tig_df$issue_yr %in% c(2010, 2011, 2012, 2013, 2014, 2015), ]
g_tig <- ggplot(tig_df, aes(grade, int_rate)) 
g_tig + geom_boxplot(outlier.size = 0.5, color = 'red') +
        facet_grid(term ~ issue_yr) + 
        labs(title = 'Interes Rate Distribution by Grade', x = 'Grade', y = 'Interest Rate(%)')

