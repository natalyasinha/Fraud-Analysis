install.packages("openxlsx")

library(openxlsx)
library(dplyr)
library(ggplot2)
library(scales)
library(varhandle)
library(xts)
library(RcppRoll)


setwd("C:/Users/Natalya/Fraud Analytics")
card_data <- read.xlsx("card payments.xlsx", 1)
str(card_data)
card_data$date <- convertToDate(card_data$date, origin = "1900-01-01")
card_data$merchnum <- ifelse(card_data$merchnum==""|card_data$merchnum=="0",NA,card_data$merchnum)

## Find the most common merchant_num and zip for a particular card number. 
#Replace missing zips and merchant_nums
a <- card_data %>% filter(!is.na(merch.zip)) %>% group_by(cardnum) %>% 
  count(merch.zip,sort=TRUE) %>% slice(1) %>% select(cardnum,merch.zip)

colnames(a) <- c("cardnum","common_merch_zip")
b <- card_data  %>% filter(!is.na(merchnum)) %>% group_by(cardnum) %>% 
  count(merchnum,sort=TRUE) %>% slice(1)%>% select(cardnum,merchnum)

colnames(b) <- c("cardnum","common_merch_num")

card_data_new <- card_data %>% left_join(a,by=c("cardnum"))
card_data_new <- card_data_new %>% left_join(b,by=c("cardnum"))

# if merch.zip is missing then replace it with common_merch_zip value for a particular card number
card_data_new$merch.zip <- ifelse(is.na(card_data_new$merch.zip),card_data_new$common_merch_zip,card_data_new$merch.zip)
#sum(is.na(card_data_new$merch.zip))

# if merchnum is missing then replace it with common_merch_num value for a particular card number
card_data_new$merchnum <- ifelse(is.na(card_data_new$merchnum),card_data_new$common_merch_num,card_data_new$merchnum)


## Find the most common merchant_num and state for each zip
a <- card_data %>% filter(!is.na(merch.zip)) %>% group_by(merch.zip) %>% 
  count(merchnum,sort=TRUE) %>% slice(1) %>% select(merch.zip,merchnum)
colnames(a) <- c("merch.zip","common_merch_num_zip")

b <- card_data %>% filter(!is.na(merch.zip)) %>% group_by(merch.zip) %>% 
  count(merch.state,sort=TRUE) %>% slice(1) %>% select(merch.zip,merch.state)
colnames(b) <- c("merch.zip","common_merch_state")

card_data_new <- card_data_new %>% left_join(a,by=c("merch.zip"))
card_data_new$merchnum <- ifelse(is.na(card_data_new$merchnum),card_data_new$common_merch_num_zip,card_data_new$merchnum)

card_data_new <- card_data_new %>% left_join(b,by=c("merch.zip"))
card_data_new$merch.state <- ifelse(is.na(card_data_new$merch.state),card_data_new$common_merch_state,card_data_new$merch.state)

chk <- card_data_new[1:100,]
# card num as the entity
library(data.table)
all_date <- setDT(card_data_new)[, list(date=seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by='1 day')) , cardnum]
daily_data <- card_data_new %>% group_by(cardnum,date) %>% summarize(n = n(),
                                                            no.of.days = length(unique(date)),
                                                            avg.amt = mean(amount),
                                                            total.amount=sum(amount)
                                                            ) %>% arrange(date)
new_tb <- all_date %>% left_join(daily_data,by=c("date","cardnum"))
new_tb$n <- ifelse(is.na(new_tb$n),0,new_tb$n)
new_tb$avg.amt <- ifelse(is.na(new_tb$avg.amt),0,new_tb$avg.amt)
new_tb$total.amount <- ifelse(is.na(new_tb$total.amount),0,new_tb$total.amount)
new_tb$no.of.days <- ifelse(is.na(new_tb$no.of.days),0,new_tb$no.of.days)


### 7 days
new_tb_7 <- new_tb %>%
  group_by(cardnum) %>%
  mutate(no_tx_7days = abs(roll_sum(n, 8, align = "right", fill = 0) ),
         avg_amt_7days = abs(roll_sum(avg.amt, 8, align = "right", fill = 0) ),
         tot_amt_7days = abs(roll_sum(total.amount, 8, align = "right", fill = 0) ),
         tot_days_7days = abs(roll_sum(no.of.days, 8, align = "right", fill = 0))) %>% 
        select(date, cardnum,no_tx_7days,avg_amt_7days,tot_amt_7days,tot_days_7days)
new_tb_7 <- new_tb_7 %>% inner_join(new_tb,by=c("cardnum","date"))
new_tb_7$no_tx_7days <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$no_tx_7days-new_tb_7$n,new_tb_7$no_tx_7days)
new_tb_7$tot_amt_7days <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$tot_amt_7days-new_tb_7$total.amount,new_tb_7$tot_amt_7days)
new_tb_7$avg_amt_7days <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$tot_amt_7days/new_tb_7$no_tx_7days,new_tb_7$avg_amt_7days)
new_tb_7$tot_days_7days <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$tot_days_7days-new_tb_7$no.of.days,new_tb_7$tot_days_7days)
new_tb_7 <- new_tb_7[,-c(7:10)]

final_df <- card_data_new %>% inner_join(new_tb_7,by=c("cardnum","date"))
final_df.0 <- final_df[final_df$no_tx_7days == 0,]
final_df.1 <- final_df[!final_df$no_tx_7days == 0,]
# details.5142148452 <- final_df[final_df$cardnum == 5142148452,]

?rollapply
res <- final_df.1 %>% 
  group_by(cardnum) %>% 
  arrange(date) %>% filter(date>as.Date('2010-01-07')) %>%
  mutate(uzip_7=rollapply(merch.zip, width=no_tx_7days, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'),
         umerch_7=rollapply(merchnum, width=no_tx_7days, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'))
res1 <- final_df.1 %>% 
  group_by(cardnum, date) %>% 
  arrange(date) %>% filter(date<=as.Date('2010-01-07'))
final_df.0$uzip_7 <- 0
final_df.0$umerch_7 <- 0
str(final_df.0)
str(res)
final_df <- rbind(as.data.frame(res),final_df.0)
final_df <- final_df %>% 
  arrange(record.number)

### 3 day
new_tb_3 <- new_tb %>%
  group_by(cardnum) %>%
  mutate(no_tx_3days = abs(roll_sum(n, 4, align = "right", fill = 0) ),
         avg_amt_3days = abs(roll_sum(avg.amt, 4, align = "right", fill = 0) ),
         tot_amt_3days = abs(roll_sum(total.amount, 4, align = "right", fill = 0) ),
         tot_days_3days = abs(roll_sum(no.of.days, 4, align = "right", fill = 0))) %>% 
  select(date, cardnum,no_tx_3days,avg_amt_3days,tot_amt_3days,tot_days_3days)

new_tb_3 <- new_tb_3 %>% inner_join(new_tb,by=c("cardnum","date"))
new_tb_3$no_tx_3days <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$no_tx_3days-new_tb_3$n,new_tb_3$no_tx_3days)
new_tb_3$tot_amt_3days <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$tot_amt_3days-new_tb_3$total.amount,new_tb_3$tot_amt_3days)
new_tb_3$avg_amt_3days <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$tot_amt_3days/new_tb_3$no_tx_3days,new_tb_3$avg_amt_3days)
new_tb_3$tot_days_3days <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$tot_days_3days-new_tb_3$no.of.days,new_tb_3$tot_days_3days)
new_tb_3 <- new_tb_3[,-c(7:10)]

final_df <- final_df %>% inner_join(new_tb_3,by=c("cardnum","date"))

final_df.0 <- final_df[final_df$no_tx_3days == 0,]
final_df.1 <- final_df[!final_df$no_tx_3days == 0,]

res <- final_df.1 %>% 
  group_by(cardnum) %>% 
  arrange(date) %>% filter(date>as.Date('2010-01-04')) %>%
  mutate(uzip_3=rollapply(merch.zip, width=no_tx_3days, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'),
         umerch_3=rollapply(merchnum, width=no_tx_3days, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'))
res1 <- final_df.1 %>% 
  group_by(cardnum, date) %>% 
  arrange(date) %>% filter(date<=as.Date('2010-01-04'))
res1$uzip_3 <- 0
res1$umerch_3 <- 0

# final_df <- rbind(as.data.frame(res1),as.data.frame(res))

final_df.0$uzip_3 <- 0
final_df.0$umerch_3 <- 0
str(final_df.0)
# str(final_df)
final_df <- rbind(as.data.frame(res1),as.data.frame(res),final_df.0)
final_df <- final_df %>% 
  arrange(record.number)



### 1 day
new_tb_1 <- new_tb %>%
  group_by(cardnum) %>%
  mutate(no_tx_1days = abs(roll_sum(n, 2, align = "right", fill = 0) ),
         avg_amt_1days = abs(roll_sum(avg.amt, 2, align = "right", fill = 0) ),
         tot_amt_1days = abs(roll_sum(total.amount, 2, align = "right", fill = 0) ),
         tot_days_1days = abs(roll_sum(no.of.days, 2, align = "right", fill = 0))) %>% 
  select(date, cardnum,no_tx_1days,avg_amt_1days,tot_amt_1days,tot_days_1days)

new_tb_1 <- new_tb_1 %>% inner_join(new_tb,by=c("cardnum","date"))
new_tb_1$no_tx_1days <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$no_tx_1days-new_tb_1$n,new_tb_1$no_tx_1days)
new_tb_1$tot_amt_1days <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$tot_amt_1days-new_tb_1$total.amount,new_tb_1$tot_amt_1days)
new_tb_1$avg_amt_1days <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$tot_amt_1days/new_tb_1$no_tx_1days,new_tb_1$avg_amt_1days)
new_tb_1$tot_days_1days <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$tot_days_1days-new_tb_1$no.of.days,new_tb_1$tot_days_1days)
new_tb_1 <- new_tb_1[,-c(7:10)]


final_df <- final_df %>% inner_join(new_tb_1,by=c("cardnum","date"))

final_df.0 <- final_df[final_df$no_tx_1days == 0,]
final_df.1 <- final_df[!final_df$no_tx_1days == 0,]


res <- final_df.1 %>% 
  group_by(cardnum) %>% 
  arrange(date) %>% filter(date>as.Date('2010-01-02')) %>%
  mutate(uzip_1=rollapply(merch.zip, width=no_tx_1days, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'),
         umerch_1=rollapply(merchnum, width=no_tx_1days, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'))
res1 <- final_df.1 %>% 
  group_by(cardnum, date) %>% 
  arrange(date) %>% filter(date<=as.Date('2010-01-02'))
res1$uzip_1 <- 0
res1$umerch_1 <- 0

# final_df <- rbind(as.data.frame(res1),as.data.frame(res))

final_df.0$uzip_1 <- 0
final_df.0$umerch_1 <- 0
str(final_df.0)
# str(final_df)
final_df <- rbind(as.data.frame(res1),as.data.frame(res),final_df.0)
final_df <- final_df %>% 
  arrange(record.number)



# merchant num as the entity
all_date <- setDT(card_data_new)[, list(date=seq(as.Date('2010-01-01'), as.Date('2010-12-31'), by='1 day')) , merchnum]
daily_data <- card_data_new %>% group_by(merchnum,date) %>% summarize(n = n(),
                                                                     no.of.days = length(unique(date)),
                                                                     avg.amt = mean(amount),
                                                                     total.amount=sum(amount)
) %>% arrange(date)
new_tb <- all_date %>% left_join(daily_data,by=c("date","merchnum"))
new_tb$n <- ifelse(is.na(new_tb$n),0,new_tb$n)
new_tb$avg.amt <- ifelse(is.na(new_tb$avg.amt),0,new_tb$avg.amt)
new_tb$total.amount <- ifelse(is.na(new_tb$total.amount),0,new_tb$total.amount)
new_tb$no.of.days <- ifelse(is.na(new_tb$no.of.days),0,new_tb$no.of.days)


### 7 days
new_tb_7 <- new_tb %>%
  group_by(merchnum) %>%
  mutate(no_tx_7days_merch = abs(roll_sum(n, 8, align = "right", fill = 0) ),
         avg_amt_7days_merch = abs(roll_sum(avg.amt, 8, align = "right", fill = 0) ),
         tot_amt_7days_merch = abs(roll_sum(total.amount, 8, align = "right", fill = 0) ),
         tot_days_7days_merch = abs(roll_sum(no.of.days, 8, align = "right", fill = 0))) %>% 
  select(date, merchnum,no_tx_7days_merch,avg_amt_7days_merch,tot_amt_7days_merch,tot_days_7days_merch)

new_tb_7 <- new_tb_7 %>% inner_join(new_tb,by=c("merchnum","date"))
new_tb_7$no_tx_7days_merch <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$no_tx_7days_merch-new_tb_7$n,new_tb_7$no_tx_7days_merch)
new_tb_7$tot_amt_7days_merch <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$tot_amt_7days_merch-new_tb_7$total.amount,new_tb_7$tot_amt_7days_merch)
new_tb_7$avg_amt_7days_merch <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$tot_amt_7days_merch/new_tb_7$no_tx_7days_merch,new_tb_7$avg_amt_7days_merch)
new_tb_7$tot_days_7days_merch <- ifelse(new_tb_7$date>as.Date('2010-01-07'),new_tb_7$tot_days_7days_merch-new_tb_7$no.of.days,new_tb_7$tot_days_7days_merch)
new_tb_7 <- new_tb_7[,-c(7:10)]

final_df <- final_df %>% inner_join(new_tb_7,by=c("merchnum","date"))

final_df.0 <- final_df[final_df$no_tx_7days_merch == 0,]
final_df.1 <- final_df[!final_df$no_tx_7days_merch == 0,]

res <- final_df.1 %>% 
  group_by(merchnum) %>% 
  arrange(date) %>% filter(date>as.Date('2010-01-07')) %>%
  mutate(uzip_7_merch=rollapply(merch.zip, width=no_tx_7days_merch, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'),
         umerch_7_merch=rollapply(merchnum, width=no_tx_7days_merch, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'))
res1 <- final_df.1 %>% 
  group_by(merchnum, date) %>% 
  arrange(date) %>% filter(date<=as.Date('2010-01-07'))
final_df.0$uzip_7_merch <- 0
final_df.0$umerch_7_merch <- 0
str(final_df.0)
# str(res)
final_df <- rbind(as.data.frame(res),final_df.0)
final_df <- final_df %>% 
  arrange(record.number)

### 3 days
new_tb_3 <- new_tb %>%
  group_by(merchnum) %>%
  mutate(no_tx_3days_merch = abs(roll_sum(n, 4, align = "right", fill = 0) ),
         avg_amt_3days_merch = abs(roll_sum(avg.amt, 4, align = "right", fill = 0) ),
         tot_amt_3days_merch = abs(roll_sum(total.amount, 4, align = "right", fill = 0) ),
         tot_days_3days_merch = abs(roll_sum(no.of.days, 4, align = "right", fill = 0))) %>% 
  select(date, merchnum,no_tx_3days_merch,avg_amt_3days_merch,tot_amt_3days_merch,tot_days_3days_merch)

new_tb_3 <- new_tb_3 %>% inner_join(new_tb,by=c("merchnum","date"))
new_tb_3$no_tx_3days_merch <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$no_tx_3days_merch-new_tb_3$n,new_tb_3$no_tx_3days_merch)
new_tb_3$tot_amt_3days_merch <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$tot_amt_3days_merch-new_tb_3$total.amount,new_tb_3$tot_amt_3days_merch)
new_tb_3$avg_amt_3days_merch <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$tot_amt_3days_merch/new_tb_3$no_tx_3days_merch,new_tb_3$avg_amt_3days_merch)
new_tb_3$tot_days_3days_merch <- ifelse(new_tb_3$date>as.Date('2010-01-04'),new_tb_3$tot_days_3days_merch-new_tb_3$no.of.days,new_tb_3$tot_days_3days_merch)
new_tb_3 <- new_tb_3[,-c(7:10)]


final_df <- final_df %>% inner_join(new_tb_3,by=c("merchnum","date"))

final_df.0 <- final_df[final_df$no_tx_3days_merch == 0,]
final_df.1 <- final_df[!final_df$no_tx_3days_merch == 0,]


res <- final_df.1 %>% 
  group_by(merchnum) %>% 
  arrange(date) %>% filter(date>as.Date('2010-01-04')) %>%
  mutate(uzip_3_merch=rollapply(merch.zip, width=no_tx_3days_merch, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'),
         umerch_3_merch=rollapply(merchnum, width=no_tx_3days_merch, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'))
res1 <- final_df.1 %>% 
  group_by(merchnum, date) %>% 
  arrange(date) %>% filter(date<=as.Date('2010-01-04'))
res1$uzip_3_merch <- 0
res1$umerch_3_merch <- 0

final_df.0$uzip_3_merch <- 0
final_df.0$umerch_3_merch <- 0
str(final_df.0)
# str(res)
final_df <- rbind(as.data.frame(res1),as.data.frame(res),final_df.0)
final_df <- final_df %>% 
  arrange(record.number)


### 1 day
new_tb_1 <- new_tb %>%
  group_by(merchnum) %>%
  mutate(no_tx_1days_merch = abs(roll_sum(n, 2, align = "right", fill = 0) ),
         avg_amt_1days_merch = abs(roll_sum(avg.amt, 2, align = "right", fill = 0) ),
         tot_amt_1days_merch = abs(roll_sum(total.amount, 2, align = "right", fill = 0) ),
         tot_days_1days_merch = abs(roll_sum(no.of.days, 2, align = "right", fill = 0))) %>% 
  select(date, merchnum,no_tx_1days_merch,avg_amt_1days_merch,tot_amt_1days_merch,tot_days_1days_merch)
new_tb_1 <- new_tb_1 %>% inner_join(new_tb,by=c("merchnum","date"))
new_tb_1$no_tx_1days_merch <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$no_tx_1days_merch-new_tb_1$n,new_tb_1$no_tx_1days_merch)
new_tb_1$tot_amt_1days_merch <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$tot_amt_1days_merch-new_tb_1$total.amount,new_tb_1$tot_amt_1days_merch)
new_tb_1$avg_amt_1days_merch <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$tot_amt_1days_merch/new_tb_1$no_tx_1days_merch,new_tb_1$avg_amt_1days_merch)
new_tb_1$tot_days_1days_merch <- ifelse(new_tb_1$date>as.Date('2010-01-02'),new_tb_1$tot_days_1days_merch-new_tb_1$no.of.days,new_tb_1$tot_days_1days_merch)
new_tb_1 <- new_tb_1[,-c(7:10)]


final_df <- final_df %>% inner_join(new_tb_1,by=c("merchnum","date"))

final_df.0 <- final_df[final_df$no_tx_1days_merch == 0,]
final_df.1 <- final_df[!final_df$no_tx_1days_merch == 0,]
res <- final_df.1 %>% 
  group_by(merchnum) %>% 
  arrange(date) %>% filter(date>as.Date('2010-01-02')) %>%
  mutate(uzip_1_merch=rollapply(merch.zip, width=no_tx_1days_merch, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'),
         umerch_1_merch=rollapply(merchnum, width=no_tx_1days_merch, FUN=function(x) length(unique(x[!is.na(x)])), partial=TRUE, align='right'))
res1 <- final_df.1 %>% 
  group_by(merchnum, date) %>% 
  arrange(date) %>% filter(date<=as.Date('2010-01-02'))
res1$uzip_1_merch <- 0
res1$umerch_1_merch <- 0

final_df.0$uzip_1_merch <- 0
final_df.0$umerch_1_merch <- 0
str(final_df.0)
# str(res)
final_df <- rbind(as.data.frame(res1),as.data.frame(res),final_df.0)
final_df <- final_df %>% 
  arrange(record.number)

write.csv(final_df,"final_df_new_2.csv")

