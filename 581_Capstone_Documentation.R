# libraries

library(tidyverse)
library(lubridate)
library(SentimentAnalysis)
library(SnowballC)
library(tm)
library(quantmod)

# datasets

Tweet <- read.csv("C:/Users/Brian/Desktop/CSU/0.MIS_581/Datasets/Tweets/Tweet.csv")
raw_partner_headlines <- read.csv("C:/Users/Brian/Desktop/CSU/0.MIS_581/Datasets/Headlines/raw_partner_headlines.csv")
getSymbols("AAPL", src = "yahoo")
getSymbols("GOOG", src = "yahoo")
getSymbols("AMZN", src = "yahoo")
getSymbols("MSFT", src = "yahoo")
getSymbols("^IXIC", src = "yahoo")

# visualizing the stock data with quantmod package

chartSeries(AAPL)
chartSeries(GOOG)
chartSeries(AMZN)
chartSeries(MSFT)

# filtering stock data sets

# quantmod stock cleaner function

stock_cleaner_function <- function(quantmod_set){
  
  df_set <- as.data.frame(quantmod_set)
  
  NAmatrix <- matrix(data = NA, nrow = nrow(df_set), ncol = 7)
  
  NAmatrix[1:nrow(NAmatrix),1] <- row.names(df_set)
  
  NAmatrix[1:nrow(NAmatrix),2] <- quantmod_set[,1]
  
  NAmatrix[1:nrow(NAmatrix),3] <- quantmod_set[,2]
  
  NAmatrix[1:nrow(NAmatrix),4] <- quantmod_set[,3]
  
  NAmatrix[1:nrow(NAmatrix),5] <- quantmod_set[,4]
  
  NAmatrix[1:nrow(NAmatrix),6] <- quantmod_set[,5]
  
  NAmatrix[1:nrow(NAmatrix),7] <- quantmod_set[,6]
  
  inc_tibble <- as_tibble(NAmatrix) %>%
    
    rename("Date.Full" = "V1",
           "Open" = "V2",
           "High" = "V3",
           "Low" = "V4",
           "Close" = "V5",
           "Volume" = "V6",
           "Adjusted" = "V7")
  
  inc_tibble$Date.Full <- as_date(inc_tibble$Date.Full)
  
  inc_tibble$Open <- as.numeric(inc_tibble$Open)
  
  inc_tibble$High <- as.numeric(inc_tibble$High)
  
  inc_tibble$Low <- as.numeric(inc_tibble$Low)
  
  inc_tibble$Close <- as.numeric(inc_tibble$Close)
  
  inc_tibble$Volume <- as.integer(inc_tibble$Volume)
  
  inc_tibble$Adjusted <- as.numeric(inc_tibble$Adjusted)
  
  inc_tibble <- inc_tibble %>%
    
    mutate(Year = year(inc_tibble$Date.Full),
           Month = month(inc_tibble$Date.Full),
           Day = day(inc_tibble$Date.Full)) %>%
             
    filter(Year >= 2011 & Year <= 2020) %>%
                
    filter(Year %in% c(2011:2019) | Year ==2020 & Month < 6) %>%
    
    select(Date.Full, Open:Adjusted)
    
    
  return(inc_tibble)
  
}

apple_clean <- stock_cleaner_function(AAPL)
amazon_clean <- stock_cleaner_function(AMZN)
google_clean <- stock_cleaner_function(GOOG)
microsoft_clean <- stock_cleaner_function(MSFT)
nasdaq_clean <- stock_cleaner_function(IXIC)

remove(AAPL, AMZN, GOOG, MSFT, IXIC)

# export as xls file for abnormal return calculations

# percent change

a <- a %>%
  mutate(
    pct_change = (Adjusted/lag(Adjusted) - 1) * 100)

# add close and percent change for nasdaq to each data set

# add mean-adjusted returns, market-adjusted returns, and risk-adjusted returns Models

# need to create estimation period numbers for each stock

abnormal_returns_function <- function(clean_quantmod_set, clean_quantmod_nasdaq_set){
  
  a <- clean_quantmod_set
  b <- clean_quantmod_nasdaq_set
  
  a <- a %>%
    mutate(
      NASDAQ.Close = b$Close,
      Stock.Change = round((a$Close/lag(a$Close) - 1) * 100, 4),
      NASDAQ.Change = round((b$Close/lag(b$Close) - 1) * 100, 4)
    )
  
  stock_mean <- mean(as.matrix(a[2:1258,9]))
  nasdaq_mean <- mean(as.matrix(a[2:1258,10]))
  
  stock_sd <- sd(as.matrix(a[2:1258,9]))
  nasdaq_sd <- sd(as.matrix(a[2:1258,10]))
  
  y <- as.matrix(a[2:1258,9])
  x <- as.matrix(a[2:1258,10])
  model <- lm(y~x)
  coef <- coef(model)
  intercept <- coef[1]
  slope <- coef[2]
  
  observation_set <- a[1259:nrow(a),1:ncol(a)] %>%
    mutate(
      MAR = round(Stock.Change - stock_mean, 4),
      MKAR = round(Stock.Change - nasdaq_mean, 4),
      RAR = round((Stock.Change - (intercept + (slope*NASDAQ.Change))), 4)
    ) %>%
    select(
      Date.Full, Close, NASDAQ.Close, Stock.Change, NASDAQ.Change, MAR:RAR
    )
  
  return(observation_set)
    
}

# observation periods of the stocks and their abnormal returns estimations

amazon_ar <- abnormal_returns_function(amazon_clean, nasdaq_clean)

apple_ar <- abnormal_returns_function(apple_clean, nasdaq_clean)

google_ar <- abnormal_returns_function(google_clean, nasdaq_clean)

microsoft_ar <- abnormal_returns_function(microsoft_clean, nasdaq_clean)

remove(amazon_clean, apple_clean, google_clean, microsoft_clean, nasdaq_clean)

# need to set an acceptable range for abnormal returns worthy of observation by specific days:

#1. the stock against itself 

#2. the stock against the market

# if the stock falls above Q3 or below Q1 in any metric

# quantile-abnormalities function

quantile_abnormalities_function <- function(ar_set){
  
  a <- ar_set
  
  empty_matrix <- matrix(data = NA, nrow = 4, ncol = 2)
  
  colnames(empty_matrix) <- c("Q1", "Q3")
  rownames(empty_matrix) <- c("Stock.Change", "MAR", "MKAR", "RAR")
  
  sc <- quantile(a$Stock.Change)
  mar <- quantile(a$MAR)
  mkar <- quantile(a$MKAR)
  rar <- quantile(a$RAR)
  
  empty_matrix[1,1:2] <- c(sc[2],sc[4])
  empty_matrix[2,1:2] <- c(mar[2],mar[4])
  empty_matrix[3,1:2] <- c(mkar[2],mkar[4])
  empty_matrix[4,1:2] <- c(rar[2],rar[4])
  
  a <- a %>%
    filter(
      
      Stock.Change < empty_matrix[1,1] | Stock.Change > empty_matrix[1,2],
      
      MAR < empty_matrix[2,1] | MAR > empty_matrix[2,2],
      
      MKAR < empty_matrix[3,1] | MKAR > empty_matrix[3,2],
      
      RAR < empty_matrix[4,1] | RAR > empty_matrix[4,2],
      
    ) %>%
    
    select(Date.Full:Close, Stock.Change:RAR)
  
  return(a)
  
}

amazon_abnormalities <- quantile_abnormalities_function(amazon_ar)

apple_abnormalities <- quantile_abnormalities_function(apple_ar)

google_abnormalities <- quantile_abnormalities_function(google_ar)

microsoft_abnormalities <- quantile_abnormalities_function(microsoft_ar)

remove(amazon_ar, apple_ar, google_ar, microsoft_ar)

# further reduction of abnormalities needed

# greater than 1% or 2% daily change between stock and entire market?

# mutate to create difference and then filter/select

#mhm <- amazon_abnormalities %>%
  mutate(
    
    Difference = (Stock.Change - NASDAQ.Change)
    
  ) %>%
  filter(
    
    Difference > 1
    
  ) %>%
  select(
    
    Year:RAR
    
  )

# need to look through tweet and headlines data sets in search of full company names and stock ticker

# headlines

amazon_headlines <- raw_partner_headlines %>%
  filter(
    
    str_detect(headline, "Amazon") | str_detect(headline, "AMZN")
    
  ) %>%
  mutate(
    
    Date = ymd_hms(date),
    Date = ymd(Date)
    
  ) %>%
  select(
    
    X, Date, headline, url, publisher, stock
    
  )

apple_headlines <- raw_partner_headlines %>%
  filter(
    
    str_detect(headline, "Apple") | str_detect(headline, "AAPL")
    
  ) %>%
  mutate(
    
    Date = ymd_hms(date),
    Date = ymd(Date)
    
  ) %>%
  select(
    
    X, Date, headline, url, publisher, stock
    
  )

google_headlines <- raw_partner_headlines %>%
  filter(
    
    str_detect(headline, "Google") | str_detect(headline, "GOOG")
    
  ) %>%
  mutate(
    
    Date = ymd_hms(date),
    Date = ymd(Date)
    
  ) %>%
  select(
    
    X, Date, headline, url, publisher, stock
    
  )

microsoft_headlines <- raw_partner_headlines %>%
  filter(
    
    str_detect(headline, "Microsoft") | str_detect(headline, "MSFT")
    
  ) %>%
  mutate(
    
    Date = ymd_hms(date),
    Date = ymd(Date)
    
  ) %>%
  select(
    
    X, Date, headline, url, publisher, stock
    
  )

# tweets

amazon_tweets <- Tweet %>%
  filter(
    
    str_detect(body, "Amazon") | str_detect(body, "AMZN") | str_detect(body, "amazon") | str_detect(body, "amzn")
    
  ) %>%
  mutate(
    full = as_datetime(post_date),
    breakdown = ymd_hms(full),
    Date = as_date(breakdown)
  ) %>%
  select(
    tweet_id:writer, Date, body:like_num
  )

apple_tweets <- Tweet %>%
  filter(
    
    str_detect(body, "Apple") | str_detect(body, "AAPL") | str_detect(body, "apple") | str_detect(body, "aapl")
    
  )%>%
  mutate(
    full = as_datetime(post_date),
    breakdown = ymd_hms(full),
    Date = as_date(breakdown)
  ) %>%
  select(
    tweet_id:writer, Date, body:like_num
  )

google_tweets <- Tweet %>%
  filter(
    
    str_detect(body, "Google") | str_detect(body, "GOOG") | str_detect(body, "google") | str_detect(body, "goog")
    
  )%>%
  mutate(
    full = as_datetime(post_date),
    breakdown = ymd_hms(full),
    Date = as_date(breakdown)
  ) %>%
  select(
    tweet_id:writer, Date, body:like_num
  )

microsoft_tweets <- Tweet %>%
  filter(
    
    str_detect(body, "Microsoft") | str_detect(body, "MSFT") | str_detect(body, "microsoft") | str_detect(body, "msft")
    
  )%>%
  mutate(
      full = as_datetime(post_date),
      breakdown = ymd_hms(full),
      Date = as_date(breakdown)
  ) %>%
  select(
    tweet_id:writer, Date, body:like_num
  )

# adjust dates on tweets data sets

# create function that:

#1. sorts through headlines/tweets datasets based upon dates in abnormal returns
#2. quantifies sentiments in each row, averages them by day
#3. 

sentiments_aggregator <- function(ar_stock,company_headlines, company_tweets){
  
  a <- ar_stock
  b <- company_headlines
  c <- company_tweets
  
  dates <- a$Date.Full
  
  b <- b %>%
    filter(
      
      b$Date %in% dates
      
    ) %>%
    select(
      
      Date:stock
      
    ) %>%
    arrange(
      Date
    )
  
  bb <- analyzeSentiment(b$headline) 
  
  b <- b %>%
    mutate(
      
      Sent.Score = bb$SentimentQDAP
      
    ) %>%
    group_by(Date) %>%
    summarise(Sent.Avg = mean(Sent.Score, na.rm = TRUE))
  
  b_plus <- b %>%
    filter(Sent.Avg >= 0)
  b_minus <- b %>%
    filter(Sent.Avg < 0)
  
  # can do 25,000 rows at a time for sentiment analysis
  
  c <- c %>%
    filter(
      
      c$Date %in% dates
     
    ) %>%
    select(
      
      Date:body
      
    ) %>%
    arrange(
      Date
    ) 
  
  cc <- analyzeSentiment(c$body) 
  
  c <- c %>%
    mutate(
      
      Sent.Score = cc$SentimentQDAP
      
    ) %>%
    group_by(Date) %>%
    summarise(Sent.Avg = mean(Sent.Score, na.rm = TRUE))
  
  c_plus <- c %>%
    filter(Sent.Avg >= 0)
  c_minus <- c %>%
    filter(Sent.Avg < 0)
  
  
  
  return()
}

# replicating sentimentanalysis within function ideas

for(i in apple_tweets){
  
  yup <- (nrow(apple_tweets)/25000)
  a <- 100
  
  while(yup > 1){
    print(mhm)
    mhm <- apple_tweets[1:25000,3:4]
  }
  
}

# yup ~ 60

#a1 <- analyzeSentiment(mhm[1:25000,4]) %>%
  select(SentimentQDAP)
cbind(mhm,a1)

mhm

# unique identifier by date

c <- c %>%
  group_by() %>%
  mutate(flag = match(Date, unique(Date)))

# repeating code

# make a separate matrix to contain column of sentiment scores

s_matrix <- matrix(data = NA, nrow = 221777, ncol = 1)

# repeated actions

dates <- c(amazon_abnormalities$Date.Full)

a <- amazon_tweets %>%
  filter(
    Date %in% dates
  ) %>%
  select(Date, body)

#aa <- analyzeSentiment(a[1:20000, 2]) %>%
  select(SentimentQDAP)

s_matrix <- as.data.frame(s_matrix)

s_matrix[1:20000,1] <- aa

# write_csv(s_matrix, "filename")

# combine scores with company days that match with abnormal dates
# group by day, summarize sentiment score
# pearson correlation coefficient

# a %>%
  mutate(
    Sent.Score = s_matrix[1:nrow(s_matrix),1]
  ) %>%
  group_by(Date) %>%
  summarise(Sent.Score = mean(Sent.Score))

# microsoft tweets stop at 12/31/2019
# checking others
# all tweets end on 12/31/2019; need to remove 2020 abnormal returns

amazon_abnormalities <- amazon_abnormalities[1:329,1:7]
apple_abnormalities <- apple_abnormalities[1:345,1:7]
google_abnormalities <- google_abnormalities[1:314,1:7]
microsoft_abnormalities <- microsoft_abnormalities[1:342,1:7]


# either google or apple has wrong number of sentiment scores, need to re-do

# latest section of Apple re-do, do the next sequence by 15,000

#aa <- analyzeSentiment(a[255001:2700000, 2]) %>%
  select(SentimentQDAP)
s_matrix[255001:270000,1] <- aa

# have aggregated sentiments by date
# need to correlate between closing prices and sentiments
# should i separate by positive/negative abnormal returns with a dummy variable or just compare them as-is?
# dummy variables are needed to comprehensively observe all measures of abnormal return
# removal of tweet information

remove(a, amazon_tweets, apple_tweets, google_tweets, microsoft_tweets, raw_partner_headlines, s_matrix, Tweet, i, yup)

# need to aggregate sentiments of headlines

#a <- analyzeSentiment(amazon_h_scores$headline) %>%
  select(SentimentQDAP)

amazon_h_scores <- amazon_h_scores %>%
  select(Date, SentimentQDAP) %>%
  group_by(Date) %>%
  summarise(Sent.Score = mean(SentimentQDAP))

# create dummy variables for abnormal returns (+1 | -1)

# correlation between headlines and tweets separately or together?

dummy_function <- function(abnormal_returns){
  
  a <- abnormal_returns %>%
    group_by_all() %>%
    summarise(
      binary = if(Stock.Change >= 0){
        binary = 1
      } else {
        binary = -1
      }
    )
  
  return(a)
}

# transform abnormal returns datasets

amazon_abnormalities <- dummy_function(amazon_abnormalities)
apple_abnormalities <- dummy_function(apple_abnormalities)
google_abnormalities <- dummy_function(google_abnormalities)
microsoft_abnormalities <- dummy_function(microsoft_abnormalities)

# correlation between tweets and headlines separately
# abnormal returns for headlines will be reduced from overall abnormal returns since headlines didn't account for every day of abnormal returns and cannot be correlated by Pearson's Test

# tweet correlations

amazon_tweets_correlation <- cor(amazon_aggregated_sentiments$Sent.Score, amazon_abnormalities$binary)

apple_tweets_correlation <- cor(apple_aggregated_sentiments$Sent.Score, apple_abnormalities$binary)

google_tweets_correlation <- cor(google_aggregated_sentiments$Sent.Scores, google_abnormalities$binary)

microsoft_tweets_correlation <- cor(microsoft_aggregated_sentiments$Sent.Score, microsoft_abnormalities$binary)

# headlines correlations

# Amazon

dates <- amazon_abnormalities$Date.Full

AMZN_headlines_scores <- amazon_headlines %>%
  filter(Date %in% dates) %>%
  select(Date, headline)

a <- analyzeSentiment(AMZN_headlines_scores$headline) %>%
  select(SentimentQDAP)

AMZN_headlines_scores <- AMZN_headlines_scores %>%
  mutate(
    Sent.Score = a$SentimentQDAP
  ) %>%
  select(
    Date, Sent.Score
  ) %>%
  group_by(Date) %>%
  summarise(mean(Sent.Score))

headline_dates <- c(AMZN_headlines_scores$Date)

amzn_abnormal_headlines <- amazon_abnormalities %>%
  filter(Date.Full %in% headline_dates)

amazon_headlines_correlation <- cor(AMZN_headlines_scores$`mean(Sent.Score)`, amzn_abnormal_headlines$binary)

# apple

dates <- apple_abnormalities$Date.Full

AAPL_headlines_scores <- apple_headlines %>%
  filter(Date %in% dates) %>%
  select(Date, headline)

a <- analyzeSentiment(AAPL_headlines_scores$headline) %>%
  select(SentimentQDAP)

AAPL_headlines_scores <- AAPL_headlines_scores %>%
  mutate(
    Sent.Score = a$SentimentQDAP
  ) %>%
  select(
    Date, Sent.Score
  ) %>%
  group_by(Date) %>%
  summarise(mean(Sent.Score))

headline_dates <- c(AAPL_headlines_scores$Date)

aapl_abnormal_headlines <- apple_abnormalities %>%
  filter(Date.Full %in% headline_dates)

apple_headlines_correlation <- cor(AAPL_headlines_scores$`mean(Sent.Score)`, aapl_abnormal_headlines$binary)

# google

dates <- google_abnormalities$Date.Full

GOOG_headlines_scores <- google_headlines %>%
  filter(Date %in% dates) %>%
  select(Date, headline)

a <- analyzeSentiment(GOOG_headlines_scores$headline) %>%
  select(SentimentQDAP)

GOOG_headlines_scores <- GOOG_headlines_scores %>%
  mutate(
    Sent.Score = a$SentimentQDAP
  ) %>%
  select(
    Date, Sent.Score
  ) %>%
  group_by(Date) %>%
  summarise(mean(Sent.Score))

headline_dates <- c(GOOG_headlines_scores$Date)

goog_abnormal_headlines <- google_abnormalities %>%
  filter(Date.Full %in% headline_dates)

google_headlines_correlation <- cor(GOOG_headlines_scores$`mean(Sent.Score)`, goog_abnormal_headlines$binary)

# microsoft

dates <- microsoft_abnormalities$Date.Full

MSFT_headlines_scores <- microsoft_headlines %>%
  filter(Date %in% dates) %>%
  select(Date, headline)

a <- analyzeSentiment(MSFT_headlines_scores$headline) %>%
  select(SentimentQDAP)

MSFT_headlines_scores <- MSFT_headlines_scores %>%
  mutate(
    Sent.Score = a$SentimentQDAP
  ) %>%
  select(
    Date, Sent.Score
  ) %>%
  group_by(Date) %>%
  summarise(mean(Sent.Score))

headline_dates <- c(MSFT_headlines_scores$Date)

msft_abnormal_headlines <- microsoft_abnormalities %>%
  filter(Date.Full %in% headline_dates)

microsoft_headlines_correlation <- cor(MSFT_headlines_scores$`mean(Sent.Score)`, msft_abnormal_headlines$binary)

# clean-up

remove(a, dates)