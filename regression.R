library("haven")    
library("AER")   
library("fastDummies")
df <- read.csv("table1.csv")
head(df)

df$dummy8090 <- ifelse(df$Year < 1990, 1, 0)
df$dummy9000 <- ifelse(df$Year >= 1990 & df$Year < 2000, 1, 0)
df$dummy0010 <- ifelse(df$Year >= 2000 & df$Year < 2010, 1, 0)
df$dummy1018 <- ifelse(df$Year >= 2010 , 1, 0)

df <- dummy_cols(df, select_columns = "State")

fm0 <- lm(unemployment_rate ~ Effective.Minimum.Wage.2020.Dollars + dummy8090 + dummy9000+dummy0010+dummy1018 , data = df)
summary(fm0)
coeftest(fm0, df = Inf, vcov = vcovHC(fm0, type = "HC1"))