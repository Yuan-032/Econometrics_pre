library("haven")    
library("AER")   
library("fastDummies")
library("plm")
df <- read.csv("table1.csv")
head(df)

#generate dummies
df$dummy8090 <- ifelse(df$Year < 1990, 1, 0)
df$dummy9000 <- ifelse(df$Year >= 1990 & df$Year < 2000, 1, 0)
df$dummy0010 <- ifelse(df$Year >= 2000 & df$Year < 2010, 1, 0)
df$dummy1018 <- ifelse(df$Year >= 2010 , 1, 0)

df <- dummy_cols(df, select_columns = "State")

# try to involve state dummies 
cols <- 9:59
nval <- 0:50
df$stateid <- t(sweep(df[, cols], 2, nval, "*"))[t(df[, cols]) != 0]
df$stateid


#model1:base model
fm0 <- lm(unemployment_rate ~ Effective.Minimum.Wage.2020.Dollars , data = df)
summary(fm0)
coeftest(fm0, df = Inf, vcov = vcovHC(fm0, type = "HC1"))

#model2:base model+year fixed effect + year*effective + state fixed effect + state*effective
fm1 <- lm(unemployment_rate ~ Effective.Minimum.Wage.2020.Dollars+stateid + dummy8090 + dummy9000 + dummy0010 +Effective.Minimum.Wage.2020.Dollars*dummy8090+Effective.Minimum.Wage.2020.Dollars*dummy9000+Effective.Minimum.Wage.2020.Dollars*dummy0010 +Effective.Minimum.Wage.2020.Dollars*stateid, data = df)
summary(fm1)
coeftest(fm1, df = Inf, vcov = vcovHC(fm1, type = "HC1"))

#model3: base model+year fixed effect(treat individual year instead of grouping the year into three) + state fixed effect
fm2 <- plm(unemployment_rate ~ Effective.Minimum.Wage.2020.Dollars, 
    data = df,
    index = c("State", "Year"), 
    model = "within")
summary(fm2)
coeftest(fm2, df = Inf, vcov = vcovHC(fm2, type = "HC1"))




