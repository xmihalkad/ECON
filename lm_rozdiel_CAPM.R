library(dplyr)
library(tidyverse)
library(readxl)
library(timeSeries)
library(TTR) # pocita vynosy
library(scales) # kvoli grafom
library(ggplot2)
library(PerformanceAnalytics)



df <- read_excel("C:/Users/Užívatel/Desktop/Ekonometria/DU1/DATA_UKOL_1_premenovane.xls")

#typeof(df)
# pocet premennych k
k <- length(df)
# pocet pozorovani n (mesacne data)
n <- nrow(df)

# pomocna tabulka na vytiahnutie mien stlpcov
d <- df[ 10:n,]

# data
df <- df[ 11:n,]

## vektor datumov po mesiacoch  v obdobi 2015-2019
dates <- seq(as.Date("2015/1/1"), by = "month", length.out = 60)

# pomenovanie stlpcov v datach
colnames(df) <- as.character(d[1,])

# prvy stlpec ako datumy
df[,1] <- dates

# zmena zvysnych stlpcov na numeric
df[, c(2:k)] <- sapply(df[, c(2:k)], as.numeric)

# transform
# odobranie skrytych riadkov
df <- df[,-c(3,4)]

# typ
#str(df)

# premerna bezrizikova urokova miera
#mean(df$r_f)
# std. odchylka bezrizikova urokova miera
#sd(df$r_f)

# momentum pocita rozdiel medzi danymi mesiacmi
#momentum(df$NASDAQCOM)
#momentum(df$Apple)
# da sa to aj vykreslit
#plot(dates, momentum(df$NASDAQCOM), type = "l")

# funkcia ROC pocita log vynosnost v percentach
# vynosy
returns_df <- tibble(NASDAQCOM = ROC(df$NASDAQCOM),
                     Apple = ROC(df$Apple),
                     AMD = ROC(df$AMD),
                     Amazon = ROC(df$Amazon),
                     Boeing = ROC(df$Boeing),
                     Disney = ROC(df$Disney),
                     eBay = ROC(df$eBay),
                     Ford = ROC(df$Ford),
                     Facebook = ROC(df$Facebook),
                     Google = ROC(df$Google),
                     Intel = ROC(df$Intel),
                     JNJ = ROC(df$JNJ),
                     KO = ROC(df$KO),
                     Microsoft = ROC(df$Microsoft),
                     Netflix = ROC(df$Netflix),
                     Nokia = ROC(df$Nokia),
                     NVIDIA = ROC(df$NVIDIA),
                     Starbucks = ROC(df$Starbucks),
                     Sony = ROC(df$Sony),
                     Tesla = ROC(df$Tesla),
                     Vodafone = ROC(df$Vodafone),
                     r_f = df$r_f
)


# vymazanie prveho riadku s NA hodnotam
returns_df <- na.omit(returns_df)


# datumy ako nazvy riadkov (mozno pre dalsie pouzitie)
#rownames(returns_df) <- dates[2:60]
Date <- dates[2:60]
#akcie <- colnames(returns_df)


# Potrebujeme vytvorit regresne modely pre vsetky nase akcie (portfoliia) - je ich 21
##########################################################################################
NASDAQCOM.fit <- lm(NASDAQCOM ~ r_f + risk.premium, data = returns_df)
Apple.fit <- lm(Apple ~ r_f + risk.premium, data = returns_df)
AMD.fit <- lm(AMD ~ r_f + risk.premium, data = returns_df)
Amazon.fit <- lm(Amazon ~ r_f + risk.premium, data = returns_df)
Boeing.fit <- lm(Boeing ~ r_f + risk.premium, data = returns_df)
Disney.fit <- lm(Disney ~ r_f + risk.premium, data = returns_df)
eBay.fit <- lm(eBay ~ r_f + risk.premium, data = returns_df)
Ford.fit <- lm(Ford ~ r_f + risk.premium, data = returns_df)
Facebook.fit <- lm(Facebook ~ r_f + risk.premium, data = returns_df)
Google.fit <- lm(Google ~ r_f + risk.premium, data = returns_df)
Intel.fit <- lm(Intel ~ r_f + risk.premium, data = returns_df)
JNJ.fit <- lm(JNJ ~ r_f + risk.premium, data = returns_df)
KO.fit <- lm(KO ~ r_f + risk.premium, data = returns_df)
Microsoft.fit <- lm(Microsoft ~ r_f + risk.premium, data = returns_df)
Netflix.fit <- lm(Netflix ~ r_f + risk.premium, data = returns_df)
Nokia.fit <- lm(Nokia ~ r_f + risk.premium, data = returns_df)
NVIDIA.fit <- lm(NVIDIA ~ r_f + risk.premium, data = returns_df)
Starbucks.fit <- lm(Starbucks ~ r_f + risk.premium, data = returns_df)
Sony.fit <- lm(Sony ~ r_f + risk.premium, data = returns_df)
Tesla.fit <- lm(Tesla ~ r_f + risk.premium, data = returns_df)
Vodafone.fit <- lm(Vodafone ~ r_f + risk.premium, data = returns_df)
##########################################################################################
# tabulka rozdielov j-teho portfolia a bezrizikovej urokovej miery
##########################################################################################
rozdiely <- tibble(
    Apple = returns_df$Apple - returns_df$r_f,
    AMD = returns_df$AMD - returns_df$r_f,
    Amazon = returns_df$Amazon - returns_df$r_f,
    Boeing = returns_df$Boeing - returns_df$r_f,
    Disney = returns_df$Disney - returns_df$r_f,
    eBay = returns_df$eBay - returns_df$r_f,
    Ford = returns_df$Ford - returns_df$r_f,
    Facebook = returns_df$Facebook - returns_df$r_f,
    Google = returns_df$Google - returns_df$r_f,
    Intel = returns_df$Intel - returns_df$r_f,
    JNJ = returns_df$JNJ - returns_df$r_f,
    KO = returns_df$KO - returns_df$r_f,
    Microsoft = returns_df$Microsoft - returns_df$r_f,
    Netflix = returns_df$Netflix - returns_df$r_f,
    Nokia = returns_df$Nokia - returns_df$r_f,
    NVIDIA = returns_df$NVIDIA - returns_df$r_f,
    Starbucks = returns_df$Starbucks - returns_df$r_f,
    Sony = returns_df$Sony - returns_df$r_f,
    Tesla = returns_df$Tesla - returns_df$r_f,
    Vodafone = returns_df$Vodafone - returns_df$r_f,
    risk.premium = returns_df$r_m - returns_df$r_f)
##########################################################################################

#Potrebujeme vytvorit regresne modely (rozdielov) pre vsetky nase akcie (portfoliia)
##########################################################################################
Apple.fit.rozdiely <- lm(Apple ~  risk.premium, data = rozdiely)
AMD.fit.rozdiely <- lm(AMD ~ + risk.premium, data = rozdiely)
Amazon.fit.rozdiely <- lm(Amazon ~  risk.premium, data = rozdiely)
Boeing.fit.rozdiely <- lm(Boeing ~  risk.premium, data = rozdiely)
Disney.fit.rozdiely <- lm(Disney ~  risk.premium, data = rozdiely)
eBay.fit.rozdiely <- lm(eBay ~  risk.premium, data = rozdiely)
Ford.fit.rozdiely <- lm(Ford ~  risk.premium, data = rozdiely)
Facebook.fit.rozdiely <- lm(Facebook ~ risk.premium, data = rozdiely)
Google.fit.rozdiely <- lm(Google ~ risk.premium, data = rozdiely)
Intel.fit.rozdiely <- lm(Intel ~ risk.premium, data = rozdiely)
JNJ.fit.rozdiely <- lm(JNJ ~  risk.premium, data = rozdiely)
KO.fit.rozdiely <- lm(KO ~  risk.premium, data = rozdiely)
Microsoft.fit.rozdiely <- lm(Microsoft ~ risk.premium, data = rozdiely)
Netflix.fit.rozdiely <- lm(Netflix ~ risk.premium, data = rozdiely)
Nokia.fit.rozdiely <- lm(Nokia ~  risk.premium, data = rozdiely)
NVIDIA.fit.rozdiely <- lm(NVIDIA ~  risk.premium, data = rozdiely)
Starbucks.fit.rozdiely <- lm(Starbucks ~  risk.premium, data = rozdiely)
Sony.fit.rozdiely <- lm(Sony ~  risk.premium, data = rozdiely)
Tesla.fit.rozdiely <- lm(Tesla ~ risk.premium, data = rozdiely)
Vodafone.fit.rozdiely <- lm(Vodafone ~  + risk.premium, data = rozdiely)
##########################################################################################

# zistime co vsetko sa da z toho vytiahnut
#names(summary(NASDAQCOM.fit.rozdiely))


# Vytvorena tabulka vynosnoti
stock <- c('Apple', 'AMD', 'Amazon', 'Boeing', 'Disney',"eBay","Ford",
           "Facebook","Google","Intel","JNJ","KO","Microsoft","Netflix","Nokia","NVIDIA",
           "Starbucks","Sony","Tesla","Vodafone")


##########################################################################################
expected.return <- c(
                     Apple.fit$coefficients[3],
                     AMD.fit$coefficients[3],
                     Amazon.fit$coefficients[3],
                     Boeing.fit$coefficients[3],
                     Disney.fit$coefficients[3],
                     eBay.fit$coefficients[3],
                     Ford.fit$coefficients[3],
                     Facebook.fit$coefficients[3],
                     Google.fit$coefficients[3],
                     Intel.fit$coefficients[3],
                     JNJ.fit$coefficients[3],
                     KO.fit$coefficients[3],
                     Microsoft.fit$coefficients[3],
                     Netflix.fit$coefficients[3],
                     Nokia.fit$coefficients[3],
                     NVIDIA.fit$coefficients[3],
                     Starbucks.fit$coefficients[3],
                     Sony.fit$coefficients[3],
                     Tesla.fit$coefficients[3],
                     Vodafone.fit$coefficients[3])
##########################################################################################

# tabulka s hodnotami beta
Tabulka_vynosnosti <- data.fraApple.fit.rozdiely me(stock, expected.return)

#How do we interpret the values of the risk premiums?
#Well, the higher the risk premium the more volatile or risky the asset is, and therefore
#investors should be compennsated with a return that justifies the risk of the asset.

# skladanie tabulky z dat
##########################################################################################
Tabulka_vynosnosti$p_val <- c(
                              summary(Apple.fit)$coefficients[1,4],
                              summary(AMD.fit)$coefficients[1,4],
                              summary(Amazon.fit)$coefficients[1,4],
                              summary(Boeing.fit)$coefficients[1,4],
                              summary(Disney.fit)$coefficients[1,4],
                              summary(eBay.fit)$coefficients[1,4],
                              summary(Ford.fit)$coefficients[1,4],
                              summary(Facebook.fit)$coefficients[1,4],
                              summary(Google.fit)$coefficients[1,4],
                              summary(Intel.fit)$coefficients[1,4],
                              summary(JNJ.fit)$coefficients[1,4],
                              summary(KO.fit)$coefficients[1,4],
                              summary(Microsoft.fit)$coefficients[1,4],
                              summary(Netflix.fit)$coefficients[1,4],
                              summary(Nokia.fit)$coefficients[1,4],
                              summary(NVIDIA.fit)$coefficients[1,4],
                              summary(Starbucks.fit)$coefficients[1,4],
                              summary(Sony.fit)$coefficients[1,4],
                              summary(Tesla.fit)$coefficients[1,4],
                              summary(Vodafone.fit)$coefficients[1,4])
##########################################################################################

##########################################################################################
Tabulka_vynosnosti$p_val_alpha_rozdiel <- c(
                                            summary(Apple.fit.rozdiely)$coefficients[1,4],
                                            summary(AMD.fit.rozdiely)$coefficients[1,4],
                                            summary(Amazon.fit.rozdiely)$coefficients[1,4],
                                            summary(Boeing.fit.rozdiely)$coefficients[1,4],
                                            summary(Disney.fit.rozdiely)$coefficients[1,4],
                                            summary(eBay.fit.rozdiely)$coefficients[1,4],
                                            summary(Ford.fit.rozdiely)$coefficients[1,4],
                                            summary(Facebook.fit.rozdiely)$coefficients[1,4],
                                            summary(Google.fit.rozdiely)$coefficients[1,4],
                                            summary(Intel.fit.rozdiely)$coefficients[1,4],
                                            summary(JNJ.fit.rozdiely)$coefficients[1,4],
                                            summary(KO.fit.rozdiely)$coefficients[1,4],
                                            summary(Microsoft.fit.rozdiely)$coefficients[1,4],
                                            summary(Netflix.fit.rozdiely)$coefficients[1,4],
                                            summary(Nokia.fit.rozdiely)$coefficients[1,4],
                                            summary(NVIDIA.fit.rozdiely)$coefficients[1,4],
                                            summary(Starbucks.fit.rozdiely)$coefficients[1,4],
                                            summary(Sony.fit.rozdiely)$coefficients[1,4],
                                            summary(Tesla.fit.rozdiely)$coefficients[1,4],
                                            summary(Vodafone.fit.rozdiely)$coefficients[1,4])
##########################################################################################

##########################################################################################
p_val_alpha_rozdiel <- c(
                         summary(Apple.fit.rozdiely)$coefficients[1,4],
                         summary(AMD.fit.rozdiely)$coefficients[1,4],
                         summary(Amazon.fit.rozdiely)$coefficients[1,4],
                         summary(Boeing.fit.rozdiely)$coefficients[1,4],
                         summary(Disney.fit.rozdiely)$coefficients[1,4],
                         summary(eBay.fit.rozdiely)$coefficients[1,4],
                         summary(Ford.fit.rozdiely)$coefficients[1,4],
                         summary(Facebook.fit.rozdiely)$coefficients[1,4],
                         summary(Google.fit.rozdiely)$coefficients[1,4],
                         summary(Intel.fit.rozdiely)$coefficients[1,4],
                         summary(JNJ.fit.rozdiely)$coefficients[1,4],
                         summary(KO.fit.rozdiely)$coefficients[1,4],
                         summary(Microsoft.fit.rozdiely)$coefficients[1,4],
                         summary(Netflix.fit.rozdiely)$coefficients[1,4],
                         summary(Nokia.fit.rozdiely)$coefficients[1,4],
                         summary(NVIDIA.fit.rozdiely)$coefficients[1,4],
                         summary(Starbucks.fit.rozdiely)$coefficients[1,4],
                         summary(Sony.fit.rozdiely)$coefficients[1,4],
                         summary(Tesla.fit.rozdiely)$coefficients[1,4],
                         summary(Vodafone.fit.rozdiely)$coefficients[1,4])
##########################################################################################

##########################################################################################
Tabulka_vynosnosti$p_val_beta_rozdiel <- c(
                        summary(Apple.fit.rozdiely)$coefficients[2,4],
                        summary(AMD.fit.rozdiely)$coefficients[2,4],
                        summary(Amazon.fit.rozdiely)$coefficients[2,4],
                        summary(Boeing.fit.rozdiely)$coefficients[2,4],
                        summary(Disney.fit.rozdiely)$coefficients[2,4],
                        summary(eBay.fit.rozdiely)$coefficients[2,4],
                        summary(Ford.fit.rozdiely)$coefficients[2,4],
                        summary(Facebook.fit.rozdiely)$coefficients[2,4],
                        summary(Google.fit.rozdiely)$coefficients[2,4],
                        summary(Intel.fit.rozdiely)$coefficients[2,4],
                        summary(JNJ.fit.rozdiely)$coefficients[2,4],
                        summary(KO.fit.rozdiely)$coefficients[2,4],
                        summary(Microsoft.fit.rozdiely)$coefficients[2,4],
                        summary(Netflix.fit.rozdiely)$coefficients[2,4],
                        summary(Nokia.fit.rozdiely)$coefficients[2,4],
                        summary(NVIDIA.fit.rozdiely)$coefficients[2,4],
                        summary(Starbucks.fit.rozdiely)$coefficients[2,4],
                        summary(Sony.fit.rozdiely)$coefficients[2,4],
                        summary(Tesla.fit.rozdiely)$coefficients[2,4],
                        summary(Vodafone.fit.rozdiely)$coefficients[2,4])
##########################################################################################

##########################################################################################
Tabulka_vynosnosti$bety_rozdiel <- c(
                                     Apple.fit.rozdiely$coefficients[2],
                                     AMD.fit.rozdiely$coefficients[2],
                                     Amazon.fit.rozdiely$coefficients[2],
                                     Boeing.fit.rozdiely$coefficients[2],
                                     Disney.fit.rozdiely$coefficients[2],
                                     eBay.fit.rozdiely$coefficients[2],
                                     Ford.fit.rozdiely$coefficients[2],
                                     Facebook.fit.rozdiely$coefficients[2],
                                     Google.fit.rozdiely$coefficients[2],
                                     Intel.fit.rozdiely$coefficients[2],
                                     JNJ.fit.rozdiely$coefficients[2],
                                     KO.fit.rozdiely$coefficients[2],
                                     Microsoft.fit.rozdiely$coefficients[2],
                                     Netflix.fit.rozdiely$coefficients[2],
                                     Nokia.fit.rozdiely$coefficients[2],
                                     NVIDIA.fit.rozdiely$coefficients[2],
                                     Starbucks.fit.rozdiely$coefficients[2],
                                     Sony.fit.rozdiely$coefficients[2],
                                     Tesla.fit.rozdiely$coefficients[2],
                                     Vodafone.fit.rozdiely$coefficients[2])
##########################################################################################

##########################################################################################
bety_rozdiel <- c(
                  Apple.fit.rozdiely$coefficients[2],
                  AMD.fit.rozdiely$coefficients[2],
                  Amazon.fit.rozdiely$coefficients[2],
                  Boeing.fit.rozdiely$coefficients[2],
                  Disney.fit.rozdiely$coefficients[2],
                  eBay.fit.rozdiely$coefficients[2],
                  Ford.fit.rozdiely$coefficients[2],
                  Facebook.fit.rozdiely$coefficients[2],
                  Google.fit.rozdiely$coefficients[2],
                  Intel.fit.rozdiely$coefficients[2],
                  JNJ.fit.rozdiely$coefficients[2],
                  KO.fit.rozdiely$coefficients[2],
                  Microsoft.fit.rozdiely$coefficients[2],
                  Netflix.fit.rozdiely$coefficients[2],
                  Nokia.fit.rozdiely$coefficients[2],
                  NVIDIA.fit.rozdiely$coefficients[2],
                  Starbucks.fit.rozdiely$coefficients[2],
                  Sony.fit.rozdiely$coefficients[2],
                  Tesla.fit.rozdiely$coefficients[2],
                  Vodafone.fit.rozdiely$coefficients[2])
##########################################################################################

##########################################################################################
konstanta_rozdiel <- c(
                       Apple.fit.rozdiely$coefficients[1],
                       AMD.fit.rozdiely$coefficients[1],
                       Amazon.fit.rozdiely$coefficients[1],
                       Boeing.fit.rozdiely$coefficients[1],
                       Disney.fit.rozdiely$coefficients[1],
                       eBay.fit.rozdiely$coefficients[1],
                       Ford.fit.rozdiely$coefficients[1],
                       Facebook.fit.rozdiely$coefficients[1],
                       Google.fit.rozdiely$coefficients[1],
                       Intel.fit.rozdiely$coefficients[1],
                       JNJ.fit.rozdiely$coefficients[1],
                       KO.fit.rozdiely$coefficients[1],
                       Microsoft.fit.rozdiely$coefficients[1],
                       Netflix.fit.rozdiely$coefficients[1],
                       Nokia.fit.rozdiely$coefficients[1],
                       NVIDIA.fit.rozdiely$coefficients[1],
                       Starbucks.fit.rozdiely$coefficients[1],
                       Sony.fit.rozdiely$coefficients[1],
                       Tesla.fit.rozdiely$coefficients[1],
                       Vodafone.fit.rozdiely$coefficients[1])
##########################################################################################

# ked uz mame vypocitane Bety mozeme si vypocitat ocakavane vynosy nasich akcii
##########################################################################################
NASDAQCOM.predict <- predict.lm(NASDAQCOM.fit)
Apple.predict <- predict.lm(Apple.fit)
AMD.predict <- predict.lm(AMD.fit)
Amazon.predict <- predict.lm(Amazon.fit)
Boeing.predict <- predict.lm(Boeing.fit)
Disney.predict <- predict.lm(Disney.fit)
eBayy.predict <- predict.lm(eBay.fit)
Ford.predict <- predict.lm(Ford.fit)
Facebook.predict <- predict.lm(Facebook.fit)
Google.predict <- predict.lm(Google.fit)
Intel.predict <- predict.lm(Intel.fit)
JNJ.predict <- predict.lm(JNJ.fit)
KO.predict <- predict.lm(KO.fit)
Microsoft.predict <- predict.lm(Microsoft.fit)
Netflix.predict <- predict.lm(Netflix.fit)
Nokia.predict <- predict.lm(Nokia.fit)
NVIDIA.predict <- predict.lm(NVIDIA.fit)
Starbucks.predict <- predict.lm(Starbucks.fit)
Sony.predict <- predict.lm(Sony.fit)
Tesla.predict <- predict.lm(Tesla.fit)
Vodafone.predict <- predict.lm(Vodafone.fit)
##########################################################################################

# tabulka
##########################################################################################
predict_returns_df <- tibble(
    NASDAQCOM.predict = NASDAQCOM.predict,
    Apple.predict = Apple.predict,
    AMD.predict = AMD.predict,
    Amazon.predict = Amazon.predict,
    Boeing.predict = Boeing.predict,
    Disney.predict = Disney.predict,
    eBayy.predict = eBayy.predict,
    Ford.predict = Ford.predict,
    Facebook.predict = Facebook.predict,
    Google.predict = Google.predict,
    Intel.predict = Intel.predict,
    JNJ.predict = JNJ.predict,
    KO.predict = KO.predict,
    Microsoft.predict = Microsoft.predict,
    Netflix.predict = Netflix.predict,
    Nokia.predict = Nokia.predict,
    NVIDIA.predict = NVIDIA.predict,
    Starbucks.predict = Starbucks.predict,
    Sony.predict = Sony.predict,
    Tesla.predict = Tesla.predict,
    Vodafone.predict = Vodafone.predict)
##########################################################################################
