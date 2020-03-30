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

df
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
returns_df <- tibble(   Apple = ROC(df$Apple),
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
                        r_f = df$r_f,
                        r_m = ROC(df$r_m)
)


# vymazanie prveho riadku s NA hodnotam
returns_df <- na.omit(returns_df)


# datumy ako nazvy riadkov (mozno pre dalsie pouzitie)
#rownames(returns_df) <- dates[2:60]
Date <- dates[2:60]
#akcie <- colnames(returns_df)


# graf vynosov akcii 1-5
##########################################################################################
ret_1_5 <- returns_df %>%
    ggplot(aes(x = Date)) +
    geom_line(aes(y = Apple,colour = "Apple")) +
    geom_line(aes(y = AMD,colour = "AMD")) +
    geom_line(aes(y = Amazon,colour = "Amazon")) +
    geom_line(aes(y = Boeing,colour = "Boeing")) +
    geom_line(aes(y = Disney, colour = "Disney")) +
    scale_colour_manual("Akcie",
                        breaks = c("Apple", "AMD","Amazon","Boeing","Disney"),
                        values = c( "Apple" = "grey2",
                                    "AMD" = "blue4","Amazon" = "sienna3",
                                    "Boeing" = "green","Disney" = "black")) +
    theme_classic() +
    theme(legend.position = "right") +
    scale_x_date(breaks = date_breaks("year"),labels = date_format("%Y"),) +
    labs(title = "Graf vynosov akcii 1-5",x = "Time series", y = "Returns")

ret_1_5
##########################################################################################

# graf vynosov akcii 6-10
##########################################################################################
ret_6_10 <- returns_df %>%
    ggplot(aes(x = Date)) +
    geom_line(aes(y = eBay,colour = "eBay")) +
    geom_line(aes(y = Ford,colour = "Ford")) +
    geom_line(aes(y = Facebook,colour = "Facebook")) +
    geom_line(aes(y = Google,colour = "Google")) +
    geom_line(aes(y = Intel, colour = "Intel")) +
    scale_colour_manual("Akcie",
                        breaks = c("eBay", "Ford","Facebook","Google","Intel"),
                        values = c( "eBay"="grey2","Ford"="blue4","Facebook"="sienna3",
                                    "Google"="green","Intel" = "black")) +
    theme_classic() +
    theme(legend.position = "right") +
    scale_x_date(breaks = date_breaks("year"),labels = date_format("%Y"),) +
    labs(title = "Graf vynosov akcii 6-10",x = "Time series", y = "Returns")
ret_6_10
##########################################################################################

# graf vynosov akcii 11-15
##########################################################################################
ret_11_15 <- returns_df %>%
    ggplot(aes(x = Date)) +
    geom_line(aes(y = JNJ,colour = "JNJ")) +
    geom_line(aes(y = KO,colour = "KO")) +
    geom_line(aes(y = Microsoft,colour = "Microsoft")) +
    geom_line(aes(y = Netflix,colour = "Netflix")) +
    geom_line(aes(y = Nokia, colour = "Nokia")) +
    scale_colour_manual("Akcie",
                        breaks = c("JNJ", "KO","Microsoft","Netflix","Nokia"),
                        values = c("JNJ"="grey2","KO"="blue4","Microsoft"="sienna3",
                                   "Netflix"="green","Nokia"="black")) +
    theme_classic() +
    theme(legend.position = "right") +
    scale_x_date(breaks = date_breaks("year"),labels = date_format("%Y"),) +
    labs(title = "Graf vynosov akcii 11-15",x = "Time series", y = "Returns")

ret_11_15
##########################################################################################

# graf vynosov akcii 16-20
##########################################################################################
ret_16_20 <- returns_df %>%
    ggplot(aes(x = Date)) +
    geom_line(aes(y = NVIDIA,colour = "NVIDIA")) +
    geom_line(aes(y = Starbucks,colour = "Starbucks")) +
    geom_line(aes(y = Sony,colour = "Sony")) +
    geom_line(aes(y = Tesla,colour = "Tesla")) +
    geom_line(aes(y = Vodafone,colour = "Vodafone")) +
    scale_colour_manual("Akcie",
                        breaks = c("NVIDIA", "Starbucks","Sony","Tesla","Vodafone"),
                        values = c("NVIDIA"="grey2","Starbucks"="blue4","Sony"="sienna3",
                                   "Tesla"="green","Vodafone"="black")) +
    theme_classic() +
    theme(legend.position = "right") +
    scale_x_date(breaks = date_breaks("year"),labels = date_format("%Y"),) +
    labs(title = "Graf vynosov akcii 16-20",x = "Time series", y = "Returns")

ret_16_20
##########################################################################################


returns_df$risk.premium <- returns_df$r_m - returns_df$r_f


# graf bezrizikovej urokovej miery
returns_df %>%
    ggplot(aes(x = Date, y = r_f)) +
    geom_line() +
    theme_bw() +
    labs(title = "Graf vyvoja bezrizikovej urokovej miery",x = "Time series",
         y = "r_f (Bezrizikova urokova miera)")


# graf  ocakavanej vynosnosti trhu
returns_df %>%
    ggplot(aes(x = Date,y = r_m)) +
    geom_line() +
    theme_bw() +
    labs(title = "Graf ocakavaneho vynosu porfolia",x = "Time series",
         y = "r_m (Ocakavny vynos)")


# graf prveho porfolia (ostatne rovnako)
ggplot(data = returns_df, aes(y = Apple, x = risk.premium)) +
    geom_point(col = 'blue') +
    xlab('Risk Premium') +
    ylab('Expected Return') +
    theme_bw() +
    ggtitle('Apple Stock Returns') +
    geom_abline(method = 'lm')
# It’s worth noting that the higher the risk premium, the greater the expected return should be



# Potrebujeme vytvorit regresne modely pre vsetky nase akcie (portfoliia) - je ich 20
##########################################################################################
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


# zistime co vsetko sa da z toho vytiahnut
#names(summary(NASDAQCOM.fit.rozdiely))


# Vytvoreny vektor akcii
stock <- c('Apple', 'AMD', 'Amazon', 'Boeing', 'Disney',"eBay","Ford",
           "Facebook","Google","Intel","JNJ","KO","Microsoft","Netflix","Nokia","NVIDIA",
           "Starbucks","Sony","Tesla","Vodafone")

# vektor ocakavanych vynosov(Beta) z lin. modelu
##########################################################################################
expected.return <- c(Apple.fit$coefficients[3],
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

#How do we interpret the values of the risk premiums?
#Well, the higher the risk premium the more volatile or risky the asset is, and therefore
#investors should be compennsated with a return that justifies the risk of the asset.


# ked uz mame vypocitane Bety mozeme si vypocitat ocakavane vynosy nasich akcii
##########################################################################################
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

# predict_returns_df - pewdikovane vynosy
##########################################################################################
predict_returns_df <- tibble(
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


# spojenie dvoch tabuliek dokopy na vykreslenie grafov
tabulka <- cbind(returns_df,predict_returns_df)

# Fitovanie regresii
##########################################################################################
#theme_set(theme_bw())

#Apple
Apple.plot <- ggplot(tabulka, aes(y = Apple.predict, x = risk.premium)) +
    geom_smooth(col='green', method='lm') +
    geom_point(aes(y = Apple, x = risk.premium)) +
    theme_set(theme_bw()) +
    ggtitle('Apple') +
    xlab('Risk Premium') +
    ylab('Expected Return')
#Apple.plot

#AMD
AMD.plot <- ggplot(tabulka, aes(y = AMD.predict, x = risk.premium)) +
    geom_smooth(col='blue', method='lm') +
    geom_point(aes(y = AMD, x = risk.premium), col='red') +
    theme_set(theme_bw()) +
    ggtitle('AMD') +
    xlab('Risk Premium') +
    ylab('Expected Return')

# KO
KO.plot <- ggplot(tabulka, aes(y = KO.predict, x = risk.premium)) +
    geom_smooth(col='tomato2', method='lm') +
    geom_point(aes(y = KO, x = risk.premium)) +
    theme_set(theme_bw()) +
    ggtitle('KO') +
    xlab('Risk Premium') +
    ylab('Expected Return')


# ostatne akcie uplne rovnako len treba pouzit prisluchajuce data
##########################################################################################


# hodnory alpha, beta a p-hodnoty
##########################################################################################
tab <- data.frame(Date,returns_df$Apple,returns_df$AMD,returns_df$Amazon,
                  returns_df$Boeing,returns_df$Disney,returns_df$eBay,returns_df$Ford,
                  returns_df$Facebook,returns_df$Google,returns_df$Intel,returns_df$JNJ,
                  returns_df$KO,returns_df$Microsoft,returns_df$Netflix,returns_df$Nokia,
                  returns_df$NVIDIA,returns_df$Starbucks,returns_df$Sony,returns_df$Tesla,
                  returns_df$Vodafone,returns_df$r_m,returns_df$r_f)



#spravny format dat
tab <- xts(tab[,2:length(tab)],order.by = as.Date(tab[,1]))

#summary(lm(tab$returns_df.NASDAQCOM~tab$returns_df.r_m))$coefficients[1,1]

alpha <- c(summary(lm(tab$returns_df.Apple~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.AMD~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Amazon~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Boeing~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Disney~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.eBay~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Ford~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Facebook~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Google~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Intel~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.JNJ~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.KO~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Microsoft~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Netflix~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Nokia~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.NVIDIA~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Starbucks~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Sony~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Tesla~tab$returns_df.r_m))$coefficients[1,1],
           summary(lm(tab$returns_df.Vodafone~tab$returns_df.r_m))$coefficients[1,1])

alpha_p_val <- c(summary(lm(tab$returns_df.Apple~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.AMD~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Amazon~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Boeing~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Disney~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.eBay~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Ford~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Facebook~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Google~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Intel~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.JNJ~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.KO~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Microsoft~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Netflix~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Nokia~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.NVIDIA~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Starbucks~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Sony~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Tesla~tab$returns_df.r_m))$coefficients[1,4],
                 summary(lm(tab$returns_df.Vodafone~tab$returns_df.r_m))$coefficients[1,4])

Beta <- c(summary(lm(tab$returns_df.Apple~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.AMD~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Amazon~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Boeing~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Disney~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.eBay~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Ford~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Facebook~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Google~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Intel~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.JNJ~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.KO~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Microsoft~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Netflix~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Nokia~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.NVIDIA~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Starbucks~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Sony~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Tesla~tab$returns_df.r_m))$coefficients[2,1],
          summary(lm(tab$returns_df.Vodafone~tab$returns_df.r_m))$coefficients[2,1])

Beta_p_val <-  c(summary(lm(tab$returns_df.Apple~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.AMD~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Amazon~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Boeing~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Disney~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.eBay~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Ford~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Facebook~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Google~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Intel~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.JNJ~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.KO~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Microsoft~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Netflix~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Nokia~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.NVIDIA~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Starbucks~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Sony~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Tesla~tab$returns_df.r_m))$coefficients[2,4],
                 summary(lm(tab$returns_df.Vodafone~tab$returns_df.r_m))$coefficients[2,4])


tab_uloha_3 <- tibble(stock,alpha,alpha_p_val,Beta,Beta_p_val)
##########################################################################################

# tabulka s hodnotami beta
#Tabulka_vynosnosti <- data.frame(stock, Beta)




#CAPM.CML.slope(Ra = returns_df$Apple,Rb = returns_df$r_m,Rf = returns_df$r_f)

# alpha
alpha_function <- c(CAPM.alpha(tab[,1,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,2,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,3,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,4,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,5,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,6,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,7,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,8,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,9,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,10,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,11,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,12,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,13,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,14,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,15,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,16,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,17,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,18,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,19,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                    CAPM.alpha(tab[,20,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]))



# beta
beta_function <- c(CAPM.beta(tab[,1,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,2,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,3,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,4,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,5,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,6,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,7,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,8,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,9,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,10,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,11,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,12,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,13,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,14,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,15,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,16,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,17,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,18,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,19,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]),
                   CAPM.beta(tab[,20,drop=FALSE], tab[,21,drop=FALSE], Rf=tab[,22,drop=FALSE]))

tab_returns_function <- tibble(Akcia = stock,
                               Alpha = alpha_function,
                               Beta = beta_function)


# iba pre zaujimvost ukazany rozdiel medzi dvomi odhadmi parametrov alpha a beta
#abs(tab_uloha_3$alpha - tab_returns_function$Alpha)
#abs(tab_uloha_3$Beta - tab_returns_function$Beta)




##########################################################################################

#library(strucchange)

mean_returns <- c(mean(returns_df$Apple),
                  mean(returns_df$AMD),
                  mean(returns_df$Amazon),
                  mean(returns_df$Boeing),
                  mean(returns_df$Disney),
                  mean(returns_df$eBay),
                  mean(returns_df$Ford),
                  mean(returns_df$Facebook),
                  mean(returns_df$Google),
                  mean(returns_df$Intel),
                  mean(returns_df$JNJ),
                  mean(returns_df$KO),
                  mean(returns_df$Microsoft),
                  mean(returns_df$Netflix),
                  mean(returns_df$Nokia),
                  mean(returns_df$NVIDIA),
                  mean(returns_df$Starbucks),
                  mean(returns_df$Sony),
                  mean(returns_df$Tesla),
                  mean(returns_df$Vodafone))

# standartne odchylky vynosov nasich akcii
sigma_2 <- c(sd(returns_df$Apple),
             sd(returns_df$AMD),
             sd(returns_df$Amazon),
             sd(returns_df$Boeing),
             sd(returns_df$Disney),
             sd(returns_df$eBay),
             sd(returns_df$Ford),
             sd(returns_df$Facebook),
             sd(returns_df$Google),
             sd(returns_df$Intel),
             sd(returns_df$JNJ),
             sd(returns_df$KO),
             sd(returns_df$Microsoft),
             sd(returns_df$Netflix),
             sd(returns_df$Nokia),
             sd(returns_df$NVIDIA),
             sd(returns_df$Starbucks),
             sd(returns_df$Sony),
             sd(returns_df$Tesla),
             sd(returns_df$Vodafone))
# rpi - rf (vynosy portfolii - bezrizikova urkova miera)
mean_rpi_rf_returns <- c(mean(returns_df$Apple - returns_df$r_f),
                         mean(returns_df$AMD - returns_df$r_f),
                         mean(returns_df$Amazon - returns_df$r_f),
                         mean(returns_df$Boeing - returns_df$r_f),
                         mean(returns_df$Disney - returns_df$r_f),
                         mean(returns_df$eBay- returns_df$r_f),
                         mean(returns_df$Ford - returns_df$r_f),
                         mean(returns_df$Facebook - returns_df$r_f),
                         mean(returns_df$Google - returns_df$r_f),
                         mean(returns_df$Intel - returns_df$r_f),
                         mean(returns_df$JNJ - returns_df$r_f),
                         mean(returns_df$KO - returns_df$r_f),
                         mean(returns_df$Microsoft - returns_df$r_f),
                         mean(returns_df$Netflix - returns_df$r_f),
                         mean(returns_df$Nokia - returns_df$r_f),
                         mean(returns_df$NVIDIA - returns_df$r_f),
                         mean(returns_df$Starbucks - returns_df$r_f),
                         mean(returns_df$Sony - returns_df$r_f),
                         mean(returns_df$Tesla - returns_df$r_f),
                         mean(returns_df$Vodafone - returns_df$r_f))


# uloha 4
# SMT
# vynosy : R_f + beta * risk.premium
# risk.premium = R_m - R_f
# cervene
#summary(lm(mean_rpi_rf_returns ~ tab_uloha_3$Beta + tab_uloha_3$Beta^2))
plot(tab_uloha_3$Beta, mean_returns, pch = 19,col = "red", xlab = "Beta",ylab = "Ocakavane vynosy")
abline(lm(mean_rpi_rf_returns ~ tab_uloha_3$Beta), col = "darkblue")
#abline(lm(mean_rpi_rf_returns ~ tab_uloha_3$Beta + beta_22), col = "green")
#abline(lm(mean_returns ~ tab_uloha_3$Beta + tab_uloha_3$Beta^2), col = "orange")

# hodnota druheho parametru by sa mala zhodovat s risk.premium
summary(lm(mean_rpi_rf_returns ~ tab_uloha_3$Beta + tab_uloha_3$Beta^2))



# percentualne R_m = 1%
#100 * mean(returns_df$r_m)
# percentualne risk.premium = 0.8%
#100 * mean(returns_df$risk.premium)
# percetualne R_f = 0.25%
#100 * mean(returns_df$r_f)
# ocakavany vynos podla CAPM
ocak_vynos_CAPM <- data.frame(Akcie = stock,
                              "Ocakavany vynos v percentach" = 100 *
                                  (rep(mean(returns_df$r_f),20) +
                                       (tab_uloha_3$Beta *
                                            rep(mean(returns_df$risk.premium),20))))
# tabulka ocakavanych vynosov pre nase akcie podla predpovede modelu CAPM
ocak_vynos_CAPM


# standartne odchzlky vynosov na druhu
sigma_2 <- sigma_2 * sigma_2

# dokazanie ze existuje iba linearny vztah a nie je kvadraticky vztah
summary(lm(mean_returns ~ tab_uloha_3$Beta + tab_uloha_3$Beta^2))
# nic sa nezmeni ani ked dame beta^3
#summary(lm(mean_returns ~ tab_uloha_3$Beta^2 + tab_uloha_3$Beta ))

# pre informacie o residuach modelu
# plot(lm(mean_returns ~ tab_uloha_3$Beta + tab_uloha_3$Beta^2))

# vybrane rezidualy z modelu a dane na 2
#sigma_22 <- summary(lm(mean_returns ~tab_uloha_3$Beta + tab_uloha_3$Beta^2))$residuals * summary(lm(mean_returns ~ tab_uloha_3$Beta + tab_uloha_3$Beta^2))$residuals

# Uloha 7
beta_22 <- tab_uloha_3$Beta^2
# vidime ze ani rozptyl rezidualvo nie je vyznamni
summary(lm(mean_returns ~ tab_uloha_3$Beta + beta_22 + sigma_2))

# summary(lm(mean_returns ~ tab_uloha_3$Beta + tab_uloha_3$Beta^2  + sigma_22))
# stredba hodnota residualov je nulova
#mean(summary(lm(mean_returns ~ tab_uloha_3$Beta^2))$residuals)







# Uloha 5

# odhad zlomu bety (kde budu 1 a kde 0 v modeli)
plot(tab_uloha_3$Beta, type = "l")
# sekneme to u 12 akcie (bety)

# premenne
dummy <- as.numeric(tab_uloha_3$Beta > 0)
dummy2 <- as.numeric(tab_uloha_3$Beta < 0)
beta1 <-  tab_uloha_3$Beta * dummy
beta2 <- tab_uloha_3$Beta * dummy2
# urobime 2 modely, jeden s dummy premennou a druhy bez nej
summary(lm(mean_returns ~ beta1 + beta2))

# zakladny velky model porovnany F-testom s mensim modelim len s koef Beta 1
anova(lm(mean_returns ~ beta1),lm(mean_returns ~ beta1 + beta2))
# zakladny velky model porovnany F-testom s mensim modelim len s koef Beta 2
anova(lm(mean_returns ~ beta2),lm(mean_returns ~ beta1 + beta2))
# ako vidime obe bety su vyznamne v nami zvolenom (rozdeleni podmodelu) podmodeli
# v oboch pripadoch mame vypocitanu aj F- statistiku hypotezy H_0

# priemerne vynosy akcii
#mean_returns
# priemerne vynosi akcie - risk free rate
#mean_rpi_rf_returns

# uloha 6

sigma_returns <- c(summary(lm(Apple ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(AMD ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Amazon ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Boeing ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Disney ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(eBay ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Ford ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Facebook ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Google ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Intel ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(JNJ ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(KO ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Microsoft ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Netflix ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Nokia ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(NVIDIA ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Starbucks ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Sony ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Tesla ~ r_f + risk.premium, data = returns_df))$sigma^2,
                   summary(lm(Vodafone ~ r_f + risk.premium, data = returns_df))$sigma^2
)


#summary(lm(rep(mean(returns_df$risk.premium),20) ~ tab_uloha_3$Beta + sigma_returns))

# toto by malo byt riesenie tej 6 ulohy
summary(lm(mean_returns ~ tab_uloha_3$Beta + sigma_returns))

uloha_5 <- tibble(Beta = tab_uloha_3$Beta,
                  Beta1 =beta1,
                  Beta2 =beta2,
                  skup = dummy,
                  akcie = mean_returns)



# graf k ulohe 6
beta1.plot <- ggplot(uloha_5, aes(y = Beta, x = akcie)) +
    geom_smooth(col='green', method='lm') +
    geom_point(aes(colour = Beta))+
    theme_set(theme_bw()) +
    ggtitle('Priemer vynosnosti akcii v zavyslosti na koeficientoch beta') +
    xlab('Priemer vynosnosti akcii') +
    ylab('Beta')



##ggplot(uloha_5, aes(y = Beta, x = akcie)) +
##    geom_smooth(col='green', method='lm') +
##    geom_point(aes(y = Beta, x =akcie,group = skup))+
##   theme_set(theme_bw()) +
##    ggtitle('') +
##   xlab('Priemer vynosnosti akcii') +
##    ylab('Beta')







# sumacia riadkov aby sa zistili vynosy v dany mesiac
returns_df <- returns_df %>% mutate(sum=Reduce("+",.[1:20]))

posit <- as.numeric(returns_df$sum > 0)
negat <- as.numeric(returns_df$sum < 0)


horna_beta <- c(summary(lm(Apple * posit ~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(AMD * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Amazon * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Boeing * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Disney * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(eBay * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Ford * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Facebook * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Google * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Intel * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(JNJ * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(KO * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Microsoft * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Netflix * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Nokia * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(NVIDIA * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Starbucks * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Sony * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Tesla * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Vodafone * posit~ r_f + risk.premium, data = returns_df))$coefficients[1,3])

dolna_beta <- c(summary(lm(Apple * negat ~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(AMD * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Amazon * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Boeing * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Disney * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(eBay * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Ford * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Facebook * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Google * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Intel * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(JNJ * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(KO * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Microsoft * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Netflix * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Nokia * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(NVIDIA * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Starbucks * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Sony * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Tesla * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3],
                summary(lm(Vodafone * negat~ r_f + risk.premium, data = returns_df))$coefficients[1,3])

# vysledky v tabulke pre dane akcie, ked sa trhu "dari" a "nedari"
uloha_6 <- tibble(akcie = stock,
                  "Negativne vynosy Beta" = dolna_beta,
                  "Pozitivne vynosy Beta" = horna_beta)

# testujeme ci su obe rozdelenia zhodne teda H_0 : horna_beta = dolna_beta
t.test(horna_beta,dolna_beta)
# z vysledkov vidime ze zamietame H_0 (zalezi na vynosoch akcii v dany den)
