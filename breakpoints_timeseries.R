library(strucchange)
library(tidyquant)

# na fungovanie tohoto kodu treba najprv nacitat premenne a tabulky z kodu CAPM.R
##########################################################################################


 aa <- ts(returns_df)



 plot(aa[,"Apple"])
 fs.apple <- Fstats(aa[,"Apple"] ~ 1)
 plot(fs.apple)
 breakpoints(fs.apple)
 lines(breakpoints(fs.apple))
 ## or
 bp.apple <- breakpoints(aa[,"Apple"] ~ 1)
 summary(bp.apple)
 ## the BIC also chooses one breakpoint
 plot(bp.apple)
 breakpoints(bp.apple)


 # apple
 bp.apple <- breakpoints(aa[,"Apple"] ~ 1)
 fm1a <- lm(aa[,"Apple"] ~ 1)
 fm1b <- lm(aa[,"Apple"] ~ breakfactor(bp.apple, breaks = 1))
 plot(aa[,"Apple"])
 lines(ts(fitted(fm1a), start = 0), col = 3)
 lines(ts(fitted(fm1b), start = 0), col = 4)


 #AMD
 bp.amd <- breakpoints(aa[,"AMD"] ~ 1)
 fm2a <- lm(aa[,"AMD"] ~ 1)
 fm2b <- lm(aa[,"AMD"] ~ breakfactor(bp.amd, breaks = 1))
 plot(aa[,"AMD"])
 lines(ts(fitted(fm2a), start = 0), col = 3)
 lines(ts(fitted(fm2b), start = 0), col = 4)


# Amazon
bp.amazon <- breakpoints(aa[,"Amazon"] ~ 1)
fm3a <- lm(aa[,"Amazon"] ~ 1)
fm3b <- lm(aa[,"Amazon"] ~ breakfactor(bp.amazon, breaks = 1))
plot(aa[,"Amazon"])
lines(ts(fitted(fm3a), start = 0), col = 3)
lines(ts(fitted(fm3b), start = 0), col = 4)

# Boeing
bp.boeing <- breakpoints(aa[,"Boeing"] ~ 1)
fm4a <- lm(aa[,"Boeing"] ~ 1)
fm4b <- lm(aa[,"Boeing"] ~ breakfactor(bp.boeing, breaks = 1))
plot(aa[,"Boeing"])
lines(ts(fitted(fm4a), start = 0), col = 3)
lines(ts(fitted(fm4b), start = 0), col = 4)

# Disney
bp.disney <- breakpoints(aa[,"Disney"] ~ 1)
fm5a <- lm(aa[,"Disney"] ~ 1)
fm5b <- lm(aa[,"Disney"] ~ breakfactor(bp.disney, breaks = 1))
plot(aa[,"Disney"])
lines(ts(fitted(fm5a), start = 0), col = 3)
lines(ts(fitted(fm5b), start = 0), col = 4)

# eBay
bp.ebay <- breakpoints(aa[,"eBay"] ~ 1)
fm6a <- lm(aa[,"eBay"] ~ 1)
fm6b <- lm(aa[,"eBay"] ~ breakfactor(bp.ebay, breaks = 1))
plot(aa[,"eBay"])
lines(ts(fitted(fm6a), start = 0), col = 3)
lines(ts(fitted(fm6b), start = 0), col = 4)

# Ford
bp.ford <- breakpoints(aa[,"Ford"] ~ 1)
fm7a <- lm(aa[,"Ford"] ~ 1)
fm7b <- lm(aa[,"Ford"] ~ breakfactor(bp.ford, breaks = 1))
plot(aa[,"Ford"])
lines(ts(fitted(fm7a), start = 0), col = 3)
lines(ts(fitted(fm7b), start = 0), col = 4)

# Facebook
bp.facebook <- breakpoints(aa[,"Facebook"] ~ 1)
fm8a <- lm(aa[,"Facebook"] ~ 1)
fm8b <- lm(aa[,"Facebook"] ~ breakfactor(bp.facebook, breaks = 1))
plot(aa[,"Facebook"])
lines(ts(fitted(fm8a), start = 0), col = 3)
lines(ts(fitted(fm8b), start = 0), col = 4)

# Google
bp.google <- breakpoints(aa[,"Google"] ~ 1)
fm9a <- lm(aa[,"Google"] ~ 1)
fm9b <- lm(aa[,"Google"] ~ breakfactor(bp.google, breaks = 1))
plot(aa[,"Google"])
lines(ts(fitted(fm9a), start = 0), col = 3)
lines(ts(fitted(fm9b), start = 0), col = 4)

# Intel
bp.intel <- breakpoints(aa[,"Intel"] ~ 1)
fm10a <- lm(aa[,"Intel"] ~ 1)
fm10b <- lm(aa[,"Intel"] ~ breakfactor(bp.intel, breaks = 1))
plot(aa[,"Intel"])
lines(ts(fitted(fm10a), start = 0), col = 3)
lines(ts(fitted(fm10b), start = 0), col = 4)

# JNJ
bp.JNJ <- breakpoints(aa[,"JNJ"] ~ 1)
fm11a <- lm(aa[,"JNJ"] ~ 1)
fm11b <- lm(aa[,"JNJ"] ~ breakfactor(bp.JNJ, breaks = 1))
plot(aa[,"JNJ"])
lines(ts(fitted(fm11a), start = 0), col = 3)
lines(ts(fitted(fm11b), start = 0), col = 4)

# KO
bp.KO <- breakpoints(aa[,"KO"] ~ 1)
fm12a <- lm(aa[,"KO"] ~ 1)
fm12b <- lm(aa[,"KO"] ~ breakfactor(bp.KO, breaks = 1))
plot(aa[,"KO"])
lines(ts(fitted(fm12a), start = 0), col = 3)
lines(ts(fitted(fm12b), start = 0), col = 4)

# Microsoft
bp.microsoft <- breakpoints(aa[,"Microsoft"] ~ 1)
fm13a <- lm(aa[,"Microsoft"] ~ 1)
fm13b <- lm(aa[,"Microsoft"] ~ breakfactor(bp.microsoft, breaks = 1))
plot(aa[,"Microsoft"])
lines(ts(fitted(fm13a), start = 0), col = 3)
lines(ts(fitted(fm13b), start = 0), col = 4)

# Netflix
bp.netflix <- breakpoints(aa[,"Netflix"] ~ 1)
fm14a <- lm(aa[,"Netflix"] ~ 1)
fm14b <- lm(aa[,"Netflix"] ~ breakfactor(bp.netflix, breaks = 1))
plot(aa[,"Netflix"])
lines(ts(fitted(fm14a), start = 0), col = 3)
lines(ts(fitted(fm14b), start = 0), col = 4)

# Nokia
bp.nokia <- breakpoints(aa[,"Nokia"] ~ 1)
fm15a <- lm(aa[,"Nokia"] ~ 1)
fm15b <- lm(aa[,"Nokia"] ~ breakfactor(bp.nokia, breaks = 1))
plot(aa[,"Nokia"])
lines(ts(fitted(fm15a), start = 0), col = 3)
lines(ts(fitted(fm15b), start = 0), col = 4)

# NVIDIA
bp.NVIDIA <- breakpoints(aa[,"NVIDIA"] ~ 1)
fm16a <- lm(aa[,"NVIDIA"] ~ 1)
fm16b <- lm(aa[,"NVIDIA"] ~ breakfactor(bp.NVIDIA, breaks = 1))
plot(aa[,"NVIDIA"])
lines(ts(fitted(fm16a), start = 0), col = 3)
lines(ts(fitted(fm16b), start = 0), col = 4)

# Starbucks
bp.Starbucks <- breakpoints(aa[,"Starbucks"] ~ 1)
fm17a <- lm(aa[,"Starbucks"] ~ 1)
fm17b <- lm(aa[,"Starbucks"] ~ breakfactor(bp.Starbucks, breaks = 1))
plot(aa[,"Starbucks"])
lines(ts(fitted(fm17a), start = 0), col = 3)
lines(ts(fitted(fm17b), start = 0), col = 4)

# Sony
bp.Sony <- breakpoints(aa[,"Sony"] ~ 1)
fm18a <- lm(aa[,"Sony"] ~ 1)
fm18b <- lm(aa[,"Sony"] ~ breakfactor(bp.Sony, breaks = 1))
plot(aa[,"Sony"])
lines(ts(fitted(fm18a), start = 0), col = 3)
lines(ts(fitted(fm18b), start = 0), col = 4)

# Tesla
bp.Tesla <- breakpoints(aa[,"Tesla"] ~ 1)
fm19a <- lm(aa[,"Tesla"] ~ 1)
fm19b <- lm(aa[,"Tesla"] ~ breakfactor(bp.Tesla, breaks = 1))
plot(aa[,"Tesla"])
lines(ts(fitted(fm19a), start = 0), col = 3)
lines(ts(fitted(fm19b), start = 0), col = 4)

# Vodafone
bp.Vodafone <- breakpoints(aa[,"Vodafone"] ~ 1)
fm20a <- lm(aa[,"Vodafone"] ~ 1)
fm20b <- lm(aa[,"Vodafone"] ~ breakfactor(bp.Vodafone, breaks = 1))
plot(aa[,"Vodafone"])
lines(ts(fitted(fm20a), start = 0), col = 3)
lines(ts(fitted(fm20b), start = 0), col = 4)
