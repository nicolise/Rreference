#Nicole E Soltis
#ANOVAS in R
#03/04/13
#RSelfHelp
#-------------------------------------------------------------
rm(list=ls())
#learn current directory, and set it to the correct one
getwd()
setwd("~/TUFTS/Research/RAnalyses/ResearchAnalyses")

#read csv file and make a new object from HWAC data
HWA.Carb <-read.csv("RAnalysisHWACnatabun.csv")
#check structure of new object
str(HWA.Carb)
#read column names
attach(HWA.Carb)
names(HWA.Carb)
class(HWA.Carb) #it's a data.frame
#---------------------------------------------------------------
#add power analysis
library(pwr)
#one-way anova
pwr.anova.test(k=5,f=0.25,sig.level=0.05,power=0.8)
#two-way anova
#cohen's f2, related to R-squared (model fit)
#u is df of numerator
    #= ((levels of factor 1)-1) * ((levels of factor 2)-1)
#v is total number of subjects across all categories
    #MINUS (levels of factor 1)*(levels of factor 2)
    #these values are given in anova table
pwr.f2.test(u=2, v=294, sig.level=0.05, power=0.8)
#f2 is Cohen's f-squared value, ~R-squared, so effect size low.
#by convention, f2 0.02 is small, 0.15 is medium, 0.35 is large
#--------------------------------------------------------------
#ASSUMPTION OF NORMALITY
#check for normality
hist(fnew)
shapiro.test(fnew) #not normal
transf <- (log((fnew)))
hist(transf)
shapiro.test(transf)

#negative skew: small to large
transf <- (Cpercent)^2
transf <- (Cpercent)^3
#positive skew: small to large
transf <- (Cpercent)^.5
transf <- log(Cpercent)
transf <- log10(CtoN)
transf <- (-1/((Cpercent)^.5))

#the usual transformations don't help
library(car)
#transform by BOX-COX scaled power transformations 
#(U^((lambda)-1)/lambda
traf <- bcPower(fnew+1,45)
hist(traf)
shapiro.test(traf) #made it: p=0.06328
HWA.RSA <-transform(HWA.RSA, traf = (bcPower(fnew+1,45)))
attach(HWA.RSA)
View(HWA.RSA)
#----------------------------------------------------------------
#ASSUMPTION OF HOMOSCEDASTICITY
#las=3 rotates x-axis labels
boxplot(traf~InsTx*Tissue,
        ylab="RSA", main="Boxplot of RSA", las=3)
bartlett.test(traf~InsTx*Tissue) 
#OR
leveneTest(traf~InsTx)
#OR
var.test(CGMS~CGTx)


#----------------------------------------------------------------
#PARAMETRIC TESTS
#two-way ANOVA
RSA.ANOVA <- anova(lm(traf~InsTx*Tissue))
summary(RSA.ANOVA)
RSA.ANOVA # InsTx p=0.04463
#Tissue p<0.001
#InsTx:Tissue p=0.03972
interaction.plot(Tissue,InsTx,traf)

#example: two-way ANOVA: full factorial
#assume that the two factors are not correlated
rm(list=ls())
setwd()
TGM <-read.csv("ToothGrowthMod.csv")
attach(TGM)

#biological question: is there a difference in tooth length
#as a factor of supplement and dose?
str(TGM)
boxplot(Length~Supp*Dose, data = TGM,
        ylab="Length", main="Boxplot of TGM")
#could apply a transformation if it looks bad
bartlett.test(Length~Supp*Dose,data=TGM) #p=0.2331, good.
#want to look at residuals so, make a new residuals object
TGMResid <-lm(Length~Supp*Dose)
#run shapiro-wilk goodness of fit test on the residuals
shapiro.test(residuals(TGMResid)) #p=0.67 so all ~normally dist
anova(TGMResid)
plot(TGMResid)
TGM.ANOVA <-anova(lm(Length~Supp*Dose))
#OR
TGM.aov <- aov(Length~Supp*Dose)
plot(TGM.aov)
summary(TGM.aov)

#post-hoc tests
TukeyHSD(TGM.aov)
interaction.plot(Dose,Supp,Length)

#----------------------------------------------------------------
#SPECIAL CASES
#manova
solution <- (manova(cbind(hdlngth,skullw,totlngth,taill,
                          footlgth,earconch,
                          eye,chest,belly)~as.factor(sex)))
# Overall significance of model
summary(solution)
# Pairwise t-tests (M vs. F for each dependent variable)
summary.aov(solution)

# 2x2 Factorial MANOVA with 3 Dependent Variables. 
Y <- cbind(y1,y2,y3)
fit <- manova(Y ~ A*B)
summary(fit, test="Pillai")

#Other test options are "Wilks", "Hotelling-Lawley", and "Roy". 
#Use summary.aov( ) to get univariate statistics. TukeyHSD( ) 
#and plot( ) will not work with a MANOVA fit. Run each dependent 
#variable separately to obtain them. Like ANOVA, MANOVA results 
#in R are based on Type I SS. To obtain Type III SS, vary the 
#order of variables in the model and rerun the analyses. 
#For example, fit y~A*B for the TypeIII B effect and y~B*A for 
#the Type III A effect.

#-----------------------------------------------------------------
#NONPARAMETRIC TESTS
#Kruskal-Wallis: nonparametric one-way ANOVA
kruskal.test(fnew~InsTx) #p=0.419
kruskal.test(fnew~Tissue) #p<0.001

#nonparametric alternative to two-way ANOVA
#equivalent to sign test if 2 columns
friedman.test(fnew~Tissue|InsTx,data=HWA.RSA) #FIX THIS