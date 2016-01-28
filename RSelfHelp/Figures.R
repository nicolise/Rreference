#Nicole E Soltis
#FIGURES IN R
#03/04/13
#RSelfHelp
#-------------------------------------------------------------
#get coordinates from mouse clicks
locator() #hit Esc when done or right-click

#-------------------------------------------------------------
#save plot nicely
png(file="mygraphic.png",width=400,height=350)
plot(x=rnorm(10),y=rnorm(10),main="example")
dev.off()

#remove margin space for labels
png(file="notitle.png",width=400, height=350)
par(mar=c(5,3,2,2)+0.1)
hist(rnorm(100),ylab=NULL,main=NULL)
dev.off()

#multiple plots
opar <- par(mfrow=c(2,2), mex=0.6, mar=c(4,4,3,2)+.3)
plot(lm.velo, which=1:4)
par(opar) #reset to defaults at the end

#scatter plot of each variable against each other
#bits in caps are the variables
scatterplotMatrix(~RODENTSP+DISTX+AGE+PERSHRUB,data=bolger)

#3D plots: scatterplot3d
library(car)
HWA.Carb <- as.data.frame(HWA.Carb)
#(x,y,z)
with(HWA.Carb, scatter3d(d13Cex,CN,InsDens))

library(Rcmdr)
attach(mtcars)
scatter3d(wt, disp, mpg)

#FIGURES

#violin plot R (google it)

#las=3 rotates x-axis labels
boxplot(traf~InsTx*Tissue,
        ylab="RSA", main="Boxplot of RSA", las=3)

#calculate mean of each group using tapply, 
#which returns a table/matrix
mean.RSA <-tapply(fnew, list(InsTx, Tissue), mean)
mean.RSA
#calculate standard deviation of each group using tapply
#which returns a matrix of the same dimension as mean.fruit
sd.RSA <-tapply(fnew, list(InsTx, Tissue), sd)
sd.RSA
#Calculate sample size per group
n.RSA <-tapply(fnew, list(InsTx, Tissue), length)
n.RSA

#make a barplot using the means from tapply
# with x and y-axis labels
#and extend the y-axis length
#c means combine values into a vector
# packages for barplots with error bars:
#gplots, Hmics, plotrix, ggplot2

#make error bars from arrows
#mids assigns value returned by barplot to an object called mids
mids <- barplot(mean.RSA,
                xlab = "Tissue",
                ylab = "Relative Specific Allocation",
                ylim = c(-0.025,0.015),
                col = c("forestgreen","firebrick2", 
                        "cornflowerblue"),
                beside = TRUE,
                legend.text = TRUE,
                args.legend = list(x = "topleft"))

#another nice color combo
col = c("mediumseagreen","goldenrod1", 
        "mediumpurple4"),

#output includes x-axis midpoints of bars
#?arrows: x0 and x1 will be from mids.
#y0 will be mean - sd, y1 will be mean + sd
#use arrows to put error bars on the plot
arrows(mids, mean.RSA - (sd.RSA/(n.RSA^.5)), mids, 
       mean.RSA + (sd.RSA/(n.RSA^.5)),
       code = 3, angle = 90, length = 0.1)
#note that arrow tips are perpendicular to line segment
#angle = 90
#code =3: draw arrows at BOTH ends of lines
#length = 0.1mm arrows
#---------------------------------------------------------------
#barplot in plotrix
library(plotrix)
x <- c("LG","NL","NS","OL","OS","R","S")
#specify legend.pos if I don't want to use clickable locator
barp(mean.13C, names.arg=x, legend.lab=c("Control",
    "HWA","Natural Abundance"), col=c("mediumseagreen",
                "goldenrod1","mediumpurple4"), ylim=c(-2,4))
axis.break(axis=2,breakpos=4,pos=NA,bgcol="white",
           breakcol="black",
           style="slash",brw=0.02)
#----------------------------------------------------------------
#histogram with normal curve

#define data
x <- mtcars$mpg 
#draw histogram. breaks = breakpoints.
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

#histogram split by factor
#factors go in interaction, comma-separated
histogram(~Sepal.Length | interaction(Species), data = iris)
#---------------------------------------------------------------
#stripchart
#stripchart with means and SEM
xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
sem <- s/sqrt(n)
#vert = T rotates chart so that strips are vertical
#jitter staggers x-values so that all points are visible
stripchart(folate~ventilation, method="jitter",
           jitter=0.05, pch=16, vert=T)
#arrows adds the error bars
#arrows(x1, y1, x2, y2) for arrow endpoints
#length gives arrowhead length (inches)
#code=3 makes heads at both ends
#could also use xbar +- 2sem (confidence intervals)
#or xbar +- 2 sd
#or pooled sd if group sizes are very very small
arrows(1:3,xbar+sem,1:3,xbar-sem,angle=90,code=3,length=.1)
#indicate averages and connecting lines
#"b" is both: points and lines are printed
#with gaps in lines for symbols
#pch=4 is a cross
#cex=2 is 2x size
lines(1:3,xbar,pch=4,type="b",cex=2)