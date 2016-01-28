#Nicole E Soltis
#Managing data tables in R
#03/04/13
#RSelfHelp
#-------------------------------------------------------------
#read csv file and make a new object from HWAC data
HWA.Carb <-read.csv("d13CuptakeLG.csv")

#make a .csv file from matrix edited in R
write.csv(iris, "iris.csv") #writes to current working directory

#rename an object (can help avoid overwriting)
HWA.RSA <- HWA.Carb # new <- original

#reorders data.frame by column 1 (replicate)
HWA.RSA[order(HWA.RSA[,1]),]

#remove column "d13Cex"
HWA.RSA <- HWA.RSA[-grep('d13Cex',colnames(HWA.RSA))]

#subset only certain columns
CNsort2 <- subset(CNsort, select=c(Tx,TimeScaled,TissueAge,Npercent))

#add a column
#DoseR is new table
#Dose is original table
#logDoseR is new variable
#NtSleep is original variable
DoseR <- transform(Dose, logDoseR=log(Dose$NtSleep))

#explicitly order the means for graphing
df.N$Treatment = factor(df.N$Treatment,c("ControlOld","ControlNew","ControlWee",
                                         "HWAOld","HWANew","HWAWee"))

#-------------------------------------------------------------------
#add a column of site names
sites <-gl(3,)
#add a column of Ec values
natabun <- gl(6,26,156,labels=c(-27.06355, -26.57383,
                                -28.79079, -27.86536, -26.48491, -27.05925))
#make them numeric
natabun <- as.numeric(levels(natabun))[natabun]
HWA.RSA["Ec"] <-NA
HWA.RSA$Ec <-natabun
#-------------------------------------------------------------------
#remove incomplete cases
SmBrcc <- na.omit(SmBr)

#input data
count <- c(152,39,53,6)
#gl() generate levels of a factor
# gl(numberoflevels,numberofreplicates,lengthofresults)
type <- gl(4,1,4,c("yellowsmooth","yellowwrinkled","greensmooth",
                   "greenwrinkled"))
#build a data.frame, listed by type of seed and number of counts
seeds <- data.frame(type,count)
#tabulate that -- now it's a table! how fancy
seeds.xtab <-xtabs(count~type, seeds)

#build a matrix
cancer.status <-matrix(c(683,1498,2537,8747),2)
#add labels to a matrix
rownames(cancer.status) <-c("Yes", "No")
colnames(cancer.status)<-c(">30","<29")

#-----------------------------------------------------------------
#gives blood.glucose but only for patients with values 
#for short.velocity
lines(blood.glucose[!is.na(short.velocity)],fitted(lm.velo))
#OR
cc <- complete.cases(thuesen)
#OR
options(na.action=na.exclude)
lm.velo <- lm(short.velocity~blood.glucose)

fitted(lm.velo)
segments(blood.glucose,fitted(lm.velo),
         blood.glucose,short.velocity)

#---------------------------------------------------------------
#how to make scatterplot with long format data
#turn into wide format
str(Br.Mech)
head(Br.Mech)
summary(Br.Mech)
#need to add plant ID so that the reshape knows what to do
Br.Mech.Lg <-transform(Br.Mech, plant.ID=1:59)
head(Br.Mech.Lg)
#a.wide <- reshape(data set, direction="wide", 
#v.names="variables in the long format that correspond to 
#multiple variables in the wide format", 
#idvar="names that identify records from the same group or individual"
#timevar="variable in long format that differentiates multiple 
#records from the same group")    
i.wide <- reshape(irisID,direction="wide", 
                  v.names="Length", 
                  timevar="Organ", idvar="plant.ID")
head(i.wide)
head(iris)
colnames(i.wide) <- c("Species","plant.ID","Sepal.Length",
                      "Petal.Width","Sepal.Width","Petal.Width")
head(i.wide)

