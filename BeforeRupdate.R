#Nicole E Soltis
#02/17/16
#steps to save a list of currently installed R packages prior to updating R, then to reinstall them in the new version of R
#script from Stackoverflow via Michael
#http://www.schulte-mecklenbeck.com/2011/04/resinstalling-packages-in-r-after-update/
#-----------------------------------------------------------------

#--run in the old version of R 
setwd("~/Projects")
packages <- installed.packages()[,"Package"] 
save(packages, file="Rpackages") 

#INSTALL NEW R VERSION (using installr)
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.

#--run in the new version 
setwd("~/Projects") 
load("Rpackages") 
for (p in setdiff(packages, installed.packages()[,"Package"])) 
  install.packages(p) 