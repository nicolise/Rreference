#Nicole E Soltis
#2014 07 10
#script to add citations to an MS using R
#original author: Carl Boettiger
#from: http://www.carlboettiger.info/2012/03/24/citations-in-markdown-using-knitr.html
#---------------------------------------------------------

#install.packages("devtools")
library(devtools)
install_github("knitcitations", "cboettig")

#load the package
require(knitcitations)

#find a bibtex file
library(bibtex)
write.bib(c('bibtex', 'knitr', 'knitcitations'), 
          file="example.bib")

#read in bibtex file
biblio <- read.bib("example.bib")
biblio[[1]]

#generate parenthetical citation from DOI:
citep("10.1111/j.1461-0248.2005.00827.x")

#cite by URL:
citep("http://knowledgeblog.org/greycite")

#generate a parenthetical citation from bibliography:
citet(biblio[1])
citep[biblio[2:3] #cite a couple of citations together
      
#print final biblio
bibliography()
