install.packages("extrafont")

library(extrafont)
font_import()

opar<-par()
par(mfrow=c(2,1), mar=c(2,4,2,2))
#plot figure here
par(opar)



#lines to add
#before data table
EHS.N.S$TissueED <- gsub("R", "TR", EHS.N.S$Tissue)

#before plot
pdf("EHS15N.pdf", family="Arial", width=6, height=4)

#in plot
names.arg=c("NN","NB","ON","OB","S","R"),
axis.lty = 1,
args.legend = list(x = "topleft", bty = "n"))
)

#after plot
mtext("(b)", side=3, adj=0, padj=-0.2)
dev.off()