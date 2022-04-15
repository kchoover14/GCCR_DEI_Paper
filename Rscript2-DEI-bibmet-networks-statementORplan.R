library(tidyverse); library(png)
library(ggplot2); library(ggpubr); library(patchwork)
library(bibliometrix)

#links only visible with WoS subscription
#https://www.webofscience.com/wos/woscc/summary/6806fa57-3edb-408d-802b-54458855247b-25226194/relevance/1
#Data
db.statementORplan <- convert2df('data-wos-statement or plan-1mar22.txt', db = "wos", format = "plaintext")

#country collaboration network
db.statementORplan <- metaTagExtraction(db.statementORplan, Field = "AU_CO", sep = ";")
NetMatrix.statementORplan <- biblioNetwork(db.statementORplan, n=20,analysis = "collaboration", network = "countries", sep = ";")
#plot in bad rgb
networkPlot(NetMatrix.statementORplan, n = dim(NetMatrix.statementORplan)[1], Title = "", type = "fruchterman", size=TRUE,
            remove.multiple=FALSE,size.cex = TRUE, cluster="optimal", weighted=TRUE)

#descriptives
netstat.statementORplan <- networkStat(NetMatrix.statementORplan)
summary(netstat.statementORplan,k=10)