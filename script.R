#package needed.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("parameters","apa", "dplyr", "skimr","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN", "semTools", "readr", "readxl")
ipak(packages)
#data
library(readxl)
df <- read_excel("valor_percibido_400.xlsx")
df <- as.data.frame(df)
skim(df)
#kMO
KMO(df)
#n factors ASI
results_nfactor<-n_factors(df, rotate = "oblimin", fm = "mle", n = NULL)
plot(results_nfactor)
results_nfactorASI
as.data.frame(results_nfactorASI)
summary(results_nfactorASI)
#Exploratory Factorial ANalysis ASI
factor<-fa(df, nfactors = 4,fm = "ml",rotate ="oblimin",cor = "poly")
print(factor,digits = 2,cut = F,sort=TRUE)
#eliminar 12, 13, 14
names(df)
df2<- select(df, -"x12", -"x13", -"x14" )
#kMO
KMO(df2)
bartlett.test(df2)
#cronbach and item-total
psych::alpha(df2)
#Exploratory Factorial ANalysis ASI
factor<-fa(df2, nfactors = 4,fm = "ml",rotate ="oblimin",cor = "poly")
print(factor,digits = 2,cut = F,sort=TRUE)
#p-value
factor$PVAL
factor$score.cor
