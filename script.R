#package needed.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("car","readxl","parameters","apa", "dplyr", "skimr","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN", "semTools", "readr", "readxl", "psycho")
ipak(packages)

Basedatos <- read_excel("BD.xlsx")
names(Basedatos)
BD <- Basedatos[1:300,]
BD <- dplyr::select(BD, VFC1:VC6)

#kMO
KMO(BD)
bartlett.test(BD)
#n factors pakage more info: https://github.com/easystats/parameters/blob/main/R/n_factors.R
results_nfactor<-n_factors(BD, type = "FA", algorithm = "mle", package = c("nFactors", "psych"))
plot(results_nfactor)
results_nfactor
as.data.frame(results_nfactor)
#Exploratory Factorial Analysis ASI
factor<-fa(BD, nfactors = 4,fm = "ml",rotate ="oblimin",cor = "cor")
print(factor,digits = 2,cut = .30,sort=TRUE)

#Eliminar Problematicos
names(df)
df<- dplyr::select(BD, -"VE1", -"VE2", -"VE3", -"VE7", -"VE8", -"VE9")
#kMO
KMO(df)
bartlett.test(df)
#cronbach and item-total
psych::alpha(df)
#Exploratory Factorial ANalysis ASI
results_nfactor2<-n_factors(df, type = "FA", algorithm = "mle", package = c("nFactors", "psych"))
plot(results_nfactor2)
factor<-fa(df, nfactors = 4,fm = "ml",rotate ="oblimin",cor = "cor")
print(factor,digits = 2,cut = .30,sort=TRUE)
#p-value
factor$PVAL
factor$score.cor