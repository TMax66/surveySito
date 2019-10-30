library(readxl)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(DataExplorer)
library(missMDA)
library("eRm")
library("ltm")
library("difR")

dati <- read_excel("surveysito.xlsx")
dati<-dati[, -c(1:5)]


names(dati)<-c("uso","settori","presentazione", "grafica", "descrizione",
               "chiarezza", "user-friendly", "infoinutili", "wronginfo",
               "consigli_errori", "comunicazione", "conoscenza_attività", 
               "intervista", "ricezione_info", "cral", "biblioupdate", "strumenti")
dati<-mutate_all(dati, funs(tolower))

dati<-dati %>% 
   dplyr::select(3,4,5,6,7,8,9,11)

dati$comunicazione<-ifelse(dati$comunicazione=="no , va bene così", "si", "no")

dati<- data.frame(lapply(dati, as.factor), stringsAsFactors=FALSE)
dati<-data.frame(lapply(dati, as.numeric))

funz<-function(x){
  
  (x)-1
}

dati[,1:8]<-apply(dati[,1:8], 2, funz)

score<-dati %>% 
  mutate(sc=rowSums(.[1:8]))






##############RASCH LOGISTIC REGRESSION##########################





#########MCA###############
####inputo i missing data####
nb = estim_ncpMCA(dati,ncp.max=5)
tab.disj = imputeMCA(dati, ncp=4)$tab.disj
res.mca = MCA(dati,tab.disj=tab.disj)



res.mca <- MCA(dati, graph = FALSE)
print(res.mca)

eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())


fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())


fviz_mca_biplot(res.mca, repel = TRUE,
                ggtheme = theme_minimal())

fviz_cos2(res.mca, choice = "var", axes = 1:2)

fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)


fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)


fviz_ellipses(res.mca, c("chiarezza", "descrizione", "grafica"),
              geom = "point")


fviz_ellipses(res.mca, 1:11, geom = "point")
