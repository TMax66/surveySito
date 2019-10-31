library(readxl)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(DataExplorer)
library(missMDA)
library(eRm)
library(ltm)
library(difR)
library(janitor)
library(mirt)
library(ShinyItemAnalysis)
library(lme4)

dati <- read_excel("surveysito.xlsx")
dati<-dati[-2, -c(1:5)]


names(dati)<-c("uso","settori","presentazione", "grafica", "descrizione",
               "chiarezza", "user-friendly", "infoinutili", "infoerrate",
               "consigli_errori", "comunicazione", "conoscenza_attività", 
               "intervista", "ricezione_info", "cral", "biblioupdate", "strumenti")
dati<-mutate_all(dati, funs(tolower))

dati<-dati %>% 
   dplyr::select(3,4,5,6,7,8,9,11)

dati$comunicazione<-ifelse(dati$comunicazione=="no , va bene così", "si", "no")


dati$infoinutili<-ifelse(dati$infoinutili=="no", "si", "no")
dati$infoerrate<-ifelse(dati$infoerrate=="no", "si", "no")
dati<- data.frame(lapply(dati, as.factor), stringsAsFactors=FALSE)
dati<-data.frame(lapply(dati, as.numeric))

funz<-function(x){
  
  (x)-1
}

dati[,1:8]<-apply(dati[,1:8], 2, funz)

rscore<-dati %>% 
  mutate(sc=rowSums(.[1:8], na.rm=TRUE)) %>% 
  dplyr::select(sc) %>% 
  ggplot(aes(sc))+geom_bar(fill="steelblue3")+labs(x="Soddisfazione", y="frequenza")+
  scale_x_continuous(breaks = seq(0,8,1))



cscore<-dati %>%
  rownames_to_column() %>% 
  adorn_totals("row")
cscore<-data.frame(t(cscore[217,2:9]))
names(cscore)<-"Qualità"

cscore %>% 
  rownames_to_column(var = "Item") %>% 
  arrange(Qualità) %>% 
  mutate(Item = factor(Item, unique(Item))) %>% 
   ggplot(aes(x=Item, y=Qualità))+
  geom_bar(stat="identity", fill="steelblue3")+labs(x="")+
  coord_flip()+theme(axis.text=element_text(size=12))



##############RASCH LOGISTIC REGRESSION##########################
raschdat1 <- as.data.frame(raschdat1)

raschdat1.long <- raschdat1
raschdat1.long$tot <- rowSums(raschdat1.long) # Create total score




raschdat1.long$ID <- 1:nrow(raschdat1.long) # create person ID
raschdat1.long <- tidyr::gather(raschdat1.long, item, value, I1:I30) # Wide to long
# Make item a factor
raschdat1.long$item <- factor(
  raschdat1.long$item)#, levels = paste("V", 1:30), ordered = TRUE)



res.mlm.l <- glmer(
  value ~ item + (1 | ID), raschdat1.long, family = binomial,
  contrasts = list(item = rbind(rep(-1, 29), diag(29))))


res.mlm.l <- glmer(
  value ~ item + (1 | ID), raschdat1.long, family = binomial,
  contrasts = list(item = rbind(rep(-1, 29), diag(29))),
  start = list(fixef = fixef(res.mlm.l), theta = getME(res.mlm.l, "theta")))



item.diff <- -1 * coef(summary(res.mlm.l))[, 1] # Regression coefficients * -1
item.diff[1] <- -1 * sum(item.diff[2:30])


item.diff <- data.frame(
  item.diff = as.numeric(item.diff), item = paste("V", 1:30))







###item parameters####
res.rasch <- RM(dati)






res <- PCM(dati)
plotINFO(res)
betas <- coef(res.rasch)     # Item difficulty parameters
round(sort(betas), 2)

plotICC(res.rasch, item.subset = 1:8)


plotPImap(res.rasch, cex.gen = .55, sorted = TRUE)

####person parameters####
pers.rasch <- person.parameter(res.rasch)


lrres.rasch <- LRtest(res.rasch, splitcr = "mean")
plotGOF(lrres.rasch, beta.subset = c(14, 5, 18, 7, 1), tlab = "item",
        conf = list(ia = FALSE, col = "blue", lty = "dotted"))




# loading 100-item medical admission test data sets
data(dataMedical)
# binary data set
dataBin <- dataMedical[, 1:100]

# fit Rasch model with mirt package
fit <- mirt(dati, model = 1, itemtype = "Rasch")
# factor scores
theta <- as.vector(fscores(fit))
# difficulty estimates
b <- coef(fit, simplify = T)$items[, "d"]

ggWrightMap(theta, b)


ggWrightMap(theta, b, item.names = names(dati))



















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
