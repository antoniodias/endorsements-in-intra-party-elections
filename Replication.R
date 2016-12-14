##
# Replication code for Cancela, Dias and Lisi (2016),
# "The impact of endorsements in intra-party elections: 
# evidence from open primaries in a new Portuguese party",
# published in Politics.
# Please set your working directory properly and make sure you download 
# the following files into that directory: 
# "support_edges.csv", "AttributeTable.csv"
# Important: make sure you use UTF-8 encoding and that you install the required packages
#
# The article is available at http://doi.org/10.1177/0263395716680125 
# Any queries regarding the code should be directed to: antoniolvdias@gmail.com
##


### Libraries -----
library(tm)
library(igraph)
library(statnet)
library(stargazer)
library(sandwich)
library(gvlma)
library(lmtest)
library(car)
library(ggplot2)
library(gridExtra)
library(grid)

### Set WD and load datasets ----- 
#Don't Forget to set the WD before running the code
setwd()

Attributes <- read.csv("AttributeTable.csv", encoding = "UTF-8")
support_edges <- read.csv("support_edges.csv", encoding = "UTF-8")

###Igraph and Centrality Measures -----
#create igrpah
livre<- graph_from_data_frame(support_edges)
#extract centrality scores
centrality<- data.frame(id = as.numeric(V(livre)$name),
                        BetweennessCentrality=centr_betw(livre)$res,
                        InDegree=centr_degree(livre, mode="in")$res)

###merge to create final dataset and Subset to Lisbon ----
dataset<-merge(Attributes, centrality, by="id")
rm(list=setdiff(ls(), "dataset"))
#subset just lisbon

lisbon <- dataset [ which(dataset$Lisboa==1), ]
lisbon$log_centrality<-log(lisbon$BetweennessCentrality + 1)
lisbon$SqrtCentrality<-sqrt(lisbon$BetweennessCentrality)
lisbon$SqrtResult<-sqrt(lisbon$Result)
lisbon$FactorOrder <- 0
lisbon$FactorOrder[lisbon$Order==1]<-1
#change names of variables
names(lisbon)
names(lisbon)[1]<-"Id"
names(lisbon)[6]<-"Comissao_Coordenadora"
names(lisbon)[7]<-"Grupo_contacto"
names(lisbon)[12]<-"Previous.party"


### Results -----
#summary of dataset
stargazer(lisbon,out="summary.html")

#after summary normalize the data set
normalize <- function (x) {
    x <- (x- mean(x, na.rm=T))/sd(x, na.rm=T)
}
names(lisbon)
lisbon[,c(8:9,14:17)] <-lapply(lisbon[,c(8:9,14:17)], normalize)

#modeling
m1 <- lm(Log ~ as.factor(FactorOrder) + as.factor(Comissao_Coordenadora) + as.factor(Grupo_contacto)  + as.factor(Previous.party), data= lisbon) 
cov1 <-coeftest(m1, vcov= vcovHC(m1))
robust.se1 <- cov1[,2]
robust.pv1 <- cov1[,4]
names(lisbon)
m2 <- lm(Log ~ InDegree+ SqrtCentrality + Apoio_Coordenadora + Apoio_Contacto, data=lisbon)
cov2 <-coeftest(m2, vcov= vcovHC(m2))
robust.se2 <- cov2[,2]
robust.pv2 <- cov2[,4]
m3 <- lm(Log ~   InDegree+ SqrtCentrality+Apoio_Coordenadora + Apoio_Contacto +
             as.factor(FactorOrder) + as.factor(Comissao_Coordenadora) + as.factor(Grupo_contacto)+
             as.factor(Previous.party), data =lisbon)
cov3 <-coeftest(m3, vcov= vcovHC(m3))
robust.se3 <- cov3[,2]
robust.pv3 <- cov3[,4]

stargazer(m1,m2,m3, out = "regression.html",
          se=list(robust.se1,robust.se2,robust.se3),
          p= list(robust.pv1,robust.pv2,robust.pv3))

### Plots ----
#Plots 

#Figure A.1 - Histogram

plot1 <-ggplot(lisbon,aes(x=Result))+geom_histogram()+theme_minimal()
plot2 <- ggplot(lisbon,aes(x=Log))+geom_histogram()+xlab("Natural Log of Results")+theme_minimal()
plot1
plot2

plot1_2<-grid.arrange(plot1,plot2,ncol=2,nrow=1)
ggsave("results_histograms.pdf",plot = plot1_2, width = 8, height = 3)

#Figure A.2
pdf("diagnostics_plot_m3.pdf")
par(mfrow=c(2,2))
par(mar = rep(2, 2))
plot(m3)
dev.off()

#Figure A.3 - betweenness centrality distribution

m4 <- lm(Log ~ BetweennessCentrality, data =lisbon)
m5 <- lm(Log ~ log_centrality, data =lisbon)
m6 <- lm(Log ~ SqrtCentrality, data = lisbon)

shapiro.test(residuals(m4))
shapiro.test(residuals(m5))
shapiro.test(residuals(m6))

plot5<- crPlot(m4, variable="BetweennessCentrality", xlab="Betweenness Centrality")
plot6<- crPlot(m5, variable="log_centrality", xlab= "Natural Log of Betweenness Centrality")
plot7<- crPlot(m6, variable="SqrtCentrality", xlab="Square Root of Betweenness Centrality")

pdf(file='multiple_plot_betweenness.pdf', width = 8, height = 3)
par(mfrow=(c(1,3)))
crPlot(m4, variable="BetweennessCentrality", xlab="A - Betweenness Centrality")
crPlot(m5, variable="log_centrality", xlab= "B - Natural Log of Betweenness Centrality")
crPlot(m6, variable="SqrtCentrality", xlab="C - Square Root of Betweenness Centrality")
dev.off()
