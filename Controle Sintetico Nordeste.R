################ Pacotes ####################
library(gsynth)
library(readxl)
library(panelView)
require(Rcpp)
require(ggplot2)
require(foreach)
require(doParallel) 
require(abind)
require(dplyr)
################ Importando os Dados################
local="/home/alexandre/Documentos/Controle Sintetico/Controle Sintetico Brasil/Painel.xlsx"
data=read_xlsx(local)
data=as.data.frame(data,header=TRUE)
data[is.na(data)]=0
################ Controle Sintetico Nordeste ###############
#### Maranhão
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 21)&(data$Ano[i]>=2005)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data21<-data%>%filter(ID %in% c("11","12","13","14","15","21","16","35","41","43","42"))
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data21,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth21<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data21, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth21a.png")
plot(gsynth21)
dev.off()
png("gsynth21b")
plot(gsynth21,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth21c")
plot(gsynth21,type="loadings")
dev.off()
write.csv(gsynth21$wgt.implied,"gsynth21wgt.csv")
write.csv(gsynth21$est.beta,"gsynth21beta.csv")
write.csv(gsynth21$est.att,"gsynth21att.csv")
write.csv(gsynth21$est.avg,"gsynth21avg.csv")

####Piauí
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 22)&(data$Ano[i]>=2006)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data22<-data%>%filter(ID %in% c("11","12","13","14","15","22","16","35","41","43","42"))
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data22,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth22<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data22, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth22a.png")
plot(gsynth22)
dev.off()
png("gsynth22b")
plot(gsynth22,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth22c")
plot(gsynth22,type="loadings")
dev.off()
write.csv(gsynth22$wgt.implied,"gsynth22wgt.csv")
write.csv(gsynth22$est.beta,"gsynth22beta.csv")
write.csv(gsynth22$est.att,"gsynth22att.csv")
write.csv(gsynth22$est.avg,"gsynth22avg.csv")

####Ceará 
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 23)&(data$Ano[i]>=2003)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data23<-data%>%filter(ID %in% c("11","12","13","14","15","23","16","35","41","43","42"))
data$Tratamento
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data23,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth23<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth23a.png")
plot(gsynth23)
dev.off()
png("gsynth23b")
plot(gsynth23,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth23c")
plot(gsynth23,type="loadings")
dev.off()
write.csv(gsynth23$wgt.implied,"gsynth23wgt.csv")
write.csv(gsynth23$est.beta,"gsynth23beta.csv")
write.csv(gsynth23$est.att,"gsynth23att.csv")
write.csv(gsynth23$est.avg,"gsynth23avg.csv")

#### Rio Grande do Norte 
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 24)&(data$Ano[i]>=2004)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data24<-data%>%filter(ID %in% c("11","12","13","14","15","24","16","35","41","43","42"))
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data24,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth24<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data24, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth24a.png")
plot(gsynth24)
dev.off()
png("gsynth24b")
plot(gsynth24,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth24c")
plot(gsynth24,type="loadings")
dev.off()
write.csv(gsynth24$wgt.implied,"gsynth24wgt.csv")
write.csv(gsynth24$est.beta,"gsynth24beta.csv")
write.csv(gsynth24$est.att,"gsynth24att.csv")
write.csv(gsynth24$est.avg,"gsynth24avg.csv")

Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 25)&(data$Ano[i]>=2005)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
data25<-data%>%filter(ID %in% c("11","12","13","14","15","25","16","35","41","43","42"))
data$Tratamento
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data25,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth25<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data25, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth25a.png")
plot(gsynth25)
dev.off()
png("gsynth25b")
plot(gsynth25,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth25c")
plot(gsynth25,type="loadings")
dev.off()
write.csv(gsynth25$wgt.implied,"gsynth23wgt.csv")
write.csv(gsynth25$est.beta,"gsynth25beta.csv")
write.csv(gsynth25$est.att,"gsynth25att.csv")
write.csv(gsynth25$est.avg,"gsynth25avg.csv")

####Pernambuco
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 26)&(data$Ano[i]>=2003)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data26<-data%>%filter(ID %in% c("11","12","13","14","15","26","16","35","41","43","42"))
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data26,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth26<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data26, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth26a.png")
plot(gsynth26)
dev.off()
png("gsynth26b")
plot(gsynth26,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth26c")
plot(gsynth26,type="loadings")
dev.off()
write.csv(gsynth26$wgt.implied,"gsynth26wgt.csv")
write.csv(gsynth26$est.beta,"gsynth26beta.csv")
write.csv(gsynth26$est.att,"gsynth26att.csv")
write.csv(gsynth26$est.avg,"gsynth26avg.csv")

####Alagoas
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 27)&(data$Ano[i]>=2005)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data27<-data%>%filter(ID %in% c("11","12","13","14","15","27","16","35","41","43","42"))
data$Tratamento
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data27,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth27<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data27, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth27a.png")
plot(gsynth27)
dev.off()
png("gsynth27b")
plot(gsynth27,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth27c")
plot(gsynth27,type="loadings")
dev.off()
write.csv(gsynth27$wgt.implied,"gsynth27wgt.csv")
write.csv(gsynth27$est.beta,"gsynth27beta.csv")
write.csv(gsynth27$est.att,"gsynth27att.csv")
write.csv(gsynth27$est.avg,"gsynth27avg.csv")

####Sergipe
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 28)&(data$Ano[i]>=2003)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data28<-data%>%filter(ID %in% c("11","12","13","14","15","28","16","35","41","43","42"))
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data28,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth28<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data28, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth28a.png")
plot(gsynth28)
dev.off()
png("gsynth28b")
plot(gsynth28,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth28c")
plot(gsynth28,type="loadings")
dev.off()
write.csv(gsynth28$wgt.implied,"gsynth28wgt.csv")
write.csv(gsynth28$est.beta,"gsynth28beta.csv")
write.csv(gsynth28$est.att,"gsynth28att.csv")
write.csv(gsynth28$est.avg,"gsynth28avg.csv")

####Bahia
Tratamento=c()
for (i in 1:length(data$ID)){
if ((data$ID[i]== 29)&(data$Ano[i]>=2004)){
Tratamento=append(Tratamento,1,after=length(Tratamento))
}
else{
Tratamento=append(Tratamento,0,after=length(Tratamento))
}
}
data$Tratamento<-Tratamento
class(Tratamento)
data29<-data%>%filter(ID %in% c("11","12","13","14","15","29","16","35","41","43","42"))
data$Tratamento
panelView(Pobreza~Tratamento+Desigualdade+Pobreza,data=data29,index=c("ID","Ano"))
####Estimando os Resultados 
gsynth29<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data29, index=c('ID',"Ano"),force="two-way",EM=F,CV=TRUE,se=TRUE, nboots=3000, inference='parametric',parallel=TRUE)
png("gsynth29a.png")
plot(gsynth29)
dev.off()
png("gsynth29b")
plot(gsynth29,type="counterfactual",raw="band",xlab="Time",ylim=c(-10,100))
dev.off()
png("gsynth29c")
plot(gsynth29,type="loadings")
dev.off()
write.csv(gsynth29$wgt.implied,"gsynth29wgt.csv")
write.csv(gsynth29$est.beta,"gsynth29beta.csv")
write.csv(gsynth29$est.att,"gsynth29att.csv")
write.csv(gsynth29$est.avg,"gsynth29avg.csv")
