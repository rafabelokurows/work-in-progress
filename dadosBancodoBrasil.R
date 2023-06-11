# install.packages("rbcb")
# install.packages("forecastHybrid")
library(rbcb)
library(ggplot2)
library(forecastHybrid)
IGPM<- rbcb::get_series(c(IGPM="189 "))
IGPMts<-ts(IGPM$IGPM,start = c(2000,1),frequency = 12)

Vend = rbcb::get_series(c(Vendas="1455 "))
Vendts<-ts(Vend$Vendas,start = c(2000,1),frequency = 12)

Energia = rbcb::get_series(c(Enel="1406"))
Enegts<-ts(Energia$Enel,start = c(2000,1),frequency = 12)
IGPM
Vend
Energia

TxSelic = rbcb::get_series(c(selic="432"))
TxSelicts <- ts(TxSelic$selic, start = c(2000, 1), frequency = 12)

QM = rbcb::get_series(c(meta="27783"))
QMts <- ts(QM$meta, start = c(2000, 1), frequency = 12)

Juros = rbcb::get_series(c(mora="12501"))
jts <- ts(Juros$mora, start = c(2000, 1), frequency = 12)

TxSelic
QM
Juros
#Economia Internacional;

PIBUSA = rbcb::get_series(c(pibeua="3711"))
PIBUSAts<-ts(PIBUSA$pibeua,start = c(2000,1),frequency = 12)

PIBCHI =rbcb::get_series(c(pibchina="25079"))
PIBCHIts<-ts(PIBCHI$pibchina,start = c(2000,1),frequency = 12)

PIBGER = rbcb::get_series(c(pibale="25079"))
PIBGERts<-ts(PIBGER$pibale,start = c(2000,1),frequency = 12)
PIBGER
PIBUSA
PIBCHI
#ATIVIDADE ECONÔMICA
#IGPM
ModIGPM <- hybridModel(IGPMts, weights = c("equal"))
prevIGPM<-forecast(ModIGPM,h=60)

#Vendas
ModVend<-hybridModel(Vendts, weights = c("equal"))
prevend<-forecast(ModVend,h=60)

#Energia
ModEnel<- hybridModel(Enegts, weights = c("equal"))
prevenel<-forecast(ModEnel,h=60)

#PM
#Selic
ModTxSel<-hybridModel(TxSelicts, weights = c("equal"))
prevsel<- forecast(ModTxSel,h=60)

#Quantidade de Moeda na Economia
ModQM<-hybridModel(QMts, weights = c("equal"))
prevQM<- forecast(ModQM,h=60)

#Juros 
ModJ<- hybridModel(jts, weights = c("equal"))
prevJ<- forecast(ModJ,h=60)

#Economia Internacional
#PIB Estados Unidos

ModPIBUSA<- hybridModel(PIBUSAts, weights = c("equal"))
prevPIBUSA<- forecast(ModPIBUSA,h=60)

#PIB China
ModPIBCHI<- hybridModel(PIBUSAts, weights = c("equal"))
prevPIBCHI<- forecast(ModPIBCHI,h=60)

#PIB Alemanha

ModPIBAle<- hybridModel(PIBGERts, weights = c("equal"))
prevPIBAle<- forecast(ModPIBAle,h=60)

print(prevIGPM)

print(prevend)

print(prevenel)

print(prevsel)


autoplot(IGPMts) + autolayer(prevIGPM, series = "Previsão") + xlab("Ano") + 
  ylab("Índice (2000=100)") + ggtitle("Previsão para o IGPM")

autoplot(Vendts) + autolayer(prevend, series = "Previsão") + xlab("Ano") + 
  ylab("Índice (2000=100)") + ggtitle("Previsão para a Vendas")

autoplot(Enegts) + autolayer(prevenel, series = "Previsão") + xlab("Ano") + 
  ylab("Índice (2000=100)") + ggtitle("Previsão para a Quantidade de Energia gasta")

##Política Monetária:

autoplot(TxSelicts) + autolayer(prevsel, series = "Previsão") + xlab("Ano") + 
  ylab("Índice (2000=100)") + ggtitle("Previsão para a Taxa Selic")

