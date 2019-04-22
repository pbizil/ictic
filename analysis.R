library(xlsx)
library(ggplot2)
library(zoo)
library(dplyr)
library(GGally)
library(corrplot)
library(forecast)

#carga dos dados 
data_IGPM <- read.xlsx("TimeSeries_IGPM.xlsx", sheetIndex = 1)
data_IGPM$IGPM_index_month <- as.numeric(sub(",", ".", data_IGPM$index_month))
data_IGPM <- data_IGPM[order(data_IGPM$date),]
data_INPC <- read.xlsx("TimeSerie_INPC.xlsx", sheetIndex = 1)
data_IPCA <- read.xlsx("TimeSeries_IPCA.xlsx", sheetIndex = 1)
data_IST <- read.xlsx("TimeSeries_IST.xlsx", sheetIndex = 1)
data_ICTIC <- read.xlsx("TimeSeries_ICTIC.xlsx", sheetIndex = 1)

#join das bases e dados agregados
data_agg <- data_ICTIC[, c(1, 3)]
data_agg <- merge(data_agg, data_IST[data_IST$date>"2013-01-01", c(1, 3)], by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
data_agg <- merge(data_agg, data_IPCA[data_IPCA$date>"2013-01-01", c(1, 2)], by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
data_agg <- merge(data_agg, data_IGPM[data_IGPM$date>"2013-01-01", c(1, 6)], by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
data_agg <- merge(data_agg, data_INPC[data_INPC$date>"2013-01-01", c(1, 3)], by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
colnames(data_agg)[1:6] <- c("date","ICTIC", "IST", "IPCA", "IGPM", "INPC") 
data_agg <- ts(data = data_agg, start = c(2013, 01), end = c(2019, 02), frequency = 12)


#plot das séries 

ggplot() + geom_line(data = data_agg, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = data_agg, aes(x = date, y = IST, color = "IST"), size = 1) + 
  geom_line(data = data_agg, aes(x = date, y = IPCA, color = "IPCA"), size = 1) +
  geom_line(data = data_agg, aes(x = date, y = INPC, color = "INPC"), size = 1) +
  geom_line(data = data_agg, aes(x = date, y = IGPM, color = "IGPM"), size = 1) 

ggplot() + geom_line(data = data_agg, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = data_agg, aes(x = date, y = IST, color = "IST"), size = 1) +  
  labs(x="Meses", y="Valor dos Índices", title="ICTI e IST", color="Índices")

ggplot() + geom_line(data = data_agg, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = data_agg, aes(x = date, y = IPCA, color = "IPCA"), size = 1)  +  
  labs(x="Meses", y="Valor dos Índices", title="ICTI e IPCA", color="Índices")

ggplot() + geom_line(data = data_agg, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = data_agg, aes(x = date, y = IGPM, color = "IGPM"), size = 1) +
  labs(x="Meses", y="Valor dos Índices", title="ICTI e IGPM", color="Índices")

ggplot() + geom_line(data = data_agg, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = data_agg, aes(x = date, y = INPC, color = "INPC"), size = 1)  +
  labs(x="Meses", y="Valor dos Índices", title="ICTI e INPC", color="Índices")


#DECOMPOSIÇÃO CLÁSSICA DAS SÉRIES

#comparando componentes - tendência e ciclo 
trend <- data.frame(as.Date(data_agg[,1]), decompose(data_agg[,2])$trend, decompose(data_agg[,3])$trend, decompose(data_agg[,4])$trend, 
                    decompose(data_agg[,5])$trend, decompose(data_agg[,6])$trend)
colnames(trend)[1:6] <- c("date","ICTIC", "IST", "IPCA", "IGPM", "INPC") 

ggplot() + geom_line(data = trend, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = trend, aes(x = date, y = IST, color = "IST"), size = 1) +  
  labs(x="Meses", y="Valor dos Índices", title="Tendência e Ciclo - ICTI e IST", color="Índices")

ggplot() + geom_line(data = trend, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = trend, aes(x = date, y = IPCA, color = "IPCA"), size = 1)  +  
  labs(x="Meses", y="Valor dos Índices", title="Tendência e Ciclo - ICTI e IPCA", color="Índices")

ggplot() + geom_line(data = trend, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = trend, aes(x = date, y = IGPM), color = "IGPM", size = 1) +
  labs(x="Meses", y="Valor dos Índices", title="Tendência e Ciclo - ICTI e IGPM", color="Índices")

ggplot() + geom_line(data = trend, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = trend, aes(x = date, y = INPC), color = "INPC", size = 1)  +
  labs(x="Meses", y="Valor dos Índices", title="Tendência e Ciclo - ICTI e INPC", color="Índices")


#comparando componentes - sazonalidade 
seasonal <- data.frame(as.Date(data_agg[,1]), decompose(data_agg[,2])$seasonal, decompose(data_agg[,3])$seasonal, decompose(data_agg[,4])$seasonal, 
                       decompose(data_agg[,5])$seasonal, decompose(data_agg[,6])$seasona)
colnames(seasonal)[1:6] <- c("date","ICTIC", "IST", "IPCA", "IGPM", "INPC") 

ggplot() + geom_line(data = seasonal, aes(x = date, y = ICTIC, colour = "ICTI"), size = 1) +
  geom_line(data = seasonal, aes(x = date, y = IST, colour = "IST"), size = 1) +  
  labs(x="Meses", y="Valor dos Índices", title="Sazonalidade - ICTI e IST", colour="Índices")

ggplot() + geom_line(data = seasonal, aes(x = date, y = ICTIC, colour = "ICTI"), size = 1) +
  geom_line(data = seasonal, aes(x = date, y = IPCA, colour = "IPCA"), size = 1)  +  
  labs(x="Meses", y="Valor dos Índices", title="Sazonalidade - ICTI e IPCA", colour="Índices")

ggplot() + geom_line(data = seasonal, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = seasonal, aes(x = date, y = IGPM, color = "IGPM"), size = 1) +
  labs(x="Meses", y="Valor dos Índices", title="Sazonalidade - ICTI e IGPM", colour="Índices")

ggplot() + geom_line(data = seasonal, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = seasonal, aes(x = date, y = INPC, color = "INPC"), size = 1)  +
  labs(x="Meses", y="Valor dos Índices", title="Sazonalidade - ICTI e INPC", colour="Índices") 

#comparando componentes - erro
random <- data.frame(as.Date(data_agg[,1]), decompose(data_agg[,2])$random, decompose(data_agg[,3])$random, decompose(data_agg[,4])$random, 
                     decompose(data_agg[,5])$random, decompose(data_agg[,6])$random)
colnames(random)[1:6] <- c("date","ICTIC", "IST", "IPCA", "IGPM", "INPC") 

ggplot() + geom_line(data = random, aes(x = date, y = ICTIC, colour = "ICTI"), size = 1) +
  geom_line(data = random, aes(x = date, y = IST, colour = "IST"), size = 1) +  
  labs(x="Meses", y="Valor dos Índices", title="Erro - ICTI e IST", colour="Índices")

ggplot() + geom_line(data = random, aes(x = date, y = ICTIC, colour = "ICTI"), size = 1) +
  geom_line(data = random, aes(x = date, y = IPCA, colour = "IPCA"), size = 1)  +  
  labs(x="Meses", y="Valor dos Índices", title="Erro - ICTI e IPCA", colour="Índices")

ggplot() + geom_line(data = random, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = random, aes(x = date, y = IGPM, color = "IGPM"), size = 1) +
  labs(x="Meses", y="Valor dos Índices", title="Erro - ICTI e IGPM", colour="Índices")

ggplot() + geom_line(data = random, aes(x = date, y = ICTIC, color = "ICTI"), size = 1) +
  geom_line(data = random, aes(x = date, y = INPC, color = "INPC"), size = 1)  +
  labs(x="Meses", y="Valor dos Índices", title="Erro - ICTI e INPC", colour="Índices") 


#ANÁLISE MULTIVARIADA

#regressao linear

ts_data_agg <- ts(data = na.omit(data_agg), start = c(2013, 01), end = c(2019, 02), frequency = 12)
reg_indices <- tslm(ICTIC ~ IST + IPCA + IGPM + INPC, data=ts_data_agg)
summary(reg_indices)

#outras correlacoes

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(na.omit(ts_data_agg[,(2:6)]), diag.panel = panel.hist, upper.panel = panel.cor)
ggcorr(ts_data_agg[,(2:6)], label = T)
cor = cor(na.omit(ts_data_agg[,(2:6)]))
corrplot(cor, method = "circle")

#Correlação cruzada 

ccf_IST <- ccf(na.omit(ts_data_agg[,2]), na.omit(ts_data_agg[,3]))
ccf_IPCA <- ccf(na.omit(ts_data_agg[,2]), na.omit(ts_data_agg[,4]))
ccf_IGPM <- ccf(na.omit(ts_data_agg[,2]), na.omit(ts_data_agg[,5]))
ccf_INPC <- ccf(na.omit(ts_data_agg[,2]), na.omit(ts_data_agg[,6]))

plot(ccf_IST)
plot(ccf_IPCA)
plot(ccf_IGPM)
plot(ccf_INPC)

#Matriz de covariancia 

plot(cov(data_agg[,c(2:6)]))

#Estudo dos resíduos 




