library(xlsx)
library(ggplot2)
library(zoo)
library(dplyr)
library(GGally)
library(corrplot)

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
names(data_agg)[2:6] <- c("ICTIC_index_month", "IST_index_month", "IPCA_index_month", "IGPM_index_month", "INPC_index_month") 
data_agg <- ts(data = data_agg, start = c(2013, 01), end = c(2019, 02), frequency = 12)

#decomposição das séries 
autoplot(decompose(data_agg[,2]))
autoplot(decompose(data_agg[,3]))
autoplot(decompose(data_agg[,4]))
autoplot(decompose(data_agg[,5]))
autoplot(decompose(data_agg[,6]))

#visualização dos dados - series temporais

ggplot() + geom_line(data = data_agg, aes(x = date, y = ICTIC_index_month), color = "#585858", size = 1) +
  geom_line(data = data_agg, aes(x = date, y = IST_index_month), color = "#2E64FE", size = 1) + 
  geom_line(data = data_agg, aes(x = date, y = IPCA_index_month), color = "#FE2E2E", size = 1) +
  geom_line(data = data_agg, aes(x = date, y = INPC_index_month), color = "#F7FE2E", size = 1) +
  geom_line(data = data_agg, aes(x = date, y = IGPM_index_month), color = "#04B45F", size = 1) 

ggplot() + 
  geom_area(data = data_agg, aes(x = date, y = ICTIC_index_month, color = "#585858", fill = "#585858"), alpha = 0.5, position = position_dodge(0.8)) +
  geom_area(data = data_agg, aes(x = date, y = IST_index_month, color = "#2E64FE", fill = "#2E64FE"), alpha = 0.5, position = position_dodge(0.8)) +
  geom_area(data = data_agg, aes(x = date, y = IPCA_index_month, color = "#FE2E2E", fill = "#FE2E2E"), alpha = 0.5, position = position_dodge(0.8)) + 
  geom_area(data = data_agg, aes(x = date, y = INPC_index_month, color = "#F7FE2E", fill = "#F7FE2E"), alpha = 0.5, position = position_dodge(0.8)) + 
  geom_area(data = data_agg, aes(x = date, y = IGPM_index_month, color = "#2EFE2E", fill = "#2EFE2E"), alpha = 0.5, position = position_dodge(0.8))
  

#visualização dos dados - correlacoes 

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

pairs(na.omit(data_agg[,(2:6)]), diag.panel = panel.hist, upper.panel = panel.cor)
ggcorr(data_agg[,(2:6)], label = T)
cor = cor(na.omit(data_agg[,(2:6)]))
corrplot(cor, method = "circle")


