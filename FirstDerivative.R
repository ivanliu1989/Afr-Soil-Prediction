library(data.table)

train <- fread("./training.csv")

#Remove CO2
train <- train[,':='(2656:2670, NULL)]

#Get first derivative for the MIR columns:
train_deriv <- copy(train)
train_deriv[,2:3564 := (train[,2:3564,with= FALSE] - cbind(NA,train[,2:3563, with = FALSE]))]


#Melt it and plot a few PIDNs
library(reshape2)
library(ggplot2)
library(stringr)

#Raw first
train_long <- melt(train,id.vars = "PIDN", measure.vars = 2:3564) #Note... this is much bigger than you need for the graph
train_long$variable <- as.numeric(str_replace_all(train_long$variable,"m",""))
ids <- c("XNhoFZW5", "9XNspFTd", "WDId41qG", "JrrJf1mN", "ZoIitegA", "QkhF9azr", "kFpRoFpt", "QH0y7Gc0", "yTfzYmjL")
ggplot(train_long[PIDN %in% ids], aes(x = variable, y = value, colour = PIDN)) + geom_line()


#First derivative
train_long <- melt(train_deriv,id.vars = "PIDN", measure.vars = 2:3564)
train_long$variable <- as.numeric(str_replace_all(train_long$variable,"m",""))
ids <- c("XNhoFZW5", "9XNspFTd", "WDId41qG", "JrrJf1mN", "ZoIitegA", "QkhF9azr", "kFpRoFpt", "QH0y7Gc0", "yTfzYmjL")
ggplot(train_long[PIDN %in% ids], aes(x = variable, y = value, colour = PIDN)) + geom_line()