require(data.table)
library(ggplot2)
library(dplyr)
require(skimr)
library(fpp)
require(ggcorrplot)

data_path <- "C:/Users/Asus/Desktop/2022 - Spring/IE360/HW2/IE360_Spring22_HW2_data.csv"

data <- fread(data_path)
colnames(data) <- c("Quarters", "UGS", "RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNPT")

summary(data)
str(data)

fmt <- "%Y_Q%q"
data$Quarters <- as.yearqtr(data$Quarters, format = fmt)


ggplot(data,aes(x=Quarters,y=UGS)) + geom_line()

ggplot(data[Quarters<"2002 Q2"],aes(x=Quarters,y=UGS)) + geom_line()

# showing decreasing trend yearly
data$year = as.numeric(format(data$Quarters, "%Y"))
yearly_UGS <- data[,list(yearly_sale = mean(UGS)),by= list(year)]
ggplot(yearly_UGS,aes(x=year,y=yearly_sale)) + geom_line()


# trend component added
data[,trend := 1: .N ]


# linear regression model with only trend
yearly_UGS[,trend := 1: .N ]
Model1 <- lm(UGS ~ trend, data)
summary(Model1)

data[,trend_const := predict(Model1,data)]

ggplot(data,aes(x=Quarters)) + geom_line(aes(y=UGS, color="real")) + geom_line(aes(y=trend_const, color="trend_const"))

# decreasing trend and quarterly seasonality
acf(data$UGS[1:28])

data$QuarterFactor = as.factor(format(data$Quarters, "%q"))
Model2 <- lm(UGS ~ trend + QuarterFactor, data)
summary(Model2)
data[,trend_const_QuarterlySeasonality := predict(Model2,data)]
ggplot(data,aes(x=Quarters)) + geom_line(aes(y=UGS, color="real")) + geom_line(aes(y=trend_const_QuarterlySeasonality, color="trend_const"))



# residuals
ggplot(data,aes(x=Quarters)) + geom_line(aes(y=UGS - trend_const_QuarterlySeasonality, color="residual")) 
checkresiduals(Model2$residuals)
acfof_res <- acf(Model2$residuals)
acfof_res$acf < 0.2


# correlation variables
res_table <- data.table()
res_table <- data[,3:11]
res_table[,residuals := c(Model2$residuals, rep(NA,4))]


correl_info=cor(res_table[1:28])

ggcorrplot(correl_info, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


#res_table[,diff := c(rep(NA,2), res_table$residuals[1:28],rep(NA,2))]
#res_table[,diff :=res_table$GNPA - res_table$diff]
#res_table[,diff :=NULL]
ggplot(res_table,aes(x=RNUV, y=residuals)) + geom_smooth() 
ggplot(res_table,aes(x=GNPA, y=residuals)) + geom_smooth() 
ggplot(res_table,aes(x=diff, y=residuals )) + geom_smooth() 

# Model3 with RNUV

Model3 <- lm(UGS ~ trend + QuarterFactor + RNUV, data)
summary(Model3)
data[,pred_model3 := predict(Model3,data)]
ggplot(data,aes(x=Quarters)) + geom_line(aes(y=UGS, color="real")) + geom_line(aes(y=pred_model3, color="model3"))
checkresiduals(Model3$residuals)
data$pred_model3[29:32]


# further improvements on model3
res_table[,residuals := c(Model3$residuals, rep(NA,4))]
res_table[,RNUV := NULL]

correl_info2=cor(res_table[1:28])

ggcorrplot(correl_info2, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

ggplot(res_table,aes(x=PU, y=residuals)) + geom_smooth()
ggplot(res_table,aes(x=PG, y=residuals)) + geom_smooth()


Model4 <- lm(UGS ~ trend + QuarterFactor + RNUV + PG, data)
summary(Model4)
checkresiduals(Model4$residuals)
data[,pred_model4 := predict(Model4,data)]
ggplot(data,aes(x=Quarters)) + geom_line(aes(y=UGS, color="real")) + geom_line(aes(y=pred_model4, color="model4"))
data$pred_model4[29:32]

