# Libraries
library(survival, quietly = TRUE)
library(caret, quietly = TRUE)
library(glmnet, quietly = TRUE)
library(rms, quietly = TRUE)
library(risksetROC, quietly = TRUE)
library(doParallel, quietly = TRUE)
library(randomForestSRC)
library(ggRandomForests)
library(RMySQL)
library(VIM)
library(dplyr)
library(purrr)
library(DMwR)
library(tidyr)
library(pec)

# config
options(scipen=999)

# Paralelizo
registerDoParallel(detectCores() - 2 )  ## registerDoMC( detectCores()-1 ) in Linux
detectCores()
options(rf.cores = detectCores() - 2, 
        mc.cores = detectCores() - 2)  ## Cores for parallel processing

#####################################################################################################################################
                                                            # DATA IMPORT 
# Importo data
mydb <- dbConnect(MySQL(), user='XX', password='XX', dbname='XX', host='XX')
rs <- dbSendQuery(mydb, "select * from Z_TRAIN")
df <- fetch(rs, n=-1)

#####################################################################################################################################
                                                          # DATA PREPROCESS 
# Donation_id como index
rownames(df) <- df$DONATION_ID

# Drop de IDs
df$DONOR_ID <- NULL
df$CAMPAIGN_ID <- NULL
df$DONATION_ID <- NULL

# Drop variables que no van
df$DONACION_ONETIME <- NULL
df$DONACION_MENSUAL <- NULL
df$DONACION_FIN_PROGRAMADO <- NULL
df$DONACION_INICIO <- NULL
df$DONACION_FIN<- NULL
df$DONACION_FIN_ANIO_MES<- NULL
df$DONACION_INICIO_FECHA<- NULL
df$DONACION_FIN_FECHA<- NULL
df$CAMPANIA_INICIO<- NULL
df$CAMPANIA_FIN<- NULL
df$CAMPANIA_FIN_ANIO_MES<- NULL
df$ONG_INICIO_EN_DO<- NULL
df$ONG_FIN_EN_DO<- NULL
df$ONG_FIN_EN_DO_ANIO_MES<- NULL
df$DONOR_FECHA_ACTUALIZACION<- NULL
df$DONACION_IP<- NULL

# Transformacion de datatype
#df$DONACION_IP <- as.factor(df$DONACION_IP)

# Imputacion de missings
df$DONACION_DURACION_PROGRAMADA [is.na(df$DONACION_DURACION_PROGRAMADA )] <- 9999 
df$DONOR_VERSION_CANTIDAD_ACTUALIZACIONES [is.na(df$DONOR_VERSION_CANTIDAD_ACTUALIZACIONES )] <- 0 
df$DONOR_VERSION_ULTIMA_ACTUALIZACION [is.na(df$DONOR_VERSION_ULTIMA_ACTUALIZACION)] <- 9999
df$USERS_CAMP_Q_USERS [is.na(df$USERS_CAMP_Q_USERS)] <- 0
df$USERS_CAMP_Q_LOGIN [is.na(df$USERS_CAMP_Q_LOGIN)] <- 0
df$SHARES_CAMP_Q_SHARES [is.na(df$SHARES_CAMP_Q_SHARES)] <- 0

df$DONOR_VERSION_ULTIMA_ACTUALIZACION <- as.numeric(df$DONOR_VERSION_ULTIMA_ACTUALIZACION)

# Missing data
mice_plot <- aggr(df, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

df$DONACION_RISK_MEASURE <- NULL

#####################################################################################################################################
                                                               # EXPLORATORY


# Exploratorio
hist(df$TENURE)
describe(df)
table(df$CHURN)

# EXPLORATORY
gg_dta <- gg_survival(interval="TENURE", censor="CHURN", 
                      data=df) 
plot(gg_dta)

gg_dta <- gg_survival(interval="TENURE", censor="CHURN", 
                      data=df, by = "DONOR_GENERO") 
plot(gg_dta)

#####################################################################################################################################
                                                                  # TRAIN
# Only Train, Train - Test split is not needed as RSF algorithm uses Out of Bag samples for test. 
train <- df

# check cantidad de casos en training
table(train$CHURN)

# SMOTE (Test if balancing performes better)

#as.data.frame(table(train$CHURN))
#train$CHURN <- as.factor(train$CHURN)
#train.balanced <- SMOTE(CHURN ~ ., train, perc.over = 100, k = 5, perc.under = 400)
#as.data.frame(table(train.balanced$CHURN))

# Choose to work with inbalance or balanced dataset 
train <- train
#train <- train.balanced

as.data.frame(table(train$CHURN))
#train$CHURN <- as.numeric(train$CHURN)

#####################################################################################################################################
                                                  # PARAMETER TUNNING
# Genero vector con target para poder indicar que es un dataset desbalanceado
y <- train$CHURN

# Tunning mtry & nodesize
## default tuning call
o <- tune(Surv(TENURE, CHURN) ~ . , train, na.action = 'na.impute', case.wt = randomForestSRC:::make.wt(y))
print(o$rf)

## visualize the nodesize/mtry OOB surface
if (library("akima", logical.return = TRUE)) {
  
  ## nice little wrapper for plotting results
  plot.tune <- function(o, linear = TRUE) {
    x <- o$results[,1]
    y <- o$results[,2]
    z <- o$results[,3]
    so <- interp(x=x, y=y, z=z, linear = linear)
    idx <- which.min(z)
    x0 <- x[idx]
    y0 <- y[idx]
    filled.contour(x = so$x,
                   y = so$y,
                   z = so$z,
                   xlim = range(so$x, finite = TRUE) + c(-2, 2),
                   ylim = range(so$y, finite = TRUE) + c(-2, 2),
                   color.palette =
                     colorRampPalette(c("yellow", "red")),
                   xlab = "nodesize",
                   ylab = "mtry",
                   main = "OOB error for nodesize and mtry",
                   key.title = title(main = "OOB error", cex.main = 1),
                   plot.axes = {axis(1);axis(2);points(x0,y0,pch="x",cex=1,font=2);
                     points(x,y,pch=16,cex=.25)})
  }
  
  ## plot the surface
  plot.tune(o)
  
}


# Modelo para tunnear
out.rsf.tunning <- rfsrcFast(Surv(TENURE, CHURN) ~ . , 
                   data = train,
                   mtry = 40,
                   nodesize = 2,
                   ntree = 200, 
                   na.action = 'na.impute',
                   case.wt = randomForestSRC:::make.wt(y),
                   block.size = 10) # Guarda info de error cada x cantidad de arboles incrementales

out.rsf.tunning

# Tunning ntree
error <- gg_error(out.rsf.tunning)
plot(na.omit(error))

gg_dta <- gg_variable(out.rsf.tunning, time=21)

#####################################################################################################################################
                                                          # RSF MODEL
# Modelo
out.rsf.1 <- rfsrcFast(Surv(TENURE, CHURN) ~ . , 
                             data = train,
                             mtry = 40,
                             nodesize = 2,
                             ntree = 75, 
                             na.action = 'na.impute',
                             case.wt = randomForestSRC:::make.wt(y), # indica dataset desbalanceado
                             importance = TRUE)

# Feature importance
importance <- data.frame(keyName=names(out.rsf.1$importance), value=out.rsf.1$importance, row.names=NULL)

# Error rate
rcorr.cens(out.rsf.1$predicted.oob, 
             Surv(train$TENURE, train$CHURN))["C Index"]

err.rate.rsf = out.rsf.1$err.rate[ out.rsf.1$ntree ]
err.rate.rsf

# Curva ROC
w.ROC = risksetROC(Stime = train$TENURE,  
                   status = train$CHURN, 
                   marker = out.rsf.1$predicted.oob, 
                   predict.time = median(train$TENURE), 
                   method = "Cox", 
                   main = paste("OOB Survival ROC Curve at t=", 
                                median(train$TENURE)), 
                   lwd = 3, 
                   col = "red" )
w.ROC$AUC


#####################################################################################################################################
                                                        # VALIDATION (=TEST)

# Validation
rs <- dbSendQuery(mydb, "select * from Z_VALIDATION WHERE CHURN = 0")
validation <- fetch(rs, n=-1)

# Descargo las variables que me permiten entender si la donacion esta viva al momento de la evaluacion
validation$CHURN <- NULL
validation$TENURE <- NULL

# Donation_id como index
rownames(validation) <- validation$DONATION_ID

# Donation_id como index
rownames(validation) <- validation$DONATION_ID

# Drop de IDs
validation$DONOR_ID <- NULL
validation$CAMPAIGN_ID <- NULL
validation$DONATION_ID <- NULL

# Drop variables que no van
validation$DONACION_ONETIME <- NULL
validation$DONACION_MENSUAL <- NULL
validation$DONACION_FIN_PROGRAMADO <- NULL
validation$DONACION_INICIO <- NULL
validation$DONACION_FIN<- NULL
validation$DONACION_FIN_ANIO_MES<- NULL
validation$DONACION_INICIO_FECHA<- NULL
validation$DONACION_FIN_FECHA<- NULL
validation$CAMPANIA_INICIO<- NULL
validation$CAMPANIA_FIN<- NULL
validation$CAMPANIA_FIN_ANIO_MES<- NULL
validation$ONG_INICIO_EN_DO<- NULL
validation$ONG_FIN_EN_DO<- NULL
validation$ONG_FIN_EN_DO_ANIO_MES<- NULL
validation$DONOR_FECHA_ACTUALIZACION<- NULL
validation$DONACION_IP<- NULL

# Imputacion de missings
validation$DONACION_DURACION_PROGRAMADA [is.na(validation$DONACION_DURACION_PROGRAMADA )] <- 9999 
validation$DONOR_VERSION_CANTIDAD_ACTUALIZACIONES [is.na(validation$DONOR_VERSION_CANTIDAD_ACTUALIZACIONES )] <- 0 
validation$DONOR_VERSION_ULTIMA_ACTUALIZACION [is.na(validation$DONOR_VERSION_ULTIMA_ACTUALIZACION)] <- 9999
validation$USERS_CAMP_Q_USERS [is.na(validation$USERS_CAMP_Q_USERS)] <- 0
validation$USERS_CAMP_Q_LOGIN [is.na(validation$USERS_CAMP_Q_LOGIN)] <- 0
validation$SHARES_CAMP_Q_SHARES [is.na(validation$SHARES_CAMP_Q_SHARES)] <- 0

validation$DONOR_VERSION_ULTIMA_ACTUALIZACION <- as.numeric(validation$DONOR_VERSION_ULTIMA_ACTUALIZACION)

# Missing data
validation$DONACION_RISK_MEASURE <- NULL


colnames(validation)[colnames(validation) == 'TENURE_FUTURO'] <- 'TENURE'
colnames(validation)[colnames(validation) == 'CHURN_FUTURO'] <- 'CHURN'
test <- validation

# Aplico modelo
pred.validation.fin = predict(out.rsf.1, 
                            newdata = test, 
                            outcome = "test",
                            na.action = 'na.impute',
                            importance = FALSE,
                            block.size = 1,
                            err.block=1)


#####################################################################################################################################
                                                      # PERFORMANCE METRICS
# IBS: Debe ser menor a 33%, 25% y baseline. 
# Se prueban resultados con distintos modelos. 
out.rsf.1

# C-Index
rcorr.cens(-pred.validation.fin$predicted , 
           Surv(validation$TENURE, validation$CHURN))["C Index"]

# ROC general
w.ROC = risksetROC(Stime = validation$TENURE,  
                   status = validation$CHURN, 
                   marker = pred.validation.fin$predicted, 
                   predict.time = 30, 
                   method = "Cox", 
                   main = "OOB Survival ROC Curve at t = 30", 
                   lwd = 3, 
                   col = "red" )
cat("ROC mediana tenure ", w.ROC$AUC)

# ROC en distintos timeframes
## Define a helper functio nto evaluate at various t
risksetROC_helper <- function(t) {
  risksetROC(Stime = validation$TENURE,  
             status = validation$CHURN, 
             marker = pred.validation.fin$predicted, 
             predict.time = t,
             method = "Cox", 
             plot         = FALSE)
}

## Evaluate differente t
risksetROC_data <- data_frame(t = c(2,5,10,15,30,45,60,80,365)) %>%
  mutate(risksetROC = map(t, risksetROC_helper),
         ## Extract scalar AUC
         auc = map_dbl(risksetROC, magrittr::extract2, "AUC"),
         ## Put cut off dependent values in a data_frame
         df_risksetROC = map(risksetROC, function(obj) {
           ## marker column is too short!
           marker <- c(-Inf, obj[["marker"]], Inf)
           bind_cols(data_frame(marker = marker),
                     as_data_frame(obj[c("TP","FP")]))
         })) %>%
  dplyr::select(-risksetROC) %>%
  unnest() %>%
  arrange(t, FP, TP)

## Plot ROCs
risksetROC_data %>%
  ggplot(mapping = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  geom_label(data = risksetROC_data %>% dplyr::select(t,auc) %>% unique,
             mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
  facet_wrap( ~ t) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())

## PLOT AUC con el paso del tiempo
AUC <- risksetAUC(Stime = validation$TENURE,  
                  status = validation$CHURN, 
                  marker = pred.validation.fin$predicted, 
                  tmax = 100, 
                  method = "Cox", 
                  main = "AUC at different point times", 
                  lty = 2, 
                  col = "red" )


#####################################################################################################################################
                                                      # OUTPUT FILES
# Survival curve
df <- data.frame(pred.validation.fin$survival)
colnames(df) <- pred.validation.fin$time.interest
pred.survival <- cbind(data.frame(rownames(validation)),validation$TENURE,validation$CHURN,df)
colnames(pred.survival)[colnames(pred.survival) == 'rownames.validation.'] <- 'DONATION_ID'
colnames(pred.survival)[colnames(pred.survival) == 'validation$TENURE'] <- 'TENURE'
colnames(pred.survival)[colnames(pred.survival) == 'validation$CHURN'] <- 'CHURN'
rm(df)


write.csv2(pred.survival, file = "./results/validation_predict_survival_curve.csv")

# CHF
df <- data.frame(pred.validation.fin$chf)
colnames(df) <- pred.validation.fin$time.interest
pred.survival <- cbind(data.frame(rownames(validation)),validation$TENURE,validation$CHURN,df)
colnames(pred.survival)[colnames(pred.survival) == 'rownames.validation.'] <- 'DONATION_ID'
colnames(pred.survival)[colnames(pred.survival) == 'validation$TENURE'] <- 'TENURE'
colnames(pred.survival)[colnames(pred.survival) == 'validation$CHURN'] <- 'CHURN'
rm(df)

write.csv2(pred.survival, file = "./results/validation_predict_chf.csv")

# Predicted ensemble mortality
df <- data.frame(pred.validation.fin$predicted)
pred.survival <- cbind(data.frame(rownames(validation)),validation$TENURE,validation$CHURN,df)
colnames(pred.survival)[colnames(pred.survival) == 'rownames.validation.'] <- 'DONATION_ID'
colnames(pred.survival)[colnames(pred.survival) == 'validation$TENURE'] <- 'TENURE'
colnames(pred.survival)[colnames(pred.survival) == 'validation$CHURN'] <- 'CHURN'
colnames(pred.survival)[colnames(pred.survival) == 'pred.validation.fin.predicted'] <- 'ENSEMBLE MORTALITY PREDICTION'
rm(df)

write.csv2(pred.survival, file = "./results/validation_predict_ensamble_mortality.csv")

rm(pred.survival)


