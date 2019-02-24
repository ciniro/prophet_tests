rm(list=ls())
cat('\014')
library(prophet)
library(TTR)
library(MLmetrics)

POCID <- function(previsto, real)
{
  tam <- length(real)
  calc <- (previsto[2:tam]-previsto[1:(tam-1)])*(real[2:tam]-real[1:(tam-1)])
  for (i in 1:length(calc))
  {
    if (calc[i]>0)
      calc[i] <- 1
    else
      calc[i] <- 0
  }
  return(mean(calc))
}

removeOutliers <- function(x, na.rm = TRUE, ...) 
{
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  
  for(i in 1:length(y)) 
  {
    #caso o primeiro valor seja NA procura o proximo valor nao NA e coloca
    #no lugar do NA
    if (is.na(y[1]) == TRUE)
    {
      encontrou = FALSE
      cont = 1
      posterior = NA
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[1+cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          posterior <- y[1+cont];
          encontrou <- TRUE
        }
      }
      
      y[1] <- posterior
    }
    
    #caso o ultimo valor seja NA procura o primeiro valor anterior que nao NA e coloca
    #no lugar do NA
    if (is.na(y[length(y)]) == TRUE)
    {
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[length(y)-cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          anterior <- y[length(y)-cont];
          encontrou <- TRUE
        }
      }
      
      y[length(y)] <- anterior
    }
    
    
    
    if (is.na(y[i])==TRUE)
    {
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[i-cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          anterior <- y[i-cont];
          encontrou <- TRUE
        }
      }
      
      encontrou = FALSE
      cont = 1
      posterior = NA
      
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[i+cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          posterior <- y[i+cont];
          encontrou <- TRUE
        }
      }
      
      #executa uma media entre o anterior e posterior valor valido na serie e insere no lugar do outlier
      y[i] <- (anterior+posterior)/2
    }
  }
  
  return(y)
}

normalize <- function(s)
{
  retorno <- (s - min(s))/(max(s)-min(s))
  return(retorno)
}

df <- read.csv("precos.csv")
plot(df[,2], type="l")

#POSSIBLE TRANSFORMATIONS
#df[,2] <- log(df[,2])
#df[,2] <- c(0,diff(df[,2]))
#df[,2] <- normalize(removeOutliers(c(0,diff(df[,2]))))
#plot(df[,2], type="l")

m <- prophet(df)

future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)

p <- 21

nobs <- 1000 + p
y <- df$y[1:nobs]
yhat <- forecast$yhat[1:nobs]

sma <- SMA(y, p)
  
plot(y[(p+1):nobs], type="l", col="blue")
par(new=T)
plot(yhat[(p+1):nobs], type="l", col="red")
par(new=T)
plot(sma[(p+1):nobs], type="l", col="green")

RMSE(yhat, y)
POCID(yhat, y)

RMSE(sma[(p+1):nobs], y[(p+1):nobs])
POCID(sma[(p+1):nobs], y[(p+1):nobs])
