library(shiny)
library(shinydashboard)
library(deSolve)
library(readxl)
library(httr)
library(deSolve)
library(ggplot2)
library(rmarkdown)
library(reshape2)
library(readxl)
library(dplyr)
library(sqldf)
library(data.table)
library(scales)
library(lubridate)
library(tseries)
library(MASS)
library(fBasics)
library(forecast)

importData <- function() {
  url <- "https://analisi.transparenciacatalunya.cat/api/views/c7sd-zy9j/rows.csv?accessType=DOWNLOAD&bom=true&format=true&sorting=true"
  complete_df <- read.csv(url)
  data <- complete_df[order(as.Date(complete_df$DATA, format="%d/%m/%Y")),]
  
  infectats <- data %>% group_by(DATA, CODI) %>% summarise(Total = sum(CASOS_CONFIRMAT))
  infectats <- infectats[order(as.Date(infectats$DATA, format="%d/%m/%Y")), ]
  inf <- sqldf('SELECT DATA, SUM(Total)  AS Total FROM infectats GROUP BY DATA')
  inf <- inf[order(as.Date(inf$DATA, format="%d/%m/%Y")), ]
  
  inf_acum <- inf
  for (row in 2:nrow(inf_acum)) {
    inf_acum[row, "Total"] = inf_acum[row-1, "Total"] + inf_acum[row, "Total"]
  }
  
  # Calculem els infectats totals en un dia com la suma dels infectats en t_0,...t_-9 (és una estimació)
  inf_totals <- inf$Total
  inf_mitja_7dies <- inf$Total
  nous <- inf$Total
  for (i in 1:length(inf_totals)) {
    if (i == 1) {
      inf_totals[i] = nous[i]
      inf_mitja_7dies[i] = nous[i]
    }else if (i == 2) {
      inf_totals[i] = nous[i] + nous[i-1]
      inf_mitja_7dies[i] = 1/2*(nous[i]+nous[i-1])
    }else if (i ==3){
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2]
      inf_mitja_7dies[i] = 1/3*(nous[i] + nous[i-1] + nous[i-2])
    }else if (i == 4) {
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3]
      inf_mitja_7dies[i] = 1/4*(nous[i] + nous[i-1] + nous[i-2] + nous[i-3])
    }else if (i ==5){
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4]
      inf_mitja_7dies[i] = 1/5*(nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4])
    }else if (i == 6) {
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5]
      inf_mitja_7dies[i] = 1/6*(nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5])
    }else if (i ==7){
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5] + nous[i-6]
    }else if (i == 8) {
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5] + nous[i-6] + nous[i-7]
    }else if (i ==9){
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5] + nous[i-6] + nous[i-7] + nous[i-8]
    }else if (i ==10){
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5] + nous[i-6] + nous[i-7] + nous[i-8] + nous[i-9]
    }else{
      inf_totals[i] = nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5] + nous[i-6] + nous[i-7] + nous[i-8] + nous[i-9] + nous[i-10]
      inf_mitja_7dies[i] =  (nous[i] + nous[i-1] + nous[i-2] + nous[i-3] + nous[i-4] + nous[i-5] + nous[i-6] + nous[i-7])*1/7
    }
  }
  
  exitus <- data %>% group_by(DATA, CODI) %>% summarise(Total = sum(EXITUS))
  exitus <- exitus[order(as.Date(exitus$DATA, format="%d/%m/%Y")), ]
  morts <- sqldf('SELECT DATA, SUM(Total)  AS Total FROM exitus GROUP BY DATA')
  morts <- morts[order(as.Date(morts$DATA, format="%d/%m/%Y")), ]
  
  morts_acum <- morts
  for (row in 2:nrow(morts_acum)) {
    morts_acum[row, "Total"] = morts_acum[row-1, "Total"] + morts_acum[row, "Total"]
  }
  
  formated_date = as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
  dataset_reals <- data.frame(DATA = formated_date, infectats = inf$Total, infectats_totals = inf_totals, infectats_acum = inf_acum$Total, inf_mitja_7dies = inf_mitja_7dies, morts = morts$Total, morts_acum = morts_acum$Total)
  
  url <- "https://analisi.transparenciacatalunya.cat/api/views/cuwj-bh3b/rows.csv?accessType=DOWNLOAD&sorting=true"
  complete_vacunats_df <- read.csv(url)
  complete_vacunats_df2 <- filter(complete_vacunats_df, DOSI == "2")
  data_vacunats <- complete_vacunats_df[order(as.Date(complete_vacunats_df2$DATA, format="%d/%m/%Y")),]
  
  vac <- data_vacunats %>% group_by(DATA, COMARCA_CODI) %>% summarise(Total = sum(RECOMPTE))
  vac <- vac[order(as.Date(vac$DATA, format="%d/%m/%Y")), ]
  vacunats <- sqldf('SELECT DATA, SUM(Total)  AS Total FROM vac GROUP BY DATA')
  vacunats <- vacunats[order(as.Date(vacunats$DATA, format="%d/%m/%Y")), ]
  
  vacunats_acum <- vacunats
  for (row in 2:nrow(vacunats_acum)) {
    vacunats_acum[row, "Total"] = vacunats_acum[row-1, "Total"] + vacunats_acum[row, "Total"]
  }
  
  formated_date = as.Date(vacunats$DATA, tryFormats = c("%d/%m/%Y"))
  dataset_vacunats <- data.frame(DATA = formated_date, vacunats = vacunats$Total, vacunats_acum = vacunats_acum$Total)
  types <- data_vacunats %>% group_by(FABRICANT) %>% summarise(Total = sum(RECOMPTE))
}

errors <- function() {
  df_errors <- data.frame(DATA = formated_date, errors_infectats = dataset_reals$infectats_totals - compartments3$I,
                          errors_morts = dataset_reals$morts - compartments3$D, errors_infectats_dif = c(diff(dataset_reals$infectats_totals - compartments3$I),0))
  
  ggplot() + geom_line(data = df_errors[1:100,], aes(x = DATA[1:100], y = errors_infectats[1:100])) +
    labs(x = "Període de dies", y = "Errors nombre d'infectats")
  
  ggplot() + geom_line(data = df_errors[1:100,], aes(x = DATA[1:100], y = errors_infectats_dif[1:100])) +
    labs(x = "Període de dies", y = "Errors diferenciats nombre d'infectats")
  
  ggplot() + geom_line(data = df_errors[1:100,], aes(x = DATA[1:100], y = errors_morts[1:100])) +
    labs(x = "Període de dies", y = "Errors nombre de morts")
  
  # ********************* INFECTATS ********************* #
  errors1 <- dataset_reals$infectats_totals - compartments3$I
  mean(errors1)
  ts.plot(errors1)
  
  errors2 <- dataset_reals$morts  - compartments3$D
  mean(errors2)
  ts.plot(errors2)
  
  t = as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
  # La nostra unitat de temps és una setmana, i per cada setmana tenim 7 observacions
  myts <- ts(errors1, start=as.numeric(format(t[1], "%j")), frequency = 7)
  plot.ts(myts, xlab="Període de dies", ylab="Sèrie", 
          main="Errors comesos en les prediccions mostrals del nombre d'infectats diaris a Catalunya")
  
  par(mfrow=c(2,1))
  acf(myts, lag=60, main="")
  pacf(myts, lag=60, main="")
  
  # Comprovem que la sèrie NO és encara estacionària
  # p-valor > 0.05 ==> H0 cert ==> no estacionària
  adf.test(myts)
  
  myts_diff <- diff(myts)
  plot(myts_diff)
  acf(myts_diff, lag=100, main="")
  pacf(myts_diff, lag=100, main="")
  
  # Ara ja sí estacionària :)
  adf.test(myts_diff)
  
  acf(myts_diff, lag=60, main="")
  pacf(myts_diff, lag=60, main="")
  
  modArima <- auto.arima(myts_diff)
  modArima
  
  model1 <- arima(myts_diff, c(1,0,3), seasonal=list(order=c(0,0,2)), include.mean = TRUE)
  model1
  
  # Prediccions extra-mostrals INFECTATS ------------------------------- #
  # inicial: 100(7,30) (ARIMA(2,0,1)), 180(30), 280(30), 230(7,30) MILLOREN AMB LES CORRECCIONS
  inicial <- 100
  preds <- 30
  inicial2 <- inicial + 1
  final <- inicial + preds
  
  errors <- dataset_reals$infectats_totals - compartments3$I
  myts <- ts(errors, start=as.numeric(format(t[1], "%j")), frequency = 7)
  myts_diff <- diff(myts)
  errors_mostrals <- errors[1:inicial]
  
  # Prediccions amb la cerca tal qual
  seq <- seq(from=inicial, to=inicial+preds, by=1)
  times <- seq(from=dataset_reals$DATA[inicial],by="day",length.out=preds)
  parameters <- c(beta=params3$beta[inicial],gamma=params3$gamma[inicial],delta=params3$delta[inicial],mu=params3$mu[inicial],N=7500001)
  state <- c(S=compartments3$S[inicial], E=compartments3$E[inicial], I=compartments3$I[inicial],R=compartments3$R[inicial], D=compartments3$D[inicial])
  out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
  gs_pred_inf <- as.data.frame(out)$I
  gs_pred_inf
  
  sub_ts <- ts(myts_diff[1:inicial], start=as.numeric(format(t[1], "%j")), frequency = 7)
  adf.test(sub_ts)
  arima1 <- auto.arima(sub_ts)
  arima1
  
  mpred_1 <- arima(sub_ts[1:inicial], c(2,0,1), seasonal=list(order=c(0,0,0)), include.mean = TRUE)
  mpred_1
  model1_pred <- predict(mpred_1, n.ahead=preds)
  errors_pred1 <- model1_pred$pred
  
  plot.ts(errors_pred1)
  
  # VALIDACIO:
  
  par(mfrow=c(1,1))
  # p-valor < 0.05 => residus no normals
  # Sembla que si a les gràfiques però es per un problema d'apuntament
  jarqueberaTest(mpred_1$residuals)
  hist(mpred_1$residuals, breaks = 60, main="Histograma dels residus", xlab="Residus", ylab="Freqüències", xlim=c(-1500,1500))
  curve(dnorm(x, 0, 0.022), col="red", add = TRUE)
  
  pnorm(c(abs(mpred_1$coef)/sqrt(diag(mpred_1$var.coef))), mean=0, sd=1, lower.tail=FALSE)
  
  plot.ts(mpred_1$residuals, xlab="Període de dies", ylab="Residus")
  tsdiag(mpred_1)
  
  par(mfrow=c(2,1))
  acf(mpred_1$residuals,ylim=c(-1,1),lag=100, main="Funció autocorrelació simple")
  pacf(mpred_1$residuals,ylim=c(-1,1),lag=100, main="Funció autocorrelació parcial")
  
  lines(xfit, yfit, col = "black", lwd = 2)
  
  phi1 = -0.4187
  phi2 = -0.4071
  theta1 = -0.4071
  delta = 0.2786
  
  x1 = Mod((phi1 + sqrt(as.complex(phi1^2 + 4*phi2)))/(-2*phi2))
  
  errors2 <- dataset_reals$morts - compartments3$D
  myts2 <- ts(errors, start=as.numeric(format(t[1], "%j")), frequency = 7)
  myts_diff2 <- diff(myts2)
  errors_mostrals2 <- errors[1:inicial]
  
  ### Errors SENSE correccions:
  err <- abs(gs_pred_inf[2:length(gs_pred_inf)] - dataset_reals$infectats_totals[inicial2:final])
  epam_sense <- mean(err/abs(dataset_reals$infectats_totals[inicial2:final]))*100
  epam_sense
  
  corregit <- errors_pred1 + gs_pred_inf[2:(length(gs_pred_inf))]
  ### Errors AMB correccions: 
  err <- abs(corregit - dataset_reals$infectats_totals[inicial2:final])
  epam_amb <- mean(err/abs(dataset_reals$infectats_totals[inicial2:final]))*100
  epam_amb
  
  
  ## ERRORS:
  # Avaluació capacitat predictiva
  inf_reals <- dataset_reals[101:114,]$infectats_totals
  
  inf_sense <- df_pred_correcio$inf_sense[2:15]
  errors_sense <- inf_reals-inf_sense
  errors_abs_sense <- abs(errors_sense)
  errors_quadratics_sense <- errors_sense^2
  mean_errors_abs_sense <- mean(errors_abs_sense)
  mean_errors_quadratics_sense <- mean(errors_quadratics_sense)
  epa <- errors_abs_sense/inf_reals
  epam <- mean(errors_abs_sense/inf_reals)
  epam100 <- epam*100
  epam100
  
  inf_amb <- df_pred_correcio$inf_amb[2:15]
  errors_amb <- inf_reals-inf_amb
  errors_abs_amb <- abs(errors_amb)
  errors_quadratics_amb <- errors_amb^2
  mean_errors_abs_amb <- mean(errors_abs_amb)
  mean_errors_quadratics_amb <- mean(errors_quadratics_amb)
  epa <- errors_abs_amb/inf_reals
  epam <- mean(errors_abs_amb/inf_reals)
  epam100 <- epam*100
  epam100
  
  morts_reals<- dataset_reals[101:107,]$morts
  
  morts_sense <- df_pred_correcio$morts_sense[2:8]
  errors_sense <- morts_reals-morts_sense
  errors_abs_sense <- abs(errors_sense)
  errors_quadratics_sense <- errors_sense^2
  mean_errors_abs_sense <- mean(errors_abs_sense)
  mean_errors_quadratics_sense <- mean(errors_quadratics_sense)
  epa <- errors_abs_sense/morts_reals
  epam <- mean(errors_abs_sense/morts_reals)
  epam100 <- epam*100
  epam100
  
  morts_amb <- df_pred_correcio$morts_amb[2:8]
  errors_amb <- morts_reals-morts_amb
  errors_abs_amb <- abs(errors_amb)
  errors_quadratics_amb <- errors_amb^2
  mean_errors_abs_amb <- mean(errors_abs_amb)
  mean_errors_quadratics_amb <- mean(errors_quadratics_amb)
  epa <- errors_abs_amb/morts_reals
  epam <- mean(errors_abs_amb/morts_reals)
  epam100 <- epam*100
  epam100
  
  ## MORTS:
  
  errors2 <- dataset_reals$morts - compartments3$D
  myts2 <- ts(errors2, start=as.numeric(format(t[1], "%j")), frequency = 7)
  myts_diff2 <- diff(myts2)
  errors_mostrals2 <- errors2[1:inicial]
  
  # Prediccions amb la cerca tal qual
  seq <- seq(from=inicial, to=inicial+preds, by=1)
  times <- seq(from=dataset_reals$DATA[inicial],by="day",length.out=preds)
  parameters <- c(beta=params3$beta[inicial],gamma=params3$gamma[inicial],delta=params3$delta[inicial],mu=params3$mu[inicial],N=7500001)
  state <- c(S=compartments3$S[inicial], E=compartments3$E[inicial], I=compartments3$I[inicial],R=compartments3$R[inicial], D=compartments3$D[inicial])
  out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
  gs_pred_morts <- as.data.frame(out)$D
  gs_pred_morts
  
  sub_ts2 <- ts(myts_diff2[1:inicial], start=as.numeric(format(t[1], "%j")), frequency = 7)
  adf.test(sub_ts2)
  arima2 <- auto.arima(sub_ts2)
  arima2
  
  mpred_2 <- arima(sub_ts2[1:inicial], c(2,0,3), seasonal=list(order=c(2,0,0)), include.mean = TRUE)
  mpred_2
  model2_pred <- predict(mpred_2, n.ahead=preds)
  errors_pred2 <- model2_pred$pred
  
  plot.ts(errors_pred2)
  
 # df_errors_extra <- data.frame(DATA = formated_date[101:(101+preds-1)], err_extra_inf = errors_pred1,
 #                    err_extra_morts = errors_pred2)
  
 # df_pred_correcio <- data.frame(DATA = inf$DATA[100:114], inf_sense = compartments3$I[100:114],
 #                      inf_amb = compartments3$I[100:114] + df_errors_extra$err_extra_inf[1:15],
 #                  morts_sense = compartments3$D[100:114],
 #                 morts_amb = compartments3$D[100:114] + df_errors_extra$err_extra_morts[1:15])
  
  par(mfrow=c(1,1))
  df <- data.frame(date=date[inicial2:(inicial+preds)], sense=gs_pred_inf[1:preds], amb=as.numeric(corregit))
  ggplot() + 
    geom_line(data = df, aes(x = date, y = sense, color = "blue")) +
    geom_line(data = df, aes(x = date, y = amb, color = "red")) +
    labs(x = "Període de dies", y = "Nombre d'infectats estimats") +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_colour_manual(labels = c("Sense correccions", "Amb correccions"),
                        values = c("blue", "red")) + scale_x_date(date_labels="%d/%m/%Y")
  
  # ---------------------------------------------------------- #
  
  tsdiag(model1)
  
  par(mfrow=c(2,1))
  acf(model1$residuals,ylim=c(-1,1),lag=100, main="Residus")
  pacf(model1$residuals,ylim=c(-1,1),lag=100, main="Residus")
  
  # Coeficients significatius
  pnorm(c(abs(model1$coef)/sqrt(diag(model1$var.coef))), mean=0, sd=1, lower.tail=FALSE)
  
  par(mfrow=c(1,1))
  # p-valor < 0.05 => residus no normals
  # Sembla que si a les gràfiques però es per un problema d'apuntament
  jarqueberaTest(model1$residuals)
  hist(model1$residuals, breaks = 20, main="Histograma dels residus", xlab="Residus", ylab="Freqüències")
  
  # Prediccions : (7 perquè el model té memòria de 7 dades enrere com a màxim, després s'estabilitza en la última)
  # Només cal canviar "inicial1" per fer les 7 prediccions on desitgem 
  inicial1 <- 300
  periode1 <- 40
  final1 <- inicial1 + periode1
  inicial2 <- final1 + 1
  periode2 <- 7
  final2 <- final1 + periode2
  
  mpred_1 <- arima(myts[1:inicial2], c(2,0,3), seasonal=list(order=c(0,0,2)), include.mean = TRUE)
  model1_pred <- predict(mpred_1, n.ahead=7)
  par(mfrow=c(1,1))
  mostral <- ts(myts[inicial1:inicial2],start=inicial1)
  extra <- ts(myts[inicial2:final2],start=inicial2)
  pred <- ts(model1_pred$pred, start=inicial2)
  ts.plot(mostral,extra,pred,col=c(2,4,7))
  abline(v=inicial2, lty=2, col=1)
  legend("topright", col=c(2,4,7), lty=c(1,1), legend=c("Dades període mostral", "Dades reals període extra-mostral", "Dades predites període extra-mostral"))
  
  # Avaluació capacitat predictiva
  errors_post1<-myts[inicial2:final2]-model1_pred$pred
  ts.plot(errors_post1,col=2)
  eqm1<-sum(errors_post1*errors_post1)/7
  reqm1<-eqm^(.5)
  eam1<-sum(abs(errors_post1))/7
  epam1<-sum(abs(errors_post1)/abs(myts[inicial2:final2]))/7
  
  eqm1
  reqm1
  eam1
  epam1 # per inicial1 = 400 : EPAM del 29.59176 !!! ~ 2959.176%  ==> MOLT MOLT ELEVAT
  # si traiem el 199.3123 (valor súper gran!!!):
  epam_arreglat <- (2.4192896+3.4436539+0.2335859+0.1592460+0.6086433+0.9655934)/6
  epam_arreglat # 1.305 ~ 103.5 % ==> SEGUEIX SENT MOLT GRAN PERÒ NO ÉS TAN DESCABELLAO
   
  # Observem els errors estàndard:
  model1_pred$se
  
  inf1 = model1_pred$pred - 2*model1_pred$se
  sup1 = model1_pred$pred + 2*model1_pred$se
  
  # Bandes de confiança del 95%
  ts.plot(myts[441:447], model1_pred$pred, col=c(2,4), ylim=c(-3000, 3000), xlab="")
  lines(inf1, col="black", lty="dashed") 
  lines(sup1, col="black", lty="dashed")
  
  # ********************* MORTS ********************* #
  # inicial: 100(7,20)
  errors2 <- dataset_reals$morts - compartments3$D
  mean(errors2)
  par(mfrow=c(1,1))
  ts.plot(errors2)
  
  inicial <- 390
  preds <- 7
  inicial2 <- inicial + 1
  final <- inicial + preds

  errors_mostrals_morts <- errors2[1:417]
  t = as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
  # La nostra unitat de temps és una setmana, i per cada setmana tenim 7 observacions
  myts <- ts(errors_mostrals, start=as.numeric(format(t[1], "%j")), frequency = 7)
  par(mfrow=c(1,1))
  plot.ts(myts, xlab="Període de dies", ylab="Sèrie", 
          main="Errors comesos en les prediccions mostrals del nombre de morts diaris a Catalunya")
  
  par(mfrow=c(2,1))
  acf(myts2, lag=60, main="")
  pacf(myts2, lag=60, main="")
  
  adf.test(myts2)

  # Prediccions amb la cerca tal qual
  seq <- seq(from=inicial, to=inicial+preds, by=1)
  times <- seq(from=dataset_reals$DATA[inicial],by="day",length.out=preds)
  parameters <- c(beta=params3$beta[inicial],gamma=params3$gamma[inicial],delta=params3$delta[inicial],mu=params3$mu[inicial],N=7500001)
  state <- c(S=compartments3$S[inicial], E=compartments3$E[inicial], I=compartments3$I[inicial],R=compartments3$R[inicial], D=compartments3$D[inicial])
  out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
  gs_pred_morts <- as.data.frame(out)$D
  gs_pred_morts
  
  sub_ts <- ts(myts2[1:inicial], start=as.numeric(format(t[1], "%j")), frequency = 7)
  adf.test(sub_ts)
  arima1 <- auto.arima(sub_ts)
  arima1
  
  mpred_1 <- arima(sub_ts[1:inicial], c(2,1,4), seasonal=list(order=c(0,0,1)), include.mean = TRUE)
  model1_pred <- predict(mpred_1, n.ahead=preds)
  errors_pred1 <- model1_pred$pred
  
  plot.ts(errors_pred1)
  
  ### Errors SENSE correccions:
  err <- abs(gs_pred_morts[2:length(gs_pred_morts)] - dataset_reals$morts[inicial2:final])
  epam_sense <- mean(err/abs(dataset_reals$morts[inicial2:final]))*100
  epam_sense
  
  corregit <- errors_pred1 + gs_pred_morts[2:(length(gs_pred_morts))]
  ### Errors AMB correccions: 
  err <- abs(corregit - dataset_reals$morts[inicial2:final])
  epam_amb <- mean(err/abs(dataset_reals$morts[inicial2:final]))*100
  epam_amb

  par(mfrow=c(1,1))
  df <- data.frame(date=date[inicial2:(inicial+preds)], sense=gs_pred_morts[1:preds], amb=as.numeric(corregit))
  ggplot() + 
    geom_line(data = df, aes(x = date, y = sense, color = "blue")) +
    geom_line(data = df, aes(x = date, y = amb, color = "red")) +
    labs(x = "Període de dies", y = "Nombre de morts estimats") +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_colour_manual(labels = c("Sense correccions", "Amb correccions"),
                        values = c("blue", "red")) + scale_x_date(date_labels="%d/%m/%Y")
  # ----------------------------------- #
  
  # Comprovem que la sèrie NO és encara estacionària
  # p-valor < 0.05 ==> H0 no ==> sí estacionària
  adf.test(myts2)
  
  modArima2 <- auto.arima(myts2[1:40])
  modArima2
  
  model2 <- arima(myts2, c(2,1,4), seasonal=list(order=c(0,0,1)), include.mean = TRUE)
  model2
  
  tsdiag(model2)
  
  par(mfrow=c(2,1))
  acf(model2$residuals,ylim=c(-1,1),lag=100, main="Residus")
  pacf(model2$residuals,ylim=c(-1,1),lag=100, main="Residus")
  
  # Coeficients significatius
  pnorm(c(abs(model2$coef)/sqrt(diag(model2$var.coef))), mean=0, sd=1, lower.tail=FALSE)
  
  par(mfrow=c(1,1))
  # p-valor < 0.05 => residus no normals
  # Sembla que si a les gràfiques però es per un problema d'apuntament
  jarqueberaTest(model2$residuals)
  hist(model2$residuals, breaks = 20, main="Histograma dels residus", xlab="Residus", ylab="Freqüències")
  
  # Prediccions : (7 perquè el model té memòria de 7 dades enrere com a màxim, després s'estabilitza en la última)
  inicial1 <- 100
  periode1 <- 40
  final1 <- inicial1 + periode1
  inicial2 <- final1 + 1
  periode2 <- 7
  final2 <- final1 + periode2
  
  mpred_2 <- arima(myts2[1:inicial2], c(3,0,1), seasonal=list(order=c(0,0,2)), include.mean = TRUE)
  model2_pred <- predict(mpred_2, n.ahead=7)
  par(mfrow=c(1,1))
  mostral2 <- ts(myts2[inicial1:inicial2],start=inicial1)
  extra2 <- ts(myts2[inicial2:final2],start=inicial2)
  pred2 <- ts(model2_pred$pred, start=inicial2)
  ts.plot(mostral2,extra2,pred2,col=c(2,4,7))
  abline(v=inicial2, lty=2, col=1)
  legend("bottomright", col=c(2,4,7), lty=c(1,1), legend=c("Dades període mostral", "Dades reals període extra-mostral", "Dades predites període extra-mostral"))
  
  # Avaluació capacitat predictiva
  errors_post2<-myts2[inicial2:final2]-model2_pred$pred
  ts.plot(errors_post2,col=2)
  eqm2<-sum(errors_post2*errors_post2)/7
  reqm2<-eqm^(.5)
  eam2<-sum(abs(errors_post2))/7
  epam2<-sum(abs(errors_post2)/abs(myts[inicial2:final2]))/7
  
  eqm2
  reqm2
  eam2
  epam2 # per inicial1 = 400 : EPAM del 0.05419292 ~ 5.419292 % ==> MITJANAMENT BÉ
  
  # Observem els errors estàndard:
  model2_pred$se
  
  inf2 = model2_pred$pred - 2*model2_pred$se
  sup2 = model2_pred$pred + 2*model2_pred$se
  
  # Bandes de confiança del 95%
  ts.plot(myts2[441:447], model2_pred$pred, col=c(2,4), ylim=c(-50, 50), xlab="")
  lines(inf2, col="black", lty="dashed") 
  lines(sup2, col="black", lty="dashed")
}

correcio_errors_infectats <- function() {
  mp <- compartments3$I
  # infectats predits amb la correcció SARIMA
  mpc <- rep(0, length(mp))
  pem <- rep(0, length(mp))
  for (day in c(7:(length(mp)-1))) {
    subserie <- myts_diff[1:day]
    arima1 <- auto.arima(subserie)
    arimacoef <- arima1$arma
    sm <- arima(subserie, c(arimacoef[1],arimacoef[6],arimacoef[2]), seasonal=list(order=c(arimacoef[3],arimacoef[7],arimacoef[4])), include.mean = TRUE)
    smp <- predict(sm, n.ahead=1)
    pem[day+1] <- smp$pred[1]
    mpc[day+1] <- mp[day+1]+smp$pred[1]
  }
  mpc
  n <- 447
  par(mfrow=c(1,1))
  ts.plot(ts(dataset_reals$infectats_totals[1:n]), ts(mpc[1:n]), col=c(2,4))
  legend("topright", legend=c("Dades reals", "Dades predites"),col=c(2,4), lty=c(1,1), cex=1.2)
  
  ts.plot(ts(dataset_reals$infectats_totals[1:n]), ts(compartments3$I[1:n]), col=c(2,4))
  legend("topright", legend=c("Dades reals", "Dades predites"),col=c(2,4), lty=c(1,1), cex=1.2)
  
  err_abs2 = abs(dataset_reals$infectats_totals[1:n]-mpc[1:n])
  mean_err_abs2 = mean(err_abs2)
  err_q2 = (dataset_reals$infectats_totals[1:n]-mpc[1:n])^2
  mean_err_q2 = mean(err_q2)
  epam2 = abs(dataset_reals$infectats_totals[1:n]-mpc[1:n])/(dataset_reals$infectats_totals[1:n]+1)
  mean_epam2 = mean(epam2)
  morts_repl = replace(dataset_reals$infectats_totals, dataset_reals$infectats_totals==0, 1)
  epam_perc2 = abs(dataset_reals$infectats_totals[1:n]-mpc[1:n])/(morts_repl)*100
  mean_epam_perc2 = mean(epam_perc2) 
  # FA ELS ERRORS MÉS GRANS :((((( !!!
  mean_epam_perc2
}

correcio_errors_morts <- function() {
  mp <- compartments3$D
  # morts predits amb la correcció SARIMA
  mpc <- rep(0, length(mp))
  pem <- rep(0, length(mp))
  for (day in c(7:(length(mp)))) {
    subserie <- myts2[1:day]
    arima1 <- auto.arima(subserie)
    arimacoef <- arima1$arma
    sm <- arima(subserie, c(arimacoef[1],arimacoef[6],arimacoef[2]), seasonal=list(order=c(arimacoef[3],arimacoef[7],arimacoef[4])), include.mean = TRUE)
    smp <- predict(sm, n.ahead=1)
    pem[day+1] <- smp$pred[1]
    mpc[day+1] <- mp[day+1]+smp$pred[1]
  }
  mpc
  n <- 447
  par(mfrow=c(1,1))
  ts.plot(ts(dataset_reals$morts[1:n]), ts(mpc[1:n]), col=c(2,4))
  legend("topright", legend=c("Dades reals", "Dades predites"),col=c(2,4), lty=c(1,1), cex=1.2)
  
  ts.plot(ts(dataset_reals$morts[1:n]), ts(compartments3$D[1:n]), col=c(2,4))
  legend("topright", legend=c("Dades reals", "Dades predites"),col=c(2,4), lty=c(1,1), cex=1.2)
  
  err_abs2 = abs(dataset_reals$morts[1:n]-mpc[1:n])
  mean_err_abs2 = mean(err_abs2)
  err_q2 = (dataset_reals$morts[1:n]-mpc[1:n])^2
  mean_err_q2 = mean(err_q2)
  epam2 = abs(dataset_reals$morts[1:n]-mpc[1:n])/(dataset_reals$morts[1:n]+1)
  mean_epam2 = mean(epam2)
  morts_repl = replace(dataset_reals$morts, dataset_reals$morts==0, 1)
  epam_perc2 = abs(dataset_reals$morts[1:n]-mpc[1:n])/(morts_repl)*100
  mean_epam_perc2 = mean(epam_perc2) 
  # FA ELS ERRORS MÉS PETITS :DDDDDDDD !!!
  mean_epam_perc2
}

spreadSIR <- function(time, state, parameters) {
  with(
    as.list(c(state,parameters)), {
      dS <- -(beta*S*I/N)
      dI <- (beta*S*I)/N-gamma*I
      dR <- gamma*I
      return (list(c(dS, dI, dR)))
    }
  )
}

spreadSEIR <- function(time, state, parameters) {
  with(
    as.list(c(state,parameters)), {
      dS <- -(beta*S*I/N)
      dE <- (beta*S*I/N) - delta*E
      dI <- delta*E - gamma*I
      dR <- gamma*I
      return (list(c(dS, dE, dI, dR)))
    }
  )
}

spreadSEIRD <- function(time, state, parameters) {
  with(
    as.list(c(state,parameters)), {
      dS <- -(beta*S*I/N)
      dE <- (beta*S*I/N) - delta*E
      dI <- delta*E - gamma*I
      dR <- (1-mu)*gamma*I
      dD <- mu*gamma*I
      return (list(c(dS, dE, dI, dR, dD)))
    }
  )
}

spreadSEIRDV <- function(time, state, parameters) {
  with(
    as.list(c(state,parameters)), {
      dS <- -(beta*S*I/N) - sigma*N
      dE <- (beta*S*I/N) - delta*E
      dI <- delta*E - gamma*I
      dR <- (1-mu)*gamma*I + sigma*N
      dD <- mu*gamma*I
      return (list(c(dS, dE, dI, dR, dD)))
    }
  )
}

different <- function(t, x, y) {
  if (t == "beta") {
    return(abs(x-y)>2)
  }
  if (t == "gamma") {
    return(abs(x-y)>0.25)
  }
  if (t == "delta") {
    return(abs(x-y)>0.5)
  }
  return(abs(x-y)>0.05)
}

gridSearchEstimation <- function() {
  betaRange <- seq(0, 5, by=0.05)
  gammaRange <- seq(0, 1, by=0.05)
  deltaRange <- seq(0, 2, by=0.05)
  muRange <- seq(0, 0.1, by=0.005)
  
  n = length(inf$DATA)
  N = 7500000

  compartments3 <- data.frame(rep(0,n), rep(0,n), rep(0,n), rep(0,n), rep(0,n))
  colnames(compartments3) <- c("S", "E", "I", "R", "D")
  compartments3[1, "I"] = 1
  compartments3[1, "S"] = N
  
  params3 <- data.frame(rep(0,n), rep(0,n), rep(0,n), rep(0,n))
  colnames(params3) <- c("beta", "gamma", "delta", "mu")
  params3[1, "beta"] = 4.5
  params3[1, "gamma"] = 1.0
  params3[1, "delta"] = 0.15
  params3[1, "mu"] = 0.01
    
  for (day in c(1:(n-1))) {
    print("****************")
    print("ENTRA EN EL DIA:")
    print(day)
    sub_ini <- c(S=compartments3[day, "S"],E=compartments3[day, "E"],I=dataset_reals$infectats_totals[day],R=compartments3[day, "R"],D=dataset_reals$morts[day])
    sub_params <- c(beta=params3$beta[day], gamma=params3$gamma[day], delta=params3$delta[day], mu=params3$mu[day], N=N)
    out <- ode(y=sub_ini, times=seq(from=day, to=day+1, by=1), func=spreadSEIRD, parms=sub_params)
    out.df = as.data.frame(out)
    compartments3[day+1, "S"] = out.df$S[2]
    compartments3[day+1, "E"] = out.df$E[2]
    compartments3[day+1, "I"] = out.df$I[2]
    compartments3[day+1, "R"] = out.df$R[2]
    compartments3[day+1, "D"] = out.df$D[2]
    min_err <- 10E20
    for (beta in betaRange) {
      if (different("beta", beta, params[day, "beta"])) {
        next
      }
      for (gamma in gammaRange) {
        if (different("gamma", gamma, params[day, "gamma"])) {
          next
        }
        for (delta in deltaRange) {
          if (different("delta", delta, params[day, "delta"])) {
            next
          }
          for (mu in muRange) {
            if (different("mu", mu, params[day, "mu"])) {
              next
            }
            sub_ini <- c(S=compartments3[day, "S"],E=compartments3[day, "E"],I=dataset_reals$infectats_totals[day],R=compartments3[day, "R"],D=dataset_reals$morts[day])
            sub_params <- c(beta=beta, gamma=gamma, delta=delta, mu=mu, N=N)
            out <- ode(y=sub_ini, times=seq(from=day, to=day+1, by=1), func=spreadSEIRD, parms=sub_params)
            out.df = as.data.frame(out)
            err <- getNormalizedError(out.df$I[2], out.df$D[2], dataset_reals$infectats_totals[day+1], dataset_reals$morts[day+1])
            if (err < min_err) {
              params3[day+1, "beta"] = beta
              params3[day+1, "gamma"] = gamma
              params3[day+1, "delta"] = delta
              params3[day+1, "mu"] = mu
              min_err <- err
            }
          }
        }
      }
    }
    print("****************")
  }
  
  formated_date = as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
  dataset_params_GS <- data.frame(DATA = formated_date, beta = params3$beta, gamma = params3$gamma, delta = params3$delta, mu = params3$mu)
  
  ts.plot(ts(dataset_reals$infectats_totals[1:n]), ts(compartments3$I[1:n]), col=c(2,4))
  legend("topleft", legend=c("Dades reals", "Dades predites"),col=c(2,4), lty=c(1,1), cex=1.2)
  
  err_abs = abs(dataset_reals$infectats_totals[1:n]-compartments3$I[1:n])
  mean_err_abs = mean(err_abs)
  err_q = (dataset_reals$infectats_totals[1:n]-compartments3$I[1:n])^2
  mean_err_q = mean(err_q)
  epam = abs(dataset_reals$infectats_totals[1:n]-compartments3$I[1:n])/dataset_reals$infectats_totals[1:n]
  mean_epam = mean(epam)
  epam_perc = abs(dataset_reals$infectats_totals[1:n]-compartments3$I[1:n])/dataset_reals$infectats_totals[1:n]*100
  mean_epam_perc = mean(epam_perc)
  
  ts.plot(ts(dataset_reals$morts[1:n]), ts(compartments3$D[1:n]), col=c(2,4))
  legend("topleft", legend=c("Dades reals", "Dades predites"),col=c(2,4), lty=c(1,1), cex=1.2)
  
  err_abs2 = abs(dataset_reals$morts[1:n]-compartments3$D[1:n])
  mean_err_abs2 = mean(err_abs2)
  err_q2 = (dataset_reals$morts[1:n]-compartments3$D[1:n])^2
  mean_err_q2 = mean(err_q2)
  epam2 = abs(dataset_reals$morts[1:n]-compartments3$D[1:n])/(dataset_reals$morts[1:n]+1)
  mean_epam2 = mean(epam2)
  morts_repl = replace(dataset_reals$morts, dataset_reals$morts==0, 1)
  epam_perc2 = abs(dataset_reals$morts[1:n]-compartments3$D[1:n])/(morts_repl)*100
  mean_epam_perc2 = mean(epam_perc2) 
  
  incr <- 14
  decr <- 6
  time <- as.Date(inf$DATA[t:(t+incr)], tryFormats=c("%d/%m/%Y"))
  seq <- seq(from=t, to=t+incr, by=1)
  parameters <- c(beta=dataset_params_GS$beta[t],gamma=dataset_params_GS$gamma[t],delta=dataset_params_GS$delta[t],mu=dataset_params_GS$mu[t],N=7500001)
  state <- c(S=compartments3$S[t],E=compartments3$E[t],I=dataset_reals$infectats_totals[t],R=compartments3$R[t],D=dataset_reals$morts[t])
  out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
  dataset <- as.data.frame(out)
}

getNormalizedError <- function(inf_pred, death_pred, inf_real, death_real) {
  error_death <- abs(death_pred-death_real)
  error_inf <- abs(inf_pred-inf_real)*0.005121445 
  error <- 1/2*((error_death^2)+(error_inf^2))
  return(error)
}

header <- dashboardHeader(
  title = "Modelitzant la COVID-19",
  titleWidth = 300
)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Dades reals a Catalunya", tabName = "REAL", icon = icon("angle-right"))
  ),
  sidebarMenu(
    menuItem("Estimació dels paràmetres", tabName = "estimation", icon = icon("angle-right"))
  ),
  sidebarMenu(
    menuItem("Model SIR", tabName = "SIR", icon = icon("angle-right"))
  ),
  sidebarMenu(
    menuItem("Model SEIR", tabName = "SEIR", icon = icon("angle-right"))
  ),
  sidebarMenu(
    menuItem("Model SEIRD", tabName = "SEIRD", icon = icon("angle-right"))
  ),
  sidebarMenu(
    menuItem("Model SEIRD + Vacunats", tabName = "SEIRDV", icon = icon("angle-right"))
  ),
  sidebarMenu(
    menuItem("Model SEIRD + Cerca en quadrícula", tabName = "grid_search", icon = icon("angle-right"))
  ),
  sidebarMenu(
    menuItem("Correcions SARIMA", tabName = "SARIMA", icon = icon("angle-right"))
  )
)

body <- dashboardBody(
  tags$style(HTML("
  .box.box-solid.box-primary>.box-header {
    color:#fff;
      background:#222d32;
  }
  .box.box-solid.box-primary{
    border-bottom-color:#222d32;;
    border-left-color:#222d32;;
    border-right-color:#222d32;;
    border-top-color:#222d32;;
  }
  ")),
  tabItems(
    tabItem(tabName = "REAL",
            h2("Dades reals a Catalunya"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            HTML("<em>Dades extretes de la pàgina web: <b>Dades Obertes</b> de la Generalitat (http://governobert.gencat.cat/ca/dades_obertes/) </em>"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Casos nous comptabilitzats d'individus infectats per la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_I"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Mitjanes de 7 dies dels casos comptabilitzats d'individus infectats per la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_I_Mitjana"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Casos comptabilitzats acumulats d'individus infectats per la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_I_Acumulats"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació dels casos totals d'individus infectats per la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_I_Totals"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Casos nous comptabilitzats d'individus morts per la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_M"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Casos comptabilitzats acumulats d'individus morts per la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_M_Acumulats"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Individus vacunats contra la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_Vacunats"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Individus acumulats vacunats contra la COVID-19 a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("Reals_Vacunats_Acumulats"))
              )
            )
          ),
    tabItem(tabName = "estimation",
            h2("Estimació dels paràmetres"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació del nombre de reproducció efectiu a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_rt"))
              )
            ),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la taxa de transmissió a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_beta"))
              )
            ),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la taxa de retir a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_gamma"))
              )
            ),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la raó de mortalitat a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_mu"))
              )
            ),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la raó de vacunació a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_sigma"))
              )
            )
    ),
    tabItem(tabName = "SIR",
            h2("Model SIR"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            withMathJax(paste0("Condicions inicials emprades: $$S_0 = 7.500.000, I_0 = 1, R_0 = 0, t_0 = 01/03/2020$$")),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre d'infectats a Catalunya amb la resolució exacta de les equacions per estats inicials (suposant S ~ constant):"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 4,
                        sliderInput(inputId = "timeSIR_Comp_Sct", label = div(style="font-size:1rem","Període de dies"), value = 5, min = 1, max = 50),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "betaSIR_Comp_Sct", label = div(style="font-size:1rem","Taxa de transmissió"), value = 1, min = 0, max = 5, step = 0.05),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "gammaSIR_Comp_Sct", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                      ),
                    ),
                    plotOutput("SIR_I_Sct_Comp"))
              ),
            ),
            fluidRow(
              column (
                width = 12,
                  box(width=12, title=div(style="font-size: 1.5rem","Resolució numèrica de les equacions diferencials:"),
                      solidHeader = TRUE, status = "primary",
                  fluidRow(
                    column (
                      width = 12,
                      column (
                        width = 4,
                        sliderInput(inputId = "timeSIR", label = div(style="font-size:1rem","Període de dies"), value = 20, min = 1, max = 50),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "betaSIR", label = div(style="font-size:1rem","Taxa de transmissió"), value = 2, min = 0, max = 5, step = 0.05),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "gammaSIR", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                      )
                    ),
                  ),
                  plotOutput("SIR"))
              ),
          ),
          fluidRow(
            column (
              width = 12,
              box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre d'infectats a Catalunya amb les dades simulades numèricament pel model SIR:"),
              solidHeader = TRUE, status = "primary",
              fluidRow(
                column (
                  width = 4,
                  sliderInput(inputId = "timeSIR_Comp", label = div(style="font-size:1rem","Període de dies"), value = 5, min = 1, max = 50),
                ),
                column (
                  width = 4,
                  sliderInput(inputId = "betaSIR_Comp", label = div(style="font-size:1rem","Taxa de transmissió"), value = 1, min = 0, max = 5, step = 0.05),
                ),
                column (
                  width = 4,
                  sliderInput(inputId = "gammaSIR_Comp", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                ),
              ),
              plotOutput("SIR_I_Comp"))
            ),
          )
    ),
    tabItem(tabName = "SEIR",
            h2("Model SEIR"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            withMathJax(paste0("Condicions inicials emprades: $$S_0 = 7.500.000, E_0 = 0, I_0 = 1, R_0 = 0, t_0 = 01/03/2020$$")),
            fluidRow(
              column (
                width = 12,
                box(width=12, title=div(style="font-size: 1.5rem","Resolució numèrica de les equacions diferencials:"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 12,
                        column (
                          width = 3,
                          sliderInput(inputId = "timeSEIR", label = div(style="font-size:1rem","Període de dies"), value = 35, min = 1, max = 50),
                        ),
                        column (
                          width = 3,
                          sliderInput(inputId = "betaSEIR", label = div(style="font-size:1rem","Taxa de transmissió"), value = 3, min = 0, max = 5, step = 0.05),
                        ),
                        column (
                          width = 3,
                          sliderInput(inputId = "deltaSEIR", label = div(style="font-size:1rem","Taxa de latència"), value = 0.5, min = 0, max = 2, step = 0.01),
                        ),
                        column (
                          width = 3,
                          sliderInput(inputId = "gammaSEIR", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                        )
                      ),
                    ),
                    plotOutput("SEIR"))
                )
              ),
              fluidRow(
                column (
                  width = 12,
                  box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre d'infectats a Catalunya amb les dades simulades numèricament pel model SEIR:"),
                      solidHeader = TRUE, status = "primary",
                      fluidRow(
                        column (
                          width = 3,
                          sliderInput(inputId = "timeSEIR_Comp", label = div(style="font-size:1rem","Període de dies"), value = 5, min = 1, max = 50),
                        ),
                        column (
                          width = 3,
                          sliderInput(inputId = "betaSEIR_Comp", label = div(style="font-size:1rem","Taxa de transmissió"), value = 2, min = 0, max = 5, step = 0.05),
                        ),
                        column (
                          width = 3,
                          sliderInput(inputId = "deltaSEIR_Comp", label = div(style="font-size:1rem","Taxa de latència"), value = 1.0, min = 0, max = 2, step = 0.01),
                        ),
                        column (
                          width = 3,
                          sliderInput(inputId = "gammaSEIR_Comp", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                        ),
                      ),
                      plotOutput("SEIR_I_Comp"))
                ),
              ),
              withMathJax(paste0("Paràmetres i últim dia simulació escollits: $$ \\beta = 1.68, \\delta = 0.33, \\gamma = 0.20, t_f = 01/06/2020$$")),
              fluidRow(
                column (
                  width = 12,
                  box(width=12, title=div(style="font-size: 1.5rem","Evolució de la pandèmia segons les mesures de contenció"),
                      solidHeader = TRUE, status = "primary",
                      fluidRow(
                        column (
                          width = 12,
                          column (
                            width = 4,
                            sliderInput(inputId = "timeSEIR_mesures", label = div(style="font-size:1rem","Primer dia mesures"), value = 30, min = 1, max = 40),
                          ),
                          column (
                            width = 4,
                            sliderInput(inputId = "betaSEIR_mesures", label = div(style="font-size:1rem","Taxa de transmissió"), value = 1, min = 0, max = 1.68, step = 0.05),
                          )
                        ),
                      ),
                      plotOutput("SEIR_Mesures_Contencio"))
                  ),
              )
      ),
    tabItem(tabName = "SEIRD",
            h2("Model SEIRD"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            withMathJax(paste0("Condicions inicials emprades: $$S_0 = 7.500.000, E_0 = 0, I_0 = 1, R_0 = 0, D_0 = 0, t_0 = 01/03/2020$$")),
            fluidRow(
              column (
                width = 12,
                box(width=12, title=div(style="font-size: 1.5rem","Resolució numèrica de les equacions diferencials:"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "timeSEIRD", label = div(style="font-size:1rem","Període de dies"), value = 35, min = 1, max = 50),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "betaSEIRD", label = div(style="font-size:1rem","Taxa de transmissió"), value = 3, min = 0, max = 5, step = 0.05),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "deltaSEIRD", label = div(style="font-size:1rem","Taxa de latència"), value = 0.5, min = 0, max = 2, step = 0.01),
                        )
                      ),
                      column(
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "gammaSEIRD", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "muSEIRD", label = div(style="font-size:1rem","Raó de mortalitat"), value = 0.1, min = 0, max = 1, step = 0.01),
                        )
                      )
                    ),
                    plotOutput("SEIRD"))
                )
            ),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre de morts a Catalunya amb les dades simulades numèricament pel model SEIRD:"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "timeSEIRD_Comp", label = div(style="font-size:1rem","Període de dies"), value = 35, min = 1, max = 50),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "betaSEIRD_Comp", label = div(style="font-size:1rem","Taxa de transmissió"), value = 1, min = 0, max = 5, step = 0.05),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "deltaSEIRD_Comp", label = div(style="font-size:1rem","Taxa de latència"), value = 0.25, min = 0, max = 2, step = 0.01),
                        )
                      ),
                      column(
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "gammaSEIRD_Comp", label = div(style="font-size:1rem","Taxa de retir"), value = 0.15, min = 0, max = 1, step = 0.01),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "muSEIRD_Comp", label = div(style="font-size:1rem","Raó de mortalitat"), value = 0.05, min = 0, max = 1, step = 0.01),
                        )
                      )
                    ),
                    plotOutput("SEIRD_D_Comp"))
                )
            ),
          fluidRow(
            column (
              width = 12,
              box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre d'infectats i morts a Catalunya amb les dades simulades numèricament pel model SEIRD:"),
                  solidHeader = TRUE, status = "primary",
                  fluidRow(
                    column (
                      width = 12,
                      column (
                        width = 4,
                        sliderInput(inputId = "timeSEIRD_Comp2", label = div(style="font-size:1rem","Període de dies"), value = 15, min = 1, max = 50),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "betaSEIRD_Comp2", label = div(style="font-size:1rem","Taxa de transmissió"), value = 2, min = 0, max = 5, step = 0.05),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "deltaSEIRD_Comp2", label = div(style="font-size:1rem","Taxa de latència"), value = 0.35, min = 0, max = 2, step = 0.01),
                      )
                    ),
                    column(
                      width = 12,
                      column (
                        width = 4,
                        sliderInput(inputId = "gammaSEIRD_Comp2", label = div(style="font-size:1rem","Taxa de retir"), value = 0.15, min = 0, max = 1, step = 0.01),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "muSEIRD_Comp2", label = div(style="font-size:1rem","Raó de mortalitat"), value = 0.1, min = 0, max = 1, step = 0.01),
                      )
                    )
                  ),
                  plotOutput("SEIRD_Comp"))
              )
            )
        ),
    tabItem(tabName = "SEIRDV",
            h2("Model SEIRD + Vacunats"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            withMathJax(paste0("Condicions inicials emprades: $$S_0 = 7.500.000, E_0 = 0, I_0 = 1, R_0 = 0, D_0 = 0, t_0 = 01/03/2020$$")),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Resolució numèrica de les equacions diferencials afegint els individus vacunats:"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "timeSEIRDV", label = div(style="font-size:1rem","Període de dies"), value = 35, min = 1, max = 50),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "betaSEIRDV", label = div(style="font-size:1rem","Taxa de transmissió"), value = 3, min = 0, max = 5, step = 0.05),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "deltaSEIRDV", label = div(style="font-size:1rem","Taxa de latència"), value = 0.5, min = 0, max = 2, step = 0.01),
                        )
                      ),
                      column(
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "gammaSEIRDV", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "muSEIRDV", label = div(style="font-size:1rem","Raó de mortalitat"), value = 0.1, min = 0, max = 1, step = 0.01),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "sigmaSEIRDV", label = div(style="font-size:1rem","Raó de vacunació"), value = 0.01, min = 0, max = 0.05, step = 0.00001),
                        )
                      )
                    ),
                    plotOutput("SEIRDV"))
              )
            ),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            withMathJax(paste0("Condicions inicials emprades: $$S_0 = 7.133.385, E_0 = 17.118, I_0 = 18.760, R_0 = 313.619, D_0 = 17.118, t_0 = 27/12/2020$$")),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Resolució numèrica de les equacions diferencials afegint els individus vacunats:"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "timeSEIRDV_Comp", label = div(style="font-size:1rem","Període de dies"), value = 35, min = 1, max = 50),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "betaSEIRDV_Comp", label = div(style="font-size:1rem","Taxa de transmissió"), value = 3, min = 0, max = 5, step = 0.05),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "deltaSEIRDV_Comp", label = div(style="font-size:1rem","Taxa de latència"), value = 0.5, min = 0, max = 2, step = 0.01),
                        )
                      ),
                      column(
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "gammaSEIRDV_Comp", label = div(style="font-size:1rem","Taxa de retir"), value = 0.1, min = 0, max = 1, step = 0.01),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "muSEIRDV_Comp", label = div(style="font-size:1rem","Raó de mortalitat"), value = 0.1, min = 0, max = 1, step = 0.01),
                        ),
                        column (
                          width = 4,
                          sliderInput(inputId = "sigmaSEIRDV_Comp", label = div(style="font-size:1rem","Raó de vacunació"), value = 0.01, min = 0, max = 0.05, step = 0.00001),
                        )
                      )
                    ),
                    plotOutput("SEIRDV_Comp"))
              )
          ),
          fluidRow(
            column (
              sytle = "margin:5px",
              width = 12,
              box(width = 12, title=div(style="font-size: 1.5rem","Simulació nombre d'infectats a Catalunya segons diverses raons de vacunació:"),
                  solidHeader = TRUE, status = "primary",
                  fluidRow(
                    style="padding-left: 1.5rem",
                    "Paràmetres emprats:",
                  ),
                  fluidRow(
                    align = "center",
                    HTML("<b>Taxa de transmissió</b> = 2.0, <b>Taxa de latència</b> = 0.33, <b>Taxa de retir</b> = 0.2, <b>Raó de mortalitat</b> = 0.01"),
                  ),
                  plotOutput("SEIRDV_Inf"))
              )
          ),
          fluidRow(
            column (
              width = 12,
              box(width = 12, title=div(style="font-size: 1.5rem","Simulació nombre de morts a Catalunya segons diverses raons de vacunació:"),
                  solidHeader = TRUE, status = "primary",
                  fluidRow(
                    style="padding-left: 1.5rem",
                    "Paràmetres emprats:",
                  ),
                  fluidRow(
                    align = "center",
                    HTML("<b>Taxa de transmissió</b> = 2.0, <b>Taxa de latència</b> = 0.33, <b>Taxa de retir</b> = 0.2, <b>Raó de mortalitat</b> = 0.01"),
                  ),
                  plotOutput("SEIRDV_Morts"))
            )
          ),
          fluidRow(
            column (
              width = 12,
              box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre de vacunats reals a Catalunya amb els simulats segons la raó de vacunació:"),
                  solidHeader = TRUE, status = "primary",
                  fluidRow(
                    column (
                      width = 12,
                      column (
                        width = 4,
                        sliderInput(inputId = "timeSEIRDV_Vac", label = div(style="font-size:1rem","Període de dies"), value = 35, min = 2, max = 120),
                      ),
                      column (
                        width = 4,
                        sliderInput(inputId = "sigmaSEIRDV_Vac", label = div(style="font-size:1rem","Raó de vacunació"), value = 0.0003, min = 0, max = 0.01, step = 0.00001),
                      )
                    ),
                  ),
                  plotOutput("SEIRDV_Vac"))
            )
          )
        ),
    tabItem(tabName = "grid_search",
            h2("Model SEIRD + Cerca en quadrícula"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació del nombre d'infectats diaris a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_inf_GS"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació del nombre de morts diaris a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_morts_GS"))
              ),
            ),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació del nombre de susceptibles diaris a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("susceptibles_GS"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació del nombre d'exposats diaris a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("exposats_GS"))
              )
              ,column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació del nombre de retirats diaris a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("retirats_GS"))
              ),
            ),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la taxa de transmissió a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_beta_GS"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la taxa de latència a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_delta_GS"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la taxa de retir a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_gamma_GS"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimació de la raó de mortalitat a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("estimation_mu_GS"))
              )
            ),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre d'infectats reals a Catalunya amb les prediccions extra-mostrals:"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "timeGS_EM_Infectats", label = div(style="font-size:1rem","Primer dia prediccions extra-mostrals"), value = 18, min = 7, max = 430),
                        ),
                      ),
                    ),
                    plotOutput("GS_EM_Infectats"))
              )
            ),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Comparativa del nombre de morts reals a Catalunya amb les prediccions extra-mostrals:"),
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column (
                        width = 12,
                        column (
                          width = 4,
                          sliderInput(inputId = "timeGS_EM_Morts", label = div(style="font-size:1rem","Primer dia prediccions extra-mostrals"), value = 18, min = 7, max = 430),
                        ),
                      ),
                    ),
                    plotOutput("GS_EM_Morts"))
              )
            )
        ),
    tabItem(tabName = "SARIMA",
            h2("Correccions SARIMA"),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Errors comesos en les estimacions del nombre d'infectats a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("errors_infectats"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Errors comesos en les estimacions del nombre de morts a Catalunya:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("errors_morts"))
              )
            ),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            withMathJax(paste0("Per a les següents gràfiques, interval de temps que suposem com a mostral i quantitat de prediccions extra-mostrals realitzades: $$t_0 = 01/03/2020, t_f = 09/07/2020, n_{prediccions} = 14$$")),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimacions extra-mostrals dels errors comesos en les prediccions del nombre d'infectats (amb el model ARMA(2,1):"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("err_inf"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Estimacions extra-mostrals dels errors comesos en les prediccions del nombre de morts (amb el model SARIMA(2,0,3)x(2,0,0)):"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("err_morts"))
              )
            ),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Errors reals registrats i estimacions extra-mostrals dels errors comesos en les prediccions del nombre d'infectats:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("err_inf_mostr_extra"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Errors reals registrats i estimacions extra-mostrals dels errors comesos en les prediccions del nombre de morts:"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("err_morts_mostr_extra"))
              )
            ),
            HTML("<hr style='border-top: solid 0.1rem black'>"),
            fluidRow(
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Comparativa d'infectats estimats sense i amb les correccions obtingudes amb el model ARMA(2,1):"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("inf_corregits"))
              ),
              column (
                width = 12,
                box(width = 12, title=div(style="font-size: 1.5rem","Comparativa de morts estimats sense i amb les correccions obtingudes amb el model SARIMA(2,0,3)x(2,0,0):"),
                    solidHeader = TRUE, status = "primary",
                    plotOutput("morts_corregits"))
              )
            ),
        )
    )
)


ui <- dashboardPage(skin = "black", header, sidebar, body)
