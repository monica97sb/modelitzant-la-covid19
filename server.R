server <- function(input, output) {
  output$Reals_I <- renderPlot({
    ggplot(dataset_reals, aes(x = DATA, y = infectats, group = 1)) + geom_line() +
    scale_x_date(date_labels="%d/%m/%Y") + labs(x = "Període de dies", y = "Nombre d'infectats")
  })
  output$Reals_I_Mitjana <- renderPlot({
    ggplot(dataset_reals, aes(x = DATA, y = inf_mitja_7dies, group = 1)) + geom_line() +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[12]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[60]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[210]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[270]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[280]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[360]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[370]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[435]), color="blue", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[29]), color="red", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[241]), color="red", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[320]), color="red", linetype = "dashed") +
    #geom_vline(xintercept=as.numeric(dataset_reals$DATA[409]), color="red", linetype = "dashed") +
      labs(x = "Període de dies", y = "Nombre d'infectats") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$Reals_I_Acumulats <- renderPlot({
    ggplot(dataset_reals, aes(x = DATA, y = infectats_acum, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Nombre d'infectats") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$Reals_I_Totals <- renderPlot({
    ggplot(dataset_reals, aes(x = DATA, y = infectats_totals, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Nombre d'infectats") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$Reals_M <- renderPlot({
    ggplot(dataset_reals, aes(x = DATA, y = morts, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Nombre de morts") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$Reals_M_Acumulats <- renderPlot({
    ggplot(dataset_reals, aes(x = DATA, y = morts_acum, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Nombre de morts") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$Reals_Vacunats <- renderPlot({
    ggplot(dataset_vacunats, aes(x = DATA, y = vacunats, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Nombre de vacunats") + scale_x_date(date_labels="%d/%m/%Y")
  }) 
  output$Reals_Vacunats_Acumulats <- renderPlot({
    ggplot(dataset_vacunats, aes(x = DATA, y = vacunats_acum, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Nombre de vacunats") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_gamma <- renderPlot({
    date <- as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
    n = length(date)
    N = replace(dataset_reals$inf_mitja_7dies, dataset_reals$inf_mitja_7dies==0, 1)
    Rt <- rep(0, n)
    for (i in 8:n) {
      Rt[i] = (N[i]+N[i-1]+N[i-2])/(N[i-5]+N[i-6]+N[i-7])
    }
    S <- rep(0, n)
    for (i in 1:n) {
      S[i] = 7500000 - dataset_reals$infectats_acum[i]
    }
    estimation_beta <- (7500000*dataset_reals$inf_mitja_7dies)/(S*dataset_reals$infectats_totals)
    estimation_beta_mean_7dies <- rep(0, n)
    estimation_beta_mean_7dies[1] = estimation_beta[1]
    estimation_beta_mean_7dies[2] = 1/2*(estimation_beta[1]+estimation_beta[2])
    estimation_beta_mean_7dies[3] = 1/3*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3])
    estimation_beta_mean_7dies[4] = 1/4*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4])
    estimation_beta_mean_7dies[5] = 1/5*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4]+estimation_beta[5])
    estimation_beta_mean_7dies[6] = 1/6*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4]+estimation_beta[5]+estimation_beta[6])
    estimation_beta_mean_7dies[7] = 1/7*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4]+estimation_beta[5]+estimation_beta[6]+estimation_beta[7])
    for (i in 8:n) {
      estimation_beta_mean_7dies[i] = 1/7*(estimation_beta[i-1]+estimation_beta[i-2]+estimation_beta[i-3]+estimation_beta[i-4]+
                                      estimation_beta[i-5]+estimation_beta[i-6]+estimation_beta[i-7])
    }
    estimation_gamma <- Rt*estimation_beta_mean_7dies
    df <- data.frame(date[25:n], estimation_gamma[25:n])
    ggplot(df, aes(x = date[25:n], y = estimation_gamma[25:n], group = 1)) + geom_line() + 
      labs(x = "Període de dies", y = "Estimació la taxa de retir") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_beta <- renderPlot({
    date <- as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
    n = length(date)
    S <- rep(0, n)
    for (i in 1:n) {
      S[i] = 7500000 - dataset_reals$infectats_acum[i]
    }
    estimation <- (7500000*dataset_reals$inf_mitja_7dies)/(S*dataset_reals$infectats_totals)
    estimation_mean_7dies <- rep(0, n)
    estimation_mean_7dies[1] = estimation[1]
    estimation_mean_7dies[2] = 1/2*(estimation[1]+estimation[2])
    estimation_mean_7dies[3] = 1/3*(estimation[1]+estimation[2]+estimation[3])
    estimation_mean_7dies[4] = 1/4*(estimation[1]+estimation[2]+estimation[3]+estimation[4])
    estimation_mean_7dies[5] = 1/5*(estimation[1]+estimation[2]+estimation[3]+estimation[4]+estimation[5])
    estimation_mean_7dies[6] = 1/6*(estimation[1]+estimation[2]+estimation[3]+estimation[4]+estimation[5]+estimation[6])
    estimation_mean_7dies[7] = 1/7*(estimation[1]+estimation[2]+estimation[3]+estimation[4]+estimation[5]+estimation[6]+estimation[7])
    for (i in 8:n) {
      estimation_mean_7dies[i] = 1/7*(estimation[i-1]+estimation[i-2]+estimation[i-3]+estimation[i-4]+
                                        estimation[i-5]+estimation[i-6]+estimation[i-7])
    }
    df <- data.frame(date[25:n], estimation_mean_7dies[25:n])
    ggplot(df, aes(x = date[25:n], y = estimation_mean_7dies[25:n], group = 1)) + geom_line() + 
      labs(x = "Període de dies", y = "Estimació taxa de transmissió") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_rt <- renderPlot({
    date <- as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
    n = length(date)
    N = replace(dataset_reals$inf_mitja_7dies, dataset_reals$inf_mitja_7dies==0, 1)
    Rt <- rep(0, n)
    for (i in 8:n) {
      Rt[i] = (N[i]+N[i-1]+N[i-2])/(N[i-5]+N[i-6]+N[i-7])
    }
    df <- data.frame(date[25:n], Rt[25:n])
    ggplot(df, aes(x = date[25:n], y = Rt[25:n], group = 1)) + geom_line() + 
      geom_hline(yintercept=1, color="red", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[12]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[60]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[210]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[270]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[280]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[360]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[370]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[435]), color="blue", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[29]), color="red", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[241]), color="red", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[320]), color="red", linetype = "dashed") +
      #geom_vline(xintercept=as.numeric(dataset_reals$DATA[409]), color="red", linetype = "dashed") +
      labs(x = "Període de dies", y = "Estimació nombre de reproducció efectiu") +
      scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_mu <- renderPlot({
    date <- as.Date(inf$DATA, tryFormats = c("%d/%m/%Y"))
    n = length(date)
    N = replace(dataset_reals$inf_mitja_7dies, dataset_reals$inf_mitja_7dies==0, 1)
    Rt <- rep(0, n)
    for (i in 8:n) {
      Rt[i] = (N[i]+N[i-1]+N[i-2])/(N[i-5]+N[i-6]+N[i-7])
    }
    S <- rep(0, n)
    for (i in 1:n) {
      S[i] = 7500000 - dataset_reals$infectats_acum[i]
    }
    estimation_beta <- (7500000*dataset_reals$inf_mitja_7dies)/(S*dataset_reals$infectats_totals)
    estimation_beta_mean_7dies <- rep(0, n)
    estimation_beta_mean_7dies[1] = estimation_beta[1]
    estimation_beta_mean_7dies[2] = 1/2*(estimation_beta[1]+estimation_beta[2])
    estimation_beta_mean_7dies[3] = 1/3*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3])
    estimation_beta_mean_7dies[4] = 1/4*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4])
    estimation_beta_mean_7dies[5] = 1/5*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4]+estimation_beta[5])
    estimation_beta_mean_7dies[6] = 1/6*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4]+estimation_beta[5]+estimation_beta[6])
    estimation_beta_mean_7dies[7] = 1/7*(estimation_beta[1]+estimation_beta[2]+estimation_beta[3]+estimation_beta[4]+estimation_beta[5]+estimation_beta[6]+estimation_beta[7])
    for (i in 8:n) {
      estimation_beta_mean_7dies[i] = 1/7*(estimation_beta[i-1]+estimation_beta[i-2]+estimation_beta[i-3]+estimation_beta[i-4]+
                                             estimation_beta[i-5]+estimation_beta[i-6]+estimation_beta[i-7])
    }
    estimation_gamma <- Rt*estimation_beta_mean_7dies
    estimation_mu <- dataset_reals$morts*(1/estimation_gamma)*(1/dataset_reals$infectats_totals)
    df <- data.frame(date[25:n], estimation_mu[25:n])
    ggplot(df, aes(x = date[25:n], y = estimation_mu[25:n], group = 1)) + geom_line() + 
      labs(x = "Període de dies", y = "Estimació la raó de mortalitat") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_sigma <- renderPlot({ 
    N = 7500000
    date <- as.Date(vacunats$DATA, tryFormats = c("%d/%m/%Y"))
    estimation_sigma <- dataset_vacunats$vacunats/N
    estimation_sigma_mean_7dies <- rep(0, length(date))
    estimation_sigma_mean_7dies[1] = estimation_sigma[1]
    estimation_sigma_mean_7dies[2] = 1/2*(estimation_sigma[1]+estimation_sigma[2])
    estimation_sigma_mean_7dies[3] = 1/3*(estimation_sigma[1]+estimation_sigma[2]+estimation_sigma[3])
    estimation_sigma_mean_7dies[4] = 1/4*(estimation_sigma[1]+estimation_sigma[2]+estimation_sigma[3]+estimation_sigma[4])
    estimation_sigma_mean_7dies[5] = 1/5*(estimation_sigma[1]+estimation_sigma[2]+estimation_sigma[3]+estimation_sigma[4]+estimation_sigma[5])
    estimation_sigma_mean_7dies[6] = 1/6*(estimation_sigma[1]+estimation_sigma[2]+estimation_sigma[3]+estimation_sigma[4]+estimation_sigma[5]+estimation_sigma[6])
    estimation_sigma_mean_7dies[7] = 1/7*(estimation_sigma[1]+estimation_sigma[2]+estimation_sigma[3]+estimation_sigma[4]+estimation_sigma[5]+estimation_sigma[6]+estimation_sigma[7])
    for (i in 8:length(date)) {
      estimation_sigma_mean_7dies[i] = 1/7*(estimation_sigma[i-1]+estimation_sigma[i-2]+estimation_sigma[i-3]+estimation_sigma[i-4]+
                                              estimation_sigma[i-5]+estimation_sigma[i-6]+estimation_sigma[i-7])
    }
    df <- data.frame(date, estimation_sigma_mean_7dies)
    ggplot(df, aes(x = date, y = estimation_sigma_mean_7dies, group = 1)) + geom_line() + 
      labs(x = "Període de dies", y = "Estimació la raó de vacunació") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SIR_I_Sct_Comp <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSIR_Comp_Sct+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSIR_Comp_Sct, by=1)
    I <- exp(((input$betaSIR_Comp_Sct*7500000)/7500001-input$gammaSIR_Comp_Sct)*seq)
    dataset <- data.frame(time=time, I=I)
    dataset2 <- dataset_reals[1:(input$timeSIR_Comp_Sct+1),]
    ggplot() + geom_line(data = dataset2, aes(x = time, y = infectats_totals, color = "blue")) +
    geom_line(data = dataset, aes(x = time, y = I, color = "red")) +
    labs(x = "Període de dies", y = "Nombre d'infectats") +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_colour_discrete(labels = c("Reals", "Aproximació SIR (S ~ constant)")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SIR <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSIR+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSIR, by=1)
    parameters <- c(beta=input$betaSIR,gamma=input$gammaSIR,N=7500001)
    state <- c(S=7500000,I=1,R=0)
    out <- ode(y=state, times=seq, func=spreadSIR, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    ggplot() + geom_line(data = dataset, aes(x = time, y = S, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "green")) +
      geom_line(data = dataset, aes(x = time, y = R, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'individus") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Susceptibles", "Infectats", "Retirats"),
                          values = c("red", "blue", "green")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SIR_I_Comp <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSIR_Comp+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSIR_Comp, by=1)
    parameters <- c(beta=input$betaSIR_Comp,gamma=input$gammaSIR_Comp,N=7500001)
    state <- c(S=7500000,I=1,R=0)
    out <- ode(y=state, times=seq, func=spreadSIR, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    dataset2 <- dataset_reals[1:(input$timeSIR_Comp+1),]
    ggplot() + geom_line(data = dataset2, aes(x = time, y = infectats_totals, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_discrete(labels = c("Reals", "SIR")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIR <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIR+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIR, by=1)
    parameters <- c(beta=input$betaSEIR,delta=input$deltaSEIR,gamma=input$gammaSEIR,N=7500001)
    state <- c(S=7500000,E=0,I=1,R=0)
    out <- ode(y=state, times=seq, func=spreadSEIR, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    ggplot() + geom_line(data = dataset, aes(x = time, y = S, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = E, color = "orange")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "green")) +
      geom_line(data = dataset, aes(x = time, y = R, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'individus") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Susceptibles", "Exposats", "Infectats", "Retirats"),
                          values = c("red", "orange", "blue", "green")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIR_I_Comp <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIR_Comp+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIR_Comp, by=1)
    parameters <- c(beta=input$betaSEIR_Comp,gamma=input$gammaSEIR_Comp,delta=input$deltaSEIR_Comp,N=7500001)
    state <- c(S=7500000,E=0,I=1,R=0)
    out <- ode(y=state, times=seq, func=spreadSEIR, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    dataset2 <- dataset_reals[1:(input$timeSEIR_Comp+1),]
    ggplot() + geom_line(data = dataset2, aes(x = time, y = infectats_totals, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_discrete(labels = c("Reals", "SEIR")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIR_Mesures_Contencio <- renderPlot({
    # Sense mesures
    time <- as.Date(inf$DATA[1:(input$timeSEIR_mesures+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIR_mesures, by=1)
    parameters <- c(beta=1.68,delta=1/3,gamma=1/5,N=7500001)
    state <- c(S=7500000,E=0,I=1,R=0)
    out <- ode(y=state, times=seq, func=spreadSEIR, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    # Prenent mesures
    time2 <- as.Date(inf$DATA[(input$timeSEIR_mesures+1):92], tryFormats=c("%d/%m/%Y"))
    seq2 <- seq(from=input$timeSEIR_mesures, to=91, by=1)
    parameters2 <- c(beta=input$betaSEIR_mesures,delta=1/3,gamma=1/5,N=7500001)
    state2 <- c(S=tail(dataset$S,n=1),E=tail(dataset$E,n=1),I=tail(dataset$I,n=1),R=tail(dataset$R,n=1))
    out2 <- ode(y=state2, times=seq2, func=spreadSEIR, parms=parameters2)
    dataset2 <- as.data.frame(out2)
    dataset2["time"] <- time2
    # Gràfica dues parts
    ggplot() + geom_vline(xintercept=as.numeric(time2[1])) +
      geom_line(data = dataset, aes(x = time, y = S, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = E, color = "orange")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "green")) +
      geom_line(data = dataset, aes(x = time, y = R, color = "red")) +
      geom_line(data = dataset2, aes(x = time2, y = S, color = "blue"), linetype = "dashed") +
      geom_line(data = dataset2, aes(x = time2, y = E, color = "orange"), linetype = "dashed") +
      geom_line(data = dataset2, aes(x = time2, y = I, color = "green"), linetype = "dashed") +
      geom_line(data = dataset2, aes(x = time2, y = R, color = "red"), linetype = "dashed") +
      labs(x = "Període de dies", y = "Nombre d'individus") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Susceptibles", "Exposats", "Infectats", "Retirats"),
                          values = c("red", "orange", "blue", "green")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRD <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIRD+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIRD, by=1)
    parameters <- c(beta=input$betaSEIRD,delta=input$deltaSEIRD,gamma=input$gammaSEIRD,mu=input$muSEIRD,N=7500001)
    state <- c(S=7500000,E=0,I=1,R=0,D=0)
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    ggplot() + geom_line(data = dataset, aes(x = time, y = S, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = E, color = "orange")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "green")) +
      geom_line(data = dataset, aes(x = time, y = R, color = "purple")) +
      geom_line(data = dataset, aes(x = time, y = D, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'individus") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Susceptibles", "Exposats", "Infectats", "Retirats", "Difunts"),
                          values = c("red", "orange", "blue", "green", "purple")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRD_D_Comp <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIRD_Comp+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIRD_Comp, by=1)
    parameters <- c(beta=input$betaSEIRD_Comp,gamma=input$gammaSEIRD_Comp,delta=input$deltaSEIRD_Comp,mu=input$muSEIRD_Comp,N=7500001)
    state <- c(S=7500000,E=0,I=1,R=0,D=0)
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    dataset2 <- dataset_reals[1:(input$timeSEIRD_Comp+1),]
    ggplot() + geom_line(data = dataset2, aes(x = time, y = morts, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = D, color = "red")) +
      labs(x = "Període de dies", y = "Nombre de morts") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_discrete(labels = c("Reals", "SEIRD")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRD_Comp <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIRD_Comp2+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIRD_Comp2, by=1)
    parameters <- c(beta=input$betaSEIRD_Comp2,gamma=input$gammaSEIRD_Comp2,delta=input$deltaSEIRD_Comp2,mu=input$muSEIRD_Comp2,N=7500001)
    state <- c(S=7500000,E=0,I=1,R=0,D=0)
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    dataset2 <- dataset_reals[1:(input$timeSEIRD_Comp2+1),]
    ggplot() + geom_line(data = dataset2, aes(x = time, y = infectats_totals, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "orange")) +
      geom_line(data = dataset2, aes(x = time, y = morts, color = "green")) +
      geom_line(data = dataset, aes(x = time, y = D, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'individus") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Infectats registrats", "Morts registrats", "Infectats SEIRD", "Morts SEIRD"),
                          values = c("red", "orange", "blue", "green")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRDV<- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIRDV+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIRDV, by=1)
    N = 7500001
    parameters <- c(beta=input$betaSEIRDV,delta=input$deltaSEIRDV,gamma=input$gammaSEIRDV,mu=input$muSEIRDV,sigma=input$sigmaSEIRDV,N=N)
    state <- c(S=N-1,E=0,I=1,R=0,D=0)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    # Correcció dels nombre que resulten negatius + nombre total de la població
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "S"] < 0) {
        dataset[row, "S"] = 0
      }
      if (dataset[row, "E"] < 0) {
        dataset[row, "E"] = 0
      }
      if (dataset[row, "I"] < 0) {
        dataset[row, "I"] = 0
      }
      if (dataset[row, "R"] < 0) {
        dataset[row, "R"] = 0
      }
      if (dataset[row, "D"] < 0) {
        dataset[row, "D"] = 0
      }
      max = N - dataset[row, "S"] - dataset[row, "E"] - dataset[row, "I"] - dataset[row, "D"]
      dataset[row, "R"] = max
    }
    ggplot() + geom_line(data = dataset, aes(x = time, y = S, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = E, color = "orange")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "green")) +
      geom_line(data = dataset, aes(x = time, y = R, color = "purple")) +
      geom_line(data = dataset, aes(x = time, y = D, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'individus") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Susceptibles", "Exposats", "Infectats", "Retirats", "Difunts"),
                          values = c("red", "orange", "blue", "green", "purple")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRDV_Comp<- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIRDV_Comp+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to=input$timeSEIRDV_Comp, by=1)
    N = 7500000
    parameters <- c(beta=input$betaSEIRDV_Comp,delta=input$deltaSEIRDV_Comp,gamma=input$gammaSEIRDV_Comp,mu=input$muSEIRDV_Comp,sigma=input$sigmaSEIRDV_Comp,N=N)
    state <- c(S=7133385,E=364401,I=1088,R=1088,D=38)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    # Correcció dels nombre que resulten negatius + nombre total de la població
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "S"] < 0) {
        dataset[row, "S"] = 0
      }
      if (dataset[row, "E"] < 0) {
        dataset[row, "E"] = 0
      }
      if (dataset[row, "I"] < 0) {
        dataset[row, "I"] = 0
      }
      if (dataset[row, "R"] < 0) {
        dataset[row, "R"] = 0
      }
      if (dataset[row, "D"] < 0) {
        dataset[row, "D"] = 0
      }
      max = N - dataset[row, "S"] - dataset[row, "E"] - dataset[row, "I"] - dataset[row, "D"]
      dataset[row, "R"] = max
    }
    ggplot() + geom_line(data = dataset, aes(x = time, y = S, color = "blue")) +
      geom_line(data = dataset, aes(x = time, y = E, color = "orange")) +
      geom_line(data = dataset, aes(x = time, y = I, color = "green")) +
      geom_line(data = dataset, aes(x = time, y = R, color = "purple")) +
      geom_line(data = dataset, aes(x = time, y = D, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'individus") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Susceptibles", "Exposats", "Infectats", "Retirats", "Difunts"),
                          values = c("red", "orange", "blue", "green", "purple")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRDV_Inf<- renderPlot({
    time <- as.Date(inf$DATA[1:31], tryFormats=c("%d/%m/%Y"))
    data <- setNames(data.frame(matrix(ncol = 5, nrow = 31)), c("x0", "x1", "x2", "x3", "x4"))
    seq <- seq(from=0, to=30, by=1)
    N = 7500000
    beta = 2
    delta = 1/3
    gamma = 1/5
    mu = 0.01
    sigma0 = 0
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma0,N=N)
    state <- c(S=7133385,E=364401,I=1088,R=1088,D=38)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "I"] < 0) {
        dataset[row, "I"] = 0
      }
    }
    data[, "x0"] = dataset[,"I"]
    sigma1 = 0.001
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma1,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "I"] < 0) {
        dataset[row, "I"] = 0
      }
    }
    data[, "x1"] = dataset[,"I"]
    sigma2 = 0.01
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma2,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "I"] < 0) {
        dataset[row, "I"] = 0
      }
    }
    data[, "x2"] = dataset[,"I"]
    sigma3 = 0.05
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma3,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "I"] < 0) {
        dataset[row, "I"] = 0
      }
    }
    data[, "x3"] = dataset[,"I"]
    sigma4 = 0.1
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma4,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "I"] < 0) {
        dataset[row, "I"] = 0
      }
    }
    data[, "x4"] = dataset[,"I"]
    ggplot() + geom_line(data = data, aes(x = time, y = x0, color = "blue")) +
      geom_line(data = data, aes(x = time, y = x1, color = "green")) +
      geom_line(data = data, aes(x = time, y = x2, color = "orange")) +
      geom_line(data = data, aes(x = time, y = x3, color = "purple")) +
      geom_line(data = data, aes(x = time, y = x4, color = "red")) +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("0%", "0.1%", "1%", "5%", "10%"),
                          values = c("green", "red", "blue", "purple", "orange"))  + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRDV_Morts<- renderPlot({
    time <- as.Date(inf$DATA[1:61], tryFormats=c("%d/%m/%Y"))
    data <- setNames(data.frame(matrix(ncol = 5, nrow = 61)), c("x0", "x1", "x2", "x3", "x4"))
    seq <- seq(from=0, to= 60, by=1)
    N = 7500000
    beta = 2
    delta = 1/3
    gamma = 1/5
    mu = 0.01
    sigma0 = 0
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma0,N=N)
    state <- c(S=7070670,E=62685,I=35880,R=313647,D=17118)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "D"] < 0) {
        dataset[row, "D"] = 0
      }
    }
    data[, "x0"] = dataset[,"D"]
    sigma1 = 0.001
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma1,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "D"] < 0) {
        dataset[row, "D"] = 0
      }
    }
    data[, "x1"] = dataset[,"D"]
    sigma2 = 0.01
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma2,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "D"] < 0) {
        dataset[row, "D"] = 0
      }
    }
    data[, "x2"] = dataset[,"D"]
    sigma3 = 0.05
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma3,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "D"] < 0) {
        dataset[row, "D"] = 0
      }
    }
    data[, "x3"] = dataset[,"D"]
    sigma4 = 0.1
    parameters <- c(beta=beta,delta=delta,gamma=gamma,mu=mu,sigma=sigma4,N=N)
    out <- ode(y=state, times=seq, func=spreadSEIRDV, parms=parameters)
    dataset <- as.data.frame(out)
    for (row in 2:nrow(dataset)) {
      if (dataset[row, "D"] < 0) {
        dataset[row, "D"] = 0
      }
    }
    data[, "x4"] = dataset[,"D"]
    ggplot() + geom_line(data = data, aes(x = time, y = x0, color = "blue")) +
      geom_line(data = data, aes(x = time, y = x1, color = "green")) +
      geom_line(data = data, aes(x = time, y = x2, color = "orange")) +
      geom_line(data = data, aes(x = time, y = x3, color = "purple")) +
      geom_line(data = data, aes(x = time, y = x4, color = "red")) +
      labs(x = "Període de dies", y = "Nombre de morts") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("0%", "0.1%", "1%", "5%", "10%"),
                          values = c("green", "red", "blue", "purple", "orange"))  + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$SEIRDV_Vac <- renderPlot({
    time <- as.Date(inf$DATA[1:(input$timeSEIRDV_Vac+1)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=0, to= input$timeSEIRDV_Vac, by=1)
    dataset <- dataset_vacunats[1:(input$timeSEIRDV_Vac+1),]
    vac <- rep(7500000, (input$timeSEIRDV_Vac+1))
    vac <- vac*seq*input$sigmaSEIRDV_Vac
    dataset2 <- data.frame(time=time, vac=vac)
    ggplot() + geom_line(data = dataset, aes(x = time, y = vacunats_acum, color = "purple")) +
      geom_line(data = dataset2, aes(x = time, y = vac, color = "red")) +
      labs(x = "Període de dies", y = "Nombre de vacunats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Reals", "Simulats"),
                          values = c("red", "purple"))  + scale_x_date(date_labels="%d/%m/%Y")
  }) 
  output$estimation_beta_GS <- renderPlot({
    ggplot(dataset_params_GS, aes(x = date, y = beta, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Estimació paràmetre beta")  + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_gamma_GS <- renderPlot({
    ggplot(dataset_params_GS, aes(x = date, y = gamma, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Estimació paràmetre gamma") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_delta_GS <- renderPlot({
    ggplot(dataset_params_GS, aes(x = date, y = delta, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Estimació paràmetre delta") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_mu_GS <- renderPlot({
    ggplot(dataset_params_GS, aes(x = date, y = mu, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Estimació paràmetre mu") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_inf_GS <- renderPlot({
    seq <- seq(from=n, to=n+14, by=1)
    times <- seq(from=dataset_reals$DATA[n],by="day",length.out=15)
    parameters <- c(beta=params3$beta[n],gamma=params3$gamma[n],delta=params3$delta[n],mu=params3$mu[n],N=7500001)
    state <- c(S=compartments3$S[n], E=compartments3$E[n], I=compartments3$I[n],R=compartments3$R[n], D=compartments3$D[n])
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    gs_pred_inf <- as.data.frame(out)
    gs_pred_inf$time <- times
    ggplot() + geom_line(data = dataset_reals, aes(x = DATA, y = infectats_totals, color = "blue"), size = 0.3) +
      geom_line(data = compartments3, aes(x = dataset_reals$DATA, y = I, color = "orange"), size = 0.3) +
      geom_line(data = gs_pred_inf, aes(x = times, y = I, color = "red"), size = 0.3) +
      geom_vline(xintercept=as.numeric(times[1])) +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.title = element_blank()) +
      scale_colour_manual(labels = c("Registrats", "Estimacions mostrals", "Estimacions extra-mostrals"),
                          values = c("red", "blue", "orange")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_morts_GS <- renderPlot({
    seq <- seq(from=n, to=n+14, by=1)
    times <- seq(from=dataset_reals$DATA[n],by="day",length.out=15)
    parameters <- c(beta=params3$beta[n],gamma=params3$gamma[n],delta=params3$delta[n],mu=params3$mu[n],N=7500001)
    state <- c(S=compartments3$S[n], E=compartments3$E[n], I=compartments3$I[n],R=compartments3$R[n], D=compartments3$D[n])
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    gs_pred_morts <- as.data.frame(out)
    gs_pred_morts$time <- times
    ggplot() + geom_line(data = dataset_reals, aes(x = DATA, y = morts, color = "blue"), size = 0.3) +
      geom_line(data = compartments3, aes(x = dataset_reals$DATA, y = D, color = "orange"), size = 0.3) +
      geom_line(data = gs_pred_morts, aes(x = times, y = D, color = "red"), size = 0.3) +
      geom_vline(xintercept=as.numeric(times[1]), linetype = "dashed") +
      labs(x = "Període de dies", y = "Nombre de morts") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.title = element_blank()) +
      scale_colour_manual(labels = c("Registrats", "Estimacions mostrals", "Estimacions extra-mostrals"),
                          values = c("red", "blue", "orange")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$susceptibles_GS <- renderPlot({
    ggplot(compartments3, aes(x = date, y = S, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Estimació nombre de susceptibles")  + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$exposats_GS <- renderPlot({
    ggplot(compartments3, aes(x = date, y = E, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Estimació nombre d'exposats")  + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$retirats_GS <- renderPlot({
    ggplot(compartments3, aes(x = date, y = R, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Estimació nombre de retirats")  + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$GS_EM_Infectats <- renderPlot({
    t <- input$timeGS_EM_Infectats
    incr <- 14
    decr <- 6
    time <- as.Date(inf$DATA[t:(t+incr)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=t, to=t+incr, by=1)
    parameters <- c(beta=dataset_params_GS$beta[t],gamma=dataset_params_GS$gamma[t],delta=dataset_params_GS$delta[t],mu=dataset_params_GS$mu[t],N=7500001)
    state <- c(S=compartments3$S[t],E=compartments3$E[t],I=dataset_reals$infectats_totals[t],R=compartments3$R[t],D=dataset_reals$morts[t])
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    dataset2 <- dataset_reals[t:(t+incr),]
    time2 <- as.Date(inf$DATA[(t-decr):t], tryFormats=c("%d/%m/%Y"))
    dataset3 <- dataset_reals[(t-decr):t,]
    ggplot() + geom_line(data = dataset3, aes(x = time2, y = infectats_totals, color = "blue"), linetype = "dashed") +
      geom_line(data = dataset2, aes(x = time, y = infectats_totals, color = "purple"), linetype = "dashed") +
      geom_line(data = dataset, aes(x = time, y = I, color = "red")) +
      geom_vline(xintercept=as.numeric(time[1]), linetype = "dashed") +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Reals mostrals",  "Reals extra-mostrals", "Predits extra-mostrals"),
                          values = c("red", "blue", "purple")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$GS_EM_Morts <- renderPlot({
    t <- input$timeGS_EM_Morts
    incr <- 14
    decr <- 6
    time <- as.Date(inf$DATA[t:(t+incr)], tryFormats=c("%d/%m/%Y"))
    seq <- seq(from=t, to=t+incr, by=1)
    parameters <- c(beta=dataset_params_GS$beta[t],gamma=dataset_params_GS$gamma[t],delta=dataset_params_GS$delta[t],mu=dataset_params_GS$mu[t],N=7500001)
    state <- c(S=compartments3$S[t],E=compartments3$E[t],I=dataset_reals$infectats_totals[t],R=compartments3$R[t],D=dataset_reals$morts[t])
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    dataset <- as.data.frame(out)
    dataset["time"] <- time
    dataset2 <- dataset_reals[t:(t+incr),]
    time2 <- as.Date(inf$DATA[(t-decr):t], tryFormats=c("%d/%m/%Y"))
    dataset3 <- dataset_reals[(t-decr):t,]
    ggplot() + geom_line(data = dataset3, aes(x = time2, y = morts, color = "blue"), linetype = "dashed") +
      geom_line(data = dataset2, aes(x = time, y = morts, color = "purple"), linetype = "dashed") +
      geom_line(data = dataset, aes(x = time, y = D, color = "red")) +
      geom_vline(xintercept=as.numeric(time[1]), linetype = "dashed") +
      labs(x = "Període de dies", y = "Nombre de morts") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Reals mostrals",  "Reals extra-mostrals", "Predits extra-mostrals"),
                          values = c("red", "blue", "purple")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_inf_GS_zoom <- renderPlot({
    seq <- seq(from=n, to=n+14, by=1)
    times <- seq(from=dataset_reals$DATA[n],by="day",length.out=15)
    parameters <- c(beta=params3$beta[n],gamma=params3$gamma[n],delta=params3$delta[n],mu=params3$mu[n],N=7500001)
    state <- c(S=compartments3$S[n], E=compartments3$E[n], I=compartments3$I[n],R=compartments3$R[n], D=compartments3$D[n])
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    gs_pred_inf <- as.data.frame(out)
    gs_pred_inf$time <- times
    dataset_reals_2 <- dataset_reals[200:30,]
    ggplot() + geom_line(data = dataset_reals_2, aes(x = DATA, y = infectats_totals, color = "blue"), size = 0.3) +
      geom_line(data = compartments3[200:30,], aes(x = dataset_reals$DATA[200:30], y = I, color = "orange"), size = 0.3) +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.title = element_blank()) +
      scale_colour_manual(labels = c("Registrats", "Estimacions mostrals", "Estimacions extra-mostrals"),
                          values = c("red", "blue", "orange")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$estimation_morts_GS_zoom <- renderPlot({
    seq <- seq(from=n, to=n+14, by=1)
    times <- seq(from=dataset_reals$DATA[n],by="day",length.out=15)
    parameters <- c(beta=params3$beta[n],gamma=params3$gamma[n],delta=params3$delta[n],mu=params3$mu[n],N=7500001)
    state <- c(S=compartments3$S[n], E=compartments3$E[n], I=compartments3$I[n],R=compartments3$R[n], D=compartments3$D[n])
    out <- ode(y=state, times=seq, func=spreadSEIRD, parms=parameters)
    gs_pred_inf <- as.data.frame(out)
    gs_pred_inf$time <- times
    dataset_reals_2 <- dataset_reals[400:420,]
    ggplot() + geom_line(data = dataset_reals_2, aes(x = DATA, y = infectats_totals, color = "blue"), size = 0.3) +
      geom_line(data = compartments3[400:420,], aes(x = dataset_reals$DATA[400:420], y = I, color = "orange"), size = 0.3) +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.title = element_blank()) +
      scale_colour_manual(labels = c("Registrats", "Estimacions mostrals", "Estimacions extra-mostrals"),
                          values = c("red", "blue", "orange")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$errors_infectats <- renderPlot({
    ggplot(df_errors, aes(x = DATA, y = errors_infectats, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Errors en el nombre d'infectats estimats") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$errors_morts <- renderPlot({
    ggplot(df_errors, aes(x = DATA, y = errors_morts, group = 1)) + geom_line() +
      labs(x = "Període de dies", y = "Errors en el nombre de morts estimats") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$err_inf<- renderPlot({
    ggplot() + geom_line(data = df_errors_extra[2:15,], aes(x = DATA, y = err_extra_inf, group = 1)) +
      scale_x_date(date_labels="%d/%m/%Y") + labs(x = "Període de dies", y = "Errors nombre d'infectats")
  })
  output$err_morts<- renderPlot({
    ggplot() + geom_line(data = df_errors_extra[2:15,], aes(x = DATA, y = err_extra_morts, group = 1)) +
      labs(x = "Període de dies", y = "Errors nombre de morts") + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$err_inf_mostr_extra<- renderPlot({
    time1 <- as.Date(inf$DATA[1:100], tryFormats=c("%d/%m/%Y"))
    time2 <- as.Date(inf$DATA[100:114], tryFormats=c("%d/%m/%Y"))
    df_errors_extra$err_extra_inf[1] = dataset_reals$infectats_totals[100] - compartments3$I[100]
    ggplot() + geom_line(data = df_errors_mostral[1:100,], aes(x = time1, y = errors_infectats, color = "black")) +
      geom_line(data = df_errors_extra[1:15,], aes(x = time2, y = err_extra_inf, color = "red")) +
      geom_vline(xintercept=as.numeric(time1[100]), linetype = "dashed") +
      labs(x = "Període de dies", y = "Errors nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Errors mostrals reals",  "Errors estimats"),
                          values = c("black", "red")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$err_morts_mostr_extra<- renderPlot({
    time1 <- as.Date(inf$DATA[1:100], tryFormats=c("%d/%m/%Y"))
    time2 <- as.Date(inf$DATA[100:114], tryFormats=c("%d/%m/%Y"))
    df_errors_extra$err_extra_morts[1] = dataset_reals$morts[100] - compartments3$D[100]
    ggplot() + geom_line(data = df_errors_mostral[1:100,], aes(x = time1, y = errors_morts, color = "black")) +
      geom_line(data = df_errors_extra[1:15,], aes(x = time2, y = err_extra_morts, color = "red")) +
      geom_vline(xintercept=as.numeric(time1[100]), linetype = "dashed") +
      labs(x = "Període de dies", y = "Errors nombre de morts") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.title = element_blank()) +
      scale_colour_manual(labels = c("Errors mostrals reals",  "Errors estimats"),
                          values = c("black", "red")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$inf_corregits <- renderPlot({
    time0 <- as.Date(inf$DATA[90:100], tryFormats=c("%d/%m/%Y"))
    time1 <- as.Date(inf$DATA[100:114], tryFormats=c("%d/%m/%Y"))
    time2 <- as.Date(inf$DATA[90:114], tryFormats=c("%d/%m/%Y"))
    df_pred_correcio$inf_sense[1] = dataset_reals$infectats_totals[100]
    df_pred_correcio$inf_amb[1] = dataset_reals$infectats_totals[100]
    ggplot() + geom_line(data = dataset_reals[90:114,], aes(x = time2, y = infectats_totals, color = "black"), size = 0.3) +
      geom_line(data = df_pred_correcio, aes(x = time1, y = inf_sense, color = "purple"), size = 0.3) +
      geom_line(data = df_pred_correcio, aes(x = time1, y = inf_amb, color = "red"), size = 0.3) +
      geom_vline(xintercept=as.numeric(time1[1]), linetype = "dashed") +
      labs(x = "Període de dies", y = "Nombre d'infectats") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.title = element_blank()) +
      scale_colour_manual(labels = c("Registrats període mostral",  "Estimacions sense correccions ARMA", "Estimacions amb correccions ARMA"),
                          values = c("black", "purple", "red")) + scale_x_date(date_labels="%d/%m/%Y")
  })
  output$morts_corregits <- renderPlot({
    time0 <- as.Date(inf$DATA[90:100], tryFormats=c("%d/%m/%Y"))
    time1 <- as.Date(inf$DATA[100:114], tryFormats=c("%d/%m/%Y"))
    time2 <- as.Date(inf$DATA[90:114], tryFormats=c("%d/%m/%Y"))
    df_pred_correcio$morts_sense[1] = dataset_reals$morts[100]
    df_pred_correcio$morts_amb[1] = dataset_reals$morts[100]
    ggplot() + geom_line(data = dataset_reals[90:114,], aes(x = time2, y = morts, color = "black"), size = 0.3) +
      geom_line(data = df_pred_correcio, aes(x = time1, y = morts_sense, color = "purple"), size = 0.3) +
      geom_line(data = df_pred_correcio, aes(x = time1, y = morts_amb, color = "red"), size = 0.3) +
      geom_vline(xintercept=as.numeric(time1[1]), linetype = "dashed") +
      labs(x = "Període de dies", y = "Nombre de morts") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.title = element_blank()) +
      scale_colour_manual(labels = c("Registrats període mostral",  "Estimacions sense correccions SARIMA", "Estimacions amb correccions SARIMA"),
                          values = c("black", "purple", "red")) + scale_x_date(date_labels="%d/%m/%Y")
  })
}
