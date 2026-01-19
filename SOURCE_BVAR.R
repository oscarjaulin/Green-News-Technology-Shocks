# Upload the data
DATA      = read_excel('Quarter_data_long.xlsx')

if(Ratio_anal==1){
  DATA[,'RGPBII']   <- (exp(DATA[,'GPBII'])/exp(DATA[,'NGPBII']))
  VarNames  = c('RGPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
}

if(firms_anal==1){
  DATA[,'GPBII']   <- DATA[,var_gpbii_firms]
}

year_ini  = substr(DATA$Date,1,4)[1]
quarter_ini = as.numeric(substr(DATA$Date,6,7)[1])/3 + 2/3
Dates     = as.matrix(as.character(DATA$Date,1))

# Inputs
Confidence = 0.68
nDraws     = 500
nSteps     = 13
pLags      = 4




# Shock Variables
#VarNames  = c('NGPBII','GPBII',"UA-TFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")


Long_Names = read_excel('Names.xlsx')
Data_CGV  = matrix(as.numeric(as.matrix(DATA[,VarNames])),nrow(DATA),length(VarNames))
colnames(Data_CGV) = VarNames

Dates     = Dates[complete.cases(Data_CGV),,drop=F]
Data_CGV  = Data_CGV[complete.cases(Data_CGV),]


# Take out the seasonal component.
Data_CGV  = ts(Data_CGV,frequency=4,start=c(year_ini,quarter_ini))

Shock_value        = 1

set.seed(123456)
mn_l <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5), #sets the prior for the tightness (lambda)
  alpha = bv_alpha(mode =2), #shrinkage parameter alpha set to 2
  var = 1e07) #large variance on constant
# I do not include the soc or sur priors because results are much closer 
# to the original paper without them.

priors <- bv_priors(hyper = "auto", mn = mn_l)

mh <- bv_metropolis(scale_hess = c(0.05),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)

Model          <- bvar(Data_CGV,lags=pLags,n_draw=20000,nburn=5000,n_thin = 10L, priors = priors, mh = mh, verbose = FALSE)

## Compute the forecast error variance decomposition using the cholesky decomposition

# FEVD using Cholesky identification.
# IMPORTANT: the Cholesky ordering is the column order of Data_CGV.
vd <- fevd(Model,horizon = 40,conf_bands=c(0.1))
# vd$quants[Quantiles,vars to be explained, horizon, shocks]

if(show_FEVD==1){
  #par(mfrow=c(ceiling(length(VarNames)^(1/2)),ceiling(length(VarNames)/ceiling(length(VarNames)^(1/2)))))  
  for(j in 1:length(vd$variables)){
    
    nn = Long_Names[which(Long_Names[,'Name']==VarNames[j]),'Long_Name']
    FEVD = vd$quants[2,j,,1:2]
    #FEVD dimensions: i. three, the second value is the median
    #                 ii. the variable I am decomposing
    #                 iii. horizons
    #                 iv. The shocks I am analyzing
    
    
    # Calculate the unexplained component
    Other = 1 - rowSums(FEVD)
    
    # Convert to data frame for ggplot
    df <- data.frame(
      Horizon = 1:dim(FEVD)[1],
      Other = Other,
      Common = FEVD[,1],
      Idiosyncratic = FEVD[,2]
    )
    
    # Melt for ggplot
    df_melt <- reshape2::melt(df, id.vars = "Horizon", variable.name = "Shock", value.name = "Contribution")
    
    # Create the bar plot
    L_Name = as.character(Long_Names[which(Long_Names[,1]==VarNames[j]),2])
    
    #jpeg(paste0(L_Name, ".jpeg"), width = 350, height = 350)
    
    # Create the plot and assign it to a variable
    p <- ggplot(df_melt, aes(x = Horizon, y = Contribution, fill = Shock)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c( "Other" = "gray75","Common" = "blue4", "Idiosyncratic" = "darkorange3")) +
      labs(title = nn, x = "Horizon", y = "Contribution") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
    
    # Print the plot to the JPEG device
    print(p)
    
    
    #dev.off()
    
  }
  
}


pos_var = which(VarNames==varshock)

if(any(pos_var)){
  #Cholesky IRF
  IRF_BVAR             <- irf(Model,horizon = nSteps)
  IRF_BVAR             <- array(IRF_BVAR$irf[,,,which(VarNames==varshock)],dim=c(dim(IRF_BVAR$irf)[1],dim(IRF_BVAR$irf)[2],dim(IRF_BVAR$irf)[3]))
  
  #Put the IRF in the format I want
  
  IR_Ch              = matrix(NaN,nSteps,ncol(Data_CGV))
  IR_Ch_Low          = matrix(NaN,nSteps,ncol(Data_CGV))
  IR_Ch_sup          = matrix(NaN,nSteps,ncol(Data_CGV))
  for(n in 1:length(VarNames)){
    for(h in 1:nSteps){
      IR_Ch[h,n]         = quantile(IRF_BVAR[,n,h]*100,0.5)
      IR_Ch_Low[h,n]     = quantile(IRF_BVAR[,n,h]*100,(1-Confidence)/2)
      IR_Ch_sup[h,n]     = quantile(IRF_BVAR[,n,h]*100,1-(1-Confidence)/2)
    }
    
  }
  
  if(Ratio_anal==0){
    # plot the IRFs 
    par(mfrow=c(ceiling(length(VarNames)^(1/2)),ceiling(length(VarNames)/ceiling(length(VarNames)^(1/2)))))
    
    for(i in 1:length(VarNames)){
      nn = Long_Names[which(Long_Names[,'Name']==VarNames[i]),'Long_Name']
      plot.ts(IR_Ch[,i],ylim=c(min(IR_Ch_Low[,i]),max(IR_Ch_sup[,i])),main=as.character(nn),ylab='Percent',xlab='Horizons')
      abline(h=0, col='azure4')
      lines(IR_Ch_Low[,i],col=1,lty=2)
      lines(IR_Ch_sup[,i],col=1,lty=2) 
    } 
  }
  

  vcov    = vcov(Model)
  A_Chol  = t(chol(vcov))
  U_t              = as.matrix(residuals(Model))
  E_t              = t(solve(A_Chol)%*%t(U_t))
  inst= scale(E_t[,which(VarNames==varshock)])  
  
  inst = cbind(Dates[(Model$meta$lags+1):nrow(Dates),1],inst)
  colnames(inst) = c('Date','Instrument')
  inst = as.data.frame(inst)
  inst$Instrument = as.numeric(inst$Instrument)
} else {
  U_t              = as.matrix(residuals(Model))
  Depend           = U_t[,which(VarNames=='GPBII'),drop=F]
  independ         = U_t[,which(VarNames=='NGPBII'),drop=F]
  
  Data_Shocks      = cbind(Depend,independ)
  Data_Shocks      = as.data.frame(Data_Shocks)

  LM_Model         = lm(GPBII ~ NGPBII, data = Data_Shocks)
  R2               = summary(LM_Model)
  print(R2$r.squared)
  inst1            = scale(LM_Model$fitted.values)
  inst1            = cbind(Dates[(Model$meta$lags+1):nrow(Dates),1],inst1)
  colnames(inst1)  = c('Date','Instrument')
  inst1 = as.data.frame(inst1)
  inst1$Instrument = as.numeric(inst1$Instrument)
  inst2            = scale(LM_Model$residuals)
  inst2            = cbind(Dates[(Model$meta$lags+1):nrow(Dates),1],inst2)
  colnames(inst2)  = c('Date','Instrument')
  inst2 = as.data.frame(inst2)
  inst2$Instrument = as.numeric(inst2$Instrument)
}

