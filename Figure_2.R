# Upload the data

DATA      = read_excel('Quarter_data_Long.xlsx')
year_ini  = substr(DATA$Date,1,4)[1]
quarter_ini = as.numeric(substr(DATA$Date,6,7)[1])/3 + 2/3
#Dates     = as.matrix(as.character(DATA$Date,1))
Dates = as.matrix(as.character(DATA$Date))

# Inputs
Confidence = 0.68
nDraws     = 500
nSteps     = 21
pLags      = 4



# Model 1 using only the GPBII
VarNames1  = c('GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment','Stock_prices')

Long_Names = read_excel('Names.xlsx')
Data_CGV  = matrix(as.numeric(as.matrix(DATA[,VarNames1])),nrow(DATA),length(VarNames1))
colnames(Data_CGV) = VarNames1

Data_CGV  = Data_CGV[complete.cases(Data_CGV),]

Data_CGV  = ts(Data_CGV,frequency=4,start=c(year_ini,quarter_ini))

Shock_value        = 1

set.seed(123456)
mn_l <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5), #sets the prior for the tightness (lambda)
  alpha = bv_alpha(mode =2), #shrinkage parameter alpha set to 2
  var = 1e07) 

priors <- bv_priors(hyper = "auto", mn = mn_l)

mh <- bv_metropolis(scale_hess = c(0.05),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)

Model1          <- bvar(Data_CGV,lags=pLags,n_draw=20000,nburn=5000,n_thin = 10L, priors = priors, mh = mh, verbose = FALSE)

## IRF of Model 1

varshock = c('GPBII')
#Cholesky IRF
IRF_BVAR             <- irf(Model1,horizon = nSteps)
IRF_BVAR             <- array(IRF_BVAR$irf[,,,which(VarNames1==varshock)],dim=c(dim(IRF_BVAR$irf)[1],dim(IRF_BVAR$irf)[2],dim(IRF_BVAR$irf)[3]))

#Put the IRF in the format I want

IR_Ch1              = matrix(NaN,nSteps,ncol(Data_CGV))
IR_Ch_Low1          = matrix(NaN,nSteps,ncol(Data_CGV))
IR_Ch_sup1          = matrix(NaN,nSteps,ncol(Data_CGV))
for(n in 1:length(VarNames1)){
  for(h in 1:nSteps){
    IR_Ch1[h,n]         = quantile(IRF_BVAR[,n,h]*100,0.5)
    IR_Ch_Low1[h,n]     = quantile(IRF_BVAR[,n,h]*100,(1-Confidence)/2)
    IR_Ch_sup1[h,n]     = quantile(IRF_BVAR[,n,h]*100,1-(1-Confidence)/2)
  }
}

# Reescale the IRF for the TFP to have a value of 1 in period 10

Factor1                 = IR_Ch1[10,which(VarNames1=='UATFP')]

IR_Ch1 = IR_Ch1/Factor1
IR_Ch_Low1 = IR_Ch_Low1/Factor1
IR_Ch_sup1 = IR_Ch_sup1/Factor1


## Model 2 using only the NGPBII

VarNames2  = c('NGPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment','Stock_prices')

Long_Names = read_excel('Names.xlsx')
Data_CGV  = matrix(as.numeric(as.matrix(DATA[,VarNames2])),nrow(DATA),length(VarNames2))
colnames(Data_CGV) = VarNames2

Data_CGV  = Data_CGV[complete.cases(Data_CGV),]

Data_CGV  = ts(Data_CGV,frequency=4,start=c(year_ini,quarter_ini))

Shock_value        = 1

set.seed(123456)
mn_l <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5), #sets the prior for the tightness (lambda)
  alpha = bv_alpha(mode =2), #shrinkage parameter alpha set to 2
  var = 1e07) #large variance on constant
# I do not include the soc or sur priors because results are much closer 
# to the original paper without them.


priors <- suppressWarnings(
  bv_priors(hyper = "auto", mn = mn_l)
)


mh <- bv_metropolis(scale_hess = c(0.05),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)


Model2 <- suppressWarnings(
  bvar(
    Data_CGV,
    lags = pLags,
    n_draw = 20000,
    nburn = 5000,
    n_thin = 10L,
    priors = priors,
    mh = mh,
    verbose = FALSE
  )
)

## IRF of Model 2

varshock = c('NGPBII')
#Cholesky IRF
IRF_BVAR             <- irf(Model2,horizon = nSteps)
IRF_BVAR             <- array(IRF_BVAR$irf[,,,which(VarNames2==varshock)],dim=c(dim(IRF_BVAR$irf)[1],dim(IRF_BVAR$irf)[2],dim(IRF_BVAR$irf)[3]))

#Put the IRF in the format I want

IR_Ch2              = matrix(NaN,nSteps,ncol(Data_CGV))
IR_Ch_Low2          = matrix(NaN,nSteps,ncol(Data_CGV))
IR_Ch_sup2          = matrix(NaN,nSteps,ncol(Data_CGV))
for(n in 1:length(VarNames2)){
  for(h in 1:nSteps){
    IR_Ch2[h,n]         = quantile(IRF_BVAR[,n,h]*100,0.5)
    IR_Ch_Low2[h,n]     = quantile(IRF_BVAR[,n,h]*100,(1-Confidence)/2)
    IR_Ch_sup2[h,n]     = quantile(IRF_BVAR[,n,h]*100,1-(1-Confidence)/2)
  }
}

# Reescale the IRF for the TFP to have a value of 1 in period 10

Factor2                 = IR_Ch2[10,which(VarNames2=='UATFP')]

IR_Ch2 = IR_Ch2/Factor2
IR_Ch_Low2 = IR_Ch_Low2/Factor2
IR_Ch_sup2 = IR_Ch_sup2/Factor2



  # plot the IRFs 
VarNames3 = VarNames2[VarNames2!=varshock]

  par(mfrow=c(ceiling(length(VarNames3)^(1/2)),ceiling(length(VarNames3)/ceiling(length(VarNames3)^(1/2)))))

  for(i in 1:length(VarNames1)) {
    if(VarNames1[i]!= 'NGPBII'&VarNames1[i]!= 'GPBII'){
      nn <- Long_Names[which(Long_Names[, 'Name'] == VarNames1[i]), 'Long_Name']
      L_Name      = as.character(nn)
      #jpeg(paste0(L_Name, ".jpeg"), width = 350, height = 350)
      plot.ts(IR_Ch1[, i], ylim = c(min(cbind(IR_Ch_Low1[, i],IR_Ch_Low2[, i])), max(cbind(IR_Ch_sup1[, i],IR_Ch_sup2[, i]))),
              main = as.character(nn),
              ylab = 'Percent',
              xlab = 'Horizons',
              col = 'chartreuse4',
              lwd = 2)
      lines(IR_Ch_Low1[, i], col = 'chartreuse4', lty = 2, lwd = 2)
      lines(IR_Ch_sup1[, i], col = 'chartreuse4', lty = 2, lwd = 2)
      
      
      # Agregar área sombreada
      polygon(x = c(time(IR_Ch1), rev(time(IR_Ch1))), 
              y = c(IR_Ch_Low1[, i], rev(IR_Ch_sup1[, i])), 
              col = alpha('chartreuse4', 0.2), 
              border = NA)
      
      lines(IR_Ch2[, i], col = 'brown', lwd = 2)
      lines(IR_Ch_Low2[, i], col = 'brown', lty = 2, lwd = 2)
      lines(IR_Ch_sup2[, i], col = 'brown', lty = 2, lwd = 2)
      
      # Agregar área sombreada
      polygon(x = c(time(IR_Ch2), rev(time(IR_Ch2))), 
              y = c(IR_Ch_Low2[, i], rev(IR_Ch_sup2[, i])), 
              col = alpha('brown', 0.2), 
              border = NA)
      
      
      
      # Agregar línea horizontal en y=0
      abline(h = 0, col = 'azure4', lwd = 2)
      
      # Agregar rejillas (grids)
      grid(col = "gray", lty = "dotted")
      #dev.off()
    }
    
    
  }
  

