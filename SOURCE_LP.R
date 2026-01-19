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
DATA_COP  = DATA


# Inputs
Confidence = 0.68
nSteps     = 28

conf_type  = c('boot') #c('HAC') 

#VarNames  = c('NGPBII','GPBII',"UA-TFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
#VarNames  = c('Energy_eff',"WTI",'CPU','EPSITOT','EPSIMARKET','EPSISUP')



Long_Names = read_excel('Names.xlsx')

## Assign the value of the instrument properly
pos_var = which(VarNames==varshock)
if(any(pos_var)){
  Data_inst      = inst 
} else {
  Data_inst      = get(shock_to_eval)
}

if(struc_anal==0){
  par(mfrow=c(ceiling(length(VarNames)^(1/2)),ceiling(length(VarNames)/ceiling(length(VarNames)^(1/2)))))
  
  
  
  if(shock_to_eval==c('inst2')||shock_to_eval==c('GPBII')||shock_to_eval==c('RGPBII')){
    color         = "darkorange3"
  } else {
    color         = "blue4"
  }
  for(nn in VarNames){
    YY           = as.matrix(DATA[,nn])
    YY           = cbind(DATA[1:nrow(DATA),'Date'],YY)
    
    Date_bo      = intersect(as.matrix(Data_inst[,'Date']),as.character(YY[,'Date']))
    if(Controls==1){
      Date_bo      = intersect(Date_bo, Data_Cont$Date) 
    }
    
    YY           = YY[which(YY[,'Date']%in%Date_bo),]
    
    inss          = as.matrix(as.numeric(Data_inst[which(as.matrix(Data_inst[,'Date'])%in%Date_bo),'Instrument']))
  
    colnames(inss) = c('inst')  
    
    if(Controls==1){
      Conts      = as.matrix(Data_Cont[as.matrix(Data_Cont[,'Date'])%in%Date_bo,names_cont])
    }
    
    IRF          = NULL
    for(h in 1:nSteps){
      Depend     = as.matrix(YY[(h+2-1):nrow(YY),nn,drop=F] -  YY[1:(nrow(YY)-h),nn,drop=F])
      ins        = inss[2:(nrow(inss)-h+1),'inst',drop=F]
        
        # Build Data_temp as you already do
        if (!Controls) {
          Data_temp  <- data.frame(Depend, ins)
          fml <- paste(colnames(Depend[,drop=FALSE]), "~", 'inst')
        } else {
          css <- Conts[1:(nrow(Conts)-h), , drop = FALSE]
          # Make sure css columns have names and live in the data frame
          Data_temp  <- data.frame(Depend, ins, css, check.names = FALSE)
          # Either list columns explicitly or just use '.' to include all others:
          fml <- paste(colnames(Depend[,drop=FALSE]), "~", 'inst + css')
        }
      
      lm_model <- lm(fml, data = Data_temp)
      
      if(conf_type==c('boot')){
        
        original_coefs <- coef(lm_model)
        regression_fn <- function(d) {
          d <- as.data.frame(d)
          fit <- lm(fml, data = d)
          coef(fit)
        }
        
        # Do NOT try to subset inside regression_fn; tsboot handles resampling.
        boot_out <- tsboot(
          tseries = Data_temp,
          statistic = regression_fn,
          R = 1000,
          l = 4,
          sim = "fixed"    # or "geom"
        )
        
        # Calcular p-values bootstrap (proporción de veces que coef ≤ 0)
        boot_pvals <- apply(boot_out$t, 2, function(x) 2 * min(mean(x <= 0), mean(x >= 0)))
        
        # Calcular percentiles para ICs
        ci_lower1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = (1-Confidence)/2))
        ci_upper1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = 1-(1-Confidence)/2))
        pons_ins  = which(names(lm_model$coefficients)=='inst')
        
        Coefs      = cbind(ci_lower1[pons_ins,1],lm_model$coefficients['inst'],ci_upper1[pons_ins,1])
        
        
        IRF        = rbind(IRF,Coefs)
      } else {
        
        # Calcular la matriz de covarianza robusta (Newey-West)
        cov_matrix <- NeweyWest(lm_model, lag = 2, prewhite = F)
        
        # Extract coefficients
        coef_estimates <- as.matrix(coef(lm_model))
        
        # Calcular los errores estándar robustos
        robust_se <- as.matrix(sqrt(diag(cov_matrix)))
        
        # Calcular los coeficientes y los intervalos de confianza robustos
        confint_robust <- array(coef_estimates,dim=c(length(robust_se),2)) + t(array(qt(c((1-Confidence)/2, 1-(1-Confidence)/2), df = Inf),dim=c(2,length(robust_se)))) * array(robust_se,dim=c(length(robust_se),2))
        # Create a data frame for better readability
        CI <- as.data.frame(confint_robust)
        pons_ins  = which(names(lm_model$coefficients)=='inst')
        Coefs      = cbind(CI[pons_ins,1],lm_model$coefficients['inst'],CI[pons_ins,2])
        
        
        IRF        = rbind(IRF,Coefs)
      }
      
    }
    
    if(any(c('IDIF','UNEMP',"JFR","JDR")%in%nn)){
      Factor= 100
      ylab  = 'p.p.'
    } else {
      Factor= 100
      ylab  = 'percent'
    }
    IRF     = IRF*Factor
    L_Name      = as.character(Long_Names[which(Long_Names[,1]==nn),2])
    
    IRF_xls <- as.data.frame(IRF)
    names(IRF_xls) <- NULL
    
    
    if(plot==1){
      #jpeg(paste0(L_Name, ".jpeg"), width = 350, height = 350)
      plot.ts(IRF[,2], 
              ylim = c(min(IRF[,1],IRF[,2],IRF[,3]), max(IRF[,1],IRF[,2],IRF[,3])), 
              main = L_Name, 
              ylab = 'Percent', 
              xlab = 'Horizons', 
              col = color,  # Color de la serie principal
              lwd = 2)  # Grosor de la serie principal
      
      # Graficar las líneas de IR_Ch_Low y IR_Ch_sup
      lines(IRF[,1], col = color, lty = 2, lwd = 2)  # Línea punteada y roja, más gruesa
      lines(IRF[,3], col = color, lty = 2, lwd = 2)  # Línea punteada y roja, más gruesa
      
      # Agregar sombra entre las curvas superior e inferior
      polygon(c(time(IRF), rev(time(IRF))), c(IRF[, 1], rev(IRF[, 3])), col = alpha(color, 0.2), border = NA)
      
      # Agregar línea horizontal en y=0
      abline(h = 0, col = 'azure4', lwd = 2)
      
      # Agregar rejillas (grids)
      grid(col = "gray", lty = "dotted")
      #dev.off()
    }
  }
} else if(struc_anal==1){


  ##### INCLUDE INTERACTION WITH INSTRUMENT AND DUMMY OF TIME. 
  par(mfrow=c(ceiling(length(VarNames)^(1/2)),ceiling(length(VarNames)/ceiling(length(VarNames)^(1/2)))))
  
  if(shock_to_eval==c('inst2')||shock_to_eval==c('GPBII')){
    color         = "darkorange3"
  } else if(shock_to_eval==c('inst1')){
    color         = "blue4"
  }
  if(struc_anal == 1){
    Dummy          = as.matrix((DATA$Date>='1990-01-01')*1)
    DATA$Dummy     = Dummy
    
    for(nn in VarNames){
      YY           = as.matrix(DATA[,nn])
      YY           = cbind(DATA[1:nrow(DATA),'Date'],YY)
      
      Date_bo      = intersect(as.matrix(Data_inst[,'Date']),as.character(YY[,'Date']))
      if(Controls==1){
        Date_bo      = intersect(Date_bo, Data_Cont$Date) 
      }
      
      YY           = YY[which(YY[,'Date']%in%Date_bo),]
      
      inss          = as.matrix(as.numeric(Data_inst[which(as.matrix(Data_inst[,'Date'])%in%Date_bo),'Instrument']))
      colnames(inss) = c('inst') 
      
      Dum           = as.matrix(DATA[,'Dummy'])
      Dum           = cbind(DATA[1:nrow(DATA),'Date'],Dum) 
      Dum           = Dum[which(Dum[,'Date']%in%Date_bo),]
      
      
      
      if(Controls==1){
        Conts      = as.matrix(Data_Cont[as.matrix(Data_Cont[,'Date'])%in%Date_bo,names_cont])
      }
      
      IRF          = NULL
      IRF0         = NULL
      for(h in 1:nSteps){
        Depend     = as.matrix(YY[(h+2-1):nrow(YY),nn,drop=F] -  YY[1:(nrow(YY)-h),nn,drop=F])
        ins        = inss[2:(nrow(inss)-h+1),'inst',drop=F]
        du         = Dum[2:(nrow(Dum)-h+1),'Dummy',drop=F]
        if(!Controls){
          Data_temp  = data.frame(Depend,ins,du)
          colnames(Data_temp) = c(colnames(Depend[,drop=FALSE]),'ins','du')
          Data_temp$D1 <- ifelse(Data_temp$du == 1, 1, 0)
          Data_temp$D0 <- ifelse(Data_temp$du == 0, 1, 0)
          Data_temp$s_D1 <- Data_temp$ins * Data_temp$D1
          Data_temp$s_D0 <- Data_temp$ins * Data_temp$D0
          
          fml        <- paste(colnames(Depend[,drop=FALSE]), "~", 's_D0 + D1 + s_D1')
          
        } else {
          css        = Conts[1:(nrow(Conts)-h),,drop=F]
          Data_temp  = data.frame(Depend,ins,css,du)
          Data_temp$D1 <- ifelse(Data_temp$du == 1, 1, 0)
          Data_temp$D0 <- ifelse(Data_temp$du == 0, 1, 0)
          Data_temp$s_D1 <- Data_temp$ins * Data_temp$D1
          Data_temp$s_D0 <- Data_temp$ins * Data_temp$D0
          
          fml        <- paste(colnames(Depend[,drop=FALSE]), "~", ' css + s_D0 + D1 + s_D1 + css*du')
        }
        
        lm_model   <- lm(fml, data = Data_temp) 
        if(conf_type==c('boot')){
          
          original_coefs <- coef(lm_model)
          regression_fn <- function(d) {
            d <- as.data.frame(d)
            fit <- lm(fml, data = d)
            coef(fit)
          }
          
          # Do NOT try to subset inside regression_fn; tsboot handles resampling.
          boot_out <- tsboot(
            tseries = Data_temp,
            statistic = regression_fn,
            R = 1000,
            l = 4,
            sim = "fixed"    # or "geom"
          )
          
          # Calcular p-values bootstrap (proporción de veces que coef ≤ 0)
          boot_pvals <- apply(boot_out$t, 2, function(x) 2 * min(mean(x <= 0), mean(x >= 0)))
          
          # Calcular percentiles para ICs
          ci_lower1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = (1-Confidence)/2))
          ci_upper1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = 1-(1-Confidence)/2))
          pons_ins  = which(names(lm_model$coefficients)=='s_D1')
          
          Coefs      = cbind(ci_lower1[pons_ins,1],lm_model$coefficients['s_D1'],ci_upper1[pons_ins,1])
          
          
          IRF        = rbind(IRF,Coefs)

          pons_ins  = which(names(lm_model$coefficients)=='s_D0')
          Coefs0      = cbind(ci_lower1[pons_ins,1],lm_model$coefficients['s_D0'],ci_upper1[pons_ins,1])
          
          
          #CI0         = confint(lm_model,'s_D0',level=Confidence)
          #Coefs0      = cbind(CI0[1],lm_model$coefficients['s_D0'],CI0[2])
          IRF0        = rbind(IRF0,Coefs0)
        } else {
          
          # Calcular la matriz de covarianza robusta (Newey-West)
          cov_matrix <- NeweyWest(lm_model, lag = 2, prewhite = F)
          
          # Extract coefficients
          coef_estimates <- as.matrix(coef(lm_model))
          
          # Calcular los errores estándar robustos
          robust_se <- as.matrix(sqrt(diag(cov_matrix)))
          
          # Calcular los coeficientes y los intervalos de confianza robustos
          confint_robust <- array(coef_estimates,dim=c(length(robust_se),2)) + t(array(qt(c((1-Confidence)/2, 1-(1-Confidence)/2), df = Inf),dim=c(2,length(robust_se)))) * array(robust_se,dim=c(length(robust_se),2))
          # Create a data frame for better readability
          CI <- as.data.frame(confint_robust)
          pons_ins  = which(names(lm_model$coefficients)=='s_D1')
          Coefs      = cbind(CI[pons_ins,1],lm_model$coefficients['s_D1'],CI[pons_ins,2])
          
          
          #CI         = confint(lm_model,'s_D1',level=Confidence)
          #Coefs      = cbind(CI[1],lm_model$coefficients['s_D1'],CI[2])
          IRF        = rbind(IRF,Coefs)
          
          CI0 <- as.data.frame(confint_robust)
          pons_ins  = which(names(lm_model$coefficients)=='s_D0')
          Coefs0      = cbind(CI[pons_ins,1],lm_model$coefficients['s_D0'],CI[pons_ins,2])
          
          
          #CI0         = confint(lm_model,'s_D0',level=Confidence)
          #Coefs0      = cbind(CI0[1],lm_model$coefficients['s_D0'],CI0[2])
          IRF0        = rbind(IRF0,Coefs0)
        }
        
      }
      
      if(any(c('IDIF','UNEMP',"JFR","JDR")%in%nn)){
        Factor= 100
        ylab  = 'p.p.'
      } else {
        Factor= 100
        ylab  = 'percent'
      }
      IRF     = IRF*Factor
      IRF0     = IRF0*Factor
      L_Name      = as.character(Long_Names[which(Long_Names[,1]==nn),2])
      
      IRF_xls <- as.data.frame(IRF)
      IRF0_xls <- as.data.frame(IRF0)
      names(IRF_xls) <- NULL
      names(IRF0_xls) <- NULL
      
      if(plot==1){
       
        h <- 1:nrow(IRF)
        
        # Abrir dispositivo si guardas a archivo
        #jpeg(paste0(L_Name,'_dummy', ".jpeg"), width = 350, height = 350)
        
        # Crear gráfico base con IRF principal
        plot(h, IRF[,2], 
             type = 'l',
             ylim = c(min(IRF[,1], IRF0[,1]), max(IRF[,3], IRF0[,3])),
             main = L_Name,
             ylab = "Percent",
             xlab = "Horizons",
             col = color,
             lwd = 2)
        
        # Sombra IRF0 (gris) — dibujada primero para quedar por debajo
        polygon(c(h, rev(h)),
                c(IRF0[,1], rev(IRF0[,3])),
                col = adjustcolor("gray", alpha.f = 0.4),
                border = NA)
        
        # Líneas IRF0
        lines(h, IRF0[,2], col = "gray20", lwd = 2)
        lines(h, IRF0[,1], col = "gray20", lty = 2, lwd = 2)
        lines(h, IRF0[,3], col = "gray20", lty = 2, lwd = 2)
        
        # Sombra IRF principal
        polygon(c(h, rev(h)),
                c(IRF[,1], rev(IRF[,3])),
                col = alpha(color, 0.2),
                border = NA)
        
        # Líneas IRF principal
        lines(h, IRF[,1], col = color, lty = 2, lwd = 2)
        lines(h, IRF[,3], col = color, lty = 2, lwd = 2)
        
        # Ya estaba trazada en plot(): línea central
        # lines(h, IRF[,2], col = color, lwd = 2)
        
        # Línea horizontal
        abline(h = 0, col = 'azure4', lwd = 2)
        
        # Rejilla
        grid(col = "gray", lty = "dotted")
        #dev.off()
      }
    }
    
  }
}