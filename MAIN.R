# Clear the memory
rm(list=ls())

# call the packages we need
require('readxl')
require('BVAR')
require('plm')
library(zoo) 
require('ggplot2')
library(lpirfs)
library(seasonal)
#for the HAC standard Errors
require('sandwich')
require('lmtest')
library(boot)

## Figure 1
  source('Figure_1.r')

## Figure 2
  source('Figure_2.r')

## Figure 3

  # Extract the shock to eval
  varshock  = c('inst1')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst1')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  
  source('SOURCE_LP.r')


## Figure 4
  
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r')
  
## Figure 5
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c( 	'CPILFESL','CPIENGSL',	'CUSR0000SAD',	'CUSR0000SAN',	'CUSR0000SAS')
  source('SOURCE_LP.r')

## Figure 6
  
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c( 'OILGSUS',	'ELECTUS',	'MNINGUS',	'AUTOSUS',	'RTAILUS',	'TRLESUS')
  source('SOURCE_LP.r')
  
## Figure 7
  # Extract the shock to eval
  varshock  = c('inst1')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst1')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c('Energy_eff',"WTI",'CPU','EPSITOT','EPSIMARKET','EPSISUP')
  source('SOURCE_LP.r')

## Figure 8
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c('Energy_eff',"WTI",'CPU','EPSITOT','EPSIMARKET','EPSISUP')
  source('SOURCE_LP.r')
  
## Figure 9 
  source('Figure_9.r')
## Figure 10
  
  # Extract the shock to eval
  varshock  = c('inst1')
  Ratio_anal= 0
  firms_anal= 1
  var_gpbii_firms = c('GFGPBII')
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst1')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 1
  var_gpbii_firms = c('GFGPBII')
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r')
  
  
## Figure 11
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 1
  var_gpbii_firms = c('GFGPBII')
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 1
  var_gpbii_firms = c('GFGPBII')
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r')

## Figure 12
  
  # Extract the shock to eval
  varshock  = c('inst1')
  Ratio_anal= 0
  firms_anal= 1
  var_gpbii_firms = c('NGFGPBII')
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst1')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 1
  var_gpbii_firms = c('NGFGPBII')
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r')
  
  
## Figure 13
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 1
  var_gpbii_firms = c('NGFGPBII')
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 1
  var_gpbii_firms = c('NGFGPBII')
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r')  

## Figure 14
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 1
  var_gpbii_firms = c('GFGPBII')
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 1
  var_gpbii_firms = c('GFGPBII')
  VarNames  = c('Energy_eff',"WTI",'CPU','EPSITOT','EPSIMARKET','EPSISUP')
  source('SOURCE_LP.r')  
  
## Figure 15
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 1
  var_gpbii_firms = c('NGFGPBII')
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 1
  var_gpbii_firms = c('NGFGPBII')
  VarNames  = c('Energy_eff',"WTI",'CPU','EPSITOT','EPSIMARKET','EPSISUP')
  source('SOURCE_LP.r')  

## Figure 16
  
  # Extract the shock to eval
  varshock  = c('inst1')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst1')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 1
  firms_anal   = 0
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  
  source('SOURCE_LP.r')
  
  
## Figure 17
  
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 1
  firms_anal   = 0
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r') 
  
## Figure A1
  source('Figure_A1.r')
 
## Figure A2
  # Extract the shock to eval
  varshock  = c('RGPBII')
  Ratio_anal= 1
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('RGPBII')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 1
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r')
  
## Figure A3
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 1
  VarNames  = c('NGPBII','GPBII',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
## Figure A4
  # Extract the shock to eval
  varshock  = c('inst2')
  Ratio_anal= 0
  firms_anal= 0
  show_FEVD = 0
  VarNames  = c('NGPBII','GPBII','Energy_eff',"UATFP",'GDP','CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_BVAR.r')
  
  shock_to_eval = c('inst2')
  Controls     = 0
  plot         = 1
  Ratio_anal   = 0
  struc_anal   = 0
  firms_anal   = 0
  VarNames  = c('NGPBII','GPBII',"UATFP","GDP",'CONPC','INVPC','Hours','CPI','FED','Sentiment',"Stock_prices")
  source('SOURCE_LP.r')