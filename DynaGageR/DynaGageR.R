# DynaGageR
#  
# R script to calculate sapflow from logged  raw Voltages of Dynamax Dynagage sapflow gages
# v0.8
# David Basler 2020
#

message('\nsourcing DynaGageR ...\n')

#### I/O

#' Read Campbell Logger .dat files
#' This data.frame object returned by this function dataframe contains basic information about model inpiut parameters and is used for the check_parameters function
#' @param fn filename
#' @return a data.frame object containing the data with units and logger name as attribute
read_campbell<-function(fn){
  info  <- as.character(read.csv(fn,sep=',',nrows=1,header=FALSE))
  header<- as.character(read.csv(fn,sep=',',nrows=1,header=FALSE,skip = 1))
  unit <- as.character(read.csv(fn,sep=',',nrows=1,header=FALSE,skip = 2))
  integ <- as.character(read.csv(fn,sep=',',nrows=1,header=FALSE,skip = 3))
  data  <- read.csv(fn,sep=',',nrows=-1,header=FALSE,skip = 4,na.strings = "NAN")
  names(data)<-header
  attributes(data)$units<-unit
  attributes(data)$logger<-info[2]
  data$date<-strptime(data$TIMESTAMP,'%Y-%m-%d %H:%M:%S')
  return(data)
}

#' Read Metadata 
#' Read the metadata. The metadata required is:
#' 
#' Sensor definition table with the following fields:
#'    serialNumber : Sensor serial number
#'    model        : Sensor Model, e.g. SGB16
#'    resistance   : heater resistance in [Ohm]
#'    comment      : any comment
#'   
#' Sensor installation table with the following fields:
#'    label            : user defined identifier
#'    logger           : id of the Campbell Logger
#'    channel          : Channel the sensor is connected to 1:8
#'    serialNumber     : Sensor serial number (needs to match to one in sensor definitions)
#'    stemDiameter     : in [m].Used to calculate stem Area
#'    stemtype         : one of 'woody', 'herbaceous' or 'hollow'
#'    installationDate : installation date, first day of valid data 
#'    notes            : further comments to sensor installation
#'
#' This data.frame object returned by this function dataframe contains basic information about model inpiut parameters and is used for the check_parameters function
#' @param sensor_installation_file filename
#' @param sensor_definition_file   filename
#' @return a data.frame containg all sensor constants and installation parameters
#' 
read_metadata<-function(sensor_installation_file,sensor_definition_file){
  sensor_installation <-read.csv(sensor_installation_file, sep=',',header=TRUE)
  sensor_definition   <-read.csv(sensor_definition_file,   sep=',',header=TRUE)
  sensors <- merge(sensor_installation,sensor_definition,by='serialNumber')
  sensors$stemA <- (sensors$stemDiameter/2)^2*pi
  return(sensors)
}


#### BASIC FUNCTION DEFINITION FOR SAPFLOW CALCULATION

get_pin <-function(vin,res) vin^2/res #[W]
get_Qv  <-function(AH,BH,stemA,dx,Kst) (BH-AH)*stemA*Kst/(dx*0.04) #[W]
get_Qr  <-function(CH,Ksh) CH*Ksh  #[W]
get_Qf  <-function(Pin,Qv,Qr) Pin-Qv-Qr  #[W]

get_KsH_apparent <-function(CH,Pin,Qv) { #[W/mV]
  KsH_apparent<-NA
  KsH_apparent[CH>0] <- (Pin[CH>0]-Qv[CH>0])/CH[CH>0]
  return (KsH_apparent)
} 

get_Mflow <-function(Qf,dT) { # [g/s]
  CpH2O<-4.186 #[J/g/K] heat capacity of water
  return(Qf/(dT*CpH2O)) 
} 

get_dT <-function(CH,AH,threshold_dT) {
  dT<-(CH+AH)/2/0.04
  dT[ (CH+AH)/2/0.04 < threshold_dT ]<-threshold_dT
  return (dT)
}

#' Estimate Ksh dynamically by taking into account the nighttime values of the last 48h
#' Implementation inspired by xls datasheet by R.Zweifel
nightly_ksh<-function(cdate,date,Ksh_apparent,Ksh_timerange){
  ksh<-mean(Ksh_apparent[date$h>Ksh_timerange[1] & date$h<Ksh_timerange[2] & date > (cdate-48*3600)])
  if (is.na(ksh)) return (1)
  return (ksh)
} 

get_Ksh<-function(Ksh_apparent,date,Ksh_timerange,Ksh_offset){
  Ksh<-sapply(date,FUN=nightly_ksh,date,Ksh_apparent,Ksh_timerange)
  Ksh<-Ksh-Ksh_offset
  return(Ksh)
}

get_Flow <-function(Mflow) Mflow*3600     # [g/h]

#' Flow filter function
#' These filters screen, flag and replace suspicous values exceeding low flow or high flow conditions
#' @param MFlow current flow rate in []
#' ...
#' @return a data.frame object containing the data with units and logger name as attribute
filter_flow <-function(Mflow,Pin,Qf,dT,stemA,dTmin,max_flowrate){
  Mflow_flt<-Mflow
  flag=rep(NA,length(Mflow))
  #Check for low flow
  Mflow_flt[ Qf < (0.2* Pin) & dT < dTmin ] <- 0  # Low Flow Qf is energy transported by sap
  flag     [ Qf < (0.2* Pin) & dT < dTmin ] <- 'L'
  Mflow_flt[ Qf < 0 ] <- -0.0001		
  flag     [ Qf < 0 ] <- 'L'
  #Check for High flow
  Fmax<-max_flowrate*stemA*10000 # convert stemA to cm2
  Mflow_flt[ Mflow > Fmax/3600 ] <- Fmax/3600
  flag     [ Mflow > Fmax/3600 ] <- 'H'
  return (list(Mflow_flt*3600,flag))
}

#' get default settigs for the function parameters
get_default_settings<-function(){
  # SETTINGS
  settings<- list(
    Ksh_timerange = c(2,6), # time in H
    Ksh_offset    = 0.02,    # define manually from graph
    max_flowrate  = 600,     # MAx flow
    threshold_dT  = 1.5, # for dT calculation
    dTmin  = 0.75 # for dT calculation
  )
  return (settings)
}

##### Main Claulation function

#' calculate sapflow of dynagate
#' This function performs all the steps to calculate sap flow from the input parameters
#' @param date  posixlt timestamp
#' @param CH    Sensor value [mV]
#' @param AH    Sensor value [mV]
#' @param BH    Sensor value [mV]
#' @param Vin   Sensor value [V]
#' @param res   heating resistor [Ohm]
#' @param stemA stem area [m2]
#' @param sensormodel sensor model, eg. SGB25
#' @param stemtype one of 'woody','herbaceous','hollow'
#' @param settings list of parameters to set Ksh_offset,max_flowrate,threshold_dT,dTmin
#' ...
#' @return a data.frame object containing the calculated data. units as attribute

calc_gageflow<-function(date,CH,AH,BH,Vin,res,stemA,sensormodel,stemtype,settings){
  #CONSTANTS
  Kst<- c(0.42,0.54,0.28) # [W/m*K] Woody stem 0.42 0.54 herbaceous 0.28 hollow
  names(Kst)<-c('woody','herbaceous','hollow')
  gapDx<- c(1,1,3,4,4,4,5,5,7,10,10,13,15)/1000 # [m]
  names(gapDx)<-c('SGA2','SGA3','SGA5','SGA9','SGA10','SGA13','SGB16','SGB19','SGB25','SGB35','SGB50','SGB70','SGB100')
  # Calculation
  Pin          <- get_pin(Vin,res)
  Qv           <- get_Qv(AH,BH,stemA,gapDx[sensormodel],Kst[stemtype])
  Ksh_apparent <- get_KsH_apparent(CH,Pin,Qv)
  Ksh          <- get_Ksh(Ksh_apparent,date,settings[['Ksh_timerange']],settings[['Ksh_offset']])
  Qr           <- get_Qr(CH,Ksh)
  Qf           <- get_Qf(Pin,Qv,Qr)
  dT           <- get_dT(CH,AH,settings[['threshold_dT']])
  Mflow        <- get_Mflow(Qf,dT) # [g/h]
  flow         <- get_Flow(Mflow)
  ff           <- filter_flow(Mflow,Pin,Qf,dT,stemA,settings[['dTmin']],settings[['max_flowrate']])
  flow_filtered<-ff[[1]]
  flow_flag    <-ff[[2]]
  
  sfdf<-data.frame(cbind(CH,AH,BH,Vin,Pin,Qv,Qr,Qf,dT,Ksh_apparent,Ksh,Mflow,flow,flow_filtered))
  sfdf$flow_flag<-flow_flag
  csort<-c('date',names(sfdf))
  sfdf$date<-date
  sfdf<-sfdf[,csort]
  attributes(sfdf)$units<-c('[Y-m-d H:M:S]','[mV]','[mV]','[mV]','[V]','[W]','[W]','[W]','[W]','[W/mV]', '[K]',	'[g/s]', '[g/h]', '[g/h]','[]')
  return(sfdf)
}


#' Calculate sapflow from loggerdata as setup using the SCCII CR1000 sapflow program
#'
dynagageSapflow<-function(logdata,sensors,logger,channel,settings){
  csensor<-sensors[sensors$logger==logger & sensors$channel==channel,]
  gagecols <- c('date',sprintf ("%s_gage%02i",c('CH','AH','BH','Vin'),channel))
  clog<-logdata[,gagecols]
  names(clog)<-c('date','CH','AH','BH','Vin')
  if (sum(is.na(clog$CH))==length(clog$CH)) { 
    print(sprintf("Error: No data for sensor %02i on Logger %s",channel,logger))
    return (NULL)
  }
  clog$date<-strptime(clog$date,'%Y-%m-%d %H:%M:%S')
  sfdata<-calc_gageflow(clog$date,clog$CH,clog$AH,clog$BH,clog$Vin,csensor$res,csensor$stemA,csensor$model,csensor$stemtype,settings)
  attributes(sfdata)$label<-csensor$label
  return(sfdata)
}


###### PLOTS
plot_flow<-function(sf,...){
  par(mar=c(5.1,4.1,4.1,4.1))
  main=''
  if ('label' %in% names(attributes(sf))) main=attributes(sf)$label
  plot(sf$date,sf$flow,ylim=c(-100,300),ylab='[g/h]',type='l',las=1, xlab=NA,col='grey60',bty='u', main=main, ...)
  lines(sf$date,sf$flow_filtered)
  segments(0,0,1e22,0,lty=1,col='black')
  par(new=TRUE)
  plot(sf$date,sf$dT,col='red',ylim=c(-5,5),ylab=NA,type='l',las=1, xlab=NA,bty='n',xaxt='n',yaxt='n', ...)
  axis(side=4,las=1,)
  mtext(side=4,'dT [C]',line=2)
  #segments(0,0,1e22,0,lty=2,col='#FF000066')
  legend('top',col=c('grey60','black','red'),lty=1,c('flow','flow (filtered)','dT axial'),bty='n',horiz=TRUE)
  data_per_hour<-length(sf$date[ sf$date>=sf$date[1] & sf$date< (sf$date[1]+3600)])
  legend('bottomright',
         c(sprintf('Total flow: %0.2f g', sum(sf$flow)/data_per_hour),
           sprintf('Total flow (filtered): %0.2f g', sum(sf$flow_filtered)/data_per_hour)),
         bty='n'
         )
}

plot_voltage<-function(sf,...){
  par(mar=c(5.1,4.1,4.1,4.1))
  main=''
  if ('label' %in% names(attributes(sf))) main=attributes(sf)$label
  plot(sf$date,sf$CH,ylim=c(-0.1,0.35),ylab='[mV]',type='l',las=1, lwd=2, xlab=NA,col='blue',bty='u',main=main, ...)
  segments(0,0,1e22,0,lty=1,col='grey60')
  lines(sf$date,sf$AH,col='red')
  lines(sf$date,sf$BH,col='darkgreen')
  par(new=TRUE)
  ylim=mean(sf$Vin,na.rm = TRUE)*c(0.95,1.05)
  plot(sf$date,sf$Vin,ylim=ylim,ylab=NA,type='l',las=1, xlab=NA,col='black',bty='n',xaxt='n',yaxt='n', ...)
  axis(side=4,las=1,)
  mtext(side=4,'Vin [V]',line=2)
  legend('top',col=c('blue','red','darkgreen','black'),lty=1,lwd=c(2,1,1,1),c('CH','AH','BH','Vin'),bty='n',horiz=TRUE)
}

plot_Ksh<-function(sf,...){
  par(mar=c(5.1,4.1,4.1,4.1))
  main=''
  if ('label' %in% names(attributes(sf))) main=attributes(sf)$label
  plot(sf$date,sf$Ksh_apparent,ylim=c(0,10),ylab='[W/mV]',type='l',las=1, xlab=NA,col='black',bty='u',main=main,...)
  lines(sf$date,sf$Ksh,col='blue',lwd=2)
  legend('top',col=c('black','blue'),lty=1,lwd=c(1,2),c('Ksh apparent','Ksh used'),bty='n',horiz=TRUE)
}

# Generate a random test dataset
# ##Test data
# load_dynagage_demo<-function(){
#   sensor_definition<-data.frame(serialNumber=c(12345:12350),model='SGB19',resistance=rnorm(6,40,2),comment=sprintf('demo%i',1:6))
#   sensor_installation<-data.frame(label=sprintf('DYN%02i',1:6),logger="A",channel=1:6,serialNumber=c(12345:12350),stemDiameter=rnorm(6,18,1),stemtype='woody',installationDate=NA,notes=NA)
#   sensors <- merge(sensor_installation,sensor_definition,by='serialNumber')
#   sensors$stemA <- (sensors$stemDiameter/2)^2*pi
#   logdata<- data.frame(
#     date=strftime(Sys.time()+seq(120,1200,120),"%Y-%m-%d %H:%M:00"),
#     CH_gage01 =rnorm(10,0.16,0.1),
#     AH_gage01 =rnorm(10,0.16,0.1),
#     BH_gage01 =rnorm(10,0.16,0.1),
#     Vin_gage01=rnorm(10,4.5,0.2),
#     CH_gage02 =rnorm(10,0.16,0.1),
#     AH_gage02 =rnorm(10,0.16,0.1),
#     BH_gage02 =rnorm(10,0.16,0.1),
#     Vin_gage02=rnorm(10,4.5,0.2),
#     CH_gage03 =rnorm(10,0.16,0.1),
#     AH_gage03 =rnorm(10,0.16,0.1),
#     BH_gage03 =rnorm(10,0.16,0.1),
#     Vin_gage03=rnorm(10,4.5,0.2))
#   sensors <<- sensors
#   logdata <<- logdata 
# }

#example:
# #Load Demo dataset
# load_dynagage_demo()
#settings<-get_default_settings()
# sf<-dynagageSapflow(logdata,sensors,"A",3,settings)
# plot_flow(sf)
# plot_voltage(sf)
# plot_Ksh(sf)

