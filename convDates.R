##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
## Function to convert variable "time_as_string" to date format in R
convDates <- function(datmat,acc=TRUE){
  require(xts)
  options(digits.secs=6)
  if (acc == TRUE){
    print('Commencing date convert for ACC-data')  
    t1<-Sys.time()
    print(paste('Time is now: ',t1))  
    timeAsString<-as.character(datmat[,15])
    n=length(timeAsString)
    temp<-c(rep(0,n))
    dateVar<-c(rep(0.0,n))
    for (i in 1:n){
      temp[i]<-paste(substr(timeAsString[i],start = 1,stop = 10),substr(timeAsString[i],start = 12,stop = 24))
    }
    dateVar<- temp
    #     <-xts(1:n,as.POSIXct(temp))
    t2<-Sys.time()
    print(paste('Date convert successful! Time spent: ',format(t2-t1), '.'))
    print('###################################################################')
    print(paste('Date values of length: ',length(dateVar),' - Is returned.'))
  }
  if (acc == FALSE){
    print('Commencing date convert for GPS-data')
    t1<-Sys.time()
    print(paste('Time is now: ',t1))  
    
    timeAsString<-as.character(datmat[,17])
    n=length(timeAsString)
    temp<-c(rep(0,n))
    dateVar<-c(rep(0.0,n))
    for (i in 1:n){
      temp[i]<-paste(substr(timeAsString[i],start = 1,stop = 10),substr(timeAsString[i],start = 12,stop = 24))
    }
    
    dateVar<- temp
    #     <-xts(1:n,as.POSIXct(temp))
    t2<-Sys.time()
    print(paste('Date convert successful! Time spent: ',format(t2-t1), '.'))
    print('###################################################################')
    print(paste('Date values of length: ',length(dateVar),' - Is returned.'))
    
  }
  
  return(invisible(dateVar))
}

##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
# Function to estimate features in data
fitArimaModels <- function(accmat,gpmat,forces,windowSize)
{
  require(moments)
  require(signal)
  sizes<-round((dim(accmat)[1])/(windowSize)) #No. of obs in each comp. window 
  sizes_R<-(dim(accmat)[1])/(windowSize) #W/o rounding
  size<-round((dim(accmat)[1])/(sizes_R)) #division of parts into roughly X minute windows

  #Check proper dimensions:
  maxsize<-(size*sizes)
  maxdat<-dim(accmat)[1]

  greatMat<-matrix(0,sizes,35*6)
for (l in 1:forces)  {
  

  require(forecast)
  require(xts)
  
  t1<-Sys.time()
  accDat<-convDates(accmat,acc = TRUE)
  gpDat<-convDates(gpmat,acc = FALSE)
  print(paste('* Completed date conversion'))
  
  timAccx<-as.POSIXlt(accDat)
  timAcc<-xts(1:length(accDat),as.POSIXct(accDat))
  timAcc<-zoo(accmat[,(l+6)][1:dim(accmat)[1]],timAcc)
  t2<-Sys.time()
  print(paste('* Accel. time conv. Completed, time est:',format(t2-t1)))
  
  timGPx<-as.POSIXlt(gpDat)
  timGP<-xts(1:length(gpDat),as.POSIXct(gpDat))
  timGP<-zoo(1:length(gpDat),timGP)
  t3<-Sys.time()
  print(paste('* GPS time conv. Completed, time est:',format(t3-t1)))
  
  ifelse(l==1,{
    ## Preparing datamatrices for model fit 
    mainMat<-matrix(0,sizes,37) #Enabling for a Maximum of 5 MA, 4 DIFF, 5 AR 
    colnames(mainMat)<-c("AR1","AR2","AR3","AR4","AR5","MA1","MA2","MA3","MA4","MA5","RMSE","SD"
                         ,"AIC","Sign","mag","mom1","mom2","mom3","mom4","Res0","Res25","Res50"
                         ,"AIC","ZCR","Sign","mag","mom1","mom2","mom3","mom4","Res0","Res25","Res50"
                         ,"Res75","Res100","Delta","Sp0","Sp25","Sp50","Sp75"
                         ,"Sp100","Intercept","SpeedREG","tInter","tBeta","Time","TimeElapsed")
    colnames(mainMat)<-paste(names(accmat)[l+6],colnames(mainMat),sep="")  
  },{
     mainMat<-matrix(0,sizes,26) #Enabling for a Maximum of 5 MA, 1 DIFF, 5 AR and 1 XREG, mean_r,sd_r,aic,order
    colnames(mainMat)<-c("AR1","AR2","AR3","AR4","AR5","MA1","MA2","MA3","MA4","MA5","RMSE","SD"
                         ,"AIC","Sign","mag","mom1","mom2","mom3","mom4","Res0","Res25","Res50"
                         ,"AIC","ZCR","Sign","mag","mom1","mom2","mom3","mom4","Res0","Res25","Res50"
                         ,"Res75","Res100","Delta")
    colnames(mainMat)<-paste(names(accmat)[l+6],colnames(mainMat),sep="")  
  })

  j<-1
  for(i in 1:sizes){
    
    t4<-Sys.time()
    print(paste('* Initiating ARIMA Prep, time est:',format(t4-t1)))
    if(i==1){
      timAccp<-timAccx[1:size]  
      timAcct<-timAcc[1:size] 
      scalfact<-floor(ifelse(length(accmat$accX)%%length(gpmat$speedInMS)==0,
                             length(accmat$accX)/length(gpmat$speedInMS),
                             length(accmat$accX)/length(gpmat$speedInMS)+1))
      gpSpeed.all<-interp(gpmat$speedInMS,scalfact)
      acclen<-length(accmat$accX)
      gpSpeed<-gpSpeed.all[1:size]
    }
    if(i!=1){
      tryCatch({
      timAccp<-timAccx[(j*size):(i*size)]
      timAcct<-timAcc[(j*size):(i*size)] 
      gpSpeed<-gpSpeed.all[(j*size):(i*size)]
      j<-j+1
      },error=function(e){
        timAccp<-timAccx[((j-1)*size):length(timAccx)]
        timAcct<-timAcc[((j-1)*size):length(timAcc)]
        gpSpeed<-gpSpeed.all[((j-1)*size):length(timAcc)]
        print(paste("WARNING: Dimensionality correction, iteration: ",i))
      })
    }
    
    print(paste('* Fitting ARIMA, time est:',format(t4-t1)))
    
    #Checking if a refitting procedure form an old ARIMA order should take place
    if(missing(refitflag) && missing(refitMat)){

          modX<-auto.arima(timAcct,max.p=5,max.q=5,max.d=4)
    }
    else{
      modX<-arima(timAcct,order=c(order,1,order),include.mean=FALSE,method="CSS")
    }
    ## Filling in the parameters in the main matrix
    if(length(modX$model$theta)!=0){
      mainMat[i,1:length(modX$model$theta)]<-modX$model$theta #MA-parameters
    }
    if(length(modX$model$phi)!=0){
      mainMat[i,5+1:length(modX$model$phi)]<-modX$model$phi #AR-parameters
    }
    
    #mean of residuals
    mainMat[i,11]<-modX$sigma2
    
    #SD of residuals
    mainMat[i,12]<-sqrt(modX$sigma2)
    
    #AIC of model
    mainMat[i,13]<-modX$aic

    #No. of Sign changes in signal
    mainMat[i,14]<-sum(diff(sign(timAcct))!=0)

    #Zero-Crossing Rate
    mainMat[i,15]<-sum(diff(sign(diff(timAcct)*timAcct[1:(length(timAcct)-1)])) < 0)/(length(timAcct)-1)

    #Magnitude vector
    mainMat[i,16]<-sqrt(sum(timAcct)^2)

    #calculate the sample spectrum
    tempspec<-spectrum(ts(timAcct,start=0,end=round(size/200),frequency=200),plot=FALSE)
  
    #Moments, mean
    mainMat[i,17]<-moment(tempspec$spec,order=1,absolute = FALSE,central = TRUE)
    
    #Moments, std. dev
    mainMat[i,18]<-moment(tempspec$spec,order=2,absolute = FALSE,central = TRUE)

    #Moments, skewness
    mainMat[i,19]<-moment(tempspec$spec,order=3,absolute = FALSE,central = TRUE)

    #Moments, kurtosis
    mainMat[i,20]<-moment(tempspec$spec,order=4,absolute = FALSE,central = TRUE)

    #percentiles Residuals
    mainMat[,21:25]<-quantile(modX$residuals)

    #DIFF operator
    tryCatch({mainMat[,25]<-modX$model$Delta}
             ,error=function(e){mainMat[,25]<-0})
    tryCatch({mainMat[,26]<-modX$model$Delta}
             ,error=function(e){mainMat[,26]<-0})

    # Conduct only following assignments ONCE per day of aggregated sampling
    ifelse(l==1,{
      #percentiles Speed
      mainMat[i,27:31]<-quantile(gpSpeed) #Bug speed  
      
      linmod<-lm(1:length(gpSpeed)~gpSpeed)
      
      #Regressional coefficients
      mainMat[i,32:33]<-linmod$coefficients
      
      #T-statistics
      mainMat[i,34:35]<-t(summary(linmod)$coefficients[,3])
      
      #Add time tracking
      mainMat[i,36]<-as.POSIXct(timAccp[1]) #To convert back to origin: as.POSIXct(mainMat[i,35],origin="1970-01-01",tz="CET")
      timEl<-t4-t1
      units(timEl)<-"mins" #defining format
      mainMat[i,37]<- timEl # Time elapsed
      
    },print(paste("Force",l," - No speed added to matrix.")))
    
    t5<-Sys.time()
    print(paste('* ARIMA model fitted, time est:',format(t5-t1),'Iteration no:',i, 'Out of: ',sizes))
    print(paste('Coefficients being: ',modX$coef))
    print(paste('Present time in data: ',timAccp[1], "Remaining iter:",size*i,"Out of: ",maxsize))
    
  }
  
  #Combining all the juicies of all matrices into - the Great Matrix
  if(l==1){
    greatMat<-cbind(NA,mainMat)
  }
  else {
    greatMat<-cbind(greatMat,mainMat)
  }
  

  }
  print(paste('** COMPLETED ARIMA FIT for multiple functions and forces, no: ',l))
  greatMat<-greatMat[,-1]
  return(invisible(greatMat))
}

 
findMinimumTimeDist <- function(accmat,gpmat,orderflag){ 
accDat<-convDates(cdatacc,acc=T)
gpDat<-convDates(cdatgp,acc=F)

acc_tim<-as.POSIXlt(accDat)
timGPx<-as.POSIXlt(gp_tim)

intObs<-integer()
t1<-Sys.time()
if(orderflag==1){
for (i in 1:length(intObs.tim)){
  intObs[i]<-which(pmin(abs(acc_tim-intObs[i]))==min(pmin(abs(acc_tim-intObs[i])))
                       ,arr.ind=TRUE)   
  print(paste("Iteration: ",i,"Out of: ",length(intObs.tim)))
}

}else{
  
intObs.idxAlt<-integer()
for (i in 1:length(acc_tim)){
  intObs[i]<-which(pmin(abs(acc_tim[i]-intObs))==min(pmin(abs(acc_tim[i]-intObs)))
                          ,arr.ind=TRUE)   
  t2<-Sys.time()
  print(paste("Iteration: ",i,"Out of: ",length(acc_tim)," Time elapsed: ",t2-t1))
}
}

return(invisble(intObs))
}

convSpeed <- function(accmat,gpmat,forces){
  
  ndatacc<-accmat #Temporary fix.
  ndatgp<-gpmat   #Temporary fix.
  
  
  
  timeGP<-convDates(acc=F,ndatgp)
  timeACC<-convDates(acc=T,ndatacc)
  
  acc_tim<-as.POSIXlt(timeACC)
  gp_tim<-as.POSIXlt(timeGP)
  
  maxlen<-floor(length(ndatacc$accX))
  corSpd<-matrix(0,maxlen,1)
  row.names(corSpd)<-1:length(corSpd)
  
  for(i in 1:(maxlen))
  {
    print(i)
    tryCatch({
      corSpd[i]<-which(pmin(abs(acc_tim[(i*200)]-gp_tim))==min(pmin(abs(acc_tim[(i*200)]-gp_tim)))
                       ,arr.ind=TRUE)
      rownames(corSpd)[i]<-i*200
    }
    ,error=function(e){
      reslen<-length(ndatacc$accX[as.integer(row.names(corSpd)[i]):length(ndatacc$accX)])
      print(paste("RESIDUAL LENGTH: ",reslen))
      corSpd[i]<-which(pmin(abs(acc_tim[(reslen)]-gp_tim))==min(pmin(abs(acc_tim[(reslen)]-gp_tim)))
                       ,arr.ind=TRUE)
      rownames(corSpd)[i]<-i+reslen
    },warning=function(e){
      reslen<-length(ndatacc$accX[as.integer(row.names(corSpd)[i]):length(ndatacc$accX)])
      print(paste("RESIDUAL LENGTH: ",reslen))
      corSpd[i]<-which(pmin(abs(acc_tim[(reslen)]-gp_tim))==min(pmin(abs(acc_tim[(reslen)]-gp_tim)))
                       ,arr.ind=TRUE)
      rownames(corSpd)[i]<-i+reslen
    })
  }
  return(invisible(corSpd))
}

clc <- function() cat("\014")


cleanDat <- function(dat,apvflag){
  dat<-as.data.frame(dat) 
  
  if(apvflag==1){
    keepVars<-setdiff(names(dat),
                      c("accXDelta","accYDelta","accZDelta","rollDelta","pitchDelta","yawDelta","class"
                        ,"accXTimeElapsed"))  
    dat<-dat[,keepVars]
  }else{
    keepVars<-setdiff(names(dat),
                      c("accXTimeElapsed","accXTime"))
    dat<-dat[,keepVars]   
    #   "accXDelta","accYDelta","accZDelta","rollDelta","pitchDelta","yawDelta"
  }
  
  return(invisible(dat))
}

