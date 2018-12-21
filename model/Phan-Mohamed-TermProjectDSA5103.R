library(RColorBrewer)
library(gridExtra)
library(fiftystater) # state map
library(maps) #county map
library(ggplot2)
library(reshape2)
library(pls)#pls
library(caret)
library(glmnet) #ridge
library(elasticnet) #elasticnet
library(VIM)
library(mice)
##-------OKLAHOMA coord. loading...----------####
okmap = fifty_states[fifty_states$id=="oklahoma",]
okcounty = data.frame(lat=map("county","Oklahoma")$y,long=map("county","Oklahoma")$x)
##-------LOAD UIC volume and Earthquake data-###
##read csv files:
## UIC volume:
UICVolume <- read.csv('UICVolume.csv')
UICVolume$InjBotDepth <- as.numeric(UICVolume$InjBotDepth)
## Equakes:
Equake <- read.csv('Equake.csv')
## plot Oklahoma map for UIC wells and cum volume:
ggplot(data=okcounty,aes(x=long,y=lat),color="grey")+ geom_path(color="grey58",size=0.01)+
  geom_path(data=okmap,aes(x=long,y=lat),size=1.2)+
  geom_point(data=UICVolume,aes(y=Lat_Y,x=Long_X,size=Year.Volume,color=Year.Volume),alpha=1)+
  scale_color_gradient(low="lightblue",high="red")+
  theme_dark()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

## plot Oklahoma map for Equakes:
ggplot(data=okcounty,aes(x=long,y=lat),color="grey")+ geom_path(color="grey58",size=0.01)+
  geom_path(data=okmap,aes(x=long,y=lat),size=1.2)+
  geom_point(data=Equake,aes(y=Lat_Y,x=Long_X,size=prefmag,color=prefmag),alpha=1)+
  scale_color_gradient(low="lightblue",high="red")+
  theme_dark()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

##-------LOAD combined data for modeling-###

OkEquakeUIC <- read.csv('OkEquakeUIC.csv')
meltdata <- melt(OkEquakeUIC,id='id')
##histogram, boxplot,...
   ###numeric data:
   datanum <- OkEquakeUIC[,c(1,2,3,4,5,6,7,10,11,12,13)]
   meltdatanum <- melt(datanum,id='id')
   ###categorical data:
   datacat <- OkEquakeUIC[,c(1,8,9)]
   meltdatacat <- melt(datacat,id='id')
   ###histogram:
   ggplot(data=meltdatanum,aes(x=value,fill=variable)) +
     geom_histogram()+facet_wrap(~variable,scale='free') # hist of num vars
   ggplot(data=meltdatacat,aes(x=value,fill=variable)) +
     geom_histogram(stat='count')+facet_wrap(~variable,scale='free') # hist of cat vars
   ###boxplot:
   ggplot(data=meltdatanum,aes(y=value,fill=variable)) +
     geom_boxplot()+facet_wrap(~variable,scale='free') # hist of num vars

   ##missing, inputation,...
   missing_VIM <- aggr(OkEquakeUIC,col=c('steelblue','yellow'),prob=T,numbers=F,sortVars=T,gap=2,cex.axis=0.55,bars=T)
 
  OkQuakeUICmiss <- OkEquakeUIC[,c(4,5,6,7)]
  OkQuakeUICimp <- mice(OkQuakeUICmiss,m=10)
  OkQuakeUICimp <- complete(OkQuakeUICimp,10)

  ####PLOTS###:
  windows()
  par(mfrow=c(2,4))
  hist(OkEquakeUIC$InjTopDepth)
  hist(OkQuakeUICimp$InjTopDepth)
  hist(OkEquakeUIC$InjBotDepth)
  hist(OkQuakeUICimp$InjBotDepth)
  hist(OkEquakeUIC$TotalCumVolume)
  hist(OkQuakeUICimp$TotalCumVolume)
  hist(OkEquakeUIC$AvgPressurePSI)
  hist(OkQuakeUICimp$AvgPressurePSI)
  
   ##feature engineering,..
  ##mapping imputation:
  OkEquakeUIC$InjTopDepth <- OkQuakeUICimp$InjTopDepth
  OkEquakeUIC$InjBotDepth <- OkQuakeUICimp$InjBotDepth
  OkEquakeUIC$TotalCumVolume <- OkQuakeUICimp$TotalCumVolume
  OkEquakeUIC$AvgPressurePSI <- OkQuakeUICimp$AvgPressurePSI 
  #new features:
  ## thickness:
  OkEquakeUIC$Thickness <- OkEquakeUIC$InjBotDepth - OkEquakeUIC$InjTopDepth
  ## InjVol / feet:
  OkEquakeUIC$FracPressure <- OkEquakeUIC$InjBotDepth*0.433 + OkEquakeUIC$AvgPressurePSI
  ## Depth difference:
  OkEquakeUIC$DepthDiff <- OkEquakeUIC$QuakeDepth*3280.84 - OkEquakeUIC$InjBotDepth
  
##-------MODELING PART-----###
  ###---@-----------@----###
   ####---------------####
    #####---zzz----#####
     ######-----######
      #######-######
       ###########
  
##cross-validation:
  OkEquakeUIC<-OkEquakeUIC[,-c(8,9)]
   control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
   grid <- expand.grid(ncomp = 1:10)
   
   
###PLS###
   ###
   
   model.pls <- train(data=OkEquakeUIC,Avgmagnitude~.,
                      method = "pls", trControl = control,
                      tuneGrid = grid)
   plot(model.pls)
   model.pls
   # Result:
   #ncomp  RMSE       Rsquared     MAE      
   #1     0.4232620  0.001471229  0.3066016
   #2     0.4229289  0.019572933  0.3057933
   #3     0.4221228  0.022701189  0.3040201
   #4     0.4213921  0.024419204  0.3048536
   #5     0.4219125  0.025480897  0.3053657
   #6     0.4218676  0.026300125  0.3052053
   #7     0.4166244  0.045653230  0.3005771
   #8     0.2761265  0.573424598  0.2048703
   #9     0.2440894  0.667498946  0.1803778
   #10     0.2436752  0.668723122  0.1805082
   
   #RMSE was used to select the optimal model using the smallest value.
   #The final value used for the model was ncomp = 10.
   model.pls$results
   #ncomp      RMSE    Rsquared       MAE      RMSESD  RsquaredSD       MAESD
   #  10 0.2436752 0.668723122 0.1805082 0.01887480 0.056724385 0.01071111

###RIDGE### (glmnet)
   ####

   grid <- expand.grid(alpha = 0, lambda = seq(0,0.1, by = 0.01))
   model.ridge <- train(data=OkEquakeUIC,Avgmagnitude~.,
                      method = "glmnet", trControl = control,
                      tuneGrid = grid)
   ####
   lambdas <- 10^seq(3, 0, by = -.05)
   x <- as.matrix(OkEquakeUIC[,c(1:7,10:12)])
   y <-as.matrix(OkEquakeUIC[,13])
   cv.ridge <-cv.glmnet(x,y,alpha=0,lambda=lambdas)
   
   plot(cv.ridge)
   model.ridge
   #lambda  RMSE       Rsquared   MAE      
   #0.00    0.2209861  0.6762411  0.1652221
   #0.01    0.2209861  0.6762411  0.1652221
   #0.02    0.2209861  0.6762411  0.1652221
   #0.03    0.2213305  0.6759638  0.1654116
   #..
   #Tuning parameter 'alpha' was held constant at a value of 0
   #RMSE was used to select the optimal model using the smallest value.
   #The final values used for the model were alpha = 0 and lambda = 0.02.
   model.ridge$result
   #1      0   0.00 0.2465873 0.6664280 0.1818106 0.01825642 0.05318186 0.01059382
   #2      0   0.01 0.2465873 0.6664280 0.1818106 0.01825642 0.05318186 0.01059382
   #3      0   0.02 0.2465873 0.6664280 0.1818106 0.01825642 0.05318186 0.01059382
   #4      0   0.03 0.2466412 0.6664086 0.1818324 0.01827405 0.05315577 0.01059950
   #5      0   0.04 0.2481078 0.6655862 0.1825870 0.01795833 0.05170978 0.01045980
   #6      0   0.05 0.2498597 0.6644709 0.1835550 0.01780124 0.05043956 0.01038147
   #7      0   0.06 0.2518073 0.6630987 0.1846681 0.01777921 0.04938795 0.01035199
   #8      0   0.07 0.2538892 0.6615122 0.1859051 0.01784958 0.04851955 0.01035215
   #9      0   0.08 0.2560527 0.6597580 0.1871731 0.01798571 0.04780771 0.01038133
   #10     0   0.09 0.2582681 0.6578592 0.1884732 0.01816047 0.04722487 0.01038400
   #11     0   0.10 0.2605073 0.6558458 0.1897913 0.01835848 0.04675139 0.01040046
   
###LASSO### (alpha =1)
   ####
   grid <- expand.grid(alpha = 1, lambda = seq(0,0.01, by = 0.001))
   model.lasso <- train(data=OkEquakeUIC,Avgmagnitude~.,
                        method = "glmnet", trControl = control,
                        tuneGrid = grid)
   model.lasso
   #lambda  RMSE       Rsquared   MAE      
   #0.000   0.2453813  0.6662129  0.1805495
   #0.001   0.2453037  0.6663607  0.1804561
   #0.002   0.2451258  0.6667317  0.1802572
   #0.003   0.2449831  0.6670586  0.1801132
   #0.004   0.2448600  0.6673854  0.1800253
   #0.005   0.2447747  0.6676524  0.1799673
   #0.006   0.2447445  0.6678265  0.1799609
   #0.007   0.2447629  0.6679169  0.1799939
   #0.008   0.2448251  0.6679355  0.1800533
   #0.009   0.2449240  0.6678943  0.1801415
   #0.010   0.2450566  0.6677993  0.1802519
   
   #Tuning parameter 'alpha' was held constant at a value of 1
   #RMSE was used to select the optimal model using the smallest value.
   #The final values used for the model were alpha = 1 and lambda = 0.006.
   model.lasso$result
   #alpha lambda      RMSE  Rsquared       MAE      RMSESD RsquaredSD       MAESD
   
   #1      1  0.000 0.2453813 0.6662129 0.1805495 0.01687285 0.04359325 0.008735419
   #2      1  0.001 0.2453037 0.6663607 0.1804561 0.01673920 0.04338646 0.008765117
   #3      1  0.002 0.2451258 0.6667317 0.1802572 0.01642224 0.04282345 0.008805520
   #4      1  0.003 0.2449831 0.6670586 0.1801132 0.01611058 0.04225902 0.008831724
   #5      1  0.004 0.2448600 0.6673854 0.1800253 0.01583161 0.04178150 0.008847741
   #6      1  0.005 0.2447747 0.6676524 0.1799673 0.01557882 0.04133097 0.008867475
   #7      1  0.006 0.2447445 0.6678265 0.1799609 0.01533808 0.04088015 0.008892517
   #8      1  0.007 0.2447629 0.6679169 0.1799939 0.01511827 0.04046068 0.008903959
   #9      1  0.008 0.2448251 0.6679355 0.1800533 0.01491297 0.04008142 0.008909538
   #10     1  0.009 0.2449240 0.6678943 0.1801415 0.01472045 0.03971420 0.008906512
   #11     1  0.010 0.2450566 0.6677993 0.1802519 0.01454566 0.03938144 0.008906278
   

###ELASTICNET###:
    ########
   grid <- expand.grid(lambda = seq(0,0.005, length=20), fraction =seq(0.5,1, length = 10))
   model.enet <- train(data=OkEquakeUIC,Avgmagnitude~.,
                        method = "enet", trControl = control,
                        tuneGrid = grid)
   plot(model.enet)
   model.enet
   #lambda  fraction  RMSE       Rsquared   MAE
   #0.0    0.8888889   0.2442909  0.6678670   0.1800885
   #RMSE was used to select the optimal model using the smallest value.
   #The final values used for the model were fraction = 0.8888889 and lambda = 0
   model.enet$result
   #lambda fraction      RMSE     Rsquared       MAE      RMSESD RsquaredSD       MAESD
   #0.0   0.8888889 0.2442909 0.6678670 0.1800885 0.01532513 0.04675756 0.008034092

###BEST MODEL - LASSO: ###:
   
