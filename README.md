# OKEarthquake
## Inspiration
This project studies the relationship between the earthquake’s frequency and magnitudes, and the wastewater that is being reinjected in underground wells for some years. Using data science and analytics different tools, the study aims at linking the different intensive relevant data, understanding, visualizing and exploring the data, and constructing different models to try and predict the earthquakes magnitudes based on different characteristics of the wastewater such as the injected water volume, the distances between the injection wells and the earthquakes locations, the dates of the injection, the dates of measuring the earthquakes magnitudes, and the formation used for reinjection. This study also tries to prove or disprove the beforementioned relationship and answer questions such as: does the reinjection of the wastewater cause the increasing earthquakes in Oklahoma? and what are the attributes that have the highest impact? Which factors should we focus on, so we can reduce that harmful effect?

![image] (model/UICVolume.png)
## Method
Supervised algorithms encompassing PLS, Ridge, LASSO and ElasticNet to investigate Oklahoma earthquake data (OGS source) and UIC Well Volume (OCCC source).
## Data Source
[OGS](http://www.ou.edu/ogs/research/earthquakes/catalogs)

[OCCC](http://www.occeweb.com/og/oghome.htm)

## Code
```R
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
library(EnvStats) # 
library(MASS) # 
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
   #Tuning parameter 'alpha' was held constant at a value of 1
   #RMSE was used to select the optimal model using the smallest value.
   #The final values used for the model were alpha = 1 and lambda = 0.006.
   model.lasso$result
   #alpha lambda      RMSE  Rsquared       MAE      RMSESD RsquaredSD       MAESD
   
   #1      1  0.000 0.2453813 0.6662129 0.1805495 0.01687285 0.04359325 0.008735419
   #2      1  0.001 0.2453037 0.6663607 0.1804561 0.01673920 0.04338646 0.008765117
###ELASTICNET###:
    ########
   grid <- expand.grid(lambda = seq(0,0.005, length=20), fraction =seq(0.5,1, length = 10))
   model.enet <- train(data=OkEquakeUIC,Avgmagnitude~.,
                        method = "enet", trControl = control,
                        tuneGrid = grid)
   plot(model.enet)
   model.enet
   #RMSE was used to select the optimal model using the smallest value.
   #The final values used for the model were fraction = 0.8888889 and lambda = 0
   model.enet$result
   #lambda fraction      RMSE     Rsquared       MAE      RMSESD RsquaredSD       MAESD
   #0.0   0.8888889 0.2442909 0.6678670 0.1800885 0.01532513 0.04675756 0.008034092

###BEST MODEL - LASSO: ###:
   grid <- expand.grid(alpha = 1, lambda = 0.006)
   x <- data.matrix(OkEquakeUIC[,1:13])
   y <- OkEquakeUIC[,14]
   model.best <- glmnet(x,y)
                       
   ncvTest(model.best)
   par(mfrow=c(2,2))
   
   plot(model.best)
   library(plotmo)
   plotres(model.best)
```
