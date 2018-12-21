# OKEarthquake
## Inspiration
This project studies the relationship between the earthquake’s frequency and magnitudes, and the wastewater that is being reinjected in underground wells for some years. Using data science and analytics different tools, the study aims at linking the different intensive relevant data, understanding, visualizing and exploring the data, and constructing different models to try and predict the earthquakes magnitudes based on different characteristics of the wastewater such as the injected water volume, the distances between the injection wells and the earthquakes locations, the dates of the injection, the dates of measuring the earthquakes magnitudes, and the formation used for reinjection. This study also tries to prove or disprove the beforementioned relationship and answer questions such as: does the reinjection of the wastewater cause the increasing earthquakes in Oklahoma? and what are the attributes that have the highest impact? Which factors should we focus on, so we can reduce that harmful effect?

## Method
Supervised algorithms encompassing PLS, Ridge, LASSO and ElasticNet to investigate Oklahoma earthquake data (OGS source) and UIC Well Volume (OCCC source).
## Data Source
[OGS](http://www.ou.edu/ogs/research/earthquakes/catalogs)
[OCCC](http://www.occeweb.com/og/oghome.htm)

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
```
