library(tidyverse)
## visiting rate
mob_cate=read.csv("C:/Users/liyaq/Downloads/Global_Mobility_Report.csv",header=T)
mob=read.csv("C:/Users/liyaq/Downloads/DL-us-m50.csv",header=T)
positive=read.csv("C:/Users/liyaq/Downloads/daily.csv",header=T)
mob_cate_us=mob_cate[which(mob_cate[,2]=="United States"),]
mob_cate_us$date = as.Date(mob_cate_us$date, format="%m/%d/%Y")
total_c=mob_cate_us%>%
filter(sub_region_1==mob_cate_us$sub_region_1[1])%>%
  filter(date>="2020-03-07")
state_c=mob_cate_us%>%
  filter(sub_region_1!=mob_cate_us$sub_region_1[1])%>%
  filter(sub_region_2==mob_cate_us$sub_region_2[1])%>%
  filter(date>="2020-03-07")
plot.ts(total_c[,6:11] , main = "", xlab = "")
## activity range
state_mod=mob%>%
  filter(admin2==mob$admin2[1])
state_mod=state_mod[-44,]
state_mod=gather(state_mod, date,mobility , X2020.03.01:X2020.05.02, factor_key=TRUE)
state_mod$date = as.Date(state_mod$date, format="X%Y.%m.%d")
state_mod=state_mod%>%
  filter(date>="2020-03-07"&date<="2020-04-26")
us_mod=state_mod%>%
  group_by(date)%>%
  summarize(mobility=median(mobility))
plot.ts(us_mod[,2] , main = "", xlab = "")
##positive increase rate
positive$date=as.Date(as.character(positive$date), format="%Y%m%d")
pos=positive%>%
  filter(date>="2020-03-07"&date<="2020-04-26")
pos=pos[order(pos$date),]
pos$rate=pos$positiveIncrease/pos$positive
pos[which(is.na(pos$rate)),"rate"]=0

us_pos=pos%>%
  group_by(date)%>%
  summarize(positive=mean(positive,na.rm=T),increase=mean(positiveIncrease,na.rm = T),rate=mean(rate,na.rm = T))
plot.ts(us_pos[,2:4] , main = "", xlab = "")
## merge data
## missing 4/20 data
state_mod%>%
  group_by(admin1)%>%
  summarize(n=n())

pos%>%
  group_by(state)%>%
  summarize(n=n())
state_c%>%
  group_by(sub_region_1)%>%
  summarize(n=n())

state_mod%>%
  group_by(date)%>%
  summarize(n=n())
pos%>%
  group_by(date)%>%
  summarize(n=n())
state_c%>%
  group_by(date)#%>%
  summarize(n=n())
## state name abb in positive to full name
pos$state1=state.name[match(pos$state,state.abb)]
pos[which(pos$state=="DC"),"state1"]="DC"
state_c$sub_region_1=as.character(state_c$sub_region_1)
state_c[which(state_c$sub_region_1=="District of Columbia"),"sub_region_1"]="DC"
state_mod$admin1=as.character(state_mod$admin1)
state_mod[which(state_mod$admin1=="Washington, D.C."),"admin1"]="DC"
pos_f=pos[,c("date","state1","positive","positiveIncrease","rate")]
state_c_f=state_c[,3:11]
state_mod_f=state_mod[,c("admin1","date","mobility")]
a=merge(state_c_f,pos_f,by.x=c("sub_region_1","date"),by.y=c("state1","date"),all.x=T)
data_f=merge(a,state_mod_f,by.x=c("sub_region_1","date"),by.y=c("admin1","date"),all.x=T)
data_f=data_f[,-3]
colnames(data_f)[1]="state"
#### plot and simple statistics
## how changes in us level
library(dplyr)
b=merge(total_c[,5:11],us_pos,by="date",all.x=T)
us_mod=as.data.frame(us_mod)
us_data=merge(b,us_mod,by="date")
us_data$rate=us_data$rate*100
df <- us_data %>%
  select(date,retail_recreation,grocery_pharmacy,parks,transit_stations,workplaces,residential,mobility,rate) %>%
  gather(key = "variable", value = "value", -date)
ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()
###changes in state total
data_f=data_f[order(data_f$date),]
plot.ts(data_f[,c(3:8,11:12)] , main = "", xlab = "")  

### clustering
library(MASS)
library(mclust) 
library(urca)   
library(vars)
library(strucchange)
library(lmtest)
  data_cc<- data_f[complete.cases(data_f),]
  y=data_cc
  y <- as.matrix(y[,c(3,11)])
  nDepVar <- ncol(y)  
  nPersons=51
  MaxLag=3
  ind=unique(data_cc[,1])
  RegrModel_per_pers = matrix(0, nPersons, (nDepVar*nDepVar)*MaxLag+nDepVar)
  n=1
  RegrCoeff_pers = matrix(0, nDepVar, (nDepVar)*MaxLag+1)
  for(pers in ind){
    
    
    v <- VAR(y[which(data_cc[,1]==pers), ], type = "const", #  const is to include an intercept
             season = NULL, p=MaxLag,
             lag.max=MaxLag)
    
    
    Terms = length(v$varresult[[1]]$coefficients) - 1
    
    for(vas in 1:nDepVar){
      RegrCoeff_pers[vas, 1] = tail(v$varresult[[vas]]$coefficients, n = 1) 
      RegrCoeff_pers[vas, 2:(Terms+1)] = v$varresult[[vas]]$coefficients[1:Terms]
    }
    
    RegrModel_per_pers[n, ] = as.vector(t(RegrCoeff_pers))
    n=n+1
  }
###VAR fit into ncluster

  nClusters=2
  # Use Mclust() to cluster using random partitioning as start
  mod <- Mclust(RegrModel_per_pers, G = nClusters, 
                initialization = list(hcPairs = randomPairs(RegrModel_per_pers))) 
    print(summary(mod)$bic)
mod$classification
####

