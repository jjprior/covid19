# Covid19 Analysis http://app.jackprior.org 
# Jack Prior (covid19@jackprior.org, https://covid19.jackprior.org)
# Licence: https://www.gnu.org/licenses/gpl-3.0.en.html
# reduce messages on shiny server at startup-------
shhh <- suppressPackageStartupMessages 
shhh(library(curl)) #You need to run install.packages("curl") etc once for each library to install
shhh(library(jsonlite))
shhh(library(lubridate))
shhh(library(stringr))
shhh(library(shiny))
shhh(library(scales))
shhh(library(tidyverse))
# set some global values for program-----------
options(warn = 1, scipen = 999)#no scientific notation in plots
runLocal           = FALSE # interactive at console rather than shiny - use with select all lines run
#runLocal           = TRUE # uncomment to run at console with select all lines run
forceRefresh       = FALSE #Uncomment to force reload of data from web
#forceRefresh       = TRUE #Uncomment to force reload of data from web -- remember need refresh if  CODE changed!!!
debugprint          = 1   #set = 1 for pp(txt,var) debug printing
lookback            = 50  #how far back model show plot model on top of data
distwindow          = 21 #how far back for default social distancing window
maxforecastdays     = 120 #maximum forward forecasting
defaultforecastdays = 60  #default forward forecasting
projectDays         = 45  #Days to project out for ranking case growth rate
caseRankThreshold   = 500 #min # of cases to be included in "ALL" rankings
deathRankThreshold  = 50  #min # of deaths to be included in "ALL" rankings
assumedError        = .5  # 30% 3 sigma -- THIS SHOULD BE CODED based on fits eventually. 
nHotspots           = 6 # number of hot spots in drop down. 
plotHeight          = "750px"
#PlotHeight = "1400px" 
refState    = "NY"
refCountry = "USA"
deepBlueState = c("CA", "DC", "HI","MD", "MA","NY","RI","VT")
deepRedState  = c("AL", "AK","AR", "ID","KS", "KY", "LA","MS","MT","NE","ND","OK","SC","SD","TN","TX","UT","WV","WY")
electoralBlue = c("CA", "NY", "IL", "NJ", "VA", "MA", "MD", "MN", "CO","WA", "CT", "OR", "NV", "NM", "NH", "RI", "DE", "HI", "VT", "ME") 
electoralRed  = c("AK", "MT", "ND", "SD", "WY", "ID", "NE", "WV", "AR","IA", "KS", "MS", "UT", "OK", "KY", "LA", "AL", "SC", "MO", "WI", "AZ", "IN", "TN","NC", "GA", "MI", "OH", "PA", "FL", "TX")
electoralAll  = c(electoralRed,electoralBlue)
pp   <- function(p1,p2="",p3="",p4=""){if (debugprint == 1){print(paste(p1,p2,p3,p4))}} #debug printing of 4 values
#GET DATA FROM WEB Or DISK--------------------------------------------------------
get_world_data    = function(refreshData=TRUE){ 
  #Load world data from web, backup load from disk
  if (refreshData){try({WorldDataCache= fromJSON(txt = "https://opendata.ecdc.europa.eu/covid19/casedistribution/json/")
  saveRDS(WorldDataCache,file="WorldDataCache.Rdu")})} #save for future timeout protection
  x         = readRDS(file="WorldDataCache.Rdu")$records    #fall back to read from disk if timed out
  x$rdate             = parse_date_time(x$dateRep,orders="%d%m%y") 
  x$positiveIncrease  = as.numeric(x$cases)
  x$deathIncrease     = as.numeric(x$deaths)
  x$state             = x$countriesAndTerritories
  x$pop               = as.numeric(x$popData2018)
  x$positive          = NA #initialize 
  x$death             = NA
  x            = select(x,rdate,pop,state,positive,positiveIncrease,death,deathIncrease,continentExp) #thin down to needed data
  x            = x[order(x$rdate),]
  x$continentExp[x$continentExp=="America"]="Americas"
  for (s in unique(x$state)){   #calculate cumulative counts for cases and deaths
    index = (x$state == s)
    x$positive[index]  = cumsum(x$positiveIncrease[index])  
    x$death[index]     = cumsum(x$deathIncrease[index])   }
  x$hosp             = NA  
  x$hospIncrease     = NA
  x$test             = NA
  x$testIncrease     = NA
  x$state[x$state == "Democratic_Republic_of_the_Congo"] = "DRC" #length and menu placement changes
  x$state[x$state == "United_Republic_of_Tanzania"]   = "Tanzania"
  x$state[x$state == "United_States_of_America"]      = "USA"
  x$state[x$state == "Georgia"]                       = "Rep of Georgia"
  x$state[x$state == "Cases_on_an_international_conveyance_Japan"] = "Intl Convenance Jp"
  x = x[!x$state=="Cambodia",]   #faulty data countries
  x = x[!x$state=="Burundi",]
  x = x[!x$state=="Curaçao",]
  x = covid_calc(x)
  return(x)}

get_amer_data     <- function(refreshData=TRUE){
  #Load US data from web, add backup laod from disk
  if (refreshData){try({data = fromJSON(txt = "https://covidtracking.com/api/v1/states/daily.json")
                        saveRDS(data,file="AmerDataCache.Rdu")})}
  data              = readRDS(file="AmerDataCache.Rdu") 
  data              = data[!grepl("MP|VI|GU|AS",data$state),]  #take out MP, its small and outlier
  data$test         = data$totalTestResults  #rename columns
  data$hosp         = data$hospitalizedCumulative
  data$hospIncrease = data$hospitalizedIncrease
  data$hosp[data$state=="CT"]         = NA  #bad data
  data$hospIncrease[data$state=="CT"] = NA
  data$hosp[data$state=="IN"]         = NA  #bad data
  data$hospIncrease[data$state=="IN"] = NA
  data$continentExp = "USA"
  data        = data[,c("date","positive","death","positiveIncrease","deathIncrease","hospIncrease","hosp","test","state","continentExp")]
  data$rdate  = parse_date_time(data$date,orders = "%y%m%d")
  spops       = read.csv("statepopstable.csv")  #spreadsheet of state populations
  data$test[data$state=="PR"]  = NA #faulty data
  data               = data[order(data$rdate),]
  data$pop           = NA  #initialize population column
  data$testIncrease  = NA
  for (s in unique(data$state)){  #add population data 
    index                    = (data$state == s)
    data$pop[index]          = get_state_pop(s,spops)  
    data$testIncrease[index] = calc_daily(data$test[index])}
  data        = covid_calc(data)          #Adds calculated fields
  data        = subset( data, select = -date ) 
  data        = data[,order(colnames(data))] #order columns for easier rbind match up check
  return(data)}

calc_daily <- function(x){
  tday = tail(x,length(x)-1)
  yest = head(x,length(x)-1)
  daily = c(0,tday-yest)
  return(daily)}

get_newton_data   <- function(){ #Get Newton, MA data from disk (ideally from googl sheet)
    newton        = read_csv("newton.csv",col_types=cols())
    newton        = as.data.frame(newton)
    newton$rdate  = parse_date_time(newton$date,orders = "%m%d%y")
    index         = !(newton$positiveIncrease=="#N/A")
    newton        = newton[index,]
    index         = newton$deathIncrease=="#N/A"
    newton$deathIncrease[index] = NA
    newton$death[index]         = NA
    newton       = newton[,c("rdate","positive","death","positiveIncrease","deathIncrease")]
    newton$state ="Newton"
    newton$positiveIncrease = as.numeric(newton$positiveIncrease)
    newton$deathIncrease    = as.numeric(newton$deathIncrease)
    newton$death            = as.numeric(newton$death)
    newton$positive         = as.numeric(newton$positive)
    newton$pop              = 85000 # rough population of Newton
    newton$hosp               = NA
    newton$hospIncrease       = NA
    newton$test               = NA
    newton$testIncrease       = NA
    newton$continentExp = "other"
    newton    = covid_calc(newton)
    return(newton)}

region_aggregate <- function(data,state="World"){ #sum up countries to world or states to USA
  #this doesn't deal with incomplete data for a country in final days and give smaller counts than full at end. 
  data = data[data$rdate < Sys.Date() , ] #avoid partial data for "today"
  Regionpositive               = aggregate(positive~rdate,             data = data, FUN = sum, na.rm =TRUE, na.action = NULL)
  RegionpositiveIncrease       = aggregate(positiveIncrease~rdate,     data = data, FUN = sum, na.rm =TRUE, na.action = NULL)
  RegiondeathIncrease          = aggregate(deathIncrease~rdate,        data = data, FUN = sum, na.rm =TRUE, na.action = NULL)
  Regiondeath                  = aggregate(death~rdate,                data = data, FUN = sum, na.rm =TRUE, na.action = NULL)
  Regionpop                    = aggregate(pop~rdate,                  data = data, FUN = sum, na.rm =TRUE, na.action = NULL)
  Regiondata = merge(Regionpositive, Regiondeath,       by ="rdate", all=T)
  Regiondata = merge(Regiondata,RegionpositiveIncrease, by ="rdate", all=T)
  Regiondata = merge(Regiondata,RegiondeathIncrease,    by ="rdate", all=T)
  Regiondata = merge(Regiondata,Regionpop,              by ="rdate", all=T)
  if (state=="USA"){
    Regiontest          = aggregate(test~rdate,                   data = data, FUN = sum, na.rm = TRUE, na.action = NULL)
    RegiontestIncrease  = aggregate(testIncrease~rdate,           data = data, FUN = sum, na.rm = TRUE, na.action = NULL)
    Regionhosp          = aggregate(hosp~rdate,                   data = data, FUN = sum, na.rm = TRUE, na.action = NULL)
    RegionhospIncrease  = aggregate(hospIncrease~rdate,           data = data, FUN = sum, na.rm = TRUE, na.action = NULL)
    Regiondata          = merge(Regiondata,Regionhosp,         by ="rdate", all = T)
    Regiondata          = merge(Regiondata,RegionhospIncrease, by ="rdate", all = T)   
    Regiondata          = merge(Regiondata,Regiontest,         by ="rdate", all = T)
    Regiondata          = merge(Regiondata,RegiontestIncrease, by ="rdate", all = T)
    Regiondata$continentExp = "USA"}
  else
    {Regiondata$test        = NA
    Regiondata$testIncrease = NA
    Regiondata$continentExp = "Aggregated"
    Regiondata$hospIncrease = NA
    Regiondata$hosp         = NA}
  Regiondata$state = state
  Regiondata       = covid_calc(Regiondata)
  return(Regiondata)}

get_state_pop <- function(estate,spops){ 
  #get US population data from a csv file sourced data frame. 
  index= (spops$State==estate)
  if (sum(index)==1){pop = as.numeric(spops$Pop[index])}else{pop=NA}
  return(pop)}

#ADD FEATURES to DATA -------------------------------------------------------------
project_growth_rate <- function(x,s,totfeature,fracfeature){
  #project case, death, etc growth rate "projectDays" into the future -- measures severity of flattening issues for ranking
  fit =  get_growth(x,s,fracfeature, Sys.Date()-distwindow,Sys.Date()-1)
  if ( !is.na(fit[2]) ) {if (fit[2]>0) {fit[2]=0}}#don't ramp if positive assume current stays 
  return(fit[1]*exp(fit[2]*projectDays))}

growth_rate_decline_rate <- function(x,s,totfeature,fracfeature){
  #project case, death, etc growth rate "projectDays" into the future -- measures severity of flattening issues for ranking
  fit =  get_growth(x,s,fracfeature, Sys.Date()-distwindow,Sys.Date()-1)
  return(fit[2])}


project_total_growth_rate <- function(x,s,totfeature,fracfeature,t){
  #We want to know what regions will be moving danger zone over a certain period of time.   We know how the growth rate
  #is changing, but don't want to assume growth rate will climb further if its trending up, so we cap climbing growth rate 
  #at the current value.   
  #
  # x(t) = x(o) * exp ( u(t) * t)      |solution if u was constant
  # u(t) = u(o) * exp (k * t)   |constrain fit(2) <= 0 as growth won't go up forever, but head to constant exponential
  #  the daily rate at time t is u(t) * x (t)
  #  Rather than integrate the 1st term just approximate with average for now -- this is for ranking risk
  #  total growth at time t = u(t)*x(t) = (x(o) * exp(uavg * t)) * (u(o) * exp(k * t))
  #
  fit =  get_growth(x,s,fracfeature, Sys.Date()-distwindow,Sys.Date()-1)
  uo = fit[1]
  k  = fit[2]
  if (is.na(k | length(k)==0)){return(NA)}
  if (k>0){k = 0}
  X=x[x$state==s,totfeature]
  Xo = tail(X,1)
  ut = uo*exp(k*t)
  uavg = (ut+uo)/2 
  total_growth = Xo * exp(uavg * t) * ut
  return(total_growth)}

covid_calc <- function(x){
  #Add calculated attributes to raw imported data
  x$cfr                 = x$death/x$positive
  x$cfrIncremental      = x$deathIncrease/x$positiveIncrease
  x$ifrRatio            = x$cfr/0.0066
  x$ifrRatioIncremental = x$cfrIncremental/0.0066
  x$positiveEst         = x$positive*x$ifrRatio
  x$positiveIncreaseEst = x$positiveIncrease*x$ifrRatio
  x$dperh               = x$death/x$hosp #deaths per hospitalization
  x$dperhIncremental    = x$deathIncrease/x$hospIncrease
  x$fracpos             = x$positive/x$test
  x$fracposIncremental  = x$positiveIncrease/x$testIncrease
  x$fracpoptested       = x$test/x$pop
  x$fracpoptestedIncremental = x$testIncrease/x$pop
  x$positiveIncremental      = x$positiveIncrease/x$testIncrease
  x$deathIncremental         = x$deathIncrease/x$hospIncrease
  x$hospIncremental          = x$hospIncrease/x$positiveIncrease
  x$fracPositiveIncrease     = NA
  x$fracHospIncrease         = NA
  x$fracDeathIncrease        = NA
  x$fracPositiveIncreaseEst  = NA
  x$fracTestIncrease         = NA
  x$mday                     = as.numeric(difftime(x$rdate,Sys.Date(),units=c("days"))) #need to regress against days relative to today. 
  x =  x[with(x,order(rdate)),]  
  for (s in unique(x$state)){#calculated day over day growth frac (rates) in cases, etc. 
    index= (x$state==s)
    x[index,] = calc_growth_since_last_change(x[index,],"positiveIncrease",    "positive",    "fracPositiveIncrease")
    x[index,] = calc_growth_since_last_change(x[index,],"positiveIncreaseEst", "positiveEst", "fracPositiveIncreaseEst")
    x[index,] = calc_growth_since_last_change(x[index,],"deathIncrease",       "death",       "fracDeathIncrease")
    x[index,] = calc_growth_since_last_change(x[index,],"hospIncrease",        "hosp",        "fracHospIncrease")
    x[index,] = calc_growth_since_last_change(x[index,],"testIncrease",        "test",        "fracTestIncrease")
  
    x$CaseGrowthRateDeclineRate[index]  = growth_rate_decline_rate(x,s,   "positive",   "fracPositiveIncrease")
    x$TestGrowthRateDeclineRate[index]  = growth_rate_decline_rate(x,s,   "test",       "fracTestIncrease")
    x$DeathGrowthRateDeclineRate[index] = growth_rate_decline_rate(x,s,   "death",      "fracDeathIncrease")
    x$HospGrowthRateDeclineRate[index]  = growth_rate_decline_rate(x,s,   "hosp",       "fracHospIncrease")
    x$EstCaseGrowthRateDeclineRate[index] =growth_rate_decline_rate(x,s, "PositiveEst","fracPositiveIncreaseEst")
    
    x$projectedCaseGrowth[index]    = project_total_growth_rate(x,s,   "positive",   "fracPositiveIncrease",projectDays) * 1e6/x$pop[index] #get ppm growth rather than % growth
    x$projectedTestGrowth[index]    = project_total_growth_rate(x,s,   "test",       "fracTestIncrease",projectDays)     * 1e6/x$pop[index]
    x$projectedDeathGrowth[index]   = project_total_growth_rate(x,s,   "death",      "fracDeathIncrease",projectDays)    * 1e6/x$pop[index]
    x$projectedHospGrowth[index]    = project_total_growth_rate(x,s,   "hosp",        "fracHospIncrease",projectDays)     * 1e6/x$pop[index]
    x$projectedEstCaseGrowth[index] = project_total_growth_rate(x,s,   "positiveEst", "fracPositiveIncreaseEst",projectDays)     * 1e6/x$pop[index] }
  x = get_pct_complete(x, 90, Sys.Date()-distwindow, Sys.Date()-1)
  x = x[,order(colnames(x))]
  return(x)}

calc_growth_since_last_change <- function(x,increaseFeature, totalFeature,fracIncreaseFeature){
  #deal with infrequently changing counts
  t  = x$mday
  y  = x[[totalFeature]]
  dy = x[[increaseFeature]]
  index = !( (dy == 0) |  is.na(dy) | is.null(dy) )
  y  = y[index]
  dy = dy[index]
  t  = t[index]
  yo  = head(c(NA,y), length(y))
  dyo = head(c(NA,dy),length(y))
  to  = head(c(NA,t), length(y))
  gr  = dy/(yo*(t-to))
  x[index, fracIncreaseFeature]  = gr
  x[!index,fracIncreaseFeature]  = NA
return(x)}

get_growth       <- function(adata,estate, yffeature, sSocialDist, eSocialDist){
  #get slope, intercept of growth rate slowing fit within social distancing window.
  adata$yf = adata[[yffeature]]
  sdata   = subset(adata,(state == estate) &(rdate >= sSocialDist) &(rdate <= eSocialDist) )
  index = !(sdata$yf==0)
  if (sum(index, na.rm =TRUE )==0) { return(NA) }
  sdata   = subset(sdata,!is.na(yf) & !(yf==0))
  if (is.null(sdata)) {return(NA)}
  
  fitdata = subset(sdata,(rdate >= sSocialDist) & (rdate<=eSocialDist) & (yf>0))
  model   = lm(log(yf)~mday, data=fitdata)
  return(c(exp(model$coefficients[1]),model$coefficients[2]))}

get_pct_complete <- function(x,  lookahead, sSocialDist, eSocialDist){
  #estimate % complete epidemic for a state - not used yet. 
  x =  x[with(x,order(rdate)),]  
  thestates = unique(x$state)
  for (s in thestates){
    index= (x$state==s) 
    x$pctPositiveComplete[index]    = x$positive[index]   /  max(-Inf,calc_forecast(x,s,"positive",    "positiveIncrease",    "fracPositiveIncrease",   lookahead,sSocialDist, eSocialDist)$positive)     
    x$pctDeathComplete[index]       = x$death[index]      /  max(-Inf,calc_forecast(x,s,"death",       "deathIncrease",       "fracDeathIncrease",      lookahead,sSocialDist, eSocialDist)$death)       
    x$pctPositiveEstComplete[index] = x$positiveEst[index]/  max(-Inf, calc_forecast(x,s,"positiveEst", "positiveIncreaseEst", "fracPositiveIncreaseEst",lookahead,sSocialDist, eSocialDist)$positiveEst) 
    x$pctHospComplete[index]        = x$hosp[index]       /  max(-Inf, calc_forecast(x,s,"hosp",        "hospIncrease",        "fracHospIncrease",       lookahead,sSocialDist, eSocialDist)$hosp) }
  return(x)}

growth_estimate  <- function(date0,mday0,x0,lookahead,lookback, m){
  #create forecast from model 
  a      = unname(coef(m)[1])  #log(fracpostivve growth rate at date 0 fit)
  b      = unname(coef(m)[2])  #time constant for fracpositive growth rate decline
  if (b>0){b=0}  #don't extrapolate increases in growth rate. 
  if (exp(a)>0.25){
    pp("high growth = ", exp(a))
    a = log(0.25)
    }# don't extrapolate exponential growth above 0.25 -- for sparse reporting isssue
  k      <- function(t){exp(a)*exp(b*t)}   #frac positive growth rates 
  l      = lookahead
  y      = seq(1,lookahead,1) # initialize
  yy     = seq(1,lookback,1) # initialize
  y[1]         = x0 #first lookahead is case0
  yy[lookback] = x0 #last lookback is case0
  for (j in seq(2,lookahead,1)){y[j]      = y[j - 1] * (1 + k(mday0 + j - 2)) }
  for (j in seq(lookback,1,-1)){yy[j - 1] = yy[j] /    (1 + k(mday0-lookback + j- 1))}
  yy = head(yy,length(yy)-1) #take off last lookback
  y  = c(yy,y)  #combine
  return(y)
}

indexFit <- function(data,cstate,sSocialDist,eSocialDist,fracfeaturestr){
  index =          (data$state == cstate) & 
    (data$rdate >= sSocialDist) & 
    (data$rdate <= eSocialDist) & 
    (data[[fracfeaturestr]]>0) & 
    (!is.na(data[[fracfeaturestr]]))
  return(index)}
  
canBeFit = function(data,cstate,sSocialDist,eSocialDist,fracfeaturestr){
  index = indexFit(data,cstate,sSocialDist,eSocialDist,fracfeaturestr)
  result = !( (sum(index) < 5) | is.null(index) )
return(result)}

calc_forecast    <- function(data,cstate,totfeaturestr,increasefeaturestr, fracfeaturestr,lookahead,sSocialDist, eSocialDist){
  #calculate a fitted/forecasted total, daily, and growthrate for cases, estimated cases, deaths and hospitalizations. 
  data$totfeature =  data[[totfeaturestr]]
  data$fracfeature = data[[fracfeaturestr]]
  data$increasefeature = data[[increasefeaturestr]]
  if(!canBeFit(data,cstate,sSocialDist,eSocialDist,fracfeaturestr)){return(NULL)}
  index = indexFit(data,cstate,sSocialDist,eSocialDist,fracfeaturestr)
  data = data[index,]
  cgmodel=NULL
  try({cgmodel = lm(log(fracfeature)~mday, data=data)})
  if (is.null(cgmodel)){return(NULL)}   #hack to deal with missing/insufficent data, need to clean up
  idx = !(abs(cgmodel$residuals)==max(abs(cgmodel$residuals)))
  if (sum(idx)==1){data=data[idx,]}
  data = data[with(data,order(rdate)),]
  
  if (nrow(data)<5){return(NULL)}
  try({cgmodel = lm(log(fracfeature)~mday, data=data)})
  if(is.null(cgmodel)){return(NULL)}
  date0 = max(data$rdate)
  newdates  = as.POSIXct(seq(date0-days(lookback-1),date0+days(lookahead-1), by="day"))
  x        = data.frame(rdate= newdates)
  
  case0 = max(data$totfeature)
  mday0 = max(data$mday)
  if (!is.null(cgmodel)){
    cases    = growth_estimate(date0,mday0,case0,lookahead,lookback,cgmodel)
    newcases = c(NA,tail(cases,length(cases)-1)-head(cases,length(cases)-1))}
  else { cases   =NA
        newcases =NA}
  x[[totfeaturestr]] = cases
  x[[increasefeaturestr]] = newcases 
  x[[fracfeaturestr]] = newcases/cases
  return(as.data.frame(x))}

#PLOT Utility functions-----------------------------------------------------
ma            <- function(x, n = 7){
  if (length(x)==6){x=c(NA,x)}# quick hack to deal with bad data
  if (length(x)==5){x=c(NA,x,NA)}
  if (length(x)==4){x=c(NA, NA,x,NA)}
  if (length(x)==3){x=c(NA, NA,x,NA,NA)}
  if (length(x)==2){x=c(NA, NA,NA,x,NA,NA)}
  if (length(x)==1){x=c(NA, NA,NA,x,NA,NA,NA)}
  if (length(x)==0) {return(NULL)}
  stats::filter(x, rep(1 / n, n), sides = 2)} #7 day centered moving average

get_hot_spots = function (nowdata){
  return(c(get_hot_spots_by(nowdata,"projectedEstCaseGrowth",nHotspots),get_hot_spots_by(nowdata,"projectedDeathGrowth",1)))
}
get_hot_spots_by = function(nowdata,sfeature,n){ 
  nowdata$feature = nowdata[[sfeature]]                       #make the summarized feature accessible.   
  nowdata         = nowdata[!is.na(nowdata$feature) & (nowdata$rdate > ( Sys.Date() - 8 ) ) & ( nowdata$rdate < Sys.Date() ), ] # limiit to last week non NA 
  nowdata         = nowdata[( (nowdata$positive > caseRankThreshold) & (nowdata$death > deathRankThreshold)),]    #limit to major regions
  nowdata$feature = nowdata$feature 
  nowdata         = nowdata[!is.na(nowdata$feature),]
  nowdata         = select(nowdata,c(state,feature))
  states          = unique(nowdata$state)
  newdata         = data.frame(states)
  if (nrow(nowdata)==0){return(NULL)}
  newdata$feature=NA
  for ( s in states ){
    index1 = nowdata$state == s
    index2 = newdata$state == s 
    newdata$feature[index2] = mean(nowdata$feature[index1]) } #consider using sum/days
  nowdata  = newdata
  nowdata  = nowdata[order(-nowdata$feature),]
  hotspots = head(nowdata$state,n)
  return(levels(droplevels(hotspots)))}

main_caption  <- function(s1,s2){paste(format(Sys.Date(), format = "%d%B") , " app.jackprior.org  - Social Dist. Basis: ",format(s1, format = "%d%b"),"-",format(s2, format = "%d%b"),sep = "")}

main_title    <- function(s,gr=""){ggtitle(paste(  str_replace( str_replace_all(s,"_"," "),"Summary",""), gr))}

p_add_vline      <- function(vdate){geom_vline(aes(xintercept =   as.numeric(as.POSIXct(vdate))))} # add vertical line on date plots

plot_unavailable <- function(txt=""){ggplot() + ggtitle(paste("Report unavailable",txt))} #for menu choices with no data

p_log_scale   <- function(p,plotlog,pct=0){
  #set Y axis as log or not and format accordingly, noting if it is a percent unit
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))  
  if ((plotlog==1)&(pct==0)) {p=p+scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, labels = scales::comma) }
  if ((plotlog==1)&(pct==1)) {p=p+scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, labels=scales::percent)}
  if ((plotlog==0)&(pct==0)) {p=p+scale_y_continuous(labels = scales::comma)}
  if ((plotlog==0)&(pct==1)) {p=p+scale_y_continuous(labels = scales::percent)}
  return(p)} 

p_log_scaleX  <- function(p,plotlog,pct=0){ 
  #set X axis as log or not and format 
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))  
  if ((plotlog==1)&(pct==0)) {p=p+scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) }
  if ((plotlog==1)&(pct==1)) {p=p+scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, labels=scales::percent)}
  if ((plotlog==0)&(pct==0)) {p=p+scale_x_continuous(labels = scales::comma)}
  if ((plotlog==0)&(pct==1)) {p=p+scale_x_continuous(labels = scales::percent)}
  return(p)}

lm_ln_eqn  <- function(m){
  #return correlation equation string for putting on plots
 eq <- substitute(italic(y) == a  %.% " exp("* b %.% italic(x)*"),"~~italic(r)^2~"="~r2, 
                  list(a = format(unname(exp(coef(m)[1])), digits = 2),
                       b = format(unname(coef(m)[2]), digits = 2),
                       r2 = format(summary(m)$r.squared, digits = 3)))
as.character(as.expression(eq));}

p_annotate <- function(rdates,plotpts,plotlog,txt="",atmin=FALSE){
  #annotate with value at max date/value
  if (!sum(!is.na(plotpts)>0)){return(NULL)}
  if (!atmin){y = max(plotpts,na.rm = TRUE)} else {y = min(plotpts,na.rm=TRUE)}
  if (!atmin){x = max(rdates[plotpts==y], na.rm= TRUE)} else {x = max(rdates[plotpts==y], na.rm= TRUE)}
  if (txt == ""){ txt = round(y)
                  txt = format(txt, big.mark=",")}
                  txt = str_replace(txt, "_","")
  return(annotate("text",x = x, y = y, label = txt, parse=FALSE,vjust=0,hjust=1))}

p_annotate_text <- function(x,y,txt){#annotate with value at max date/value
  return(annotate("text",x = x, y = y, label = txt,vjust=0,hjust=0))}

mid_point   <- function(x, ht=0.5){ # place scaled amongst data for annotation.
  y = min(x)+ ht* (max(x)-min(x))
  return(y)}

format_plot <- function(p, estate, ytitle, plotlog,sSocialDist,eSocialDist,pct=0){
  # format plots generally
  p = p + theme_set(theme_gray(base_size = 16))
  p = p + theme(axis.title.x=element_blank()) 
  p = p + theme(legend.title=element_blank())
  p = p + theme(plot.caption = element_text(hjust = 0))
  p = p + labs(caption=main_caption(sSocialDist,eSocialDist)) 
  p = p + main_title(estate)
  try({p=p_log_scale(p,plotlog,pct) })
  return(p)}

format_date_plot <- function(p, estate, ytitle, plotlog,sSocialDist,eSocialDist,pct=0){
  # format plots vs. date
  p = format_plot(p, estate, ytitle, plotlog,sSocialDist,eSocialDist,pct)
  p = p + scale_x_datetime(date_labels = "%d%b", date_breaks = "1 month")
  p = p + ylab(ytitle)
  p = p + theme(axis.title.x = element_text(size = 0)) 
  try({p = p + p_add_vline(as.Date(sSocialDist))+p_add_vline(as.Date(eSocialDist))})
  return(p)}

format_bar_plot  <- function(p, estate, ytitle, plotlog,sSocialDist,eSocialDist,pct=0){
  # format bar plots
   p = format_plot(p, estate, ytitle, plotlog,sSocialDist,eSocialDist,pct)
   p = p + coord_flip()
   p = p + theme(axis.title.y=element_blank()) 
  return(p)}

format_legend = function(p,estate){ if (length(estate)<11){p = p + theme(legend.position = "bottom")}else {p = p + theme(legend.position = "none")}}

is_pct_axis <- function(sfeature,focusplot){if  (grepl("frac|cfr|per|dperh|Pct|pct|Rate",sfeature)|(grepl("Rate",focusplot))){return(1)}else{return(0)}}

#Main Plotting Functions -------------------------------------------------
plot_feature        <- function(sdata,feature,ftitle,cstate,plotlog,lookahead,sSocialDist,eSocialDist,overlay=FALSE){
  #Plot a feature generically
  sdata$y = sdata[[feature]]
  sdata   = sdata[!is.na(sdata$y),]
  overlay = length(cstate)>1
  if (overlay) {sdata$flegend=sdata$state} else {sdata$flegend = feature}
  sdata   = subset(sdata, grepl(paste(cstate,collapse="|"),state) & !is.na(y) & y>0)
  if (nrow(sdata)==0){return(plot_unavailable())}  
  for (s in cstate){sdata$movingAvg[sdata$state==s] = as.numeric(ma(sdata$y[sdata$state==s],7)) }
  ispct=is_pct_axis(feature,"NA")
  p=ggplot()  
  if (!overlay){p = p+geom_point(sdata, mapping=aes(x=rdate,y=y,color=flegend))}
  p = p + geom_line(data = sdata    , mapping =aes(x = rdate, y = movingAvg, colour = flegend))
  for (s in cstate){p= p+p_annotate(sdata$rdate[sdata$state==s],sdata$movingAvg[sdata$state==s],plotlog,s)}
  p = format_date_plot(p,cstate,ftitle,plotlog,sSocialDist,eSocialDist,ispct)
  p = format_legend(p,cstate)
  return(p)}

plot_now_summary    <- function(nowdata, focusplot, plotlog, normalize, sfeature, sSocialDist, eSocialDist, nStates = 40, lookahead = NA ){ 
  #plot ranked bar graphs
  nowdata$feature = nowdata[[sfeature]]                       #make the summarized feature accessible.   
  nowdata         = nowdata[ (nowdata$rdate > ( Sys.Date() - 8 ) ) & ( nowdata$rdate < Sys.Date() ), ] # limiit to last week 
  nowdata         = nowdata[!is.na(nowdata$feature),]         # limit to non-NA values
  if (sum(grepl("France",nowdata$state))>0){nowdata = nowdata[( (nowdata$positive > caseRankThreshold) & (nowdata$death > deathRankThreshold)),]}
  ispct = is_pct_axis(sfeature,focusplot)                     
  if (!ispct & normalize){nowdata$feature = nowdata$feature / as.numeric(nowdata$pop) * 1e6
                          nowdata          = nowdata[!is.na(nowdata$feature),]
                          focusplot        = paste(focusplot,("(per million)"))}
  nowdata = select(nowdata,c(state,feature))
  state   = unique(nowdata$state)
  newdata = data.frame(state)
  states  = state
  if (nrow(nowdata)==0){return(NULL)}
  newdata$feature=NA
  for ( s in states ){newdata$feature[newdata$state == s ] = mean(nowdata$feature[nowdata$state == s]) } #consider using sum/days
  nowdata = newdata
  nowdata = nowdata[order(-nowdata$feature),]
  nowdata$Status  = "All"
  if (nrow(nowdata)>nStates){top = head(nowdata,nStates/2)
                             bot = tail(nowdata,nStates/2)
                             top$Status="High"
                             bot$Status="Low"
                             nowdata=rbind(top,bot)}
  if (nrow(nowdata)==0){  return(plot_unavailable())}else{p=ggplot(data=nowdata,mapping=aes(x=reorder(state,feature), y=feature, color=Status))+geom_bar(stat="identity")
    if (focusplot=="%Complete Cases Summary"){focusplot="%Cases done"}
    if (focusplot=="%Complete Deaths Summary"){focusplot="%Deaths done"}
    if (focusplot=="%Complete Hospitalizations Summary"){focusplot="%Hospitalizations done"}
    if (focusplot=="%Complete Est Cases Summary"){focusplot="%Est Cases done"}
    if (grepl("done",focusplot)){p = p +  geom_hline(aes(yintercept =  .9))}
    p = format_bar_plot(p,focusplot,"", plotlog,sSocialDist,eSocialDist,ispct)}
  return(p)}   

plot_trend  <- function(p,pdata,estate,plotlog,totFeature,increaseFeature,fracFeature,flegend,lookahead,sSocialDist,eSocialDist,normalize,daily=FALSE,overlay=FALSE){
  pdata$tot      = pdata[[totFeature]]
  pdata$increase = pdata[[increaseFeature]]
  pdata$frac     = pdata[[fracFeature]]
  pdata          = subset(pdata,   grepl(paste(estate,collapse="|"),state) & !is.na(tot) & !is.na(increase)) 
  if (normalize) { pdata$tot      = pdata$tot/pdata$pop*1e6     }
  if (normalize) { pdata$increase = pdata$increase/pdata$pop*1e6}
  if (nrow(pdata) == 0){return(p) }
  
  forecast = NULL
  estate=estate[!is.na(estate)]
  for (s in estate){index                     = (pdata$state==s & !is.na(pdata$increase))
                     if (is.na(s)){s="missing"} #eu bug?
                     if (sum(index)>0){
                       pdata$movingAvg[index] = as.numeric(ma(pdata$increase[index],7))
                       temp                   = calc_forecast(pdata,s,"tot"   ,"increase"   ,"frac",   lookahead,sSocialDist,eSocialDist)
                  
                       if (!is.null(temp)){temp$state = s
                                           forecast   = rbind(forecast,temp)
                                           forecast   = forecast[!is.na(forecast$increase),]}}}
  
  forecastOK = ( !is.null(forecast) ) & ( sum( !is.na( forecast$tot ) ) > 0 )       
  
  if (forecastOK) {if (!overlay){forecast$flegend = flegend} else {forecast$flegend = forecast$state}
                                 if (!overlay){pdata$flegend    = flegend} else { pdata$flegend    = pdata$state}
                                 if ( overlay){forecast         = forecast[forecast$rdate>=(Sys.Date()-3),]}

      #constrain forecasts to not exceed population to avoid blowing up graphs
      if (normalize){forecast$tot[forecast$tot>1e6]=NA
                     forecast$increase[forecast$increase>1e6]=NA}
      else          {forecast$tot[forecast$tot>forecast$pop]=NA
                     forecast$increase[forecast$increase>forecast$pop]=NA}}

  madata = pdata[!is.na(pdata$movingAvg),]
  
  if(!daily){ #cumulative totals
       if (overlay){p = p +  geom_line(data = pdata, mapping =aes(x = rdate,y = tot, colour = flegend)) } #show as line in overlay
            else   {p = p + geom_point(data = pdata, mapping =aes(x = rdate,y = tot, colour = flegend)) }
       p = p + expand_limits(y = max(pdata$tot)*1.05)
       if (forecastOK){
       if (!overlay){p = p + geom_line(data = forecast,mapping = aes(x = rdate,y = tot, colour = flegend),linetype="dashed") 
                     p = p + expand_limits(y = max(forecast$tot)*1.05)
                     p = p + p_annotate(pdata$rdate,    pdata$tot,    plotlog)
                     p = p + p_annotate(forecast$rdate, forecast$tot, plotlog)}
       else {for (s in estate){p =  p + p_annotate(forecast$rdate[forecast$state == s],forecast$tot[forecast$state == s],plotlog,s)}}
       p = p + geom_line(data = forecast,mapping = aes(x = rdate,y = tot, colour = flegend), linetype="dashed") 
       p = p + expand_limits(y = max(forecast$tot)*1.05)}}
  else #daily plot
     { p = p + geom_line(data = madata    , mapping =aes(x = rdate, y = movingAvg, colour = flegend))
       if (!overlay){p = p + geom_point(data = pdata, mapping=aes(x = rdate,y = increase, colour = flegend))
                     if (forecastOK) {
                       hforecast = forecast
                       lforecast = forecast
                       hforecast$increase = hforecast$increase * (1 + assumedError)
                       lforecast$increase = lforecast$increase * assumedError
                       p = p + geom_line(data = hforecast, mapping =aes(x = rdate, y = increase, colour = flegend),linetype="dotted")
                       p = p + geom_line(data = lforecast, mapping =aes(x = rdate, y = increase, colour = flegend),linetype="dotted")}}
                     
      if (forecastOK) {p = p + geom_line(data =  forecast, mapping =aes(x = rdate, y = increase, colour = flegend), linetype="dashed")}
      if (overlay){for (s in estate){index= (pdata$state==s)
                                     p = p+p_annotate(pdata$rdate[index],pdata$movingAvg[index],plotlog,s)
                                     index= (forecast$state==s)
                                     p = p+p_annotate(tail(forecast$rdate[index],1),tail(forecast$increase[index],1),plotlog,s)}}}
    
     #General Plot Attributes for both Total and Daily     
    if (daily)     {ytitle = "Daily Total"} else {ytitle = "Total"}
    if (overlay)   {ytitle = paste(ytitle,flegend)                }
    if (normalize) {ytitle = paste( ytitle, "(per million)" )     }    
    if (!overlay)  {p = p + scale_colour_manual("", breaks = c("Deaths", "Cases", "Hospitalizations", "Est Cases", "Tests"), values = c("red","black", "green","blue", "orange")) 
                   gtitle=estate}
    else {gtitle="" }#no title for overlays for now}
    p = format_date_plot(p, gtitle,ytitle, plotlog, sSocialDist, eSocialDist, 0)
    p = format_legend(p,estate)
    return(p)}

plot_total  <- function(data,estate,plotlog,showcase, showdeath, showtest, showhosp, showest, lookahead,sSocialDist,eSocialDist, normalize, daily=FALSE,overlay=FALSE) {
  #plot total and daily plots for 1 or more attributes - key routine in program.
  p = ggplot() 
  if (is.null(estate)){return(p)}
  if (showcase  == 1) {p = plot_trend(p,data,estate,plotlog,"positive",    "positiveIncrease",    "fracPositiveIncrease",    "Cases",            lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showest   == 1) {p = plot_trend(p,data,estate,plotlog,"positiveEst", "positiveIncreaseEst", "fracPositiveIncreaseEst", "Est Cases",        lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showdeath == 1) {p = plot_trend(p,data,estate,plotlog,"death",       "deathIncrease",       "fracDeathIncrease",       "Deaths",           lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showtest  == 1) {p = plot_trend(p,data,estate,plotlog,"test",         "testIncrease",        "fracTestIncrease",       "Tests",            lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showhosp  == 1) {p = plot_trend(p,data,estate,plotlog,"hosp",        "hospIncrease",        "fracHospIncrease",        "Hospitalizations", lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  return(p)}

plot_growth_original <- function(p,adata,sSocialDist, eSocialDist,lookahead){ 
  #Add a gray fit to early social distancing performance.
  if (is.null(adata)| nrow(adata)==0){return(NULL)}
  if (eSocialDist<ymd("20200401")){eSocialDist=ymd("20200401")} #avoid crash with no data
  if (sum("China" == adata$state) > 0) { #china distanced earlier than rest in 1st wave. 
    sSocialDist=ymd("20200208")
    eSocialDist=ymd("20200308")}
  if (sum("South_Korea" == adata$state ) > 0) {
    sSocialDist=ymd("20200223")
    eSocialDist=ymd("20200316")}
  sdata   = subset(adata,
                  (rdate >= sSocialDist) &
                  (rdate <= eSocialDist))
  sdata   = sdata[with(sdata,order(rdate)),]
  plotit1          = sdata
  plotit1$flegend  = "Early Performance"
  fitdata = subset(sdata,(rdate >= sSocialDist) & (rdate<=eSocialDist) & yf>0)
  if (nrow(fitdata)<7){return(p)}
  model   = lm(log(yf)~mday, data=fitdata)
  newdays   = data.frame(mday=c((min(sdata$mday)-lookback):(max(sdata$mday)+lookahead  )))
  newdates  = as.POSIXct(seq(min(sdata$rdate)-days(lookback),max(sdata$rdate)+days(lookahead), by="day"))
  newgrowth = exp(predict(model,newdays))
  plotit2   = data.frame(rdate=newdates,mday=newdays,death="NA",yf=newgrowth) 
  plotit2$flegend ="Early Performance"
  annodate     = mid_point(plotit2$rdate,.66)
  annoy        = mid_point(plotit1$yf,   .66)
  p = p + annotate("text",x = annodate,y = annoy, label = lm_ln_eqn(model),parse=TRUE)
  p = p + geom_point( plotit1,  mapping=aes(x = rdate,y = yf, color = flegend)) +
           geom_line( plotit2,  mapping=aes(x = rdate,y = yf, color = flegend))}

plot_growth          <- function(focusplot, theTotField,thefracField,sdata,gstate,plotlog,lookahead,sSocialDist,eSocialDist, overlay=FALSE){
  #Flattening plots
  sdata$yt = sdata[[theTotField]]
  sdata$yf = sdata[[thefracField]]
  adata    = subset(sdata,grepl(paste(gstate,collapse="|"),state) & (!is.na(yt) & yf>0))
  if (nrow(adata)==0) {return(plot_unavailable())}
  if (overlay){adata$flegend=adata$state}else{adata$flegend="All"}
  pdata           = adata
  pdata$movingAvg = NA
  for (s in gstate){
    index=  (pdata$state ==  s)
    pdata$movingAvg[index] = as.numeric(ma(pdata$yf[index],7))}
  if (overlay) {pdata$flegend=adata$state} else {pdata$flegend="Mov Avg"}
  p=ggplot()
    madata = pdata[!is.na(pdata$movingAvg),]
  if (overlay)
    {ptitle="Flattening Rates"
     p = p + geom_line( madata,   mapping=aes(x = rdate,y = movingAvg, color = flegend))
     thestates=unique(pdata$state)
       for (s in thestates){
         index= (pdata$state==s)
         p= p+p_annotate(pdata$rdate[index],pdata$movingAvg[index],plotlog,s,TRUE)}}
  else
    { sdata = subset(adata,(rdate >= sSocialDist) & (rdate <= eSocialDist))
      sdata = sdata[with(sdata,order(rdate)),]
    
      if (nrow(sdata)==0) {return(plot_unavailable())}
      
      plotit1  = sdata
     if (overlay){plotit1$flegend=plotit1$state} else {plotit1$flegend  = "Fitted"}
  
      fitdata = subset(sdata,(rdate >= sSocialDist) & (rdate<=eSocialDist) & (yf>0))
      model   = lm(log(yf)~mday, data=fitdata)
      
      newdays   = data.frame(mday=c((min(sdata$mday)-lookback):(max(sdata$mday)+lookahead  )))
      newdates  = as.POSIXct(seq(min(sdata$rdate)-days(lookback),max(sdata$rdate)+days(lookahead), by="day"))
      newgrowth = exp(predict(model,newdays))
    
      plotit2   = data.frame(rdate=newdates,mday=newdays,death="NA",yf=newgrowth) 
      plotit2$flegend ="Fitted"
    
      annodate     = mid_point(plotit2$rdate,.66)
      annoy        = mid_point(plotit1$yf,.66)
    
      p=p + geom_point( adata,    mapping=aes(x = rdate, y = yf ))+
            geom_point( plotit1,  mapping=aes(x = rdate, y = yf, color = flegend))+
            geom_line(  pdata,    mapping=aes(x = rdate, y = movingAvg, color = flegend))+
            geom_line(  plotit2,  mapping=aes(x = rdate, y = yf, color = flegend, ))
      
      p = p + annotate("text",x = annodate,y = annoy, label = lm_ln_eqn(model),parse=TRUE)
      ptitle = paste(gstate," ",round(model$coeff[2]*100,1),'% per day',sep="")
      p      = plot_growth_original(p,adata,ymd("20200324"),sSocialDist,lookahead)
      p     = p+ scale_colour_manual("", breaks = c("Early Performance", "Fitted", "Data", "Mov Avg"),values = c("grey",    "red", "black","blue"))
    }
    
    p = format_date_plot(p,ptitle,focusplot, !plotlog ,sSocialDist,eSocialDist,1)
    p = format_legend(p,gstate)
  return(p)} 

#Plot Selection based on UI choices: identify_plot indentifies report to run and passes to generate_plot
generate_plot <- function(focusplot,input,data,plotlog,lookahead,sSocialDist,eSocialDist,nStates){
  #takes menu input and chosen report (focusplot) and returns plot object to UI
  if (is.null(focusplot) | (focusplot=="NA")) {return(plot_unavailable())}
  
  sSocialDist  = input$sdw[1]
  eSocialDist  = input$sdw[2]
  normalize    = grepl(paste(input$options,collapse="|"),"normalize")
  lookahead    = as.integer(input$look-eSocialDist)
  
  if (grepl(paste(input$options,collapse="|"),"log"))   {plotlog  = 1} else {plotlog  = 0} #changed from 0 1 boolean in interface late in game
  try({
    if (input$scope == "All")     {data = allData } #Choose the data to use 
    if (input$scope == "USA")     {data = amerData}
    if (input$scope == "World")   {data = worldData}
    
    if (!input$hotspots){if ((input$scope == "All")   ) {estate = input$region}     
                         if ((input$scope == "World") ) {estate = input$country}   
                         if ((input$scope == "USA")   ) {estate = input$state}}
                  else  {if ((input$scope == "All")   ) {estate = input$hregion}
                         if ((input$scope == "World") ) {estate = input$hcountry}
                         if ((input$scope == "USA")   ) {estate = input$hstate}}
    if (input$scope == "Custom")  {
      estate = input$cregion
      if (estate == "IR BE FR MA")       {estate=c("Ireland","Belgium","France","MA")}
      else if (estate == "Red vs. Blue") {estate=c("Deep _Red State","Deep Blue State")}
      else if (estate == "Blue States")  {estate= electoralBlue}
      else if (estate == "Red States")   {estate =electoralRed}
      else if (estate == "All States")   {estate = electoralAll}
      else if (estate == "Flattening Spectrum"){estate = c("Brazil","China","Russia","South_Korea","Switzerland","Germany","Iceland","USA","Croatia", "Sweden","Deep _Red State","Deep Blue State","Chile")}
      else if (estate == "Europe")       {estate= unique(allData$state[allData$continentExp=="Europe" ])}
      else if (estate == "Americas")     {estate= unique(allData$state[allData$continentExp=="Americas"])}
      else if (estate == "Oceania")      {estate= unique(allData$state[allData$continentExp=="Oceania"])}
      else if (estate == "Asia")         {estate= unique(allData$state[allData$continentExp=="Asia"   ])}
      else if (estate == "Africa")       {estate= unique(allData$state[allData$continentExp=="Africa" ])}
      else if (estate == "All Countries"){estate= unique(worldData$state)}
      data = allData[grepl(paste(estate,collapse = "|"),allData$state),]}
  }) #catch bad old hyperlinks with try
  
  if (length(estate)>1){overlay=TRUE}else{overlay=FALSE}
  
  if (input$mode=="Trends"){ if (input$scope=="World") { inputfeature=input$feature}        else {inputfeature=input$featureUSA} }
  else                      {if (input$scope=="World") { inputfeature=input$featureRanking} else {inputfeature=input$featureRankingUSA}}
  if (overlay & (inputfeature == "All"))   {return(plot_unavailable("for 'all' aspects for 2+ regions"))}
  if ((input$aspect == "CFR etc") & (inputfeature=="All"))   {return(plot_unavailable("for 'all' except daily/total"))}
  if ( grepl(paste( input$options, collapse = "|"), "march1"))  { data = data[data$rdate >= ymd("20200315"),]}
  
  daily=FALSE
  showhosp = 0
  showest  = 0 
  showtest = 0 
  if (focusplot == "Total Tests")           {return(plot_total(data,estate,plotlog,0,0,1      , showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Total Cases")           {return(plot_total(data,estate,plotlog,1,0,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Total Deaths")          {return(plot_total(data,estate,plotlog,0,1,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Total Est Cases")       {return(plot_total(data,estate,plotlog,0,0,showtest,showhosp,      1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Total Hospitalizations"){return(plot_total(data,estate,plotlog,0,0,showtest,1        ,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Total All")             {return(plot_total(data,estate,plotlog,1,1,1,       1       ,1      ,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  
  if (focusplot == "%Complete Tests")           {return(plot_total(data,estate,plotlog,0,0,1      , showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Cases")           {return(plot_total(data,estate,plotlog,1,0,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Deaths")          {return(plot_total(data,estate,plotlog,0,1,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Est Cases")       {return(plot_total(data,estate,plotlog,0,0,showtest,showhosp,      1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Hospitalizations"){return(plot_total(data,estate,plotlog,0,0,showtest,1        ,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete All")             {return(plot_total(data,estate,plotlog,1,1,1,       1       ,1      ,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  
  daily = TRUE
  if (focusplot == "Daily Cases")             {return(plot_total(data,estate,plotlog,1,0,0,showhosp,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Tests")             {return(plot_total(data,estate,plotlog,0,0,1,showhosp,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Deaths")            {return(plot_total(data,estate,plotlog,0,1,0,showhosp,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Hospitalizations")  {return(plot_total(data,estate,plotlog,0,0,0,1       ,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Est Cases")         {return(plot_total(data,estate,plotlog,0,0,0,showhosp,1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily All")               {return(plot_total(data,estate,plotlog,1,1,1,1       ,1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  
  if (focusplot == "Flattening Tests")            {return(plot_growth(focusplot,"test"                  ,"fracTestIncrease",data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Flattening Cases")            {return(plot_growth(focusplot,"positive"              ,"fracPositiveIncrease",data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Flattening Deaths")           {return(plot_growth(focusplot,"death"                 ,"fracDeathIncrease",   data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Flattening Hospitalizations") {return(plot_growth(focusplot,"hosp"                  ,"fracHospIncrease",    data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Flattening Est Cases" )       {return(plot_growth(focusplot,"positiveEst"           ,"fracPositiveIncreaseEst",data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  
  if (focusplot == "Growth (/million) Tests")            {return(plot_total(data,estate,plotlog,0,0,1      , showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Growth (/million) Cases")            {return(plot_total(data,estate,plotlog,1,0,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Growth (/million) Deaths")           {return(plot_total(data,estate,plotlog,0,1,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Growth (/million) Est Cases")        {return(plot_total(data,estate,plotlog,0,0,showtest,showhosp,      1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Growth (/million) Hospitalizations") {return(plot_total(data,estate,plotlog,0,0,showtest,1        ,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Growth (/million) All")              {return(plot_total(data,estate,plotlog,1,1,1,       1       ,1      ,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  
  if (focusplot == "Case Fatality Rate")           {return(plot_feature(data,"cfr",           focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "% Positive")                   {return(plot_feature(data,"fracpos",       focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "% Pop Tested")                 {return(plot_feature(data,"fracpoptested", focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Deaths per Hospitalization")   {return(plot_feature(data,"dperh",         focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "IFR Multiplier")               {return(plot_feature(data,"ifrRatio",      focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,""))}
  
  if (focusplot == "Incremental Case Fatality Rate")     {return(plot_feature(data,"cfrIncremental",       focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}     
  if (focusplot == "Incremental % Positive")             {return(plot_feature(data,"fracposIncremental",       focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Incremental % Pop Tested")                 {return(plot_feature(data,"fracpoptestedIncremental", focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}#not needed?
  if (focusplot == "Incremental Deaths per Hospitalization")   {return(plot_feature(data,"dperhIncremental",         focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Incremental IFR Multiplier")               {return(plot_feature(data,"ifrRatioIncremental",      focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist))}
  
  #Top/Bottom Summaries 
  if (focusplot == "Total Tests Summary")            { return(plot_now_summary(data,focusplot,plotlog,normalize,"test",     sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Cases Summary")            { return(plot_now_summary(data,focusplot,plotlog,normalize,"positive", sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Deaths Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"death",    sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Hospitalizations Summary") { return(plot_now_summary(data,focusplot,plotlog,normalize,"hosp",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Est Cases Summary")        { return(plot_now_summary(data,focusplot,plotlog,normalize,"positiveEst",sSocialDist,eSocialDist,nStates) )}
  
  if (focusplot == "%Complete Cases Summary")            { return(plot_now_summary(data,focusplot,plotlog,normalize,"pctPositiveComplete", sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "%Complete Deaths Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"pctDeathComplete",    sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "%Complete Hospitalizations Summary") { return(plot_now_summary(data,focusplot,plotlog,normalize,"pctHospComplete",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "%Complete Est Cases Summary")        { return(plot_now_summary(data,focusplot,plotlog,normalize,"pctPositiveEstComplete",sSocialDist,eSocialDist,nStates) )}
  
  if (focusplot == "Daily Tests Summary")            { return(plot_now_summary(data,focusplot,plotlog,normalize,"testIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Cases Summary")            { return(plot_now_summary(data,focusplot,plotlog,normalize,"positiveIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Deaths Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"deathIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Hospitalizations Summary") { return(plot_now_summary(data,focusplot,plotlog,normalize,"hospIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Est Cases Summary")        { return(plot_now_summary(data,focusplot,plotlog,normalize,"positiveIncreaseEst",sSocialDist,eSocialDist,nStates) )}
  
  if (focusplot == "% Positive Summary")                 { return(plot_now_summary(data,focusplot,plotlog,normalize,"fracpos",            sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "% Pop Tested Summary")               { return(plot_now_summary(data,focusplot,plotlog,0,"fracpoptested",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Case Fatality Rate Summary")         { return(plot_now_summary(data,focusplot,plotlog,0,"cfr",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "IFR Multiplier Summary")             { return(plot_now_summary(data,focusplot,plotlog,0,"ifrRatio",     sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Deaths per Hospitalization Summary") { return(plot_now_summary(data,focusplot,plotlog,0,"dperh",sSocialDist,eSocialDist,nStates))}
  
  if (focusplot == "Incremental % Positive Summary")                 { return(plot_now_summary(data,focusplot,plotlog,normalize,"fracposIncremental",            sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Incremental % Pop Tested Summary")               { return(plot_now_summary(data,focusplot,plotlog,0,"fracpoptestedIncremental",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Incremental Case Fatality Rate Summary")         { return(plot_now_summary(data,focusplot,plotlog,0,"cfrIncremental",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Incremental IFR Multiplier Summary")             { return(plot_now_summary(data,focusplot,plotlog,0,"ifrRatioIncremental",     sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Incremental Deaths per Hospitalization Summary") { return(plot_now_summary(data,focusplot,plotlog,0,"dperhIncremental",sSocialDist,eSocialDist,nStates))}
  
  if (focusplot == "Growth (/million) Tests Summary")            {return(plot_now_summary(data,focusplot,plotlog,0,"projectedTestGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Cases Summary")            {return(plot_now_summary(data,focusplot,plotlog,0,"projectedCaseGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Est Cases Summary")        {return(plot_now_summary(data,focusplot,plotlog,0,"projectedEstCaseGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Deaths Summary")           {return(plot_now_summary(data,focusplot,plotlog,0,"projectedDeathGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Hospitalizations Summary") {return(plot_now_summary(data,focusplot,plotlog,0,"projectedHospGrowth",sSocialDist,eSocialDist,nStates) )}

  if (focusplot == "Flattening Tests Summary")            {return(plot_now_summary(data,focusplot,plotlog,0,"TestGrowthRateDeclineRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Flattening Cases Summary")            {return(plot_now_summary(data,focusplot,plotlog,0,"CaseGrowthRateDeclineRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Flattening Est Cases Summary")        {return(plot_now_summary(data,focusplot,plotlog,0,"EstCaseGrowthRateDeclineRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Flattening Deaths Summary")           {return(plot_now_summary(data,focusplot,plotlog,0,"DeathGrowthRateDeclineRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Flattening Hospitalizations Summary") {return(plot_now_summary(data,focusplot,plotlog,0,"HospGrowthRateDeclineRate",sSocialDist,eSocialDist,nStates) )}
  
  return(plot_unavailable())} # if nothing matched

namePlot      <- function(input){
  #general naming based on inputs
  if (input$mode=="Rankings") {aspect       = input$aspectRanking } else { aspect = input$aspect}
  if (input$mode=="Trends"){ if (input$scope=="World") { inputfeature=input$feature}        else {inputfeature=input$featureUSA} }
  else                      {if (input$scope=="World") { inputfeature=input$featureRanking} else {inputfeature=input$featureRankingUSA}}
  pname = paste(aspect, inputfeature) 
  pname = str_replace(pname,"Hot","Growth (/million)")
  pname = str_replace(pname,"Estimated","Est")
  return(pname)}

identify_plot <- function(input){
  #pick plot based on UI choices
  if (input$mode=="Ranking") {aspect       = input$aspectRanking } else { aspect=input$aspect}
  if (input$mode=="Trends"){ if (input$scope=="World") { inputfeature=input$feature}        else {inputfeature=input$featureUSA} }
  else                      {if (input$scope=="World") { inputfeature=input$featureRanking} else {inputfeature=input$featureRankingUSA}}
  
  focusplot = namePlot(input) #default nameing
  #ratio reports
    if (aspect == "CFR etc" | aspect=="Incremental"){
      if (inputfeature  == "Tests")            {focusplot = "% Pop Tested" }
      if (inputfeature  == "Cases")            {focusplot = "% Positive" }
      if (inputfeature  == "Estimated Cases")  {focusplot = "IFR Multiplier" }
      if (inputfeature  == "Deaths")           {focusplot = "Case Fatality Rate" }
      if (inputfeature == "Hospitalizations")  {focusplot = "Deaths per Hospitalization"}
      if (aspect == "Incremental") {focusplot = paste("Incremental",focusplot)} }
    if (aspect == "All"){ # All Plots
      if (inputfeature == "Tests") {focusplot  = "Total Tests" }
      if (inputfeature == "Cases") {focusplot  = "Total Cases" }
      if (inputfeature == "Estimated Cases"){focusplot =  "Total Est Cases"}
      if (inputfeature == "Deaths"){focusplot =  "Total Deaths" }
      if (inputfeature == "Hospitalizations") {focusplot =  "Total Hospitalizations" }
      if (inputfeature == "All"){ if (aspect == "Total")      {focusplot=   "Total All" }
                                  if (aspect == "Daily")      {focusplot=   "Daily All"  }
                                  if (aspect == "Flattening") {focusplot=   "NA" }
                                  if (aspect == "CFR etc")    {focusplot=   "NA" }}}
      
    if (input$mode=="Rankings"){focusplot =  paste(focusplot, "Summary")}
    return(focusplot)}

#UI and Server for Shiny------------------------------------------
server <- function(input, output, session){ 
  #Shiny Server. Plot0 is main plot, Plots1-4 are for "all" reports, 5-6 where for comparisions
  output$Plot0 <- renderPlot(generate_plot(identify_plot(input),input,data,plotlog,lookahead,sSocialDist,eSocialDist,54))#single plot
  output$plot.ui <- renderUI({  plotOutput("Plot0", height=plotHeight)})
}

ui     <- function(request){
  #User interface
  scFun =  function(){unique(amerData$state)}
  wcFun =  function(){unique(worldData$state)}
  ccFun =  function(){unique(customData$state)}
  rcFun =  function(){unique(allData$state)}
  
  customFun = function() {return(c(c("IR BE FR MA"),c("Flattening Spectrum"), c("Red vs. Blue"),c("Red States"),c("Blue States"),c("All States"),c("Europe"),c("America"),c("Africa"),c("Asia"),c("Oceania"), c("All Countries")))}
  fluidPage(
    titlePanel("Covid-19 Data & Forecasts"),
    sidebarLayout(
      sidebarPanel(
        bookmarkButton(label="Share",inline=TRUE), 
        tags$a(href="http://app.jackprior.org", "RESET"),
        #tags$a(href="", "RESET"), #this doesn't reset bookmarked iphone 
        tags$a(href="https://covid19.jackprior.org/app-jackprior-org/", target="_blank","HELP"),
        radioButtons("mode",  "Which Analysis?", c("Trends","Rankings"), selected = c("Trends"), inline=TRUE),
        radioButtons("scope", "Where to Look?",  c("World","USA", "All", "Custom"),                  selected = "USA",      inline=TRUE),
        
        conditionalPanel(condition = "(input.scope =='All') &   (!input.hotspots)   & (input.mode == 'Trends')", selectInput("region",  'Select Countries/States', rcFun(), multiple=TRUE, selected = "World")),
        conditionalPanel(condition = "(input.scope =='World')&  (!input.hotspots)   & (input.mode == 'Trends')", selectInput("country", 'Select Countries',        wcFun(), multiple=TRUE, selected = refCountry )),
        conditionalPanel(condition = "(input.scope =='USA')   & (!input.hotspots)   & (input.mode == 'Trends')", selectInput("state",   'Select States',           scFun(), multiple=TRUE, selected = "MA" )),
        
        conditionalPanel(condition = "(input.hotspots & input.scope=='All'   & input.mode == 'Trends')", selectInput("hregion", 'All Hot Spots (+NY ref)',  rcFun(), multiple=TRUE, selected=c(refState,get_hot_spots(allData)))),
        conditionalPanel(condition = "(input.hotspots & input.scope=='World' & input.mode == 'Trends')", selectInput("hcountry",'World Hot Spots (+USA ref)',wcFun(), multiple=TRUE, selected=c(refCountry,get_hot_spots(worldData)))),
        conditionalPanel(condition = "(input.hotspots & input.scope=='USA'   & input.mode == 'Trends')", selectInput("hstate",  'US Hot Spots (+NY ref)',     scFun(), multiple=TRUE, selected=c(refState,get_hot_spots(amerData )))),
        conditionalPanel(condition = "(input.scope == 'Custom')",                                        selectInput("cregion",  'Select Custom Dataset',         customFun(), multiple=FALSE, selected = c("All States"))),
        checkboxInput("hotspots",    "Focus on Hot Spots", TRUE),
        
        conditionalPanel( condition = "input.mode  == 'Trends' & input.scope == 'World'", radioButtons("feature", "What Aspect?",           c("Cases", "Deaths",  "Estimated Cases",                             "All"), selected = "Cases",inline=TRUE)),
        conditionalPanel( condition = "input.mode  == 'Trends' & input.scope !== 'World'", radioButtons("featureUSA", "What Aspect?",        c("Cases", "Deaths",  "Estimated Cases", "Tests","Hospitalizations", "All"), selected = "Cases",inline=TRUE)),
        conditionalPanel( condition = "input.mode !== 'Trends' & input.scope == 'World'", radioButtons("featureRanking", "What Aspect?",    c("Cases", "Deaths",  "Estimated Cases"                                   ), selected = "Cases",inline=TRUE)), 
        conditionalPanel( condition = "input.mode !== 'Trends' & input.scope  !== 'World'", radioButtons("featureRankingUSA", "What Aspect?", c("Cases", "Deaths",  "Estimated Cases", "Tests","Hospitalizations"       ), selected = "Cases",inline=TRUE)),                     
                         
        conditionalPanel(condition = "input.mode  == 'Trends'",  radioButtons("aspect",  "What Dimension?",c("Total","Daily","Flattening", "CFR etc","Incremental"),  selected = "Daily",inline=TRUE) ),
        conditionalPanel(condition = "input.mode !== 'Trends' ", radioButtons("aspectRanking",  "What Dimension?", c("Total","Daily","Flattening", "Hot", "%Complete", "CFR etc","Incremental"), selected = "Daily",inline=TRUE)),
                                              
        checkboxGroupInput('options',"Options",choices=c("LogY"="log","/mil"="normalize","15Mar-"="march1"),selected=c("normalize","march1"), inline=TRUE),
        sliderInput("look", "What Forecast Horizon?",    min = Sys.Date()+4,            max = Sys.Date()+maxforecastdays, value = floor_date(Sys.Date()+defaultforecastdays,"month")),
        sliderInput("sdw",  "What Data for Model",       min = Sys.Date()-3*distwindow, max = Sys.Date()-1,               value = c(as.Date(as.Date(Sys.Date()-distwindow)),as.Date(Sys.Date()-1),round=TRUE,dragRange=FALSE))
        ),
      mainPanel(  uiOutput("plot.ui"))
      ))}

#Launch Program----------------------------------
refresh=TRUE
if (runLocal){ setwd("Documents/My R/covid19local/covid19code")}
try({load("alldata.RData" )
     if (max(amerData$rdate)<(Sys.Date()-1)){refresh=TRUE}else{refresh=FALSE}})
if (refresh|forceRefresh) {
  pp("reloading data")
  amerData     = get_amer_data(refresh)
  worldData    = get_world_data(refresh)
  newtonData   = get_newton_data() 
  USnoNYNJ              = region_aggregate( amerData[!grepl( "NY|NJ",                            amerData$state), ],  "US-NewYorkNewJersey") 
  USStateData           = region_aggregate( amerData[ grepl( paste(electoralAll,  collapse="|"), amerData$state), ],  "USA") #doesn't sum states without final entries. has hospitalization data 
  deepBlueStateData     = region_aggregate( amerData[ grepl( paste(deepBlueState, collapse="|"), amerData$state), ],  "Deep Blue State")
  deepRedStateData      = region_aggregate( amerData[ grepl( paste(deepRedState,  collapse="|"), amerData$state), ],  "Deep _Red State")
  blueStateData         = region_aggregate( amerData[ grepl( paste(electoralBlue, collapse="|"), amerData$state), ],  "VoteBlueState")
  redStateData          = region_aggregate( amerData[ grepl( paste(electoralRed,  collapse="|"), amerData$state), ],  "Vote_RedState")
  #worldData$state[worldData$state == "USA"]="USA_source2" 
  worldData = worldData[!worldData$state == "USA",]#replace international usa data with sum of states + some territories. 
  worldData    = rbind(worldData,USStateData)
  world        = region_aggregate(worldData,"World")
  allData      = rbind(amerData,worldData,newtonData,world,USnoNYNJ,deepBlueStateData,deepRedStateData,blueStateData,redStateData,USStateData)
  save(amerData,worldData,allData, file = "alldata.RData")}
if(!runLocal){shinyApp(ui = ui, server = server,  enableBookmarking = "url")} #select all run code for console access to data