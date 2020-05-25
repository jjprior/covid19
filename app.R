#Covid19 Analysis 
# Jack Prior
# http://app.jackprior.org 
# https://covid19.jackprior.org 
# covid19@jackprior.org 13May2020
# Licence: https://www.gnu.org/licenses/gpl-3.0.en.html

#reduce messages on shiny server at startup-------
shhh <- suppressPackageStartupMessages 
shhh(library(curl)) #You need to run install.packages("curl") etc once for each library to install
shhh(library(jsonlite))
shhh(library(lubridate))
shhh(library(stringr))
shhh(library(shiny))
shhh(library(scales))
shhh(library(tidyverse))
#----set some global values for program-----
runLocal           = FALSE # interactive at console rather than shiny - use with select all lines run
#runLocal           = TRUE # uncomment to run at console with select all lines run
forceRefresh       = FALSE #Uncomment to force reload of data from web
#forceRefresh       = TRUE #Uncomment to force reload of data from web
options(warn = 1, scipen = 999)#no scientific notation in plots
debugprint          = 1   #set = 1 for pp(txt,var) debug printing
lookback            = 50  #how far back model show plot model on top of data
distwindow          = 21  #how far back for default social distancing window
readyBack           = 21  #Days to use in "hot" growth projection
maxforecastdays     = 120 #maximum forward forecasting
defaultforecastdays = 60  #default forward forecasting
projectDays         = 21  #Days to project out for ranking case growth rate
caseRankThreshold   = 500 #min # of cases to be included in "ALL" rankings
deathRankThreshold  = 50  #min # of deaths to be included in "ALL" rankings
perCapitaDeathRankThreshold = 25  #poorly behaved regressions on very low death rates 
nHotspots = 4 # number of hot spots in drop down. 
refState    = "MA"
refCountry = "_USA"

pp   <- function(p1,p2="",p3="",p4=""){if (debugprint == 1){print(paste(p1,p2,p3,p4))}} #debug printing of 4 values

#GET DATA FROM WEB Or DISK--------------------------------------------------------
#Three functions below retrieve data for World, US, and Newton, MA (from disk csv updated periodically)
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
  x$positive   = NA #initialize columns
  x$death      = NA
  x            = select(x,rdate,pop,state,positive,positiveIncrease,death,deathIncrease) #thin down to needed data
  x            = x[order(x$rdate),]
  states       = unique(x$state)
  for (s in states){   #calculate cumulative counts for cases and deaths
    index = (x$state == s)
    x$positive[index]  = cumsum(x$positiveIncrease[index])  
    x$death[index]     = cumsum(x$deathIncrease[index])
  }
  x$hosp            = NA  
  x$hospIncrease    = NA
  x$fracHospIncrease = NA
  x$test            = NA
  x$testIncrease    = NA
  
  x$state[x$state == "Democratic_Republic_of_the_Congo"] = "DR Congo" #length and menu placement changes
  x$state[x$state == "United_Republic_of_Tanzania"]   = "Tanzania"
  x$state[x$state == "China"]                         = "_China"
  x$state[x$state == "France"]                        = "_France"
  x$state[x$state == "Ireland"]                      = "_Ireland"
  x$state[x$state == "Germany"]                       = "_Germany"
  x$state[x$state == "South_Korea"]                   = "_South_Korea"
  x$state[x$state == "United_States_of_America"]      = "_USA"
  x$state[x$state == "Georgia"]                       = "Rep of Georgia"
  x$state[x$state == "Cases_on_an_international_conveyance_Japan"] = "Intl Convenance Jp"
  x               = covid_calc(x)
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
  data        = data[,c("date","positive","death","positiveIncrease","deathIncrease","hospIncrease","hosp","test","state")]
  
  data$rdate  = parse_date_time(data$date,orders = "%y%m%d")
  spops       = read.csv("statepopstable.csv")  #spreadsheet of state populations
  
  data$test[data$state=="PR"]  = NA
  
  #Mass hospitilzation data is bogus from x to 4/22 -- fix this someday. 
  #index= ((data$state=="MA") & (data$rdate>ymd("20200407")) & (data$rdate< ymd("20200422") ))
  #data$hosp[index]=NA
  #data$hospIncrease[index]=NA
  
  data        = data[order(data$rdate),]
  states = unique(data$state)
  data$pop           = NA  #initialize population column
  data$testIncrease  = NA
  for (s in states){  #add population data 
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
  return(daily)
}

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
    newton$hosp = NA
    newton$hospIncrease   = NA
    newton$fracHospIncrease       = NA
    newton$test                  = NA
    newton$testIncrease          = NA
    newton    = covid_calc(newton)
    return(newton)}

region_aggregate <- function(data,state="_World"){ #sum up countries to world or states to USA
  #this doesn't deal with incomplete data for a country in final days and give smaller counts than full at end. 
  data = data[data$rdate < Sys.Date() , ] #avoid partial data for "today"
  Regionpositive               = aggregate(positive~rdate,             data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
  RegionpositiveIncrease       = aggregate(positiveIncrease~rdate,     data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
  RegiondeathIncrease          = aggregate(deathIncrease~rdate,        data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
  Regiondeath                  = aggregate(death~rdate,                data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
  Regionpop                    = aggregate(pop~rdate,                  data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
  Regiondata = merge(Regionpositive, Regiondeath,       by ="rdate", all=T)
  Regiondata = merge(Regiondata,RegionpositiveIncrease, by ="rdate", all=T)
  Regiondata = merge(Regiondata,RegiondeathIncrease,    by ="rdate", all=T)
  Regiondata = merge(Regiondata,Regionpop,              by ="rdate", all=T)
  if (state=="_USA"){
    Regiontest          = aggregate(test~rdate,                   data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
    RegiontestIncrease  = aggregate(testIncrease~rdate,           data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
    Regionhosp          = aggregate(hosp~rdate,                   data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
    RegionhospIncrease  = aggregate(hospIncrease~rdate,           data = data, FUN=sum, na.rm=TRUE, na.action=NULL)
    Regiondata          = merge(Regiondata,Regionhosp,         by ="rdate", all=T)
    Regiondata          = merge(Regiondata,RegionhospIncrease, by ="rdate", all=T)   
    Regiondata          = merge(Regiondata,Regiontest,         by ="rdate", all=T)
    Regiondata          = merge(Regiondata,RegiontestIncrease, by ="rdate", all=T)}
  else
    {Regiondata$test        = NA
    Regiondata$testIncrease = NA
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

covid_calc <- function(x){
  #Add calculated attributes to raw imported data
  x$cfr                 = x$death/x$positive
  x$cfrMarginal         = x$deathIncrease/x$positiveIncrease
  x$ifrRatio            = x$cfr/0.0066
  x$ifrRatioMarginal    = x$cfrMarginal/0.0066
  x$positiveEst         = x$positive*x$ifrRatio
  x$positiveIncreaseEst = x$positiveIncrease*x$ifrRatio
  x$dperh               = x$death/x$hosp #deaths per hospitalization
  x$dperhMarginal       = x$deathIncrease/x$hospIncrease
  x$fracpos             = x$positive/x$test
  x$fracposMarginal     = x$positiveIncrease/x$testIncrease
  x$fracpoptested       = x$test/x$pop
  x$fracpoptestedMarginal = x$testIncrease/x$pop
  x$positiveMarginal    = x$positiveIncrease/x$testIncrease
  x$deathMarginal       = x$deathIncrease/x$hospIncrease
  x$hospMarginal        = x$hospIncrease/x$positiveIncrease
  x$fracPositiveIncrease    = NA
  x$fracHospIncrease        = NA
  x$fracDeathIncrease       = NA
  x$fracPositiveIncreaseEst = NA
  x$fracTestIncrease        = NA
  x$mday                    = as.numeric(difftime(x$rdate,Sys.Date(),units=c("days"))) #need to regress against days relative to today. 
  x =  x[with(x,order(rdate)),]  
  thestates = unique(x$state)  #calculated day over day growth frac (rates) in cases, etc. 
  for (s in thestates){
    index= (x$state==s)
    x[index,] = calc_growth_since_last_change(x[index,],"positiveIncrease",    "positive",    "fracPositiveIncrease")
    x[index,] = calc_growth_since_last_change(x[index,],"positiveIncreaseEst", "positiveEst", "fracPositiveIncreaseEst")
    x[index,] = calc_growth_since_last_change(x[index,],"deathIncrease",       "death",       "fracDeathIncrease")
    x[index,] = calc_growth_since_last_change(x[index,],"hospIncrease",        "hosp",        "fracHospIncrease")
    x[index,] = calc_growth_since_last_change(x[index,],"testIncrease",        "test",        "fracTestIncrease")
    
    x$projectedCaseGrowthRate[index]  = project_growth_rate(x,s,   "positive",   "fracPositiveIncrease")
    x$projectedTestGrowthRate[index]  = project_growth_rate(x,s,   "test",       "fracTestIncrease")
    x$projectedDeathGrowthRate[index] = project_growth_rate(x,s,   "death",      "fracDeathIncrease")
    x$projectedHospGrowthRate[index]  = project_growth_rate(x,s,   "hosp",       "fracHospIncrease")
    x$projectedEstCaseGrowth[index] = project_growth_rate(x,s, "PositiveEst","fracPositiveIncreaseEst")
  
    x$projectedCaseGrowth[index]  = project_growth_rate(x,s,   "positive",   "fracPositiveIncrease")^2   *  tail(x$positive[index],1)     *1e6/x$pop[index] #get ppm growth rather than % growth
    x$projectedTestGrowth[index]  = project_growth_rate(x,s,   "test",       "fracTestIncrease")^2       *  tail(x$test[index],1)         *1e6/x$pop[index]
    x$projectedDeathGrowth[index] = project_growth_rate(x,s,   "death",      "fracDeathIncrease")^2      *  tail(x$death[index],1)        *1e6/x$pop[index]
    x$projectedHospGrowth[index]  = project_growth_rate(x,s,   "hosp",       "fracHospIncrease")^2       *  tail(x$hosp[index],1)         *1e6/x$pop[index]
    #x$projectedEstCaseGrowth[index] = project_growth_rate(x,s, "PositiveEst","fracPositiveIncreaseEst") * tail(x$positiveIncreseEst[index],1)   *1e6/x$pop[index]
  }
  x = get_pct_complete(x, 90, Sys.Date()-readyBack, Sys.Date()-1)
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
  yo  = head(c(NA,y),length(y))
  dyo = head(c(NA,dy),length(y))
  to  = head(c(NA,t),length(y))
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
  return(c(exp(model$coefficients[1]),model$coefficients[2]))
}

get_pct_complete <- function(x,  lookahead, sSocialDist, eSocialDist){
  #estimate % complete epidemic for a state - not used yet. 
  x =  x[with(x,order(rdate)),]  
  thestates = unique(x$state)
  for (s in thestates){
    index= (x$state==s) 
    index2 = index & (x$rdate>sSocialDist) & ( x$rdate<eSocialDist) 
    x$pctPositiveComplete[index] = NA
    x$pctDeathComplete[index]    = NA
    x$pctHospComplete[index]     = NA
    if (sum(!is.na(x$fracPositiveIncrease[index2]))     > 2) {x$pctPositiveComplete[index]    = x$positive[index]   /  max(calc_forecast(x,s,"positive",    "positiveIncrease",    "fracPositiveIncrease",   lookahead,sSocialDist, eSocialDist)$positive)    }
    if (sum(!is.na(x$fracDeathIncrease[index2]))        > 2) {x$pctDeathComplete[index]       = x$death[index]      /  max(calc_forecast(x,s,"death",       "deathIncrease",       "fracDeathIncrease",      lookahead,sSocialDist, eSocialDist)$death)       }
    if (sum(!is.na(x$fracPositiveIncreaseEst[index2]))  > 2) {x$pctPositiveEstComplete[index] = x$positiveEst[index]/  max(calc_forecast(x,s,"positiveEst", "positiveIncreaseEst", "fracPositiveIncreaseEst",lookahead,sSocialDist, eSocialDist)$positiveEst) }
    if (sum(!is.na(x$fracHospIncrease[index2]))         > 2) {x$pctHospComplete[index]        = x$hosp[index]       /  max(calc_forecast(x,s,"hosp",        "hospIncrease",        "fracHospIncrease",       lookahead,sSocialDist, eSocialDist)$hosp)         }}
  return(x)}

growth_estimate  <- function(date0,mday0,x0,lookahead,lookback, m){
  #create forecast from model 
  a      = unname(coef(m)[1])  #log(fracpostivve growth rate at date 0 fit)
  b      = unname(coef(m)[2])  #time constant for fracpositive growth rate decline
  if (b>0){b=0}  #don't extrapolate increases in growth rate. !!!!!!! - confirm. 
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

calc_forecast    <- function(data,cstate,totfeaturestr,increasefeaturestr, fracfeaturestr,lookahead,sSocialDist, eSocialDist){
  #calculate a fitted/forecasted total, daily, and growthrate for cases, estimated cases, deaths and hospitalizations. 
  data$totfeature =  data[[totfeaturestr]]
  data$fracfeature = data[[fracfeaturestr]]
  data$increasefeature = data[[increasefeaturestr]]
  index =          (data$state == cstate) & 
                   (data$rdate >= sSocialDist) & 
                   (data$rdate <= eSocialDist) & 
                   (data$fracfeature>0) & 
                   (!is.na(data$fracfeature))
  n = sum(index)
  if (n==0){return(NULL)}
  
  data = data[index,]
  cgmodel=NULL
  try({cgmodel = lm(log(fracfeature)~mday, data=data)})
  if (is.null(cgmodel)){return(NULL)}   #hack to deal with missing/insufficent data, need to clean up
  idx = !(abs(cgmodel$residuals)==max(abs(cgmodel$residuals)))
  if (sum(idx)==1){data=data[idx,]}
  data = data[with(data,order(rdate)),]
  try({cgmodel = lm(log(fracfeature)~mday, data=data)})
  if(is.null(cgmodel)){return(NULL)}
  date0 = max(data$rdate)
  newdates  = as.POSIXct(seq(date0-days(lookback-1),date0+days(lookahead-1), by="day"))
  x        = data.frame(rdate= newdates)
  
  case0 = max(data$totfeature)
  mday0 = max(data$mday)
  if (n>1 & !is.null(cgmodel)){
   cases   = growth_estimate(date0,mday0,case0,lookahead,lookback,cgmodel)
  newcases = c(NA,tail(cases,length(cases)-1)-head(cases,length(cases)-1))}
  else
  {
  cases=NA
  newcases=NA }
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
  stats::filter(x, rep(1 / n, n), sides = 2)} #7 day centered moving average

main_caption  <- function(s1,s2){paste(format(Sys.Date(), format = "%d%B") , " app.jackprior.org  - Social Dist. Basis: ",format(s1, format = "%d%b"),"-",format(s2, format = "%d%b"),sep = "")}

main_title    <- function(s,gr=""){ggtitle(paste(  str_replace( str_replace_all(s,"_"," "),"Summary",""), gr))}

p_add_vline      <- function(vdate){geom_vline(aes(xintercept =   as.numeric(as.POSIXct(vdate))))} # add vertical line on date plots

plot_unavailable <- function(){ggplot() + ggtitle("Report Unavailable")} #for menu choices with no data

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
  #maxrdate   =       max(rdates, na.rm = TRUE)
  if (!atmin){
  maxplotpts = max(plotpts,na.rm = TRUE)
  index = plotpts==maxplotpts
  maxrdate = max(rdates[index], na.rm= TRUE)}
  else
  {
    maxplotpts = min(plotpts,na.rm = TRUE) #I know, its a quick hack
    index = plotpts==maxplotpts
    maxrdate = min(rdates[index], na.rm=TRUE)}
  if (txt == ""){
  txt = round(maxplotpts)
  txt = format(txt, big.mark=",")}
  txt = str_replace(txt, "_","")
  return(annotate("text",x = maxrdate, y = maxplotpts, label = txt, parse=FALSE,vjust=0,hjust=1))}

p_annotate_text <- function(x,y,txt){#annotate with value at max date/value
  return(annotate("text",x = x, y = y, label = txt,vjust=0,hjust=0))}

mid_point   <- function(x, ht=0.5){ # place scaled amongst data for annotation.
  y = min(x)+ ht* (max(x)-min(x))
  return(y)}

format_plot <- function(p, estate, ytitle, plotlog,sSocialDist,eSocialDist,pct=0){
  # format plots 
  p = p + theme_set(theme_gray(base_size = 16))
  p = p + theme(legend.position = "bottom") 
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
  #p = p + expand_limits(x=as.POSIXct(ymd("20200301")))
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

#Main Plotting Functions -------------------------------------------------
plot_feature        <- function(sdata,feature,ftitle,cstate,plotlog,lookahead,sSocialDist,eSocialDist,overlay=FALSE){
  #Plot a feature generically
  sdata$y       = sdata[[feature]]
  overlay = length(cstate)>1
  if (overlay){sdata$flegend=sdata$state}else{sdata$flegend = feature}
  
  sdata         = subset(sdata, grepl(paste(cstate,collapse="|"),state) & !is.na(y))
  sdata         = subset(sdata,y>0) #get rid of trailing flat data
  if (nrow(sdata)==0){return(plot_unavailable())}  
  
  for (s in cstate){
    index=sdata$state==s
    sdata$movingAvg[index] = as.numeric(ma(sdata$y[index],7)) }
  
  if ((grepl("frac|CFR|cfr|per|dperh|Pct",ftitle))|(grepl("frac|CFR|cfr|per|dperh|Pct|Marginal",feature))){ispct=1}else{ispct=0}
  p=ggplot()  
  if (!overlay){p = p+geom_point(sdata, mapping=aes(x=rdate,y=y,color=flegend))}
  
  p = p + geom_line(data = sdata    , mapping =aes(x = rdate, y = movingAvg, colour = flegend))

    for (s in cstate){
      index= (sdata$state==s)
      p= p+p_annotate(sdata$rdate[index],sdata$movingAvg[index],plotlog,s)
    }
  
  p = format_date_plot(p,cstate,ftitle,plotlog,sSocialDist,eSocialDist,ispct)
  return(p)}

get_hot_spots = function(nowdata){ 
  sfeature="projectedCaseGrowth"
  nowdata         = nowdata[!nowdata$state == "_World",]        #leave the aggregated "World" out 
  nowdata$feature = nowdata[[sfeature]]                       #make the summarized feature accessible.   
  nowdata         = nowdata[ (nowdata$rdate > ( Sys.Date() - 8 ) ) & ( nowdata$rdate < Sys.Date() ), ] # limiit to last week 
  nowdata         = nowdata[!is.na(nowdata$feature),]         # limit to non-NA values
  nowdata = nowdata[( (nowdata$positive > caseRankThreshold) & #limit to major regions
                        (nowdata$death > deathRankThreshold)   &
                        ((nowdata$death*1e6/nowdata$pop) > perCapitaDeathRankThreshold)),] 
  nowdata$feature = nowdata$feature 
  nowdata         = nowdata[!is.na(nowdata$feature),]
  nowdata = select(nowdata,c(state,feature))
  state   = unique(nowdata$state)
  newdata = data.frame(state)
  states  = state
  if (nrow(nowdata)==0){return(NULL)}
  newdata$feature=NA
  for ( s in states ){
    index1 = nowdata$state == s
    index2 = newdata$state == s 
    newdata$feature[index2] = mean(nowdata$feature[index1]) } #consider using sum/days
  nowdata = newdata
  nowdata    = nowdata[order(-nowdata$feature),]
  hotspots = head(nowdata$state,nHotspots)
  return(levels(droplevels(hotspots)))}

plot_now_summary    <- function(nowdata, focusplot, plotlog, normalize, sfeature, sSocialDist, eSocialDist, nStates = 40, lookahead = NA ){ 
  #plot ranked bar graphs
  nowdata         = nowdata[!nowdata$state == "_World",]        #leave the aggregated "World" out of rankings. 
  nowdata$feature = nowdata[[sfeature]]                       #make the summarized feature accessible.   
  nowdata         = nowdata[ (nowdata$rdate > ( Sys.Date() - 8 ) ) & ( nowdata$rdate < Sys.Date() ), ] # limiit to last week 
  nowdata         = nowdata[!is.na(nowdata$feature),]         # limit to non-NA values
  nowdata = nowdata[( (nowdata$positive > caseRankThreshold) & 
                        (nowdata$death > deathRankThreshold)   &
                       ((nowdata$death*1e6/nowdata$pop) > perCapitaDeathRankThreshold)),] 
  if  (grepl("frac|cfr|per|dperh|Pct|pct|Rate",sfeature)|(grepl("Rate",focusplot))) {ispct=1}else{ispct=0}
  if (!ispct & normalize )
  { nowdata$feature = nowdata$feature / as.numeric(nowdata$pop) * 1e6
    nowdata         = nowdata[!is.na(nowdata$feature),]
    sfeature = "ppm"
    focusplot = paste(focusplot,("(per million)"))
    ispct = 0 }
  nowdata = select(nowdata,c(state,feature))
  state   = unique(nowdata$state)
  newdata = data.frame(state)
  states  = state
  if (nrow(nowdata)==0){return(NULL)}
  newdata$feature=NA
  for ( s in states ){
    index1 = nowdata$state == s
    index2 = newdata$state == s 
    newdata$feature[index2] = mean(nowdata$feature[index1]) } #consider using sum/days
  nowdata = newdata
  nowdata    = nowdata[order(-nowdata$feature),]
  nowdata$Status  = "All"
  if (nrow(nowdata)>nStates)
  { top = head(nowdata,nStates/2)
    bot = tail(nowdata,nStates/2)
    top$Status="High"
    bot$Status="Low"
    nowdata=rbind(top,bot)}
  
  if (nrow(nowdata)==0){  return(plot_unavailable())}else
  {p=ggplot(data=nowdata,mapping=aes(x=reorder(state,feature), y=feature, color=Status))+geom_bar(stat="identity")
  if (focusplot=="%Complete Cases Summary"){focusplot="%Cases done"}
  if (focusplot=="%Complete Deaths Summary"){focusplot="%Deaths done"}
  if (focusplot=="%Complete Hospitalizations Summary"){focusplot="%Hospitalizations done"}
  if (focusplot=="%Complete Est Cases Summary"){focusplot="%Est Cases done"}
  if (grepl("done",focusplot)){p = p +  geom_hline(aes(yintercept =  .9))}
  p = format_bar_plot(p,focusplot,"", plotlog,sSocialDist,eSocialDist,ispct)}
  return(p)}   

plot_trend  <- function(p,pdata,estate,plotlog,totFeature,increaseFeature,fracFeature,flegend,lookahead,sSocialDist,eSocialDist,normalize,daily=FALSE,overlay=FALSE){
  if (is.null(estate)){return(p)}
    assumedError  = .5  # 30% 3 sigma -- THIS SHOULD BE CODED based on fits eventually. 
    pdata$tot      = pdata[[totFeature]]
    pdata$increase = pdata[[increaseFeature]]
    pdata$frac     = pdata[[fracFeature]]
    pdata          = subset(pdata,   grepl(paste(estate,collapse="|"),state) & !is.na(tot) & !is.na(increase)) 
    
    if (normalize) { pdata$tot      = pdata$tot/pdata$pop*1e6     }
    if (normalize) { pdata$increase = pdata$increase/pdata$pop*1e6}
    
    if (nrow(pdata) == 0){ return(p) }
    
    forecast = NULL
    for (s in estate){
     index                  = (pdata$state==s & !is.na(pdata$increase))
  
     if (sum(index)>0){
       pdata$movingAvg[index]= as.numeric(ma(pdata$increase[index],7))
       temp                   = calc_forecast(pdata,s,"tot"   ,"increase"   ,"frac",   lookahead,sSocialDist,eSocialDist)
       temp$state             = s
       try({forecast               = rbind(forecast,temp)})
       forecast               = forecast[!is.na(forecast$increase),]}}
    
    if (!is.null(forecast) & !length(forecast)==0)  {
    if (!overlay){forecast$flegend = flegend} else {forecast$flegend = forecast$state}
    if (!overlay){pdata$flegend    = flegend} else{ pdata$flegend    = pdata$state}
    if ( overlay){forecast         = forecast[forecast$rdate>=(Sys.Date()-3),]}
    }
    madata = pdata[!is.na(pdata$movingAvg),]
    if(!daily){ #cumulative totals
       if (overlay){p = p +  geom_line(data = pdata, mapping =aes(x = rdate,y = tot, colour = flegend)) } #show as line in overlay
            else   {p = p + geom_point(data = pdata, mapping =aes(x = rdate,y = tot, colour = flegend)) }
        
       p = p + expand_limits(y = max(pdata$tot)*1.05)
        
       if ((!is.null(forecast)) & (length(forecast)>0)){
       if (!overlay){
           p = p + geom_line(data = forecast,mapping = aes(x = rdate,y = tot, colour = flegend),linetype="dashed") 
           p = p + expand_limits(y = max(forecast$tot)*1.05)
           p = p + p_annotate(pdata$rdate, pdata$tot, plotlog)
           p = p + p_annotate(forecast$rdate, forecast$tot, plotlog)}
       else {
          for (s in estate){
            index = (forecast$state == s)
            p =  p + p_annotate(forecast$rdate[index],forecast$tot[index],plotlog,s)}
         }
       p = p + geom_line(data = forecast,mapping = aes(x = rdate,y = tot, colour = flegend), linetype="dashed") 
       p = p + expand_limits(y = max(forecast$tot)*1.05)}}
    
   else #daily plot
      
     { p = p + geom_line(data = madata    , mapping =aes(x = rdate, y = movingAvg, colour = flegend))
       if (!overlay){
         hforecast= forecast
         lforecast = forecast
         hforecast$increase = hforecast$increase * (1 + assumedError)
         lforecast$increase = lforecast$increase * assumedError
         p = p + geom_point(data = pdata, mapping=aes(x = rdate,y = increase, colour = flegend))
         
         if (( !is.null(forecast) ) & ( sum( !is.na( forecast$tot ) ) > 0 ) ) {
           p = p + geom_line(data = hforecast, mapping =aes(x = rdate, y = increase, colour = flegend),linetype="dotted")
           p = p + geom_line(data = lforecast, mapping =aes(x = rdate, y = increase, colour = flegend),linetype="dotted")}}
         
      if (( !is.null(forecast) ) & ( sum( !is.na( forecast$tot ) ) > 0 ) ) {
         p = p + geom_line(data =  forecast, mapping =aes(x = rdate, y = increase, colour = flegend), linetype="dashed")}
      
        if (overlay){
          for (s in estate){
            index= (pdata$state==s)
            p= p+p_annotate(pdata$rdate[index],pdata$movingAvg[index],plotlog,s)
            index= (forecast$state==s)
            p= p+p_annotate(tail(forecast$rdate[index],1),tail(forecast$increase[index],1),plotlog,s)
            }}}
    
     #General Plot Attributes for both Total and Daily     
    if (daily)     {ytitle = "Daily Total"} else {ytitle = "Total"}
    if (overlay)   {ytitle = paste(ytitle,flegend)}
    if (normalize) { ytitle = paste( ytitle, "(per million)" ) }    
    if (!overlay){p = p + scale_colour_manual("", breaks = c("Deaths", "Cases", "Hospitalizations", "Est Cases", "Tests"), values = c("red","black", "green","blue", "orange")) }
     else {estate="" }#no title for overlays for now}
    p = format_date_plot(p, estate,ytitle, plotlog, sSocialDist, eSocialDist, 0)
    return(p)}

plot_total  <- function(data,estate,plotlog,showcase, showdeath, showtest, showhosp, showest, lookahead,sSocialDist,eSocialDist, normalize, daily=FALSE,overlay=FALSE) {
  #plot total and daily plots for 1 or more attributes - key routine in program.
  p = ggplot() 
  if (showcase  == 1) {p = plot_trend(p,data,estate,plotlog,"positive",    "positiveIncrease",    "fracPositiveIncrease",    "Cases",            lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showest   == 1) {p = plot_trend(p,data,estate,plotlog,"positiveEst", "positiveIncreaseEst", "fracPositiveIncreaseEst", "Est Cases",        lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showdeath == 1) {p = plot_trend(p,data,estate,plotlog,"death",       "deathIncrease",       "fracDeathIncrease",       "Deaths",           lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showtest == 1)  {p = plot_trend(p,data,estate,plotlog,"test",         "testIncrease",        "fracTestIncrease",       "Tests",            lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  if (showhosp  == 1) {p = plot_trend(p,data,estate,plotlog,"hosp",        "hospIncrease",        "fracHospIncrease",        "Hospitalizations", lookahead,sSocialDist,eSocialDist,normalize,daily,overlay)}
  return(p)}

plot_growth_original <- function(p,adata,sSocialDist, eSocialDist,lookahead){ 
  #Add a gray fit to early social distancing performance.
  if (eSocialDist<ymd("20200401")){eSocialDist=ymd("20200401")} #avoid crash with no data
  if (sum("_China" == adata$state) > 0) { #china distanced earlier than rest in 1st wave. 
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
  model   = lm(log(yf)~mday, data=fitdata)
  newdays   = data.frame(mday=c((min(sdata$mday)-lookback):(max(sdata$mday)+lookahead  )))
  newdates  = as.POSIXct(seq(min(sdata$rdate)-days(lookback),max(sdata$rdate)+days(lookahead), by="day"))
  newgrowth = exp(predict(model,newdays))
  plotit2   = data.frame(rdate=newdates,mday=newdays,death="NA",yf=newgrowth) 
  plotit2$flegend ="Early Performance"
  annodate     = mid_point(plotit2$rdate,.66)
  annoy        = mid_point(plotit1$yf,.66)
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
  
  pdata = adata
  pdata$movingAvg = NA
  
  for (s in gstate){
    index=  (pdata$state ==  s)
    pdata$movingAvg[index] = as.numeric(ma(pdata$yf[index],7))}
  
  if (overlay) {pdata$flegend=adata$state} else {pdata$flegend="Mov Avg"}

    p=ggplot()
    madata = pdata[!is.na(pdata$movingAvg),]
  if (overlay)
    {ptitle="Comparision of Flattening Rates"
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
  
    p=p + geom_point( adata,    mapping=aes(x = rdate,y = yf ))+
          geom_point( plotit1,  mapping=aes(x = rdate,y = yf, color = flegend))+
          geom_line( pdata,   mapping=aes(x = rdate,y = movingAvg, color = flegend))+
          geom_line(  plotit2,  mapping=aes(x = rdate,y = yf, color = flegend, ))
    
    p = p + annotate("text",x = annodate,y = annoy, label = lm_ln_eqn(model),parse=TRUE)
    ptitle = paste(gstate," ",round(model$coeff[2]*100,1),'% per day',sep="")
    p      = plot_growth_original(p,adata,ymd("20200324"),sSocialDist,lookahead)
    
    p = format_date_plot(p,ptitle,focusplot, 1,sSocialDist,eSocialDist,1)
    p = p+ scale_colour_manual("", breaks = c("Early Performance", "Fitted", "Data", "Mov Avg"),values = c("grey",    "red", "black","blue"))}
  
  p = format_date_plot(p,ptitle,focusplot, 1,sSocialDist,eSocialDist,1)
  return(p)} 

#Plot Selection based on UI choices ---- 
#identify_plot indentifies report to run and passes to generate_plot
generate_plot <- function(focusplot,input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,nStates){
  #takes menu input and chosen report (focusplot) and returns plot object to UI
  sSocialDist  = input$sdw[1]
  eSocialDist  = input$sdw[2]
  normalize    = input$normalize
  lookahead = as.integer(input$look-eSocialDist)
 
  if (!compareState){
    if (!input$hotspots){
      if ((input$scope == "All")   ) {estate = input$region}     
      if (input$scope  == "Custom" ) {estate = input$cregion}
      if ((input$scope == "World") ) {estate = input$country}   
      if ((input$scope == "USA")   ) {estate = input$state}}
    else {    
      if ((input$scope == "All")   ) {estate = input$hregion}
      if (input$scope  == "Custom" ) {estate = input$cregion}
      if ((input$scope == "World")   ) {estate = input$hcountry}
      if ((input$scope == "USA") ) {estate = input$hstate}}}
    else{
      if  (input$scope == "All")    {estate=input$region2} 
      if (input$scope == "Custom") {estate=input$cregion2}
      if (input$scope == "World")  {estate=input$country2}
      if (input$scope == "USA")    {estate=input$state2}}
      
  if (length(estate)>1){overlay=TRUE}else{overlay=FALSE}
  
  if (input$scope == "Custom")  {data = allData[grepl(paste(estate,collapse = "|"),allData$state),]}
  if (!input$hotspots){
   if (input$scope == "All")     {data = allData } #Choose the data to use 
   if (input$scope == "USA")     {data = amerData}
   if (input$scope == "World")   {data = worldData}}
  else
   {if (input$scope == "All")     {data = allData}
    if (input$scope == "USA")     {data = amerData}
    if (input$scope == "World")     {data = worldData}}
  
  if (input$march1){data=data[data$rdate>=ymd("20200315"),]}
  
  showhosp = 0
  showest  = 0 
  showtest = 0 
  if (input$log)  {plotlog  = 1} else {plotlog  = 0} #changed from 0 1 boolean in interface late in game
  if (is.null(focusplot)){return(plot_unavailable())}
  
  if (overlay & (input$feature == "All"))   {return(plot_unavailable())}
  if (focusplot == "NA")                    {return(plot_unavailable())}
  daily=FALSE
  if (focusplot == "Total Tests")           {return(plot_total(data,estate,plotlog,0,0,1      , showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Total Cases")           {return(plot_total(data,estate,plotlog,1,0,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Total Deaths")          {return(plot_total(data,estate,plotlog,0,1,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Total Est Cases")       {return(plot_total(data,estate,plotlog,0,0,showtest,showhosp,      1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Total Hospitalizations"){return(plot_total(data,estate,plotlog,0,0,showtest,1        ,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Total All")             {return(plot_total(data,estate,plotlog,1,1,1,       1       ,1      ,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  
  if (focusplot == "%Complete Tests")           {return(plot_total(data,estate,plotlog,0,0,1      , showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Cases")     {return(plot_total(data,estate,plotlog,1,0,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Deaths")          {return(plot_total(data,estate,plotlog,0,1,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Est Cases")       {return(plot_total(data,estate,plotlog,0,0,showtest,showhosp,      1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete Hospitalizations"){return(plot_total(data,estate,plotlog,0,0,showtest,1        ,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "%Complete All")             {return(plot_total(data,estate,plotlog,1,1,1,       1       ,1      ,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  daily=TRUE
  if (focusplot == "Daily Cases")             {return(plot_total(data,estate,plotlog,1,0,0,showhosp,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Tests")             {return(plot_total(data,estate,plotlog,0,0,1,showhosp,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Deaths")            {return(plot_total(data,estate,plotlog,0,1,0,showhosp,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Hospitalizations")  {return(plot_total(data,estate,plotlog,0,0,0,1       ,0,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily Est Cases")         {return(plot_total(data,estate,plotlog,0,0,0,showhosp,1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Daily All")               {return(plot_total(data,estate,plotlog,1,1,1,1       ,1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  
  if (focusplot == "Growth Rate Tests")            {return(plot_growth(focusplot,"test"                  ,"fracTestIncrease",data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Growth Rate Cases")            {return(plot_growth(focusplot,"positive"              ,"fracPositiveIncrease",data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Growth Rate Deaths")           {return(plot_growth(focusplot,"death"                 ,"fracDeathIncrease",   data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Growth Rate Hospitalizations") {return(plot_growth(focusplot,"hosp"                  ,"fracHospIncrease",    data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Growth Rate Est Cases" )       {return(plot_growth(focusplot,"positiveEst"           ,"fracPositiveIncreaseEst",data,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  
  if (focusplot == "Growth (/million) Tests")           {return(plot_total(data,estate,plotlog,0,0,1      , showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Growth (/million) Cases")           {return(plot_total(data,estate,plotlog,1,0,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily,overlay))}
  if (focusplot == "Growth (/million) Deaths")          {return(plot_total(data,estate,plotlog,0,1,showtest, showhosp,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Growth (/million) Est Cases")       {return(plot_total(data,estate,plotlog,0,0,showtest,showhosp,      1,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Growth (/million) Hospitalizations"){return(plot_total(data,estate,plotlog,0,0,showtest,1        ,showest,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  if (focusplot == "Growth (/million) All")             {return(plot_total(data,estate,plotlog,1,1,1,       1       ,1      ,lookahead,sSocialDist,eSocialDist,normalize,daily, overlay))}
  
  
  #if (focusplot == "Case Fatality Rate")           {return(plot_cfr(data,estate,plotlog,lookahead,sSocialDist,eSocialDist))} #includes a forecast component
  if (focusplot == "Case Fatality Rate")          {return(plot_feature(data,"cfr",       focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "% Positive")             {return(plot_feature(data,"fracpos",       focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "% Pop Tested")                 {return(plot_feature(data,"fracpoptested", focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Deaths per Hospitalization")   {return(plot_feature(data,"dperh",         focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "IFR Multiplier")               {return(plot_feature(data,"ifrRatio",      focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,""))}
  
  if (focusplot == "Marginal Case Fatality Rate")     {return(plot_feature(data,"cfrMarginal",       focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}     
  if (focusplot == "Marginal % Positive")             {return(plot_feature(data,"fracposMarginal",       focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Marginal % Pop Tested")                 {return(plot_feature(data,"fracpoptestedMarginal", focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}#not needed?
  if (focusplot == "Marginal Deaths per Hospitalization")   {return(plot_feature(data,"dperhMarginal",         focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist,overlay))}
  if (focusplot == "Marginal IFR Multiplier")               {return(plot_feature(data,"ifrRatioMarginal",      focusplot,estate,plotlog,lookahead,sSocialDist,eSocialDist))}
  
  #Top/Bottom Summaries 
  if (focusplot == "Total Tests Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"test",     sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Cases Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"positive", sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Deaths Summary")          { return(plot_now_summary(data,focusplot,plotlog,normalize,"death",    sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Hospitalizations Summary"){ return(plot_now_summary(data,focusplot,plotlog,normalize,"hosp",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Total Est Cases Summary")       { return(plot_now_summary(data,focusplot,plotlog,normalize,"positiveEst",sSocialDist,eSocialDist,nStates) )}
  
  if (focusplot == "%Complete Cases Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"pctPositiveComplete", sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "%Complete Deaths Summary")          { return(plot_now_summary(data,focusplot,plotlog,normalize,"pctDeathComplete",    sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "%Complete Hospitalizations Summary"){ return(plot_now_summary(data,focusplot,plotlog,normalize,"pctHospComplete",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "%Complete Est Cases Summary")       { return(plot_now_summary(data,focusplot,plotlog,normalize,"pctPositiveEstComplete",sSocialDist,eSocialDist,nStates) )}
  
  if (focusplot == "Daily Tests Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"testIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Cases Summary")           { return(plot_now_summary(data,focusplot,plotlog,normalize,"positiveIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Deaths Summary")          { return(plot_now_summary(data,focusplot,plotlog,normalize,"deathIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Hospitalizations Summary"){ return(plot_now_summary(data,focusplot,plotlog,normalize,"hospIncrease",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Daily Est Cases Summary")       { return(plot_now_summary(data,focusplot,plotlog,normalize,"positiveIncreaseEst",sSocialDist,eSocialDist,nStates) )}
  
  if (focusplot == "% Positive Summary")                 { return(plot_now_summary(data,focusplot,plotlog,normalize,"fracpos",            sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "% Pop Tested Summary")              { return(plot_now_summary(data,focusplot,plotlog,0,"fracpoptested",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Case Fatality Rate Summary")        { return(plot_now_summary(data,focusplot,plotlog,0,"cfr",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "IFR Multiplier Summary")            { return(plot_now_summary(data,focusplot,plotlog,0,"ifrRatio",     sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Deaths per Hospitalization Summary"){ return(plot_now_summary(data,focusplot,plotlog,0,"dperh",sSocialDist,eSocialDist,nStates))}
  
  
  if (focusplot == "Marginal % Positive Summary")               { return(plot_now_summary(data,focusplot,plotlog,normalize,"fracposMarginal",            sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Marginal % Pop Tested Summary")              { return(plot_now_summary(data,focusplot,plotlog,0,"fracpoptestedMarginal",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Marginal Case Fatality Rate Summary")        { return(plot_now_summary(data,focusplot,plotlog,0,"cfrMarginal",          sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Marginal IFR Multiplier Summary")            { return(plot_now_summary(data,focusplot,plotlog,0,"ifrRatioMarginal",     sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Marginal Deaths per Hospitalization Summary"){ return(plot_now_summary(data,focusplot,plotlog,0,"dperhMarginal",sSocialDist,eSocialDist,nStates))}
  
  if (focusplot == "Growth (/million) Tests Summary")           {return(plot_now_summary(data,focusplot,plotlog,0,"projectedTestGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Cases Summary")           {return(plot_now_summary(data,focusplot,plotlog,0,"projectedCaseGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Est Cases Summary")       {return(plot_now_summary(data,focusplot,plotlog,0,"projectedEstCaseGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Deaths Summary")          {return(plot_now_summary(data,focusplot,plotlog,0,"projectedDeathGrowth",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth (/million) Hospitalizations Summary"){return(plot_now_summary(data,focusplot,plotlog,0,"projectedHospGrowth",sSocialDist,eSocialDist,nStates) )}

  if (focusplot == "Growth Rate Tests Summary")           {return(plot_now_summary(data,focusplot,plotlog,0,"projectedTestGrowthRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth Rate Cases Summary")           {return(plot_now_summary(data,focusplot,plotlog,0,"projectedCaseGrowthRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth Rate Est Cases Summary")       {return(plot_now_summary(data,focusplot,plotlog,0,"projectedEstCaseGrowthRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth Rate Deaths Summary")          {return(plot_now_summary(data,focusplot,plotlog,0,"projectedDeathGrowthRate",sSocialDist,eSocialDist,nStates) )}
  if (focusplot == "Growth Rate Hospitalizations Summary"){return(plot_now_summary(data,focusplot,plotlog,0,"projectedHospGrowthRate",sSocialDist,eSocialDist,nStates) )}
  
  print(focusplot)
  
  return(plot_unavailable())
}

namePlot      <- function(input){
  #general naming based on inputs
  pname = paste(input$aspect, input$feature)
  pname = str_replace(pname,"Flattening","Growth Rate")
  pname = str_replace(pname,"Hot","Growth (/million)")
  pname = str_replace(pname,"Estimated","Est")
  return(pname)}

identify_plot <- function(input,n){
  #pick plot based on UI choices
   
    focusplot ="NA"
    focusplot2="NA"
    focusplot3="NA"
    focusplot4="NA"
    focusplot = namePlot(input) #default nameing
    #ratio reports
    if (input$aspect == "CFR etc"){
      if (input$feature  == "Tests")            {focusplot = "% Pop Tested" }
      if (input$feature  == "Cases")            {focusplot = "% Positive" }
      if (input$feature  == "Estimated Cases")  {focusplot = "IFR Multiplier" }
      if (input$feature  == "Deaths")           {focusplot = "Case Fatality Rate" }
      if (input$feature == "Hospitalizations")  {focusplot = "Deaths per Hospitalization"}}
    
    if (input$aspect == "Marginal"){
      if (input$feature  == "Tests")            {focusplot = "Marginal % Pop Tested" }
      if (input$feature  == "Cases")            {focusplot = "Marginal % Positive" }
      if (input$feature  == "Estimated Cases")  {focusplot = "Marginal IFR Multiplier" }
      if (input$feature  == "Deaths")           {focusplot = "Marginal Case Fatality Rate" }
      if (input$feature == "Hospitalizations")  {focusplot = "Marginal Deaths per Hospitalization"}}
        
    if (input$aspect == "All"){ # All Plots
      if (input$feature=="Tests")
       {focusplot  = "Total Tests" 
        focusplot2 = "Daily Tests"
        focusplot3 = "Growth Rate Tests"
        focusplot4 = "% Pop Tested"} 
      if (input$feature=="Cases")
       {focusplot  = "Total Cases" 
        focusplot2 = "Daily Cases"
        focusplot3 = "Growth Rate Cases"
        focusplot4 = "% Positive"}
      if (input$feature == "Estimated Cases")
      {focusplot =  "Total Est Cases" 
        focusplot2 = "Daily Est Cases"
        focusplot3 = "Growth Rate Est Cases"
        focusplot4 = "IFR Multiplier"}
      if (input$feature == "Deaths")
      {focusplot =  "Total Deaths" 
        focusplot2 = "Daily Deaths"
        focusplot3 = "Growth Rate Deaths"
        focusplot4 = "Case Fatality Rate"}
      if ((input$feature=="Hospitalizations") ) #& (input$scope == "USA")) 
       {focusplot =  "Total Hospitalizations" 
        focusplot2 = "Daily Hospitalizations"
        focusplot3 = "Growth Rate Hospitalizations"
        focusplot4 = "Deaths per Hospitalization"}
      if (input$feature=="All"){
        if (input$aspect == "Total")      {focusplot=   "Total All" }
        if (input$aspect == "Daily")      {focusplot=   "Daily All"  }
        if (input$aspect == "Flattening") {focusplot=   "NA" }
        if (input$aspect == "CFR etc")    {focusplot=   "NA"            }
        if (input$aspect == "All")        {focusplot=   "Total All"  
                                           focusplot2 = "Daily All"  }}} # end if all 
    if (grepl("Rank",input$mode)){
      focusplot =  paste(focusplot, "Summary")
      focusplot2 = paste(focusplot2,"Summary")
      focusplot3 = paste(focusplot3,"Summary")
      focusplot4 = paste(focusplot4,"Summary")}
    focusplots = c(focusplot, focusplot2, focusplot3, focusplot4, focusplot, focusplot)
    focusplot  = focusplots[n]
    return(focusplot)}

#UI and Server for Shiny------------------------------------------
server <- function(input, output, session){ 
  #Shiny Server. Plot0 is main plot, Plots1-4 are for "all" reports, 5-6 for comparisions
  compareState=FALSE
  
  output$Plot0 <- renderPlot(generate_plot(identify_plot(input,1),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,54))#single plot
  output$Plot1 <- renderPlot(generate_plot(identify_plot(input,1),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,20))#4 box
  output$Plot2 <- renderPlot(generate_plot(identify_plot(input,2),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,20))
  output$Plot3 <- renderPlot(generate_plot(identify_plot(input,3),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,20))
  output$Plot4 <- renderPlot(generate_plot(identify_plot(input,4),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,20))
  output$Plot5 <- renderPlot(generate_plot(identify_plot(input,5),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,20))#compare1  
  output$Plot6 <- renderPlot(generate_plot(identify_plot(input,6),input,data,TRUE,plotlog,lookahead,sSocialDist,eSocialDist,20))        #compare2 
  output$Plot7 <- renderPlot(generate_plot(identify_plot(input,1),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,20)) #all all 
  output$Plot8 <- renderPlot(generate_plot(identify_plot(input,2),input,data,compareState,plotlog,lookahead,sSocialDist,eSocialDist,20)) #all all
  #  query <- parseQueryString(session$clientData$url_search)
  #  paste(names(query), query, sep = "=", collapse=", ")
  }

ui     <- function(request){
  #User interface
  scFun =  function(){unique(amerData$state)}
  wcFun =  function(){unique(worldData$state)}
  ccFun =  function(){unique(customData$state)}
  rcFun =  function(){unique(allData$state)}
  fluidPage(
    titlePanel("Covid-19 Data & Forecasts"),
    sidebarLayout(
      sidebarPanel(
        bookmarkButton(label="Share",inline=TRUE), 
        #tags$a(href="http://app.jackprior.org", "RESET"),
        tags$a(href="", "RESET"),tags$a(href="https://covid19.jackprior.org/app-jackprior-org/", target="_blank","HELP"),
        radioButtons("mode",  "Which Analysis?", c("Trends","Comparisons","Rankings"), selected = c("Trends"), inline=TRUE),
        radioButtons("scope", "Where to Look?",  c("World","USA", "All", "Custom"),                  selected = "USA",      inline=TRUE),
        checkboxInput("hotspots",    "Focus on Hot Spots", TRUE),
        conditionalPanel(condition = "(input.scope =='All') &   (!input.hotspots)   & ((input.mode == 'Trends') |(input.mode == 'Comparisons'))", selectInput("region",  'Select Countries/States', rcFun(), multiple=TRUE, selected = "_World")),
        conditionalPanel(condition = "(input.scope =='World')&  (!input.hotspots)   & ((input.mode == 'Trends') |(input.mode == 'Comparisons'))", selectInput("country", 'Select Countries',        wcFun(), multiple=TRUE, selected = refCountry )),
        conditionalPanel(condition = "(input.scope =='USA')   & (!input.hotspots)   & ((input.mode == 'Trends') |(input.mode == 'Comparisons'))", selectInput("state",   'Select States',           scFun(), multiple=TRUE, selected = refState )),
        
        conditionalPanel(condition = "(input.hotspots & input.scope=='All'   & ((input.mode == 'Trends') |(input.mode == 'Comparisons')))", selectInput("hregion", 'All Hot Spots (Use Reset)',  rcFun(), multiple=TRUE, selected=c(refState,get_hot_spots(allData)))),
        conditionalPanel(condition = "(input.hotspots & input.scope=='World' & ((input.mode == 'Trends') |(input.mode == 'Comparisons')))", selectInput("hcountry",'World Hot Spots (Use Reset)',wcFun(), multiple=TRUE, selected=c(refCountry,get_hot_spots(worldData)))),
        conditionalPanel(condition = "(input.hotspots & input.scope=='USA'   & ((input.mode == 'Trends') |(input.mode == 'Comparisons')))", selectInput("hstate",  'US Hot Spots (Use Reset)',     scFun(), multiple=TRUE, selected=c(refState,get_hot_spots(amerData )))),
        conditionalPanel(condition = "(input.scope == 'Custom')",                                                              selectInput("cregion",  'Select Custom Dataset',         rcFun(), multiple=TRUE, selected = c("_Ireland","Belgium","_France","MA"))), 
        
        conditionalPanel(condition = "(input.scope =='All')    & (input.mode == 'Comparisons')",    selectInput("region2",  'Select comparision',      rcFun(),  multiple=TRUE, selected = "_USA" )),
        conditionalPanel(condition = "(input.scope =='World')  & (input.mode == 'Comparisons')",    selectInput("country2", 'Select comparison',       wcFun(),  multiple=TRUE, selected = "_USA" )),
        conditionalPanel(condition = "(input.scope =='USA')    & (input.mode == 'Comparisons')",    selectInput("state2",   'Select comparison',       scFun(),  multiple=TRUE, selected = "CT"   )),
        conditionalPanel(condition = "(input.scope =='Custom') & (input.mode == 'Comparisons')",    selectInput("cregion2", 'Select comparision',      rcFun(),   multiple=TRUE, selected= "MA"  )),
        
        radioButtons("feature", "What Aspect?",   c("Cases", "Deaths",  "Estimated Cases", "Tests","Hospitalizations", "All"), selected = "Cases",inline=TRUE),
        radioButtons("aspect",  "What Dimension?",c("Total","Daily","Flattening", "Hot", "%Complete", "CFR etc","Marginal","All"),                selected = "Daily",inline=TRUE),
        checkboxInput("log",    "Log Scale", FALSE),
        checkboxInput("normalize","Normalize?",value=TRUE),checkboxInput("march1","Start 15Mar?",value=TRUE),
        sliderInput("look", "What Forecast Horizon?",    min = Sys.Date()+4,            max = Sys.Date()+maxforecastdays, value = floor_date(Sys.Date()+defaultforecastdays,"month")),
        sliderInput("sdw",  "What Data for Model",       min = Sys.Date()-3*distwindow, max = Sys.Date()-1,               value = c(as.Date(as.Date(Sys.Date()-distwindow)),as.Date(Sys.Date()-1),round=TRUE,dragRange=FALSE))
        ),
      mainPanel(
                 conditionalPanel(condition = "(input.aspect !== 'All') & ( input.mode !== 'Comparisons')",  plotOutput("Plot0",height="750px")),
                 conditionalPanel(condition = "input.mode  == 'Comparisons'",
                                  fluidRow(column(12,  plotOutput("Plot5"))),
                                  fluidRow(column(12,  plotOutput("Plot6")))),
                conditionalPanel(condition = "(input.aspect  == 'All') & (input.feature !== 'All')",
                                  fluidRow(column(6,  plotOutput("Plot1")), column(6,plotOutput("Plot3"))),
                                  fluidRow(column(6,  plotOutput("Plot2")), column(6,plotOutput("Plot4")))),
                conditionalPanel(condition = "(input.aspect  == 'All') & (input.feature == 'All')",
                                 fluidRow(column(12,  plotOutput("Plot7"))),
                                 fluidRow(column(12,  plotOutput("Plot8")))))))}

#Launch Program----------------------------------
refresh=TRUE
if (runLocal){ setwd("Documents/My R/covid19local/covid19code")}
try({load("alldata.RData" )
     if (max(allData$rdate)<(Sys.Date()-1)){refresh=TRUE}else{refresh=FALSE}})
#forceRefresh=TRUE
if (refresh|forceRefresh) {
  print("reloading data")
  amerData     = get_amer_data(refresh)
  worldData    = get_world_data(refresh)
  newtonData   = get_newton_data() 
  USnoNYNJ     = region_aggregate(amerData[!grepl("NY|NJ",amerData$state), ],"US-NYNJ") #doesn't sum states without final entries. has hospitalization data 
  #worldData    = worldData[!(worldData$state == "_USA"),]#replace international usa data with sum of states + some territories. 
  #worldData    = rbind(worldData,USStates)
  world        = region_aggregate(worldData,"_World")
  allData      = rbind(amerData,worldData,newtonData,world,USnoNYNJ)
  save(amerData,worldData,allData, file = "alldata.RData")}
if(!runLocal){shinyApp(ui = ui, server = server,  enableBookmarking = "url")} #select all run code for console access to data