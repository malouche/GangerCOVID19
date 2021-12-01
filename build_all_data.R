source('function_data.R')

load('tools.RData')
library(dplyr)
dcov=update_covid_data()


i=match(dcov$iso3c,data_geo_all$iso3c)
j=grep('iso3c',colnames(data_geo_all))
dcov=cbind.data.frame(dcov,data_geo_all[i,-j]) # iso3c
dcov=dcov%>%na.omit()


dvac=update_vacc_data_V2(x_iso3c = unique(dcov$iso3c))





dcov=dcov%>%full_join(dvac,by=c('date','iso3c'))



dcov_US=update_covid_US_data()

dlast=max(dcov$date)


usa_states=unique(dcov_US$iso3c)


dvac_US=update_vacc_US_data_V2(x_iso3c = unique(dcov_US$iso3c))

i=match(dcov_US$iso3c,data_geo_all$iso3c)
dcov_US=cbind.data.frame(dcov_US,data_geo_all[i,-j])


dvac_US=update_vacc_US_data_V2(x_iso3c = unique(dcov_US$iso3c))
dcov_US=dcov_US%>%full_join(dvac_US,by=c('date','iso3c'))

###

dcov$type='country'
dcov_US$type='USA'
dcov_all=dcov%>%bind_rows(dcov_US)

###

dcov_all=dcov_all%>%mutate(vaccination=ifelse(is.na(vaccination)==T,0,vaccination))


dcov_all=dcov_all%>%group_by(iso3c)%>%mutate(confirmed=cummax(confirmed),
                                             deaths=cummax(deaths),vaccination=cummax(vaccination))




###

i=which(is.na(dcov_all$confirmed)==T)
if(length(i)>0){
  dcov_all=dcov_all[-i,]
}


dcov_all=dcov_all%>%group_by(iso3c)%>%mutate(active=confirmed-recovered-deaths)

dcov_all=dcov_all%>%group_by(iso3c)%>%mutate(people_vaccinated=ifelse(is.na(people_vaccinated)==T,0,people_vaccinated))

dcov_all=dcov_all%>%group_by(iso3c)%>%mutate(len_iso3c=length(iso3c))%>%filter(len_iso3c>180)%>%select(-len_iso3c)

dcov_all=dcov_all%>%group_by(iso3c)%>%mutate(pfv_temp=lag(people_fully_vaccinated,13))%>%
  mutate(pfv_temp=ifelse(is.na(pfv_temp)==T,0,pfv_temp))%>%
  mutate(immunized=pfv_temp+c(confirmed[1:180],diff(confirmed,180))-deaths)%>%select(-pfv_temp)

dcov_all=dcov_all%>%group_by(iso3c)%>%mutate(immunized=ifelse(is.na(immunized)==T,0,immunized))%>%mutate(immunized=cummax(immunized))


  
dcov_all=dcov_all%>%group_by(iso3c)%>%arrange(date, .by_group = TRUE)%>%
  mutate(dcases=c(confirmed[1],diff(confirmed)),
         ddeaths=c(deaths[1],diff(deaths)),
         drecovered=c(recovered[1],diff(recovered)),
         dvaccinated=c(vaccination[1],diff(vaccination)),
         dimmunized=c(immunized[1],diff(immunized)))

dcov_all=dcov_all%>%mutate(dvaccinated=ifelse(is.na(dvaccinated)==T,0,dvaccinated))

####

x=dcov_all%>%group_by(iso3c)%>%summarise(x=length(iso3c))

isoc3c_2keep=x$iso3c[x$x>50]


dcov_all=dcov_all%>%filter(iso3c%in%isoc3c_2keep)


####


sumNA<-function(x) sum(x,na.rm=T)


d_world=dcov_all%>%filter(type=='country')%>%ungroup%>%select(date,confirmed,deaths,recovered,active,vaccination,immunized,dcases,ddeaths,drecovered,dvaccinated,dimmunized,population,people_vaccinated,people_fully_vaccinated)%>%group_by(date)%>%summarise_all(funs(sumNA))


d_world$population=max(d_world$population)

d_world$type='WORLD'

d_world$iso3c='WORLD'

###

d_continent=dcov_all%>%filter(type=='country')%>%ungroup%>%select(continent,date,confirmed,deaths,recovered,active,vaccination,immunized,dcases,ddeaths,drecovered,dvaccinated,dimmunized,population,people_vaccinated,people_fully_vaccinated)%>%group_by(date,continent)%>%summarise_all(funs(sumNA))

d_continent$population=max(d_continent$population)


d_continent$type='continent'

d_continent=d_continent%>%rename(iso3c=continent)



d_region=dcov_all%>%filter(type=='country')%>%ungroup%>%select(region,date,confirmed,deaths,recovered,active,vaccination,immunized,dcases,ddeaths,drecovered,dvaccinated,dimmunized,population,people_vaccinated,people_fully_vaccinated)%>%group_by(date,region)%>%summarise_all(funs(sumNA))

d_region$population=max(d_region$population)


d_region$type='region'

d_region=d_region%>%rename(iso3c=region)

d_income=dcov_all%>%filter(type=='country')%>%ungroup%>%select(income,date,confirmed,deaths,recovered,active,vaccination,immunized,dcases,ddeaths,drecovered,dvaccinated,dimmunized,population,people_vaccinated,people_fully_vaccinated)%>%group_by(date,income)%>%summarise_all(funs(sumNA))

d_income$population=max(d_income$population)

d_income$type='income'

d_income=d_income%>%rename(iso3c=income)


d_georegion=dcov_all%>%filter(type=='country')%>%ungroup%>%select(georegion,date,confirmed,deaths,recovered,active,vaccination,immunized,dcases,ddeaths,drecovered,dvaccinated,dimmunized,population,people_vaccinated,people_fully_vaccinated)%>%group_by(date,georegion)%>%summarise_all(funs(sumNA))

d_georegion$population=max(d_georegion$population)


d_georegion$type='georegion'

d_georegion=d_georegion%>%rename(iso3c=georegion)

d_EU=dcov_all%>%filter(type=='country')%>%ungroup%>%select(EU,date,confirmed,deaths,recovered,active,vaccination,immunized,dcases,ddeaths,drecovered,dvaccinated,dimmunized,population,people_vaccinated,people_fully_vaccinated)%>%group_by(date,EU)%>%summarise_all(funs(sumNA))%>%filter(EU=='EU')

d_EU$population=max(d_EU$population)

d_EU$type='EU'

d_EU=d_EU%>%rename(iso3c=EU)


dcov_all=dcov_all%>%bind_rows(d_world)%>%
  bind_rows(d_continent)%>%
  bind_rows(d_region)%>%
  bind_rows(d_income)%>%
  bind_rows(d_georegion)%>%
  bind_rows(d_EU)

### id

i=which(dcov_all$type=='region' & dcov_all$iso3c=='North America')
dcov_all$iso3c[i]='North America(region)'
i=which(dcov_all$type=='region' & dcov_all$iso3c=='South America')

i=which(dcov_all$type=='continent' & dcov_all$iso3c=='South America')
dcov_all$iso3c[i]='South America(continent)'


i=which(dcov_all$type=='continent' & dcov_all$iso3c=='North America')
dcov_all$iso3c[i]='North America(continent)'


dcov_all$id=paste0(dcov_all$iso3c,dcov_all$type)

xx=na.omit(unique(dcov_all$region))
z=plyr::mapvalues(dcov_all$region,from=xx,
                  to=c(xx[1:6],'North America(region)'))


dcov_all$region=z


xx=na.omit(unique(dcov_all$continent))
z=plyr::mapvalues(dcov_all$continent,from=xx,
                  to=c(xx[1:3],'South America(continent)','North America(continent)',xx[6]))

dcov_all$continent=z
### Stat by 1M pop

dcov_all=dcov_all%>%group_by(id)%>%mutate(c1M=confirmed/population*10^6,
                                          d1M=deaths/population*10^6,
                                          r1M=recovered/population*10^6,
                                          a1M=active/population*10^6,
                                          v1M=vaccination/population*10^6,
                                          mortrate=deaths/confirmed,
                                          activerate=active/confirmed,
                                          mortrate_back=cumsum(rev(ddeaths))[length(id):1]/cumsum(rev(dcases))[length(id):1])





dcov_all=dcov_all%>%group_by(id)%>%mutate(pV=vaccination/population,
                                          pI=immunized/population,
                                          daysvac=runner::sum_run(vaccination>0),
                                          dailyvac=runner::mean_run(dvaccinated,7))
###

dcov_all=dcov_all%>%group_by(id)%>%mutate(dcases_r7=runner::mean_run(dcases,7),
                                          dcases_r14=runner::mean_run(dcases,14),
                                          ddeaths_r7=runner::mean_run(ddeaths,7),
                                          ddeaths_r14=runner::mean_run(ddeaths,14),
                                          dvacc_r7=runner::mean_run(dvaccinated,7),
                                          dvacc_r14=runner::mean_run(dvaccinated,14),
                                          dimm_r7=runner::mean_run(dimmunized,7),
                                          dimm_r14=runner::mean_run(dimmunized,14))



dcov_all=dcov_all%>%group_by(id)%>%mutate(mortrate_back=cumsum(rev(ddeaths_r14))[length(id):1]/cumsum(rev(dcases_r14))[length(id):1])


growth_rate<-function(x,k){
  xx=na.omit(lag(x,k))
  z=diff(x,k)/xx
  z=ifelse(xx==0,NA,z)
  p=length(z)
  q=length(x)
  z=c(rep(NA,q-p),z)
  z
  
}

dcov_all=dcov_all%>%group_by(id)%>%mutate(dcases_g1=growth_rate(dcases_r14,1),
                                          dcases_g7=growth_rate(dcases_r14,7),
                                          dcases_g14=growth_rate(dcases_r14,14),
                                          ddeaths_g1=growth_rate(ddeaths_r14,1),
                                          ddeaths_g7=growth_rate(ddeaths_r14,7),
                                          ddeaths_g14=growth_rate(ddeaths_r14,14))




doubling_time<-function(x){
  x[is.na(x)==T]=0
  n=1:length(x)
  z=sapply(n,function(i) sum((x[1:i]>x[i]/2)))
  z=unlist(z)
  z
}



dcov_all=dcov_all%>%group_by(id)%>%mutate(doub_cases=doubling_time(confirmed),
                                          doub_deaths=doubling_time(deaths))%>%
  mutate(doub_cases=ifelse(doub_cases>0,doub_cases,NA),
         doub_deaths=ifelse(doub_deaths>0,doub_deaths,NA))


x=dcov_all%>%group_by(id)%>%summarise(vacc=max(vaccination,na.rm=T))%>%filter(vacc>0)

dcov_all=dcov_all%>%filter(id%in%x$id)

####################################
## R0 ##############################
####################################


m=3.96
sig_m=(4.39-3.53)/2/1.96
ml=3.53
mu=4.39

std=4.75
std_sig=(5.07-4.46)/2/1.96
stdl=4.46
stdu=5.07


dcov_all$R0=NA
dcov_all$dR0=NA

library(EpiEstim)
estim_R0<-function(xf){
  x=round(as.numeric(xf))
  x[x<0]=0
  
  if(sum(x)>50){
    res <- suppressMessages(estimate_R(x, method = "parametric_si",
                                       config = make_config(list(mean_si = m, std_si = std))))
    
    yy=res$R$`Mean(R)`
    yy=c(rep(NA,length(x)-length(yy)),yy)
  }
  else yy=NA
  return(yy)
  
}


id=unique(dcov_all$id)
p=length(id)

for(j in 1:p){
  cat("\r",j,"/",p)
  i=which(dcov_all$id==id[j])
  type=unique(dcov_all$type[i])
  xf1=dcov_all$dcases_r14[i]
  xf1[is.na(xf1)==T]=0
  xf2=dcov_all$ddeaths_r14[i]
  xf2[is.na(xf2)==T]=0
  yyC=estim_R0(xf1)
  yyD=estim_R0(xf2)
  dcov_all$R0[i]=yyC
  dcov_all$dR0[i]=yyD
}



######################################################################
#### Making links  ###################################################
######################################################################



################

dcov_all=dcov_all%>%mutate(z=gsub(" ","",id),
                           z=gsub("-","",z),
                           z=gsub("\\&","",z),
                           z=gsub("\\(","",z),
                           z=gsub("\\)","",z),
                           z=tolower(z))%>%mutate(link=paste0("htmlfiles/",z,".html"))%>%
  mutate(link=ifelse(type=='country',paste0("<a  target=_self href=",link,">",country,"</a>"),
                     paste0("<a  target=_self href=",link,">",iso3c,"</a>")))%>%select(-z)





z=gsub(" ","",dcov_all$id)
z=gsub("-","",z)
z=gsub("\\&","",z)
z=tolower(z)
z=gsub("\\(","",z)
z=gsub("\\)","",z)
z=paste0('htmlfiles/',z,'.html')
dcov_all$htmlfile=z






i=which(is.na(dcov_all$flags)==T)
dcov_all$flags[i]=paste0(dcov_all$iso3c[i],'.png')


dcov_all=dcov_all%>%group_by(id)%>%mutate(days=1:length(id))


###### Immunisation

source('predict_herdIm.R')
library(forecast)
set.seed(12395)
dv=dcov_all%>%group_by(id)%>%mutate(days=length(id))%>%group_by(id)%>%summarise(pI0=ifelse(max(vaccination)==0,
                                                                                           max(pI),pI[max(days)-max(daysvac)]))
i=match(dcov_all$id,dv$id)

dcov_all$pI0=dv$pI0[i]


date_today=max(dcov_all$date)
ddlast=dcov_all%>%filter(date==date_today)%>%ungroup%>%select(id,dcases,ddeaths,dcases_g1,ddeaths_g1,R0,dR0,doub_cases,doub_deaths,vaccination,daysvac,dailyvac,pV,pI,pI0)


dlast=max(dcov_all$date)
dvax=ddlast%>%select(id,vaccination,daysvac,dailyvac,pV,pI,pI0)
dvax$firstday=dlast-dvax$daysvac

###


noUS=c("Virgin IslandsUSA","Northern Mariana IslandsUSA","GuamUSA" )

i=which(dcov_all$id%in%noUS)
if(length(i)>0) dcov_all=dcov_all[-i,]

i=which(dvax$id%in%noUS)
if(length(i)>0) dvax=dvax[-i,]



xx=dcov_all%>%group_by(id)%>%summarise(daysvac=max(daysvac,na.rm = T),pI=max(pI,na.rm=T),
                                       population=max(population))%>%filter(daysvac>20)




dtemp<-dcov_all %>% filter(date>=as.Date("2021-01-01"))%>%mutate(month = lubridate::floor_date(date, "month"))%>%
  group_by(id,month)%>%summarise(x=n())%>%filter(x>27)
mms=unique(dtemp$month)
dcov_month=
  dcov_all %>% filter(date>=as.Date("2021-01-01"))%>%
  group_by(id,month = lubridate::floor_date(date, "month")) %>%
  filter(month%in%mms)%>%
  summarize(mcases = sum(dcases),mdeaths=sum(ddeaths),
            population=max(population))



dcov_month=dcov_month%>%group_by(id)%>%na.omit()



dcov_month=dcov_month%>%group_by(id)%>%mutate(mcases_gr=growth_rate(mcases,1),
                                              mdeaths_gr=growth_rate(mdeaths,1),
                                              mortrate=ifelse(mcases>0,mdeaths/mcases,NA))




dcov_month=dcov_month%>%group_by(id)%>%mutate(d1M=mdeaths/population*10^5,
                                              c1M=mcases/population*10^5)


