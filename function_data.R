fix_data<-function(x){
  z=c(x[1],diff(x))
  z[z<0]=0
  y=cumsum(z)
  y
}

update_vacc_data<-function(x_iso3c){
  
  u1<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
  
  d_vac=read.csv(u1)
  
  
  d_vac=d_vac%>%mutate(date=as.Date(date))%>%rename(iso3c=iso_code)
  
  
  
  d_vac=d_vac%>%filter(iso3c%in%x_iso3c)
  
  d_vac=d_vac%>%ungroup%>%select(date,iso3c,total_vaccinations)%>%na.omit()
  
  
  d_vac=d_vac%>%filter(total_vaccinations>0)
  

  
  
  library(tidyr)
  d_vac=d_vac%>%complete(date,nesting(iso3c),fill=list(total_vaccinations=0))
  
  d_vac=d_vac%>%group_by(iso3c)%>%mutate(total_vaccinations=cummax(total_vaccinations))
  d_vac=d_vac%>%filter(total_vaccinations>0)
  d_vac=d_vac%>%rename(vaccination=total_vaccinations)

  i=which(d_vac$iso3c=="")
  if(length(i)>0) d_vac=d_vac[-i,]
  
  i=which(nchar(as.character(d_vac$iso3c))>3)
  if(length(i)>0) d_vac=d_vac[-i,]
  
  
  d_vac
}


update_vacc_data_V2<-function(x_iso3c){
  
  u1<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
  
  d_vac=read.csv(u1)
  
  d_vac=d_vac%>%mutate(date=as.Date(date))%>%rename(iso3c=iso_code)
  
  d_vac=d_vac%>%filter(iso3c%in%x_iso3c)
  
  d_vac=d_vac%>%ungroup%>%select(date,iso3c,total_vaccinations,people_vaccinated,people_fully_vaccinated)
  
  
  
  d_vac=d_vac%>%mutate(total_vaccinations=ifelse(is.na(total_vaccinations)==T,0,total_vaccinations),
         people_vaccinated=ifelse(is.na(people_vaccinated)==T,0,people_vaccinated),
         people_fully_vaccinated=ifelse(is.na(people_fully_vaccinated)==T,0,people_fully_vaccinated))
  
  d_vac=d_vac%>%group_by(iso3c)%>%mutate(nodeclardata=cumsum(total_vaccinations == 0))
  
  d_vac=d_vac%>%group_by(iso3c)%>%mutate(total_vaccinations=cummax(total_vaccinations),
                                         people_vaccinated=cummax(people_vaccinated),
                                         people_fully_vaccinated=cummax(people_fully_vaccinated))
  
  d_vac=d_vac%>%filter(total_vaccinations>0)
  d_vac=d_vac%>%rename(vaccination=total_vaccinations)
  
  i=which(d_vac$iso3c=="")
  if(length(i)>0) d_vac=d_vac[-i,]
  
  i=which(nchar(as.character(d_vac$iso3c))>3)
  if(length(i)>0) d_vac=d_vac[-i,]
  
  
  d_vac
}

update_covid_data<-function(){
  
  dcov_conf=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  dcov_deaths=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  dcov_recovered=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  
  
  dcov_conf=dcov_conf%>%dplyr::select(-c(Province.State,Lat,Long))%>%group_by(Country.Region)%>%summarise_all(funs(sum))
  
  dcov_conf=reshape2::melt(dcov_conf,id.vars=1)  
  
  dcov_conf=dcov_conf%>%mutate(iso3c=countrycode::countrycode(Country.Region,origin = "country.name",destination = "iso3c"))%>%na.omit()
  
  
  dcov_conf=dcov_conf%>%mutate(date=gsub(pattern = "X",replacement = "",variable))%>%mutate(date=as.Date(date,format = "%m.%d.%y"))%>%dplyr::select(-variable)
  
  dcov_conf=dcov_conf%>%rename(confirmed=value)
  
  
  dcov_deaths=dcov_deaths%>%dplyr::select(-c(Province.State,Lat,Long))%>%group_by(Country.Region)%>%summarise_all(funs(sum))
  
  dcov_deaths=reshape2::melt(dcov_deaths,id.vars=1)  
  
  dcov_deaths=dcov_deaths%>%mutate(iso3c=countrycode::countrycode(Country.Region,origin = "country.name",destination = "iso3c"))%>%na.omit()
  
  
  dcov_deaths=dcov_deaths%>%mutate(date=gsub(pattern = "X",replacement = "",variable))%>%mutate(date=as.Date(date,format = "%m.%d.%y"))%>%dplyr::select(-variable)
  
  dcov_deaths=dcov_deaths%>%rename(deaths=value)
  
  
  
  dcov_recovered=dcov_recovered%>%dplyr::select(-c(Province.State,Lat,Long))%>%group_by(Country.Region)%>%summarise_all(funs(sum))
  
  dcov_recovered=reshape2::melt(dcov_recovered,id.vars=1)  
  
  dcov_recovered=dcov_recovered%>%mutate(iso3c=countrycode::countrycode(Country.Region,origin = "country.name",destination = "iso3c"))%>%na.omit()
  
  
  dcov_recovered=dcov_recovered%>%mutate(date=gsub(pattern = "X",replacement = "",variable))%>%mutate(date=as.Date(date,format = "%m.%d.%y"))%>%dplyr::select(-variable)
  
  dcov_recovered=dcov_recovered%>%rename(recovered=value)
  
  
  dcov_conf=dcov_conf%>%dplyr::select(date,iso3c,confirmed)
  dcov_deaths=dcov_deaths%>%dplyr::select(date,iso3c,deaths)
  dcov_recovered=dcov_recovered%>%dplyr::select(date,iso3c,recovered)
  
  dcov_conf=dcov_conf%>%group_by(iso3c)%>%arrange(date, .by_group = TRUE)
  dcov_deaths=dcov_deaths%>%group_by(iso3c)%>%arrange(date, .by_group = TRUE)
  dcov_recovered=dcov_recovered%>%group_by(iso3c)%>%arrange(date, .by_group = TRUE)
  
  dcov1=dcov_conf%>%left_join(dcov_deaths,by=c("date","iso3c"))
  
  dcov1=dcov1%>%left_join(dcov_recovered,by=c("date","iso3c"))
  
  dcov1=dcov1%>%filter(confirmed>0)
  dcov1=dcov1%>%group_by(iso3c)%>%mutate(confirmed=fix_data(confirmed),
                                         deaths=fix_data(deaths),
                                         recovered=fix_data(recovered))
  i=which(nchar(dcov1$iso3c)>3)
  if(length(i)>0) dcov1=dcov1[-i,]
  dcov1
}


update_covid_US_data<-function(){
  #d_US=COVID19::covid19(country = "USA",level = 2,cache = F,verbose = F)
  d_US=readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  d_US=d_US%>%select(-fips)%>%mutate(recovered=0)%>%rename(iso3c=state,confirmed=cases)
  
  d_US=d_US%>%ungroup()%>%dplyr::select(date,iso3c,confirmed,recovered,deaths)
  
  d_US=d_US%>%group_by(iso3c)%>%mutate(recovered=ifelse(is.na(recovered)==T,0,recovered))
  d_US=d_US%>%filter(confirmed>0)
  
  d_US=d_US%>%group_by(iso3c)%>%mutate(confirmed=cummax(confirmed),
                                         deaths=cummax(deaths),
                                         recovered=cummax(recovered))
  d_US
}


update_vacc_US_data<-function(x_iso3c){
  
  
  u1<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
  d_vacUS=read.csv(u1)
  
  d_vacUS=d_vacUS%>%select(date,location,total_vaccinations)%>%rename(iso3c=location,vaccination=total_vaccinations)
  
  d_vacUS=d_vacUS%>%group_by(iso3c)%>%mutate(vaccination=ifelse(is.na(vaccination)==T,0,vaccination))%>%
    mutate(vaccination=cummax(vaccination))
  
  library(tidyr)
  d_vacUS=d_vacUS%>%complete(date,nesting(iso3c),fill=list(vaccination=0))
  
  d_vacUS$iso3c=gsub('New York State','New York',d_vacUS$iso3c)
  
  
  
  d_vacUS=d_vacUS%>%filter(iso3c%in%x_iso3c)
  d_vacUS$date=as.Date(d_vacUS$date)
  d_vacUS=d_vacUS%>%group_by(iso3c)%>%mutate(vaccination=cummax(vaccination))
  d_vacUS
}


update_vacc_US_data_V2<-function(x_iso3c){
  u1<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
  
  # https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv
  d_vacUS=read.csv(u1)
  
  d_vacUS=d_vacUS%>%mutate(date=as.Date(date))%>%rename(iso3c=location)
  
  
  d_vacUS$iso3c=gsub('New York State','New York',d_vacUS$iso3c)
  
  
  #
  
  d_vacUS=d_vacUS%>%filter(iso3c%in%x_iso3c)
  
  d_vacUS=d_vacUS%>%ungroup%>%select(date,iso3c,total_vaccinations,people_vaccinated,people_fully_vaccinated)
  
  d_vacUS=d_vacUS%>%mutate(total_vaccinations=ifelse(is.na(total_vaccinations)==T,0,total_vaccinations),
                           people_vaccinated=ifelse(is.na(people_vaccinated)==T,0,people_vaccinated),
                           people_fully_vaccinated=ifelse(is.na(people_fully_vaccinated)==T,0,people_fully_vaccinated))
  
  
  
  d_vacUS=d_vacUS%>%group_by(iso3c)%>%mutate(nodeclardata=cumsum(total_vaccinations == 0))
  
  
  d_vacUS=d_vacUS%>%group_by(iso3c)%>%mutate(total_vaccinations=cummax(total_vaccinations),
                                             people_vaccinated=cummax(people_vaccinated),
                                             people_fully_vaccinated=cummax(people_fully_vaccinated))
  
  d_vacUS=d_vacUS%>%filter(total_vaccinations>0)
  d_vacUS=d_vacUS%>%rename(vaccination=total_vaccinations)
  
  
  d_vacUS
}


mysmooth<-function(y,d){
  z=supsmu(x = d,y= y)
  z$y
}

