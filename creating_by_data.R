colnames(dcov_all)

library(dplyr)
library(broom)
library(tidyr)

dcov_by_year=dcov_all%>%select(date,id,iso3c,country,continent,region,income,georegion,USA,EU,dcases,ddeaths,vaccination,mortrate,pI,pV)


dcov_by_year=dcov_by_year%>%mutate(daymonth=format(date,'%d/%m'),year=format(date,"%Y"))

### Ganger test cases 

dcases_df=dcov_by_year%>%select(daymonth,year,id,dcases,ddeaths,pI)%>%
  pivot_wider(names_from = year,values_from = c(dcases,ddeaths,pI),
              id_cols = c(daymonth,id))


dcases_df=dcases_df%>%mutate(daymonth=as.Date(daymonth,"%d/%m"))



dcases_df=dcases_df%>%filter(daymonth<lubridate::today())


dcases_df=dcases_df%>%filter(daymonth>=as.Date("2021-04-01"))

mm<-dcov_all%>%group_by(id)%>%summarise(mm=max(deaths))%>%dplyr::filter(mm>1000)

dcases_df=dcases_df%>%dplyr::filter(id%in%mm$id)

library(timeSeries)

library(NlinTS)

i=which(dcases_df$id=='DEUcountry')
x=dcases_df$ddeaths_2020[i]
y=dcases_df$ddeaths_2021[i]

x=runner::mean_run(x,7)
y=runner::mean_run(y,7)

model = causality.test (y, x, 7, FALSE)
z=model$summary





p=length(unique(dcases_df$id))
gg_res<-vector('list',p)
for(j in 1:p){
  cat('\r',j)
  i=which(dcases_df$id==unique(dcases_df$id)[j])
  x=dcases_df$ddeaths_2020[i]
  y=dcases_df$ddeaths_2021[i]
  
  x=runner::mean_run(x,7)
  y=runner::mean_run(y,7)
  
  model = causality.test (y, x, 7, FALSE)
  gg_res[[j]]=model
}

