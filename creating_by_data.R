colnames(dcov_all)


dcov_by_year=dcov_all%>%select(date,id,iso3c,country,continent,region,income,georegion,USA,EU,dcases,ddeaths,vaccination,mortrate,pI,pV)


dcov_by_year=dcov_by_year%>%mutate(daymonth=format(date,'%d/%m'),year=format(date,"%Y"))

### Ganger test cases 

dcases_df=dcov_by_year%>%select(daymonth,year,id,dcases,ddeaths,pI)%>%
  pivot_wider(names_from = year,values_from = c(dcases,ddeaths,pI),
              id_cols = c(daymonth,id))


dcases_df=dcases_df%>%mutate(daymonth=as.Date(daymonth,"%d/%m"))

lubridate::today()


dcases_df=dcases_df%>%filter(daymonth<lubridate::today())


dcases_df=dcases_df%>%filter(daymonth>=as.Date("2021-04-01"))


library (timeSeries)
library (NlinTS)

i=which(dcases_df$id=='RUScountry')
x=dcases_df$ddeaths_2020[i]
y=dcases_df$ddeaths_2021[i]

x=runner::mean_run(x,7)
y=runner::mean_run(y,7)

model = causality.test (y, x, 2, FALSE)