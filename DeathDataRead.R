library(dplyr)
library(tidyr)
library(readxl)


years=c(1980:2015)

# deaths=read_xlsx("DeathA92015.xlsx", sheet=as.character(1980), skip=3)%>%
#   gather(age, deaths, -County)%>%
#   filter(!grepl("State Total", County), !grepl("Total", age))%>%
#   mutate(year=1980)

deathlist=list()

for (i in years){
  tmp=read_xlsx("DeathA92015.xlsx", sheet=as.character(i), skip=3)%>%
    gather(age, deaths, -County)%>%
    filter(!grepl("State Total", County), !grepl("Total", age), !grepl("Source:", County), !grepl("Unk", age))%>%
    mutate(year=i,
           age=ifelse(age=="Over", "85", age))
  
  deathlist[[i]]=tmp

  # assign(paste0("deaths_",i),tmp )
  # rm(tmp)
  
}


deaths=bind_rows(deathlist)%>%
  mutate(age=recode(age, "< 1"="0-4","1-4"="0-4", "85"="85+"),
         year=as.character(year))%>%
  group_by(County, age, year)%>%
  summarise(deaths=sum(deaths))%>%
  ungroup()


population=read_excel("ofm_pop_sade_county_2010_to_2016.xlsx", sheet="Total")%>%
  filter(Year!=".", !grepl("Total", `Age Group`))%>%
  rename(age=`Age Group`, County=`Area Name`, year=Year)%>%
  mutate(age=recode(age, "5-9"="5-14","10-14"="5-14","30-34"="25-34","25-29"="25-34","35-39"="35-44",
                         "40-44"="35-44","45-49"="45-54","50-54"="45-54","55-59"="55-64","60-64"="55-64",
                         "65-69"="65-74","70-74"="65-74", "75-79"="75-84","80-84"="75-84"))%>%
  select(County, year, age, Total)%>%
  group_by(County, year, age)%>%
  summarise(Total=sum(as.numeric(Total)))%>%
  ungroup()

life_table=inner_join(deaths, population)%>%
  filter(year!="2016")%>%
  mutate(age=ordered(age, levels=c("0-4",   "5-14", "15-19", "20-24", "25-34", "35-44", "45-54",   
                                   "55-64", "65-74", "75-84" ,"85+")))%>%
  arrange(County,age)%>%
  group_by(County,age)%>%
  summarise(Total=sum(Total),
            deaths=sum(deaths))%>%
  mutate(nMx=deaths/Total)%>%
  ungroup()


