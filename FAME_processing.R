# library -----------------------------------------------------------------

library(sp)
library(sf)
library(tmap)
library(tmaptools)
library(rgeos)
library(spatialEco)
library(ggplot2)
library(scales)
library(ggthemes)
library(data.table)
library(stringr)
library(dplyr)
library(plyr)
library(tidyr)
library(tidyverse)
library(Hmisc)
library(diverse)
library(REAT)
library(reshape2)
library(rmapshaper)
library(magrittr)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(fmsb)
library(diverse)
library(pheatmap)
library(igraph)

options(scipen = 20)
options(digits = 3)

# 0. import data -----------------------------------------------------------

# use FREAD function to read huge file -- FREAD can only read UTF-8
# we have to read them progressively
FAME_test = fread("FAME_complete_UTF8.tsv", header = TRUE, sep="\t",encoding="UTF-8",nrow = 100)%>%
  data.frame()

FAME_complete1 = fread("FAME_complete_UTF8.tsv", header = TRUE, sep="\t",encoding="UTF-8",nrow = 1000000)%>%
  data.frame()%>%
  select(-V1,-R.O.Address,-R.O.City,-R.O.Latitude,-R.O.Longitude,
         -Primary.trading.address,-Primary.trading.address.City,-Primary.trading.address.Latitude,-Primary.trading.address.Longitude, 
         -starts_with("Trading."),-ends_with(".2019"),-ends_with(".2020"),-Status.date,
         -Primary.NACE.Rev..2.code,-Number.of.trading.addresses,-Branch.name,-Date.of.incorporation,
         -GUO...Name,-GUO...BvD.ID.number,-GUO...NACE..br.Core.code,-GUO...Information.date,
         -GUO...Operating.revenue..Turnover..m.GBP,-GUO...Number.of.employees)

FAME_complete2 = fread("FAME_complete_UTF8.tsv", header = FALSE, sep="\t",encoding="UTF-8",nrow = 1000000,skip = 5000000)%>%
  data.frame()%>%
  set_colnames(colnames(FAME_test))%>%
  select(-V1,-R.O.Address,-R.O.City,-R.O.Latitude,-R.O.Longitude,
         -Primary.trading.address,-Primary.trading.address.City,-Primary.trading.address.Latitude,-Primary.trading.address.Longitude, 
         -starts_with("Trading."),-ends_with(".2019"),-ends_with(".2020"),-Status.date,
         -Primary.NACE.Rev..2.code,-Number.of.trading.addresses,-Branch.name,-Date.of.incorporation,
         -GUO...Name,-GUO...BvD.ID.number,-GUO...NACE..br.Core.code,-GUO...Information.date,
         -GUO...Operating.revenue..Turnover..m.GBP,-GUO...Number.of.employees)

FAME_complete3 = fread("FAME_complete_UTF8.tsv", header = FALSE, sep="\t",encoding="UTF-8",nrow = 1000000,skip = 2000000)%>%
  data.frame()%>%
  set_colnames(colnames(FAME_test))%>%
  select(-V1,-R.O.Address,-R.O.City,-R.O.Latitude,-R.O.Longitude,
         -Primary.trading.address,-Primary.trading.address.City,-Primary.trading.address.Latitude,-Primary.trading.address.Longitude, 
         -starts_with("Trading."),-ends_with(".2019"),-ends_with(".2020"),-Status.date,
         -Primary.NACE.Rev..2.code,-Number.of.trading.addresses,-Branch.name,-Date.of.incorporation,
         -GUO...Name,-GUO...BvD.ID.number,-GUO...NACE..br.Core.code,-GUO...Information.date,
         -GUO...Operating.revenue..Turnover..m.GBP,-GUO...Number.of.employees)

FAME_complete4 = fread("FAME_complete_UTF8.tsv", header = FALSE, sep="\t",encoding="UTF-8",nrow = 1000000,skip = 3000000)%>%
  data.frame()%>%
  set_colnames(colnames(FAME_test))%>%
  select(-V1,-R.O.Address,-R.O.City,-R.O.Latitude,-R.O.Longitude,
         -Primary.trading.address,-Primary.trading.address.City,-Primary.trading.address.Latitude,-Primary.trading.address.Longitude, 
         -starts_with("Trading."),-ends_with(".2019"),-ends_with(".2020"),-Status.date,
         -Primary.NACE.Rev..2.code,-Number.of.trading.addresses,-Branch.name,-Date.of.incorporation,
         -GUO...Name,-GUO...BvD.ID.number,-GUO...NACE..br.Core.code,-GUO...Information.date,
         -GUO...Operating.revenue..Turnover..m.GBP,-GUO...Number.of.employees)

FAME_complete5 = fread("FAME_complete_UTF8.tsv", header = FALSE, sep="\t",encoding="UTF-8",nrow = 1000000,skip = 4000000)%>%
  data.frame()%>%
  set_colnames(colnames(FAME_test))%>%
  select(-V1,-R.O.Address,-R.O.City,-R.O.Latitude,-R.O.Longitude,
         -Primary.trading.address,-Primary.trading.address.City,-Primary.trading.address.Latitude,-Primary.trading.address.Longitude, 
         -starts_with("Trading."),-ends_with(".2019"),-ends_with(".2020"),-Status.date,
         -Primary.NACE.Rev..2.code,-Number.of.trading.addresses,-Branch.name,-Date.of.incorporation,
         -GUO...Name,-GUO...BvD.ID.number,-GUO...NACE..br.Core.code,-GUO...Information.date,
         -GUO...Operating.revenue..Turnover..m.GBP,-GUO...Number.of.employees)

FAME_complete6 = fread("FAME_complete_UTF8.tsv", header = FALSE, sep="\t",encoding="UTF-8",nrow = 1000000,skip = 5000000)%>%
  data.frame()%>%
  set_colnames(colnames(FAME_test))%>%
  select(-V1,-R.O.Address,-R.O.City,-R.O.Latitude,-R.O.Longitude,
         -Primary.trading.address,-Primary.trading.address.City,-Primary.trading.address.Latitude,-Primary.trading.address.Longitude, 
         -starts_with("Trading."),-ends_with(".2019"),-ends_with(".2020"),-Status.date,
         -Primary.NACE.Rev..2.code,-Number.of.trading.addresses,-Branch.name,-Date.of.incorporation,
         -GUO...Name,-GUO...BvD.ID.number,-GUO...NACE..br.Core.code,-GUO...Information.date,
         -GUO...Operating.revenue..Turnover..m.GBP,-GUO...Number.of.employees)

FAME_complete7 = fread("FAME_complete_UTF8.tsv", header = FALSE, sep="\t",encoding="UTF-8",nrow = 1000000,skip = 6000000)%>%
  data.frame()%>%
  set_colnames(colnames(FAME_test))%>%
  select(-V1,-R.O.Address,-R.O.City,-R.O.Latitude,-R.O.Longitude,
         -Primary.trading.address,-Primary.trading.address.City,-Primary.trading.address.Latitude,-Primary.trading.address.Longitude, 
         -starts_with("Trading."),-ends_with(".2019"),-ends_with(".2020"),-Status.date,
         -Primary.NACE.Rev..2.code,-Number.of.trading.addresses,-Branch.name,-Date.of.incorporation,
         -GUO...Name,-GUO...BvD.ID.number,-GUO...NACE..br.Core.code,-GUO...Information.date,
         -GUO...Operating.revenue..Turnover..m.GBP,-GUO...Number.of.employees)

FAME_complete = rbind(FAME_complete1,FAME_complete2,FAME_complete3,FAME_complete4,FAME_complete5,FAME_complete6,FAME_complete7)
remove(FAME_complete1,FAME_complete2,FAME_complete3,FAME_complete4,FAME_complete5,FAME_complete6,FAME_complete7)

# unify the NA and missing values
# and drop the items that have no geo and SIC imformation
FAME_complete[FAME_complete == "" | FAME_complete == "n.a."] = NA
FAME_complete = FAME_complete %>% drop_na(R.O.Full.Postcode, Primary.UK.SIC..2007..code)
# discard "non-active" and "non-england" 
FAME_complete = FAME_complete %>%
  filter(R.O.Country == "England") %>%
  filter(Company.status == "Active")

# 0.1 data processing -----------------------------------------------------------

# import SIC code data
siccode = read.csv("https://pkgstore.datahub.io/core/uk-sic-2007-condensed/uk-sic-2007-condensed_csv/data/e384a7d5f7872871ea9b87179d12c117/uk-sic-2007-condensed_csv.csv")

# deal with some missing characters
siccode$section_description = as.character(siccode$section_description)
siccode[siccode =="Wholesale and retail trade; repair of motor vehicles and"] = "Wholesale and retail trade; repair of motor vehicles and motorcycles"

# match with industry
siccode $ Industry = 1
siccode[which(siccode$section %in% c("A",'B','D','E')),] $ Industry = "Agriculture, energy and water"
siccode[which(siccode$section %in% c('C')),] $ Industry = "Manufacturing"
siccode[which(siccode$section %in% c('F')),] $ Industry = "Construction"
siccode[which(siccode$section %in% c("G",'I')),] $ Industry = "Distribution, hotels and restaurants"
siccode[which(siccode$section %in% c("H",'J')),] $ Industry = "Transport and communication"
siccode[which(siccode$section %in% c('K', 'L', 'M', 'N')),] $ Industry = "Financial, Real Estate, Professional and Administrative activities"
siccode[which(siccode$section %in% c('O', 'P', 'Q')),] $ Industry = "Public administration, education and health"
siccode[which(siccode$section %in% c('R', 'S', 'T', 'U')),] $ Industry = "Other"


FAME_complete = merge(FAME_complete,
                      siccode %>%select(sic_code,section,section_description,Industry),
                      by.x = "Primary.UK.SIC..2007..code", by.y="sic_code")


# match with the postcode
postcode = fread("Postcodes___Local_Authorities_only_v01.csv", header = TRUE, sep=",",encoding="UTF-8")%>%
  data.frame()%>%
  select(Postcode.3,Local.Authority.Code)

local_region = fread("Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv", header = TRUE, sep=",",encoding="UTF-8")%>%
  as.data.frame()%>%
  select(-FID)

FAME_complete = merge(FAME_complete,
                      postcode,
                      by.x = "R.O.Full.Postcode", by.y="Postcode.3")
  
FAME_complete = merge(FAME_complete,
                      local_region,
                      by.x = "Local.Authority.Code", by.y="LAD19CD")
# add 3 zero -- detected!
FAME_complete[6:23]= FAME_complete[6:23]*1000
# 0.2 branch --------------------------------------------------------------
FAME_branches = fread("FAME_branches copy.tsv", header = TRUE, sep="\t",encoding="UTF-8",nrows = 100)%>%
  data.frame()

FAME_branches %>% View()

FAME_branches[FAME_branches == "" | FAME_branches == "n.a."| FAME_branches == "NA"] = NA
FAME_branches = FAME_branches %>% 
  filter(country == "England") %>% 
  select(id,branch_name,postcode)

glimpse(FAME_branches)
FAME_branches = merge(FAME_branches,
                      postcode,
                      by.x = "postcode", by.y="Postcode.3")

FAME_branches = merge(FAME_branches,
                      local_region,
                      by.x = "Local.Authority.Code", by.y="LAD19CD")

colnames(FAME_branches) = c("Local.Authority.Code","R.O.Full.Postcode",
                            "BvD.ID.number","Company.name",
                            "LAD19NM","RGN19CD","RGN19NM")

FAME_branches = merge(FAME_branches,
      FAME_complete %>% select(BvD.ID.number,Industry),
      by = "BvD.ID.number")

FAME_complete = bind_rows(FAME_complete,FAME_branches)
glimpse(FAME_complete)

# branches ⬇
FAME_complete %>%
  filter(is.na(Primary.UK.SIC..2007..code))

#
FAME_complete %>%
  filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2018$BvD.ID.number)

# 0.3 firm number -------------------------------------------------------------
# 2018
fame2018 = drop_na(FAME_complete,Turnover.th.GBP.2018)
fame2018 = bind_rows(fame2018,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2018$BvD.ID.number)) %>%
    select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2018$year = 2018

# 2017
fame2017 = drop_na(FAME_complete,Turnover.th.GBP.2017)
fame2017 = bind_rows(fame2017,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2017$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2017$year = 2017

# 2016
fame2016 = drop_na(FAME_complete,Turnover.th.GBP.2016)
fame2016 = bind_rows(fame2016,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2016$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2016$year = 2016

# 2015
fame2015 = drop_na(FAME_complete,Turnover.th.GBP.2015)
fame2015 = bind_rows(fame2015,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2015$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2015$year = 2015

# 2014
fame2014 = drop_na(FAME_complete,Turnover.th.GBP.2014)
fame2014 = bind_rows(fame2014,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2014$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2014$year = 2014

# 2013
fame2013 = drop_na(FAME_complete,Turnover.th.GBP.2013)
fame2013 = bind_rows(fame2013,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2013$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2013$year = 2013

# 2012
fame2012 = drop_na(FAME_complete,Turnover.th.GBP.2012)
fame2012 = bind_rows(fame2012,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2012$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2012$year = 2012

# 2011
fame2011 = drop_na(FAME_complete,Turnover.th.GBP.2011)
fame2011 = bind_rows(fame2011,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2011$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2011$year = 2011

# 2010
fame2010 = drop_na(FAME_complete,Turnover.th.GBP.2010)
fame2010 = bind_rows(fame2010,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2010$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2010$year = 2010

# 2009
fame2009 = drop_na(FAME_complete,Turnover.th.GBP.2009)
fame2009 = bind_rows(fame2009,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2009$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2009$year = 2009

# 2008
fame2008 = drop_na(FAME_complete,Turnover.th.GBP.2008)
fame2008 = bind_rows(fame2008,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2008$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2008$year = 2008

# 2007
fame2007 = drop_na(FAME_complete,Turnover.th.GBP.2007)
fame2007 = bind_rows(fame2007,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2007$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2007$year = 2007

# 2006
fame2006 = drop_na(FAME_complete,Turnover.th.GBP.2006)
fame2006 = bind_rows(fame2006,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2006$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2006$year = 2006

# 2005
fame2005 = drop_na(FAME_complete,Turnover.th.GBP.2005)
fame2005 = bind_rows(fame2005,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2005$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2005$year = 2005

# 2004
fame2004 = drop_na(FAME_complete,Turnover.th.GBP.2004)
fame2004 = bind_rows(fame2004,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2004$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2004$year = 2004

# 2003
fame2003 = drop_na(FAME_complete,Turnover.th.GBP.2003)
fame2003 = bind_rows(fame2003,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2003$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2003$year = 2003

# 2002
fame2002 = drop_na(FAME_complete,Turnover.th.GBP.2002)
fame2002 = bind_rows(fame2002,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2002$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2002$year = 2002

# 2001
fame2001 = drop_na(FAME_complete,Turnover.th.GBP.2001)
fame2001 = bind_rows(fame2001,FAME_complete %>%
                       filter(is.na(Primary.UK.SIC..2007..code) ,BvD.ID.number %in% fame2001$BvD.ID.number)) %>%
  select(LAD19NM,RGN19NM,Industry)%>%group_by(LAD19NM,RGN19NM,Industry)%>%count()
fame2001$year = 2001

fame = bind_rows(fame2001,fame2002,fame2003,fame2004,fame2005,fame2006,fame2007,fame2008,fame2009,
          fame2010,fame2011,fame2012,fame2013,fame2014,fame2015,fame2016,fame2017,fame2018)



# (X)0.4 census --------------------------------------------------------------
census2001 = fread("2001.csv", header = TRUE, sep=",",encoding="UTF-8")%>%
  data.frame() %>%
  set_colnames(c("LAD19NM","Local.Authority.Code","all",
                 "Agriculture.energy.and.water","Manufacturing","Construction","Distribution.hotels.and.restaurants",
                 "Transport.and.communication","Financial.Real.Estate.Professional.and.Administrative.activities",
                 "Public.administration.education.and.health","Other"))

census2011 = fread("2011.csv", header = TRUE, sep=",",encoding="UTF-8")%>%
  data.frame() %>%
  set_colnames(c("LAD19NM","Local.Authority.Code","all",
                 "Agriculture.energy.and.water","Manufacturing","Construction","Distribution.hotels.and.restaurants",
                 "Transport.and.communication","Financial.Real.Estate.Professional.and.Administrative.activities",
                 "Public.administration.education.and.health","Other"))

# 0.5 top firms profile -------------------------------------------------------


top_500 = FAME_complete %>%
  arrange(desc(Turnover.th.GBP.2018)) %>% head(500)

top_200 = FAME_complete %>%
  arrange(desc(Turnover.th.GBP.2018)) %>% head(200)

top_100 = FAME_complete %>%
  arrange(desc(Turnover.th.GBP.2018)) %>% head(100) 

# 0.6 how many missing values -------------------------------------------------
missingdata = FAME_complete %>% 
  drop_na(section)%>%
  summarise_all(~ sum(is.na(.)))
missingdata[2,] = 1751787

missingdata %>% glimpse()
missingdata %>% View()

# valid items
missingdata[3,] = missingdata[2,] - missingdata[1,]

# valid items / total
missingdata[4,23:6] = missingdata[1,23:6] / missingdata[2,23:6]
missingdata$name = c("Firms without valid turnover","Total firms","Firms with valid turnover","propotion")

ggplot(missingdata[c(1,3),c(51,23:6)]%>%
         set_colnames(c("name",seq(2001,2018,1)))%>%
         melt(id.vars="name",
              variable.name="year",
              value.name="turnover"), 
       aes(fill=name, y=turnover, x=year)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("% of total firms")+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave(
  "0. missing firm(turnover).png",
  width = 10,
  height = 4,
  dpi = 1200
)


# employment
ggplot(missingdata[c(1,3),c(51,41:24)]%>%
         set_colnames(c("name",seq(2001,2018,1)))%>%
         melt(id.vars="name",
              variable.name="year",
              value.name="turnover"), 
       aes(fill=name, y=turnover, x=year)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("% of total firms")+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

ggsave(
  "0. missing firm(employment).png",
  width = 10,
  height = 4,
  dpi = 1200
)

# 0.7 when dropping the top firms ---------------------------------------------
par(mfrow = c(2, 2))
fnx = function(v){v/1e+12}
FAME_complete %>%
  filter(Company.name %in% top_100$Company.name) %>%
  select(starts_with("Turnover")) %>% 
  drop_na() %>% rev()%>%
  lapply(sum,na.rm = TRUE) %>%
  lapply(fnx) %>%
  as.data.frame() %>% t() %>% 
  plot(x=2001:2018, type = "b",col="blue",pch=15,
       xlab = "",
       ylab="Total turnover (£ billion)",
       main="Turnover of top 100 firms (dropping missing values in top 100 firms)")

FAME_complete %>%
  filter(Company.name %in% top_200$Company.name) %>%
  select(starts_with("Turnover")) %>% 
  drop_na() %>% rev()%>%
  lapply(sum,na.rm = TRUE) %>%
  lapply(fnx) %>%
  as.data.frame() %>% t() %>% 
  plot(x=2001:2018, type = "b",col="red",pch=15,
       xlab = "",
       ylab="Total turnover (£ billion)",
       main="Turnover of top 200 firms (dropping missing values in top 200 firms)")

FAME_complete %>%
  filter(Company.name %in% top_500$Company.name) %>%
  select(starts_with("Turnover")) %>% 
  drop_na() %>% rev()%>%
  lapply(sum,na.rm = TRUE) %>%
  lapply(fnx) %>%
  as.data.frame() %>% t() %>% 
  plot(x=2001:2018, type = "b",col="green",pch=15,
       xlab = "",
       ylab="Total turnover (£ billion)",
       main="Turnover of top 500 firms (dropping missing values in top 500 firms)")

FAME_complete %>%
  select(starts_with("Turnover")) %>% 
  drop_na() %>% rev()%>%
  lapply(sum,na.rm = TRUE) %>%
  lapply(fnx) %>%
  as.data.frame() %>% t() %>% 
  plot(x=2001:2018, type = "b",col="black",pch=15,
       xlab = "",
       ylab="Total turnover (£ billion)",
       main="Total turnover (dropping all missing values)")  

# top_100 ----------------------------------------------------------------

top_100firms = cbind( 
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2001)) %>% 
    head(100) %>% summarise(turnover2001 = sum(Turnover.th.GBP.2001)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2002)) %>% 
    head(100) %>% summarise(turnover2002 = sum(Turnover.th.GBP.2002)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2003)) %>% 
    head(100) %>% summarise(turnover2003 = sum(Turnover.th.GBP.2003)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2004)) %>% 
    head(100) %>% summarise(turnover2004 = sum(Turnover.th.GBP.2004)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2005)) %>% 
    head(100) %>% summarise(turnover2005 = sum(Turnover.th.GBP.2005)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2006)) %>% 
    head(100) %>% summarise(turnover2006 = sum(Turnover.th.GBP.2006)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2007)) %>% 
    head(100) %>% summarise(turnover2007 = sum(Turnover.th.GBP.2007)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2008)) %>% 
    head(100) %>% summarise(turnover2008 = sum(Turnover.th.GBP.2008)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2009)) %>% 
    head(100) %>% summarise(turnover2009 = sum(Turnover.th.GBP.2009)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2010)) %>% 
    head(100) %>% summarise(turnover2010 = sum(Turnover.th.GBP.2010)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2011)) %>% 
    head(100) %>% summarise(turnover2011 = sum(Turnover.th.GBP.2011)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2012)) %>% 
    head(100) %>% summarise(turnover2012 = sum(Turnover.th.GBP.2012)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2013)) %>% 
    head(100) %>% summarise(turnover2013 = sum(Turnover.th.GBP.2013)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2014)) %>% 
    head(100) %>% summarise(turnover2014 = sum(Turnover.th.GBP.2014)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2015)) %>% 
    head(100) %>% summarise(turnover2015 = sum(Turnover.th.GBP.2015)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2016)) %>% 
    head(100) %>% summarise(turnover2016 = sum(Turnover.th.GBP.2016)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2017)) %>% 
    head(100) %>% summarise(turnover2017 = sum(Turnover.th.GBP.2017)),
  
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2018)) %>% 
    head(100) %>% summarise(turnover2018 = sum(Turnover.th.GBP.2018))
)
top_100firms $ name = NULL
top_100p = top_100firms/turnover_year

# top 200 -----------------------------------------------------------------

top_200firms = cbind( 
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2001)) %>% 
    head(200) %>% summarise(turnover2001 = sum(Turnover.th.GBP.2001)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2002)) %>% 
    head(200) %>% summarise(turnover2002 = sum(Turnover.th.GBP.2002)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2003)) %>% 
    head(200) %>% summarise(turnover2003 = sum(Turnover.th.GBP.2003)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2004)) %>% 
    head(200) %>% summarise(turnover2004 = sum(Turnover.th.GBP.2004)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2005)) %>% 
    head(200) %>% summarise(turnover2005 = sum(Turnover.th.GBP.2005)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2006)) %>% 
    head(200) %>% summarise(turnover2006 = sum(Turnover.th.GBP.2006)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2007)) %>% 
    head(200) %>% summarise(turnover2007 = sum(Turnover.th.GBP.2007)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2008)) %>% 
    head(200) %>% summarise(turnover2008 = sum(Turnover.th.GBP.2008)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2009)) %>% 
    head(200) %>% summarise(turnover2009 = sum(Turnover.th.GBP.2009)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2010)) %>% 
    head(200) %>% summarise(turnover2010 = sum(Turnover.th.GBP.2010)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2011)) %>% 
    head(200) %>% summarise(turnover2011 = sum(Turnover.th.GBP.2011)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2012)) %>% 
    head(200) %>% summarise(turnover2012 = sum(Turnover.th.GBP.2012)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2013)) %>% 
    head(200) %>% summarise(turnover2013 = sum(Turnover.th.GBP.2013)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2014)) %>% 
    head(200) %>% summarise(turnover2014 = sum(Turnover.th.GBP.2014)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2015)) %>% 
    head(200) %>% summarise(turnover2015 = sum(Turnover.th.GBP.2015)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2016)) %>% 
    head(200) %>% summarise(turnover2016 = sum(Turnover.th.GBP.2016)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2017)) %>% 
    head(200) %>% summarise(turnover2017 = sum(Turnover.th.GBP.2017)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2018)) %>% 
    head(200) %>% summarise(turnover2018 = sum(Turnover.th.GBP.2018))
)
top_200p = top_200firms/turnover_year

# top_500 ----------------------------------------------------------------

top_500firms = cbind( 
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2001)) %>% 
    head(500) %>% summarise(turnover2001 = sum(Turnover.th.GBP.2001)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2002)) %>% 
    head(500) %>% summarise(turnover2002 = sum(Turnover.th.GBP.2002)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2003)) %>% 
    head(500) %>% summarise(turnover2003 = sum(Turnover.th.GBP.2003)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2004)) %>% 
    head(500) %>% summarise(turnover2004 = sum(Turnover.th.GBP.2004)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2005)) %>% 
    head(500) %>% summarise(turnover2005 = sum(Turnover.th.GBP.2005)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2006)) %>% 
    head(500) %>% summarise(turnover2006 = sum(Turnover.th.GBP.2006)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2007)) %>% 
    head(500) %>% summarise(turnover2007 = sum(Turnover.th.GBP.2007)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2008)) %>% 
    head(500) %>% summarise(turnover2008 = sum(Turnover.th.GBP.2008)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2009)) %>% 
    head(500) %>% summarise(turnover2009 = sum(Turnover.th.GBP.2009)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2010)) %>% 
    head(500) %>% summarise(turnover2010 = sum(Turnover.th.GBP.2010)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2011)) %>% 
    head(500) %>% summarise(turnover2011 = sum(Turnover.th.GBP.2011)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2012)) %>% 
    head(500) %>% summarise(turnover2012 = sum(Turnover.th.GBP.2012)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2013)) %>% 
    head(500) %>% summarise(turnover2013 = sum(Turnover.th.GBP.2013)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2014)) %>% 
    head(500) %>% summarise(turnover2014 = sum(Turnover.th.GBP.2014)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2015)) %>% 
    head(500) %>% summarise(turnover2015 = sum(Turnover.th.GBP.2015)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2016)) %>% 
    head(500) %>% summarise(turnover2016 = sum(Turnover.th.GBP.2016)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2017)) %>% 
    head(500) %>% summarise(turnover2017 = sum(Turnover.th.GBP.2017)),
  FAME_complete %>% arrange(desc(Turnover.th.GBP.2018)) %>% 
    head(500) %>% summarise(turnover2018 = sum(Turnover.th.GBP.2018))
)
top_500p = top_500firms/turnover_year

top = data.frame(name=c("top_100_value","Share of top 100 firms",
                        "top_200_value","Share of top 200 firms",
                        "top_500_value","Share of top 500 firms","turnover"))
top = cbind(top,rbind(top_100firms,top_100p,
                      top_200firms,top_200p,
                      top_500firms,top_500p,turnover_year))

top[c(2,4,6),] %>%
  set_colnames(c("name",seq(2001,2018,1)))%>%
  melt(id.vars="name",
       variable.name="year",
       value.name="proportion")%>%
  ggplot(aes(x = year,
             y = proportion,
             group = name,
             color= name,
             label = round(proportion,3)))+
  geom_line(size = 0.5)+
  geom_point(size=2)+
  #scale_colour_brewer(palette="Spectral")+
  geom_text(nudge_y = -0.015)+
  xlab("")+
  ylab("Share of total annual turnover of top firms")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggsave(
  "0. top firms proportion.png",
  width = 10,
  height = 5,
  dpi = 1200
)  

top_500 %>%
  filter(Industry == "Financial, Real Estate, Professional and Administrative activities")%>%
  arrange(desc(Turnover.th.GBP.2012)) %>%
  dplyr::select(Company.name,Industry,Turnover.th.GBP.2012,Turnover.th.GBP.2011,
                Turnover.th.GBP.2010,Turnover.th.GBP.2009)%>% head(20) %>% View()



# 1. basic analysis -----------------------------------------------------
# an example of headquarter and branches
FAME_complete %>%
  filter(Company.name == "BP P.L.C.") %>% 
  select(Company.name,Turnover.th.GBP.2018,RGN19NM) %>%
  group_by(Company.name,RGN19NM) %>%
  count() %>%
  View()
# save some space
FAME_complete %>% glimpse()
FAME_complete $ Registered.number = NULL
FAME_complete $ R.O.Country = NULL
FAME_complete $ Primary.trading.address.Full.Postcode = NULL
FAME_complete $ Primary.trading.address.Country = NULL
FAME_complete $ Company.status = NULL
FAME_complete $ RGN19CD = NULL
FAME_complete $ R.O.Country = NULL
remove(postcode)
remove(FAME_branches)


# 1.1 Number of Firms by Region -----------------------------------------------
FAME_complete %>%
  select(RGN19NM)%>%
  group_by(RGN19NM)%>%
  count()%>%
  ggplot(aes(x=reorder(RGN19NM,freq),y=freq,group=1,label=freq))+
  coord_flip()+
  geom_bar(stat="identity",fill ="#add4e6",show.legend = FALSE)+
  geom_text(nudge_y = -20000)+
  xlab("")+
  ylab("Number of firms")+
  theme_bw()
sum(is.na(FAME_complete$Number.of.employees.2012))

ggsave(
  "0. Number of Firms by Region.png",
  width = 10,
  height = 4,
  dpi = 1200
)
# 1.1 Number of Firms by sector -----------------------------------------------
FAME_complete %>%
  select(Industry)%>%
  group_by(Industry)%>%
  count()%>%
  ggplot(aes(x=reorder(Industry,freq),y=freq,group=1,label=freq))+
  coord_flip()+
  geom_bar(stat="identity",fill ="#add4e6",show.legend = FALSE)+
  geom_text()+
  xlab("")+
  ylab("Number of firms")+
  theme_bw()

ggsave(
  "0. Number of Firms by sector.png",
  width = 10,
  height = 4,
  dpi = 1200
)

# (X)geo-insection -----------------------------------------------------------

# import the map data
localmap = st_read("D:/OneDrive - University College London/CASA0004_Dissertation/DataProcessing/Shapefiles/Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")
regionmap = st_read("D:/OneDrive - University College London/CASA0004_Dissertation/DataProcessing/Shapefiles/Regions_December_2018_EN_BFC.shp")

# adjust...
regionmap = st_transform(regionmap, crs = 4326)
localmap = localmap %>%
  filter(str_detect(`lad17cd`,"^E06") | str_detect(`lad17cd`,"^E07") | str_detect(`lad17cd`,"^E08") | str_detect(`lad17cd`,"^E09"))

# simplify the shapefile
regionmap = ms_simplify(regionmap, keep = 0.01,
            keep_shapes = TRUE)
localmap = ms_simplify(localmap, keep = 0.01,
                        keep_shapes = TRUE)

# 1.2 national overall  --------------------------------------------------------------------
# total turnover
FAME_complete %>%
  summarise(turnover2001 = sum(Turnover.th.GBP.2001,NA, na.rm = TRUE),
            turnover2002 = sum(Turnover.th.GBP.2002,NA, na.rm = TRUE),
            turnover2003 = sum(Turnover.th.GBP.2003,NA, na.rm = TRUE),
            turnover2004 = sum(Turnover.th.GBP.2004,NA, na.rm = TRUE),
            turnover2005 = sum(Turnover.th.GBP.2005,NA, na.rm = TRUE),
            turnover2006 = sum(Turnover.th.GBP.2006,NA, na.rm = TRUE),
            turnover2007 = sum(Turnover.th.GBP.2007,NA, na.rm = TRUE),
            turnover2008 = sum(Turnover.th.GBP.2008,NA, na.rm = TRUE),
            turnover2009 = sum(Turnover.th.GBP.2009,NA, na.rm = TRUE),
            turnover2010 = sum(Turnover.th.GBP.2010,NA, na.rm = TRUE),
            turnover2011 = sum(Turnover.th.GBP.2011,NA, na.rm = TRUE),
            turnover2012 = sum(Turnover.th.GBP.2012,NA, na.rm = TRUE),
            turnover2013 = sum(Turnover.th.GBP.2013,NA, na.rm = TRUE),
            turnover2014 = sum(Turnover.th.GBP.2014,NA, na.rm = TRUE),
            turnover2015 = sum(Turnover.th.GBP.2015,NA, na.rm = TRUE),
            turnover2016 = sum(Turnover.th.GBP.2016,NA, na.rm = TRUE),
            turnover2017 = sum(Turnover.th.GBP.2017,NA, na.rm = TRUE),
            turnover2018 = sum(Turnover.th.GBP.2018,NA, na.rm = TRUE))%>% 
  set_colnames(seq(2001,2018,1))%>% 
  melt(variable.name="year",
       value.name="turnover") %>%
  ggplot(aes(x = year,
             y = turnover/1E+12,
             group = 1,
             label = round(turnover/1E+12,2)))+
  geom_line()+geom_point()+
  geom_text(nudge_y = 2)+
  ylab("Total turnover(trillion £)")+theme_bw()

ggsave(
  "1. national turnover.png",
  width = 10,
  height = 6,
  dpi = 1200
)

# revenue per employee
FAME_complete %>%
  summarise(turnover_pc2001 = mean(Turnover.th.GBP.2001/Number.of.employees.2001,na.rm = TRUE),
            turnover_pc2002 = mean(Turnover.th.GBP.2002/Number.of.employees.2002,na.rm = TRUE),
            turnover_pc2003 = mean(Turnover.th.GBP.2003/Number.of.employees.2003,na.rm = TRUE),
            turnover_pc2004 = mean(Turnover.th.GBP.2004/Number.of.employees.2004,na.rm = TRUE),
            turnover_pc2005 = mean(Turnover.th.GBP.2005/Number.of.employees.2005,na.rm = TRUE),
            turnover_pc2006 = mean(Turnover.th.GBP.2006/Number.of.employees.2006,na.rm = TRUE),
            turnover_pc2007 = mean(Turnover.th.GBP.2007/Number.of.employees.2007,na.rm = TRUE),
            turnover_pc2008 = mean(Turnover.th.GBP.2008/Number.of.employees.2008,na.rm = TRUE),
            turnover_pc2009 = mean(Turnover.th.GBP.2009/Number.of.employees.2009,na.rm = TRUE),
            turnover_pc2010 = mean(Turnover.th.GBP.2010/Number.of.employees.2010,na.rm = TRUE),
            turnover_pc2011 = mean(Turnover.th.GBP.2011/Number.of.employees.2011,na.rm = TRUE),
            turnover_pc2012 = mean(Turnover.th.GBP.2012/Number.of.employees.2012,na.rm = TRUE),
            turnover_pc2013 = mean(Turnover.th.GBP.2013/Number.of.employees.2013,na.rm = TRUE),
            turnover_pc2014 = mean(Turnover.th.GBP.2014/Number.of.employees.2014,na.rm = TRUE),
            turnover_pc2015 = mean(Turnover.th.GBP.2015/Number.of.employees.2015,na.rm = TRUE),
            turnover_pc2016 = mean(Turnover.th.GBP.2016/Number.of.employees.2016,na.rm = TRUE),
            turnover_pc2017 = mean(Turnover.th.GBP.2017/Number.of.employees.2017,na.rm = TRUE),
            turnover_pc2018 = mean(Turnover.th.GBP.2018/Number.of.employees.2018,na.rm = TRUE))%>% 
  set_colnames(seq(2001,2018,1))%>%
  melt(variable.name="year",
       value.name="turnover_pc") %>%
  ggplot(aes(x = year,
             y = turnover_pc,
             group = 1,
             label = round(turnover_pc,1)))+
  geom_line()+
  geom_point()+
  geom_text(nudge_y = 2000)+
  ylab("Revenue per employee (£)")+
  theme_bw()
ggsave(
  "1. national revenue per employment.png",
  width = 10,
  height = 6,
  dpi = 1200
)


# total employment
FAME_complete%>%
  summarise(employee2001 = sum(Number.of.employees.2001,NA, na.rm = TRUE),
            employee2002 = sum(Number.of.employees.2002,NA, na.rm = TRUE),
            employee2003 = sum(Number.of.employees.2003,NA, na.rm = TRUE),
            employee2004 = sum(Number.of.employees.2004,NA, na.rm = TRUE),
            employee2005 = sum(Number.of.employees.2005,NA, na.rm = TRUE),
            employee2006 = sum(Number.of.employees.2006,NA, na.rm = TRUE),
            employee2007 = sum(Number.of.employees.2007,NA, na.rm = TRUE),
            employee2008 = sum(Number.of.employees.2008,NA, na.rm = TRUE),
            employee2009 = sum(Number.of.employees.2009,NA, na.rm = TRUE),
            employee2010 = sum(Number.of.employees.2010,NA, na.rm = TRUE),
            employee2011 = sum(Number.of.employees.2011,NA, na.rm = TRUE),
            employee2012 = sum(Number.of.employees.2012,NA, na.rm = TRUE),
            employee2013 = sum(Number.of.employees.2013,NA, na.rm = TRUE),
            employee2014 = sum(Number.of.employees.2014,NA, na.rm = TRUE),
            employee2015 = sum(Number.of.employees.2015,NA, na.rm = TRUE),
            employee2016 = sum(Number.of.employees.2016,NA, na.rm = TRUE),
            employee2017 = sum(Number.of.employees.2017,NA, na.rm = TRUE),
            employee2018 = sum(Number.of.employees.2018,NA, na.rm = TRUE))%>% 
  set_colnames(seq(2001,2018,1))%>%
  melt(variable.name="year",
       value.name="employee") %>%
  ggplot(aes(x = year,
             y = employee/1E+06,
             group = 1,
             label = round(employee/1E+06,1)))+
  geom_line()+
  geom_point()+
  geom_text(nudge_y = 0.5)+
  ylab("Number of employees(millions)")+
  theme_bw()

ggsave(
  "1. national employment.png",
  width = 10,
  height = 6,
  dpi = 1200
)

# 2.1 regional overall ------------------------------------------------------------------
# regional turnover 
rgn_turnover = FAME_complete%>%
  group_by(RGN19NM) %>%
  dplyr::summarize(turnover2001 = sum(Turnover.th.GBP.2001,NA, na.rm = TRUE),
            turnover2002 = sum(Turnover.th.GBP.2002,NA, na.rm = TRUE),
            turnover2003 = sum(Turnover.th.GBP.2003,NA, na.rm = TRUE),
            turnover2004 = sum(Turnover.th.GBP.2004,NA, na.rm = TRUE),
            turnover2005 = sum(Turnover.th.GBP.2005,NA, na.rm = TRUE),
            turnover2006 = sum(Turnover.th.GBP.2006,NA, na.rm = TRUE),
            turnover2007 = sum(Turnover.th.GBP.2007,NA, na.rm = TRUE),
            turnover2008 = sum(Turnover.th.GBP.2008,NA, na.rm = TRUE),
            turnover2009 = sum(Turnover.th.GBP.2009,NA, na.rm = TRUE),
            turnover2010 = sum(Turnover.th.GBP.2010,NA, na.rm = TRUE),
            turnover2011 = sum(Turnover.th.GBP.2011,NA, na.rm = TRUE),
            turnover2012 = sum(Turnover.th.GBP.2012,NA, na.rm = TRUE),
            turnover2013 = sum(Turnover.th.GBP.2013,NA, na.rm = TRUE),
            turnover2014 = sum(Turnover.th.GBP.2014,NA, na.rm = TRUE),
            turnover2015 = sum(Turnover.th.GBP.2015,NA, na.rm = TRUE),
            turnover2016 = sum(Turnover.th.GBP.2016,NA, na.rm = TRUE),
            turnover2017 = sum(Turnover.th.GBP.2017,NA, na.rm = TRUE),
            turnover2018 = sum(Turnover.th.GBP.2018,NA, na.rm = TRUE),
            employee2001 = sum(Number.of.employees.2001,NA, na.rm = TRUE),
            employee2002 = sum(Number.of.employees.2002,NA, na.rm = TRUE),
            employee2003 = sum(Number.of.employees.2003,NA, na.rm = TRUE),
            employee2004 = sum(Number.of.employees.2004,NA, na.rm = TRUE),
            employee2005 = sum(Number.of.employees.2005,NA, na.rm = TRUE),
            employee2006 = sum(Number.of.employees.2006,NA, na.rm = TRUE),
            employee2007 = sum(Number.of.employees.2007,NA, na.rm = TRUE),
            employee2008 = sum(Number.of.employees.2008,NA, na.rm = TRUE),
            employee2009 = sum(Number.of.employees.2009,NA, na.rm = TRUE),
            employee2010 = sum(Number.of.employees.2010,NA, na.rm = TRUE),
            employee2011 = sum(Number.of.employees.2011,NA, na.rm = TRUE),
            employee2012 = sum(Number.of.employees.2012,NA, na.rm = TRUE),
            employee2013 = sum(Number.of.employees.2013,NA, na.rm = TRUE),
            employee2014 = sum(Number.of.employees.2014,NA, na.rm = TRUE),
            employee2015 = sum(Number.of.employees.2015,NA, na.rm = TRUE),
            employee2016 = sum(Number.of.employees.2016,NA, na.rm = TRUE),
            employee2017 = sum(Number.of.employees.2017,NA, na.rm = TRUE),
            employee2018 = sum(Number.of.employees.2018,NA, na.rm = TRUE),
            turnover_pc2001 = mean(Turnover.th.GBP.2001/Number.of.employees.2001,na.rm = TRUE),
            turnover_pc2002 = mean(Turnover.th.GBP.2002/Number.of.employees.2002,na.rm = TRUE),
            turnover_pc2003 = mean(Turnover.th.GBP.2003/Number.of.employees.2003,na.rm = TRUE),
            turnover_pc2004 = mean(Turnover.th.GBP.2004/Number.of.employees.2004,na.rm = TRUE),
            turnover_pc2005 = mean(Turnover.th.GBP.2005/Number.of.employees.2005,na.rm = TRUE),
            turnover_pc2006 = mean(Turnover.th.GBP.2006/Number.of.employees.2006,na.rm = TRUE),
            turnover_pc2007 = mean(Turnover.th.GBP.2007/Number.of.employees.2007,na.rm = TRUE),
            turnover_pc2008 = mean(Turnover.th.GBP.2008/Number.of.employees.2008,na.rm = TRUE),
            turnover_pc2009 = mean(Turnover.th.GBP.2009/Number.of.employees.2009,na.rm = TRUE),
            turnover_pc2010 = mean(Turnover.th.GBP.2010/Number.of.employees.2010,na.rm = TRUE),
            turnover_pc2011 = mean(Turnover.th.GBP.2011/Number.of.employees.2011,na.rm = TRUE),
            turnover_pc2012 = mean(Turnover.th.GBP.2012/Number.of.employees.2012,na.rm = TRUE),
            turnover_pc2013 = mean(Turnover.th.GBP.2013/Number.of.employees.2013,na.rm = TRUE),
            turnover_pc2014 = mean(Turnover.th.GBP.2014/Number.of.employees.2014,na.rm = TRUE),
            turnover_pc2015 = mean(Turnover.th.GBP.2015/Number.of.employees.2015,na.rm = TRUE),
            turnover_pc2016 = mean(Turnover.th.GBP.2016/Number.of.employees.2016,na.rm = TRUE),
            turnover_pc2017 = mean(Turnover.th.GBP.2017/Number.of.employees.2017,na.rm = TRUE),
            turnover_pc2018 = mean(Turnover.th.GBP.2018/Number.of.employees.2018,na.rm = TRUE))

regionmap = merge(regionmap,rgn_turnover,by.x = "rgn18nm",by.y="RGN19NM")

# 2.1 ggplot - regional turnover -------------------------------------------------------
ggplot(data = rgn_turnover[1:19] %>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year",
              value.name="turnover"), 
       aes(x= year, y= turnover/1e+9, 
           group = region,
           color = region)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("year")+
  ylab("Total turnover(billion ￡)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "1. regional turnover.png",
  width = 8,
  height = 5,
  dpi = 1200
)


# regional turnover(log)
ggplot(data = rgn_turnover[1:19] %>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year",
              value.name="turnover"), 
       aes(x= year, y= log(turnover), 
           group = region,
           color = region)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("year")+
  ylab("Total turnover (log-transformed) ")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "1. regional turnover(log).png",
  width = 8,
  height = 5,
  dpi = 1200
)


# turnover propotion
ggplot(data = cbind(rgn_turnover[1],rgn_turnover[2:19] %>% mutate_all(~prop.table(.))) %>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year",
              value.name="turnover"), 
       mapping=aes(x= year,y=turnover,fill=region,lable=turnover))+
  geom_bar(stat="identity", position="fill")+
  scale_fill_brewer(palette="Spectral")+
  geom_text(aes(label = percent(turnover,accuracy=.1)),size=3,position=position_fill(0.5))+
  theme_bw()+
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  ylab("Share of total turnover")

ggsave(
  "1. regional turnover(proportion).png",
  width = 8,
  height = 6,
  dpi = 1200
)

# employment
ggplot(data = rgn_turnover[c(1,20:37)]  %>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year"), 
       aes(x= year, y= value/1000000, group = region,color = region)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("year")+
  ylab("Employment(millions)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "1. regional employment.png",
  width = 8,
  height = 5,
  dpi = 1200
)
# employment(log)
ggplot(data = rgn_turnover[c(1,20:37)]  %>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year"), 
       aes(x= year, y= log(value), group = region,color = region)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("year")+
  ylab("log(Employment)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "1. regional employment(log).png",
  width = 8,
  height = 5,
  dpi = 1200
)

# employment propotion
ggplot(data = cbind(rgn_turnover[1],rgn_turnover[20:37] %>% mutate_all(~prop.table(.))) %>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year",
              value.name="turnover"), 
       mapping=aes(x= year,y=turnover,fill=region,lable=turnover))+
  geom_bar(stat="identity", position="fill")+
  scale_fill_brewer(palette="Spectral")+
  geom_text(aes(label = percent(turnover,accuracy=.1)),size=3,position=position_fill(0.5))+
  theme_bw()+
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  ylab("Share of total employment")

ggsave(
  "1. regional employment(proportion).png",
  width = 8,
  height = 6,
  dpi = 1200
)

# revenue per employee
ggplot(data = rgn_turnover[c(1,38:55)] %>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year",
              value.name="turnover"), 
       aes(x= year, y= turnover, 
           group = region,
           color = region)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("year")+
  ylab("Revenue per employee")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "1. regional revenue per employee.png",
  width = 8,
  height = 5,
  dpi = 1200
)

# n of firms
ggplot(data = fame %>% 
         dplyr::group_by(RGN19NM,year)%>%
         dplyr::summarize(sum = sum(freq))%>%
         set_colnames(c("region","year","sum")), 
       aes(x= year, y= sum, 
           group = region,
           color = region)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("year")+
  ylab("Number of firms")+
  scale_x_continuous(breaks = c(2001:2018,1))+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "1. regional number of firms.png",
  width = 8,
  height = 5,
  dpi = 1200
)

fame %>% 
  dplyr::group_by(RGN19NM,year)%>%
  dplyr::summarize(sum = sum(freq))%>%
  spread(year,sum)
  

# n of firm propotion
ggplot(data = cbind(fame %>% 
                      dplyr::group_by(RGN19NM,year)%>%
                      dplyr::summarize(sum = sum(freq))%>%
                      spread(year,sum)%>%
                      .[1]%>%as.data.frame(),
                    fame %>% 
                      dplyr::group_by(RGN19NM,year)%>%
                      dplyr::summarize(sum = sum(freq))%>%
                      spread(year,sum)%>%
                      .[2:19]%>% mutate_all(~prop.table(.))%>%as.data.frame())%>%
         set_colnames(c('region',seq(2001,2018,1)))%>%
         melt(id.vars=c("region"),
              variable.name ="year",
              value.name="turnover"), 
       mapping=aes(x= year,y=turnover,fill=region,lable=turnover))+
  geom_bar(stat="identity", position="fill")+
  scale_fill_brewer(palette="Spectral")+
  geom_text(aes(label = percent(turnover,accuracy=.1)),size=3,position=position_fill(0.5))+
  theme_bw()+
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  ylab("Share of number of firms")

ggsave(
  "1. regional number of firms(proportion).png",
  width = 8,
  height = 6,
  dpi = 1200
)

# 2.2 regional disparity n of firms ----------------------------------------
fame %>%
  spread(year,freq) %>%
  dplyr::group_by(RGN19NM)%>%
  dplyr::summarize(gini_2001 = gini(`2001`),gini_2002 = gini(`2002`),
                   gini_2003 = gini(`2003`),gini_2004 = gini(`2004`),
                   gini_2005 = gini(`2005`),gini_2006 = gini(`2006`),
                   gini_2007 = gini(`2007`),gini_2008 = gini(`2008`),
                   gini_2009 = gini(`2009`),gini_2010 = gini(`2010`),
                   gini_2011 = gini(`2011`),gini_2012 = gini(`2012`),
                   gini_2013 = gini(`2013`),gini_2014 = gini(`2014`),
                   gini_2015 = gini(`2015`),gini_2016 = gini(`2016`),
                   gini_2017 = gini(`2017`),gini_2018 = gini(`2018`))%>%
  as.data.frame()



# 2.3 local authority turnover ------------------------------------------------
lad_turnover = FAME_complete %>%
  group_by(RGN19NM,LAD19NM) %>%
  dplyr::summarise(turnover2001 = sum(Turnover.th.GBP.2001,NA, na.rm = TRUE),
            turnover2002 = sum(Turnover.th.GBP.2002,NA, na.rm = TRUE),
            turnover2003 = sum(Turnover.th.GBP.2003,NA, na.rm = TRUE),
            turnover2004 = sum(Turnover.th.GBP.2004,NA, na.rm = TRUE),
            turnover2005 = sum(Turnover.th.GBP.2005,NA, na.rm = TRUE),
            turnover2006 = sum(Turnover.th.GBP.2006,NA, na.rm = TRUE),
            turnover2007 = sum(Turnover.th.GBP.2007,NA, na.rm = TRUE),
            turnover2008 = sum(Turnover.th.GBP.2008,NA, na.rm = TRUE),
            turnover2009 = sum(Turnover.th.GBP.2009,NA, na.rm = TRUE),
            turnover2010 = sum(Turnover.th.GBP.2010,NA, na.rm = TRUE),
            turnover2011 = sum(Turnover.th.GBP.2011,NA, na.rm = TRUE),
            turnover2012 = sum(Turnover.th.GBP.2012,NA, na.rm = TRUE),
            turnover2013 = sum(Turnover.th.GBP.2013,NA, na.rm = TRUE),
            turnover2014 = sum(Turnover.th.GBP.2014,NA, na.rm = TRUE),
            turnover2015 = sum(Turnover.th.GBP.2015,NA, na.rm = TRUE),
            turnover2016 = sum(Turnover.th.GBP.2016,NA, na.rm = TRUE),
            turnover2017 = sum(Turnover.th.GBP.2017,NA, na.rm = TRUE),
            turnover2018 = sum(Turnover.th.GBP.2018,NA, na.rm = TRUE),
            employee2001 = sum(Number.of.employees.2001,NA, na.rm = TRUE),
            employee2002 = sum(Number.of.employees.2002,NA, na.rm = TRUE),
            employee2003 = sum(Number.of.employees.2003,NA, na.rm = TRUE),
            employee2004 = sum(Number.of.employees.2004,NA, na.rm = TRUE),
            employee2005 = sum(Number.of.employees.2005,NA, na.rm = TRUE),
            employee2006 = sum(Number.of.employees.2006,NA, na.rm = TRUE),
            employee2007 = sum(Number.of.employees.2007,NA, na.rm = TRUE),
            employee2008 = sum(Number.of.employees.2008,NA, na.rm = TRUE),
            employee2009 = sum(Number.of.employees.2009,NA, na.rm = TRUE),
            employee2010 = sum(Number.of.employees.2010,NA, na.rm = TRUE),
            employee2011 = sum(Number.of.employees.2011,NA, na.rm = TRUE),
            employee2012 = sum(Number.of.employees.2012,NA, na.rm = TRUE),
            employee2013 = sum(Number.of.employees.2013,NA, na.rm = TRUE),
            employee2014 = sum(Number.of.employees.2014,NA, na.rm = TRUE),
            employee2015 = sum(Number.of.employees.2015,NA, na.rm = TRUE),
            employee2016 = sum(Number.of.employees.2016,NA, na.rm = TRUE),
            employee2017 = sum(Number.of.employees.2017,NA, na.rm = TRUE),
            employee2018 = sum(Number.of.employees.2018,NA, na.rm = TRUE),
            turnover_pc2001 = mean(Turnover.th.GBP.2001/Number.of.employees.2001,na.rm = TRUE),
            turnover_pc2002 = mean(Turnover.th.GBP.2002/Number.of.employees.2002,na.rm = TRUE),
            turnover_pc2003 = mean(Turnover.th.GBP.2003/Number.of.employees.2003,na.rm = TRUE),
            turnover_pc2004 = mean(Turnover.th.GBP.2004/Number.of.employees.2004,na.rm = TRUE),
            turnover_pc2005 = mean(Turnover.th.GBP.2005/Number.of.employees.2005,na.rm = TRUE),
            turnover_pc2006 = mean(Turnover.th.GBP.2006/Number.of.employees.2006,na.rm = TRUE),
            turnover_pc2007 = mean(Turnover.th.GBP.2007/Number.of.employees.2007,na.rm = TRUE),
            turnover_pc2008 = mean(Turnover.th.GBP.2008/Number.of.employees.2008,na.rm = TRUE),
            turnover_pc2009 = mean(Turnover.th.GBP.2009/Number.of.employees.2009,na.rm = TRUE),
            turnover_pc2010 = mean(Turnover.th.GBP.2010/Number.of.employees.2010,na.rm = TRUE),
            turnover_pc2011 = mean(Turnover.th.GBP.2011/Number.of.employees.2011,na.rm = TRUE),
            turnover_pc2012 = mean(Turnover.th.GBP.2012/Number.of.employees.2012,na.rm = TRUE),
            turnover_pc2013 = mean(Turnover.th.GBP.2013/Number.of.employees.2013,na.rm = TRUE),
            turnover_pc2014 = mean(Turnover.th.GBP.2014/Number.of.employees.2014,na.rm = TRUE),
            turnover_pc2015 = mean(Turnover.th.GBP.2015/Number.of.employees.2015,na.rm = TRUE),
            turnover_pc2016 = mean(Turnover.th.GBP.2016/Number.of.employees.2016,na.rm = TRUE),
            turnover_pc2017 = mean(Turnover.th.GBP.2017/Number.of.employees.2017,na.rm = TRUE),
            turnover_pc2018 = mean(Turnover.th.GBP.2018/Number.of.employees.2018,na.rm = TRUE))

localmap = merge(localmap,lad_turnover,by = "lad17nm")

# 2.4 intra-regional dispersion ---------------------------------------------------------------------
lad_turnover[c(1,20)] %>%
  ggplot(aes(x=RGN19NM,y=log(turnover2018)))+
  geom_boxplot(fill="gray")+
  stat_identity()+
  theme_bw()+
  coord_flip()+
  xlab("")

ggsave(
  "2. regional dispersion.png",
  width = 10,
  height = 6,
  dpi = 1200
)

cbind(lad_turnover[2],log(lad_turnover[20]))%>%as.data.frame()

tm_shape(merge(localmap,
               cbind(lad_turnover[2],log(lad_turnover[20]))%>%as.data.frame(),
               by.x ="lad17nm",by.y="LAD19NM")) +
  tm_polygons("turnover2018", palette="RdYlBu", 
              title="Williamson index chage(2001-2018)", id="name",legend.hist = TRUE) +
  tm_borders(col = "white",alpha = 0.5)+
  tm_shape(regionmap)+tm_borders(col = "black",lwd = 2)+
  tm_fill(alpha = 0)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE, legend.outside = TRUE) +  
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05)) 




# 2.4.1 inter-regional disparity ----------------------------------------------------------------
inter_disparity = rgn_turnover %>%
  summarise(gini_turnover2001 = gini(turnover2001),gini_turnover2002 = gini(turnover2002),
            gini_turnover2003 = gini(turnover2003),gini_turnover2004 = gini(turnover2004),
            gini_turnover2005 = gini(turnover2005),gini_turnover2006 = gini(turnover2006),
            gini_turnover2007 = gini(turnover2007),gini_turnover2008 = gini(turnover2008),
            gini_turnover2009 = gini(turnover2009),gini_turnover2010 = gini(turnover2010),
            gini_turnover2011 = gini(turnover2011),gini_turnover2012 = gini(turnover2012),
            gini_turnover2013 = gini(turnover2013),gini_turnover2014 = gini(turnover2014),
            gini_turnover2015 = gini(turnover2015),gini_turnover2016 = gini(turnover2016),
            gini_turnover2017 = gini(turnover2017),gini_turnover2018 = gini(turnover2018),
            
            gini_pe2001 = gini2(as.double(turnover2001), weighting = as.double(employee2001)),gini_pe2002 = gini2(as.double(turnover2002), weighting = as.double(employee2002)),
            gini_pe2003 = gini2(as.double(turnover2003), weighting = as.double(employee2003)),gini_pe2004 = gini2(as.double(turnover2004), weighting = as.double(employee2004)),
            gini_pe2005 = gini2(as.double(turnover2005), weighting = as.double(employee2005)),gini_pe2006 = gini2(as.double(turnover2006), weighting = as.double(employee2006)),
            gini_pe2007 = gini2(as.double(turnover2007), weighting = as.double(employee2007)),gini_pe2008 = gini2(as.double(turnover2008), weighting = as.double(employee2008)),
            gini_pe2009 = gini2(as.double(turnover2009), weighting = as.double(employee2009)),gini_pe2010 = gini2(as.double(turnover2010), weighting = as.double(employee2010)),
            gini_pe2011 = gini2(as.double(turnover2011), weighting = as.double(employee2011)),gini_pe2012 = gini2(as.double(turnover2012), weighting = as.double(employee2012)),
            gini_pe2013 = gini2(as.double(turnover2013), weighting = as.double(employee2013)),gini_pe2014 = gini2(as.double(turnover2014), weighting = as.double(employee2014)),
            gini_pe2015 = gini2(as.double(turnover2015), weighting = as.double(employee2015)),gini_pe2016 = gini2(as.double(turnover2016), weighting = as.double(employee2016)),
            gini_pe2017 = gini2(as.double(turnover2017), weighting = as.double(employee2017)),gini_pe2018 = gini2(as.double(turnover2018), weighting = as.double(employee2018)))
inter_disparity$RGN19NM = "England" 


# number of firms
inter_disparity_numfirm = fame %>%
  spread(year,freq) %>%
  summarise(gini_2001 = gini(.[4]),gini_2002 = gini(.[5]),
            gini_2003 = gini(.[6]),gini_2004 = gini(.[7]),
            gini_2005 = gini(.[8]),gini_2006 = gini(.[9]),
            gini_2007 = gini(.[10]),gini_2008 = gini(.[11]),
            gini_2009 = gini(.[12]),gini_2010 = gini(.[13]),
            gini_2011 = gini(.[14]),gini_2012 = gini(.[15]),
            gini_2013 = gini(.[16]),gini_2014 = gini(.[17]),
            gini_2015 = gini(.[18]),gini_2016 = gini(.[19]),
            gini_2017 = gini(.[20]),gini_2018 = gini(.[21]))
inter_disparity_numfirm $ RGN19NM = "England" 
# 2.4.2 intra-regional disparity ---------------------------------------------------------------
intra_disparity_r = lad_turnover%>%
  group_by(RGN19NM)%>%
  dplyr::summarise(gini_turnover2001 = gini(turnover2001),gini_turnover2002 = gini(turnover2002),
            gini_turnover2003 = gini(turnover2003),gini_turnover2004 = gini(turnover2004),
            gini_turnover2005 = gini(turnover2005),gini_turnover2006 = gini(turnover2006),
            gini_turnover2007 = gini(turnover2007),gini_turnover2008 = gini(turnover2008),
            gini_turnover2009 = gini(turnover2009),gini_turnover2010 = gini(turnover2010),
            gini_turnover2011 = gini(turnover2011),gini_turnover2012 = gini(turnover2012),
            gini_turnover2013 = gini(turnover2013),gini_turnover2014 = gini(turnover2014),
            gini_turnover2015 = gini(turnover2015),gini_turnover2016 = gini(turnover2016),
            gini_turnover2017 = gini(turnover2017),gini_turnover2018 = gini(turnover2018),
            
            gini_pe2001 = gini2(as.double(turnover2001), weighting = as.double(employee2001)),gini_pe2002 = gini2(as.double(turnover2002), weighting = as.double(employee2002)),
            gini_pe2003 = gini2(as.double(turnover2003), weighting = as.double(employee2003)),gini_pe2004 = gini2(as.double(turnover2004), weighting = as.double(employee2004)),
            gini_pe2005 = gini2(as.double(turnover2005), weighting = as.double(employee2005)),gini_pe2006 = gini2(as.double(turnover2006), weighting = as.double(employee2006)),
            gini_pe2007 = gini2(as.double(turnover2007), weighting = as.double(employee2007)),gini_pe2008 = gini2(as.double(turnover2008), weighting = as.double(employee2008)),
            gini_pe2009 = gini2(as.double(turnover2009), weighting = as.double(employee2009)),gini_pe2010 = gini2(as.double(turnover2010), weighting = as.double(employee2010)),
            gini_pe2011 = gini2(as.double(turnover2011), weighting = as.double(employee2011)),gini_pe2012 = gini2(as.double(turnover2012), weighting = as.double(employee2012)),
            gini_pe2013 = gini2(as.double(turnover2013), weighting = as.double(employee2013)),gini_pe2014 = gini2(as.double(turnover2014), weighting = as.double(employee2014)),
            gini_pe2015 = gini2(as.double(turnover2015), weighting = as.double(employee2015)),gini_pe2016 = gini2(as.double(turnover2016), weighting = as.double(employee2016)),
            gini_pe2017 = gini2(as.double(turnover2017), weighting = as.double(employee2017)),gini_pe2018 = gini2(as.double(turnover2018), weighting = as.double(employee2018)))

intra_disparity_r %>% View()

intra_disparity_r$ginichange =  intra_disparity_r$gini_turnover2018 - intra_disparity_r$gini_turnover2001
intra_disparity_r$gini_pechange = intra_disparity_r$gini_pe2018 - intra_disparity_r$gini_pe2001

regionmap = merge(regionmap,intra_disparity_r,by.x = "rgn18nm",by.y='RGN19NM')
# number of firms
intra_disparity_r_numfirm = fame %>%
  spread(year,freq) %>%
  dplyr::group_by(RGN19NM)%>%
  dplyr::summarize(gini_2001 = gini(`2001`),gini_2002 = gini(`2002`),
                   gini_2003 = gini(`2003`),gini_2004 = gini(`2004`),
                   gini_2005 = gini(`2005`),gini_2006 = gini(`2006`),
                   gini_2007 = gini(`2007`),gini_2008 = gini(`2008`),
                   gini_2009 = gini(`2009`),gini_2010 = gini(`2010`),
                   gini_2011 = gini(`2011`),gini_2012 = gini(`2012`),
                   gini_2013 = gini(`2013`),gini_2014 = gini(`2014`),
                   gini_2015 = gini(`2015`),gini_2016 = gini(`2016`),
                   gini_2017 = gini(`2017`),gini_2018 = gini(`2018`))%>%
  as.data.frame()

intra_disparity_r_numfirm$ginichange =  intra_disparity_r_numfirm$gini_2018 - intra_disparity_r_numfirm$gini_2001
 
regionmap = merge(regionmap,intra_disparity_r_numfirm,by.x = "rgn18nm",by.y='RGN19NM')

# (X)local authority ------------------------------------------------------

intra_disparity_l = FAME_complete %>%
  subset(Company.status == "Active")%>%
  filter( LAD19NM != "Isles of Scilly")%>%
  group_by(LAD19NM) %>%
  summarise(gini_turnover2001 = gini(Turnover.th.GBP.2001),gini_turnover2002 = gini(Turnover.th.GBP.2002),
            gini_turnover2003 = gini(Turnover.th.GBP.2003),gini_turnover2004 = gini(Turnover.th.GBP.2004),
            gini_turnover2005 = gini(Turnover.th.GBP.2005),gini_turnover2006 = gini(Turnover.th.GBP.2006),
            gini_turnover2007 = gini(Turnover.th.GBP.2007),gini_turnover2008 = gini(Turnover.th.GBP.2008),
            gini_turnover2009 = gini(Turnover.th.GBP.2009),gini_turnover2010 = gini(Turnover.th.GBP.2010),
            gini_turnover2011 = gini(Turnover.th.GBP.2011),gini_turnover2012 = gini(Turnover.th.GBP.2012),
            gini_turnover2013 = gini(Turnover.th.GBP.2013),gini_turnover2014 = gini(Turnover.th.GBP.2014),
            gini_turnover2015 = gini(Turnover.th.GBP.2015),gini_turnover2016 = gini(Turnover.th.GBP.2016),
            gini_turnover2017 = gini(Turnover.th.GBP.2017),gini_turnover2018 = gini(Turnover.th.GBP.2018),
            
            gini_pe2001 = gini2(as.double(Turnover.th.GBP.2001), weighting = as.double(Number.of.employees.2001)),gini_pe2002 = gini2(as.double(Turnover.th.GBP.2002), weighting = as.double(Number.of.employees.2002)),
            gini_pe2003 = gini2(as.double(Turnover.th.GBP.2003), weighting = as.double(Number.of.employees.2003)),gini_pe2003 = gini2(as.double(Turnover.th.GBP.2004), weighting = as.double(Number.of.employees.2004)),
            gini_pe2005 = gini2(as.double(Turnover.th.GBP.2005), weighting = as.double(Number.of.employees.2005)),gini_pe2006 = gini2(as.double(Turnover.th.GBP.2006), weighting = as.double(Number.of.employees.2006)),
            gini_pe2007 = gini2(as.double(Turnover.th.GBP.2007), weighting = as.double(Number.of.employees.2007)),gini_pe2008 = gini2(as.double(Turnover.th.GBP.2008), weighting = as.double(Number.of.employees.2008)),
            gini_pe2009 = gini2(as.double(Turnover.th.GBP.2009), weighting = as.double(Number.of.employees.2009)),gini_pe2010 = gini2(as.double(Turnover.th.GBP.2010), weighting = as.double(Number.of.employees.2010)),
            gini_pe2011 = gini2(as.double(Turnover.th.GBP.2011), weighting = as.double(Number.of.employees.2011)),gini_pe2012 = gini2(as.double(Turnover.th.GBP.2012), weighting = as.double(Number.of.employees.2012)),
            gini_pe2013 = gini2(as.double(Turnover.th.GBP.2013), weighting = as.double(Number.of.employees.2013)),gini_pe2014 = gini2(as.double(Turnover.th.GBP.2014), weighting = as.double(Number.of.employees.2014)),
            gini_pe2015 = gini2(as.double(Turnover.th.GBP.2015), weighting = as.double(Number.of.employees.2015)),gini_pe2016 = gini2(as.double(Turnover.th.GBP.2016), weighting = as.double(Number.of.employees.2016)),
            gini_pe2017 = gini2(as.double(Turnover.th.GBP.2017), weighting = as.double(Number.of.employees.2017)),gini_pe2018 = gini2(as.double(Turnover.th.GBP.2018), weighting = as.double(Number.of.employees.2018)))




localmap = merge(localmap,intra_disparity_l,by = "lad17nm")
localmap$ginichange = localmap$gini_turnover2018 - localmap$gini_turnover2001
localmap$willchange = localmap$williamson_percapita2018 - localmap$williamson_percapita2001

# 2.5 GINI local authority(map) ----------------------------------------------------

# change
tm_shape(localmap) +
  tm_polygons("ginichange", palette="RdYlBu", midpoint = 0,
              title="GINI index chage(2001-2018)", id="name",legend.hist = TRUE) +
  tm_borders(col = "white",alpha = 0.5)+
  tm_shape(regionmap)+tm_borders(col = "black",lwd = 2)+
  tm_fill(alpha = 0)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE, legend.outside = TRUE) +  
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05)) 

# 01 and 18
tm_shape(localmap) +
  tm_polygons("gini_turnover2001", palette="RdYlBu", midpoint = 0,
              breaks = c(0,0.2,0.4,0.6,0.7,0.8,0.9,1),
              title="gini_turnover2001", id="name",legend.hist = TRUE) +
  tm_borders(col = "white",alpha = 0.5)+
  tm_shape(regionmap)+tm_borders(col = "black",lwd = 2)+
  tm_fill(alpha = 0)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE, 
            legend.outside = TRUE)+ 
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05))  

tm_shape(localmap) +
  tm_polygons("gini_turnover2018", palette="RdYlBu", midpoint = 0,
              breaks = c(0,0.2,0.4,0.6,0.7,0.8,0.9,1),
              title="gini_turnover2018", id="name",legend.hist = TRUE) +
  tm_borders(col = "white",alpha = 0.5)+
  tm_shape(regionmap)+tm_borders(col = "black",lwd = 2)+
  tm_fill(alpha = 0)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(title = "sss",
            frame = FALSE, 
            legend.outside = TRUE)+  
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05))  

# 2.5 GINI region(map) ----------------------------------------------------
# change
tm_shape(regionmap) +
  tm_polygons("ginichange", palette="RdYlBu", midpoint = 0,
              title="GINI index chage(2001-2018)", id="name") +
  tm_borders(col = "black",alpha = 1)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE) +  
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05)) 

# 01 and 18
tm_shape(regionmap) +
  tm_polygons("gini_turnover2001", palette="RdYlBu", midpoint = 0,
              title="gini_turnover2001", id="name",breaks = seq(0.45,0.85,0.05)) +
  tm_borders(col = "black",alpha = 1)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE) + 
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05))  

tm_shape(regionmap) +
  tm_polygons("gini_turnover2018", palette="RdYlBu", midpoint = 0,
              title="gini_turnover2018", id="name",breaks = seq(0.45,0.85,0.05)) +
  tm_borders(col = "black",alpha = 1)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE) + 
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05))  

regionmap%>%glimpse()
# 2.5 GINI weighted region (map) -------------------------------------------------------------
tm_shape(regionmap) +
  tm_polygons("gini_pechange", palette="RdYlBu", midpoint = 0,
              title="Gini weighted index change(2001-2018)", id="name") +
  tm_borders(col = "black",alpha = 1)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE) +  
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05)) 

# 01 and 18
tm_shape(regionmap) +
  tm_polygons("gini_pe2001", palette="RdYlBu", midpoint = 0,
              #breaks = seq(0.25,0.60,0.05),
              title="Gini index(weighted) 2001", id="name") +
  tm_borders(col = "black",alpha = 1)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE) + 
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05))  

tm_shape(regionmap) +
  tm_polygons("gini_pe2018", palette="RdYlBu", midpoint = 0,
              #breaks = seq(0.25,0.60,0.05),
              title="Gini index(weighted) 2018", id="name") +
  tm_borders(col = "black",alpha = 1)+
  tm_text("rgn18nm",size=1,col="black",just = "top") +
  tm_layout(frame = FALSE) + 
  tm_scale_bar(position = c(0.6, 0.05)) +  
  tm_compass(type = "4star", 
             size = 2.5, 
             fontsize = 0.5, 
             position = c(0.5, 0.05)) 


# 2.5 ggplot - multiple line lines ----------------------------------------------------------------------
intra_disparity_r[c(1:19)] %>% View()
intra_disparity_r[c(1,20:37)] %>% View()

# inter-regional gini
inter_disparity_numfirm$gini = "GINI_n.of firms"
inter_disparity$gini = "GINI_turnover"
inter_disparity$gini_pe = "GINI_turnover(employment weighted)"

ggplot(data = rbind(melt(inter_disparity[c(1:18,38)] %>%
                           set_colnames(c(as.character(seq(2001,2018,by=1)),"Gini.Coefficient")) %>%
                           as.data.frame(),
                         id.vars = "Gini.Coefficient"),
                    melt(inter_disparity[c(19:36,39)] %>%
                           set_colnames(c(as.character(seq(2001,2018,by=1)),"Gini.Coefficient")) %>%
                           as.data.frame(),
                         id.vars = "Gini.Coefficient"),
                    melt(inter_disparity_numfirm[c(1:18,20)] %>%
                           set_colnames(c(as.character(seq(2001,2018,by=1)),"Gini.Coefficient")) %>%
                           as.data.frame(),
                         id.vars = "Gini.Coefficient")), 
       mapping = aes(x = variable, y = value, group=Gini.Coefficient, color = Gini.Coefficient, label = round(value,3))) +
  geom_line() +
  geom_point() +
  geom_text(color = "black",nudge_y = 0.01)+
  xlab("year")+
  ylab("Gini.Coefficient")+
  ylim(0.36, 0.66)+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "1. national gini.png",
  width = 10,
  height = 5,
  dpi = 1200
)

# GINI turnover
ggplot(data = melt(rbind(inter_disparity[c(1:18,37)],
                         intra_disparity_r[c(1:19)])%>% 
                     set_colnames(c(as.character(seq(2001,2018,by=1)),"region")) %>%
                     as.data.frame(),
                   id.vars = "region"), 
       mapping = aes(x = variable, y = value, color = region, group=region)) +
  geom_line() + 
  scale_color_brewer(palette = "Spectral")+
  geom_point() +
  labs(x = "year", 
       y = "GINI - turnover", 
       title = "GINI - turnover - 2001:2018")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(
  "2. regional GINI.png",
  width = 8,
  height = 5,
  dpi = 1200
)


# GINI turnover(weighted)
ggplot(data = melt(rbind(inter_disparity[c(19:37)], 
                         intra_disparity_r[c(20:37,1)])%>% 
                     set_colnames(c(seq(2001,2018,by=1),"region")),
                   id.vars = "region"), 
       mapping = aes(x = variable, y = value, color = region, group=region)) +
  geom_line() + 
  scale_color_brewer(palette = "Spectral")+
  geom_point() +
  labs(x = "year", 
       y = "GINI turnover(weighted)", 
       title = "GINI - turnover per employee - 2001:2018")+
  theme_bw()+
  theme(legend.position = "bottom")
  
ggsave(
  "2. regional GINI(weighted).png",
  width = 8,
  height = 5,
  dpi = 1200
)

# GINI n of firm
ggplot(data = melt(intra_disparity_r_numfirm[1:19]%>% 
                     set_colnames(c("region",seq(2001,2018,by=1))),
                   id.vars = "region"), 
       mapping = aes(x = variable, y = value, color = region, group=region)) +
  geom_line() + 
  scale_color_brewer(palette = "Spectral")+
  geom_point() +
  labs(x = "year", 
       y = "GINI - number of firms", 
       title = "GINI - number of firms - 2001:2018")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "2. regional GINI firm.png",
  width = 8,
  height = 5,
  dpi = 1200
)

  
# 2.6 Regional disparities over time  --------------------------------------
data (G.counties.gdp)

betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
              conditions = NULL, print.results = TRUE)
betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp[65:66], 2012, 
              conditions = NULL, print.results = TRUE)

# regional convengence
rca (rgn_turnover$turnover2001, 2001, 
     rgn_turnover[3:19], 2018, 
     conditions = NULL, sigma.type = "trend", sigma.measure = "cv",
     beta.plot = TRUE, beta.plotLine = TRUE, 
     beta.plotX = "Ln (initial yearly turnover)", beta.plotY = "Ln (av. growth)",
     beta.plotTitle = "Beta convergence of regions 2001-2018",
     sigma.plot = TRUE, sigma.plotY = "cv of ln (turnover)",
     sigma.plotTitle = "Sigma convergence of regions 2001-2018")

rca (lad_turnover[-244,]$turnover2001, 2001, 
     lad_turnover[-244,4:20], 2018, 
     conditions = NULL, sigma.type = "trend", sigma.measure = "cv",
     beta.plot = TRUE, beta.plotLine = TRUE, 
     beta.plotX = "Ln (initial yearly turnover)", beta.plotY = "Ln (av. growth)",
     beta.plotTitle = "Beta convergence of local authorities 2001-2018",
     sigma.plot = TRUE, sigma.plotY = "cv of ln (turnover)",
     sigma.plotTitle = "Sigma convergence of local authorities 2001-2018")

# number of firms
rca (fame %>%
       spread(year,freq)%>%
       select(-Industry,-LAD19NM)%>%
       group_by(RGN19NM)%>%
       summarise_all(~ sum(.,na.rm = TRUE))%>%as.data.frame()%>%
       .[2]%>%as.data.frame()%>%sapply(as.numeric), 2001, 
     fame %>%
       spread(year,freq)%>%
       select(-Industry,-LAD19NM)%>%
       group_by(RGN19NM)%>%
       summarise_all(~ sum(.,na.rm = TRUE))%>%as.data.frame()%>%
       .[3:19]%>%as.data.frame()%>%sapply(as.numeric), 2018, 
     conditions = NULL, sigma.type = "trend", sigma.measure = "cv",
     beta.plot = TRUE, beta.plotLine = TRUE, 
     beta.plotX = "Ln (initial number of firms)", beta.plotY = "Ln (av. growth)",
     beta.plotTitle = "Beta convergence of regions 2001-2018",
     sigma.plot = TRUE, sigma.plotY = "cv of ln (n of firms)",
     sigma.plotTitle = "Sigma convergence of regions 2001-2018")

rca (fame %>%
    spread(year,freq)%>%
    select(-Industry,-RGN19NM)%>%
    group_by(LAD19NM)%>%
    summarise_all(~ sum(.,na.rm = TRUE))%>%as.data.frame()%>%
    .[2]%>%as.data.frame()%>%sapply(as.numeric), 2001, 
    fame %>%
      spread(year,freq)%>%
      select(-Industry,-RGN19NM)%>%
      group_by(LAD19NM)%>%
      summarise_all(~ sum(.,na.rm = TRUE))%>%as.data.frame()%>%
       .[3:19]%>%as.data.frame()%>%sapply(as.numeric), 2018, 
     conditions = NULL, sigma.type = "trend", sigma.measure = "cv",
     beta.plot = TRUE, beta.plotLine = TRUE, 
     beta.plotX = "Ln (initial number of firms)", beta.plotY = "Ln (av. growth)",
     beta.plotTitle = "Beta convergence of regions 2001-2018",
     sigma.plot = TRUE, sigma.plotY = "cv of ln (n of firms)",
     sigma.plotTitle = "Sigma convergence of regions 2001-2018")

# 2.7 regional growth ------------------------------------------------------
industry = c("A,B,D,E","F","G,I","K,L,M,N","C","R,S,T,U","O,P,Q","H,J")

rgn_sec_turnover %>% View()
# East Midland
locq.growth (rgn_sec_turnover[1:8,] $ turnover2001,
             rgn_sec_turnover[1:8,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[1:8,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in East Midlands",
             pmx = "Growth of RCA in East Midlands 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)

# East of England
locq.growth (rgn_sec_turnover[9:16,] $ turnover2001,
             rgn_sec_turnover[9:16,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[9:16,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in East of England",
             pmx = "Growth of RCA in East of England 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)


# london
locq.growth (rgn_sec_turnover[17:24,] $ turnover2001,
             rgn_sec_turnover[17:24,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[17:24,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in London",
             pmx = "Growth of RCA in London 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)


# North East
locq.growth (rgn_sec_turnover[25:32,] $ turnover2001,
             rgn_sec_turnover[25:32,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[25:32,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in North East",
             pmx = "Growth of RCA in North East 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)
# North West
locq.growth (rgn_sec_turnover[33:40,] $ turnover2001,
             rgn_sec_turnover[33:40,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[33:40,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in North West",
             pmx = "Growth of RCA in North West 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)

# South East
locq.growth (rgn_sec_turnover[41:48,] $ turnover2001,
             rgn_sec_turnover[41:48,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[41:48,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in South East",
             pmx = "Growth of RCA in South East 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)

# South West
locq.growth (rgn_sec_turnover[49:56,] $ turnover2001,
             rgn_sec_turnover[49:56,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[49:56,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in South West",
             pmx = "Growth of RCA in South West 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)

# West Midlands
locq.growth (rgn_sec_turnover[57:64,] $ turnover2001,
             rgn_sec_turnover[57:64,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[57:64,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in West Midlands",
             pmx = "Growth of RCA in West Midlands 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)

# Yorkshire and The Humber
locq.growth (rgn_sec_turnover[65:72,] $ turnover2001,
             rgn_sec_turnover[65:72,] $ turnover2018,
             sector_turnover $ turnover2001,
             sector_turnover $ turnover2018,
             psize = rgn_sec_turnover[65:72,] $ turnover2018 **0.5 , psize.factor = 10,
             y.axis = "n",industry.names = industry,
             pmtitle = "Growth and specialization in Yorkshire and The Humber",
             pmx = "Growth of RCA in Yorkshire and The Humber 2001-2018",
             pmy = "Growth of England 2001-2018 [%]",
             pcol.border = "grey",
             pcol = c("darkgreen", "powderblue", "chocolate", "darkred",
                      "orange", "cadetblue1", "chartreuse1", "red" ),
             leg = TRUE,leg.fsize = 1)

# 3.1 sector performance --------------------------------------------------
# sector - turnover,employment,turnoverPC
sector_turnover = FAME_complete %>%
  group_by(Industry) %>% 
  dplyr::summarise(turnover2001 = sum(Turnover.th.GBP.2001,NA, na.rm = TRUE),
            turnover2002 = sum(Turnover.th.GBP.2002,NA, na.rm = TRUE),
            turnover2003 = sum(Turnover.th.GBP.2003,NA, na.rm = TRUE),
            turnover2004 = sum(Turnover.th.GBP.2004,NA, na.rm = TRUE),
            turnover2005 = sum(Turnover.th.GBP.2005,NA, na.rm = TRUE),
            turnover2006 = sum(Turnover.th.GBP.2006,NA, na.rm = TRUE),
            turnover2007 = sum(Turnover.th.GBP.2007,NA, na.rm = TRUE),
            turnover2008 = sum(Turnover.th.GBP.2008,NA, na.rm = TRUE),
            turnover2009 = sum(Turnover.th.GBP.2009,NA, na.rm = TRUE),
            turnover2010 = sum(Turnover.th.GBP.2010,NA, na.rm = TRUE),
            turnover2011 = sum(Turnover.th.GBP.2011,NA, na.rm = TRUE),
            turnover2012 = sum(Turnover.th.GBP.2012,NA, na.rm = TRUE),
            turnover2013 = sum(Turnover.th.GBP.2013,NA, na.rm = TRUE),
            turnover2014 = sum(Turnover.th.GBP.2014,NA, na.rm = TRUE),
            turnover2015 = sum(Turnover.th.GBP.2015,NA, na.rm = TRUE),
            turnover2016 = sum(Turnover.th.GBP.2016,NA, na.rm = TRUE),
            turnover2017 = sum(Turnover.th.GBP.2017,NA, na.rm = TRUE),
            turnover2018 = sum(Turnover.th.GBP.2018,NA, na.rm = TRUE),
            employee2001 = sum(Number.of.employees.2001,NA, na.rm = TRUE),
            employee2002 = sum(Number.of.employees.2002,NA, na.rm = TRUE),
            employee2003 = sum(Number.of.employees.2003,NA, na.rm = TRUE),
            employee2004 = sum(Number.of.employees.2004,NA, na.rm = TRUE),
            employee2005 = sum(Number.of.employees.2005,NA, na.rm = TRUE),
            employee2006 = sum(Number.of.employees.2006,NA, na.rm = TRUE),
            employee2007 = sum(Number.of.employees.2007,NA, na.rm = TRUE),
            employee2008 = sum(Number.of.employees.2008,NA, na.rm = TRUE),
            employee2009 = sum(Number.of.employees.2009,NA, na.rm = TRUE),
            employee2010 = sum(Number.of.employees.2010,NA, na.rm = TRUE),
            employee2011 = sum(Number.of.employees.2011,NA, na.rm = TRUE),
            employee2012 = sum(Number.of.employees.2012,NA, na.rm = TRUE),
            employee2013 = sum(Number.of.employees.2013,NA, na.rm = TRUE),
            employee2014 = sum(Number.of.employees.2014,NA, na.rm = TRUE),
            employee2015 = sum(Number.of.employees.2015,NA, na.rm = TRUE),
            employee2016 = sum(Number.of.employees.2016,NA, na.rm = TRUE),
            employee2017 = sum(Number.of.employees.2017,NA, na.rm = TRUE),
            employee2018 = sum(Number.of.employees.2018,NA, na.rm = TRUE),
            turnover_pc2001 = mean(Turnover.th.GBP.2001/Number.of.employees.2001,na.rm = TRUE),
            turnover_pc2002 = mean(Turnover.th.GBP.2002/Number.of.employees.2002,na.rm = TRUE),
            turnover_pc2003 = mean(Turnover.th.GBP.2003/Number.of.employees.2003,na.rm = TRUE),
            turnover_pc2004 = mean(Turnover.th.GBP.2004/Number.of.employees.2004,na.rm = TRUE),
            turnover_pc2005 = mean(Turnover.th.GBP.2005/Number.of.employees.2005,na.rm = TRUE),
            turnover_pc2006 = mean(Turnover.th.GBP.2006/Number.of.employees.2006,na.rm = TRUE),
            turnover_pc2007 = mean(Turnover.th.GBP.2007/Number.of.employees.2007,na.rm = TRUE),
            turnover_pc2008 = mean(Turnover.th.GBP.2008/Number.of.employees.2008,na.rm = TRUE),
            turnover_pc2009 = mean(Turnover.th.GBP.2009/Number.of.employees.2009,na.rm = TRUE),
            turnover_pc2010 = mean(Turnover.th.GBP.2010/Number.of.employees.2010,na.rm = TRUE),
            turnover_pc2011 = mean(Turnover.th.GBP.2011/Number.of.employees.2011,na.rm = TRUE),
            turnover_pc2012 = mean(Turnover.th.GBP.2012/Number.of.employees.2012,na.rm = TRUE),
            turnover_pc2013 = mean(Turnover.th.GBP.2013/Number.of.employees.2013,na.rm = TRUE),
            turnover_pc2014 = mean(Turnover.th.GBP.2014/Number.of.employees.2014,na.rm = TRUE),
            turnover_pc2015 = mean(Turnover.th.GBP.2015/Number.of.employees.2015,na.rm = TRUE),
            turnover_pc2016 = mean(Turnover.th.GBP.2016/Number.of.employees.2016,na.rm = TRUE),
            turnover_pc2017 = mean(Turnover.th.GBP.2017/Number.of.employees.2017,na.rm = TRUE),
            turnover_pc2018 = mean(Turnover.th.GBP.2018/Number.of.employees.2018,na.rm = TRUE))

# 3.2 sector Turnover -------------------------------------------------
ggplot(data = sector_turnover[1:19] %>%
         set_colnames(c("Industry",seq(2001,2018,1)))%>%
         melt(id.vars="Industry",
              variable.name="year",
              value.name="turnover"),
       aes(x = year,y = turnover/ 1E+12,
           group = Industry,
           color = Industry)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("")+
  ylab("Turnover value (trillion ￡)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "3. sector turnover.png",
  width = 12,
  height = 6,
  dpi = 1200
)


ggplot(data = cbind(sector_turnover[1],sector_turnover[2:19] %>% mutate_all(~prop.table(.))) %>%
         set_colnames(c('Industry',seq(2001,2018,1)))%>%
         melt(id.vars=c("Industry"),
              variable.name ="year",
              value.name="turnover"), 
       mapping=aes(x= year,y=turnover,fill=Industry,lable=turnover))+
  geom_bar(stat="identity", position="fill")+
  scale_fill_brewer(palette="Spectral")+
  geom_text(aes(label = percent(turnover,accuracy=.1)),size=3,position=position_fill(0.5))+
  theme_bw()+
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  ylab("Share of total turnover")
ggsave(
  "3. sector turnover proportion.png",
  width = 12,
  height = 6,
  dpi = 1200
)

# 3.3 sector employment -------------------------------------------------
ggplot(data = sector_turnover[c(1,20:37)] %>%
         set_colnames(c("Industry",seq(2001,2018,1)))%>%
         melt(id.vars="Industry",
              variable.name="year",
              value.name="turnover"),
       aes(x = year,y = turnover/ 1E+6,
           group = Industry,
           color = Industry)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("")+
  ylab("Number of employees (millions)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(
  "3. sector employment.png",
  width = 12,
  height = 6,
  dpi = 1200
)

ggplot(data = cbind(sector_turnover[1],sector_turnover[20:37] %>% mutate_all(~prop.table(.))) %>%
         set_colnames(c('Industry',seq(2001,2018,1)))%>%
         melt(id.vars=c("Industry"),
              variable.name ="year",
              value.name="turnover"), 
       mapping=aes(x= year,y=turnover,fill=Industry,lable=turnover))+
  geom_bar(stat="identity", position="fill")+
  scale_fill_brewer(palette="Spectral")+
  geom_text(aes(label = percent(turnover,accuracy=.1)),size=3,position=position_fill(0.5))+
  theme_bw()+
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  ylab("Share of total employment")
ggsave(
  "3. sector employment proportion.png",
  width = 12,
  height = 6,
  dpi = 1200
)

# 3.4 sector turnover per employee --------------------------------------
ggplot(data = sector_turnover[c(1,38:55)] %>%
         set_colnames(c("Industry",seq(2001,2018,1)))%>%
         melt(id.vars="Industry",
              variable.name="year",
              value.name="turnover"),
       aes(x = year,y = turnover,
           group = Industry,
           color = Industry)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("")+
  ylab("turnover per employee (￡)")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(
  "3. sector turnover per employee.png",
  width = 12,
  height = 6,
  dpi = 1200
)


# 3.5 sector number of firm -----------------------------------------------

ggplot(data = fame %>% 
         group_by(Industry,year) %>%
         dplyr::summarise(n.of.firm = sum(freq)),
       aes(x = year, y = n.of.firm,
           group = Industry,
           color = Industry)) +
  geom_line(size = 0.5)+
  geom_point(size=2)+
  scale_colour_brewer(palette="Spectral")+
  xlab("")+
  ylab("Number of firms")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(
  "3. sector n.of.firms.png",
  width = 12,
  height = 6,
  dpi = 1200
)
fame %>% glimpse()
fame %>%
  group_by(Industry,year) %>% 
  dplyr::summarise(freq = sum(freq)) %>%   
  spread(year,freq) %>% .[-1] %>%
  mutate_all(~prop.table(.)) 



ggplot(data = cbind(sector_turnover[1],
                    fame %>%
                      group_by(Industry,year) %>% 
                      dplyr::summarise(freq = sum(freq)) %>%   
                      spread(year,freq) %>% .[-1] %>%
                      mutate_all(~prop.table(.)))  %>%
         set_colnames(c('Industry',seq(2001,2018,1)))%>%
         melt(id.vars=c("Industry"),
              variable.name ="year",
              value.name="turnover"), 
       mapping=aes(x= year,y=turnover,fill=Industry,lable=turnover))+
  geom_bar(stat="identity", position="fill")+
  scale_fill_brewer(palette="Spectral")+
  geom_text(aes(label = percent(turnover,accuracy=.1)),size=3,position=position_fill(0.5))+
  theme_bw()+
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = scales::percent)+
  ylab("Share of number of firms")
  
ggsave(
  "3. sector n.of firms proportion.png",
  width = 12,
  height = 6,
  dpi = 1200
)
# 4 speialization ------------------------------------------
# 4.1 number of firms ------------------------------------------
FAME_geo@data%>%
  subset(Company.status == "Active")%>%
  filter(section_description %in% top10_sector$section_description) %>%
  group_by(rgn18nm,section_description)%>%
  count()%>%
  ggplot(aes(x=reorder(rgn18nm,n),
             y=n,
             fill=section_description)) +
  geom_bar(stat="identity", position = "stack")+
  scale_fill_brewer(palette="Spectral")+
  coord_flip()+
  xlab("")+
  ylab("number of firm")+
  theme_bw()

# 4.2 import pacakage "diverse" and "pheatmap" ---------------------------
rgn_sec_turnover = FAME_complete%>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(turnover2001 = sum(Turnover.th.GBP.2001,NA, na.rm = TRUE),
          turnover2002 = sum(Turnover.th.GBP.2002,NA, na.rm = TRUE),
          turnover2003 = sum(Turnover.th.GBP.2003,NA, na.rm = TRUE),
          turnover2004 = sum(Turnover.th.GBP.2004,NA, na.rm = TRUE),
          turnover2005 = sum(Turnover.th.GBP.2005,NA, na.rm = TRUE),
          turnover2006 = sum(Turnover.th.GBP.2006,NA, na.rm = TRUE),
          turnover2007 = sum(Turnover.th.GBP.2007,NA, na.rm = TRUE),
          turnover2008 = sum(Turnover.th.GBP.2008,NA, na.rm = TRUE),
          turnover2009 = sum(Turnover.th.GBP.2009,NA, na.rm = TRUE),
          turnover2010 = sum(Turnover.th.GBP.2010,NA, na.rm = TRUE),
          turnover2011 = sum(Turnover.th.GBP.2011,NA, na.rm = TRUE),
          turnover2012 = sum(Turnover.th.GBP.2012,NA, na.rm = TRUE),
          turnover2013 = sum(Turnover.th.GBP.2013,NA, na.rm = TRUE),
          turnover2014 = sum(Turnover.th.GBP.2014,NA, na.rm = TRUE),
          turnover2015 = sum(Turnover.th.GBP.2015,NA, na.rm = TRUE),
          turnover2016 = sum(Turnover.th.GBP.2016,NA, na.rm = TRUE),
          turnover2017 = sum(Turnover.th.GBP.2017,NA, na.rm = TRUE),
          turnover2018 = sum(Turnover.th.GBP.2018,NA, na.rm = TRUE),
          employee2001 = sum(Number.of.employees.2001,NA, na.rm = TRUE),
          employee2002 = sum(Number.of.employees.2002,NA, na.rm = TRUE),
          employee2003 = sum(Number.of.employees.2003,NA, na.rm = TRUE),
          employee2004 = sum(Number.of.employees.2004,NA, na.rm = TRUE),
          employee2005 = sum(Number.of.employees.2005,NA, na.rm = TRUE),
          employee2006 = sum(Number.of.employees.2006,NA, na.rm = TRUE),
          employee2007 = sum(Number.of.employees.2007,NA, na.rm = TRUE),
          employee2008 = sum(Number.of.employees.2008,NA, na.rm = TRUE),
          employee2009 = sum(Number.of.employees.2009,NA, na.rm = TRUE),
          employee2010 = sum(Number.of.employees.2010,NA, na.rm = TRUE),
          employee2011 = sum(Number.of.employees.2011,NA, na.rm = TRUE),
          employee2012 = sum(Number.of.employees.2012,NA, na.rm = TRUE),
          employee2013 = sum(Number.of.employees.2013,NA, na.rm = TRUE),
          employee2014 = sum(Number.of.employees.2014,NA, na.rm = TRUE),
          employee2015 = sum(Number.of.employees.2015,NA, na.rm = TRUE),
          employee2016 = sum(Number.of.employees.2016,NA, na.rm = TRUE),
          employee2017 = sum(Number.of.employees.2017,NA, na.rm = TRUE),
          employee2018 = sum(Number.of.employees.2018,NA, na.rm = TRUE),
          turnover_pc2001 = mean(Turnover.th.GBP.2001/Number.of.employees.2001,na.rm = TRUE),
          turnover_pc2002 = mean(Turnover.th.GBP.2002/Number.of.employees.2002,na.rm = TRUE),
          turnover_pc2003 = mean(Turnover.th.GBP.2003/Number.of.employees.2003,na.rm = TRUE),
          turnover_pc2004 = mean(Turnover.th.GBP.2004/Number.of.employees.2004,na.rm = TRUE),
          turnover_pc2005 = mean(Turnover.th.GBP.2005/Number.of.employees.2005,na.rm = TRUE),
          turnover_pc2006 = mean(Turnover.th.GBP.2006/Number.of.employees.2006,na.rm = TRUE),
          turnover_pc2007 = mean(Turnover.th.GBP.2007/Number.of.employees.2007,na.rm = TRUE),
          turnover_pc2008 = mean(Turnover.th.GBP.2008/Number.of.employees.2008,na.rm = TRUE),
          turnover_pc2009 = mean(Turnover.th.GBP.2009/Number.of.employees.2009,na.rm = TRUE),
          turnover_pc2010 = mean(Turnover.th.GBP.2010/Number.of.employees.2010,na.rm = TRUE),
          turnover_pc2011 = mean(Turnover.th.GBP.2011/Number.of.employees.2011,na.rm = TRUE),
          turnover_pc2012 = mean(Turnover.th.GBP.2012/Number.of.employees.2012,na.rm = TRUE),
          turnover_pc2013 = mean(Turnover.th.GBP.2013/Number.of.employees.2013,na.rm = TRUE),
          turnover_pc2014 = mean(Turnover.th.GBP.2014/Number.of.employees.2014,na.rm = TRUE),
          turnover_pc2015 = mean(Turnover.th.GBP.2015/Number.of.employees.2015,na.rm = TRUE),
          turnover_pc2016 = mean(Turnover.th.GBP.2016/Number.of.employees.2016,na.rm = TRUE),
          turnover_pc2017 = mean(Turnover.th.GBP.2017/Number.of.employees.2017,na.rm = TRUE),
          turnover_pc2018 = mean(Turnover.th.GBP.2018/Number.of.employees.2018,na.rm = TRUE))
rgn_sec_turnover %>% View()

# 4.2.1 annual turnover matrix-----------------------------------------------------------------
# 2008 turnover
turnover2008 = rgn_sec_turnover %>%
  select(RGN19NM,Industry,turnover2008) %>%
  spread(Industry,turnover2008) %>%
  as.data.frame()
rownames(turnover2008) = unique(rgn_sec_turnover$RGN19NM)
turnover2008 = turnover2008 [,-1]  %>% 
  as.matrix()

pheatmap(values(turnover2008)[names(sort(rowSums(values(turnover2008)), decreasing = TRUE)),
                              names(sort(colSums(values(turnover2008))))], 
         main = "Absolute turnover value(billion pounds) - 2008", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2008,norm = 'p')[names(sort(rowSums(values(turnover2008)), decreasing = TRUE)),
                                         names(sort(colSums(values(turnover2008))))], 
         main = "Regional sectoral propotion - 2008", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2008,norm = 'rca', filter = 1)[names(sort(rowSums(values(turnover2008)), decreasing = TRUE)),
                                                       names(sort(colSums(values(turnover2008))))], 
         main = "RCA(filtered) measured by annual turnover - 2008", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)


# 2009 turnover
turnover2009 = rgn_sec_turnover %>%
  select(RGN19NM,Industry,turnover2009) %>%
  spread(Industry,turnover2009) %>%
  as.data.frame()

rownames(turnover2009) = unique(rgn_sec_turnover$RGN19NM)
turnover2009 = turnover2009 [,-1]  %>% 
  as.matrix()

pheatmap(values(turnover2009)[names(sort(rowSums(values(turnover2009)), decreasing = TRUE)),
                              names(sort(colSums(values(turnover2009))))], 
         main = "Absolute turnover value(billion pounds) - 2009", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2009,norm = 'p')[names(sort(rowSums(values(turnover2009)), decreasing = TRUE)),
                                         names(sort(colSums(values(turnover2009))))], 
         main = "Regional sectoral propotion - 2009", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2009,norm = 'rca', filter = 1)[names(sort(rowSums(values(turnover2009)), decreasing = TRUE)),
                                                       names(sort(colSums(values(turnover2009))))], 
         main = "RCA(filtered) measured by annual turnover - 2009", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
# 2010 turnvover
turnover2010 = rgn_sec_turnover %>%
  select(RGN19NM,Industry,turnover2010) %>%
  spread(Industry,turnover2010) %>%
  as.data.frame()
rownames(turnover2010) = unique(rgn_sec_turnover$RGN19NM)
turnover2010 = turnover2010 [,-1]  %>% 
  as.matrix()

pheatmap(values(turnover2010)[names(sort(rowSums(values(turnover2010)), decreasing = TRUE)),
                              names(sort(colSums(values(turnover2010))))], 
         main = "Absolute turnover value(billion pounds) - 2010", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2010,norm = 'p')[names(sort(rowSums(values(turnover2010)), decreasing = TRUE)),
                                         names(sort(colSums(values(turnover2010))))], 
         main = "Regional sectoral propotion - 2010", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2010,norm = 'rca', filter = 1)[names(sort(rowSums(values(turnover2010)), decreasing = TRUE)),
                                                       names(sort(colSums(values(turnover2010))))], 
         main = "RCA(filtered) measured by annual turnover - 2010", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
# 2014 turnover
turnover2014 = rgn_sec_turnover %>%
  select(RGN19NM,section_description,turnover2014) %>%
  spread(section_description,turnover2014) %>%
  as.data.frame()
rownames(turnover2014) = unique(rgn_sec_turnover$RGN19NM)
turnover2014 = turnover2014 [,-1]  %>% 
  as.matrix()

pheatmap(values(turnover2014)[names(sort(rowSums(values(turnover2014)), decreasing = TRUE)),
                              names(sort(colSums(values(turnover2014))))], 
         main = "Absolute turnover value(billion pounds) - 2014", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2014,norm = 'p')[names(sort(rowSums(values(turnover2014)), decreasing = TRUE)),
                                         names(sort(colSums(values(turnover2014))))], 
         main = "Regional sectoral propotion - 2014", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2014,norm = 'rca', filter = 1)[names(sort(rowSums(values(turnover2014)), decreasing = TRUE)),
                                                       names(sort(colSums(values(turnover2014))))], 
         main = "RCA(filtered) measured by annual turnover - 2014", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
# 2015 turnover
turnover2015 = rgn_sec_turnover %>%
  select(RGN19NM,Industry,turnover2015) %>%
  spread(Industry,turnover2015) %>%
  as.data.frame()
rownames(turnover2015) = unique(rgn_sec_turnover$RGN19NM)
turnover2015 = turnover2015 [,-1]  %>% 
  as.matrix()

pheatmap(values(turnover2015)[names(sort(rowSums(values(turnover2015)), decreasing = TRUE)),
                              names(sort(colSums(values(turnover2015))))], 
         main = "Absolute turnover value - 2015", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2015,norm = 'p')[names(sort(rowSums(values(turnover2015)), decreasing = TRUE)),
                                         names(sort(colSums(values(turnover2015))))], 
         main = "Regional sectoral propotion - 2015", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2015,norm = 'rca', filter = 1)[names(sort(rowSums(values(turnover2015)), decreasing = TRUE)),
                                                       names(sort(colSums(values(turnover2015))))], 
         main = "RCA(filtered) measured by annual turnover - 2015", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
# 2016 turnover
turnover2016 = rgn_sec_turnover %>%
  select(RGN19NM,section_description,turnover2016) %>%
  spread(section_description,turnover2016) %>%
  as.data.frame()
rownames(turnover2016) = unique(rgn_sec_turnover$RGN19NM)
turnover2016 = turnover2016 [,-1]  %>% 
  as.matrix()

pheatmap(values(turnover2016)[names(sort(rowSums(values(turnover2016)), decreasing = TRUE)),
                                             names(sort(colSums(values(turnover2016))))], 
         main = "Absolute turnover value - 2016", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2016,norm = 'p')[names(sort(rowSums(values(turnover2016)), decreasing = TRUE)),
                                         names(sort(colSums(values(turnover2016))))], 
         main = "Regional sectoral propotion - 2016", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)
pheatmap(values(turnover2016,norm = 'rca', filter = 1)[names(sort(rowSums(values(turnover2016)), decreasing = TRUE)),
                                                       names(sort(colSums(values(turnover2016))))], 
         main = "RCA(filtered) measured by annual turnover - 2016", 
         legend = TRUE, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)


# 4.3 diversity - turnover ------------------------------------------------------------
# diversity and entropy
diversity2001 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2001,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2002 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2002,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2003 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2003,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2004 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2004,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2005 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2005,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2006 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2006,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2007 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2007,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2008 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2008,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2009 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2009,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2010 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2010,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2011 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2011,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2012 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2012,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2013 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2013,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2014 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2014,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2015 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2015,na.rm = TRUE))%>%
  as.data.frame()%>%
  diverse::diversity()%>%
  round(3)

diversity2016 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2016,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2017 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2017,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2018 = FAME_complete%>%
  group_by(RGN19NM,Industry)%>%
  dplyr::summarise(turnover = sum(Turnover.th.GBP.2018,na.rm = TRUE))%>%
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)




# 4.4 diversity - n.firm ---------------------------------------------------
diversity2001_n = fame %>% 
  filter(year == "2001") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2002_n = fame %>% 
  filter(year == "2002") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2003_n = fame %>% 
  filter(year == "2003") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2004_n = fame %>% 
  filter(year == "2004") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2005_n = fame %>% 
  filter(year == "2005") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2006_n = fame %>% 
  filter(year == "2006") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2007_n = fame %>% 
  filter(year == "2007") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2008_n = fame %>% 
  filter(year == "2008") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2009_n = fame %>% 
  filter(year == "2009") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2010_n = fame %>% 
  filter(year == "2010") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2011_n = fame %>% 
  filter(year == "2011") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2012_n = fame %>% 
  filter(year == "2012") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2013_n = fame %>% 
  filter(year == "2013") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2014_n = fame %>% 
  filter(year == "2014") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2015_n = fame %>% 
  filter(year == "2015") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2016_n = fame %>% 
  filter(year == "2016") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2017_n = fame %>% 
  filter(year == "2017") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)

diversity2018_n = fame %>% 
  filter(year == "2018") %>%
  group_by(RGN19NM,Industry) %>%
  dplyr::summarise(n.of.firm = sum(freq)) %>% 
  as.data.frame()%>% 
  diverse::diversity()%>%
  round(3)


# 4.4.1 HHI - n.of.firm -----------------------------------------------------
HHI_all_n = rbind(diversity2001_n$HHI,diversity2002_n$HHI,diversity2003_n$HHI,diversity2004_n$HHI,
                diversity2005_n$HHI,diversity2006_n$HHI,diversity2007_n$HHI,diversity2008_n$HHI,
                diversity2009_n$HHI,diversity2010_n$HHI,diversity2011_n$HHI,diversity2012_n$HHI,
                diversity2013_n$HHI,diversity2014_n$HHI,diversity2015_n$HHI,diversity2016_n$HHI,
                diversity2017_n$HHI,diversity2018_n$HHI)%>% data.frame()

colnames(HHI_all_n) = rownames(diversity2018_n)
HHI_all_n$year = seq(2001,2018)

HHI_long_n = melt(HHI_all_n,
                id.vars = c('year'),
                measure.vars = c(as.character(rownames(diversity2018))),
                variable.name='region',
                value.name='HHI')

ggplot(data = HHI_long_n, mapping = aes(x = year, y = HHI, colour = region)) +
  scale_x_continuous(breaks=seq(2001, 2018, 1)) +
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Spectral")+
  #ggtitle("HHI by region - 2001:2018") +
  theme_bw()+theme(legend.position = "bottom")

ggsave(
  "4. HHI_n.png",
  width = 8,
  height = 6,
  dpi = 1200
)

# 4.4.1 HHI - turnover-----------------------------------------------------------------
HHI_all = rbind(diversity2001$HHI,diversity2002$HHI,diversity2003$HHI,diversity2004$HHI,
                diversity2005$HHI,diversity2006$HHI,diversity2007$HHI,diversity2008$HHI,
                diversity2009$HHI,diversity2010$HHI,diversity2011$HHI,diversity2012$HHI,
                diversity2013$HHI,diversity2014$HHI,diversity2015$HHI,diversity2016$HHI,
                diversity2017$HHI,diversity2018$HHI)%>% data.frame()

colnames(HHI_all) = rownames(diversity2018)
HHI_all$year = seq(2001,2018)

HHI_long = melt(HHI_all,
                id.vars = c('year'),
                measure.vars = c(as.character(rownames(diversity2018))),
                variable.name='region',
                value.name='HHI')

ggplot(data = HHI_long, mapping = aes(x = year, y = HHI, colour = region)) +
  scale_x_continuous(breaks=seq(2001, 2018, 1)) +
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Spectral")+
  ggtitle("HHI by region - 2001:2018") +
    theme_bw()+theme(legend.position = "bottom")

ggsave(
  "4. HHI.png",
  width = 8,
  height = 6,
  dpi = 1200
)

regionmap$LOGturnover2018 = log(regionmap$turnover2018)

plot(merge(regionmap  %>% st_drop_geometry() %>% select(rgn18nm,LOGturnover2018),
           HHI_long %>% filter(year == "2018") %>% select(-year),
           by.x = "rgn18nm" , by.y= "region") %>% 
       select(HHI,LOGturnover2018),xlab = "HHI of regions in 2018",ylab="Turnover in 2018 (log transformed)") 

model=lm(LOGturnover2018~HHI,data=merge(regionmap  %>% st_drop_geometry() %>% select(rgn18nm,LOGturnover2018),
                                 HHI_long %>% filter(year == "2018") %>% select(-year),
                                 by.x = "rgn18nm" , by.y= "region") %>% 
           select(HHI,LOGturnover2018))
abline(model,lty=2)
summary(model)


# 4.4.2 blau.index---------------------------------------------------------
blau.index_all = rbind(diversity2001$blau.index,diversity2002$blau.index,diversity2003$blau.index,diversity2004$blau.index,
                       diversity2005$blau.index,diversity2006$blau.index,diversity2007$blau.index,diversity2008$blau.index,
                       diversity2009$blau.index,diversity2010$blau.index,diversity2011$blau.index,diversity2012$blau.index,
                       diversity2013$blau.index,diversity2014$blau.index,diversity2015$blau.index,diversity2016$blau.index,
                       diversity2017$blau.index,diversity2018$blau.index,diversity2019$blau.index)%>% data.frame()

colnames(blau.index_all) = rownames(diversity2019)
blau.index_all$year = seq(2001,2019)

blau.index_long = melt(blau.index_all,
                id.vars = c('year'),
                measure.vars = c(as.character(rownames(diversity2019))),
                variable.name='region',
                value.name='value')

ggplot(data = blau.index_long, mapping = aes(x = year, y = value, colour = region)) +
  scale_x_continuous(breaks=seq(2001, 2019, 1)) +
  geom_line() + 
  geom_point() +
  ggtitle("blau.index") +
  theme_few() + 
  scale_colour_few()

# 4.4.3 evenness ------------------------------------------------------
evenness_all = rbind(diversity2001$evenness,diversity2002$evenness,diversity2003$evenness,diversity2004$evenness,
                     diversity2005$evenness,diversity2006$evenness,diversity2007$evenness,diversity2008$evenness,
                     diversity2009$evenness,diversity2010$evenness,diversity2011$evenness,diversity2012$evenness,
                     diversity2013$evenness,diversity2014$evenness,diversity2015$evenness,diversity2016$evenness,
                     diversity2017$evenness,diversity2018$evenness)%>% data.frame()

colnames(evenness_all) = rownames(diversity2018)
evenness_all$year = seq(2001,2018)

evenness_long = melt(evenness_all,
                       id.vars = c('year'),
                       measure.vars = c(as.character(rownames(diversity2019))),
                       variable.name='region',
                       value.name='evenness')
ggplot(data = evenness_long, mapping = aes(x = year, y = evenness, colour = region)) +
  scale_x_continuous(breaks=seq(2001, 2018, 1)) +
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Spectral")+
  ggtitle("Evenness by region - 2001:2018") +
  theme_bw()


# 4.4.4 entropy ----------------------------------------------------------
entropy_all = rbind(diversity2001$entropy,diversity2002$entropy,diversity2003$entropy,diversity2004$entropy,
                     diversity2005$entropy,diversity2006$entropy,diversity2007$entropy,diversity2008$entropy,
                     diversity2009$entropy,diversity2010$entropy,diversity2011$entropy,diversity2012$entropy,
                     diversity2013$entropy,diversity2014$entropy,diversity2015$entropy,diversity2016$entropy,
                     diversity2017$entropy,diversity2018$entropy)%>% data.frame()

colnames(entropy_all) = rownames(diversity2018)
entropy_all$year = seq(2001,2018)

entropy_long = melt(entropy_all,
                     id.vars = c('year'),
                     measure.vars = c(as.character(rownames(diversity2001))),
                     variable.name='region',
                     value.name='entropy')

ggplot(data = entropy_long, mapping = aes(x = year, y = entropy, colour = region)) +
  scale_x_continuous(breaks=seq(2001, 2018, 1)) +
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Spectral")+
  ggtitle("Entropy by region - 2001:2018") +
  theme_bw()+theme(legend.position = "bottom")

ggsave(
  "4. Entropy.png",
  width = 8,
  height = 6,
  dpi = 1200
)
# 4.4.5 entropy ----------------------------------------------------------
entropy_all_n = rbind(diversity2001_n$entropy,diversity2002_n$entropy,diversity2003_n$entropy,diversity2004_n$entropy,
                    diversity2005_n$entropy,diversity2006_n$entropy,diversity2007_n$entropy,diversity2008_n$entropy,
                    diversity2009_n$entropy,diversity2010_n$entropy,diversity2011_n$entropy,diversity2012_n$entropy,
                    diversity2013_n$entropy,diversity2014_n$entropy,diversity2015_n$entropy,diversity2016_n$entropy,
                    diversity2017_n$entropy,diversity2018_n$entropy)%>% data.frame()

colnames(entropy_all_n) = rownames(diversity2018)
entropy_all_n$year = seq(2001,2018)

entropy_long_n = melt(entropy_all_n,
                    id.vars = c('year'),
                    measure.vars = c(as.character(rownames(diversity2001))),
                    variable.name='region',
                    value.name='entropy')

ggplot(data = entropy_long_n, mapping = aes(x = year, y = entropy, colour = region)) +
  scale_x_continuous(breaks=seq(2001, 2018, 1)) +
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Spectral")+
  #ggtitle("Entropy by region - 2001:2018") +
  theme_bw()+theme(legend.position = "bottom")

ggsave(
  "4. Entropy_n.png",
  width = 8,
  height = 6,
  dpi = 1200
)

# 5. branches ---------------------------------------------------------------


branches_info = test2_r@data %>%
  group_by(id,rgn18cd,rgn18nm)%>%
  summarise(employees = sum(employees,NA,na.rm = TRUE),firm=n())
names(branches_info) = c( "firmID","branch.rgn18cd","branch.rgn18nm","branch.employees","branch.num")

# aggregate the ori- and des- data
network = FAME_complete@data[["BvD.ID.number"]] %>%data.frame()
network$firmlad17cd = test@data$lad17cd
network$firmlad17nm = test@data$lad17nm
names(network) = c("firmID","firm.lad17cd","firm.lad17nm")

network_r = test_r@data[["BvD.ID.number"]] %>%data.frame()
network_r$firm.rng18cd = test_r$rgn18cd
network_r$firm.rng18nm = test_r$rgn18nm
network_r$sector = test_r$section_description
names(network_r) = c("firmID","firm.rng18cd","firm.rng18nm","sector")

# extract useful columns for plotting
network_plot = network%>%
  merge(branches_info,by="firmID")%>%
  select(firm.lad17nm,branch.lad17nm,branch.employees)%>%
  drop_na()
names(network_plot) = c("origin", "destination","total")

network_r_plot = network_r%>%
  merge(branches_info,by="firmID")%>%
  group_by(firm.rng18nm, sector,branch.rgn18nm)%>%
  summarise(branch.employees = sum(branch.employees,NA,na.rm = TRUE))%>%
  select(firm.rng18nm, sector,branch.rgn18nm, branch.employees)%>%
  drop_na()
names(network_r_plot) = c("origin","sector","destination","total")
network_r_plot = network_r_plot[network_r_plot$origin!=network_r_plot$destination,]


arrange(network_r_plot,desc(total)) %>% View()
# network analysis from "igragh" 
network_r_g = graph_from_data_frame(network_r_plot,directed = T)
E(network_r_g)$width = network_r_plot$total/1000

plot(network_r_g, edge.width = E(network_r_g)$width)
# sankey
ggplot(network_r_plot,
       aes(axis1 = origin,
           axis2 = sector,
           axis3 = destination,
           y= total)) +
  scale_x_discrete(limits = c("origin", "sector","destination"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill=origin)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal()+
  ggsave("sankey2001.jpg",width = 20, height = 20)

#+
  ggtitle("Primary firm - branch",
          "Employee 2001")




# 6. network GUO ----------------------------------------------------------
glimpse(FAME_geo@data)
  
citymap = readOGR("D:/OneDrive - University College London/CASA0004_Dissertation/DataProcessing/Shapefiles/Major_Towns_and_Cities__December_2015__Boundaries.shp") 
proj4string(citymap) = CRS("+init=epsg:4326")

citymap = spTransform(citymap, CRS("+init=epsg:4326"))
# 6.1 domestic ----------------------------------------------------------------
# turn the % into number
FAME_geo@data$GUO...Direct.. = as.numeric(FAME_geo@data$GUO...Direct..)
FAME_geo@data$GUO...Total.. = as.numeric(FAME_geo@data$GUO...Total..)  

# turnoverflow = turnover*percentage
FAME_geo@data$turnoverflow = FAME_geo@data$Turnover.th.GBP.2018 * FAME_geo@data$GUO...Direct.. / 100


# 6.1.1 make a network map ------------------------------------------------------
# make a network dataframe
network_GUO_CITY_inner = FAME_geo@data%>%
  subset(Company.status == "Active")%>%
  subset(GUO...Country.ISO.code == "GB")%>%
  drop_na(turnoverflow) %>%
  group_by(GUO...City , R.O.City) %>%
  summarise(turnover = sum(turnoverflow))%>%
  drop_na()%>%
  as.data.frame()

# formatting
network_GUO_CITY_inner$turnover = round(network_GUO_CITY_inner$turnover,0)
network_GUO_CITY_inner$GUO...City = str_to_title(network_GUO_CITY_inner$GUO...City) 

network_GUO_CITY_inner$R.O.City = lapply(network_GUO_CITY_inner$R.O.City, function(i) {
  strsplit(i,",")[[1]][1]
  strsplit(i,' \\(')[[1]][1]
  str_squish(i)
  }) %>% as.character()

network_GUO_CITY_inner$GUO...City = lapply(network_GUO_CITY_inner$GUO...City, function(i) {
  strsplit(i,",")[[1]][1]
  strsplit(i,' \\(')[[1]][1]
  str_squish(i)
  }) %>% as.character()

# drop self-loop
glimpse(network_GUO_CITY_inner)
network_GUO_CITY_inner = network_GUO_CITY_inner[network_GUO_CITY_inner$R.O.City!=network_GUO_CITY_inner$GUO...City,]

# 6.1.2 how to draw a flow map -------------------------------------------------
ukcity_nodes = getSpPPolygonsLabptSlots(citymap) %>% 
  as.data.frame() %>%
  set_colnames(c("lon","lat"))

ukcity_nodes$tcity15nm = citymap$tcity15nm

coordinates(ukcity_nodes) = ~lon + lat
proj4string(ukcity_nodes) = CRS("+init=epsg:4326")

ukcity_nodes = point.in.poly(ukcity_nodes,regionmap)

# yeah!
networkGUO_inner_for_plot = merge(network_GUO_CITY_inner,ukcity_nodes@data, by.x = "GUO...City", by.y = "tcity15nm")
networkGUO_inner_for_plot = merge(networkGUO_inner_for_plot,ukcity_nodes@data, by.x = "R.O.City", by.y = "tcity15nm")

networkGUO_inner_for_plot = networkGUO_inner_for_plot[networkGUO_inner_for_plot$rgn18nm.x!=networkGUO_inner_for_plot$rgn18nm.y,]

networkGUO_inner_for_plot = networkGUO_inner_for_plot %>%
  select(rgn18nm.x,rgn18nm.y,turnover,long.x,lat.x,long.y,lat.y)%>%
  group_by(rgn18nm.x,rgn18nm.y,long.x,lat.x,long.y,lat.y)%>%
  summarise(turnover = sum(turnover))%>%
  set_colnames(c('rgn18nm.x','rgn18nm.y','x','y','xend','yend','turnover'))

networkGUO_inner_for_plot$xend = networkGUO_inner_for_plot$xend* 0.9999999999
networkGUO_inner_for_plot$yend = networkGUO_inner_for_plot$yend* 0.9999999999


maptheme <- theme(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  legend.position = "bottom",
  panel.background = element_rect(fill="white"),
  plot.margin = unit(c(0,0,0.5,0),"cm")
)

ggplot()+
  geom_sf(data= regionmap)+
  geom_curve(aes(x=x,y=y,xend=xend,yend=yend,size=turnover),
             data = networkGUO_inner_for_plot,
             curvature = 0.5, alpha=1,color = "#48AAAD", arrow.fill = "white",
             arrow = arrow(type = "closed"))+
  scale_fill_continuous(type = "gradient")+
  scale_size_continuous(range = c(0.5,2.5))+  # scale for edge widths
  geom_point(data = regionmap,
             aes(x=long,y=lat),  # draw nodes
             shape=21,fill="white",color="black",stroke=0.5)+
  #scale_size_continuous(guide = FALSE, range = c(1,6))+ # scale for node size
  geom_text_repel(data = regionmap,aes(x=long,y=lat,label=rgn18nm), # draw text labels
            size=5,color="black",fontface="bold")+maptheme





# 6.1.3 region igraph -----------------------------------------------------------

# drop self-loop
networkGUO_inner_for_plot = networkGUO_inner_for_plot[networkGUO_inner_for_plot$rgn18nm.x!=networkGUO_inner_for_plot$rgn18nm.y,]

network_GUO_region = networkGUO_inner_for_plot %>%
  select(rgn18nm.x,rgn18nm.y,turnover)  %>%
  graph_from_data_frame(directed = T)

E(network_GUO_region)$width = networkGUO_inner_for_plot$turnover**0.15
V(network_GUO_region)$degree = degree(network_GUO_region)*8**0.5
vertex_attr(network_GUO_region)

curve.reciprocal.edges <- function(g, curve=.3){
  # Return a graph where the edge-attribute $curved is reset to highlight reciprocal edges
  el <- t(apply(get.edgelist(g),1,sort))
  E(g)$curved <- 0
  E(g)[duplicated(el) | duplicated(el,fromLast =TRUE)]$curved <- curve
  (g)
}

plot(curve.reciprocal.edges(network_GUO_region), 
     vertex.color = rgb(0.8,0.4,0.3,0.8),
     vertex.frame.color = "black",
     vertex.label.dist=1.5,
     vertex.size=V(network_GUO_region)$degree,
     edge.width = E(network_GUO_region)$width,
     edge.arrow.size=1)


# 6.2 international -----------------------------------------------------------
world = ne_countries(scale = "medium", returnclass = "sf") %>%
  select(admin,iso_a2)


  
st_crs(world) = CRS("+init=epsg:4326")

st_centroid(world)

st_coordinates(st_centroid(world))

st_drop_geometry()
# turnoverflow = turnover*percentage
FAME_geo@data$turnoverflow = FAME_geo@data$Turnover.th.GBP.2018 * FAME_geo@data$GUO...Direct.. / 100

# make a network dataframe
network_GUO_CITY_international = FAME_geo@data%>%
  subset(Company.status == "Active")%>%
  drop_na(GUO...Country.ISO.code) %>%
  subset(GUO...Country.ISO.code != "GB")%>%
  subset(GUO...Country.ISO.code != "-") %>%
  drop_na(turnoverflow) %>%
  group_by(GUO...Country.ISO.code , R.O.City) %>%
  summarise(turnover = sum(turnoverflow))%>%
  subset(turnover != 0)
  as.data.frame()


# formatting
network_GUO_CITY_international$R.O.City = lapply(network_GUO_CITY_international$R.O.City, function(i) {
  strsplit(i,",")[[1]][1]
  strsplit(i,' \\(')[[1]][1]
  str_squish(i)
}) %>% as.character()

network_GUO_CITY_international = merge(network_GUO_CITY_international,world%>%st_drop_geometry(),
      by.x = "GUO...Country.ISO.code",by.y = "iso_a2")

network_GUO_CITY_international = merge(network_GUO_CITY_international,ukcity_nodes@data, by.x = "R.O.City", by.y = "tcity15nm")

network_GUO_CITY_international = network_GUO_CITY_international %>%
  #select(admin,rgn18nm,turnover,long,lat)%>%
  group_by(admin,rgn18nm)%>%
  summarise(turnover = sum(turnover))

# drop self-loop
network_GUO_CITY_international_g = 
  graph_from_data_frame(network_GUO_CITY_international[which(network_GUO_CITY_international$turnover>500000),]%>%
                          select(admin,rgn18nm,turnover),directed = T)


vattributes = merge(vertex.attributes(network_GUO_CITY_international_g) %>% as.data.frame(),
      countrycode::codelist %>% select(country.name.en,continent),
      by.x = "name",
      by.y = "country.name.en", all.x = TRUE)

vattributes$color = c("Americas","Europe","Americas","Europe",
                      "Europe","Europe","Asia","Europe",
                      "Europe","Asia","Europe","Americas",
                      "Europe","Africa","Europe","Europe",
                      "Asia","Americas","UK","UK",
                      "UK","UK","UK") %>% as.factor()
vattributes$shape = c("square","square","square","square",
                      "square","square","square","square",
                      "square","square","square","square",
                      "square","square","square","square",
                      "square","square","circle","circle",
                      "circle","circle","circle") %>% as.factor()


plot(network_GUO_CITY_international_g,
     vertex.color = vattributes$color,
     #vertex.shape = vattributes$shape,
     #vertex.size = degree(network_GUO_CITY_international_g),
     edge.arrow.size=1.5,
     edge.width = E(network_GUO_CITY_international_g)$turnover ** 0.5 / 200)


