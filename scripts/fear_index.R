library(lubridate)
library(ggplot2)
library(dplyr)
# get dataset
# csv_dir = '/Users/ytf1997/Desktop/AnE_data/csv'
# hkCasesCsvPath <- file.path(csv_dir , "covid_cases_daily.csv")
# hkCasesCsv <- read.csv(hkCasesCsvPath)
# hkCasesCsv = hkCasesCsv %>% mutate(eventdate=dmy(eventdate))
# hkCasesCsv = hkCasesCsv %>% filter(year(eventdate)==2020)

# 0/0 
pandemic = seq(as.Date("2020-01-25"),as.Date("2020-12-31"),by="days")
wave1 = seq(as.Date("2020-01-25"),as.Date("2020-03-02"),by="days")
# 01-25 HKHA activated ERI due to COVID
# 01-29 First day of complete 28d mortality on dead date data starting from 2019
# 03-02 Resumption of government services with social distancing protocol in place

wave2 = seq(as.Date("2020-03-17"),as.Date("2020-05-04"),by="days")
# 03-17 HKG banned entry from HKIA for non-HK residents
# 05-04 resumption of government services, WFH order lifted for govt officials

wave3 = seq(as.Date("2020-07-09"),as.Date("2020-08-28"),by="days")
# 07-09 tightening gathering restrictions
# 08-28 relaxing gathering/opening restrictions and mask mandate

wave4 = seq(as.Date("2020-11-14"),as.Date("2020-12-31"),by="days")
# 11-14 restricting social distancing, restaurant sitting
# 12-31 end of 2021
# reopen 10 types of merchants, including gym and cinema

wave5 = seq(as.Date("2021-01-01"),as.Date("2021-02-18"),by="days")
# 01-01 first day of 2021 
# 02-18 resumption of govt services https://www.news.gov.hk/chi/2021/02/20210210/20210210_175437_601.html
# 02-18 relaxation of gathering restriction back to 4-ppl, restaurant allowed opening hrs extended to 2200,
# reopen 10 types of merchants, including gym and cinema


waveDays=c(wave1,wave2,wave3,wave4)
waveDays.2019 = ymd(format(waveDays,"2019-%m-%d"))
waveDays.2019 = na.omit(waveDays.2019) 
waves = list("1"=wave1,"2"=wave2,"3"=wave3,"4"=wave4,"5"=wave5)
wavePandemicHkIndex = seq(as.Date("2019-01-01"), as.Date("2021-04-15"), by="days")
wavePandemicHkIndex = as_tibble(wavePandemicHkIndex) %>% 
  rename(eventdate=value) %>%
  filter(eventdate!=as.Date("2020-02-29"))
# wavePandemicHkIndex = wavePandemicHkIndex %>% mutate(wave=ifelse(eventdate %in% waveDays,1,0))

wavePandemicHkIndex = wavePandemicHkIndex %>% 
  mutate(waveFactor = case_when(
    (eventdate >= wave1[1] & eventdate <= wave4[length(wave4)] & !eventdate %in% waveDays) ~ "inter-wave",
    eventdate %in% wave1 ~ "1",
    eventdate %in% wave2 ~ "2",
    eventdate %in% wave3 ~ "3",
    eventdate %in% wave4 ~ "4",
    eventdate %in% wave5 ~ "5",
    TRUE ~ "0"
  )) %>%
  mutate(waveFactor=as.factor(waveFactor)) 

wavePandemicHkIndex.1920 = bind_rows(wavePandemicHkIndex %>% 
                                         filter(year(eventdate)==2020) %>%
                                         mutate(eventdate = ymd(format(eventdate, "2019-%m-%d"))),
                                       wavePandemicHkIndex %>% 
                                         filter(year(eventdate)==2020))

# ggplot(data=wavePandemicHkIndex) +
#   geom_rect(aes(xmin=yday(eventdate)-.5, xmax=yday(eventdate)+.5, ymin=0, ymax=Inf,
#                 fill= !waveFactor %in% c(0,"inter-wave")),alpha=0.3) +
#   labs(x="Calendar Day in 2020", y="Number of COVID case", title="COVID cases with wave designation") +
#   theme(legend.position='none') +
#   facet_wrap(~year(eventdate))
