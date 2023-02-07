## code to prepare `covid_sa` dataset

library(dplyr)
library(lubridate)

#Link to data
data_url <- 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_confirmed.csv'

#Read the data in using the url
covid19_sa <- read.csv(data_url)

#Clean and subset the data we need
covid19_sa <- covid19_sa %>% 
  dplyr::select(date) %>% 
  dplyr::mutate(date = lubridate::dmy(date)) %>%
  dplyr::filter(date <= lubridate::dmy('20-03-2020')) %>%   
  dplyr::group_by(date) %>% 
  dplyr::summarise(cases = n()) %>%   
  dplyr::ungroup()

usethis::use_data(covid19_sa, overwrite = TRUE)
