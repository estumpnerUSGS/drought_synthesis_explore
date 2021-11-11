#data exploration

library(tidyverse)

disc_chla2 <- disc_chla %>%
  dplyr::rename(dates = activity_start_date)



#add water year

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  return(adj.year)
}
?addWate

disc_chla_wy <- addWaterYear(disc_chla2)
wtr_yr(disc_chla2)

# look at Liberty

disc_SSC <- disc_rename %>% filter(characteristic_name == "Suspended Sediment Concentration (SSC)")

disc_SSC %>% filter(monitoring_location_identifier == "USGS-11455315") %>%
  ggplot() +
  geom_line(aes(x=activity_start_date, y=result_measure_value))

disc_SSC_Lib <- disc_SSC %>% filter(monitoring_location_identifier == "USGS-11455350")

summary(disc_SSC_Lib$result_measure_value)

# filter to post 1994
df_cl %>%
  filter(Station == "USGS 36",
         Date > lubridate::ymd("1994-10-01")) %>%
  #View()
  ggplot() +
  geom_line(aes(x=Date, y=Chlorophyll))