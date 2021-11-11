library(dataRetrieval)
library(readr)
library(readxl)
library(dplyr)
library(here)
library(janitor)

#parameter codes

#00608 = Ammonium + Ammonia
#00613 = Nitrite
#00631 = Nitrate + Nitrite
#00671 = Ortho-Phosphate 
#62854 = Total Dissolved Nitrogen 
#70953 = Chl-a  
#62360 = Pheophytin 
#00681 = Dissolved Organic Carbon 
#50624 = Absorbance at 254 nm
#63162 = DOC-normalizaed UV absorbance
#80154 = Suspended Sediment Concentration

parameterCd <-c("00608", "00613", "00631", "00671", "62854", "70953", "62360", "00681", "50624", "80154", "63162")

#USGS site numbers - had trouble with readWQPqw reading station list
#refer to USGS_CAWSC_stations.xlsx for station name, abbreviation, latitude and longitude

#readWQPqw will replace readNWISqw

disc <- readWQPqw(siteNumbers=c('USGS-11455350',
                  'USGS-11455385',
                  'USGS-11455508',
                  'USGS-11455338',
                  'USGS-11336600',
                  'USGS-11313433',
                  'USGS-11455478',
                  'USGS-11312968',
                  'USGS-11455136',
                  'USGS-11455095',
                  'USGS-11455335',
                  'USGS-11455142',
                  'USGS-11180770',
                  'USGS-11313440',
                  'USGS-383019121350701',
                  'USGS-381614121415301',
                  'USGS-381142122015801',
                  'USGS-11447650',
                  'USGS-11447903',
                  'USGS-11313240',
                  'USGS-380631122032201',
                  'USGS-11313431',
                  'USGS-11337190',
                  'USGS-11455146',
                  'USGS-11455143',
                  'USGS-11455315',
                  'USGS-382006121401601',
                  'USGS-382010121402301',
                  'USGS-11336790',
                  'USGS-11312676',
                  'USGS-11455165',
                  'USGS-11336930',
                  'USGS-11312685',
                  'USGS-380833122033401',
                  'USGS-11336685',
                  'USGS-11313405',
                  'USGS-11313315',
                  'USGS-11313434',
                  'USGS-11313452',
                  'USGS-381424121405601',
                  'USGS-11313460',
                  'USGS-11455166',
                  'USGS-11455167',
                  'USGS-11455420',
                  'USGS-373751122143601',
                  'USGS-11455276',
                  'USGS-11303500',
                  'USGS-11336680',
                  'USGS-11304810',
                  'USGS-11447850',
                  'USGS-11447830',
                  'USGS-11337080',
                  'USGS-11455139',
                  'USGS-11455140',
                  'USGS-11455485',
                  'USGS-11311300',
                  'USGS-11455280',
                  'USGS-11312672',
                  'USGS-11447890',
                  'USGS-11447905')
                  , parameterCd, startDate = "2010-10-01", endDate = "", tz = 'UTC', querySummary = FALSE)

write.csv(disc, "C:/Users/estumpne/Documents/R/drought/data_pull_110921.csv" )

getwd()

disc_select <-disc %>%
  select("OrganizationFormalName", 
         "MonitoringLocationIdentifier", 
         "ActivityStartDate", "ActivityStartTime.Time", 
         "ActivityStartTime.TimeZoneCode", 
         "CharacteristicName", 
         "ResultMeasureValue",
         "ResultMeasure.MeasureUnitCode",
         "ResultStatusIdentifier",
         "ResultAnalyticalMethod.MethodName",
         "DetectionQuantitationLimitTypeName",
         "DetectionQuantitationLimitMeasure.MeasureValue",
         "DetectionQuantitationLimitMeasure.MeasureUnitCode"
         )

disc_rename <- clean_names(disc_select)

# select approved data only...if you want 

disc_approved <- subset(disc_rename, result_status_identifier == "Accepted")

write_csv(disc_approved, "C:/Users/estumpne/Documents/R/drought/USGS_CAWSC_approved_2011_2021_readWQPqw.csv")

