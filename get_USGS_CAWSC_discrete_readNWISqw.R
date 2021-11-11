library(dataRetrieval)
library(readr)
library(readxl)
library(dplyr)
library(here)

startDate <- "2010-10-01"

endDate <- ""

parameterCd = c("00608", "00613", "00631", "00671", "62854", "70953", "62360", "00681", "50624", "63162")

# USGS discrete stations

USGS_CAWSC_sites <- read_xlsx("USGS_CAWSC_stations.xlsx")

siteNumber <- (USGS_CAWSC_sites$siteNumber)

#parameter codes  

#00608 = NH4 wf, 00613 = NO2 wf, 00631 = NO3+NO2 wf, 00671 = Ortho-PO4 wf, 62854 = TDN wf, 70953 = Chl-a, 62360 = Pheophytin, 00681 = DOC, 50624 = Abs 254 nm, 63162 = SUVA

#siteNumber <- site_list

#retrieve discrete USGS CAWSC  

disc <- readNWISqw(siteNumber, parameterCd, startDate, endDate, expanded=TRUE, reshape=TRUE)

#select columns

disc_select <-disc %>%
  select(startDateTime,tz_cd, site_no, remark_cd_00608, result_va_00608, dqi_cd_00608, rpt_lev_va_00608, rpt_lev_cd_00608, remark_cd_00613, result_va_00613, dqi_cd_00613, rpt_lev_va_00613, rpt_lev_cd_00613, remark_cd_00631, result_va_00631, dqi_cd_00631, rpt_lev_va_00631, rpt_lev_cd_00631, remark_cd_00671, result_va_00671, dqi_cd_00671, rpt_lev_va_00671, rpt_lev_cd_00671, remark_cd_62854, result_va_62854, dqi_cd_62854, rpt_lev_va_62854, rpt_lev_cd_62854, remark_cd_70953, result_va_70953, dqi_cd_70953, rpt_lev_va_70953, rpt_lev_cd_70953, remark_cd_62360, result_va_62360, dqi_cd_62360, rpt_lev_va_62360, rpt_lev_cd_62360, remark_cd_00681, result_va_00681, dqi_cd_00681, rpt_lev_va_00681, rpt_lev_cd_00681, remark_cd_50624, result_va_50624, dqi_cd_50624, rpt_lev_va_50624, rpt_lev_cd_50624, remark_cd_63162, result_va_63162, dqi_cd_63162, rpt_lev_va_63162, rpt_lev_cd_63162)

#id p-codes

#00608 = NH4 wf, 00613 = NO2 wf, 00631 = NO3+NO2 wf, 00671 = Ortho-PO4 wf, 62854 = TDN wf, 70953 = Chl-a, 62360 = Pheophytin, 00681 = DOC, 50624 = Abs 254 nm, 63162 = SUVA

#rename columns

disc_rename <- disc_select %>%
  dplyr::rename(NH4_mgL_remark_cd = remark_cd_00608) %>%
  dplyr::rename(NH4_mgL = result_va_00608) %>%
  dplyr::rename(NH4_mgL_dqi_cd = dqi_cd_00608) %>%
  dplyr::rename(NH4_mgL_rpt_level_va = rpt_lev_va_00608) %>%
  dplyr::rename(NH4_mgL_rpt_lev_cd = rpt_lev_cd_00608) %>%
  dplyr::rename(NO2_mgL_remark_cd = remark_cd_00613) %>%
  dplyr::rename(NO2_mgL = result_va_00613) %>%
  dplyr::rename(NO2_mgL_dqi_cd = dqi_cd_00613) %>%
  dplyr::rename(NO2_mgL_rpt_lev_va = rpt_lev_va_00613) %>%
  dplyr::rename(NO2_mgL_rpt_lev_cd = rpt_lev_cd_00613) %>%
  dplyr::rename(NO3_NO2_mgL_remark_cd = remark_cd_00631) %>%
  dplyr::rename(NO3_NO2_mgL = result_va_00631) %>%
  dplyr::rename(NO3_NO2_mgL_dqi_cd = dqi_cd_00631) %>%
  dplyr::rename(NO3_NO2_mgL_rpt_lev_va = rpt_lev_va_00631) %>%
  dplyr::rename(NO3_NO2_mgL_rpt_lev_cd = rpt_lev_cd_00631) %>%
  dplyr::rename(Ortho_PO4_mgL_remark_cd = remark_cd_00671) %>%
  dplyr::rename(Ortho_PO4_mgL = result_va_00671) %>%
  dplyr::rename(Ortho_PO4_mgL_dqi_cd = dqi_cd_00671) %>%
  dplyr::rename(Ortho_PO4_mgL_rpt_lev_va = rpt_lev_va_00671) %>%
  dplyr::rename(Ortho_PO4_mgL_rpt_lev_cd = rpt_lev_cd_00671) %>%
  dplyr::rename(TDN_mgL_remark_cd = remark_cd_62854) %>%
  dplyr::rename(TDN_mgL = result_va_62854) %>%
  dplyr::rename(TDN_mgL_dqi_cd = dqi_cd_62854) %>%
  dplyr::rename(TDN_mgL_rpt_lev_va = rpt_lev_va_62854) %>%
  dplyr::rename(TDN_mgL_rpt_lev_cd = rpt_lev_cd_62854) %>%
  dplyr::rename(Chla_ugL_remark_cd = remark_cd_70953) %>%
  dplyr::rename(Chla_ugL = result_va_70953) %>%
  dplyr::rename(Chla_ugL_dqi_cd = dqi_cd_70953) %>%
  dplyr::rename(Chla_ugL_rpt_lev_va = rpt_lev_va_70953) %>%
  dplyr::rename(Chla_ugL_rpt_lev_cd = rpt_lev_cd_70953) %>%
  dplyr::rename(Phaeo_ugL_remark_cd = remark_cd_62360) %>%
  dplyr::rename(Phaeo_ugL = result_va_62360) %>%
  dplyr::rename(Phaeo_ugL_dqi_cd = dqi_cd_62360) %>%
  dplyr::rename(Phaeo_ugL_rpt_lev_va = rpt_lev_va_62360) %>%
  dplyr::rename(Phaeo_ugL_rpt_lev_cd = rpt_lev_cd_62360) %>%
  dplyr::rename(DOC_mgL_remark_cd = remark_cd_00681) %>%
  dplyr::rename(DOC_mgL = result_va_00681) %>%
  dplyr::rename(DOC_mgL_dqi_cd = dqi_cd_00681) %>%
  dplyr::rename(DOC_mgL_rpt_lev_va = rpt_lev_va_00681) %>%
  dplyr::rename(DOC_mgL_rpt_lev_cd = rpt_lev_cd_00681) %>%
  dplyr::rename(A254_remark_cd = remark_cd_50624) %>%
  dplyr::rename(A254 = result_va_50624) %>%
  dplyr::rename(A254_dqi_cd = dqi_cd_50624) %>%
  dplyr::rename(A254_rpt_lev_va = rpt_lev_va_50624) %>%
  dplyr::rename(A254_rpt_lev_cd = rpt_lev_cd_50624) %>%
  dplyr::rename(SUVA_remark_cd = remark_cd_63162) %>%
  dplyr::rename(SUVA = result_va_63162) %>%
  dplyr::rename(SUVA_dqi_cd = dqi_cd_63162) %>%
  dplyr::rename(SUVA_rpt_lev_va = rpt_lev_va_63162) %>%
  dplyr::rename(SUVA_rpt_lev_cd = rpt_lev_cd_63162)


# select columns

# Case 1: R and A  approval codes, convert NA values to string NA for easier filtering
# NH4, NO2, NO3_NO2, Ortho, TDN, Chl-a
# approved c("R", "NA", "A")

sp <-  c("NH4_mgL", "NO2_mgL", "NO3_NO2_mgL", "Ortho_PO4_mgL", "TDN_mgL", "Chla_ugL",
         "Phaeo_ugL", "DOC_mgL", "A254", "SUVA")

res <- data.frame(startDateTime = NA, site_no = NA, tz_cd = NA)
for (i in seq_along(sp)){
  
  filter_RA <- disc_rename %>%
    select(startDateTime,
           site_no,
           tz_cd,
           starts_with(sp[i])) %>%
    mutate(across(.cols = contains("dqi_cd"),
                  .fns = ~ifelse(is.na(.), "NA", .)))
  
  if (sp[i] %in% c("NH4_mgL", "NO2_mgL", "NO3_NO2_mgL", "Ortho_PO4_mgL", "TDN_mgL", "Chla_ugL")){
    filter_f <- filter_RA %>%
      filter(if_all(contains("dqi_cd"), ~. %in% c("R", "A", "NA"))) %>%
      filter_at(vars(sp[i]), all_vars(!is.na(.)))
  } else if (sp[i] %in% c("Phaeo_ugL", "DOC_mgL", "A254", "SUVA")){
    filter_f <- filter_RA %>%
      filter(if_all(contains("dqi_cd"), ~. %in% c("R", "NA"))) %>%
      filter_at(vars(sp[i]), all_vars(!is.na(.)))
  }
  
  res <- full_join(res, filter_f)
  
}

full_data <- res %>%
  filter_all(any_vars(!is.na(.)))

write_csv(full_data, "C:/Users/estumpne/Documents/R/drought/USGS_CAWSC_approved_2011_2021.csv")

