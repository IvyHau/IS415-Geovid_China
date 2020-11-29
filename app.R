#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(sf)
library(tmap)
library(spdep)
library(ggthemes)

gc()

# Load Data
city_confirmed <- read_csv("data/aspatial/City_Confirmed_0115_1010.csv")
city_death <- read_csv("data/aspatial/City_Death_0115_1010.csv")
city_recover <- read_csv("data/aspatial/City_Recover_0115_1010.csv")
china_city <- st_read(dsn = "data/geospatial", layer = "china_city_basemap")
china_province <- st_read(dsn = "data/geospatial", layer = "china_province_basemap")

# Replace NAs with 0
city_confirmed[is.na(city_confirmed)] <- 0
city_death[is.na(city_death)] <- 0
city_recover[is.na(city_recover)] <- 0


# EPSG:3415
china_city <- china_city %>%
    dplyr::rename(`pop2010` = A101004_10)
china_3415 <- st_set_crs(china_city, 3415)

# Confirmed Cases in each City

## New Cases by month
confirmed_new <- city_confirmed[, c(1, 6:15)] %>%
    dplyr::rename(`1` = N_C_01,
                  `2` = N_C_02,
                  `3` = N_C_03, 
                  `4` = N_C_04,
                  `5` = N_C_05,
                  `6` = N_C_06,
                  `7` = N_C_07,
                  `8` = N_C_08,
                  `9` = N_C_09) 
## Change from wide table to long table
confirmed_new_wtol <- gather(confirmed_new, month, confirmed_new_count, `1`:`9`, factor_key = TRUE) %>%
    mutate(id = row_number())
## Cumulative Cases by month
confirmed_cumul <- city_confirmed[, c(1, 6, 16:24)] %>%
    dplyr::rename(`1` = T_C_01,
                  `2` = T_C_02, 
                  `3` = T_C_03,
                  `4` = T_C_04,
                  `5` = T_C_05,
                  `6` = T_C_06,
                  `7` = T_C_07,
                  `8` = T_C_08,
                  `9` = T_C_09) 
## Change from wide table to long table
confirmed_cumul_wtol <- gather(confirmed_cumul, month, confirmed_cumul_count, `1`:`9`, factor_key = TRUE)  %>%
    mutate(id = row_number())
confirmed_final <- left_join(confirmed_new_wtol, confirmed_cumul_wtol, by = ("id" = "id")) %>%
    dplyr::rename(`City_EN` = City_EN.x,
                  `Prov_EN` = Prov_EN.x,
                  `month` = month.y) %>%
    dplyr::select("id", "City_EN", "Prov_EN", "month", "confirmed_new_count", "confirmed_cumul_count")
## mapped to crs
confirmed_geo <- left_join(confirmed_final, china_3415, by = c("City_EN" = "City_EN", "Prov_EN" = "Prov_EN"))
## Compute Covid'19 Rate per Municipal
confirmed_geo <- confirmed_geo %>%
    mutate(`Covid'19 confirmed rate (Per 10,000)` = confirmed_cumul_count / (as.numeric(pop2010)/10000))
confirmed_geo_sf <- st_as_sf(confirmed_geo)
confirmed_geo_sf <- confirmed_geo_sf[, c(1:6, 24, 43:44)]


# Recovered Cases in each City

## New cases by month
recovered_new <- city_recover[,c(1, 6:15)] %>%
    dplyr::rename(`1` = N_H_01,
                  `2` = N_H_02,
                  `3` = N_H_03,
                  `4` = N_H_04,
                  `5` = N_H_05,
                  `6` = N_H_06,
                  `7` = N_H_07,
                  `8` = N_H_08,
                  `9` = N_H_09
    )
## Change from wide table to long table
recovered_new_wtol <- gather(recovered_new, month, recovered_new_count, `1`:`9`, factor_key = TRUE) %>%
    mutate(id = row_number())
## Cumulative cases by month
recovered_cumul <- city_recover[,c(1, 6, 16:24)] %>%
    dplyr::rename(`1` = T_H_01,
                  `2` = T_H_02,
                  `3` = T_H_03,
                  `4` = T_H_04,
                  `5` = T_H_05,
                  `6` = T_H_06,
                  `7` = T_H_07,
                  `8` = T_H_08,
                  `9` = T_H_09
    )
## Change from wide table to long table
recovered_cumul_wtol <- gather(recovered_cumul, month, recovered_cumul_count, `1`:`9`, factor_key = TRUE) %>%
    mutate(id = row_number())
recovered_final <-
    left_join(recovered_new_wtol, recovered_cumul_wtol, by=c("id" = "id")) %>%
    dplyr::rename(`City_EN` = City_EN.x,
                  `Prov_EN` = Prov_EN.x,
                  `month` = month.y) %>%
    dplyr::select("id", "City_EN", "Prov_EN", "month", "recovered_new_count", "recovered_cumul_count")
## mapped to crs
recovered_geo <- left_join(recovered_final, china_3415, by = c("City_EN" = "City_EN", "Prov_EN" = "Prov_EN"))
## Calculate Covid '19 death rate 10,000 
recovered_geo <- recovered_geo %>%
    dplyr::mutate("Covid'19 recovered rate (Per 10,000)" = (as.numeric(`recovered_cumul_count`)/(as.numeric(`pop2010`)/10000)))
recovered_geo_sf <- st_as_sf(recovered_geo)
recovered_geo_sf <- recovered_geo_sf[, c(1:6,24, 43:44)]

# Death Cases in each City

## New Cases by month
death_new <- city_death[, c(1, 6:15)] %>%
    dplyr::rename(`1` = N_D_01,
                  `2` = N_D_02,
                  `3` = N_D_03, 
                  `4` = N_D_04,
                  `5` = N_D_05,
                  `6` = N_D_06,
                  `7` = N_D_07,
                  `8` = N_D_08,
                  `9` = N_D_09) 
## Change from wide table to long table
death_new_wtol <- gather(death_new, month, death_new_count, `1`:`9`, factor_key = TRUE) %>%
    mutate(id = row_number())
## Cumulative Cases by month
death_cumul <- city_death[, c(1, 6, 16:24)] %>%
    dplyr::rename(`1` = T_D_0131,
                  `2` = T_D_0229,
                  `3` = T_D_0331,
                  `4` = T_D_0430,
                  `5` = T_D_0531,
                  `6` = T_D_0630,
                  `7` = T_D_0731,
                  `8` = T_D_0831,
                  `9` = T_D_0930) 
## Change from wide table to long table
death_cumul_wtol <- gather(death_cumul, month, death_cumul_count, `1`:`9`, factor_key = TRUE)  %>%
    mutate(id = row_number())
death_final <- left_join(death_new_wtol, death_cumul_wtol, by = ("id" = "id")) %>%
    dplyr::rename(`City_EN` = City_EN.x,
                  `Prov_EN` = Prov_EN.x,
                  `month` = month.y) %>%
    dplyr::select("id", "City_EN", "Prov_EN", "month", "death_new_count", "death_cumul_count")
## death_final[is.na(death_final)] <- 0
## mapped to crs
death_geo <- left_join(death_final, china_3415, by = c("City_EN" = "City_EN",  'Prov_EN' = 'Prov_EN'))
## Calculate Covid '19 death rate 10,000 
death_geo <- death_geo %>%
    dplyr::mutate("Covid'19 death rate (Per 10,000)" = (as.numeric(`death_cumul_count`)/(as.numeric(`pop2010`)/10000)))
death_geo_sf <- st_as_sf(death_geo)
death_geo_sf <- death_geo_sf[, c(1:6,24, 43:44)]
death_geo_sf <- death_geo_sf[!st_is_empty(death_geo_sf),,drop=FALSE]



#Summarize data by months
confirmed_sum <- confirmed_geo_sf %>% 
    st_set_geometry(NULL) %>%
    group_by(month) %>%
    summarise(new_total = sum(confirmed_new_count), cumul_total = sum(confirmed_cumul_count)) %>%
    dplyr::rename(`confirmed_new` = new_total,
                  `confirmed_cumul` = cumul_total)
death_sum <- death_geo_sf %>% 
    st_set_geometry(NULL) %>%
    group_by(month) %>% 
    summarise(new_total = sum(death_new_count), cumul_total = sum(death_cumul_count)) %>%
    dplyr::rename(`death_new` = new_total,
                  `death_cumul` = cumul_total)
recovered_sum <- recovered_geo_sf %>% 
    st_set_geometry(NULL) %>%
    group_by(month) %>% 
    summarise(new_total = sum(recovered_new_count), cumul_total = sum(recovered_cumul_count)) %>%
    dplyr::rename(`recovered_new` = new_total,
                  `recovered_cumul` = cumul_total)
total_cases <- left_join(confirmed_sum, death_sum, by = c('month' = 'month'))
total_cases <- left_join(total_cases, recovered_sum, by = c('month' = 'month'))

# Combine all geo_sf data

total_geo_sf <- left_join( confirmed_geo_sf, 
                           as.data.frame(recovered_geo_sf), by = 
                               c("id","City_EN", "Prov_EN","month","pop2010", "geometry"))

total_geo_sf <- left_join(total_geo_sf, as.data.frame(death_geo_sf),
                          by = c("id","City_EN","Prov_EN",
                                 "month","pop2010", "geometry"))

total_geo_sf <- total_geo_sf[, c(1:6,9:15, 7:8 )]

total_geo_sf <- st_set_crs(total_geo_sf,3415)

# Create Boxmap

boxbreaks <- function(v,mult=1.5) {
    qv <- quantile(v, na.rm = TRUE)
    iqr <- qv[4] - qv[2]
    upfence <- qv[4] + mult * iqr
    lofence <- qv[2] - mult * iqr
    # initialize break points vector
    bb <- vector(mode="numeric",length=7)
    # logic for lower and upper fences
    if (lofence < qv[1]) {  # no lower outliers
        bb[1] <- lofence
        bb[2] <- floor(qv[1])
    } else {
        bb[2] <- lofence
        bb[1] <- qv[1]
    }
    if (upfence > qv[5]) { # no upper outliers
        bb[7] <- upfence
        bb[6] <- ceiling(qv[5])
    } else {
        bb[6] <- upfence
        bb[7] <- qv[5]
    }
    bb[3:5] <- qv[2:4]
    return(bb)
}

get.var <- function(vname,df) {
    vname <- as.character(vname)
    v <- df[vname] %>% st_set_geometry(NULL)
    v <- unname(v[,1])
    return(v)
}

boxmap <- function(vnam,df,legtitle=NA,mtitle="Box Map",mult=1.5){
    var <- get.var(vnam,df)
    bb <- boxbreaks(var)
    tm_shape(df) +
        tm_fill(vnam,title=legtitle,breaks=bb,palette="-RdBu",
                labels = c("lower outlier", "< 25%", "25% - 50%", "50% - 75%","> 75%", "upper outlier"))  +
        tm_borders() +
        tm_layout(title = mtitle, title.position = c("right","bottom"))
}


set.ZeroPolicyOption(TRUE)
# Confirmed
confirmed_geo_sf_sum <- confirmed_geo_sf %>%
    group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
    filter(as.numeric(`month`) >= 1,
           as.numeric(`month`) <= 7) %>%
    dplyr::select(-`month`) %>%
    summarise(confirmed_new_count = sum(`confirmed_new_count`)) %>%
    mutate("Covid'19 confirmed rate (Per 10,000)" = (as.numeric(`confirmed_new_count`)/(as.numeric(`pop2010`)/10000)))

# Recovered
recovered_geo_sf_sum <- recovered_geo_sf %>%
    group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
    filter(as.numeric(`month`) >= 1,
           as.numeric(`month`) <= 7) %>%
    dplyr::select(-`month`) %>%
    summarise(recovered_new_count = sum(`recovered_new_count`)) %>%
    mutate("Covid'19 recovered rate (Per 10,000)" = (as.numeric(`recovered_new_count`)/(as.numeric(`pop2010`)/10000)))

# Death
death_geo_sf_sum <- death_geo_sf %>%
    group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
    filter(as.numeric(`month`) >= 1,
           as.numeric(`month`) <= 7) %>%
    dplyr::select(-`month`) %>%
    summarise(death_new_count = sum(`death_new_count`)) %>%
    mutate("Covid'19 death rate (Per 10,000)" = (as.numeric(`death_new_count`)/(as.numeric(`pop2010`)/10000)))

test <- death_geo_sf %>%
    group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
    filter(as.numeric(`month`) >= 1,
           as.numeric(`month`) <= 7) %>%
    dplyr::select(-`month`)

# Data Conversion to SP
confirmed_geo_sp <- sf:::as_Spatial(confirmed_geo_sf_sum)
recovered_geo_sp <- sf:::as_Spatial(recovered_geo_sf_sum)
death_geo_sp <- sf:::as_Spatial(death_geo_sf_sum)

# Rook Contiguity based neighbors
confirmed_wm_r <- poly2nb(confirmed_geo_sp, queen=FALSE)
recovered_wm_r <- poly2nb(recovered_geo_sp, queen=FALSE)
death_wm_r <- poly2nb(death_geo_sp, queen=FALSE)

# Queen Contiguity based neighbors
confirmed_wm_q <- poly2nb(confirmed_geo_sp, queen=TRUE)
recovered_wm_q <- poly2nb(recovered_geo_sp, queen=TRUE)
death_wm_q <- poly2nb(death_geo_sp, queen=TRUE)

# Fixed Distance Matrix
## Confirmed
c_coords <- coordinates(confirmed_geo_sp)
c_wm_d4 <- dnearneigh(c_coords, 0, 4, longlat = FALSE)
## Recovered
r_coords <- coordinates(recovered_geo_sp)
r_wm_d4 <- dnearneigh(r_coords, 0, 4, longlat = FALSE)
## Death
d_coords <- coordinates(death_geo_sp)
d_wm_d5 <- dnearneigh(d_coords, 0, 5, longlat = FALSE)

# Adaptive Distance Matrix
c_knn4 <- knn2nb(knearneigh(c_coords, k=4))
r_knn4 <- knn2nb(knearneigh(r_coords, k=4))
d_knn5 <- knn2nb(knearneigh(d_coords, k=5))

# Row Standardise Weight Matrix
## nb2listw() Style = "W" -- Equal Weight
### Rook Contiguity Based  Matrix
rsc_wm_r_w <- nb2listw(confirmed_wm_r, style = "W", zero.policy = TRUE)
rsr_wm_r_w <- nb2listw(recovered_wm_r, style = "W", zero.policy = TRUE)
rsd_wm_r_w <- nb2listw(death_wm_r, style = "W", zero.policy = TRUE)
### Queen Contiguity Based  Matrix
rsc_wm_q_w <- nb2listw(confirmed_wm_q, style = "W", zero.policy = TRUE)
rsr_wm_q_w <- nb2listw(recovered_wm_q, style = "W", zero.policy = TRUE)
rsd_wm_q_w <- nb2listw(death_wm_q, style = "W", zero.policy = TRUE)
### Fixed Matrix
rsc_wm_d4_fix_w <- nb2listw(c_wm_d4, style = "W", zero.policy = TRUE)
rsr_wm_d4_fix_w <- nb2listw(r_wm_d4, style = "W", zero.policy = TRUE)
rsd_wm_d5_fix_w <- nb2listw(d_wm_d5, style = "W", zero.policy = TRUE)
### Adaptive Matrix
rsc_knn4_adap_w <- nb2listw(c_knn4, style = "W", zero.policy = TRUE)
rsr_knn4_adap_w <- nb2listw(r_knn4, style = "W", zero.policy = TRUE)
rsd_knn5_adap_w <- nb2listw(d_knn5, style = "W", zero.policy = TRUE)
## nb2listw() Style = "B" -- Binary Weight
### Rook Contiguity Based  Matrix
rsc_wm_r_b <- nb2listw(confirmed_wm_r, style = "B", zero.policy = TRUE)
rsr_wm_r_b <- nb2listw(recovered_wm_r, style = "B", zero.policy = TRUE)
rsd_wm_r_b <- nb2listw(death_wm_r, style = "B", zero.policy = TRUE)
### Queen Contiguity Based  Matrix
rsc_wm_q_b <- nb2listw(confirmed_wm_q, style = "B", zero.policy = TRUE)
rsr_wm_q_b <- nb2listw(recovered_wm_q, style = "B", zero.policy = TRUE)
rsd_wm_q_b <- nb2listw(death_wm_q, style = "B", zero.policy = TRUE)
### Fixed Matrix
rsc_wm_d4_fix_b <- nb2listw(c_wm_d4, style = "B", zero.policy = TRUE)
rsr_wm_d4_fix_b <- nb2listw(r_wm_d4, style = "B", zero.policy = TRUE)
rsd_wm_d5_fix_b <- nb2listw(d_wm_d5, style = "B", zero.policy = TRUE)
### Adaptive Matrix
rsc_knn4_adap_b <- nb2listw(c_knn4, style = "B", zero.policy = TRUE)
rsr_knn4_adap_b <- nb2listw(r_knn4, style = "B", zero.policy = TRUE)
rsd_knn5_adap_b <- nb2listw(d_knn5, style = "B", zero.policy = TRUE)

#Local Moran I
# Order the dataset
c_fips <- order(confirmed_geo_sp$City_EN)
r_fips <- order(recovered_geo_sp$City_EN)
d_fips <- order(death_geo_sp$City_EN)
## nb2listw() Style = "W" -- Equal Weight
### Rook Contiguity Based  Matrix
#### Confirmed Cases
c_localMI_r_w <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_wm_r_w)
confirmed_geo_sp.confirmed_localMI_r_w <- cbind(confirmed_geo_sp,c_localMI_r_w)
#### Recovered Cases
r_localMI_r_w <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_wm_r_w)
recovered_geo_sp.recovered_localMI_r_w <- cbind(recovered_geo_sp,r_localMI_r_w)
#### Death Cases
d_localMI_r_w <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_wm_r_w)
death_geo_sp.death_localMI_r_w <- cbind(death_geo_sp,d_localMI_r_w)
### Queen Contiguity Based  Matrix
#### Confirmed Cases
c_localMI_q_w <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_wm_q_w)
confirmed_geo_sp.confirmed_localMI_q_w <- cbind(confirmed_geo_sp,c_localMI_q_w)
#### Recovered Cases
r_localMI_q_w <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_wm_q_w)
recovered_geo_sp.recovered_localMI_q_w <- cbind(recovered_geo_sp,r_localMI_q_w)
#### Death Cases
d_localMI_q_w <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_wm_q_w)
death_geo_sp.death_localMI_q_w <- cbind(death_geo_sp,d_localMI_q_w)
### Fixed Distance
#### Confirmed Cases
c_localMI_fix_w <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_wm_d4_fix_w)
confirmed_geo_sp.confirmed_localMI_fix_w <- cbind(confirmed_geo_sp,c_localMI_fix_w)
#### Recovered Cases
r_localMI_fix_w <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_wm_d4_fix_w)
recovered_geo_sp.recovered_localMI_fix_w <- cbind(recovered_geo_sp,r_localMI_fix_w)
#### Death Cases
d_localMI_fix_w <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_wm_d5_fix_w)
death_geo_sp.death_localMI_fix_w <- cbind(death_geo_sp,d_localMI_fix_w)
### Adaptive Distance
#### Confirmed Cases
c_localMI_adap_w <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_knn4_adap_w)
confirmed_geo_sp.confirmed_localMI_adap_w <- cbind(confirmed_geo_sp,c_localMI_adap_w)
#### Recovered Cases
r_localMI_adap_w <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_knn4_adap_w)
recovered_geo_sp.recovered_localMI_adap_w <- cbind(recovered_geo_sp,r_localMI_adap_w)
#### Death Cases
d_localMI_adap_w <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_knn5_adap_w)
death_geo_sp.death_localMI_adap_w <- cbind(death_geo_sp,d_localMI_adap_w)
## nb2listw() Style = "B" -- Binary Weight
### Rook Contiguity Based  Matrix
#### Confirmed Cases
c_localMI_r_b <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_wm_r_b)
confirmed_geo_sp.confirmed_localMI_r_b <- cbind(confirmed_geo_sp,c_localMI_r_b)
#### Recovered Cases
r_localMI_r_b <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_wm_r_b)
recovered_geo_sp.recovered_localMI_r_b <- cbind(recovered_geo_sp,r_localMI_r_b)
#### Death Cases
d_localMI_r_b <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_wm_r_b)
death_geo_sp.death_localMI_r_b <- cbind(death_geo_sp,d_localMI_r_b)
### Queen Contiguity Based  Matrix
#### Confirmed Cases
c_localMI_q_b <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_wm_q_b)
confirmed_geo_sp.confirmed_localMI_q_b <- cbind(confirmed_geo_sp,c_localMI_q_b)
#### Recovered Cases
r_localMI_q_b <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_wm_q_b)
recovered_geo_sp.recovered_localMI_q_b <- cbind(recovered_geo_sp,r_localMI_q_b)
#### Death Cases
d_localMI_q_b <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_wm_q_b)
death_geo_sp.death_localMI_q_b <- cbind(death_geo_sp,d_localMI_q_b)
### Fixed Distance
#### Confirmed Cases
c_localMI_fix_b <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_wm_d4_fix_b)
confirmed_geo_sp.confirmed_localMI_fix_b <- cbind(confirmed_geo_sp,c_localMI_fix_b)
#### Recovered Cases
r_localMI_fix_b <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_wm_d4_fix_b)
recovered_geo_sp.recovered_localMI_fix_b <- cbind(recovered_geo_sp,r_localMI_fix_b)
#### Death Cases
d_localMI_fix_b <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_wm_d5_fix_b)
death_geo_sp.death_localMI_fix_b <- cbind(death_geo_sp,d_localMI_fix_b)
### Adaptive Distance
#### Confirmed Cases
c_localMI_adap_b <- localmoran(confirmed_geo_sp$Covid.19.confirmed.rate..Per.10.000., rsc_knn4_adap_b)
confirmed_geo_sp.confirmed_localMI_adap_b <- cbind(confirmed_geo_sp,c_localMI_adap_b)
#### Recovered Cases
r_localMI_adap_b <- localmoran(recovered_geo_sp$Covid.19.recovered.rate..Per.10.000., rsr_knn4_adap_b)
recovered_geo_sp.recovered_localMI_adap_b <- cbind(recovered_geo_sp,r_localMI_adap_b)
#### Death Cases
d_localMI_adap_b <- localmoran(death_geo_sp$Covid.19.death.rate..Per.10.000., rsd_knn5_adap_b)
death_geo_sp.death_localMI_adap_b <- cbind(death_geo_sp,d_localMI_adap_b)

# Confirmed
confirmed_geo_sf_sum <- confirmed_geo_sf %>%
  group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
  filter(as.numeric(`month`) >= 1,
         as.numeric(`month`) <= 7) %>%
  dplyr::select(-`month`) %>%
  summarise(confirmed_new_count = sum(`confirmed_new_count`)) %>%
  mutate("Covid'19 confirmed rate (Per 10,000)" = (as.numeric(`confirmed_new_count`)/(as.numeric(`pop2010`)/10000)))

confirmed_geo_2 <- left_join(confirmed_final, china_3415, by = c("City_EN" = "City_EN", "Prov_EN" = "Prov_EN"))
confirmed_geo_2 <- confirmed_geo_2 %>%
  mutate(`confirmed_cumul_count` = confirmed_cumul_count / (as.numeric(pop2010)/10000))
confirmed_geo_acu_sf <- st_as_sf(confirmed_geo_2)

confirmed_geo_acu_sf <- confirmed_geo_acu_sf %>%
  group_by(`City_EN`, `Prov_EN`, `pop2010`)

confirmed_geo_acu_jan_sf <- confirmed_geo_acu_sf %>%
  filter(as.numeric(`month`) == 1)

confirmed_geo_acu_apr_sf <- confirmed_geo_acu_sf %>%
  filter(as.numeric(`month`) == 4)

confirmed_geo_acu_jul_sf <- confirmed_geo_acu_sf %>%
  filter(as.numeric(`month`) == 7)

confirmed_geo_acu_sep_sf <- confirmed_geo_acu_sf %>%
  filter(as.numeric(`month`) == 9)

# Recovered
recovered_geo_sf_sum <- recovered_geo_sf %>%
  group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
  filter(as.numeric(`month`) >= 1,
         as.numeric(`month`) <= 7) %>%
  dplyr::select(-`month`) %>%
  summarise(recovered_new_count = sum(`recovered_new_count`)) %>%
  mutate("Covid'19 recovered rate (Per 10,000)" = (as.numeric(`recovered_new_count`)/(as.numeric(`pop2010`)/10000)))

recovered_geo_2 <- left_join(recovered_final, china_3415, by = c("City_EN" = "City_EN", "Prov_EN" = "Prov_EN"))
recovered_geo_acu_sf <- st_as_sf(recovered_geo_2)

recovered_geo_acu_sf <- recovered_geo_acu_sf %>%
  group_by(`City_EN`, `Prov_EN`, `pop2010`)

recovered_geo_acu_jan_sf <- recovered_geo_acu_sf %>%
  filter(as.numeric(`month`) == 1)

recovered_geo_acu_apr_sf <- recovered_geo_acu_sf %>%
  filter(as.numeric(`month`) == 4)

recovered_geo_acu_jul_sf <- recovered_geo_acu_sf %>%
  filter(as.numeric(`month`) == 7)

recovered_geo_acu_sep_sf <- recovered_geo_acu_sf %>%
  filter(as.numeric(`month`) == 9)

# Death
death_geo_sf_sum <- death_geo_sf %>%
  group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
  filter(as.numeric(`month`) >= 1,
         as.numeric(`month`) <= 7) %>%
  dplyr::select(-`month`) %>%
  summarise(death_new_count = sum(`death_new_count`)) %>%
  mutate("Covid'19 death rate (Per 10,000)" = (as.numeric(`death_new_count`)/(as.numeric(`pop2010`)/10000)))

death_geo_2 <- left_join(death_final, china_3415, by = c("City_EN" = "City_EN",  'Prov_EN' = 'Prov_EN'))
death_geo_acu_sf <- st_as_sf(death_geo_2)

death_geo_acu_sf <- death_geo_acu_sf %>%
  group_by(`City_EN`, `Prov_EN`, `pop2010`)

death_geo_acu_jan_sf <- death_geo_acu_sf %>%
  filter(as.numeric(`month`) == 1)

death_geo_acu_apr_sf <- death_geo_acu_sf %>%
  filter(as.numeric(`month`) == 4)

death_geo_acu_jul_sf <- death_geo_acu_sf %>%
  filter(as.numeric(`month`) == 7)

death_geo_acu_sep_sf <- death_geo_acu_sf %>%
  filter(as.numeric(`month`) == 9)

confirmed_geo_sp <- sf:::as_Spatial(confirmed_geo_sf_sum)
confirmed_geo_acu_sp <- sf::: as_Spatial(confirmed_geo_acu_sf)
confirmed_geo_acu_jan_sp <- sf::: as_Spatial(confirmed_geo_acu_jan_sf)
confirmed_geo_acu_apr_sp <- sf::: as_Spatial(confirmed_geo_acu_apr_sf)
confirmed_geo_acu_jul_sp <- sf::: as_Spatial(confirmed_geo_acu_jul_sf)
confirmed_geo_acu_sep_sp <- sf::: as_Spatial(confirmed_geo_acu_sep_sf)

rsconfirmed_wm_q_w <- nb2listw(confirmed_wm_q, style = "W", zero.policy = TRUE)
rsconfirmed_wm_q_b <- nb2listw(confirmed_wm_q, style = "B", zero.policy = TRUE)

confirmed_acu_wm_q <- poly2nb(confirmed_geo_acu_sp, queen=TRUE)
confirmed_acu_jan_wm_q <- poly2nb(confirmed_geo_acu_jan_sp, queen=TRUE)
confirmed_acu_apr_wm_q <- poly2nb(confirmed_geo_acu_apr_sp, queen=TRUE)
confirmed_acu_jul_wm_q <- poly2nb(confirmed_geo_acu_jul_sp, queen=TRUE)
confirmed_acu_sep_wm_q <- poly2nb(confirmed_geo_acu_sep_sp, queen=TRUE)

rsconfirmed_acu_wm_q_w <- nb2listw(confirmed_acu_wm_q, style = "W", zero.policy = TRUE)
rsconfirmed_acu_jan_wm_q_w <- nb2listw(confirmed_acu_jan_wm_q, style = "W", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_q_w <- nb2listw(confirmed_acu_apr_wm_q, style = "W", zero.policy = TRUE)
rsconfirmed_acu_jul_wm_q_w <- nb2listw(confirmed_acu_jul_wm_q, style = "W", zero.policy = TRUE)
rsconfirmed_acu_sep_wm_q_w <- nb2listw(confirmed_acu_sep_wm_q, style = "W", zero.policy = TRUE)

rsconfirmed_acu_wm_q_b <- nb2listw(confirmed_acu_wm_q, style = "B", zero.policy = TRUE)
rsconfirmed_acu_jan_wm_q_b <- nb2listw(confirmed_acu_jan_wm_q, style = "B", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_q_b <- nb2listw(confirmed_acu_apr_wm_q, style = "B", zero.policy = TRUE)
rsconfirmed_acu_jul_wm_q_b <- nb2listw(confirmed_acu_jul_wm_q, style = "B", zero.policy = TRUE)
rsconfirmed_acu_sep_wm_q_b <- nb2listw(confirmed_acu_sep_wm_q, style = "B", zero.policy = TRUE)

confirmed_acu_jan_wm_r <- poly2nb(confirmed_geo_acu_jan_sp, queen=FALSE)
confirmed_acu_apr_wm_r <- poly2nb(confirmed_geo_acu_apr_sp, queen=FALSE)
confirmed_acu_jul_wm_r <- poly2nb(confirmed_geo_acu_jul_sp, queen=FALSE)
confirmed_acu_sep_wm_r <- poly2nb(confirmed_geo_acu_sep_sp, queen=FALSE)

rsconfirmed_acu_jan_wm_r_w <- nb2listw(confirmed_acu_jan_wm_r, style = "W", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_r_w <- nb2listw(confirmed_acu_apr_wm_r, style = "W", zero.policy = TRUE)
rsconfirmed_acu_jul_wm_r_w <- nb2listw(confirmed_acu_jul_wm_r, style = "W", zero.policy = TRUE)
rsconfirmed_acu_sep_wm_r_w <- nb2listw(confirmed_acu_sep_wm_r, style = "W", zero.policy = TRUE)

rsconfirmed_acu_jan_wm_r_b <- nb2listw(confirmed_acu_jan_wm_r, style = "B", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_r_b <- nb2listw(confirmed_acu_apr_wm_r, style = "B", zero.policy = TRUE)
rsconfirmed_acu_jul_wm_r_b <- nb2listw(confirmed_acu_jul_wm_r, style = "B", zero.policy = TRUE)
rsconfirmed_acu_sep_wm_r_b <- nb2listw(confirmed_acu_sep_wm_r, style = "B", zero.policy = TRUE)

c_coords <- coordinates(confirmed_geo_sp)
c_wm_d4 <- dnearneigh(c_coords, 0, 4, longlat = FALSE)

confirmed_coords_jan_wm <- coordinates(confirmed_geo_acu_jan_sp)
confirmed_coords_apr_wm <- coordinates(confirmed_geo_acu_apr_sp)
confirmed_coords_jul_wm <- coordinates(confirmed_geo_acu_jul_sp)
confirmed_coords_sep_wm <- coordinates(confirmed_geo_acu_sep_sp)

confirmed_wm_d4_jan_wm_fix <- dnearneigh(confirmed_coords_jan_wm, 0, 4, longlat = FALSE)
confirmed_wm_d4_apr_wm_fix <- dnearneigh(confirmed_coords_apr_wm, 0, 4, longlat = FALSE)
confirmed_wm_d4_jul_wm_fix <- dnearneigh(confirmed_coords_jul_wm, 0, 4, longlat = FALSE)
confirmed_wm_d4_sep_wm_fix <- dnearneigh(confirmed_coords_sep_wm, 0, 4, longlat = FALSE)

rsc_wm_d4_fix_w <- nb2listw(c_wm_d4, style = "W", zero.policy = TRUE)

rsconfirmed_acu_jan_wm_fix_w <- nb2listw(confirmed_wm_d4_jan_wm_fix, style = "W", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_fix_w <- nb2listw(confirmed_wm_d4_apr_wm_fix, style = "W", zero.policy = TRUE)
rsconfirmed_acu_jul_wm_fix_w <- nb2listw(confirmed_wm_d4_jul_wm_fix, style = "W", zero.policy = TRUE)
rsconfirmed_acu_sep_wm_fix_w <- nb2listw(confirmed_wm_d4_sep_wm_fix, style = "W", zero.policy = TRUE)

rsconfirmed_acu_jan_wm_fix_b <- nb2listw(confirmed_wm_d4_jan_wm_fix, style = "B", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_fix_b <- nb2listw(confirmed_wm_d4_apr_wm_fix, style = "B", zero.policy = TRUE)
rsconfirmed_acu_d4_jul_wm_fix_b <- nb2listw(confirmed_wm_d4_jul_wm_fix, style = "B", zero.policy = TRUE)
rsconfirmed_acu_d4_sep_wm_fix_b <- nb2listw(confirmed_wm_d4_sep_wm_fix, style = "B", zero.policy = TRUE)

c_knn4 <- knn2nb(knearneigh(c_coords, k=4))

confirmed_knn4_jan_wm_adap <- knn2nb(knearneigh(confirmed_coords_jan_wm, k=4))
confirmed_knn4_apr_wm_adap <- knn2nb(knearneigh(confirmed_coords_apr_wm, k=4))
confirmed_knn4_jul_wm_adap <- knn2nb(knearneigh(confirmed_coords_jul_wm, k=4))
confirmed_knn4_sep_wm_adap <- knn2nb(knearneigh(confirmed_coords_sep_wm, k=4))

rsconfirmed_acu_jan_wm_adap_w <- nb2listw(confirmed_knn4_jan_wm_adap, style = "W", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_adap_w <- nb2listw(confirmed_knn4_apr_wm_adap, style = "W", zero.policy = TRUE)
rsconfirmed_acu_jul_wm_adap_w <- nb2listw(confirmed_knn4_jul_wm_adap, style = "W", zero.policy = TRUE)
rsconfirmed_acu_sep_wm_adap_w <- nb2listw(confirmed_knn4_sep_wm_adap, style = "W", zero.policy = TRUE)

rsconfirmed_acu_jan_wm_adap_b <- nb2listw(confirmed_knn4_jan_wm_adap, style = "B", zero.policy = TRUE)
rsconfirmed_acu_apr_wm_adap_b <- nb2listw(confirmed_knn4_apr_wm_adap, style = "B", zero.policy = TRUE)
rsconfirmed_acu_jul_wm_adap_b <- nb2listw(confirmed_knn4_jul_wm_adap, style = "B", zero.policy = TRUE)
rsconfirmed_acu_sep_wm_adap_b <- nb2listw(confirmed_knn4_sep_wm_adap, style = "B", zero.policy = TRUE)

recovered_geo_sp <- sf:::as_Spatial(recovered_geo_sf_sum)
recovered_geo_acu_sp <- sf:::as_Spatial(recovered_geo_acu_sf)
recovered_geo_acu_jan_sp <- sf:::as_Spatial(recovered_geo_acu_jan_sf)
recovered_geo_acu_apr_sp <- sf:::as_Spatial(recovered_geo_acu_apr_sf)
recovered_geo_acu_jul_sp <- sf:::as_Spatial(recovered_geo_acu_jul_sf)
recovered_geo_acu_sep_sp <- sf:::as_Spatial(recovered_geo_acu_sep_sf)

rsrecovered_wm_q_w <- nb2listw(recovered_wm_q, style = "W", zero.policy = TRUE)
rsrecovered_wm_q_b <- nb2listw(recovered_wm_q, style = "B", zero.policy = TRUE)

recovered_acu_wm_q <- poly2nb(recovered_geo_acu_sp, queen=TRUE)
recovered_acu_jan_wm_q <- poly2nb(recovered_geo_acu_jan_sp, queen=TRUE)
recovered_acu_apr_wm_q <- poly2nb(recovered_geo_acu_apr_sp, queen=TRUE)
recovered_acu_jul_wm_q <- poly2nb(recovered_geo_acu_jul_sp, queen=TRUE)
recovered_acu_sep_wm_q <- poly2nb(recovered_geo_acu_sep_sp, queen=TRUE)

rsrecovered_acu_wm_q_w <- nb2listw(recovered_acu_wm_q, style = "W", zero.policy = TRUE)
rsrecovered_acu_jan_wm_q_w <- nb2listw(recovered_acu_jan_wm_q, style = "W", zero.policy = TRUE)
rsrecovered_acu_apr_wm_q_w <- nb2listw(recovered_acu_apr_wm_q, style = "W", zero.policy = TRUE)
rsrecovered_acu_jul_wm_q_w <- nb2listw(recovered_acu_jul_wm_q, style = "W", zero.policy = TRUE)
rsrecovered_acu_sep_wm_q_w <- nb2listw(recovered_acu_sep_wm_q, style = "W", zero.policy = TRUE)

rsrecovered_acu_wm_q_b <- nb2listw(recovered_acu_wm_q, style = "B", zero.policy = TRUE)
rsrecovered_acu_jan_wm_q_b <- nb2listw(recovered_acu_jan_wm_q, style = "B", zero.policy = TRUE)
rsrecovered_acu_apr_wm_q_b <- nb2listw(recovered_acu_apr_wm_q, style = "B", zero.policy = TRUE)
rsrecovered_acu_jul_wm_q_b <- nb2listw(recovered_acu_jul_wm_q, style = "B", zero.policy = TRUE)
rsrecovered_acu_sep_wm_q_b <- nb2listw(recovered_acu_sep_wm_q, style = "B", zero.policy = TRUE)

recovered_acu_jan_wm_r <- poly2nb(recovered_geo_acu_jan_sp, queen=FALSE)
recovered_acu_apr_wm_r <- poly2nb(recovered_geo_acu_apr_sp, queen=FALSE)
recovered_acu_jul_wm_r <- poly2nb(recovered_geo_acu_jul_sp, queen=FALSE)
recovered_acu_sep_wm_r <- poly2nb(recovered_geo_acu_sep_sp, queen=FALSE)

rsrecovered_acu_jan_wm_r_w <- nb2listw(recovered_acu_jan_wm_r, style = "W", zero.policy = TRUE)
rsrecovered_acu_apr_wm_r_w <- nb2listw(recovered_acu_apr_wm_r, style = "W", zero.policy = TRUE)
rsrecovered_acu_jul_wm_r_w <- nb2listw(recovered_acu_jul_wm_r, style = "W", zero.policy = TRUE)
rsrecovered_acu_sep_wm_r_w <- nb2listw(recovered_acu_sep_wm_r, style = "W", zero.policy = TRUE)

rsrecovered_acu_jan_wm_r_b <- nb2listw(recovered_acu_jan_wm_r, style = "B", zero.policy = TRUE)
rsrecovered_acu_apr_wm_r_b <- nb2listw(recovered_acu_apr_wm_r, style = "B", zero.policy = TRUE)
rsrecovered_acu_jul_wm_r_b <- nb2listw(recovered_acu_jul_wm_r, style = "B", zero.policy = TRUE)
rsrecovered_acu_sep_wm_r_b <- nb2listw(recovered_acu_sep_wm_r, style = "B", zero.policy = TRUE)

r_coords <- coordinates(recovered_geo_sp)
r_wm_d4 <- dnearneigh(r_coords, 0, 4, longlat = FALSE)

recovered_coords_jan_wm <- coordinates(recovered_geo_acu_jan_sp)
recovered_coords_apr_wm <- coordinates(recovered_geo_acu_apr_sp)
recovered_coords_jul_wm <- coordinates(recovered_geo_acu_jul_sp)
recovered_coords_sep_wm <- coordinates(recovered_geo_acu_sep_sp)

recovered_wm_d4_jan_wm_fix <- dnearneigh(recovered_coords_jan_wm, 0, 4, longlat = FALSE)
recovered_wm_d4_apr_wm_fix <- dnearneigh(recovered_coords_apr_wm, 0, 4, longlat = FALSE)
recovered_wm_d4_jul_wm_fix <- dnearneigh(recovered_coords_jul_wm, 0, 4, longlat = FALSE)
recovered_wm_d4_sep_wm_fix <- dnearneigh(recovered_coords_sep_wm, 0, 4, longlat = FALSE)

rsr_wm_d4_fix_w <- nb2listw(r_wm_d4, style = "W", zero.policy = TRUE)
rsr_wm_d4_fix_b <- nb2listw(r_wm_d4, style = "B", zero.policy = TRUE)

rsrecovered_acu_jan_wm_fix_w <- nb2listw(recovered_wm_d4_jan_wm_fix, style = "W", zero.policy = TRUE)
rsrecovered_acu_apr_wm_fix_w <- nb2listw(recovered_wm_d4_apr_wm_fix, style = "W", zero.policy = TRUE)
rsrecovered_acu_jul_wm_fix_w <- nb2listw(recovered_wm_d4_jul_wm_fix, style = "W", zero.policy = TRUE)
rsrecovered_acu_sep_wm_fix_w <- nb2listw(recovered_wm_d4_sep_wm_fix, style = "W", zero.policy = TRUE)

rsrecovered_acu_jan_wm_fix_b <- nb2listw(recovered_wm_d4_jan_wm_fix, style = "B", zero.policy = TRUE)
rsrecovered_acu_apr_wm_fix_b <- nb2listw(recovered_wm_d4_apr_wm_fix, style = "B", zero.policy = TRUE)
rsrecovered_acu_jul_wm_fix_b <- nb2listw(recovered_wm_d4_jul_wm_fix, style = "B", zero.policy = TRUE)
rsrecovered_acu_sep_wm_fix_b <- nb2listw(recovered_wm_d4_sep_wm_fix, style = "B", zero.policy = TRUE)

recovered_knn4_jan_wm_adap <- knn2nb(knearneigh(recovered_coords_jan_wm, k=4))
recovered_knn4_apr_wm_adap <- knn2nb(knearneigh(recovered_coords_apr_wm, k=4))
recovered_knn4_jul_wm_adap <- knn2nb(knearneigh(recovered_coords_jul_wm, k=4))
recovered_knn4_sep_wm_adap <- knn2nb(knearneigh(recovered_coords_sep_wm, k=4))

rsrecovered_acu_jan_wm_adap_w <- nb2listw(recovered_knn4_jan_wm_adap, style = "W", zero.policy = TRUE)
rsrecovered_acu_apr_wm_adap_w <- nb2listw(recovered_knn4_apr_wm_adap, style = "W", zero.policy = TRUE)
rsrecovered_acu_jul_wm_adap_w <- nb2listw(recovered_knn4_jul_wm_adap, style = "W", zero.policy = TRUE)
rsrecovered_acu_sep_wm_adap_w <- nb2listw(recovered_knn4_sep_wm_adap, style = "W", zero.policy = TRUE)

rsrecovered_acu_jan_wm_adap_b <- nb2listw(recovered_knn4_jan_wm_adap, style = "B", zero.policy = TRUE)
rsrecovered_acu_apr_wm_adap_b <- nb2listw(recovered_knn4_apr_wm_adap, style = "B", zero.policy = TRUE)
rsrecovered_acu_jul_wm_adap_b <- nb2listw(recovered_knn4_jul_wm_adap, style = "B", zero.policy = TRUE)
rsrecovered_acu_sep_wm_adap_b <- nb2listw(recovered_knn4_sep_wm_adap, style = "B", zero.policy = TRUE)

death_geo_sf_sum <- death_geo_sf_sum[!st_is_empty(death_geo_sf_sum),,drop=FALSE]
death_geo_acu_sf <- death_geo_acu_sf[!st_is_empty(death_geo_acu_sf),,drop=FALSE]

death_geo_acu_jan_sf <- death_geo_acu_jan_sf[!st_is_empty(death_geo_acu_jan_sf),,drop=FALSE]
death_geo_acu_apr_sf <- death_geo_acu_apr_sf[!st_is_empty(death_geo_acu_apr_sf),,drop=FALSE]
death_geo_acu_jul_sf <- death_geo_acu_jul_sf[!st_is_empty(death_geo_acu_jul_sf),,drop=FALSE]
death_geo_acu_sep_sf <- death_geo_acu_sep_sf[!st_is_empty(death_geo_acu_sep_sf),,drop=FALSE]

death_geo_sp <- sf:::as_Spatial(death_geo_sf_sum)
death_geo_acu_sp <- sf:::as_Spatial(death_geo_acu_sf)
death_geo_acu_jan_sp <- sf:::as_Spatial(death_geo_acu_jan_sf)
death_geo_acu_apr_sp <- sf:::as_Spatial(death_geo_acu_apr_sf)
death_geo_acu_jul_sp <- sf:::as_Spatial(death_geo_acu_jul_sf)
death_geo_acu_sep_sp <- sf:::as_Spatial(death_geo_acu_sep_sf)

rsdeath_wm_q_w <- nb2listw(death_wm_q, style = "W", zero.policy = TRUE)
rsdeath_wm_q_b <- nb2listw(death_wm_q, style = "B", zero.policy = TRUE)

death_acu_wm_q <- poly2nb(death_geo_acu_sp, queen=TRUE)
death_acu_jan_wm_q <- poly2nb(death_geo_acu_jan_sp, queen=TRUE)
death_acu_apr_wm_q <- poly2nb(death_geo_acu_apr_sp, queen=TRUE)
death_acu_jul_wm_q <- poly2nb(death_geo_acu_jul_sp, queen=TRUE)
death_acu_sep_wm_q <- poly2nb(death_geo_acu_sep_sp, queen=TRUE)

rsdeath_acu_wm_q_w <- nb2listw(death_acu_wm_q, style = "W", zero.policy = TRUE)
rsdeath_acu_jan_wm_q_w <- nb2listw(death_acu_jan_wm_q, style = "W", zero.policy = TRUE)
rsdeath_acu_apr_wm_q_w <- nb2listw(death_acu_apr_wm_q, style = "W", zero.policy = TRUE)
rsdeath_acu_jul_wm_q_w <- nb2listw(death_acu_jul_wm_q, style = "W", zero.policy = TRUE)
rsdeath_acu_sep_wm_q_w <- nb2listw(death_acu_sep_wm_q, style = "W", zero.policy = TRUE)

rsdeath_acu_wm_q_b <- nb2listw(death_acu_wm_q, style = "B", zero.policy = TRUE)
rsdeath_acu_jan_wm_q_b <- nb2listw(death_acu_jan_wm_q, style = "B", zero.policy = TRUE)
rsdeath_acu_apr_wm_q_b <- nb2listw(death_acu_apr_wm_q, style = "B", zero.policy = TRUE)
rsdeath_acu_jul_wm_q_b <- nb2listw(death_acu_jul_wm_q, style = "B", zero.policy = TRUE)
rsdeath_acu_sep_wm_q_b <- nb2listw(death_acu_sep_wm_q, style = "B", zero.policy = TRUE)

death_acu_jan_wm_r <- poly2nb(death_geo_acu_jan_sp, queen=FALSE)
death_acu_apr_wm_r <- poly2nb(death_geo_acu_apr_sp, queen=FALSE)
death_acu_jul_wm_r <- poly2nb(death_geo_acu_jul_sp, queen=FALSE)
death_acu_sep_wm_r <- poly2nb(death_geo_acu_sep_sp, queen=FALSE)

rsdeath_acu_jan_wm_r_w <- nb2listw(death_acu_jan_wm_r, style = "W", zero.policy = TRUE)
rsdeath_acu_apr_wm_r_w <- nb2listw(death_acu_apr_wm_r, style = "W", zero.policy = TRUE)
rsdeath_acu_jul_wm_r_w <- nb2listw(death_acu_jul_wm_r, style = "W", zero.policy = TRUE)
rsdeath_acu_sep_wm_r_w <- nb2listw(death_acu_sep_wm_r, style = "W", zero.policy = TRUE)

rsdeath_acu_jan_wm_r_b <- nb2listw(death_acu_jan_wm_r, style = "B", zero.policy = TRUE)
rsdeath_acu_apr_wm_r_b <- nb2listw(death_acu_apr_wm_r, style = "B", zero.policy = TRUE)
rsdeath_acu_jul_wm_r_b <- nb2listw(death_acu_jul_wm_r, style = "B", zero.policy = TRUE)
rsdeath_acu_sep_wm_r_b <- nb2listw(death_acu_sep_wm_r, style = "B", zero.policy = TRUE)

d_coords <- coordinates(death_geo_sp)
d_wm_d5 <- dnearneigh(d_coords, 0, 5, longlat = FALSE)

death_coords_jan_wm <- coordinates(death_geo_acu_jan_sp)
death_coords_apr_wm <- coordinates(death_geo_acu_apr_sp)
death_coords_jul_wm <- coordinates(death_geo_acu_jul_sp)
death_coords_sep_wm <- coordinates(death_geo_acu_sep_sp)

death_wm_d5_jan_wm_fix <- dnearneigh(death_coords_jan_wm, 0, 5, longlat = FALSE)
death_wm_d5_apr_wm_fix <- dnearneigh(death_coords_apr_wm, 0, 5, longlat = FALSE)
death_wm_d5_jul_wm_fix <- dnearneigh(death_coords_jul_wm, 0, 5, longlat = FALSE)
death_wm_d5_sep_wm_fix <- dnearneigh(death_coords_sep_wm, 0, 5, longlat = FALSE)

rsd_wm_d5_fix_w <- nb2listw(d_wm_d5, style = "W", zero.policy = TRUE)
rsd_wm_d5_fix_b <- nb2listw(d_wm_d5, style = "B", zero.policy = TRUE)

rsdeath_acu_jan_wm_fix_w <- nb2listw(death_wm_d5_jan_wm_fix, style = "W", zero.policy = TRUE)
rsdeath_acu_apr_wm_fix_w <- nb2listw(death_wm_d5_apr_wm_fix, style = "W", zero.policy = TRUE)
rsdeath_acu_jul_wm_fix_w <- nb2listw(death_wm_d5_jul_wm_fix, style = "W", zero.policy = TRUE)
rsdeath_acu_sep_wm_fix_w <- nb2listw(death_wm_d5_sep_wm_fix, style = "W", zero.policy = TRUE)

rsdeath_acu_jan_wm_fix_b <- nb2listw(death_wm_d5_jan_wm_fix, style = "B", zero.policy = TRUE)
rsdeath_acu_apr_wm_fix_b <- nb2listw(death_wm_d5_apr_wm_fix, style = "B", zero.policy = TRUE)
rsdeath_acu_jul_wm_fix_b <- nb2listw(death_wm_d5_jul_wm_fix, style = "B", zero.policy = TRUE)
rsdeath_acu_sep_wm_fix_b <- nb2listw(death_wm_d5_sep_wm_fix, style = "B", zero.policy = TRUE)

death_knn5_jan_wm_adap <- knn2nb(knearneigh(death_coords_jan_wm, k=5))
death_knn5_apr_wm_adap <- knn2nb(knearneigh(death_coords_apr_wm, k=5))
death_knn5_jul_wm_adap <- knn2nb(knearneigh(death_coords_jul_wm, k=5))
death_knn5_sep_wm_adap <- knn2nb(knearneigh(death_coords_sep_wm, k=5))

rsdeath_acu_jan_wm_adap_w <- nb2listw(death_knn5_jan_wm_adap, style = "W", zero.policy = TRUE)
rsdeath_acu_apr_wm_adap_w <- nb2listw(death_knn5_apr_wm_adap, style = "W", zero.policy = TRUE)
rsdeath_acu_jul_wm_adap_w <- nb2listw(death_knn5_jul_wm_adap, style = "W", zero.policy = TRUE)
rsdeath_acu_sep_wm_adap_w <- nb2listw(death_knn5_sep_wm_adap, style = "W", zero.policy = TRUE)

rsdeath_acu_jan_wm_adap_b <- nb2listw(death_knn5_jan_wm_adap, style = "B", zero.policy = TRUE)
rsdeath_acu_apr_wm_adap_b <- nb2listw(death_knn5_apr_wm_adap, style = "B", zero.policy = TRUE)
rsdeath_acu_jul_wm_adap_b <- nb2listw(death_knn5_jul_wm_adap, style = "B", zero.policy = TRUE)
rsdeath_acu_sep_wm_adap_b <- nb2listw(death_knn5_sep_wm_adap, style = "B", zero.policy = TRUE)


confirmed_wm_r <- poly2nb(confirmed_geo_sp, queen=FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("flatly"),
                 
     # Navigation Bar
     navbarPage(title="Geovid",
                fluid=TRUE, windowTitle="Geo-spatial Analysis for Covid in China ", selected="eda",
      
       

        # EDA Tab Panel
        tabPanel("Exploratory Data Analysis", value="eda", fluid=TRUE,icon=icon("virus"),
                 titlePanel("Exploratory Data Analysis"),
                 br(),
            sidebarLayout(position="left", fluid=TRUE,
                sidebarPanel(width=3, fluid=TRUE,
                     conditionalPanel(
                         'input.EDAset === "Choropleth" || input.EDAset === "Box Map" || input.EDAset === "Line Graph"',
                         sliderInput("monthInput",
                                     "Month",
                                     min = 01,
                                     max = 09,
                                     value = c(01,09),
                                     sep = ""),
                         radioGroupButtons(
                             inputId = "inputcase",
                             label = "Select one Input:",
                             choices = c("Confirmed Cases" = "confirmed", 
                                         "Recovered Cases" = "recovered",
                                         "Death Cases" = "death"),
                             direction = "vertical",
                             justified = TRUE,
                             selected = "confirmed")
                         )
                         
                     ),
                mainPanel(width=9, fluid=TRUE,
                          fluidRow(column
                                   (12,
                                       tabsetPanel(
                                           id = 'EDAset',
                                           tabPanel("Line Graph",
                                                    p("Line Graphs shows the total number of cases of Covid-19 in each month, showing trends such as spikes and falls, as well as the changes over the period of nine months."),
                                                    br(),
                                                    fluidRow(
                                                        column(12,
                                                               plotOutput(outputId = "newPlot")),
                                                        column(12,
                                                               plotOutput(outputId = "cumulPlot")))
                                           ),
                                           tabPanel("Choropleth",
                                                    p("Choropleth shows the distribution of cases throughout cities in China. Areas with a higher and lower concentration of cases can be observed. "),
                                                    br(),
                                                    column(12,
                                                           plotOutput(outputId="choropleth",
                                                                        width = "100%",
                                                                        height = "400px")
                                                    )
                                           ),
                                           tabPanel("Box Map",
                                                    p("Box map indicates the cities in China that lies on the outlier, using the interquartile range calculations to compute."),
                                                    br(),
                                                    column(12,
                                                    plotOutput(outputId = "boxmap",
                                                               width="100%",
                                                               height="400px")
                                                    )
                                           )
                                           
                                           ))
                                   )

                          )
                          
                

                    )
           
            ),
        tabPanel("Spatial Autocorrelation Analysis", value="corr", fluid=TRUE,icon=icon("viruses"),
                 titlePanel("Spatial Autocorrelation Analysis"),
                 br(),
                 sidebarLayout(position="left", fluid=TRUE,
                               sidebarPanel(width=3, fluid=TRUE,
                                            conditionalPanel(
                                                'input.CORRset === "Local Moran I"||input.CORRset === "LISA" || input.CORRset === "Moran I ScatterPlot"',
                                                sliderTextInput(
                                                  inputId = "corrmonthInput",
                                                  label = "Month", 
                                                  grid = TRUE,
                                                  force_edges = TRUE,
                                                  choices = c("1" = "jan",
                                                              "4" = "apr",
                                                              "7" = "jul",
                                                              "9" = "sep")
                                                ),
                                                radioGroupButtons(
                                                    inputId = "corrinputcase",
                                                    label = "Select one Input:",
                                                    choices = c("Confirmed Cases" = "confirmed", 
                                                                "Recovered Cases" = "recovered",
                                                                "Death Cases" = "death"),
                                                    direction = "vertical",
                                                    justified = TRUE, 
                                                    selected = "confirmed"),
                                                radioGroupButtons(
                                                    inputId = "corrinputmatrix",
                                                    label = "Select one Matrix:",
                                                    choices = c("Rook" = "r", 
                                                                "Queen" = "q",
                                                                "Fixed" = "fix",
                                                                "Adaptive" = "adap"),
                                                    direction = "vertical",
                                                    justified = TRUE, 
                                                    selected = "r"),
                                                radioGroupButtons(
                                                    inputId = "corrinputtype",
                                                    label = "Select one Type:",
                                                    choices = c("Binary" = "b", 
                                                                "Row" = "w"),
                                                    direction = "vertical",
                                                    justified = TRUE,
                                                    selected = "b")
                                            )
                                            
                               ),
                               mainPanel(width=9, fluid=TRUE,
                                         fluidRow(column
                                                  (12,
                                                      tabsetPanel(
                                                          id = 'CORRset',
                                                          
                                                          tabPanel("Local Moran I",
                                                                   p("Local Moran I shows the local correlation to its neighbours. The Local Moran I Map shows the level of positive or negative correlation significance across different cities."),
                                                                   br(),
                                                                   column(12,
                                                                          plotOutput(outputId="localmoran",
                                                                                     width = "100%",
                                                                                     height = "400px")
                                                                   )
                                                          ),
                                                          tabPanel("LISA",
                                                                   p("LISA shows the clustering areas which have significant positive or negative correlation. Areas with no significant signs of correlation are categorised into not significant."),
                                                                   br(),
                                                                   column(12,
                                                                          plotOutput(outputId="lisa",
                                                                                     width = "100%",
                                                                                     height = "400px")
                                                                   )
                                                          ),
                                                          tabPanel("Moran I ScatterPlot",
                                                                   p("Moran I Scatter plot shows the outliers in the study area. If points are above the best fit line, it indicates a positive correlation. It also shows the presence of outliers."),
                                                                   br(),
                                                                   column(12,
                                                                          plotOutput(outputId="morani",
                                                                                     width = "100%",
                                                                                     height = "400px")
                                                                   )
                                                          )
                                                      )
                                                    
                                                    )
                                         )
                               )
                               
                               
                               
                 )
                 
        ),
        
        
        # -----Data Panel
        tabPanel("Data", value="data", fluid=TRUE, icon=icon("database"),
               mainPanel( width=9,
                         tabsetPanel(
                             id = 'dataset',
                             tabPanel("Confirmed Cases", DT::dataTableOutput("mytable1")),
                             tabPanel("Recovered Cases", DT::dataTableOutput("mytable2")),
                             tabPanel("Death Cases", DT::dataTableOutput("mytable3"))
                         )
               ))
      )
     
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
  
    plot_filter <- reactive({
        filtered <- total_cases %>%
            filter(as.numeric(`month`) >= as.numeric(as.character(input$monthInput[1])),
                   as.numeric(`month`) <= as.numeric(as.character(input$monthInput[2])))
    })
    
    choro_filter <- reactive({
        choro_filtered <- total_geo_sf %>%
            group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
            filter(as.numeric(`month`) >= as.numeric(as.character(input$monthInput[1])),
                   as.numeric(`month`) <= as.numeric(as.character(input$monthInput[2]))) %>% 
            dplyr::select(-`month`) %>%
            summarise(confirmed_new_count = sum(`confirmed_new_count`),
                      recovered_new_count = sum(`recovered_new_count`),
                      death_new_count = sum(`death_new_count`)) %>%
            mutate(
                "Covid'19 confirmed rate (Per 10,000)" = (as.numeric(`confirmed_new_count`)/(as.numeric(`pop2010`)/10000)),
                "Covid'19 recovered rate (Per 10,000)" = (as.numeric(`recovered_new_count`)/(as.numeric(`pop2010`)/10000)),
                "Covid'19 death rate (Per 10,000)" = (as.numeric(`death_new_count`)/(as.numeric(`pop2010`)/10000)))
        #choro_filtered[is.na(choro_filtered)] <- 0
        
    })

    localplot_filter <- reactive({
        if (input$corrinputcase == "confirmed"){
            confirmed_geo_sf_sum <- confirmed_geo_sf %>%
                group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
                dplyr::select(-`month`) %>%
                summarise(confirmed_new_count = sum(`confirmed_new_count`)) %>%
                mutate("Covid'19 confirmed rate (Per 10,000)" = (as.numeric(`confirmed_new_count`)/(as.numeric(`pop2010`)/10000)))
            
            confirmed_geo_sp <- sf:::as_Spatial(confirmed_geo_sf_sum)
            
        }
        if (input$corrinputcase == "recovered"){
            recovered_geo_sf_sum <- recovered_geo_sf %>%
                group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
                dplyr::select(-`month`) %>%
                summarise(recovered_new_count = sum(`recovered_new_count`)) %>%
                mutate("Covid'19 recovered rate (Per 10,000)" = (as.numeric(`recovered_new_count`)/(as.numeric(`pop2010`)/10000)))  
            
            recovered_geo_sp <- sf:::as_Spatial(recovered_geo_sf_sum)
        }
        else {
            death_geo_sf_sum <- death_geo_sf %>%
                group_by(`City_EN`, `Prov_EN`, `pop2010`) %>%
                dplyr::select(-`month`) %>%
                summarise(death_new_count = sum(`death_new_count`)) %>%
                mutate("Covid'19 death rate (Per 10,000)" = (as.numeric(`death_new_count`)/(as.numeric(`pop2010`)/10000)))
            
            death_geo_sp <- sf:::as_Spatial(death_geo_sf_sum)
            
        }
            
    })
    
    
    
    
    output$mytable1 <- DT::renderDataTable({
        DF <- as.data.frame(confirmed_geo_sf)
        DF <- DF %>% 
            mutate_if(is.numeric, round, digits = 3)
        DT::datatable(DF,style = "bootstrap")
    })
    
    output$mytable2 <- DT::renderDataTable({
        DF <- as.data.frame(recovered_geo_sf)
        DF <- DF %>% 
            mutate_if(is.numeric, round, digits = 3)
        DT::datatable(DF,style = "bootstrap")
    })
    
    output$mytable3 <- DT::renderDataTable({
        DF <- as.data.frame(death_geo_sf)
        DF <- DF %>% 
            mutate_if(is.numeric, round, digits = 3)
        DT::datatable(DF,style = "bootstrap")
    })
    
    moran_fun <- reactive({
      df <- paste0(input$corrinputcase, "_geo_acu_", input$corrmonthInput, "_sp")
      val <- eval(paste(input$corrinputcase, "_cumul_count", sep = ""))
      wm <- paste0("rs", input$corrinputcase, "_acu_", input$corrmonthInput, "_wm_", input$corrinputmatrix, "_", input$corrinputtype)
      df_get <- get(df)
      
      moran.plot(as.vector(na.omit(df_get$val)), get(wm))
    })
    
    

    lisa_fun <- reactive({
      df <- paste0(input$corrinputcase, "_geo_acu_", input$corrmonthInput, "_sp")
      val <- eval(as.name(paste(input$corrinputcase, "_cumul_count", sep = "")))
      wm <- paste0("rs", input$corrinputcase, "_acu_", input$corrmonthInput, "_wm_", input$corrinputmatrix, "_", input$corrinputtype)
      df_get <- get(df)
      
      localMI <- localmoran(as.vector(na.omit(df_get$val)),get(wm))
      localMI_bind <- cbind(df,localMI)
      
      c_quadrant<-vector(mode="numeric",length=nrow(localMI))
      DV <- df$Z.cumulcount - mean(df$Z.cumulcount)     
      C_mI <- localMI[,1] - mean(localMI[,1])    
      signif <- 0.05       
      c_quadrant[DV >0 & C_mI>0] <- 4      
      c_quadrant[DV <0 & C_mI<0] <- 1      
      c_quadrant[DV <0 & C_mI>0] <- 2
      c_quadrant[DV >0 & C_mI<0] <- 3
      c_quadrant[localMI[,5]>signif] <- 0
      
      localMI_bind$quadrant <- c_quadrant
      colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
      clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
      moran_fun()
    })
    
    observe ({ output$morani <- renderPlot({
      moran_fun()
    })
        
    })
    
    observe ({ output$lisa <- renderPlot({
      
      tm_shape(lisa_fun()) +
        tm_fill(col = "quadrant", style = "cat", palette = colors[c(sort(unique(c_quadrant)))+1], labels = clusters[c(sort(unique(c_quadrant)))+1], popup.vars = c("City_EN")) +
        tm_view(set.zoom.limits = c(11,17)) +
        tm_borders(alpha=0.5)
    })})
    
    observe ({ output$localmoran <- renderPlot({
        

        tm_shape(china_3415) +
            tm_polygons(col = "azure4")+
            tm_shape(eval(as.name(paste(input$corrinputcase, "_geo_sp.", input$corrinputcase, "_localMI_",input$corrinputmatrix, "_", input$corrinputtype, sep = "")))) +
            tm_fill(col = "Ii", 
                    style = "pretty",
                    palette = "RdBu") +
            tm_layout(main.title = paste(input$corrinputcase, " local moran statistics",sep = ""),
                      main.title.position = "center",
                      main.title.size = 1,
                      legend.height = 0.40, 
                      legend.width = 0.35,
                      legend.outside = TRUE,
                      legend.outside.size = 0.2,
                      legend.outside.position = c("right", "bottom"),
                      frame = TRUE) +
            tm_borders(alpha = 0.8)
    })})

    
    observe ({ output$choropleth <- renderPlot({
        tmap_mode("plot")
        #tm_basemap(leaflet::providers$CartoDB.Positron)+
        
        #tm_basemap(leaflet::providers$CartoDB.Positron)+
        tm_shape(choro_filter(), bbox = st_bbox(choro_filter())) +
            tm_fill(eval(paste(input$inputcase, "_new_count", sep = "")),
                    style = "fixed",
                    breaks = c(0,100,300,700,1500,3000, 7000,Inf),
                    palette = "Blues" ) +
            tm_layout(main.title = paste("Distribution of ", input$inputcase, "cases"),
                      main.title.position = "center",
                      main.title.size = 1,
                      legend.width = 0.35,
                      legend.outside = FALSE,
                      legend.position = c("right", "bottom"),
                      frame = TRUE) +
            tm_borders(alpha = 0.2)
        
        
        
        
        #bins <- c(0,100,300,700,1500,3000, 7000,Inf)
        #pal <- colorBin("Reds", domain = as.numeric(unlist(choro_filter()[,eval(paste(input$inputcase, "_new_count", sep = ""))])), bins = bins)

        #leaflet(choro_filter()) %>%
         #   addProviderTiles(providers$CartoDB.Positron)%>%
         #  addPolygons(
         #       weight = 1,
         #       opacity = 1.0,
         #       smoothFactor = 0.5,
         #       fillOpacity = 0.5,
         #       color = ~pal(as.name(eval(paste(input$inputcase, "_new_count", sep = "")))),
         #       highlightOptions = highlightOptions(color = "white", 
         #                                           weight = 2, 
         ###                                           bringToFront = TRUE),
            #    popup = paste("<b>Name:</b> ", 
            #                  choro_filter()[,1], "<br>",
            #                  "<b>Number of Cases:</b> ", 
            #                  as.numeric(unlist(choro_filter()[,eval(paste(input$inputcase, "_new_count", sep = ""))])))) %>%
            #addLegend(pal = pal, values = ~as.name(eval(paste(input$inputcase, "_new_count", sep = ""))), opacity = .5, title = "Distibution of cases")
    })
    })
    
    observe ({ output$boxmap <- renderPlot({
        #choro_filter()[is.na(choro_filter())] <- 0
        tmap_mode("plot")
        
       
        boxmap(paste("Covid'19 ",as.character(input$inputcase), " rate (Per 10,000)", sep = ""),
               choro_filter())
    })
    })

    output$newPlot <- renderPlot({
        
        name <- as.character(paste(input$inputcase, "_new", sep = ""))
        
        ggplot(data = plot_filter(), aes( x=`month`, y=eval(as.name(paste(input$inputcase, "_new", sep = ""))),group = 1 )) +
            geom_point()+
            geom_line(aes(y=eval(as.name(paste(input$inputcase, "_new", sep = "")))), color="darkred") +
            scale_x_discrete(breaks = c(seq(from = 1, to = 9, by = 1)),
                               labels = c(seq(from = 1, to = 9, by = 1) ))+
            ggrepel::geom_text_repel(aes(label = eval(as.name(paste(input$inputcase, "_new", sep = "")))))+
            xlab("Months") +
            ylab("Number of cases")+
            ggtitle("Number of New Cases by Month")+
            theme_hc() +
            theme(plot.title = element_text(lineheight=.8, face="bold",size = 20))+
            scale_colour_hc()
        
        
        
    })
    
    output$cumulPlot <- renderPlot({


        ggplot(data = plot_filter(), aes( x=`month`, y=eval(as.name(paste(input$inputcase, "_cumul", sep = ""))),group = 1 )) +
            geom_point()+
            geom_line(linetype="twodash") +
            scale_x_discrete(breaks = c(seq(from = 1, to = 9, by = 1)),
                             labels = c(seq(from = 1, to = 9, by = 1) ))+
            ggrepel::geom_text_repel(aes(label = eval(as.name(paste(input$inputcase, "_cumul", sep = "")))))+
            xlab("Months") +
            ylab("Number of cases")+
            ggtitle("Number of Cumulative Cases by Month")+
            theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))+
            theme_hc() + scale_colour_hc()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
