







shp_fn <- file.path("data", "Population2016", "Ilots-agglo-pop.shp")
shp <- read_sf(shp_fn)
in_shp <- wbe_gis_get_postal_codes_in_shp(data_shp = shp)
in_shp %>% write_csv(file.path("data", "Population2016", "Ilots-agglo-pop_postalCode"))
a %>% filter()




shp_fn <- file.path("data", "Toronto_GIS Files", "WWTP_serving_area_20180316.shp")
dat_fn <- file.path("data", "Toronto_GIS Files", "WWTP - COVID-19 - Feb 22 2021.csv")
sv_fn <- file.path("data", "Toronto_GIS Files", "summary_episode_dates.csv")


pc_nm <- "Postal_Code"
dt_col <- "Episode_Date"
dt_typ <- "episode"






dat <- read_csv(dat_fn)
shp <- read_sf(shp_fn)



ret_val <-
    wbe_gis_summarize_postal_in_shp(data = dat,
                                    data_shp = shp,
                                    postal_col = pc_nm,
                                    date_col = dt_col,
                                    date_type = dt_typ
    )


ret_val %>% write.csv(sv_fn)
