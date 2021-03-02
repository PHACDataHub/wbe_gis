
library(tidyverse)
library(rgeos)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(janitor)
library(curl)
library(jsonlite)
library(glue)
library(encryptr)
library(digest)
library(keyring)
library(stringi)
library(openssl)
library(sodium)
library(rstudioapi)




WBE_GIS_PCCF_HELPPER_FN <- file.path("data","PCCF", "Geography_2016_Geographie" , "pccf_reading_helper.txt")
WBE_GIS_PCCF_FN <- file.path("data","PCCF", "Geography_2016_Geographie" , "pccfNat_fccpNat_062017.txt")
WBE_GIS_PCCF_ENCRYPTED_FN <- file.path("data","PCCF", "PCCF_ENCRYPTED.rds")


########################################
# Needed to vectorise Digest function in digest library
wbe_gis_VDigest_func <- getVDigest()





################################################
#'
#' returns vector of same size as that is fed in, but it hashs the data first, by unique postal code
#'
#' @example
#' wbe_gis_postal_hash(postal = c("k2B 7X6", "k2B7x6", "k2B7x6", "J2W2V5", "H9G2Y3", "M4P3B4", "B5A4G6", "T6L2Z6"))
wbe_gis_postal_hash <- function(postal){
    postal %>%
        toupper() %>%
        gsub(pattern = "[[:space:]]", replacement = "", x = .) %>%
        wbe_gis_VDigest_func()
}




################################################
#'
#' gets a passkey remember to set
#'
#' @example
#' wbe_gis_passkey()
wbe_gis_passkey <- function(){
    sha256(charToRaw(wbe_gis_passkey_key()))
}




################################################
#'
#' gets a passkey key remember to set
#'
#' @example
#' wbe_gis_passkey_key()
wbe_gis_passkey_key <- function(){
    # tryCatch({
        key_get("wbe_gis_pccf")
    # }, error=function(cond) {
    #     tmppw <- rstudioapi::askForPassword(prompt = "Please enter your password")
    #     wbe_gis_passkey_set(tmppw)
    #
    # }
    # , warning=function(cond) {
    #
    #
    # }, finally={
    #
    #
    #
    # }
    #)

}
wbe_gis_passkey_key_set_ask()

################################################
#'
wbe_gis_passkey_key_set_ask <- function(){
    rstudioapi::askForPassword(prompt = "Please enter your password") %>%
        wbe_gis_passkey_set()
}

################################################
#'
#' gets a google api key
#'
#' @example
#' wbe_gis_google_api_key()
wbe_gis_google_api_key <- function(){
    key_get("wbe_gis_google_api_key")
}


################################################
#'
#' google api key
#'
#' @example
#' wbe_gis_google_api_key_set(SOME_THING_SOME_THING)
wbe_gis_google_api_key_set <- function(value){
    key_set_with_value("wbe_gis_google_api_key", password = value)
}


################################################
#'
#' mapquest api key
#'
#' @example
#' wbe_gis_map_quest_api_key()
wbe_gis_map_quest_api_key <- function(){
    key_get("wbe_gis_map_quest_api_key")
}



################################################
#'
#' mapquest api key
#'
#' @example
#' wbe_gis_map_quest_api_key_set(SOME_THING_SOME_THING)
wbe_gis_map_quest_api_key_set <- function(value){
    key_set_with_value("wbe_gis_map_quest_api_key", password = value)
}

#WBE_GOOGLE_API_BASE_URL = "https://maps.googleapis.com/maps/api/geocode/json?address={address}&key={WBE_GOOGLE_API_KEY}"






################################################
#'
#' sets a passkey also see
#' wbe_gis_passkey and wbe_gis_passkey_set_with_value
#'
#' @example
#' wbe_gis_passkey_set()
# wbe_gis_passkey_set <- function(){
#     keyring::key_set_with_value("wbe_gis_pccf", password = stri_rand_strings(1,25))
# }





################################################
#'
#' sets a passkey with a value
#' wbe_gis_passkey_key()
#'
#' @example
#' wbe_gis_passkey_set()
#' wbe_gis_passkey_set(value = "SOME_LONG_STRING_THAT_IS_NOT_THIS")
wbe_gis_passkey_set <- function(value = stri_rand_strings(1,25)){
    key_set_with_value(service = "wbe_gis_pccf", password = value)
}





################################################
#'
#' takes a vector x_v and return and vector of same length, it encrypts with a key,
#' remember to set the key with key_set("wbe_gis_pccf") befor running this function
#'
#' @example
#' wbe_gis_encrypt(x_v = c("a", "b", "c"))
wbe_gis_encrypt <- function(x_v){
    lapply(x_v, function(x){
        x.raw <- serialize(x, NULL)
        data_encrypt(x.raw, key = wbe_gis_passkey())
    })
}



################################################
#'
#' takes a vector x_v and return and vector of same length, it encrypts with a key,
#' remember to set the key with key_set("wbe_gis_pccf") befor running this function
#'
#' @example
#' wbe_gis_decrypt(x_v = wbe_gis_match_postal(postal = c("k2B 7X6", "k2B7x6", "k2B7x6", "J2W2V5", "H9G2Y3", "M4P3B4", "B5A4G6", "T6L2Z6", "Z9Z9Z9")) %>%  pull(LAT))
wbe_gis_decrypt <- function(x_v){
    lapply(x_v, function(x){
        if (is.null(x)){
            return(0)
        }
        #x.raw <- serialize(x, NULL)
        unserialize(data_decrypt(x, key = wbe_gis_passkey()))
    }) %>% unlist()
}





################################################
#'
#' reads in the unencrypted pccf file
#' note: this only works if the pccf file actually exists
#'
#' @example
#' wbe_gis_read_pccf()
wbe_gis_read_pccf <- function(helper_fn  =WBE_GIS_PCCF_HELPPER_FN,
                              fn = WBE_GIS_PCCF_FN
                              ){
    pccf_helper <-
        helper_fn %>%
        read_tsv() %>%
        clean_names() %>%
        mutate(field_name = iconv(field_name)) %>%
        mutate(type = tolower(type))



    tic <- Sys.time()
    pccf <-
        fn %>%
        read_fwf(col_positions = fwf_widths(widths = pccf_helper$size),
                 col_types  = pccf_helper$type %>% paste0(collapse = "")) %>%
        setNames(pccf_helper$field_name)
    toc <- Sys.time()
    print(toc-tic)
    return(pccf)
}


################################################
#'
#' Reads in the PCCF file, summarizes it hashes it and encrypts it,
#' It then saves it to disk as a native r object *.rds file
#' takes about 90 minutes on my laptime
#'
#' @example
#' wbe_gis_encrypt_pccf()
wbe_gis_encrypt_pccf <- function(helper_fn  =WBE_GIS_PCCF_HELPPER_FN,
                                 fn = WBE_GIS_PCCF_FN,
                                 enc_fn_location = WBE_GIS_PCCF_ENCRYPTED_FN){

    pccf_helper <-
        helper_fn %>%
        read_tsv() %>%
        clean_names() %>%
        mutate(field_name = iconv(field_name)) %>%
        mutate(type = tolower(type))



    tic <- Sys.time()
    pccf <-
        fn %>%
        read_fwf(col_positions = fwf_widths(widths = pccf_helper$size),
                 col_types  = pccf_helper$type %>% paste0(collapse = "")) %>%
        setNames(pccf_helper$field_name)
    toc <- Sys.time()
    print(toc-tic)





    tic <- Sys.time()
    print(Sys.time())
    pccf_encr <-
        pccf %>%
        group_by(Postal) %>%
        summarise(LAT = mean(LAT, na.rm = T), LONG = mean(LONG, na.rm = T)) %>%
        #distinct(Postal, LAT, LONG) %>%
        mutate(Postal = wbe_gis_postal_hash(Postal)) %>% #head(2^17) %>%
        mutate(LAT = wbe_gis_encrypt(LAT)) %>%
        mutate(LONG = wbe_gis_encrypt(LONG))
    toc <- Sys.time()
    print(toc-tic)

    tic <- Sys.time()
    pccf_encr %>% write_rds(file = enc_fn_location, compress = "none")
    toc <- Sys.time()
    print(toc-tic)


    return(TRUE)
}





############################################
#'
#' Returns the encrypted and hashed pccf file (assuming it has already been encrypted and hashed
#'
#' @example
#' wbe_gis_encrypted_pccf()
wbe_gis_encrypted_pccf <-  function(enc_fn_location = WBE_GIS_PCCF_ENCRYPTED_FN){
    read_rds(file = enc_fn_location)
}




############################################
#'
#' Returns Dataframe same size as input vector postal, this dataframe will have a hashed postal code and an encrypted LAT LONG
#'
#' @example
#' wbe_gis_match_postal(postal = c("k2B 7X6", "k2B7x6", "k2B7x6", "J2W2V5", "H9G2Y3", "M4P3B4", "B5A4G6", "T6L2Z6"))
wbe_gis_match_postal <- function(postal){
    tibble( postal_raw = postal, Postal = wbe_gis_postal_hash(postal)) %>%
        left_join(wbe_gis_encrypted_pccf(), by = "Postal")
}



#####################################################
#' returns points where each postal code is as a st object
#'
#'
#'
wbe_gis_match_postal_df <- function(data, postal_col = "postal",
                                 pnts_coords_cols = c("LONG", "LAT"),
                                 pnts_crs = 4326 # defaults to WGS 84
){

    postals_in_Polygon <-
        data[[postal_col]] %>% unique() %>%
        wbe_gis_match_postal() %>%
        mutate(LAT = wbe_gis_decrypt(LAT)) %>%
        mutate(LONG = wbe_gis_decrypt(LONG)) %>%
        st_as_sf(x = .,
                 coords = pnts_coords_cols,
                 crs = pnts_crs,        # defaults to WGS 84
                 stringsAsFactors = FALSE,
                 remove = FALSE
        )
    postals_in_Polygon
}

# #leaflet_nrcan_shp_transform(postals_in_Polygon)
# #a<-st_transform(postals_in_Polygon , leaflet_nrcan_crs_num())
# st_geometry(a) %>% as_tibble() %>% setNames(c("lon","lat"))



#####################################################
#'
#' returns a dataframe joining  a data frame with postal codes and a polygonal shp file
#' class(data_shp) should be an "sf" type
#'
#'
#' @example
#' data <- tibble(postal = c("k2B 7X6", "k2B7x6", "k2B7x6", "K2E 7L9", "J2W2V5", "H9G2Y3", "M4P3B4", "B5A4G6", "T6L2Z6", "Z9Z9Z9")) %>% mutate(index = 1:nrow(.))
#' data_shp <- read_sf(file.path("data", "HR_000a18a_e", "HR_000a18a_e.shp"))
#' wbe_gis_match_postal_in_shp(data, data_shp)
wbe_gis_match_postal_in_shp <- function(data, data_shp, postal_col = "postal",
                                        pnts_coords_cols = c("LONG", "LAT"),
                                        pnts_crs = 4326, # defaults to WGS 84
                                        polygon_prefix = "polygon_"
                                        ){

    data_shp_poly <-
        data_shp %>%
        rename_all(function(x){paste0(polygon_prefix, x)}) %>%
        mutate(always_true = TRUE)


    data_shp_crs <- st_crs(data_shp)

    data_tmp <-
        data %>%
        mutate(TEM_RAND_KEY = 1:nrow(.))
    postals_in_Polygon <-
        data_tmp[[postal_col]] %>% unique() %>%
        wbe_gis_match_postal() %>%
        #filter(!is.null(unlist(LAT)))
        mutate(LAT = wbe_gis_decrypt(LAT)) %>%
        mutate(LONG = wbe_gis_decrypt(LONG)) %>%
        st_as_sf(x = .,
             coords = pnts_coords_cols,
             crs = pnts_crs,        # defaults to WGS 84
             stringsAsFactors = FALSE,
             remove = FALSE
    )  %>%
        st_transform(data_shp_crs) %>%
        st_join(data_shp_poly, join = st_within) %>%
        mutate(found_postal_in_pccf = !is.na(always_true)) %>%
        select(-always_true)


    data_tmp %>%
        rename(postal_raw = !!sym(postal_col))    %>%
        left_join(postals_in_Polygon, by ="postal_raw") %>%
        st_as_sf(
            coords = pnts_coords_cols,
            crs = pnts_crs,        # defaults to WGS 84
            stringsAsFactors = FALSE,
            remove = TRUE
        ) %>%
        select(-geometry, -TEM_RAND_KEY, -Postal) %>%
        rename(!!sym(postal_col) := postal_raw)
}


#list.files(pattern='*\\.shp', recursive=TRUE)


##############################################
#'
#' produces a summary of case data by polygon shape file and date
#'
#' @example
#' data <- wbe_gis_gererate_fake_covid_data()
#' data <- read_csv(file.path("data", "fake_covid_data.csv"))
#' data_shp <- read_sf(file.path("data", "HR_000a18a_e", "HR_000a18a_e.shp"))
#' comb_data <- wbe_gis_summarize_postal_in_shp(data, data_shp)
wbe_gis_summarize_postal_in_shp <- function(data,
                                            data_shp,
                                            date_col = "episode_date",
                                            date_type = "episode",
                                            postal_col = "postal_code",
                                            case_type = "conf"){

    data <-
        data %>%
        mutate(postal_exists = !is.na(!!sym(postal_col)))


    data_shp_crs <- st_crs(data_shp)


    polygon_prefix = "polygon_"
    #polygon_suffix = ""
    case_data_polygon <- wbe_gis_match_postal_in_shp (data = data, data_shp = data_shp, postal_col = postal_col, polygon_prefix = polygon_prefix)


    columns <- case_data_polygon %>% colnames() %>% grep(pattern = polygon_prefix , x = ., ignore.case = T, value = T)

    # case_data_polygon %>%
    #     group_by_at(vars(one_of(columns))) %>%
    #     summarise(value = n()) %>%
    #     rename_at(vars(one_of(columns)), function(x){gsub(pattern = polygon_prefix, replacement = "", x = x)}) %>%
    #     left_join(data_shp,. )


    columns2 <- columns %>% c(date_col, "postal_exists", "found_postal_in_pccf")



    case_data_polygon %>%
        as.tibble() %>%
        group_by_at(vars(one_of(columns2))) %>%
        summarise(value = n()) %>%
        #rename_at(vars(starts_with(polygon_prefix)), function(x){gsub(pattern = polygon_prefix, replacement = "", x = x)}) %>%
        rename(date := date_col) %>%
        mutate(dateType = date_type) %>%
        mutate(type = case_type) %>%
        ungroup() %>% #count(postal_exists, found_postal_in_pccf)
        relocate(date, postal_exists, found_postal_in_pccf, value)
}


##############################################
#'
#' generates some sample case data for me to test with, this can be done if the the pccf file exists
#'
#'
#' @example
#' dat <- wbe_gis_gererate_fake_covid_data()
#' dat %>% write_csv(file.path("data", "fake_covid_data.csv"))
wbe_gis_gererate_fake_covid_data <- function(n = 50000, postal_pattern = "^M"){

    pccf <- wbe_gis_read_pccf()

    pcs <-
        pccf$Postal %>%
        grep(pattern = postal_pattern, x = ., value = T) %>%
        unique() %>%
        sample(x = ., size = floor(length(.)/10),replace = F) %>%
        sample(x = ., size = n, prob = 1.1^(1:length(.)),replace = T)
    #fake_cases <-
        tibble(
           postal_code = pcs ,
           onset_date = sample(seq(as.Date('2020/01/01'), Sys.Date(), by="day"), size = n , replace = T),
           reported_date_delta = sample(seq(-1, 30), size = n , replace = T),
           lab_date_delta = sample(seq(-3, 3), size = n , replace = T),
           end_date_delta = sample(seq(12, 55), size = n , replace = T),
           age = sample(seq(2, 107), size = n , replace = T),
           gender = sample(c("m", "f", "other"), prob = c(0.49, 0.49, 0.02), size = n , replace = T),
           status = sample(c("recovered", "dead", "other"), prob = c(0.9, 0.08, 0.02), size = n , replace = T)
           ) %>%
        mutate(reported_date = onset_date + reported_date_delta) %>%
        mutate(lab_date = onset_date + lab_date_delta) %>%
        mutate(recovered_date = if_else(status == "recovered", onset_date + end_date_delta, as.Date(NA) )) %>%
        mutate(dead_date = if_else(status == "dead", onset_date + end_date_delta , as.Date(NA) )) %>%
        select(-lab_date_delta , -reported_date_delta , -end_date_delta) %>%
        apply (., 2, function(x) {x[sample( c(1:n), floor(n/5))] <- NA; x} ) %>%
        as.tibble() %>%
        mutate(case_id = stri_rand_strings(n,25)) %>%
        mutate(age = as.integer(age)) %>%
        mutate_at(vars(matches("_date")), as.Date) %>%
        mutate(episode_date = pmin(onset_date, reported_date, lab_date , na.rm = T))
}






