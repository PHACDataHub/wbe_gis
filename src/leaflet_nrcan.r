library(tidyverse)
library(leaflet)
library(glue)




##########################################
#'
#' Returns EPSG number used by NRCAN tiles
#'
leaflet_nrcan_crs_num <- function(){3978}

leaflet_nrcan_crs_proj4def <- function(){
    '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
}



##########################################
#'
#' Returns a CRS for NRCAN tiles needed for leaflet
#' Note: this is kinda brittle and I think all the little specific numbers on res and bnds need to be exact... its wierd
#'
leaflet_nrcan_crs <- function(){

    orgn <- c(-34655800, 39310000)

    bnds =  c(-7786476.885838887,
              -5153821.09213678,
              7148753.233541353,
              7928343.534071138
    )


    res <- c(
        38364.660062653464,
        22489.62831258996,
        13229.193125052918,
        7937.5158750317505,
        4630.2175937685215,
        2645.8386250105837,
        1587.5031750063501,
        926.0435187537042,
        529.1677250021168,
        317.50063500127004,
        185.20870375074085,
        111.12522225044451,
        66.1459656252646,
        38.36466006265346,
        22.48962831258996,
        13.229193125052918,
        7.9375158750317505,
        4.6302175937685215,
        2.6458386250105836,
        1.5875031750063502,
        0.92604351875370428,
        0.52916772500211673,
        0.31750063500127002,
        0.18520870375074083,
        0.11112522225044451,
        0.066145965625264591
    )

    leafletCRS(
        crsClass = "L.Proj.CRS",
        code = glue("EPSG:{leaflet_nrcan_crs_num()}"),
        proj4def = leaflet_nrcan_crs_proj4def(),
        origin = orgn,
        bounds =  bnds,
        resolutions = res
    )
}


leaflet_nrcan <- function(...){


    urlTemplate = "https://geoappext.nrcan.gc.ca/arcgis/rest/services/BaseMaps/CBMT3978/MapServer/tile/{z}/{y}/{x}?m4h=t"

    tile_attrib <- "base from geoappext.nrcan.gc.ca"

    epsg3978 <- leaflet_nrcan_crs()

    m <- leaflet(options = leafletOptions(worldCopyJump = F,
                                          crs = epsg3978,
                                          minZoom = 2, maxZoom = 17, ...)
    ) %>%
        addTiles(urlTemplate = urlTemplate,
                 attribution = tile_attrib,
                 options = tileOptions(continuousWorld = F)
        ) %>%
        # addMarkers(lng = -75.705793,
        #            lat = 45.345134,
        #            popup = "My office."
        # ) %>%
        setView(lng = -1*(96+(40/60)+(35/3600)),
                lat = 62+(24/60),
                zoom = 3
        )


    m
}

leaflet_nrcan_point_extract <- function(shp){
    tmp<-
        shp %>%
        leaflet_nrcan_shp_transform()
    do.call(rbind, st_geometry(tmp)) %>%
        as_tibble() %>% setNames(c("lng","lat"))

}



#####################################
#'
#'unsure why but we needto transform shp files to ESPG 4326 WGS84 before sending to leaflet
#'
#'
leaflet_nrcan_shp_transform <- function(shp){
    st_transform(shp, 4326)
}
