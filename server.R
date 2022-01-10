# Shiny APP for Gap analysis landraces project 
# Dashboar to show results for all crops

#suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(rgdal)}else{library(rgdal)})
suppressMessages(if(!require(rgeos)){install.packages("rgeos");library(rgeos)}else{library(rgeos)})
suppressMessages(if(!require(sp)){install.packages("sp");library(sp)}else{library(sp)})
suppressMessages(if(!require(sf)){install.packages("sf");library(sf)}else{library(sf)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})
suppressMessages(if(!require(rlang)){install.packages("rlang");library(rlang)}else{library(rlang)})
suppressMessages(if(!require(shinycssloaders)){install.packages("shinycssloaders");library(shinycssloaders)}else{library(shinycssloaders)})
suppressMessages(if(!require(pROC)){install.packages("pROC");library(pROC)}else{library(pROC)})
suppressMessages(if(!require(googleVis)){install.packages("googleVis");library(googleVis)}else{library(googleVis)})
suppressMessages(if(!require(highcharter)){install.packages("highcharter");library(highcharter)}else{library(highcharter)})
suppressMessages(if(!require(RColorBrewer)){install.packages("RColorBrewer");library(RColorBrewer)}else{library(RColorBrewer)})
suppressMessages(if(!require(collapsibleTree)){install.packages("collapsibleTree");library(collapsibleTree)}else{library(collapsibleTree)})
suppressMessages(if(!require(shinydashboardPlus)){install.packages("shinydashboardPlus");library(shinydashboardPlus)}else{library(shinydashboardPlus)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(shinyWidgets)){install.packages("shinyWidgets");library(shinyWidgets)}else{library(shinyWidgets)})
suppressMessages(if(!require(lubridate)){install.packages("lubridate");library(lubridate)}else{library(lubridate)})
suppressMessages(if(!require(bsplus)){install.packages("bsplus");library(bsplus)}else{library(bsplus)})
suppressMessages(if(!require(stringr)){install.packages("stringr");library(stringr)}else{library(stringr)})
suppressMessages(if(!require(stringi)){install.packages("stringi");library(stringi)}else{library(stringi)})
suppressMessages(if(!require(shinyjs)){install.packages("shinyjs");library(shinyjs)}else{library(shinyjs)})
suppressMessages(if(!require(htmltools)){install.packages("htmltools");library(htmltools)}else{library(htmltools)})
suppressMessages(if(!require(devtools)){install.packages("devtools");library(devtools)}else{library(devtools)})
suppressMessages(if(!require(googleway)){devtools::install_github("SymbolixAU/googleway");library(googleway)}else{library(googleway)})
suppressMessages(if(!require(colorspace)){install.packages("colorspace");library(colorspace)}else{library(colorspace)})
suppressMessages(if(!require(zip)){install.packages("zip");library(zip)}else{library(zip)})
suppressMessages(if(!require(readr)){install.packages("readr");library(readr)}else{library(readr)})
suppressMessages(if(!require(googlePolylines)){install.packages("googlePolylines");library(googlePolylines)}else{library(googlePolylines)})
suppressMessages(if(!require(curl)){install.packages("curl", type="source");library(curl)}else{library(curl)})
suppressMessages(if(!require(RCurl)){install.packages("RCurl", type="source");library(RCurl)}else{library(RCurl)})
suppressMessages(if(!require(RColorBrewer)){install.packages("RColorBrewer");library(RColorBrewer)}else{library(RColorBrewer)})


short_info <- function(input, title, place){
  
  input %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = title, placement = place))
}

source("www/helpers.R")
source("www/google_directions_custom.R")
load("www/results_all_crops.RData")
app_credentials <- readRDS("www/credentials.rds")
shp <- shapefile("www/world_shape_simplified/all_countries_simplified.shp")
ctrs <-   shp@data %>% 
  dplyr::select(ADMIN, ISO_A2, FIPS_10_) %>% 
  dplyr::mutate(ISO_A2 = ifelse(ISO_A2 == "-99", NA_character_, ISO_A2),
                FIPS_10_ = ifelse(FIPS_10_ == "-99", NA_character_, FIPS_10_),
                A2 = case_when(
                  !is.na(ISO_A2)& !is.na(FIPS_10_) ~ ISO_A2,
                  is.na(ISO_A2) & !is.na(FIPS_10_) ~ FIPS_10_,
                  !is.na(ISO_A2)& is.na(FIPS_10_) ~ ISO_A2,
                  TRUE ~ NA_character_
                ) ) %>%
  dplyr::arrange(., ADMIN) %>%
  dplyr::filter(!is.na(A2))

map_key <- app_credentials$map_key  
hc_add_event_series2 <- function (hc, series = "series", event = "click") {
   fun <- paste0("function(e){\n  var seriesinfo = {name: e.point.name }\n  console.log(seriesinfo);\n  window.x = this;\n  if (typeof Shiny != 'undefined') { Shiny.onInputChange(this.chart.renderTo.id + '_' + '", 
                 event, "', seriesinfo); }\n\n}")
 
  
  fun <- JS(fun)
  eventobj <- structure(list(structure(list(structure(list(fun), 
                                                      .Names = event)), .Names = "events")), .Names = series)
  hc$x$hc_opts$plotOptions <- rlist::list.merge(hc$x$hc_opts$plotOptions, 
                                                eventobj)
  hc
}

### Retrieve data from FTP server 

download_sftp_files  <- function(crop_name, user, pass){
  
  ftp_url <- paste0("sftp://", user, ":", pass, "@ftp.ciat.cgiar.org/srv/ftp/acmendez/spatial_lga")
  
  curl_download("https://eu.httpbin.org/get?bar=456", tmp)
  
  file_name  <- paste0("gap_richness_", crop_name, ".rds")
  
  occ_name   <- paste0(crop_name, "_used_occ_shp.rds")
  
  occ_url    <-  paste(ftp_url, crop_name, "databases", occ_name, sep = "/")
  
  file_url <- paste(ftp_url, crop_name, "results", file_name, sep =  "/")   
  
  tmp_gp_file <- paste(tempdir(), file_name, sep = "/")
  gp_rich <- RCurl::getBinaryURL(url = file_url)
  writeBin(object = gp_rich, con = tmp_gp_file )
  
  tmp_occ_file <-  paste(tempdir(), occ_name, sep = "/")
  occ <-  getBinaryURL(url = occ_url)
  writeBin(object = occ, con = tmp_occ_file)
  
  
  r <- readRDS(tmp_gp_file)
  occ_df <- readRDS(tmp_occ_file)
  
  ret <- list(shp = r, occ_shp = occ_df)
  
  return(ret)
}



## defining crops names and their respective icon
icons_names <- list.files("www/crops_icons", pattern = ".png$")
icons_names <- stringr::str_extract(icons_names, "[a-zA-Z]+")
icons_paths <- data.frame(icons_names, paths = list.files("www/crops_icons", pattern = ".png$", full.names = T))

crops_names <- tibble(name = list.dirs("www/crops_data/", full.names = F, recursive = F)) %>% 
  dplyr::mutate(base_name = str_extract(name, "[a-zA-Z]+"),
                base_name = case_when(
                  name == "african_maize" ~ "maize",
                  #name == "common_bean" ~ "common_bean",
                  #name == "pearl_millet" ~  "pearl_millet",
                  #name == "sweet_potato"  ~ "sweet_potato",
                  TRUE ~ base_name
                )) %>%
  dplyr::left_join(., icons_paths, by = c("base_name" = "icons_names") ) %>%  
  dplyr::mutate(paths = stringr::str_replace(paths, "^www/", ""))

crops_names_ck <- read.csv("www/crops_names_CK.csv")
crops_names_group <- crops_names_ck %>% 
  dplyr::select(name, Further.app.needs) %>% 
  drop_na %>% 
  pull(name)
  

crops_names <- crops_names %>% 
  left_join(., crops_names_ck %>% dplyr::select(name, App.name)) %>% 
  dplyr::arrange(App.name) 

# Define server logic required 
server<- function(input, output, session) {
  
  ## definir valores Reactivos
  rv_list     <- shiny::reactiveValues()
  rv_list$occ <- data.frame()
  rv_list$groups_vis <- FALSE
  rv_list$flags <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", ctrs %>% pull(A2) %>% tolower())
  rv_list$countries <- ctrs %>% pull(ADMIN) %>% stringr::str_sort(., decreasing  = F)
  #rv_list$gap_richness_shp <- list()
  
  # reactive list to store collecting mission routes
  route <- reactiveValues()
  route$status <- c()
  route$places <- list()
  route$status_icon <- lapply(1:5, function(i){
    icon("fas fa-times-circle")
  })
  route$ties <- list()
  route$elevation <- list()
  route$Incoords <- list()
  route$url_route <- list()
  route$restore_session <- list(points_inf = list(), crops_inf = list(), places_inf= list(), valid_file = "valid")
  #route$path <- list()
  ##### calculate values for main dashoard page
  
  #- mean coverage for all crops
  avg_coverage <- do.call(rbind,consolidado@crop$crops_coverage) %>% 
    dplyr::mutate(mean = (lower+ upper)/2) %>% 
    dplyr:::select(lower, upper, mean) %>% 
    colMeans(., na.rm = T) %>% 
    round(., 1)
    
  
  #- Total number of occ in each country
   occ_counts <- purrr::reduce(consolidado@crop$accessions_count_per_country, left_join, by = c("ISO2", "country")) %>%
     dplyr::mutate(occ_count = rowSums(select_if(.,is.numeric), na.rm = T)) %>% 
     dplyr::select(country, ISO2, occ_count) 
 #- mean gap area for each country
   area_mean <- purrr::reduce(consolidado@crop$gap_area_country_crop, left_join, by = c("ISO2", "country")) %>% 
     dplyr::select(!matches("quantile")) %>% 
     dplyr::mutate_if(is.numeric, .funs =  function(i){ifelse(i == 0 , NA, i)}) %>% 
     dplyr::mutate(area_mean = rowMeans(select_if(.,is.numeric), na.rm = T),
                   area_mean = round(area_mean)) %>% 
     dplyr::select(country, ISO2, area_mean) 
   
   # consolidated data frame to use in interactive maps
   final_data <- consolidado@combined_results %>% 
      left_join(., occ_counts, by = c("ISO2", "country")) %>% 
      left_join(., area_mean,   by = c("ISO2", "country")) %>% 
      left_join(., consolidado@country_area,   by = c("ISO2", "country")) %>% 
      left_join(., consolidado@coverage_totals,   by = c("ISO2", "country")) %>% 
     dplyr::mutate(area_perc = round((area_mean/area_tot)*100,  2),
                   score = Total/max(Total),
                   covg_total_min = round(1-gap_total_min/sdm_total, 3),
                   covg_total_max = round(1-gap_total_max/sdm_total, 3),
                   covg_total     = round((covg_total_min+covg_total_max/2), 3)) %>% 
     dplyr::filter(Total != 0) %>% 
     drop_na() %>% 
     dplyr::rename("iso-a2"  = ISO2, "gap_count" = Total)
 
##### infoboxes
    
   occ_counts <- tibble(crop  = unlist(consolidado@crop$name),
                        n_occ = consolidado@crop$accessions_count %>% 
                          lapply(., function(i){
                            if(!any(is.na(i))){
                              x <- i %>% pluck(., "source_db_freq") %>% 
                                dplyr::pull(n) %>% 
                                sum(., narm = T)
                            }else{
                              x <- NA
                            }
                            return(x)}) %>% 
                          do.call(rbind, .) ) %>% 
     tidyr::drop_na() 
   
  total_occ <- occ_counts %>%
    dplyr::pull(n_occ) %>% 
    sum(., na.rm = T) %>% 
    prettyNum(., format = "fg", big.mark = ".", small.mark  = ".", decimal.mark = ",")
  
  total_country <- consolidado@crop$accessions_count_per_country %>% 
    do.call(rbind, .) %>% 
    dplyr::filter(!is.na(occ_count)) %>% 
    pull(country) %>% 
    unique %>% 
    length()
  
    output$infobox_total_crops <- renderInfoBox({
        infoBox(
            title = "Major crops",value =  paste0("\n",22), icon = icon("fab fa-pagelines"),
            color = "green"
        )
    })
    output$infobox_finished <- renderInfoBox({
        infoBox(
            title = "World Coverage", value = paste0(round(avg_coverage[3]), "%"), 
            subtitle = paste0("(", avg_coverage[1], "-", avg_coverage[2], ")" ),
            icon =  icon("fas fa-warehouse"),
            color = "yellow"
        )
    })
    
    output$infobox_countires <- renderInfoBox({
        infoBox(
            title = "Countries",value =  paste0(total_country), subtitle = " ", icon =  icon("fas fa-globe"),
            color = "red"
        )
    })
    output$infobox_tot_occ <- renderInfoBox({
        infoBox(
            title = "Occurrences",value =  total_occ, subtitle = " ", icon =  icon("fas fa-map-pin"),
            color = "blue"
        )
    })
    
    #mapa de total de gaps por pais
    
    output$hcontainer <- renderHighchart({
        
      #read.csv("D:/OneDrive - CGIAR/Attachments/Desktop/gaps_counts_per_countries.csv", stringsAsFactors = F, header = T)
       

        n <- 4
        stops <- data.frame(q = 0:n/n,
                            c = brewer.pal(n+1, "YlOrRd"),
                            stringsAsFactors = FALSE)
        stops <- list_parse2(stops)
        
        if(!input$change_tree){
          #mapdata <- get_data_from_map(download_map_data("custom/world-palestine-highres"))
          
          hc <- hcmap( "custom/world-palestine-highres", data = final_data,
                      joinBy = c("iso-a2"), 
                      value = "score", 
                      name= "Score",
                      showInLegend = FALSE,
                      nullColor = "#DADADA",
                      states = list(hover = list(color = "#a4edba")),
                      borderColor = "#FAFAFA", borderWidth = 0.1,
                      #dataLabels = list(enabled = F, format = '{point.name}'),
                      tooltip = list(valueDecimals = 2, valuePrefix = " ", valueSuffix = " Pts"),
                      download_map_data = F) %>% 
            hc_colorAxis( stops = stops, min = 0 , max = 1, labels = list(format = "{value}")) 
          # hc_colorAxis( minColor = "#FFFFFF", maxColor = "#434348")
          hc %>% 
            hc_add_event_series2( series = "series", event = "click")
          
        }else{
          final_data %>% 
            dplyr::select(country, score) %>% 
            dplyr::filter(score != 0) %>% 
            hchart(., "treemap", hcaes(x = country, value = score, color = score)) %>% 
            hc_add_event_series2( series = "series",event = "click")
          
        }
     
    })
    
    #create deafalt valueBoxes to  be uppdated after
    output$vBox_1 <- renderValueBox({
      valueBox(value = paste0(0), subtitle = "Occurrences" , icon = icon("fas fa-map-marker-alt"), color = "purple", width = 3 ) 
    })
    output$vBox_2 <- renderValueBox({
      valueBox(value = paste0(0), subtitle = "Gap area" , icon = icon("fas fa-chart-area"), color = "blue", width = 3 ) 
    })
    output$vBox_3 <- renderValueBox({
      valueBox(value = paste0(0), subtitle = "Coverage" , icon = icon("fas fa-globe-africa"), color = "green", width = 3 ) 
    })
    
    ##### Event to chaange valus of ValueBboxes due to MAP clicks
    observeEvent(input$hcontainer_click,{
      
      #print(input$hcontainer_click)
      country_name <- as.character(input$hcontainer_click$name)
      
      c_info <- final_data %>% 
        dplyr::mutate(country = tolower(country)) %>% 
        dplyr::filter(country == tolower(country_name))
      
      
      output$c_name <- renderText(country_name)
      output$vBox_1 <- renderValueBox({
        valueBox(value = prettyNum(c_info$occ_count, format = "fg", big.mark = ".", small.mark  = ".", decimal.mark = ","),
                 subtitle = "Occurrences",
                 icon = icon("fas fa-map-marker-alt"), 
                 color = "purple", width = 3) 
      })
      output$vBox_2 <- renderValueBox({
        valueBox(value =  paste0(prettyNum(c_info$gap_total_max, format = "fg", big.mark = ".", small.mark  = ".", decimal.mark = ","), " Km²"),
                 subtitle = "Gap area", 
                 icon = icon("fas fa-chart-area"), color = "blue", width = 3 ) 
      })
      output$vBox_3 <- renderValueBox({
        valueBox(value = paste0(c_info$covg_total*100, "%") ,
                 subtitle = "Country coverage" , 
                 icon = icon("fas fa-globe-africa"), color = "green", width = 3) 
      })
      
      
      output$descBlock_txt_1 <-  renderText(paste(c_info$gap_count, "Crops"))
      output$descBlock_txt_2 <-  renderText(paste(round(c_info$score, 2), "Pts") )
      
    })
    
    ####segundo panel de box para mostrar mas resultados
    
    output$hcontainer2 <- renderHighchart({
      
      ## occurrence number per data base source
      df <-  consolidado@crop$accessions_count %>% 
        lapply(., function(i){pluck(i, "source_db_freq")}) %>% 
        do.call(rbind, .) %>% 
        dplyr::mutate(source_db = dplyr::recode(source_db, "VIEWS" = "WIEWS"),
                      source_db = ifelse(grepl("GBIF", source_db), "GBIF", source_db),
                      source_lab = ifelse(!grepl("GBIF|WIEWS|USDA|GENESYS", source_db), "CGIAR", source_db)) %>%  
        dplyr::group_by(source_lab) %>% 
        dplyr::summarise(suma = sum(n, na.rm =T)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(total = sum(suma),
                      freq  = round(suma/total*100, 1),
                      total = NULL,
                      suma = NULL) %>% 
        dplyr::rename(name = source_lab, y = freq)
    #make the graph  
      highchart() %>%
        hc_title(text = "",
                 style = list(fontSize = "15px")) %>% 
        hc_chart(type = "pie",
                 polar = TRUE) %>% 
       hc_xAxis(categories = df$name) %>%
        hc_add_series(data = df, name = "Percentage", showInLegend = TRUE,showInLegend = TRUE,innerSize =  '60%',
                      tooltip = list(valueDecimals = 1, valuePrefix = " ", valueSuffix = " %")) %>% 
        hc_plotOptions(pie = list(dataLabels = list(enabled = FALSE)))
      
      
    })
    
    output$hcontainer3 <- renderHighchart({
      
        df <- occ_counts %>% 
          arrange(desc(n_occ)) %>% 
          slice(1:20)
      
      highchart() %>%
        hc_title(text = "",
                 style = list(fontSize = "15px")) %>% 
        hc_chart(type = "bar",
                 polar = FALSE) %>% 
        hc_xAxis(categories = df$crop) %>%
        hc_add_series(data = df$n_occ, name = "Occurrences", showInLegend = FALSE,color ="#1DBB50", 
                      tooltip = list(valueDecimals = 0, valuePrefix = " ", valueSuffix = ""),pointWidth = "13")
      
      
    })
    
    
    output$hcontainer4 <- renderHighchart({
      
      df <- do.call(rbind,consolidado@crop$crops_coverage) %>% 
        dplyr::mutate(mean = (lower+ upper)/2,
                      mean = round(mean)) %>% 
        dplyr:::select(mean) %>% 
        tibble::add_column(crop_name = consolidado@crop$name) %>% 
        tidyr::drop_na() %>% 
        dplyr::arrange(desc(mean)) %>% 
        dplyr::slice(1:20)
      
      
      highchart() %>%
        hc_title(text = "",
                 style = list(fontSize = "15px")) %>% 
        hc_chart(type = "bar",
                 polar = FALSE) %>% 
        hc_xAxis(categories = df$crop_name) %>%
        hc_add_series(data = df$mean, name = "Coverage", showInLegend = FALSE,color ="#848483", 
                      tooltip = list(valueDecimals = 0, valuePrefix = " ", valueSuffix = "%"),pointWidth = "13")
      
      
    })
    
    ### diversity tree
    output$dv_tree1 <- renderCollapsibleTree({

      dv_data <- read.csv("www/diversity_tree_template.csv") %>% 
        dplyr::add_row(start = NA, ends = "P. Vulgaris", .before = 1 ) %>% 
        dplyr::mutate(collected = runif(nrow(.), 50, 3000)) 
        
      
      collapsibleTree::collapsibleTreeNetwork(dv_data, attribute = "collected")
      

    })

    ##########################
    ##### google map ########
    ########################
    
    #print coordinates when click on map
    
    observeEvent(input$Gmap_map_click,{
      ev <- input$Gmap_map_click
      df <- paste("Latitude = ", ev$lat, ";", "Longitude = ",  ev$lon)
      output$map_LatLng <- renderText(({ unlist(df)}))
      print(df)
    })
    

    output$Gmap <- renderGoogle_map({
      google_map(key = map_key,
                 location = c(0,0),
                 zoom = 2,
                 search_box = T,
                 event_return_type = "list")
    })
    
    
    
    observeEvent(input$add_to_map ,{
      
      if(is.null(input$select_crops2)){
        shinyWidgets::sendSweetAlert(session, title = "Error", 
                                     text = "No crop has been selected from the list",
                                     type = "error")
      }else{
        
        
        updateMaterialSwitch(session, "hide_layer", value = FALSE)
        
        withBusyIndicatorServer("add_to_map",{
          
          crops <- input$select_crops2
          
          try(expr = {
            
            rv_list$map_layer_labels <- crops
            
            dwnloaded <- lapply(crops, function(i){
              
              crop_name <- i
             # dest_file_shp <- paste0("www/crops_data/",crop_name,"/", crop_name, "_used_occ_shp.rds")
              dest_file_gp <- paste0('www/crops_data/', crop_name, "/gap_richness_", crop_name, ".rds")
              
              
             # dest_file_shp <-  paste0("www/crops_data/",crop_name,"/", crop_name, "_used_occ_shp.rds")
              
              #file.exists(dest_file_shp) &
              if(file.exists(dest_file_gp)){
                
              #  occ_df <- readRDS(dest_file_shp)
                r <- readRDS(dest_file_gp)
                
                ret <- list(shp = r)#,# occ_shp = occ_df)
                
              }else{
                safeError("Download failed !!!")
                ret <- NULL
              }
              
              return(ret)
              # download_sftp_files(crop_name = i, 
              #                     user = app_credentials$sftp_user, 
              #                     pass = app_credentials$sftp_pass )
            })
            
            
          })
          if(exists("dwnloaded") ){
            
         
            pol <- lapply(dwnloaded, function(i){
              if("Spatial" %in% is(i$shp)){
                
                pol <- i$shp
                names(pol) <- "value"
               
              }else{
                pol <- NA
              }
              return(pol)
            }) %>% do.call(rbind,.)
            
            labels_nms <- rv_list$map_layer_labels
            
            pol@data <- data.frame(value = 1:(nrow(pol@data)))
            print(labels_nms)
            pol <- sf::st_as_sf(pol) %>% 
              st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>% 
              dplyr::mutate(Gaps_layer = labels_nms)
        
          
   #         occ <- lapply(dwnloaded, function(i){
  #            #if("Spatial" %in% is(i$occ_shp)){
  #            pol <- i$occ_shp
              #names(pol) <- "value"
              #}else{
                #pol <- NA
   #           #}
    #          return(pol)
       #     }) %>% bind_rows(.) 
     #         #do.call(rbind,.)
      #       if("list" %in% class(occ)){
       #        occ <- occ[[1]]
      #       }
            
       #    occ_pol <- sf::st_as_sf(occ, coords = c("Longitude", "Latitude")) %>% 
      #        st_set_crs("+proj=longlat +datum=WGS84 +no_defs") 
            
         
         #   if(!is.null(input$country_picker)){
              
        #      country_name <- input$country_picker
              
         #     country_shp <- shp[shp@data$ADMIN == country_name,]
          #    country_shp <- sf::st_as_sf(country_shp) %>% 
          #      st_set_crs("+proj=longlat +datum=WGS84 +no_defs")
             
           #   occ_pol <- sf::st_intersection(occ_pol, country_shp)
          #    pol  <- sf::st_intersection(pol, country_shp)
             
          #    if(rv_list$groups_vis & !is.null(input$select_groups)){
                
                
           #     occ_marks <- data.frame(Group =  occ_pol$ensemble,
            #                            Group_html = paste(tags$strong("Landracre Group: "), occ_pol$ensemble),
            #                            Longitude = st_coordinates(occ_pol)[,1],
            #                            Latitude = st_coordinates(occ_pol)[,2],
             #                           m_icon = "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png"
            #    )
            #  }else{
           #     occ_marks <- data.frame(Group =  occ_pol$org,
          #                              Group_html = paste(tags$strong("Type of accession: "), occ_pol$org),
           #                             Longitude = st_coordinates(occ_pol)[,1],
          #                              Latitude = st_coordinates(occ_pol)[,2],
           #                             m_icon = "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png"
            #    )
                
            #  }
              
              
          #  }
            

           
         #   rv_list$occ <- occ_marks
            rv_list$gap_richness_shp <- pol
            
          }
          
        
          
        })#end bussy indicator
        
      }#end else
      
      output$selected_crops <- renderText({ paste("Viewing:", paste(crops, collapse =  ","))})
      updateMaterialSwitch(session, "hide_layer", value = TRUE)
    })
    
    
    ##### make selectable menu to select crops for google maps 
    
    output$multiselect1 <- renderUI({
     
      shinyWidgets::multiInput(
        inputId = "select_crops2",
        label = "Select Major crops :", 
        choices = NULL,
        width = "100%",
        choiceNames = lapply(seq_along(1:nrow(crops_names)), 
                             function(i) htmltools::tagList(tags$img(src = crops_names$paths[i],
                                                          width = 30, 
                                                          height = 20), crops_names$App.name[i] )),
        choiceValues = crops_names$name)# %>%  
        #short_info(input = ., place = "top",title = "Select maximun three crops to download data.")

      
    })
    
    
 #   output$multiselect2 <- renderUI({
  #    req(input$select_crops2)
  #    crop <- input$select_crops2
      
 #     if(length(crop) == 1 ){
#        
 #       if( crop %in% crops_names_group ){
#          
 #         rv_list$groups_vis <- TRUE
          
  #        rv_list$dest_file_gp_groups <- list.files(paste0('www/crops_data/', crop, "/groups/"), pattern = "gap_richness_group", full.names = T)
          
  #        rv_list$groups_names <- gsub("([A-Za-z0-9_/]+)(group_)|(.rds)", "", rv_list$dest_file_gp_groups)
          
          
   #       shinyWidgets::multiInput(
  #          inputId = "select_groups",
  #          label = "Select Landrace Groups :", 
   #         choices = NULL,
  #          width = "100%",
   #         choiceNames = rv_list$groups_names,
    #        choiceValues = rv_list$groups_names)
    #    }
        
        
 #     }
      
      
    #})
  
    
    observeEvent(input$hide_layer,{
      
      req(rv_list$gap_richness_shp)
      
      
      crops <- rv_list$map_layer_labels
      pol <- rv_list$gap_richness_shp
      #occ_pol <- rv_list$occ
      # occ_pol <- occ_pol %>% 
      #   dplyr::mutate(Occurrence_layer = ifelse(value == 1, "Herbarium", "Germplasm"))
      
      
      if(length(crops) > 1){
        pal_colors <- brewer.pal(length(crops), "Set1")
        pal <- colorRampPalette(colors = pal_colors)
      }else{
        pal <- colorRampPalette(colors = c("#F90B0B"))
      }
      
      pal2 <- colorRampPalette(colors = c( "#070707", "#726E6E"))
      
    
      
      if(input$hide_layer){
        
        #nrow(occ_pol) == 0 &
        if( nrow(pol) == 0){
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols")# %>% 
          #  clear_markers(layer_id = "occ")
          
        }else if( nrow(pol) != 0){ #nrow(occ_pol) == 0 &
          
            
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols") %>% 
           # clear_markers(layer_id = "occ") %>% 
            add_polygons(pol,
                         #stroke_colour = "Gaps_layer",
                         stroke_weight = "2",
                         stroke_opacity = "0",
                         fill_colour = "Gaps_layer",
                         fill_opacity = "0.5",
                         update_map_view = F, 
                         legend = T, 
                         palette = pal,
                         layer_id = "pols")
        }else if(nrow(pol) == 0 ){ #& nrow(occ_pol) != 0
            
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols") %>% 
            clear_markers(layer_id = "occ") %>%
            add_markers(
              data = occ_pol,
              lat = "Latitude",
              lon = "Longitude",
              #colour= "col",
              marker_icon = "m_icon",
              mouse_over = "Group_html",
              #info_window = "Group_html",
              close_info_window = T,
              update_map_view = F,
              #label = "Group",
              focus_layer = F,
              layer_id = "occ"
            )
            # add_polygons(occ_pol,
            #              #stroke_colour = "Occurrence_layer",
            #              stroke_weight = "2",
            #              stroke_opacity = "0",
            #              fill_colour = "Occurrence_layer",
            #              fill_opacity = "0.7",
            #              update_map_view = F, 
            #              legend = T, 
            #              pal = pal2,
            #              layer_id = "occ") 
        }else{
          
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols") %>% 
           # clear_markers(layer_id = "occ") %>% 
            add_polygons(pol,
                         #stroke_colour = "Gaps_layer",
                         stroke_weight = "2",
                         stroke_opacity = "0",
                         fill_colour = "Gaps_layer",
                         fill_opacity = "0.5",
                         update_map_view = F, 
                         legend = T, 
                         palette = pal,
                         layer_id = "pols") %>%
            add_markers( data = occ_pol,
              lat = "Latitude",
              lon = "Longitude",
              #colour= "col",
              marker_icon = "m_icon",
              mouse_over = "Group_html",
              update_map_view = F,
              #info_window = "Group_html",
              close_info_window = T,
              #label = "Group",
              focus_layer = F,
              layer_id = "occ"
            )
            # add_polygons(occ_pol,
            #              #stroke_colour = "Occurrence_layer",
            #              stroke_weight = "2",
            #              stroke_opacity = "0",
            #              fill_colour = "Occurrence_layer",
            #              fill_opacity = "0.7",
            #              update_map_view = F, 
            #              legend = T, 
            #              pal = pal2,
            #              layer_id = "occ") 
          
        }
      }else{
        google_map_update("Gmap") %>% 
          #google_map(key = map_key) %>%
          clear_polygons( layer_id    = "pols") %>% 
          clear_markers( layer_id    = "occ")
      }#end else hide layer  
      
     
      #js$geocodeAddr()
    })
    
  
  #  output$country_selector <- renderUI({
      
     
  #    countries<- rv_list$countries
  #    flags <- rv_list$flags
  #    pickerInput(
  #      inputId = "country_picker",
  #      label = "Select one country:", 
  #      choices = countries,
  #      choicesOpt = list(content = lapply(seq_along(countries), 
   #                                        function(i) {HTML(paste(tags$img(src = flags[i],
  #                                                                          width = 20, 
   #                                                                        height = 15), countries[i]))})),
   #    options = list(
  #        `live-search` = TRUE),
    #    multiple = TRUE
        
     # )
      
    #})
     
    
   ### RESTORES SESSION 
    
     
    output$restoreSession <- renderMenu({
      msgs2 <- list(notificationItem(
        text = "Restore session",
        icon = icon("fas fa-undo-alt") 
      ) %>%
        tagAppendAttributes(., id = "restore"))
      
      dropdownMenu(type = "notifications", .list = msgs2, icon = icon("fas fa-user-cog") )
    })
   
    shinyjs::onclick("restore", expr = function(){
     
      output$modal1 <- renderUI({
        showModal(modalDialog(
          title = tags$strong("Restore Session"),
          tags$div(
            tags$div(style = "float:left;", tags$img(src = 'restore-icon.jpg', eigth = "45px", width = "45px")),
            tags$div(style = "margin-left: 50px;",tags$h5("
            Did your last Session closed unexpectedly or would you like to continue from you left? You can restore the variables and results from a previous session, or start a
            new session."))
          ),
          tags$hr(),
          fileInput("restorePath", "Select  Rsession.rds file:"),
          actionBttn(
            inputId = "accept_restore",
            label = "Restore",
            style = "jelly", 
            color = "primary"),
          easyClose = FALSE,
          footer = modalButton("Ok")
        ))
      })
    })
    
    
    
    observeEvent(input$accept_restore,{
     tryCatch({
       
       rsession_path <- input$restorePath$datapath
       
       if(nchar(rsession_path != 0)){
         
         rsd <- readRDS(rsession_path)
         print(rsd)
         #check if the rds file was generated by this app
         if(!is.null(rsd$valid_file)){
           
           points <- rsd$points_inf %>% 
             bind_rows %>% 
             filter(!(is.na(lat) & is.na(lng) & add == ""))
           
           places <- rsd$places_inf %>% 
             bind_rows() %>% 
             filter(!is.na(places))
           
           id_places <- places %>% 
             pull(id) %>% 
             unique()
           #content to restore here
           n <- nrow(points)
  
           counter$n <-  ifelse(n <= 3, 0, n-3)
           
           Sys.sleep(0.3)
           
           updatePickerInput(session, 
                             inputId = "country_picker", 
                             selected = rsd$crops_inf %>% pull(selected_country) %>% unique() )
           
           updateMultiInput(session,
             inputId =  "select_crops2",
             selected = rsd$crops_inf %>% pull(selected_crops),
             choices = NULL
           )
           
           Sys.sleep(0.5)
           
           updateMultiInput(session,
                            inputId = "select_groups",
                            selected = rsd$groups_in %>%  dplyr::pull(selected_groups),
                            choices = NULL)
          
          
           for(i in 1:n ){
             
             #input[[paste0("pick_plac", id)]]
             if(i %in% places$id){
               updatePickerInput(session, inputId = paste0("pick_plac", i), selected = places$places)
             }
             
             
             updateTextInput(session, inputId = paste0("Lat", i), value =  ifelse(is.na(points$lat[i]), "", as.character(points$lat[i])))
             updateTextInput(session, inputId = paste0("Lng", i), value =  ifelse(is.na(points$lng[i]), "", as.character(points$lng[i])) )
             updateTextInput(session, inputId = paste0("addres", i), value =  ifelse(is.na(points$add[i]), "", as.character(points$add[i])))
             
           }
           
           
         }else{
           safeError("Wrong .RDS file selected.")
         }
         
         
         print("Reactive values were uploaded.")
         
       }else{
         safeError("Not a valid file detected.")
       }
       
       
     },
              error = function(e){
                sendSweetAlert(
                  session = session,
                  title = "Error !!",
                  text = paste("Your Session could not be restored.  \n", e),
                  type = "error"
                )
              })
    })
    
    
}## END server
