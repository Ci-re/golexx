

render_maps <- function(dataframe, checks, trait, accessions, weather, switch){
  lev1 <- shapedata
  tmax_mean_nigeria_df <- tempdata
  prec_data <- precdata

  weather_rasterFile <- NULL
  colour <- c()
  name <- ""
  trait_sel <- ""
  traits <- toupper(trait)
  dataframe_values <- dataframe %>% filter(trait == traits) %>%
    janitor::clean_names() %>% attach_location_coordinates() %>%
    mutate(category = if_else(accession %in% checks, "checks","selection")) %>%
    filter(accession %in% accessions)
  dataframe_difference <- (dataframe) %>% filter(trait == traits) %>%
    janitor::clean_names() %>% calculate_env_checkmean(checks) %>% attach_location_coordinates() %>%
    mutate(category = if_else(accession %in% checks, "checks","selection")) %>%
    filter(accession %in% accessions)

  # print(dataframe_difference)

  lay <- NULL
  if(weather == "Temperature"){
    weather_rasterFile <- tmax_mean_nigeria_df
    colnames(weather_rasterFile) <- c("x", "y", "layer")
    colour <- scale_fill_distiller(palette = "Spectral")
    name <- "Temperature (mm)"
  } else if(weather == "Rainfall") {
    weather_rasterFile <- prec_data
    colnames(weather_rasterFile) <- c("x", "y", "layer")
    colour <- scale_fill_distiller(palette = "Spectral", trans = "reverse")
    name <- "Rainfall (mm)"
  } else {
    weather_rasterFile <- NULL
    lay <- NULL
    colour <- c()
    name <- ""
  }

  if(traits == "DYLD"){
    trait_sel <- "Dry yield"
  }else if(traits == "FYLD"){
    trait_sel <- "Fresh Yield"
  }else if(traits == "DM"){
    trait_sel <- "Dry matter"
  }else if(traits == "PLTHT"){
    trait_sel <- "Plant height"
  }else if(traits == "MCMDS"){
    trait_sel <- "Cassava mosaic"
  }else {
    trait_sel <- traits
  }

  tf <- ggplot() +
    geom_raster(data = weather_rasterFile, aes(x = x, y = y, fill = layer), interpolate = TRUE) +
    # scale_fill_gradientn(colours = terrain.colors(5)) +
    colour +
    # new_scale_fill()+
    # scale_fill_gradient2(low = "red", mid = "yellow", high = "blue") +
    # geom_sf(data = lev1, show.legend = TRUE) +
    geom_sf(data = lev1, colour = "white", size = .1, fill = NA) +
    geom_sf_text(data = lev1, aes(label = statename, alpha = 0.8))


  if(switch == FALSE){
    tf_val <- tf +
      # coord_sf() +
      geom_text(data = dataframe_values, aes(x = long, y = lat, label = location), nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
      geom_jitter(data = dataframe_values, mapping = aes(x = long, y = lat, color = values, size = values, shape = category,
                                                        text = paste0("<b> Trait: ",traits,"</b> \n",
                                                                      "<b> Accession: ", accession, "</b> \n",
                                                                      "<b>",trait_sel ,":",values,"</b>")), fill = "blue")+
      # scale_color_viridis_c() +
      scale_color_viridis(discrete = FALSE, option = "D")+
      # scale_color_manual(values = c("darkblue", "blue")) +
      # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
      # geom_sf_text(data = lev1, aes(label = statename)) +
      # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
      #                                       size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
      facet_wrap(~fct_inorder(accession), ncol = 2) +
      labs(fill = name, color = trait_sel)+
      theme_void() +
      theme(legend.position = "bottom", axis.line=element_blank(),legend.title = element_text(name),
            axis.text = element_blank(),
            plot.title = element_text(trait_sel),
            axis.ticks=element_blank(), title = element_text(""))
    # labs(title = "Historical Annual Mean Rainfall")+

  } else {
    tf_val <- tf +
      geom_text(data = dataframe_difference, aes(x = long, y = lat, label = location), nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
      geom_jitter(data = dataframe_difference, mapping = aes(x = long, y = lat, color = values, size = values > 0, shape = category,
                                                            text = paste0("<b> trait: ",traits,"</b> \n",
                                                                          "<b> accession: ", accession, "</b> \n",
                                                                          "<b>",trait_sel ,":",values,"</b>")))+
      # scale_color_viridis_c() +
      scale_color_viridis(discrete = FALSE, option = "D")+
      # scale_color_hue(l = 12, discrete = FALSE) +
      # scale_color_manual(values = c("darkblue", "blue")) +
      # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
      # geom_sf_text(data = lev1, aes(label = statename)) +
      # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
      #                                       size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
      facet_wrap(~fct_inorder(accession), ncol = 2) +
      labs(fill = name, color = trait_sel)+
      theme_void() +
      theme(legend.position = "bottom", axis.line=element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(trait_sel),
            axis.ticks=element_blank(), title = element_text(""))
  }
  return(tf_val)
}



render_leaflet_maps <- function(dataframe, checks, trait, accessions, weather, switch){
  Sys.setenv(R_CONFIG_ACTIVE = "production")
  lev1 <- shapedata
  traits <- toupper(trait)
  dataframe_values <- (dataframe) %>% filter(trait == traits)
  dataframe_values <- dataframe_values[ , colSums(is.na(dataframe_values)) < nrow(dataframe_values)]

  dataframe_values<- dataframe_values %>% janitor::clean_names() %>% attach_location_coordinates() %>%
    mutate(category = if_else(accession %in% checks, "checks","selection")) %>%
    filter(accession == accessions[length(accessions)])

  dataframe_difference <- (dataframe) %>% filter(trait == traits)
  dataframe_difference <- dataframe_difference[ , colSums(is.na(dataframe_difference)) < nrow(dataframe_difference)]
  dataframe_difference <- dataframe_difference %>% janitor::clean_names() %>% calculate_env_checkmean(checks) %>% attach_location_coordinates() %>%
    mutate(category = if_else(accession %in% checks, "checks","selection")) %>%
    filter(accession == accessions[length(accessions)])


  trait_sel <- ""

  dyld_icons <- iconList(
    good = makeIcon(iconUrl = "www/good_yield.png", iconWidth = 100, iconHeight = 100),
    bad = makeIcon(iconUrl = "www/poor_yield.png", iconWidth = 100, iconHeight = 100)
  )
  fyld_icons <- iconList(
    good = makeIcon(iconUrl = "www/good_yield.png", iconWidth = 100, iconHeight = 100),
    bad = makeIcon(iconUrl = "www/poor_yield.png", iconWidth = 100, iconHeight = 100)
  )

  dm_icons <- iconList(
    good = makeIcon(iconUrl = "www/good_yield.png", iconWidth = 60, iconHeight = 60),
    bad = makeIcon(iconUrl = "www/poor_yield.png", iconWidth = 60, iconHeight = 60)
  )
  mcmds_icons <- iconList(
    bad = makeIcon(iconUrl = "www/good_leaf.png", iconWidth = 100, iconHeight = 100),
    good = makeIcon(iconUrl = "www/bad_leaf.png", iconWidth = 100, iconHeight = 100)
  )
  pltht_icons <- iconList(
    good = makeIcon(iconUrl = "www/tall_plant.png", iconWidth = 60, iconHeight = 100),
    bad = makeIcon(iconUrl = "www/short_plant.png", iconWidth = 50, iconHeight = 70)
  )
  def_icons <- iconList(
    good = makeIcon(iconUrl = "www/tall_plant.png", iconWidth = 60, iconHeight = 100),
    bad = makeIcon(iconUrl = "www/short_plant.png", iconWidth = 50, iconHeight = 70)
  )

  if(traits == "DYLD"){
    trait_sel <- "Dry yield"
    def_icons <- dyld_icons
  }else if(traits == "FYLD"){
    trait_sel <- "Fresh Yield"
    def_icons <- fyld_icons
  }else if(traits == "DM"){
    trait_sel <- "Dry matter"
    def_icons <- dm_icons
  }else if(traits == "PLTHT"){
    trait_sel <- "Plant height"
    def_icons <- pltht_icons
  }else if(traits == "MCMDS"){
    trait_sel <- "Cassava mosaic"
    def_icons <- mcmds_icons
  }else {
    trait_sel <- traits
  }

  if(switch == FALSE){
    leaf_X <- lev1 %>% leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topography") %>%
      # addProviderTiles(providers$OpenWeatherMap.Rain, group = "Rain") %>%
      # addProviderTiles(providers$CartoDB.VoyagerLabelsUnder, group = "Voyager Label") %>%
      addLayersControl(baseGroups = c("Toner List", "Topography")) %>%
      addPolygons(color = "#cecece", weight = .8, smoothFactor = 0.5,
                  fillOpacity = .1,
                  layerId = ~statename,
                  highlightOptions = highlightOptions(color = "red", weight = 1, bringToFront = TRUE)) %>%
      # addMarkers(label = ayt20_sindex$accession_name,
      #            clusterOptions = markerClusterOptions(),
      #            popup = ifelse(ayt20_sindex$accession_name == "IITA-TMS-IBA000070",
      #                           "IITA-TMS-IBA000070 is the one", # Value if True
      #                           "Not the one")) %>%
      addMarkers(data = dataframe_values, lng = ~long, lat = ~lat,
                 icon = ~if_else(values > mean(combined), def_icons["good"],def_icons["bad"]),
                 label = ~location,
                 group = "location",
                 popup = ~paste("<table>
                                  <tr>
                                    <th> Variable </th>
                                    <th> Value </th>
                                  </tr>
                                  <tr>
                                    <td> Accession </td>
                                    <td>",accession ,"</td>
                                  </tr>
                                  <tr>
                                    <td>",trait_sel , "</td>
                                    <td>",values ,"</td>
                                  </tr>
                                  <tr>
                                    <td> Location </td>
                                    <td>",location ,"</td>
                                  </tr>
                                  <tr>
                                    <td> Category </td>
                                    <td>",category ,"</td>
                                  </tr>
                                </table>")) %>%
      addResetMapButton() %>%
      addOpenweatherTiles(layers = weather, apikey = get_golem_config("OPENWEATHERMAP")) %>%
      addSearchFeatures(targetGroups = "location",
                        options = searchFeaturesOptions(
                          openPopup = TRUE, zoom = 12,
                          firstTipSubmit = TRUE, autoCollapse = TRUE, hideMarkerOnCollapse = TRUE)) %>%
      # addControl(html = "<h1> This is a control </h1>", position = "bottomright") %>%
      setView(lng = 9.0820, lat = 8.6753, zoom = 6)
    # addMiniMap(
    #   toggleDisplay = TRUE,
    #   tiles = providers$OpenWeatherMap.Rain
    # )

  } else {

    leaf_X <- lev1 %>% leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topography") %>%
      addLayersControl(baseGroups = c("Toner Lite", "Topography")) %>%
      # addPolygons(color = "#cecece", weight = .8, smoothFactor = 0.5,
      #             fillOpacity = .1,
      #             layerId = ~statename,
      #             highlightOptions = highlightOptions(color = "red", weight = 1, bringToFront = TRUE)) %>%
      # addMarkers(label = ayt20_sindex$accession_name,
      #            clusterOptions = markerClusterOptions(),
      #            popup = ifelse(ayt20_sindex$accession_name == "IITA-TMS-IBA000070",
      #                           "IITA-TMS-IBA000070 is the one", # Value if True
      #                           "Not the one")) %>%
      addMarkers(data = dataframe_difference, lng = ~long, lat = ~lat,
                 icon = ~if_else(values > 0,def_icons["good"],def_icons["bad"]),
                 label = ~location,
                 # radius = ~ifelse(values > 0, 15, 7),
                 # color = ~if_else(values > 0,"green","red"),
                 # fill = ~if_else(values > 0,"green","red"),
                 # stroke = FALSE
                 popup = ~paste("<table style = ","border=5",">
                                            <tr>
                                              <th> Variable </th>
                                              <th> Value </th>
                                            </tr>
                                            <tr>
                                              <td> Accession </td>
                                              <td>",accession,"</td>
                                            </tr>
                                            <tr>
                                              <td> Summary </td>
                                              <td>",if_else(values >= 0,
                                                            paste(round(values, 2),"% greater check-mean"),
                                                            paste0(round(values, 2),"% lesser than check mean")) ,
                                              "</td>
                                            </tr>
                                            <tr>
                                              <td> Location </td>
                                              <td>",location ,"</td>
                                            </tr>
                                            <tr>
                                              <td> Category </td>
                                              <td>",category ,"</td>
                                            </tr>
                                          </table>")) %>%
      setView(lng = 9.0820, lat = 8.6753, zoom = 6) %>%
      addOpenweatherTiles(layers = weather, apikey = get_golem_config("OPENWEATHERMAP")) %>%
      addLayersControl(
        # overlayGroups = c("Rain","Temperature"),
        options = layersControlOptions(collapsed = FALSE))
    # addSidebar(id = "sidebar", options = list(position = "left", fit = TRUE)) %>%
    # addMiniMap(
    #   toggleDisplay = TRUE,
    #   tiles = providers$Stamen.TonerLite
    # )
  }
  return(leaf_X)
}



