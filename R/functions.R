get_all_endangerment_scores <- function(lang_ids, glotto_main, EGIDS, LEI_data, UNESCO_data) {
  
  full_dat <- lang_ids %>%
    dplyr::left_join(glotto_main %>%
                dplyr::select(LANG_ISO,
                              iso639_3_macro,
                              glottocode,
                              Family_name_glottolog,
                              Population_numeric_ethnologue,
                              endangerment_status_glottolog,
                              Language_status_ethnologue),
                by = "LANG_ISO") %>%
    dplyr::mutate(iso_macro = paste0(LANG_ISO, "-", toupper(iso639_3_macro))) %>%
    dplyr::left_join(EGIDS %>%
                dplyr::select(LANG_ISO, EGIDS, LMP_POP1)) %>%
    dplyr::distinct() %>%
    left_join(LEI_data %>%
                dplyr::select(LANG_ISO, LEI)) %>%
    dplyr::distinct() %>%
    left_join(UNESCO_data %>%
                dplyr::select(LANG_ISO, UNESCO = `Degree of endangerment`)) %>%
    dplyr::distinct() %>%
    dplyr::rename(AES = endangerment_status_glottolog)
  
  full_dat
  
}

make_endangerment_fluctuation_plots <- function(lang_endangerment_scores) {
  just_scores <- lang_endangerment_scores %>%
    dplyr::select(LANG_ISO, EGIDS, LEI, UNESCO, AES) %>%
    dplyr::mutate(EGIDS = factor(EGIDS, levels = c("1",
                                                   "2",
                                                   "3",
                                                   "4",
                                                   "5",
                                                   "6a",
                                                   "6b",
                                                   "7",
                                                   "8a",
                                                   "8b",
                                                   "9")),
                  LEI = factor(LEI, levels = c("At risk",
                                               "Vulnerable",
                                               "Threatened",
                                               "Endangered",
                                               "Severely Endangered",
                                               "Critically Endangered",
                                               "Awakening",
                                               "Dormant")),
                  UNESCO = factor(UNESCO, levels = c("Vulnerable",
                                                     "Definitely endangered",
                                                     "Severely endangered",
                                                     "Critically endangered",
                                                     "Extinct")),
                  AES = factor(AES, levels = c("not endangered",
                                               "threatened",
                                               "shifting",
                                               "moribund",
                                               "nearly extinct",
                                               "extinct")))
  
  xCol <- "EGIDS"
  yCol <- "AES"
  data <- just_scores
  my_lower <- function(data, mapping) {
    xCol <- as.character(mapping[[1]][[2]])
    yCol <- as.character(mapping[[2]][[2]])
    xLevels <- levels(data[ , xCol])
    yLevels <- levels(data[ , yCol])
    to_plot <- na.omit(data[ , c(xCol, yCol)]) %>%
      table(useNA = "always") %>%
      as_tibble()
    colnames(to_plot) <- c(xCol, yCol, "n")
    to_plot[ , xCol] <- factor(unlist(to_plot[ , xCol]), xLevels)
    to_plot[ , yCol] <- factor(unlist(to_plot[ , yCol]), yLevels)
    to_plot[ , "n"] <- to_plot[ , "n"] / sum(to_plot[ , "n"])# / (1 / nrow(to_plot))
    p <- ggplot(to_plot, aes_string(xCol, yCol)) +
      geom_point(aes(size = n, alpha = n)) + 
      scale_size_area(max_size = 5, name = "Proportion\nof Scores") +
      scale_alpha(name = "Proportion\nof Scores") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 90))
    p
  }
  
  xCol <- "EGIDS"
  yCol <- "EGIDS"
  my_diag <- function(data, mapping) {
    # xCol <- as.character(mapping[[1]][[2]])
    # yCol <- as.character(mapping[[2]][[2]])
    # xLevels <- levels(data[ , xCol])
    # yLevels <- levels(data[ , yCol])
    
    p <- ggplot(data = data, mapping = mapping) + 
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 90)) 
    p
    
    # to_plot <- na.omit(data[ , c(xCol, yCol)]) %>%
    #   table(useNA = "always") %>%
    #   as_tibble()
    # colnames(to_plot) <- c(xCol, yCol, "n")
    # to_plot[ , xCol] <- factor(unlist(to_plot[ , xCol]), xLevels)
    # to_plot[ , yCol] <- factor(unlist(to_plot[ , yCol]), yLevels)
    # p <- ggplot(to_plot, aes_string(xCol, yCol)) +
    #   geom_point(aes(size = n, alpha = n)) + 
    #   scale_size_area(max_size = 10) +
    #   theme_minimal() +
    #   theme(panel.grid = element_blank())
    # p
  }
  
  xCol <- "EGIDS"
  yCol <- "AES"
  my_cor <- function(data, mapping, sgnf=3, size = 8, ...) {
    
    xCol <- as.character(mapping[[1]][[2]])
    yCol <- as.character(mapping[[2]][[2]])
    
    xVal <- data[[xCol]] %>% as.numeric()
    yVal <- data[[yCol]] %>% as.numeric()
    
    rho <- Hmisc::rcorr(xVal, yVal, type = "spearman")$r[2,1]
    loc <- tibble(x=.5, y=.5)
    
    p <-  ggplot(data = loc, aes(x = x, y = y)) + 
      xlim(0:1) + 
      ylim(0:1) + 
      theme(panel.background = element_rect(fill = "grey95"),  
            panel.grid = element_blank()) + 
      labs(x = NULL, y = NULL) +
      geom_text(size = 8,
                label = 
                  paste("rank corr:\n", round(rho, 2), sep = "", collapse = "")) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank())
    p
  }
  
  ## 10 X 7 PDF
  ggpairs(just_scores %>% dplyr::select(-LANG_ISO), lower = list(discrete = my_lower),
          diag = list(discrete = my_diag),
          upper = list(discrete = my_cor),
          legend = c(4, 1))
}

make_language_dup_plots <- function(multilanguage_summary, lang_polys, base_map, save_pdf_file) {
  
  area_thresh <- units::set_units(1000, km^2)
  
  base_map_pretty <- base_map %>%
    filter(geounit != "Antarctica") %>%
    st_make_valid %>%
    st_union() %>%
    fill_holes(area_thresh)
  
  for(i in 1:nrow(multilanguage_summary)) {
    lang_polys_dup <- lang_polys[lang_polys$LANG_ISO %in% multilanguage_summary$LANG_ISO[[i]], ]
    ggplot(base_map) +
      geom_sf() +
      geom_sf(data = lang_polys_dup, fill = "red", colour = "red") +
      theme_minimal()
    
    ggplot(lang_polys_dup) + geom_sf()
  }
  
}

make_polygon_area_plots <- function(language_areas, hua_data_2000_language_diversity) {
  areas_km <- tibble(areas = as.vector(language_areas) / (1000 * 1000))
  p1 <- ggplot(areas_km, aes(areas)) +
    geom_histogram(bins = 100, fill = "grey60") +
    geom_vline(xintercept = 10000, linetype = 2, size = 0.8) +
    annotate("segment", x = 100000, xend = 15000, y = 250, yend = 200,
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
    annotate("text", x = 100000, y = 265, label = bquote('10,000'~km^2)) +
    ylab("Number of Languages") +
    xlab(bquote("Range Area"~(km^2))) +
    scale_x_log10(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = -45))
  
  p1
  
  language_div <- tibble(lang_num = values(hua_data_2000_language_diversity))
  num_one <- sum(language_div$lang_num == 1, na.rm = TRUE)
  num_greater_one <- sum(language_div$lang_num > 1, na.rm = TRUE)
  p2 <- ggplot(language_div, aes(lang_num)) +
    geom_histogram(bins = 30, fill = "grey60") +
    geom_vline(xintercept = 1.35, linetype = 2, size = 0.8) +
    annotate("segment", x = 5, xend = 1.05, y = 175, yend = 160,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
    annotate("text", x = 7, y = 180, label = paste0("Grid Cells with 1 Language: ", num_one)) +
    annotate("segment", x = 3, xend = 1.4, y = 125, yend = 125,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
    annotate("segment", x = 180, xend = 400, y = 125, yend = 125,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
    annotate("text", x = 22, y = 125, label = 
               paste0("Grid Cells with Greater than 1 Language: ", num_greater_one)) +
    scale_x_log10(labels = scales::comma, breaks = c(1, 10, 100, 400)) +
    xlab("Number of Languages in Cell") +
    ylab("Number of Grid Cells") +
    theme_minimal()
  
  p <- p1 + p2 + plot_layout(nrow = 2)
  p
}

make_hua_raster_high_res <- function(hua_data_12600) {
  variables <- c("altitude",
                 "altitude_range",
                 "mean_annual_temperature",
                 "annual_precipitation",
                 "temperature_seasonality",
                 "precipitation_seasonality",
                 "roughness",
                 "popden",
                 "river", 
                 "mgs",
                 "new.mgs")
  
  make_raster <- function(vari) {
    dat <- hua_data_12600
    #dat[ , vari] = dat$land_coverage * dat[ , vari]
    #dat[ , vari] = ifelse(dat$land_coverage == 0, NA, dat[ , vari])
    raster::rasterFromXYZ(dat[ , c("longitudes", 
                                              "latitudes", 
                                              vari)],
                          crs = CRS("+proj=cea +datum=WGS84 +units=m"))
  }
  
  rast_list <- lapply(variables, make_raster)
  rast_stack <- raster::stack(rast_list)
  names(rast_stack) <- variables
  
  raster::writeRaster(rast_stack, "data/cea_rasters/Hua_et_al_data_rasters_200_KM2.grd",
                      overwrite = TRUE)
  
}

get_hmr_raster_values <- function(human_footprint_raster, scaling, hmr_reprojected_lang_polys, lang_polys) {
  
  vel <- velox::velox(human_footprint_raster)
  vel$aggregate(5, "mean")
  if(!grepl("Popdensity|tempseasonality", human_footprint_raster)) {
    data_list <- vel$extract(hmr_reprojected_lang_polys, small = TRUE)
  } else {
    data_list <- vel$extract(lang_polys, small = TRUE)
  }
  data_list <- lapply(data_list, function(x) x / scaling)
  data_list
  
}

get_hua_raster_values <- function(hua_raster_high_res, hua_reprojected_lang_polys) {
  
  layer_names <- names(hua_raster_high_res)
  vel <- velox::velox(hua_raster_high_res)
  
  data_list <- vel$extract(hua_reprojected_lang_polys, small = TRUE)
  
  data_list <- lapply(data_list, function(x) {colnames(x) <- layer_names; x})
  
  data_list
  
}

clean_glottolog_tree <- function(newick_tree) {
  tree_txt <- readLines(newick_tree)
  tree_txt <- tree_txt[-1:-9]
  stray_quotes <- stringr::str_count(tree_txt, "\\'")
  stray_quote_trees <- which(stray_quotes %% 2 != 0)
  
  ## looks like I can just replace all instances of \\' with something, I'm thinking //`
  new_tree_txt <- gsub("\\\\\\'", "\\\\\\`", tree_txt)
  num_trees <- length(new_tree_txt)
  num_langs <- stringr::str_count(new_tree_txt, "\\,")
  
  good_trees <- new_tree_txt[num_langs > 0] 
  bad_trees <- new_tree_txt[num_langs == 0]
  
  bad_tree_tips <- stringr::str_match(bad_trees, "\\(\\'(.*?)\\'\\:")[, 2]
  
  writeLines(good_trees, "data/trees/glottolog_good_trees.nw")
  good_trees_phy <- ape::read.tree(file_in("data/trees/glottolog_good_trees.nw"))
  
  good_trees_bind <- purrr::reduce(good_trees_phy, `+`)
  
  bad_tree <- phytools::starTree(unlist(bad_tree_tips), branch.lengths = rep(1, length(bad_tree_tips)))
  
  all_tree_bind <- good_trees_bind + bad_tree
  
  write.tree(all_tree_bind, "data/trees/glottolog_full_tree.nw")
  
  all_tree_bind
  
}

reduce_and_cleanup_glottolog_tree <- function(glottolog_trees, ISO_to_glottocode) {
  
  tip_glottocode <- stringr::str_match(glottolog_trees$tip.label, "\\[(.*?)\\]")
  
  code_df <- dplyr::tibble(tip_glottocode = tip_glottocode[ , 2]) %>%
    dplyr::left_join(ISO_to_glottocode, by = c("tip_glottocode" = "glottocode"))
  
  tips_with_iso <- stringr::str_count(glottolog_trees$tip.label, "\\[(.*?)\\]")
  no_iso <- which(tips_with_iso == 1)
  new_tree <- ape::drop.tip(glottolog_trees, no_iso)
}

get_forest_change_values <- function(forest_gain_files) {
  gain_rasters <- lapply(forest_gain_files, raster)
  forest_gain <- do.call(merge, gain_rasters)
}

##' Make Language Diversity Grid Data
##'
##' .. content for \details{} ..
##'
##' @title Language diversity grid data function
##' @param languages_by_grid
##' @param hua_data_480_language_diversity
##' @param lang_polys_merged_cea
make_diversity_raster_480 <- function(languages_by_grid,
                                      hua_data_480_language_diversity,
                                      lang_polys_merged_cea) {
  
  language_counts <- purrr::map_int(languages_by_grid, length)
  
  # IUCN_counts <- purrr::map_int(IUCN_by_grid, length)
  # IUCN_all_counts <- purrr::map_int(IUCN_all_by_grid, length)
  # IUCN_prop <- IUCN_counts / IUCN_all_counts
  
  pops <- purrr::map(languages_by_grid, ~ lang_polys_merged_cea$LMP_POP1[.])
  total_pops <- purrr::map_dbl(pops, ~ sum(., na.rm = TRUE))
  language_evenness <- purrr::map_dbl(pops, asbio::SW.index) / log(language_counts)
  language_evenness[is.nan(language_evenness)] <- 0
  
  endangered <- purrr::map(languages_by_grid, ~ lang_polys_merged_cea$EGIDS[.])
  endangered_counts <- purrr::map_dbl(endangered, ~ sum(as.numeric(.) > 6, na.rm = TRUE))
  
  endangered_prop <- endangered_counts / language_counts
  endangered_prop[is.nan(endangered_prop)] <- 0
  
  AES_counts <- purrr::map_int(languages_by_grid, ~ sum(as.numeric(lang_polys_merged_cea$AES[.]) > 1, 
                                                        na.rm = TRUE))
  
  AES_prop <- AES_counts / language_counts
  
  language_richness_raster <- hua_data_480_language_diversity
  raster::values(language_richness_raster)[!is.na(raster::values(language_richness_raster))] <-
    language_counts[!is.na(raster::values(language_richness_raster))]
  
  language_evenness_raster <- hua_data_480_language_diversity
  raster::values(language_evenness_raster)[!is.na(raster::values(language_evenness_raster))] <-
    language_evenness[!is.na(raster::values(language_evenness_raster))]
  
  AES_raster <- hua_data_480_language_diversity
  raster::values(AES_raster)[!is.na(raster::values(AES_raster))] <-
    AES_counts[!is.na(raster::values(AES_raster))]
  
  AES_prop_raster <- hua_data_480_language_diversity
  raster::values(AES_prop_raster)[!is.na(raster::values(AES_prop_raster))] <-
    AES_prop[!is.na(raster::values(AES_prop_raster))]
  
  endangered_raster <- hua_data_480_language_diversity
  raster::values(endangered_raster)[!is.na(raster::values(endangered_raster))] <-
    endangered_counts[!is.na(raster::values(endangered_raster))]
  
  endangered_prop_raster <- hua_data_480_language_diversity
  raster::values(endangered_prop_raster)[!is.na(raster::values(endangered_prop_raster))] <-
    endangered_prop[!is.na(raster::values(endangered_prop_raster))]
  
  total_L1_POP_raster <- hua_data_480_language_diversity
  raster::values(total_L1_POP_raster)[!is.na(raster::values(total_L1_POP_raster))] <-
    total_pops[!is.na(raster::values(total_L1_POP_raster))]
  
  # IUCN_raster <- hua_data_480_language_diversity
  # raster::values(IUCN_raster)[!is.na(raster::values(IUCN_raster))] <-
  #   IUCN_counts[!is.na(raster::values(IUCN_raster))]
  
  # IUCN_prop_raster <- hua_data_480_language_diversity
  # raster::values(IUCN_prop_raster)[!is.na(raster::values(IUCN_prop_raster))] <-
  #   IUCN_prop[!is.na(raster::values(IUCN_prop_raster))]
  
  diversity_raster <- stack(list(language_richness = language_richness_raster, 
                                 language_evenness = language_evenness_raster,
                                 languages_threatened = endangered_raster,
                                 languages_prop_threatened = endangered_prop_raster,
                                 total_L1_POP = total_L1_POP_raster)#,
                                 #species_endangered = IUCN_raster, 
                                 #species_prop_endangered = IUCN_prop_raster)
                                 )
  
  diversity_raster
  
}

make_IUCN_data <- function(IUCN_by_grid, IUCN_all_by_grid, IUCN_grid_raster,
                                      lang_polys_merged_cea) {
  
  IUCN_counts <- purrr::map_int(IUCN_by_grid, length)
  IUCN_all_counts <- purrr::map_int(IUCN_all_by_grid, length)
  IUCN_prop <- IUCN_counts / IUCN_all_counts
  
  IUCN_raster <- IUCN_grid_raster
  raster::values(IUCN_raster)[!is.na(raster::values(IUCN_raster))] <-
     IUCN_counts[!is.na(raster::values(IUCN_raster))]
  
  IUCN_prop_raster <- IUCN_grid_raster
   raster::values(IUCN_prop_raster)[!is.na(raster::values(IUCN_prop_raster))] <-
     IUCN_prop[!is.na(raster::values(IUCN_prop_raster))]
  
  endangered_raster <- stack(list(species_endangered = IUCN_raster, 
                            species_prop_endangered = IUCN_prop_raster)
  )
  
  vel <- velox::velox(IUCN_raster)
  IUCN_list <- vel$extract(lang_polys_merged_cea, small = TRUE)
  species_endangered <- purrr::map_dbl(IUCN_list, ~mean(., na.rm = TRUE))
  
  vel <- velox::velox(IUCN_prop_raster)
  IUCN_prop_list <- vel$extract(lang_polys_merged_cea, small = TRUE)
  species_prop_endangered <- purrr::map_dbl(IUCN_prop_list, ~mean(., na.rm = TRUE))
  
  tibble(LANG_ISO = lang_polys_merged_cea$LANG_ISO, species_endangered = species_endangered,
         species_prop_endangered = species_prop_endangered)
  
}



##' Function to get grid-based data
##'
##' .. content for \details{} ..
##'
##' @title Grid-based data
##' @param lang_polys_cea
##' @param language_diversity_raster_480
##' @param IUCN_amphibians_raster_low_res
##' @param IUCN_mammals_raster_low_res
get_grid_based_data <- function(lang_polys_cea, diversity_raster_480) {
  
  
  diversity_grid <- sf::st_as_sf(raster::rasterToPolygons(diversity_raster_480))
  
  diversity_by_lang <- sf::st_join(lang_polys_cea %>%
                                     sf::st_make_valid(), 
                                   diversity_grid,
                                   largest = TRUE)
  
  diversity_by_lang
  
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

bring_it_all_in <- function(lang_endangerment_by_ISO_AES_EGIDS, soceco_by_lang, language_types,
                            grid_poly_data, new_language_diversity,
                            language_bordering_diversity,
                            connectivity_nb_data,
                            IUCN_data,
                            climate_by_lang,
                            used_in_ed,
                            glottolog_all,
                            language_areas_merged) {
  
  test_if_multiples <- function(dat) {
    dat %>% 
      dplyr::group_by(LANG_ISO) %>% 
      dplyr::summarise(count = n()) %>%
      dplyr::pull(count) %>%
      `>`(1) %>%
      any()
  }
  
  full_data <- lang_endangerment_by_ISO_AES_EGIDS %>%
    dplyr::left_join(soceco_by_lang) %>%
    dplyr::left_join(used_in_ed %>%
                       dplyr::group_by(LANG_ISO) %>%
                       dplyr::summarise(used_in_education = max(used_in_education, na.rm = TRUE))) %>%
    dplyr::left_join(language_types %>%
                       dplyr::select(LANG_ISO,
                                     type,
                                     official_in_any_country,
                                     IM)) %>%
    #dplyr::filter(IM == "N") %>%
    dplyr::select(-IM) %>%
    dplyr::left_join(grid_poly_data %>%
                       dplyr::select(-altitude,
                                     -mean_annual_temperature,
                                     -annual_precipitation,
                                     -temperature_seasonality,
                                     -precipitation_seasonality,
                                     -roughness,
                                     -river,
                                     -altitude,
                                     -altitude_range,
                                     -used_nb_for_landuse) %>%
                       dplyr::rename(mgs_new = new.mgs)) %>%
    dplyr::left_join(climate_by_lang, by = "LANG_ISO") %>%
    dplyr::left_join(new_language_diversity %>%
                       dplyr::select(-language_sw)) %>%
    dplyr::mutate_at(dplyr::vars(language_richness,
                                 threatened_languages,
                                 prop_threatened_languages,
                                 language_evenness),
                     ~ifelse(is.na(.), 0, .)) %>%
    dplyr::left_join(connectivity_nb_data %>%
                       dplyr::select(-altitude,
                                     -mean_annual_temperature,
                                     -annual_precipitation,
                                     -temperature_seasonality,
                                     -precipitation_seasonality,
                                     roughness_nb = roughness,
                                     -popden,
                                     river_nb = river,
                                     -mgs,
                                     -new.mgs)) %>%
    dplyr::left_join(language_bordering_diversity %>%
                       dplyr::select(LANG_ISO,
                                     bordering_language_richness,
                                     bordering_language_richness_per_km_perim,
                                     bordering_language_evenness)) %>%
    dplyr::left_join(IUCN_data) %>%
    dplyr::mutate(mean_annual_temperature_kelvin = mean_annual_temperature + 273.15,
                  mean_annual_temperature = NULL,
                  altitude = ifelse(altitude <= 0, 1, altitude),
                  used_in_education = ifelse(is.na(used_in_education), 0, 1),
                  L1pop_over_nb_popdens = L1_POP / pop_density_nb) %>%
    dplyr::left_join(language_areas_merged) %>%
    dplyr::filter(LANG_ISO != "chu") %>% ## remove weird long language polygon (Church Slavic)
    dplyr::left_join(glottolog_all %>%
                       dplyr::select(LANG_ISO,
                                     LANG_ISO_micro,
                                     category) %>%
                       tidyr::drop_na(LANG_ISO_micro) %>%
                       dplyr::group_by(LANG_ISO) %>%
                       dplyr::add_count(category, sort = TRUE) %>%
                       dplyr::summarise(category = category[1])) %>%
    dplyr::left_join(glottolog_all %>%
                       dplyr::select(LANG_ISO,
                                     LANG_ISO_micro,
                                     documentation = med) %>%
                       tidyr::drop_na(LANG_ISO_micro) %>%
                       dplyr::mutate(documentation = ifelse(is.na(documentation), "none known", documentation)#,
                                     #L1_POP_prop = L1_POP / total_L1_POP
                       ) %>%
                       dplyr::mutate(documentation = factor(documentation,
                                                            levels = c("none_known",
                                                                       "Wordlist or less",
                                                                       "phonology/text",
                                                                       "grammar sketch",
                                                                       "grammar",
                                                                       "long grammar"),
                                                            ordered = TRUE)) %>%
                       dplyr::group_by(LANG_ISO) %>%
                       dplyr::summarise(documentation = max(documentation, na.rm = TRUE))) %>%
    dplyr::mutate(documentation = dplyr::case_when(
      (documentation == "none known" | documentation == "Wordlist or less") ~ "little or none",
      (documentation == "phonology/text" | documentation == "grammar sketch") ~ "basic",
      (documentation == "grammar" | documentation == "long grammar") ~ "detailed",
      TRUE ~ "NA"
    )) %>%
    dplyr::mutate(documentation = factor(documentation, levels = c("little or none",
                                                                   "basic",
                                                                   "detailed"),
                                         ordered = TRUE)) %>%
    tidyr::unnest_wider(world_language, names_sep = "_")
  
  full_data
  
}

get_climate_data <- function(climate_rast, alt_rast, lang_polys_merged_cea) {
  clim <- velox::velox(climate_rast)
  clim_list <- clim$extract(lang_polys_merged_cea, small = TRUE)
  
  clim_df <- purrr::map_dfr(clim_list, ~apply(., 2, mean, na.rm = TRUE) )%>%
                              t() %>%
    magrittr::set_colnames(c("mean_annual_temperature", "annual_precipitation",
                             "temperature_seasonality", "precipitation_seasonality")) %>%
    dplyr::as_tibble()
  
  #alt_range_rast <- raster::calc(alt_rast, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  alt <- velox::velox(alt_rast)
  alt_list <- alt$extract(lang_polys_merged_cea, small = TRUE)
  
  clim_df <- clim_df %>%
    dplyr::mutate(altitude = purrr::map_dbl(alt_list, ~mean(., na.rm = TRUE)))
  
  clim_df <- clim_df %>%
    dplyr::mutate_all(~ifelse(is.finite(.), ., NA))
  
  dplyr::tibble(LANG_ISO = lang_polys_merged_cea$LANG_ISO) %>%
    dplyr::bind_cols(clim_df)
}

get_projections_data <- function(climate_rast, lang_polys_merged_cea) {
  clim <- velox::velox(climate_rast)
  clim_list <- clim$extract(lang_polys_merged_cea, small = TRUE)
  
  clim_df <- purrr::map_dfr(clim_list, ~apply(., 2, mean, na.rm = TRUE) )%>%
    t() %>%
    magrittr::set_colnames(c("ts_2060", "ts_2100")) %>%
    dplyr::as_tibble()
  
  clim_df <- clim_df %>%
    dplyr::mutate_all(~ifelse(is.finite(.), ., NA))
  
  dplyr::tibble(LANG_ISO = lang_polys_merged_cea$LANG_ISO) %>%
    dplyr::bind_cols(clim_df)
}

make_transformation_list <- function(transformation_table) {
  neg_recip <- scales::trans_new("neg_recip",
                                 function(x) - 1 / x,
                                 function(x) - 1 / x,
                                 breaks = reciprocal_trans()$breaks,
                                 minor_breaks = reciprocal_trans()$minor_breaks)
  
  s_sqrt <- scales::trans_new("s_sqrt",
                              function(x){sign(x)*sqrt(abs(x))},
                              function(x){x^2*sign(x)},
                              breaks = sqrt_trans()$breaks,
                              minor_breaks = sqrt_trans()$minor_breaks)
  
  log_w_zero <- function(small_add) {
    scales::trans_new("log_w_zero",
                      function(x) log(x + small_add),
                      function(x) exp(x) - small_add,
                      breaks = function(x) log10_trans()$breaks(x + small_add),
                      minor_breaks = function(x) log10_trans()$minor_breaks(x + small_add))
  }
  
  transformation_list <- mapply(function(x, y) {if(x == "log") {return(scales::log_trans())}
    else {return(ggforce::power_trans(y))}},
    transformation_table$transformation_name, 
    transformation_table$power_transform,
    SIMPLIFY = FALSE)
  
  transformation_list[transformation_table$transformation_name == "negative reciprocal"] <-
    list(neg_recip)
  
  transformation_list[transformation_table$transformation_name == "signed square root"] <-
    list(s_sqrt)
  
  small_add_indexes <- which(transformation_table$transformation_name == "log" & 
                               transformation_table$small_add > 0)
  
  transformation_list[small_add_indexes] <- purrr::map(transformation_table$small_add[small_add_indexes], 
             ~log_w_zero(.))
  
  names(transformation_list) <- transformation_table$column_name
  
  transformation_list
}

make_variable_maps <- function(lang_polys_merged_cea, bring_it_in, base_map_unioned, 
                               transformation_list, transformation_table,
                               mapping_sf) {
  maps_data <- mapping_sf %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::filter(LANG_ISO != "chu") %>%
    dplyr::left_join(bring_it_in)
  
  base_map_robin <- base_map_unioned %>%
    sf::st_transform('ESRI:54030')
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  var_names <- colnames(maps_data)[-1:-2]
  var_name <- var_names[1]
  make_a_map <- function(var_name) {
    map_df <- maps_data_robin %>%
      dplyr::select(LANG_ISO, !!var_name)
    p <- ggplot(base_map_robin) +
      geom_sf(fill = NA, size = 0.2) +
      geom_sf(data = map_df,
              aes_string(fill = var_name), 
              colour = NA,
              alpha = 0.8) +
      theme_minimal()
    
    if(transformation_table$continuous_numeric[transformation_table$column_name == var_name] == 1) {
      p <- p +
        scale_fill_viridis(trans = transformation_list[[var_name]])
    } 
    
    p
  }
  
  all_maps <- purrr::map(var_names, ~ make_a_map(.))
  names(all_maps) <- var_names
  all_maps
}


make_variable_maps2 <- function(lang_polys_merged, final_data_w_info, base_map, use_3d = FALSE) {
  maps_data <- lang_polys_merged %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::filter(LANG_ISO != "chu") %>%
    dplyr::inner_join(final_data_w_info,
                      by = c("LANG_ISO" = "id_ISO_lang")) %>%
    dplyr::select(-dplyr::ends_with("_tr")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("response_")),
                     ~factor(., ordered = FALSE)) %>%
    dplyr::arrange(dplyr::desc(lang_polygon_area_lang)) %>%
    dplyr::rename(id_ISO_lang = LANG_ISO)
  
  var_name <- "soceco_Life_exp_birth_10yr_median_country"
  
  make_a_map <- function(var_name, use_3d = FALSE) {
    tooltip <- purrr::pmap_chr(maps_data %>%
                                 dplyr::select(id_ISO_lang,
                                               info_name,
                                               !!var_name),
                               ~glue::glue("ISO: {..1}<br>
                                           Language name: {..2}<br>
                                           {var_name}: {..3}"))
    
    dat <- as.data.frame(maps_data[ , var_name])[ , var_name]
    
    if(is.numeric(dat) & all(dat >= 0) & use_3d) {
      maps_data <- maps_data %>%
        dplyr::mutate(e = dat / max(dat) * 100000)
      
      map <- mapdeck::mapdeck(
        style = mapdeck::mapdeck_style('dark'),
        pitch = 45
      ) %>%
        mapdeck::add_sf(
          data = base_map,
          stroke_colour = gray(1), 
          stroke_width = 3000,
          fill_colour = gray(0.5),
          layer_id = "base_map"
        ) %>%
        mapdeck::add_sf(
          data = maps_data %>%
            dplyr::mutate(tooltip = tooltip),
          fill_colour = var_name,
          fill_opacity = (255 / 100) * 75,
          legend = list(fill_colour = TRUE),
          auto_highlight = TRUE,
          tooltip = "tooltip",
          elevation = "e"
        )
    } else {
      map <- mapdeck::mapdeck(
        style = mapdeck::mapdeck_style('dark')
      ) %>%
        mapdeck::add_sf(
          data = base_map,
          stroke_colour = gray(1), 
          stroke_width = 3000,
          fill_colour = gray(0.5),
          layer_id = "base_map"
        ) %>%
        mapdeck::add_sf(
          data = maps_data %>%
            dplyr::mutate(tooltip = tooltip),
          fill_colour = var_name,
          fill_opacity = (255 / 100) * 75,
          stroke_colour = gray(0.2), 
          stroke_width = 3000,
          legend = list(fill_colour = TRUE),
          auto_highlight = TRUE,
          tooltip = "tooltip"
        )
    }
    
    
    map
  } 
  
  #make_a_map(var_name)
 
  var_names <- colnames(maps_data)[-1:-3]
  
  
  all_maps <- purrr::map(var_names, ~ make_a_map(., use_3d = use_3d))
  names(all_maps) <- var_names
  all_maps
}


make_EGIDS_map_labelled <- function(lang_polys_merged_cea, bring_it_in, base_map_unioned) {
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(bring_it_in %>%
                       dplyr::select(LANG_ISO, EGIDS))
  
  base_map_robin <- base_map_unioned %>%
    sf::st_transform('ESRI:54030') %>%
    sf::st_make_valid()
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030') %>%
    sf::st_make_valid()
  
  p <- ggplot(base_map_robin) +
    geom_sf(fill = "black", size = 0.2, colour = "white") +
    geom_sf(data = maps_data_robin,
            mapping = aes(fill = EGIDS), 
            colour = NA,
            alpha = 0.8) +
    ggrepel::geom_text_repel(
      aes(label = LANG_ISO, geometry = geometry),
      data = maps_data_robin,
      stat = "sf_coordinates",
      min.segment.length = 0,
      segment.size = 0.1,
      colour = "white",
      segment.colour = "white",
      size = 0.5,
      box.padding = 0.05
    ) +
    # geom_sf_text(aes(label = LANG_ISO), data = maps_data_robin,
    #              colour = "white") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "grey20", colour = NA))
  
  p
  

}

make_EGIDS_map_final <- function(lang_polys_merged_cea, final_data, base_map_unioned) {
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::filter(LANG_ISO != "chu") %>%
    dplyr::left_join(final_data %>%
                       dplyr::select(LANG_ISO, EGIDS, polygon_area))
  
  base_map_robin <- base_map_unioned %>%
    sf::st_transform('ESRI:54030') %>%
    sf::st_make_valid()
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030') %>%
    sf::st_make_valid() %>%
    dplyr::arrange(dplyr::desc(polygon_area))
  
  p <- ggplot(base_map_robin) +
    geom_sf(fill = NA, size = 0.2) +
    geom_sf(data = maps_data_robin,
            mapping = aes(fill = EGIDS), 
            colour = NA,
            alpha = 0.7) +
    theme_minimal() 
  
  p
  
  
}


make_double_check_maps <- function(lang_polys_merged_cea, bring_it_in, base_map, 
                       transformation_list, transformation_table, multi_soceco_langs) {
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(bring_it_in) %>%
    dplyr::left_join(multi_soceco_langs)
  
  base_map_robin <- base_map %>%
    sf::st_transform('ESRI:54030')
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  soceco_vars <- c("GDP_2010USD_10yr_median", "Gini_SWIID_10yr_median", "Ed_spending_pcGDP_10yr_median", "Mean_yr_school_10yr_median", "Life_exp_birth_10yr_median", "Life_exp_sixty_average", "world_language_as_official", "minority_education_policy", "urban_change")
  
  ## produce maps for languages with a single polygon vs. those with multiple
  pdf("figures/soceco_doublecheck_maps.pdf", height = 11.5, width = 14)
  for(i in 1:length(soceco_vars)) {
    var_name <- soceco_vars[[i]]
    map_df_1 <- maps_data_robin %>%
      dplyr::filter(num_polys == 1) %>%
      dplyr::select(LANG_ISO, !!var_name)
    p1 <- ggplot(base_map_robin) +
      geom_sf(data = map_df_1,
              aes_string(fill = var_name), 
              colour = NA,
              alpha = 1) +
      geom_sf(fill = NA, size = 0.2) +
      theme_minimal() +
      ggtitle("Single Country Languages") +
      theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))
    if(transformation_table$continuous_numeric[transformation_table$column_name == var_name] == 1) {
      p1 <- p1 +
        scale_fill_viridis(trans = transformation_list[[var_name]])
    } 
    #p1 
    
    map_df_2 <- maps_data_robin %>%
      dplyr::filter(num_polys > 1) %>%
      dplyr::select(LANG_ISO, !!var_name)
    p2 <- ggplot(base_map_robin) +
      geom_sf(data = map_df_2,
              aes_string(fill = var_name), 
              colour = NA,
              alpha = 1) +
      geom_sf(fill = NA, size = 0.2) +
      theme_minimal() +
      ggtitle("Languages Overlapping Multiple Countries") +
      theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))
    if(transformation_table$continuous_numeric[transformation_table$column_name == var_name] == 1) {
      p2 <- p2 +
        scale_fill_viridis(trans = transformation_list[[var_name]])
    } 
    #p2
    
    plot(p1 + p2 + plot_layout(nrow = 2))
    
  }
  dev.off()
  
}

temp_make_maps_chunks <- function(){
  loadd(variable_maps)
  var_names <- names(variable_maps)
  #var_name <- var_names[1]
  generate_map_code <- function(var_name) {
    snippet <-   
      "<details>
  <summary>Map for {var_name}</summary>
    \`\`\`{r {var_name}_map, echo=FALSE, eval=TRUE, fig.width=7.5, fig.height=3, out.width='80%'}
    plot(variable_maps[['{var_name}']])
    \`\`\`
</details>" 
    snippet <- snippet %>%
      stringr::str_replace_all("\\{var_name\\}", var_name)
  }
  
  txt <- capture.output(purrr::map_chr(var_names, ~ generate_map_code(.)) %>%
    paste(collapse = "\n\n") %>% cat()) %>%
    paste(collapse = "\n")
  
  clipr::write_clip(txt)
}

temp_make_maps_chunks2 <- function(){
  loadd(variable_maps_interactive)
  var_names <- names(variable_maps)
  #var_name <- var_names[1]
  generate_map_code <- function(var_name) {
    snippet <-   
      "#Map for {var_name}
      
    \`\`\`{r {var_name}_interactive_map, echo=FALSE, eval=TRUE, out.width='90%'}
    variable_maps_interactive[['{var_name}']]
    \`\`\`
    
  " 
    snippet <- snippet %>%
      stringr::str_replace_all("\\{var_name\\}", var_name)
    
    readr::write_lines(snippet, file.path("interactive", paste0(var_name, ".Rmd")))

  }
  
  purrr::walk(var_names, ~ generate_map_code(.)) %>%
    paste(collapse = "\n\n") %>% cat()
}

render_maps <- function(variable_maps_interactive) {
  rmarkdown::render_site("interactive")
}

snap <- function(x,rule=1,left=TRUE,...){
  stopifnot(
    is.numeric(x),
    is.numeric(rule),
    #is.finite(x),
    is.finite(rule)
  )
  if(!length(x))return(x)
  rule <- sort(unique(rule))
  if(length(rule)==1){
    stopifnot(rule > 0)
    lo <- min(x,na.rm=TRUE)
    hi <- max(x,na.rm=TRUE)
    lo <- (lo %/% rule) * rule - rule
    hi <- (hi %/% rule) * rule + rule
    rule <- seq(from=lo, to=hi, by=rule)
  }
  lt <- findInterval(x,rule)
  rt <- findInterval(x,rule) + 1
  lt[lt==0] <- 1
  rt[rt > length(rule)] <- length(rule)
  lt <- rule[lt]
  rt <- rule[rt]
  fun <- match.fun(if(left) '<=' else '<')
  closer <- ifelse(fun(abs(x-lt),abs(rt - x)), lt, rt)
  closer
}

find_best_power_ladder_transforms <- function(bring_it_in) {
  classes <- lapply(bring_it_in, class)
  numeric_vars <- which(classes == "numeric")
  
  df_num <- bring_it_in[ , numeric_vars] %>%
    as.data.frame() 
  
  binaryish <- c("minority_education_policy",
                   "official_in_any_country",
                 "used_in_education",
                 grep("world_language_", colnames(df_num), value = TRUE))
  
  df_num <- df_num[ , -which(colnames(df_num) %in% binaryish)]
  
  pos <- purrr::map_lgl(df_num, ~all(na.omit(.) >= 0))
  
  df_pos <- df_num[ , pos]
  
  df_pos[df_pos == 0] <- NA
  df_pos <- df_pos %>%
     dplyr::mutate_all(~ ifelse(!is.finite(.), NA, .))
  
  lambdas <- purrr::map_dbl(df_pos, ~car::powerTransform(.)$lambda)
  
  # lambdas <- purrr::map(df_pos, 
  #                       ~MASS::boxcox(.x~1, lambda = seq(-3, 3, 1/10),
  #                                     plotit = FALSE))
  # 
  # lambdas <- purrr::map_dbl(lambdas,
  #                           ~.x$x[which.max(.x$.y)])
  
  
  ## vars with negatives (change vars)
  # df_neg <- df_num[ , !pos]
  # df_neg[df_neg == 0] <- NA
  # lambdas_neg <- purrr::map(df_neg, ~list(neg = car::powerTransform(-.[. < 0])$lambda,
  #                                             pos = car::powerTransform(.[. > 0])$lambda))
  
  lambdas_snapped <- snap(lambdas, c(-3, -2, -1, -0.5, 0, 0.5, 1, 2, 3))
  
  lambdas_snapped[lambdas_snapped == -0.5] <- 0
  
  transform_vector <- rep(1, ncol(bring_it_in))
  names(transform_vector) <- colnames(bring_it_in)
  
  transform_vector[names(lambdas_snapped)] <- lambdas_snapped
  
  transform_name <- dplyr::case_when(
    transform_vector == -1.0 ~ "negative reciprocal",
    transform_vector == 0.0 ~ "log",
    transform_vector == 0.5 ~ "square root",
    transform_vector == 1.0 ~ "identity",
    transform_vector == 2.0 ~ "square",
    transform_vector == 3.0 ~ "cube"
  )
  
  continuous_vars <- colnames(bring_it_in) %in% names(lambdas_snapped) %>%
    as.numeric()
  
  transform_table <- dplyr::tibble(column_name = names(transform_vector),
                                   power_transform = transform_vector,
                                   transformation_name = transform_name,
                                   continuous_numeric = continuous_vars)
  
  loggers <- which(transform_table$transformation_name == "log")
  
  zeroers <- apply(bring_it_in[ , loggers], 2, function(x) sum(x == 0, na.rm = TRUE) > 0)
  
  zeroers <- names(zeroers)[zeroers]
  
  small_adds <- apply(bring_it_in[ , zeroers], 2, function(x) min(x[x != 0], na.rm = TRUE))
  small_add_vec <- rep(0, ncol(bring_it_in))
  names(small_add_vec) <- colnames(bring_it_in)
  small_add_vec[names(small_adds)] <- small_adds
  
  ## lastly make change related variables use a "signed square root" transformation
  
  transformation_table <- transform_table %>%
    dplyr::mutate(transformation_name = ifelse(endsWith(column_name, "change"), 
                                               "signed square root", transformation_name),
                  continuous_numeric = ifelse(transformation_name == "signed square root",
                                              1, continuous_numeric),
                  power_transform = ifelse(transformation_name == "signed square root",
                                              0.5, power_transform),
                  small_add = small_add_vec / 2)
  
  transformation_table
}

make_distribution_plots <- function(bring_it_in, all_data_transformed) {
  classes <- lapply(bring_it_in, class)
  numeric_vars <- which(classes == "numeric")
  
  classes2 <- lapply(all_data_transformed, class)
  numeric_vars2 <- which(classes2 == "numeric")
  
  df_num <- bring_it_in[ , numeric_vars] %>%
    dplyr::mutate_all(~(. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)) %>%
    tidyr::gather(var, val)
  
  df_num2 <- all_data_transformed[ , numeric_vars2] %>%
    dplyr::mutate_all(~(. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)) %>%
    tidyr::gather(var, val)
  
  # p_untr <- ggplot(df_num, aes(val, var)) +
  #   geom_density_ridges2() +
  #   geom_beeswarm(size = 0.25, groupOnX = FALSE, beeswarmArgs=list(side=1)) +
  #   theme_minimal()
  # 
  # p_untr
  
  pal <- hues::iwanthue(dplyr::n_distinct(df_num$var)) %>% unname()
  
  p_untr <- ggplot(df_num, aes(val, var)) +
    #geom_density_ridges2() +
    geom_quasirandom(aes(color = var), size = 1, groupOnX = FALSE) +
    scale_color_manual(values = pal) +
    ylab("Variable") +
    xlab("Standardised Value") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 24))
  
  #p_untr
  
  # p_tr <- ggplot(df_num2, aes(val, var)) +
  #   geom_density_ridges2() +
  #   geom_beeswarm(groupOnX = FALSE, beeswarmArgs=list(side=1)) +
  #   theme_minimal()
  
  
  p_tr <- ggplot(df_num2, aes(val, var)) +
    #geom_density_ridges2() +
    geom_quasirandom(aes(color = var), size = 1, groupOnX = FALSE) +
    scale_color_manual(values = pal) +
    ylab("Variable") +
    xlab("Standardised Value") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 24))
  
  #p_tr
  
  list(untransformed = p_untr, transformed = p_tr)
  
}

make_corr_plots <- function(all_data_transformed) {
  corrs <- round(cor(all_data_transformed %>%
                       dplyr::select_if(is.numeric) %>%
                       dplyr::mutate_all(~ifelse(is.finite(.), ., NA)) %>%
                       as.matrix(),
                     use = "pairwise.complete.obs"),
                     2)
  p <- ggcorrplot::ggcorrplot(corrs, type = "lower", method = "circle", hc.order = TRUE,
                  lab = TRUE, tl.cex = 12, lab_size = 2,
                  show.diag = FALSE)
  p <- p +
    scale_fill_scico(palette = "vik", limits = c(-1, 1))
  
  return(p)
  #ggplot2::ggsave("corr_test.png", p, width = 12, height = 12)
}

make_endangered_outlier_plot <- function(bring_it_in) {
  
  add_noise <- 0.15
  
  # dat_AES_POP <- bring_it_in %>%
  #   dplyr::select(LANG_ISO, AES, EGIDS, L1_POP) %>%
  #   dplyr::mutate(EGIDS_reduced = ifelse(as.numeric(EGIDS) <= 6, "not endangered", as.character(EGIDS)) %>%
  #                   factor(levels = c("not endangered", "6b", "7", "8a", "8b", "9"), ordered = TRUE)) %>%
  #   dplyr::mutate(AES_num = as.numeric(AES) + rnorm(n(), sd = add_noise), 
  #                 EGIDS_num = as.numeric(EGIDS) + rnorm(n(), sd = add_noise),
  #                 EGIDS_reduced_num = as.numeric(EGIDS_reduced) + rnorm(n(), sd = add_noise)) %>%
  #   tidyr::drop_na(AES_num, L1_POP) %>%
  #   dplyr::mutate(resid = lm(L1_POP ~ AES_num, data = .) %>%
  #                                  residuals()) %>%
  #   #dplyr::group_by(AES) %>%
  #   dplyr::mutate(quant_98 = quantile(resid, 0.98),
  #                 label = ifelse(resid >= quant_98, LANG_ISO, ""))
  
  dat <- bring_it_in %>%
    dplyr::select(LANG_ISO, AES, EGIDS, L1_POP) %>%
    dplyr::mutate(EGIDS_reduced = ifelse(as.numeric(EGIDS) <= 6, "not endangered", as.character(EGIDS)) %>%
                    factor(levels = c("not endangered", "6b", "7", "8a", "8b", "9"), ordered = TRUE)) %>%
    dplyr::mutate(AES_num = as.numeric(AES) + rnorm(n(), sd = add_noise), 
                  EGIDS_num = as.numeric(EGIDS) + rnorm(n(), sd = add_noise),
                  EGIDS_reduced_num = as.numeric(EGIDS_reduced) + rnorm(n(), sd = add_noise)) %>%
    dplyr::mutate(label_AES_POP = case_when(
      as.numeric(AES) == 6 & L1_POP > 10000 ~ LANG_ISO,
      as.numeric(AES) == 5 & L1_POP > (1e+05 - 1000) ~ LANG_ISO,
      as.numeric(AES) == 4 & L1_POP > (1e+05 + 100000) ~ LANG_ISO,
      as.numeric(AES) == 3 & L1_POP > 1e+07 ~ LANG_ISO,
      TRUE ~ ""
    ),
    label_EGIDS_POP = case_when(
      as.numeric(EGIDS_reduced) == 6 & L1_POP > 10000 ~ LANG_ISO,
      as.numeric(EGIDS_reduced) == 5 & L1_POP > (1e+05 - 1000) ~ LANG_ISO,
      as.numeric(EGIDS_reduced) == 4 & L1_POP > (1e+05 + 100000) ~ LANG_ISO,
      as.numeric(EGIDS_reduced) == 3 & L1_POP > 1e+06 - 100000 ~ LANG_ISO,
      TRUE ~ ""
    ),
    label_AES_EGIDS = ifelse((as.numeric(AES) - as.numeric(EGIDS_reduced) > 2) |
                               (EGIDS_reduced == "8a" & AES == "extinct"), 
                             LANG_ISO, "")
    )
  
  
  AES_POP <- ggplot(dat, aes(L1_POP, AES_num, label = label_AES_POP)) +
    geom_text_repel(force = 3, min.segment.length = 0.2, segment.color = "grey60") +
    geom_pointdensity(adjust = 2) +
    scale_x_log10() +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = levels(dat$AES)) +
    scale_colour_viridis(trans = "sqrt") +
    #xlab("L1 Speaker Population (note: log-scaled axis)") +
    xlab("") +
    ylab("AES Endangerment Scale") +
    theme_minimal() +
    theme(legend.position = "none") 
  
  AES_POP
  
  # pdf("test_outlier_1.pdf")
  # AES_POP
  # dev.off()
  
  EGIDS_POP <- ggplot(dat, aes(L1_POP, EGIDS_reduced_num, label = label_EGIDS_POP)) +
    geom_text_repel(force = 3, min.segment.length = 0.2, segment.color = "grey60") +
    geom_pointdensity(adjust = 2) +
    scale_x_log10() +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = levels(dat$EGIDS_reduced)) +
    scale_colour_viridis(trans = "sqrt") +
    xlab("L1 Speaker Population (note: log-scaled axis)") +
    ylab("EGIDS Endangerment Scale (reduced)") +
    theme_minimal() +
    theme(legend.position = "none")
  
  EGIDS_POP
  
  # pdf("test_outlier_2.pdf")
  # AES_POP
  # dev.off()
  
  
  AES_EGIDS <- ggplot(dat, aes(AES_num, EGIDS_reduced_num, label = label_AES_EGIDS)) +
    geom_text_repel(force = 1, min.segment.length = 0.2, segment.color = "grey60", segment.alpha = 0.7) +
    geom_pointdensity(adjust = 1) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = levels(dat$AES)) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = levels(dat$EGIDS_reduced)) +
    #scale_fill_viridis() +
    scale_colour_viridis(trans = "sqrt") +
    xlab("AES Endangerment Scale") +
    #ylab("EGIDS Endangerment Scale (reduced)") +
    ylab("") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 16, angle = -25))
  
  AES_EGIDS
  
  (AES_POP + (plot_spacer() + theme_minimal()) + EGIDS_POP + AES_EGIDS + plot_layout(ncol = 2, nrow = 2)) &
    theme(axis.title = element_text(size = 24),
          axis.text = element_text(size = 16))
}

#lang_pts <- lang_pts_other
make_lang_pts_voronoi <- function(lang_pts, base_map_unioned) {
  
  envelope <- base_map_unioned %>% 
    sf::st_transform(crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) %>%
    sf::st_make_valid()
  
  dup_pts = sf::st_equals(lang_pts) %>%
    sapply(function(x) length(x) > 1) %>%
    which()
  
  lang_pts_deduped <- lang_pts %>%
    sf::st_transform(crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) 
  
  lang_pts_deduped[dup_pts, ]$geometry <- lang_pts_deduped[dup_pts, ]$geometry %>%
    `+`(rnorm(length(dup_pts), sd = 1000))
  
  lang_pts_voronoi <- lang_pts_deduped %>%
    sf::st_combine() %>%
    sf::st_voronoi(envelope = envelope) %>%
    sf::st_cast() %>%
    sf::st_make_valid() %>%
    data.frame(geometry = .) %>%
    sf::st_sf(.) %>%
    sf::st_join(., lang_pts_deduped) %>%
    dplyr::filter(LMP_CLASS != "L") %>%
    sf::st_set_agr(., "constant") %>%
    sf::st_intersection(., envelope) %>%
    sf::st_cast() %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid()
  
  #sf::st_union() %>%
  # sf::st_voronoi(envelope = base_map_unioned %>% 
  #                  sf::st_transform(crs = sp::CRS("+proj=cea +datum=WGS84 +units=m"))),
  
  lang_pts_voronoi
}

as.phylo.formula.better <- function (x, data = parent.frame(), collapse = TRUE, ...) 
{
  err <- "Formula must be of the kind ~A1/A2/.../An."
  if (length(x) != 2) 
    stop(err)
  if (x[[1]] != "~") 
    stop(err)
  f <- x[[2]]
  taxo <- list()
  while (length(f) == 3) {
    if (f[[1]] != "/") 
      stop(err)
    f3.txt <- deparse(f[[3]])
    if (!is.factor(data[[f3.txt]])) 
      stop(paste("Variable", f3.txt, "must be a factor"))
    taxo[[f3.txt]] <- data[[f3.txt]]
    if (length(f) > 1) 
      f <- f[[2]]
  }
  f.txt <- deparse(f)
  if (!is.factor(data[[f.txt]])) 
    stop(paste("Variable", f.txt, "must be a factor."))
  taxo[[f.txt]] <- data[[f.txt]]
  taxo.data <- as.data.frame(taxo)
  leaves.names <- as.character(taxo.data[, 1])
  taxo.data[, 1] <- 1:nrow(taxo.data)
  f.rec <- function(subtaxo) {
    u <- ncol(subtaxo)
    levels <- unique(subtaxo[, u])
    if (u == 1) {
      if (length(levels) != nrow(subtaxo)) 
        warning("leaves names are not unique.")
      return(as.character(subtaxo[, 1]))
    }
    t <- character(length(levels))
    for (l in 1:length(levels)) {
      x <- f.rec(subtaxo[subtaxo[, u] == levels[l], ][1:(u - 
                                                           1)])
      t[l] <- paste0("(", paste(x, collapse = ","), 
                     ")", levels[l])
    }
    t
  }
  string <- paste0("(", paste(f.rec(taxo.data), collapse = ","), 
                   ");")
  phy <- ape::read.tree(text = string)
  if (collapse) 
    phy <- ape::collapse.singles(phy)
  phy$tip.label <- leaves.names[as.numeric(phy$tip.label)]
  phy
}

label_glottolog_tree <- function(tree_glottolog, glottolog_all) {
  
  node_label_df <- dplyr::tibble(node_label = tree_glottolog$node.label) %>%
    dplyr::left_join(glottolog_all %>%
                       dplyr::select(node_label = Glottocode,
                                     level,
                                     Name)) %>%
    dplyr::mutate(Name = gsub("'", "-", Name),
                  label = paste0(Name, " (", level, ")"))
  
  new_tree <- tree_glottolog
  new_tree$node.label <- node_label_df$Name
  
  new_tree
  
  #ape::write.tree(new_tree, "data/trees/glottolog_tree_labelled.nw")
}

scale_tree <- function(tree_glottolog_labelled, dates) {
  ages <- data.frame(a=as.character(dates$clade),b=as.numeric(dates$`relative date`))
  phylo <- ape::read.tree(text = ape::write.tree(tree_glottolog_labelled))
  label <- c(phylo$tip.label,phylo$node.label)
  node <- phylo$edge
  rownames(node) <- label[node[,1]]
  node <- cbind(node,NA,NA)
  #set dates of root and tips
  node[node[,1]==8004,3] <- 1
  node[node[,2]<8004,4] <- 0
  #set top-level family date to 1
  idx <- which(((node[,1]==8004)*(node[,2]>8004))==1)
  node[idx,4] <- 1
  idx <- unlist(sapply(node[idx,2],function (i) which(node[,1]==i)))
  node[idx,3] <- 1
  #set known dates
  idx <- unlist(sapply(as.character(ages$a),function (i) which(rownames(node)==i)))
  tmp <- unlist(sapply(as.character(ages$a),function (i) sum(rownames(node)==i, na.rm = TRUE)))
  node[idx,3] <- rep(ages$b,tmp)
  known <- unique(node[idx,1]) ##
  idx <- unlist(sapply(known,function (i) which(node[,2]==i)))
  node[idx,4] <- ages$b
  #calculate branch length for each family
  idx <- which(((node[,1]==8004)*(node[,2]>8004))==1)
  for (i in 1:(length(idx)-1)) {
    tmp <- node[idx[i]:idx[i+1],]
    idx2 <- which(tmp[,4]==0)
    path <- tmp[idx2,1]
    tim <- tmp[idx2,3]
    idx2 <- sapply(tmp[idx2,1],function (i) ifelse(i>8004,which(tmp[,2]==i),1))
    path <- cbind(path,tmp[idx2,1])
    tim <- cbind(tim,tmp[idx2,3])
    while (sum(path[,dim(path)[2]]==8004)<dim(path)[1]) {
      idx2 <- sapply(tmp[idx2,1],function (i) ifelse(i>8004,which(tmp[,2]==i),1))
      path <- cbind(path,tmp[idx2,1])
      tim <- cbind(tim,tmp[idx2,3])
    }
    if (sum(is.na(tim))>0) {
      n <- sapply(1:dim(tim)[1],function (i) min(which(!is.na(tim[i,]))))
      n <- min(tim[cbind(c(1:dim(tim)[1]),n)]/n)
      n1 <- sapply(1:dim(tim)[1],function (i) tim[i,-1]-tim[i,-dim(tim)[2]])
      n1 <- min(n1[which((n1>0)*(!is.na(n1))==1)])
      if (dim(tim)[2]>2) {
        n2 <- sapply(1:dim(tim)[1],function (i) (tim[i,-c(1,2)]-tim[i,-(dim(tim)[2]-c(0,1))])/2)
        n2 <- min(n2[which((n2>0)*(!is.na(n2))==1)])
      } else {
        n2 <- Inf
      }
      #calculate min branch length for a classification level
      step <- min(c(n,n1,n2))
      #filling unknown dates
      idx2 <- min(which(is.na(tmp[,4])))
      idx3 <- which(tmp[,1]==tmp[idx2,2])
      tmp[idx2,4] <- tmp[idx3,3] <- tmp[idx2,3]-step
      idx2 <- which(is.na(tmp[,4]))
      while (length(idx2)>0) {
        idx2 <- min(idx2)
        idx3 <- which(tmp[,1]==tmp[idx2,2])
        tmp[idx2,4] <- tmp[idx3,3] <- tmp[idx2,3]-step
        idx2 <- which(is.na(tmp[,4]))
      }
    }
    node[idx[i]:idx[i+1],] <- tmp
  }
  phylo$edge.length <- node[,3]-node[,4]
  
  phylo
}

make_nb_net_mat <- function(nb_net) {
  new_mat <- spdep::nb2mat(nb_net, zero.policy = TRUE, style = "B")
  diag(new_mat) <- 1
  new_mat
}

make_projection_maps <- function(lang_polys_merged_cea, map_hex_robin, final_data, base_map_unioned, base_map_w_antarctica) {
  projections_3 <- readr::read_csv("data/projections/projection3level.csv")
  projections_7 <- readr::read_csv("data/projections/projection7level.csv")
  
  S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
  IS_sqrt <- function(x){x^2*sign(x)}
  S_sqrt_trans <- function() scales::trans_new("S_sqrt",S_sqrt,IS_sqrt)
  
  base_map_robin <- base_map_unioned %>%
    #rmapshaper::ms_simplify(0.5, keep_shapes = TRUE) %>%
    sf::st_transform('ESRI:54030')
  
  hex_area <- map_hex_robin[[1]] %>%
    sf::st_area()
  
  base_map_no_islands <- base_map_robin %>%
    rmapshaper::ms_filter_islands(hex_area) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:n()) %>%
    sf::st_make_valid()
  
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(projections_3) %>%
    dplyr::filter(!is.na(`change in rank in 50 years`)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, `change in rank in 50 years`) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(`change in rank in 50 years`, na.rm = TRUE),
                     sd_res = sd(`change in rank in 50 years`, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf("figures/language_endangerment_projections_3level_50years.pdf", height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Change in Risk\nover 50 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)) +
    scico::scale_colour_scico(name = "Mean Change in Risk\nover 50 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Change in Risk\nover 50 Years", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Change in Risk\nover 50 Years", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()
  
  
  
  #############################################################################
  
  
  base_map_robin <- base_map_unioned %>%
    #rmapshaper::ms_simplify(0.5, keep_shapes = TRUE) %>%
    sf::st_transform('ESRI:54030')
  
  hex_area <- map_hex_robin[[1]] %>%
    sf::st_area()
  
  base_map_no_islands <- base_map_robin %>%
    rmapshaper::ms_filter_islands(hex_area) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:n()) %>%
    sf::st_make_valid()
  
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(projections_3) %>%
    dplyr::filter(!is.na(`change in rank in 100 years`)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, `change in rank in 100 years`) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(`change in rank in 100 years`, na.rm = TRUE),
                     sd_res = sd(`change in rank in 100 years`, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf("figures/language_endangerment_projections_3level_100years.pdf", height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Change in Risk\nover 100 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)) +
    scico::scale_colour_scico(name = "Mean Change in Risk\nover 100 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Change in Risk\nover 100 Years", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Change in Risk\nover 100 Years", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()
  
  
  ###############################################################################
  
  
  base_map_robin <- base_map_unioned %>%
    #rmapshaper::ms_simplify(0.5, keep_shapes = TRUE) %>%
    sf::st_transform('ESRI:54030')
  
  hex_area <- map_hex_robin[[1]] %>%
    sf::st_area()
  
  base_map_no_islands <- base_map_robin %>%
    rmapshaper::ms_filter_islands(hex_area) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:n()) %>%
    sf::st_make_valid()
  
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(projections_7) %>%
    dplyr::filter(!is.na(`change in rank in 50 years`)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, `change in rank in 50 years`) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(`change in rank in 50 years`, na.rm = TRUE),
                     sd_res = sd(`change in rank in 50 years`, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf("figures/language_endangerment_projections_7level_50years.pdf", height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Change in Risk\nover 50 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-3, -2, -1, -0.1, 0.1, 1, 2, 3)) +
    scico::scale_colour_scico(name = "Mean Change in Risk\nover 50 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-3, -2, -1, -0.1, 0.1, 1, 2, 3)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Change in Risk\nover 50 Years", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Change in Risk\nover 50 Years", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()
  
  
  
  #############################################################################
  
  
  base_map_robin <- base_map_unioned %>%
    #rmapshaper::ms_simplify(0.5, keep_shapes = TRUE) %>%
    sf::st_transform('ESRI:54030')
  
  hex_area <- map_hex_robin[[1]] %>%
    sf::st_area()
  
  base_map_no_islands <- base_map_robin %>%
    rmapshaper::ms_filter_islands(hex_area) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:n()) %>%
    sf::st_make_valid()
  
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(projections_7) %>%
    dplyr::filter(!is.na(`change in rank in 100 years`)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, `change in rank in 100 years`) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(`change in rank in 100 years`, na.rm = TRUE),
                     sd_res = sd(`change in rank in 100 years`, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf("figures/language_endangerment_projections_7level_100years.pdf", height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Change in Risk\nover 100 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-3, -2, -1, -0.1, 0.1, 1, 2, 3)) +
    scico::scale_colour_scico(name = "Mean Change in Risk\nover 100 Years", palette = "vik", limits = c(-rescaler1, rescaler1), trans = "S_sqrt", breaks = c(-3, -2, -1, -0.1, 0.1, 1, 2, 3)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Change in Risk\nover 100 Years", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Change in Risk\nover 100 Years", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()
  
}

make_latent_maps <- function(latent_preds, lang_polys_merged_cea, map_hex_robin, final_data, base_map_unioned, base_map_w_antarctica) {
  
  file_name <- "predict.all3.csv"
  variable <- "residual1"
  
  base_map_robin <- base_map_unioned %>%
    #rmapshaper::ms_simplify(0.5, keep_shapes = TRUE) %>%
    sf::st_transform('ESRI:54030')
  
  hex_area <- map_hex_robin[[1]] %>%
    sf::st_area()
  
  base_map_no_islands <- base_map_robin %>%
    rmapshaper::ms_filter_islands(hex_area) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:n()) %>%
    sf::st_make_valid()
  
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  ######## get continents ############
  
  maps_cont <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::filter(LANG_ISO %in% final_data$LANG_ISO) 
  
  cont_map <- base_map_w_antarctica %>%
    sf::st_transform(sf::st_crs(maps_cont)) %>%
    sf::st_buffer(5000) %>%
    dplyr::select(continent)
  cont_dat <- maps_cont %>% 
    sf::st_join(cont_map) 
  
  cont_dat <- cont_dat %>%
    dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
    dplyr::group_by(LANG_ISO) %>%
    dplyr::summarise(continent = continent[which.max(area)])
  
  # plot(cont_dat %>% dplyr::select(continent))
  
  readr::write_csv(cont_dat %>% dplyr::as_tibble() %>% dplyr::select(-geometry),
                   "data/continents_by_ISO.csv")
  
  ######## get continents for missing EGIDS ############
  
  maps_cont <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::filter(LANG_ISO %in% final_data_missing_EGIDS$LANG_ISO) 
  
  cont_map <- base_map_w_antarctica %>%
    sf::st_transform(sf::st_crs(maps_cont)) %>%
    sf::st_buffer(5000) %>%
    dplyr::select(continent)
  cont_dat <- maps_cont %>% 
    sf::st_join(cont_map) 
  
  cont_dat <- cont_dat %>%
    dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
    dplyr::group_by(LANG_ISO) %>%
    dplyr::summarise(continent = continent[which.max(area)])
  
  # plot(cont_dat %>% dplyr::select(continent))
  
  readr::write_csv(cont_dat %>% dplyr::as_tibble() %>% dplyr::select(-geometry),
                   "data/continents_by_ISO_missing_EGIDS.csv")
  
  final_data_missing_EGIDS <- final_data_missing_EGIDS %>%
    dplyr::left_join(cont_dat) %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(-geometry)
  
  ########### continue ###############
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(residual1, na.rm = TRUE),
                     sd_res = sd(residual1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  #########################################2##################
  
  file_name <- "predict.all3.csv"
  variable <- "residual2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(residual2, na.rm = TRUE),
                     sd_res = sd(residual2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  #########################################3##################
  
  file_name <- "predict.all.7.csv"
  variable <- "residual1"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(residual1, na.rm = TRUE),
                     sd_res = sd(residual1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  
  #########################################4##################
  
  file_name <- "predict.all.7.csv"
  variable <- "residual2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(residual2, na.rm = TRUE),
                     sd_res = sd(residual2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  #########################################5##################
  
  file_name <- "predict.latent3.csv"
  variable <- "latent1"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent1, na.rm = TRUE),
                     sd_res = sd(latent1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  
  #########################################6##################
  
  file_name <- "predict.latent3.csv"
  variable <- "latent2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent2, na.rm = TRUE),
                     sd_res = sd(latent2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  #########################################7##################
  
  file_name <- "predict.latent3.2.csv"
  variable <- "latent1"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent1, na.rm = TRUE),
                     sd_res = sd(latent1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  
  #########################################8##################
  
  file_name <- "predict.latent3.2.csv"
  variable <- "latent2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent2, na.rm = TRUE),
                     sd_res = sd(latent2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  
  #########################################9##################
  
  file_name <- "predict.latent7.csv"
  variable <- "latent1"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent1, na.rm = TRUE),
                     sd_res = sd(latent1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  
  #########################################10##################
  
  file_name <- "predict.latent7.csv"
  variable <- "latent2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent2, na.rm = TRUE),
                     sd_res = sd(latent2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  #########################################11##################
  
  file_name <- "predict.latent7.2.csv"
  variable <- "latent1"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent1, na.rm = TRUE),
                     sd_res = sd(latent1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  
  #########################################12##################
  
  file_name <- "predict.latent7.2.csv"
  variable <- "latent2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent2, na.rm = TRUE),
                     sd_res = sd(latent2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  pdf(file.path("figures", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "berlin") +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "berlin") +
    theme_minimal()
  
  ggplot(base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "berlin", begin = 0.5) +
    theme_minimal()
  
  dev.off()
  
  
  
  
}


make_latent_maps2 <- function(latent_preds2, lang_polys_merged_cea, map_hex_robin, final_data, base_map_unioned, base_map_w_antarctica) {
  
  file_name <- "predict1.csv"
  
  latent_preds <- latent_preds2
  
  latent_preds[[file_name]] <- latent_preds[[file_name]] %>%
    dplyr::mutate(y_avg_minus_y_obs = y_avg - y_obs,
                  y_hat_minus_y_obs = y_hat - y_obs)
  
  variable <- "y_avg_minus_y_obs"
  
  base_map_robin <- base_map_unioned %>%
    #rmapshaper::ms_simplify(0.5, keep_shapes = TRUE) %>%
    sf::st_transform('ESRI:54030')
  
  hex_area <- map_hex_robin[[1]] %>%
    sf::st_area()
  
  base_map_no_islands <- base_map_robin %>%
    rmapshaper::ms_filter_islands(hex_area) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:n()) %>%
    sf::st_make_valid()
  
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  ######## get continents ############
  
  maps_cont <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::filter(LANG_ISO %in% final_data$LANG_ISO) 
  
  cont_map <- base_map_w_antarctica %>%
    sf::st_transform(sf::st_crs(maps_cont)) %>%
    sf::st_buffer(5000) %>%
    dplyr::select(continent)
  cont_dat <- maps_cont %>% 
    sf::st_join(cont_map) 
  
  cont_dat <- cont_dat %>%
    dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
    dplyr::group_by(LANG_ISO) %>%
    dplyr::summarise(continent = continent[which.max(area)])
  
  # plot(cont_dat %>% dplyr::select(continent))
  
  readr::write_csv(cont_dat %>% dplyr::as_tibble() %>% dplyr::select(-geometry),
                   "data/continents_by_ISO.csv")
  
  ########### continue ###############
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(y_avg_minus_y_obs, na.rm = TRUE),
                     sd_res = sd(y_avg_minus_y_obs, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()
  
  #########################################2##################
  
  file_name <- "predict1.csv"
  
  latent_preds[[file_name]] <- latent_preds[[file_name]] %>%
    dplyr::mutate(y_avg_minus_y_obs = y_avg - y_obs,
                  y_hat_minus_y_obs = y_hat - y_obs)
  
  variable <- "y_hat_minus_y_obs"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(y_hat_minus_y_obs, na.rm = TRUE),
                     sd_res = sd(y_hat_minus_y_obs, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()
  
  #########################################3##################
  
  file_name <- "predict22.csv"
  
  latent_preds[[file_name]] <- latent_preds[[file_name]] %>%
    dplyr::mutate(y_avg_minus_y_obs = y_avg - y_obs,
                  y_hat_minus_y_obs = y_hat - y_obs)
  
  variable <- "y_avg_minus_y_obs"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(y_avg_minus_y_obs, na.rm = TRUE),
                     sd_res = sd(y_avg_minus_y_obs, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()
  
  
  #########################################4##################
  
  file_name <- "predict22.csv"
  
  latent_preds[[file_name]] <- latent_preds[[file_name]] %>%
    dplyr::mutate(y_avg_minus_y_obs = y_avg - y_obs,
                  y_hat_minus_y_obs = y_hat - y_obs)
  
  variable <- "y_hat_minus_y_obs"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(y_hat_minus_y_obs, na.rm = TRUE),
                     sd_res = sd(y_hat_minus_y_obs, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off()  
  
  #########################################5##################
  
  file_name <- "latent1.2.csv"
  
  variable <- "latent1"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent1, na.rm = TRUE),
                     sd_res = sd(latent1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off() 
  
  
  #########################################6##################
  
  file_name <- "latent1.2.csv"
  
  variable <- "latent2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent2, na.rm = TRUE),
                     sd_res = sd(latent2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off() 
  
  #########################################7##################
  
  file_name <- "latent22.2.csv"
  
  variable <- "latent1"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent1, na.rm = TRUE),
                     sd_res = sd(latent1, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off() 
  
  
  #########################################8##################
  
  file_name <- "latent22.2.csv"
  
  variable <- "latent2"
  
  maps_data <- lang_polys_merged_cea %>%
    dplyr::select(LANG_ISO) %>%
    dplyr::left_join(latent_preds[[file_name]],
                     by = c("LANG_ISO" = "X1")) %>%
    dplyr::filter(!is.na(y_avg)) 
  
  maps_data_robin <- maps_data %>%
    sf::st_transform('ESRI:54030')
  
  map_df <- maps_data_robin %>%
    dplyr::select(LANG_ISO, !!variable) %>%
    dplyr::mutate(a = 1:n())
  
  hex_map_w_data <- map_hex_robin %>%
    sf::st_as_sf() %>%
    dplyr::mutate(a = 1:n()) %>%
    sf::st_join(map_df) %>%
    dplyr::group_by(a.x) %>%
    dplyr::summarise(mean_res = mean(latent2, na.rm = TRUE),
                     sd_res = sd(latent2, na.rm = TRUE)) %>%
    sf::st_make_valid()
  
  overlaps <- hex_map_w_data %>%
    sf::st_join(base_map_no_islands)
  
  hexes_yes <- overlaps %>%
    dplyr::filter(!is.na(id)) %>%
    sf::st_intersection(base_map_no_islands)
  
  hexes_no <- overlaps %>%
    dplyr::filter(is.na(id))
  
  hexes_final <- bind_rows(hexes_no, hexes_yes) %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_crs(sf::st_crs(base_map_robin))
  
  ocean_box <- sf::st_make_grid(base_map_unioned, n = 1) 
  
  crs <- st_crs(ocean_box)
  ocean_box <- st_set_crs(ocean_box, NA)
  ocean_box <- ocean_box %>% 
    sf::st_segmentize(1) %>% 
    st_set_crs(crs) %>%
    sf::st_transform('ESRI:54030')
  
  pdf(file.path("figures/latent2", paste0(file_name, ".", variable, ".pdf")), height = 6, width = 14)
  
  rescaler1 <- max(abs(hexes_final$mean_res), na.rm = TRUE)
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = mean_res, colour = mean_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    scico::scale_colour_scico(name = "Mean Latent Risk", palette = "vik", limits = c(-rescaler1, rescaler1)) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  ggplot(ocean_box) +
    geom_sf(fill = "grey20", colour = NA) +
    geom_sf(data = base_map_robin %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)) +
    geom_sf(aes(fill = sd_res, colour = sd_res), data = hexes_final, size = 0.1) +
    geom_sf(fill = NA, inherit.aes = FALSE, colour = "black", size = 0.1) +
    scico::scale_fill_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    scico::scale_colour_scico(name = "SD Latent Risk", palette = "vik", begin = 0.5) +
    theme_minimal() +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = scales::alpha("white", 0.25)))
  
  dev.off() 
  
  
  
}

plot_continents <- function() {
  ggplot(base_map_w_antarctica %>%
           sf::st_transform('ESRI:54030') %>%
           dplyr::select(continent)) + 
    geom_sf(aes(fill = continent, colour = continent), size = 1.5) +
    geom_sf(aes(fill = continent, colour = continent), size = 4,
            data = base_map_w_antarctica[base_map_w_antarctica$continent == "Seven seas (open ocean)", ]) +
    geom_sf(inherit.aes = FALSE, fill = NA) +
    theme_minimal()
}
  

generate_centred_circles <- function(lang_polys_unioned, circle_area_km = 10000, res = 180) {
  
  circle_area <- circle_area_km * 1000000
  
  r <- sqrt(circle_area / pi)
  
  centroids <- sf::st_centroid(lang_polys_unioned, of_largest_polygon = TRUE) %>%
    sf::st_coordinates()
  
  angle <- 360 / res
  sweep <- seq(0, 360, by = angle)
  #centroid <- centroids[1, ]
  make_circle <- function(centroid, angles, r) {
    pts <- geosphere::destPoint(centroid, angles, r)
  }
  
  all_circs <- pbapply::pblapply(purrr::array_branch(centroids, 1),
                                 make_circle, angles = sweep, r = r)
  
  all_circs_sf <- purrr::map(all_circs,
                             ~sf::st_linestring(.x) %>% 
                               sf::st_polygonize()) %>%
    sf::st_as_sfc() %>%
    sf::st_sf() %>%
    dplyr::mutate(LANG_ISO = lang_polys_unioned$LANG_ISO) %>%
    sf::st_set_crs(sf::st_crs(lang_polys_unioned)) %>%
    sf::st_make_valid() %>%
    sf::st_cast()
  
  all_circs_sf
  
}

get_lang_neighbourhood_stats <- function(lang_circles, lang_polys_unioned) {
  
  circle_intersections <- readr::read_rds("data/circle_intersections.rds")
  
  circle_intersections_100000 <- lang_circles %>%
    sf::st_join(lang_polys_unioned %>%
                  rmapshaper::ms_simplify(keep_shapes = TRUE) %>%
                  sf::st_make_valid())
  
  circ_stats <- pbapply::pblapply(unique(circle_intersections$LANG_ISO.x),
                    function(x) list(n_langs = 
                                       length(circle_intersections$LANG_ISO.y[circle_intersections$LANG_ISO.x == x]),
                                     area_all = sf::st_area(sf::st_combine(circle_intersections$x[circle_intersections$LANG_ISO.x == x])),
                                     area_lang = sf::st_area(circle_intersections$x[circle_intersections$LANG_ISO.y == x])))
  
  lang_counts <- circle_intersections_10000 %>%
    dplyr::as_tibble() %>%
    dplyr::select(-x) %>%
    dplyr::group_by(LANG_ISO.x) %>%
    dplyr::summarise(count = n())
  
  areas <- circle_intersections %>%
    sf::st_area()
  
}

make_new_language_diversity <- function(lang_circle_intersections,
                                        lang_endangerment_by_ISO_AES_EGIDS) {
  
  lang_circle_div <- lang_circle_intersections %>%
    dplyr::ungroup() %>%
    dplyr::filter(LANG_ISO.x != LANG_ISO.y) %>%
    dplyr::left_join(lang_endangerment_by_ISO_AES_EGIDS,
                     by = c("LANG_ISO.y" = "LANG_ISO")) %>%
    dplyr::group_by(LANG_ISO.x) %>%
    dplyr::summarise(language_richness = dplyr::n_distinct(LANG_ISO.y),
                     threatened_languages = sum(as.numeric(EGIDS) > 6, na.rm = TRUE),
                     language_sw = asbio::SW.index(L1_POP)) %>%
    dplyr::mutate(prop_threatened_languages = threatened_languages / language_richness,
                  language_evenness = language_sw / log(language_richness)) %>%
    dplyr::mutate(language_evenness = ifelse(is.na(language_evenness), 0, language_evenness),
                  prop_threatened_languages = ifelse(is.na(prop_threatened_languages),
                                                     0,
                                                     prop_threatened_languages)) %>%
    dplyr::rename(LANG_ISO = LANG_ISO.x) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry)
  
  lang_circle_div
  
  
}

sum_w_NA <- function(x) if (all(is.na(x))) NA else sum(x, na.rm = TRUE)

build_interactive_vis <- function(human_footprint_rasters,
                                  base_map,
                                  lang_polys_merged) {
  
  
  # vel <- velox::velox(human_footprint_rasters[2])
  # vel$aggregate(5, "mean")
  # 
  # hfp <- vel$as.RasterLayer()
  # hfp_df <- as.data.frame(hfp, xy = TRUE, na.rm = TRUE)
  # 
  # plot_ly(hfp_df, x = ~x, y = ~y, z = ~layer.1) %>%
  #   add_rasterly_heatmap()
  # 
  # base_map_moll <- base_map %>%
  #   sf::st_transform(54009)
  # 
  # map <- ggplot(base_map_moll) +
  #   geom_sf(aes(fill = name), 
  #           size = 0.1,
  #           alpha.fill = 0.25,
  #           colour = "black") +
  #   theme_minimal() +
  #   theme(legend.position = "none")
  # 
  # p <- plotly::ggplotly(map)
  # 
  # p
  # p %>% add_rasterly_heatmap(data = hfp_df, x = ~x, y = ~y,
  #                            z = ~layer.1, inherit = FALSE)
  
}

build_interactive_vis <- function(final_data,
                                  base_map,
                                  lang_polys_unioned) {
  
  
  plotly::toWebGL(plotly::plot_ly(lang_polys_robin, 
                  split = ~LANG_ISO, 
                  color = ~LMP_POP1, 
                  text = ~paste0(LANG_ISO, ":"),
                  hoveron = "fills",
                  hoverinfo = "text",
                  showlegend = FALSE))
  
  lang_polys_simp <- lang_polys %>%
    rmapshaper::ms_simplify(keep_shapes = TRUE,
                            explode = FALSE)
  
  plotly::plot_mapbox(lang_polys_simp,
                      color = ~LANG_ISO)

  plotly::plot_ly() %>%
    plotly::add_sf(data = lang_polys)
  
  base_map_robin <- base_map %>%
    sf::st_transform(54009)
  
  lang_polys_robin <- lang_polys %>%
    sf::st_transform(54009)
  
  lang_polys_w_data <- lang_polys_unioned_robin %>%
    dplyr::left_join(final_data) %>%
    sf::st_make_valid()
  
  # sf::st_write(lang_polys_w_data, "data/final_data_netCDF", driver = "netCDF")
  # 
  # lang_polys_w_data <- sf::st_read("data/final_data_netCDF", driver = "netCDF")
  
  p <- plotly::plot_ly(lang_polys_w_data, 
                       split = ~LANG_ISO, 
                       color = ~EGIDS, 
                       text = ~paste0(LANG_ISO, ":"),
                       hoveron = "fills",
                       hoverinfo = "text",
                       showlegend = FALSE) %>%
    plotly::add_sf() %>%
    plotly::add_sf(data = base_map, color = I("transparent"), 
                   stroke = I("black"), 
                   span = I(1),
                   inherit = FALSE) %>%
    layout(
      title = "Drop down menus - Variable to map",
      updatemenus = list(
        list(
          y = 0.7,
          buttons = list(
            list(method = "restyle",
                 args = list("color", list(lang_polys_w_data$L1_POP)),  # put it in a list
                 label = "L1_POP"),
            list(method = "restyle",
                 args = list("color", list(lang_polys_w_data$GDP_2010USD_10yr_median)),  # put it in a list
                 label = "GDP_2010USD_10yr_median")))
      ))
  
  p
  
  p <- plot_ly(df, x = ~x, y = ~y, mode = "markers", name = "A", visible = T) %>%
    layout(
      title = "Drop down menus - Styling",
      xaxis = list(domain = c(0.1, 1)),
      yaxis = list(title = "y"),
      updatemenus = list(
        list(
          y = 0.7,
          buttons = list(
            list(method = "restyle",
                 args = list("y", list(df$y)),  # put it in a list
                 label = "Show A"),
            list(method = "restyle",
                 args = list("y", list(df$z)),  # put it in a list
                 label = "Show B")))
      ))
  p
  
}

check_islands <- function(base_map_exploded_cea, island_langs,
                          lang_polys_cea) {
  island_issues <- c("fij", "tmg", "amq",
                     "tft", "tvo", "mcm",
                     "fln", "spb", "abz",
                     "abs", "mph", "fil")
  
  island_polys <- lang_polys_cea %>%
    dplyr::filter(LANG_ISO %in% island_issues)
  
  ggplot(base_map_exploded_cea) +
    geom_sf(size = 0.1) +
    geom_sf(data = island_polys, fill = "red", colour = "darkred") +
    theme_minimal()
  
}

assign_islands <- function(final_data_polys_cea, base_map_exploded_cea) {
  
  
  initial_assignments <- final_data_polys_cea %>%
    sf::st_join(base_map_exploded_cea)
  
  unassigned <- initial_assignments %>%
    dplyr::filter(is.na(landmass_id)) %>%
    dplyr::select(-landmass_id) %>%
    .[1:nrow(.), ] %>%
    sf::st_join(base_map_exploded_cea, join = sf::st_nearest_feature)
  
  all_assignments <- unassigned %>%
    rbind(initial_assignments %>%
            dplyr::filter(!is.na(landmass_id)))
  
  landmass_area <- base_map_exploded_cea %>%
    dplyr::filter(landmass_id %in% all_assignments$landmass_id) %>%
    dplyr::mutate(area = sf::st_area(geometry) %>%
                    as.numeric) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry) %>%
    dplyr::mutate(area = area / 1000000)
  
  dist_summary <- unassigned %>%
    dplyr::mutate(distance = sf::st_distance(unassigned, unassigned %>%
                                    dplyr::as_tibble() %>%
                                    dplyr::select(-geometry) %>%
                                    dplyr::left_join(base_map_exploded_cea) %>%
                                    sf::st_sf(),
                                  by_element = TRUE) %>%
                    as.numeric()) %>%
    dplyr::mutate(distance = distance / 1000) %>%
    dplyr::as_tibble() %>%
    dplyr::select(LANG_ISO, distance)
  
  island_summ <- all_assignments %>%
    dplyr::left_join(landmass_area) %>%
    dplyr::group_by(LANG_ISO) %>%
    dplyr::summarise(count = n(), landmasses = list(landmass_id), areas = list(area),
                     mean_area = mean(area, na.rm = TRUE), 
                     sd_area = sd(area, na.rm = TRUE),
                     min_area = min(area, na.rm = TRUE),
                     max_area = max(area, na.rm = TRUE)) %>%
    dplyr::left_join(dist_summary) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry)
  
  island_summ
  
  
  
}

get_continents <- function(final_data_polys_cea, base_map_w_antarctica) {
  maps_cont <- final_data_polys_cea %>%
    dplyr::select(LANG_ISO) 
  
  cont_map <- base_map_w_antarctica %>%
    sf::st_transform(sf::st_crs(final_data_polys_cea)) %>%
    sf::st_buffer(5000) %>%
    dplyr::select(continent, subregion)
  
  cont_dat <- maps_cont %>% 
    sf::st_join(cont_map) 
  
  cont_dat <- cont_dat %>%
    ## fix ainu hokkaido issue (error 5 of 5)
    dplyr::filter(!(LANG_ISO == "ain" & continent == "Europe")) %>%
    dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
    dplyr::group_by(LANG_ISO) %>%
    dplyr::summarise(continent = continent[which.max(area)],
                     subregion = subregion[which.max(area)])
  
  
  unassigned <- cont_dat %>%
    dplyr::filter(is.na(continent)) %>%
    dplyr::select(-continent, -subregion) %>%
    .[1:nrow(.),] %>%
    sf::st_join(cont_map[1:nrow(cont_map), ], join = sf::st_nearest_feature) 

  
  cont_dat <- cont_dat %>%
    dplyr::filter(!is.na(continent)) %>%
    rbind(unassigned) %>%
    dplyr::arrange(LANG_ISO) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry) 
    
  
  cont_dat
  
  
}

#df_to_add_to <- lang_polys
add_macrolanguages <- function(df_to_add_to, macro_languages) {
  df_to_add_to <- df_to_add_to %>%
    dplyr::left_join(macro_languages %>%
                       dplyr::select(iso6393_individual,
                                     iso6393_macrolanuage,
                                     macrolanguage),
                     by = c("LANG_ISO" = "iso6393_individual")) %>%
    dplyr::mutate(macrolanguage = ifelse(is.na(macrolanguage), "none", macrolanguage)) %>%
    dplyr::rename(LANG_ISO_micro = LANG_ISO) %>%
    dplyr::mutate(LANG_ISO = ifelse(macrolanguage != "none", iso6393_macrolanuage, LANG_ISO_micro))
  df_to_add_to
}

#over-ride above function so as to remove the macrolanguage ISO replacement
add_macrolanguages <- function(df_to_add_to, macro_languages) {
  df_to_add_to <- df_to_add_to %>%
    dplyr::left_join(macro_languages %>%
                       dplyr::select(iso6393_individual,
                                     iso6393_macrolanuage,
                                     macrolanguage),
                     by = c("LANG_ISO" = "iso6393_individual")) %>%
    dplyr::mutate(macrolanguage = ifelse(is.na(macrolanguage), "none", macrolanguage)) %>%
    #dplyr::rename(LANG_ISO_micro = LANG_ISO) %>%
    #dplyr::mutate(LANG_ISO = ifelse(macrolanguage != "none", iso6393_macrolanuage, LANG_ISO_micro))
    dplyr::mutate(LANG_ISO_micro = LANG_ISO)
  df_to_add_to
}

make_variable_table <- function(final_data_w_info, transformation_table, var_list) {
  name_changes <- c(id_ISO_lang                                       = "LANG_ISO",
                    id_name_lang                                      = "name",
                    lang_continent_lang                               = "continent",
                    lang_subregion_lang                               = "subregion",
                    lang_island_lang                                  = "island",
                    response_AES_lang                                 = "AES",
                    response_EGIDS_lang                               = "EGIDS",
                    soceco_GDP.2010USD.10yr.median_country            = "GDP_2010USD_10yr_median",
                    lang_L1.POP_lang                                  = "L1_POP",
                    soceco_Gini.SWIID.10yr.median_country             = "Gini_SWIID_10yr_median",
                    edu_Ed.spending.pcGDP.10yr.median_country         = "Ed_spending_pcGDP_10yr_median",
                    edu_Mean.yr.school.10yr.median_country            = "Mean_yr_school_10yr_median",
                    soceco_Life.exp.birth.10yr.median_country         = "Life_exp_birth_10yr_median",
                    soceco_Life.exp.sixty.average_country             = "Life_exp_sixty_average",
                    landuse_urban.change_country                      = "urban_change",
                    worldlang_world.language.as.official_country      = "world_language_as_official",
                    edu_minority.education.policy_country             = "minority_education_policy",
                    worldlang_Arabic_country                          = "world_language_Arabic",
                    worldlang_Bahasa_country                          = "world_language_Bahasa",
                    worldlang_English_country                         = "world_language_English",
                    worldlang_French_country                          = "world_language_French",
                    worldlang_Hindustani_country                      = "world_language_Hindustani",
                    worldlang_Mandarin_country                        = "world_language_Mandarin",
                    worldlang_Portuguese_country                      = "world_language_Portuguese",
                    worldlang_Russian_country                         = "world_language_Russian",
                    worldlang_Spanish_country                         = "world_language_Spanish",
                    edu_used.in.education_lang                        = "used_in_education",
                    lang_type_lang                                    = "type",
                    lang_official.in.any.country_lang                 = "official_in_any_country",
                    landuse_hfp_polygon                               = "hfp",
                    landuse_built_polygon                             = "built",
                    landuse_croplands_polygon                         = "croplands",
                    landuse_pasture_polygon                           = "pasture",
                    landuse_pop.density_polygon                       = "pop_density",
                    shift_hfp.change_polygon                          = "hfp_change",
                    shift_built.change_polygon                        = "built_change",
                    shift_croplands.change_polygon                    = "croplands_change",
                    shift_pasture.change_polygon                      = "pasture_change",
                    shift_pop.density.change_polygon                  = "pop_density_change",
                    enviro_mgs_polygon                                = "mgs_new",
                    enviro_temperature.seasonality_polygon            = "temperature_seasonality",
                    enviro_precipitation.seasonality_polygon          = "precipitation_seasonality",
                    div_language.richness_nb                          = "language_richness",
                    div_threatened.languages_nb                       = "threatened_languages",
                    div_prop.threatened.languages_nb                  = "prop_threatened_languages",
                    div_language.evenness_nb                          = "language_evenness",
                    conn_nav.water_nb                                 = "nav_water_nb",
                    landuse_pop.density_nb                            = "pop_density_nb",
                    conn_roads_nb                                     = "roads_nb",
                    conn_altitude.range_nb                            = "altitude_range",
                    conn_roughness_nb                                 = "roughness_nb",
                    conn_river_nb                                     = "river_nb",
                    div_bordering.language.richness_lang              = "bordering_language_richness",
                    div_bordering.language.richness.per.km.perim_lang = "bordering_language_richness_per_km_perim",
                    div_bordering.language.evenness_lang              = "bordering_language_evenness",
                    biodiv_species.endangered_polygon                 = "species_endangered",
                    biodiv_species.prop.endangered_polygon            = "species_prop_endangered",
                    enviro_mean.annual.temperature.kelvin_polygon     = "mean_annual_temperature_kelvin",
                    div_L1pop.over.nb.popdens_nb                      = "L1pop_over_nb_popdens",
                    lang_polygon.area_lang                            = "polygon_area",
                    lang_documentation_lang                           = "documentation") %>%
    as.data.frame() %>%
    dplyr::as_tibble(rownames = "new_name")
  
  colnames(name_changes) <- c("new_name", "old_name")

  vars <- colnames(final_data_w_info %>%
                     dplyr::select(-dplyr::ends_with("_tr")))  
  
  var_tab <- dplyr::tibble(new_name = vars) %>%
    dplyr::mutate(category = purrr::map_chr(strsplit(new_name, "_"), ~.x[1])) %>%
    dplyr::mutate(collection_type = purrr::map_chr(strsplit(new_name, "_"), ~.x[length(.x)])) %>%
    dplyr::mutate(variable_name = stringr::str_match(new_name, "_(.*)_")[,2]) %>%
    dplyr::left_join(name_changes) %>%
    dplyr::left_join(transformation_table %>%
                       dplyr::select(old_name = column_name,
                                     transformation_name)) %>%
    dplyr::mutate(transformation_name = ifelse(is.na(transformation_name), "identity", transformation_name)) %>%
    dplyr::mutate(category = dplyr::case_when(category == "lang" ~ "Language",
                                              category == "soceco" ~ "Socioeconomic",
                                              category == "edu" ~ "Education",
                                              category == "landuse" ~ "Land Use",
                                              category == "worldlang" ~ "World Language",
                                              category == "shift" ~ "Shift",
                                              category == "enviro" ~ "Environment",
                                              category == "pop" ~ "Land Use",
                                              category == "div" ~ "Diversity",
                                              category == "conn" ~ "Connectivity",
                                              category == "biodiv" ~ "Biodiversity Loss",
                                              category == "id" ~ "Identifier",
                                              category == "response" ~ "Response")) %>%
    dplyr::mutate(collection_type = dplyr::case_when(collection_type == "lang" ~ "Language Based",
                                              collection_type == "country" ~ "Polygon-aggregated Nation Based",
                                              collection_type == "polygon" ~ "Polygon-aggregated Raster Based",
                                              collection_type == "nb" ~ "Neighbourhood-aggregated Raster Based")) %>%
    dplyr::left_join(var_list, by = c("old_name" = "old_variable_name"))
  
  var_tab
  
}

# pdf_file <- "figures/all_maps.pdf"
make_map_pdf <- function(variable_maps, pdf_file) {
  pdf(pdf_file, width = 24, height = 12)
  purrr::iwalk(variable_maps,
              ~plot(.x + ggtitle(.y)))
  dev.off()
}


make_regions_map <- function(base_map_w_antarctica,
                 final_data_polys_w_data_cea) {
  
  convert_df <- dplyr::tibble(subregion = c("Polynesia",
                                             "Micronesia",
                                             "Melanesia",
                                             "Caribbean",
                                             "Southern Europe",
                                             "Eastern Europe",
                                             "Western Europe",
                                             "Northern Europe",
                                             "Western Asia",
                                             "Northern Africa",
                                             "Central Asia",
                                             "Eastern Asia",
                                             "Southern Africa",
                                             "Eastern Africa",
                                             "Middle Africa"),
                              new_region = c("Oceania",
                                             "Oceania",
                                             "Oceania",
                                             "Central America",
                                             "Europe",
                                             "Europe",
                                             "Europe",
                                             "Europe",
                                             "Arab",
                                             "Arab",
                                             "Asia",
                                             "Asia",
                                             "Africa",
                                             "Africa",
                                             "Africa"))
  
  final_data_polys_w_data_cea <- final_data_polys_w_data_cea %>%
    dplyr::left_join(convert_df,
                     by = c("lang_subregion_lang" = "subregion")) %>%
    dplyr::mutate(new_region = ifelse(is.na(new_region),
                                      lang_subregion_lang,
                                      new_region))
  
  base_map_w_antarctica <- base_map_w_antarctica %>%
    dplyr::left_join(convert_df) %>%
    dplyr::mutate(new_region = ifelse(is.na(new_region),
                                      subregion,
                                      new_region)) %>%
    dplyr::filter(new_region %in% unique(final_data_polys_w_data_cea$new_region)) %>%
    dplyr::group_by(new_region) %>%
    dplyr::summarise(count = dplyr::n())
  
  ggplot(base_map_w_antarctica %>%
           sf::st_transform('ESRI:54030') %>%
           dplyr::select(new_region)) + 
    geom_sf(aes(fill = new_region, colour = new_region), size = 1.5) +
    geom_sf(aes(fill = new_region, colour = new_region), size = 2,
            data = base_map_w_antarctica[base_map_w_antarctica$new_region == "Seven seas (open ocean)", ]) +
    geom_sf(inherit.aes = FALSE, fill = NA) +
    scale_fill_discrete(name = "Subregion") +
    scale_colour_discrete(name = "Subregion") +
    theme_minimal()
  
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param var_tab
make_transform_supp_table <- function(var_tab) {
  
  var_tr_tab <- var_tab %>%
    select(`Variable Name` = variable_name, 
           `Transformation Applied` = transformation_name)
  
  write_csv(var_tr_tab, "doc/Manuscript/first_draft/Var_tr_tab.csv")
  
}

