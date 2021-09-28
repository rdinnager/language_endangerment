source("R/packages.R")
source("R/functions.R")
hmr_proj4string <- proj4string(raster(human_footprint_rasters[1]))
lang_pts <- sf::st_read(file_in("data/e17/GMI_geodata/lang/lang_all_pt.shp"))
base_map_w_antarctica <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf") %>%
  dplyr::mutate(iso_a3 = ifelse(admin == "France", "FRA", iso_a3)) %>%
  dplyr::mutate(iso_a3 = ifelse(admin == "Norway", "NOR", iso_a3)) %>%
  dplyr::mutate(iso_a3 = ifelse(admin == "Kosovo", "XKX", iso_a3))
hua_data_480 <- read_rds(file_in("data/Hua_et_al_data_480.rds"))
jager_tree_constr <- ape::read.tree(file_in("data/trees/RAxML_bestTree.world_sc_ccGlot"))
lang_pts_e16 <- sf::st_read(file_in("data/lang_pt/lang_all_pt.shp"))
education_policy <- readr::read_csv(file_in("data/education/MinorityLanguagePolicy_Final.csv"))
data_meta <- tibble::tribble(
  ~Variable.Description, ~Dataset.Column.Name,
  ~Data.Type, ~Suggested.Transformation, ~Map.Link, ~Distribution.Link,
  ~Comments, "Language ISO Code", "LANG_ISO", "Unordered Categorical",
  "None", "N / A", "N / A", "This is the code used to identify what constitutes individual languages in our analysis. This code is technically called ISO-639-3.",
  "Language Endangerment: AES", "AES", "Ordinal", "None", "#AES_map",
  "#AES_dist", "One of the two endangerment indexes that will be our response variables."
)
IUCN_amphibians <- sf::st_read(file_in("data/IUCN/amphibians/AMPHIBIANS.shp"))
lang_polys_orig <- sf::st_read(file_in("data/e17/GMI_geodata/lang/langa.shp"))
hua_data_2000 <- read_rds(file_in("data/Hua_et_al_data_2000.rds"))
latent_preds <- purrr::map(list.files("data/latent", full.names = TRUE), ~ readr::read_csv(.x)) %>%
  purrr::set_names(list.files("data/latent"))
latent_preds2 <- purrr::map(
  list.files("data/latent2_w_continent/", full.names = TRUE),
  ~ readr::read_csv(.x)
) %>% purrr::set_names(list.files("data/latent2_w_continent/"))
jager_names <- read_tsv(file_in("data/trees/dataset.tab"))
new_pop <- readr::read_csv(file_in("data/hedvig_output/POP_manual.csv"))
world_languages_initial <- readr::read_csv(file_in("data/world_languages/WorldLanguagebyCountry.280420.csv"))
macro_languages <- readr::read_csv(file_in("data/hedvig_output/iso6393_macrolanguage_mappings.csv")) %>%
  dplyr::mutate(macrolanguage = "official") %>%
  dplyr::bind_rows(readr::read_csv(file_in("data/hedvig_output/iso6393_macrolanguage_mappings_extra_non_official.csv")) %>%
    dplyr::mutate(macrolanguage = "non-official"))
var_list <- readr::read_csv(file_in("data/final_data/lang_endang_variable_list.csv"))
wlms_dat <- foreign::read.dbf(file_in("data/e17/GMI_geodata/lang/eth_wlms.dbf"))
soceco_dat <- readr::read_csv(file_in("data/SocEco/LangEndang_SoEcData_v4.csv"))
glottolog_trees <- clean_glottolog_tree(file_in("data/trees/glottolog_4_tree_newick.txt"))
hua_data_12600 <- read_rds(file_in("data/Hua_et_al_data_12600.rds"))
IUCN_mammals <- sf::st_read(file_in("data/IUCN/mammals/MAMMALS.shp"))
base_map_states <- rnaturalearth::ne_states(returnclass = "sf") %>% sf::st_make_valid()
lang_bnd <- sf::st_read(file_in("data/e17/GMI_geodata/bnd/DCW_countries.shp"))
jager_tree_unconstr <- ape::read.tree(file_in("data/trees/RAxML_bestTree.world_sc_cc"))
dates <- readr::read_csv(file_in("data/dates/dates.csv"))
base_map <- base_map_w_antarctica %>%
  filter(geounit != "Antarctica") %>%
  sf::st_make_valid()
hua_language_div_480 <- hua_data_480 %>% dplyr::mutate(language_diversity = ifelse(land_coverage ==
  0, NA, language_diversity))
extinct_e16 <- lang_pts_e16 %>% dplyr::filter(LMP_CLASS == "X")
hua_language_div_2000 <- hua_data_2000 %>% dplyr::mutate(language_diversity = ifelse(land_coverage ==
  0, NA, language_diversity))
jager_name_legend <- jager_names %>%
  dplyr::select(iso, wls_fam, wls_gen, names) %>%
  dplyr::mutate(full_name = paste(wls_fam, wls_gen, names,
    sep = "."
  )) %>%
  dplyr::mutate(full_name = gsub("-", "_",
    full_name,
    fixed = TRUE
  ))
glotto_main <- readr::read_tsv(file_in("data/tree_lgs_df_glottolog_matchup.tsv"),
  col_types = cols(
    Name = col_character(), Name_stripped_no_spaces = col_character(),
    iso639_3 = col_character(), iso639_3_macro = col_character(),
    glottocode = col_character(), Family_name_glottolog = col_character(),
    Classification_ethnologue = col_character(), Population_numeric_ethnologue = col_double(),
    endangerment_status_glottolog = col_character(), desc_status_glottolog = col_character(),
    Language_status_ethnologue = col_character(), level = col_character(),
    AUTOTYP_area = col_character(), Longitude = col_double(),
    Latitude = col_double()
  )
) %>%
  dplyr::rename(LANG_ISO = iso639_3) %>%
  add_macrolanguages(macro_languages)
official_languages <- readr::read_csv(file_in("data/world_languages/official_languages.csv")) %>%
  na.omit() %>%
  dplyr::mutate(official_in_any_country = 1) %>%
  dplyr::select(LANG_ISO = ISO, official_in_any_country) %>%
  add_macrolanguages(macro_languages)
glottolog_all <- readr::read_tsv(file_in("data/documentation/Glottolog_4_cldf.tsv")) %>%
  dplyr::rename(LANG_ISO = ISO639P3code) %>%
  add_macrolanguages(macro_languages)
LEI_data <- read_csv(file_in("data/endangerment/LEI_database_file.csv"),
  col_names = c(
    "some_num_1", "LANG_ISO", "some_name_1", "some_name_2",
    "LEI", "some_num_2", "some_name_3", "some_name_4", "some_notes_1",
    "some_notes_2", "some_place_1", "some_place_2", "coords"
  )
) %>%
  tidyr::separate(LEI, c("LEI", "LEI_Certainty"), " \\(") %>%
  tidyr::unnest(LANG_ISO = strsplit(LANG_ISO, ",")) %>%
  dplyr::mutate(LANG_ISO = str_trim(LANG_ISO)) %>%
  dplyr::mutate(LANG_ISO = str_sub(LANG_ISO, 1, 3)) %>%
  add_macrolanguages(macro_languages)
UNESCO_data <- read_csv(file_in("data/endangerment/unesco_atlas_languages_limited_dataset.csv")) %>%
  tidyr::unnest(LANG_ISO = strsplit(`ISO639-3 codes`, ",")) %>%
  dplyr::mutate(LANG_ISO = str_trim(LANG_ISO)) %>%
  add_macrolanguages(macro_languages)
used_in_ed <- readr::read_csv(file_in("data/education/EducLangsFinal.txt"),
  col_names = c("LANG_ISO")
) %>%
  dplyr::mutate(used_in_education = 1) %>%
  add_macrolanguages(macro_languages)
glottolog_extra <- readr::read_tsv(file_in("data/documentation/Glottolog_4_cldf.tsv")) %>%
  dplyr::distinct(ISO639P3code, .keep_all = TRUE) %>%
  dplyr::rename(LANG_ISO = ISO639P3code) %>%
  add_macrolanguages(macro_languages)
glotto_contact <- readr::read_tsv(file_in("data/Contact_lgs_from_tree.tsv"), col_types = cols(
  Name = col_character(),
  Name_stripped_no_spaces = col_character(), iso639_3 = col_character(),
  iso639_3_macro = col_character(), glottocode = col_character(),
  Family_name_glottolog = col_character(), Classification_ethnologue = col_character(),
  Population_numeric_ethnologue = col_double(), endangerment_status_glottolog = col_character(),
  desc_status_glottolog = col_character(), Language_status_ethnologue = col_character(),
  level = col_character(), AUTOTYP_area = col_character(),
  Longitude = col_double(), Latitude = col_double()
)) %>%
  dplyr::rename(LANG_ISO = iso639_3) %>%
  add_macrolanguages(macro_languages)
glotto_bookkeeping <- readr::read_tsv(file_in("data/bookkeeping_unattested_etc.tsv"),
  col_types = cols(
    Name = col_character(), Name_stripped_no_spaces = col_character(),
    iso639_3 = col_character(), iso639_3_macro = col_character(),
    glottocode = col_character(), Family_name_glottolog = col_character(),
    Classification_ethnologue = col_character(), Population_numeric_ethnologue = col_double(),
    endangerment_status_glottolog = col_character(), desc_status_glottolog = col_character(),
    Language_status_ethnologue = col_character(), level = col_character(),
    AUTOTYP_area = col_character(), Longitude = col_double(),
    Latitude = col_double()
  )
) %>%
  dplyr::rename(LANG_ISO = iso639_3) %>%
  add_macrolanguages(macro_languages)
lang_pts_widespread <- lang_pts %>%
  dplyr::filter(LMP_CLASS == "W") %>%
  sf::st_transform(crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) %>%
  dplyr::left_join(wlms_dat %>% dplyr::select(ID, HUB)) %>%
  dplyr::filter(as.character(LMP_C1) == as.character(HUB)) %>%
  dplyr::select(-HUB)
which_HUB_removed <- lang_pts %>%
  dplyr::filter(LMP_CLASS == "W") %>%
  sf::st_transform(crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) %>%
  dplyr::left_join(wlms_dat %>% dplyr::select(ID, HUB)) %>%
  dplyr::mutate(removed_HUB = as.character(LMP_C1) != as.character(HUB)) %>%
  dplyr::filter(removed_HUB)
num_HUB_removed <- lang_pts %>%
  dplyr::filter(LMP_CLASS == "W") %>%
  sf::st_transform(crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) %>%
  dplyr::left_join(wlms_dat %>% dplyr::select(ID, HUB)) %>%
  dplyr::mutate(removed_HUB = as.character(LMP_C1) != as.character(HUB)) %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(num_removed = sum(removed_HUB))
hua_raster_high_res <- make_hua_raster_high_res(hua_data_12600)
hua_language_div_12600 <- hua_data_12600 %>% dplyr::mutate(language_diversity = ifelse(land_coverage ==
  0, NA, language_diversity))
base_map_crs <- sf::st_crs(base_map)
world_language_iso <- world_languages_initial %>%
  dplyr::filter(is.na(iso3c) | !iso3c %in%
    unique(base_map$iso_a3)) %>%
  dplyr::mutate(territory = ifelse(territory %in%
    names(problematic_territory_names), problematic_territory_names[territory],
  territory
  )) %>%
  dplyr::left_join(base_map %>% dplyr::select(
    territory = admin,
    iso_a3 = iso_a3
  )) %>%
  dplyr::select(territory, iso_a3)
hua_data_480_language_diversity <- raster::rasterFromXYZ(hua_language_div_480[, c(
  "longitudes",
  "latitudes", "language_diversity"
)], crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) %>%
  raster::writeRaster(file_out("data/cea_rasters/language_diversity_raster_1000KM2.grd"),
    overwrite = TRUE
  )
lang_pts_other <- lang_pts %>%
  dplyr::select(c(
    "ID", "ID_ISO_A3", "ID_ISO_A2",
    "ID_FIPS", "NAM_LABEL", "NAME_PROP", "NAME2", "NAM_ANSI", "CNT",
    "C1", "LMP_POP1", "G", "LMP_CLASS", "LMP_C1", "LANG_ISO", "geometry"
  )) %>%
  dplyr::filter(!LMP_CLASS %in% c("U", "W")) %>%
  rbind(extinct_e16 %>%
    dplyr::select(c(
      "ID", "ID_ISO_A3", "ID_ISO_A2", "ID_FIPS",
      "NAM_LABEL", "NAME_PROP", "NAME2", "NAM_ANSI", "CNT", "C1",
      "LMP_POP1", "G", "LMP_CLASS", "LMP_C1", "LANG_ISO", "geometry"
    )))
extinct_e16_data <- extinct_e16 %>%
  dplyr::as_tibble() %>%
  dplyr::select(
    LANG_ISO,
    NAM_LABEL, LMP_POP1
  ) %>%
  readr::write_csv(file_out("data/extinct_e16_data.csv"))
extinct_e16_map <- mapview::mapview(extinct_e16,
  col.regions = "white", zcol = c("LANG_ISO"),
  legend = FALSE
)
hua_data_2000_language_diversity <- raster::rasterFromXYZ(hua_language_div_2000[, c(
  "longitudes",
  "latitudes", "language_diversity"
)], crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) %>%
  raster::writeRaster(file_out("data/cea_rasters/language_diversity_raster_500KM2.grd"),
    overwrite = TRUE
  )
lang_names <- glotto_main %>%
  dplyr::select(LANG_ISO, Name) %>%
  add_macrolanguages(macro_languages)
glottolog_max_class <- glottolog_extra %>%
  dplyr::mutate(class_split = strsplit(classification,
    "/",
    fixed = TRUE
  ), n_class = sapply(class_split, length)) %>%
  dplyr::summarise_at(vars(n_class), ~ max(., na.rm = TRUE))
glotto_full <- bind_rows(glotto_main, glotto_bookkeeping, glotto_contact)
hua_proj4string <- proj4string(hua_raster_high_res)
hua_data_12600_language_diversity <- raster::rasterFromXYZ(hua_language_div_12600[, c(
  "longitudes",
  "latitudes", "language_diversity"
)], crs = sp::CRS("+proj=cea +datum=WGS84 +units=m")) %>%
  raster::writeRaster(file_out("data/cea_rasters/language_diversity_raster_200KM2.grd"),
    overwrite = TRUE
  )
base_map_unioned <- base_map %>%
  sf::st_union() %>%
  smoothr::fill_holes(units::set_units(
    1000,
    km^2
  )) %>%
  sf::st_set_crs(base_map_crs)
world_languages <- readr::read_csv(file_in("data/world_languages/WorldLanguagebyCountry.280420.csv")) %>%
  dplyr::mutate(territory = ifelse(territory %in% names(problematic_territory_names),
    problematic_territory_names[territory], territory
  )) %>%
  dplyr::left_join(world_language_iso) %>%
  dplyr::mutate(iso3c = ifelse(is.na(iso3c),
    iso_a3, iso3c
  )) %>%
  dplyr::select(-iso_a3) %>%
  dplyr::select(iso3c,
    world_language = `World Language`
  ) %>%
  dplyr::mutate(world_language = strsplit(
    world_language,
    ", "
  )) %>%
  tidyr::unnest(cols = c(world_language)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(iso3c, world_language) %>%
  dplyr::mutate(world_language = ifelse(world_language ==
    "German", "n", world_language) %>% as.factor()) %>%
  dplyr::mutate(world_language_as_official = ifelse(world_language ==
    "n", 0, 1)) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(
    world_language_as_official = world_language_as_official[1],
    world_language = list(unique(world_language))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(world_language = purrr::map(
    world_language,
    ~ {
      nam <- levels(.x)
      bin <- tabulate(as.numeric(.x), nbins = nlevels(.x))
      names(bin) <- nam
      bin <- bin[-which(names(bin) == "n")]
      bin
    }
  )) %>%
  na.omit()
new_language_grid <- sf::st_as_sf(raster::rasterToPolygons(hua_data_480_language_diversity,
  na.rm = FALSE
))
glottolog_class <- glottolog_extra %>%
  tidyr::separate(classification, paste0(
    "level_",
    1:glottolog_max_class$n_class
  ), "/") %>%
  dplyr::select(dplyr::starts_with("level"),
    level_18 = LANG_ISO, -level
  )
IUCN_grid <- hua_data_12600_language_diversity %>%
  raster::disaggregate(2L) %>%
  raster::rasterToPolygons(na.rm = FALSE) %>%
  sf::st_as_sf()
IUCN_grid_raster <- hua_data_12600_language_diversity %>%
  raster::disaggregate(2L) %>%
  raster::writeRaster(file_out("data/IUCN/template_raster.grd"),
    overwrite = TRUE
  )
lang_polys_voronoi <- make_lang_pts_voronoi(lang_pts_other, base_map_unioned)
map_hex_robin <- base_map_unioned %>%
  sf::st_transform("ESRI:54030") %>%
  sf::st_make_grid(n = c(
    154,
    72
  ), square = FALSE)
base_map_states_cea <- base_map_states %>%
  sf::st_transform(sf::st_crs(new_language_grid)) %>%
  sf::st_make_valid()
base_map_cea <- base_map %>%
  sf::st_transform(sf::st_crs(new_language_grid)) %>%
  sf::st_make_valid()
class_df <- glottolog_class %>%
  as.data.frame() %>%
  apply(1, function(x) {
    x[is.na(x)] <- x["level_18"]
    x
  }) %>%
  t() %>%
  as.data.frame(stringsAsFactors = TRUE) %>%
  na.omit()
IUCN_mammals_VU_higher <- IUCN_mammals %>%
  dplyr::filter(code %in% c(
    "VU", "EN", "CR",
    "EW", "EX"
  ), terrestial == "t") %>%
  sf::st_transform(crs = sf::st_crs(IUCN_grid))
IUCN_amphibians_VU_higher <- IUCN_amphibians %>%
  dplyr::filter(category %in% c(
    "VU", "EN",
    "CR", "EW", "EX"
  )) %>%
  sf::st_transform(crs = sf::st_crs(IUCN_grid))
IUCN_all_polys_cea <- dplyr::bind_rows(IUCN_mammals %>% dplyr::filter(code != "DD") %>%
  sf::st_transform(crs = sf::st_crs(IUCN_grid)), IUCN_amphibians %>%
  dplyr::filter(category != "DD") %>% sf::st_transform(crs = sf::st_crs(IUCN_grid))) %>%
  as.data.frame() %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_set_crs(st_crs(IUCN_grid)$proj4string)
hex_area <- sf::st_area(map_hex_robin)
lang_widespread_ids <- sf::st_intersects(lang_pts_widespread, base_map_states_cea)
countries_w_soceco <- base_map_cea %>%
  dplyr::left_join(soceco_dat %>% dplyr::select(
    iso_a3 = ISO_A3,
    GDP_2010USD_10yr_median, Gini_SWIID_10yr_median, Ed_spending_pcGDP_10yr_median,
    Ed_spending_yr, Mean_yr_school_10yr_median, Life_exp_birth_10yr_median,
    Life_exp_sixty_average, `Urban_pop_pc_change_1960-2017`,
    Minority_Ed_Policy, gdp.pcap.10yrmed, expense.pcGDP.10yrmed,
    ed.sp.prm.pcGDP.10yrmed, ed.sp.sec.pcGDP.10yrmed, ed.sp.tert.pcGDP.10yrmed,
    ed.sp.tot.pcxpd.10yrmed
  ) %>% dplyr::mutate(iso_a3 = ifelse(iso_a3 ==
    "SDS", "SSD", iso_a3)) %>% dplyr::mutate(iso_a3 = ifelse(iso_a3 ==
    "SAH", "ESH", iso_a3)) %>% dplyr::mutate(iso_a3 = ifelse(iso_a3 ==
    "KOS", "XKX", iso_a3))) %>%
  dplyr::left_join(world_languages,
    by = c(iso_a3 = "iso3c")
  ) %>%
  dplyr::left_join(education_policy %>%
    dplyr::select(ISO_A3, minority_education_policy = Minority_Ed_Policy),
  by = c(iso_a3 = "ISO_A3")
  ) %>%
  dplyr::rename(urban_change = `Urban_pop_pc_change_1960-2017`)
base_map_exploded_cea <- base_map_cea %>%
  sf::st_union() %>%
  sf::st_cast("POLYGON") %>%
  sf::st_sf() %>%
  sf::st_make_valid() %>%
  dplyr::mutate(landmass_id = paste0(
    "landmass_",
    1:n()
  ))
tree_glottolog <- as.phylo.formula.better(~ level_1 / level_2 / level_3 / level_4 / level_5 / level_6 / level_7 / level_8 / level_9 / level_10 / level_11 / level_12 / level_13 / level_14 / level_15 / level_16 / level_17 / level_18,
  data = class_df, collapse = TRUE
)
IUCN_mammals_raster_high_res <- fasterize::fasterize(IUCN_mammals_VU_higher, hua_data_12600_language_diversity,
  fun = "sum"
) %>% raster::writeRaster(file_out("data/IUCN/mammals/mammal_count_high_res.grd"),
  overwrite = TRUE
)
IUCN_amphibians_raster_high_res <- fasterize::fasterize(IUCN_amphibians_VU_higher, hua_data_12600_language_diversity,
  fun = "sum"
) %>% raster::writeRaster(file_out("data/IUCN/amphibians/mammal_count_high_res.grd"),
  overwrite = TRUE
)
IUCN_polys_cea <- dplyr::bind_rows(IUCN_mammals_VU_higher, IUCN_amphibians_VU_higher) %>%
  as.data.frame() %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_set_crs(st_crs(IUCN_grid)$proj4string)
IUCN_all_by_grid <- sf::st_intersects(IUCN_grid, IUCN_all_polys_cea %>% dplyr::select(binomial))
lang_widespread_ids_labeled <- lang_widespread_ids %>%
  as.list() %>%
  magrittr::set_names(attr(
    lang_widespread_ids,
    "region.id"
  ))
tree_glottolog_labelled <- label_glottolog_tree(tree_glottolog, glottolog_all)
IUCN_mammals_raster_low_res <- raster::resample(IUCN_mammals_raster_high_res, hua_data_480_language_diversity) %>%
  raster::writeRaster(file_out("data/IUCN/mammals/mammal_count_low_res.grd"),
    overwrite = TRUE
  )
IUCN_amphibians_raster_low_res <- raster::resample(IUCN_amphibians_raster_high_res, hua_data_480_language_diversity) %>%
  raster::writeRaster(file_out("data/IUCN/amphibians/mammal_count_low_res.grd"),
    overwrite = TRUE
  )
IUCN_by_grid <- sf::st_intersects(IUCN_grid, IUCN_polys_cea %>% dplyr::select(binomial))
lang_polys_widespread <- base_map_states[unlist(lang_widespread_ids_labeled), ] %>%
  dplyr::select(geometry) %>%
  dplyr::bind_cols(lang_pts_widespread[as.numeric(names(unlist(lang_widespread_ids_labeled))), ] %>% dplyr::as_tibble() %>% dplyr::select(-geometry))
tree_glottolog_scaled <- scale_tree(tree_glottolog_labelled, dates)
lang_polys <- rbind(lang_polys_orig %>% dplyr::select(c(
  "ID", "ID_ISO_A3",
  "ID_ISO_A2", "ID_FIPS", "NAM_LABEL", "NAME_PROP", "NAME2", "NAM_ANSI",
  "CNT", "C1", "LMP_POP1", "G", "LMP_CLASS", "LMP_C1", "LANG_ISO",
  "geometry"
)), lang_polys_voronoi %>% dplyr::select(c(
  "ID", "ID_ISO_A3",
  "ID_ISO_A2", "ID_FIPS", "NAM_LABEL", "NAME_PROP", "NAME2", "NAM_ANSI",
  "CNT", "C1", "LMP_POP1", "G", "LMP_CLASS", "LMP_C1", "LANG_ISO",
  "geometry"
)), lang_polys_widespread %>% dplyr::select(c(
  "ID",
  "ID_ISO_A3", "ID_ISO_A2", "ID_FIPS", "NAM_LABEL", "NAME_PROP",
  "NAME2", "NAM_ANSI", "CNT", "C1", "LMP_POP1", "G", "LMP_CLASS",
  "LMP_C1", "LANG_ISO", "geometry"
))) %>%
  add_macrolanguages(macro_languages) %>%
  dplyr::left_join(new_pop) %>%
  dplyr::mutate(LMP_POP1 = ifelse(is.na(`POP new`),
    LMP_POP1, `POP new`
  )) %>%
  dplyr::select(-`POP new`) %>%
  dplyr::filter(!ID %in%
    c("CYM-ARG", "GLE-CAN", "vec-BRA", "vec-MEX", "vec-HRV")) %>%
  sf::st_wrap_dateline(options = "WRAPDATELINE=YES") %>%
  sf::st_cast("MULTIPOLYGON")
phy_corr_glottolog <- vcv.phylo(tree_glottolog_scaled, corr = TRUE)
world_langs_macro <- purrr::imap_dfr(world_langs, ~ dplyr::tibble(
  world_language = .y,
  LANG_ISO_micro = .x
)) %>%
  dplyr::left_join(lang_polys %>%
    dplyr::as_tibble() %>% dplyr::select(LANG_ISO_micro, LANG_ISO) %>%
    dplyr::group_by(LANG_ISO_micro) %>% dplyr::summarise(LANG_ISO = LANG_ISO[1])) %>%
  dplyr::pull(LANG_ISO) %>%
  unique() %>%
  na.omit() %>%
  as.vector()
lang_polys_cea <- lang_polys %>%
  sf::st_transform(sf::st_crs(new_language_grid)) %>%
  sf::st_make_valid()
hmr_reprojected_lang_polys <- st_transform(lang_polys, hmr_proj4string) %>% st_cast("MULTIPOLYGON")
lang_ids <- lang_polys %>%
  as.data.frame() %>%
  dplyr::select(
    ID:NAM_ANSI,
    LANG_ISO
  )
lang_polys_merged <- lang_polys %>%
  sf::st_make_valid() %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(n_poly = n(), LMP_POP1 = sum(LMP_POP1)) %>%
  dplyr::ungroup() %>%
  sf::st_cast("MULTIPOLYGON")
language_areas <- sf::st_area(lang_polys)
hua_reprojected_lang_polys <- st_transform(lang_polys, hua_proj4string) %>% st_cast("MULTIPOLYGON")
language_centroids <- lang_polys %>%
  sf::st_transform("ESRI:54030") %>%
  sf::st_centroid() %>%
  sf::st_transform(4326)
lang_polys_w_soceco_ids_initial <- st_intersection(lang_polys_cea, countries_w_soceco)
human_footprint_dat_data.HumanFootprint.Maps.Pasture2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Pasture2009.tif",
  scaling = 4, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.croplands2005.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/croplands2005.tif",
  scaling = 7, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.Popdensity2010.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Popdensity2010.tif",
  scaling = 1, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.HFP2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/HFP2009.tif",
  scaling = 1, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.Built2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Built2009.tif",
  scaling = 10, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.Built1994.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Built1994.tif",
  scaling = 10, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.HFP1993.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/HFP1993.tif",
  scaling = 1, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.NavWater2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/NavWater2009.tif",
  scaling = 4, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.Pasture1993.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Pasture1993.tif",
  scaling = 4, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.Popdensity1990.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Popdensity1990.tif",
  scaling = 1, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.croplands1992.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/croplands1992.tif",
  scaling = 7, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.NavWater1994.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/NavWater1994.tif",
  scaling = 4, hmr_reprojected_lang_polys, lang_polys
)
tempseasonality_projections_2060 <- get_hmr_raster_values(file_in("data/projections/tempseasonality/wc2.1_2.5m_bioc_CNRM-ESM2-1_ssp370_2041-2060.tif"),
  scaling = 1, hmr_reprojected_lang_polys, lang_polys
)
tempseasonality_projections_2100 <- get_hmr_raster_values(file_in("data/projections/tempseasonality/wc2.1_2.5m_bioc_CNRM-ESM2-1_ssp370_2081-2100.tif"),
  scaling = 1, hmr_reprojected_lang_polys, lang_polys
)
human_footprint_dat_data.HumanFootprint.Maps.Roads.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Roads.tif",
  scaling = 8, hmr_reprojected_lang_polys, lang_polys
)
lang_id_csv <- readr::write_csv(lang_ids, file_out("data/lang_ids.csv"))
lang_ids_ISO <- lang_ids %>%
  dplyr::select(LANG_ISO) %>%
  dplyr::distinct()
EGIDS <- glotto_main %>%
  dplyr::select(LANG_ISO, EGIDS = Language_status_ethnologue) %>%
  dplyr::left_join(lang_polys_merged %>% dplyr::as_tibble() %>%
    dplyr::select(LANG_ISO, LMP_POP1)) %>%
  tidyr::separate(
    EGIDS,
    c("EGIDS", "rest"), " "
  ) %>%
  dplyr::select(-rest)
lang_polys_unioned <- lang_polys_merged %>%
  sf::st_union(by_feature = TRUE) %>%
  sf::st_make_valid()
lang_polys_merged_simplified <- lang_polys_merged %>% rmapshaper::ms_simplify(5e-04, keep_shapes = TRUE)
language_areas_merged <- sf::st_area(lang_polys_merged) %>%
  {
    as.vector(.) / 1000 / 1000
  } %>%
  dplyr::tibble(LANG_ISO = lang_polys_merged$LANG_ISO, polygon_area = .)
lang_polys_rds <- readr::write_rds(lang_polys_merged, file_out("data/outputs/lang_polys_merged.rds"))
hua_vars_dat <- get_hua_raster_values(hua_raster_high_res, hua_reprojected_lang_polys)
unmerged_langs <- lang_polys_cea %>% dplyr::filter(!LANG_ISO %in% lang_polys_w_soceco_ids_initial$LANG_ISO)
land_use <- list(tibble(hfp = human_footprint_dat_data.HumanFootprint.Maps.HFP2009.tif %>%
  lapply(as.vector)), tibble(built = human_footprint_dat_data.HumanFootprint.Maps.Built2009.tif %>%
  lapply(as.vector)), tibble(croplands = human_footprint_dat_data.HumanFootprint.Maps.croplands2005.tif %>%
  lapply(as.vector)), tibble(pasture = human_footprint_dat_data.HumanFootprint.Maps.Pasture2009.tif %>%
  lapply(as.vector)), tibble(pop_density = human_footprint_dat_data.HumanFootprint.Maps.Popdensity2010.tif %>%
  lapply(as.vector))) %>% dplyr::bind_cols()
diffs <- list(tibble(hfp_change = mapply(
  function(x, y) {
    x - y / (actual_years["hfp_2009"] -
      actual_years["hfp_1993"])
  }, human_footprint_dat_data.HumanFootprint.Maps.HFP2009.tif,
  human_footprint_dat_data.HumanFootprint.Maps.HFP1993.tif
) %>%
  lapply(as.vector)), tibble(built_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["built_2009"] - actual_years["built_1993"])
  },
  human_footprint_dat_data.HumanFootprint.Maps.Built2009.tif,
  human_footprint_dat_data.HumanFootprint.Maps.Built1994.tif
) %>%
  lapply(as.vector)), tibble(croplands_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["croplands_2009"] - actual_years["croplands_1993"])
  },
  human_footprint_dat_data.HumanFootprint.Maps.croplands2005.tif,
  human_footprint_dat_data.HumanFootprint.Maps.croplands1992.tif
) %>%
  lapply(as.vector)), tibble(nav_water_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["navwater_2009"] - actual_years["navwater_1993"])
  },
  human_footprint_dat_data.HumanFootprint.Maps.NavWater2009.tif,
  human_footprint_dat_data.HumanFootprint.Maps.NavWater1994.tif
) %>%
  lapply(as.vector)), tibble(pasture_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["pasture_2009"] - actual_years["pasture_1993"])
  },
  human_footprint_dat_data.HumanFootprint.Maps.Pasture2009.tif,
  human_footprint_dat_data.HumanFootprint.Maps.Pasture1993.tif
) %>%
  lapply(as.vector)), tibble(pop_density_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["popdens_2009"] - actual_years["popdens_1993"])
  },
  human_footprint_dat_data.HumanFootprint.Maps.Popdensity2010.tif,
  human_footprint_dat_data.HumanFootprint.Maps.Popdensity1990.tif
) %>%
  lapply(as.vector))) %>% dplyr::bind_cols()
tempseasonality_projections_df <- list(tibble(ts_2060 = tempseasonality_projections_2060 %>% lapply(function(x) {
  as.vector(x[
    ,
    4
  ])
})), tibble(ts_2100 = tempseasonality_projections_2100 %>%
  lapply(function(x) as.vector(x[, 4])))) %>% dplyr::bind_cols()
lang_ids_ISO_csv <- lang_ids_ISO %>% readr::write_csv(file_in("data/lang_ids_ISO.csv"))
lang_endangerment_scores <- get_all_endangerment_scores(
  lang_ids, glotto_main, EGIDS, LEI_data,
  UNESCO_data
)
lang_endangerment_scores_AES_EGIDS <- lang_ids %>%
  dplyr::left_join(glotto_main %>% dplyr::select(
    LANG_ISO,
    iso639_3_macro, glottocode, Family_name_glottolog, Population_numeric_ethnologue,
    endangerment_status_glottolog
  ), by = "LANG_ISO") %>%
  dplyr::mutate(iso_macro = paste0(
    LANG_ISO,
    "-", toupper(iso639_3_macro)
  )) %>%
  dplyr::left_join(EGIDS %>%
    dplyr::select(LANG_ISO, EGIDS, LMP_POP1)) %>%
  dplyr::distinct() %>%
  dplyr::rename(AES = endangerment_status_glottolog) %>%
  dplyr::mutate(EGIDS = factor(EGIDS,
    levels = c(
      "1", "2", "3", "4", "5", "6a", "6b", "7", "8a",
      "8b", "9", "10"
    ), ordered = TRUE
  ), AES = factor(AES,
    levels = c(
      "not endangered", "threatened", "shifting", "moribund",
      "nearly extinct", "extinct"
    ), ordered = TRUE
  ))
lang_circles <- generate_centred_circles(lang_polys_unioned,
  circle_area_km = 10000,
  res = 180
)
langs_num_polygons <- purrr::map_int(lang_polys_unioned$geometry, ~ length(.x))
spat_dists <- sf::st_distance(lang_polys_merged_simplified)
hua_vars_tibble <- lapply(hua_vars_dat, function(x) {
  x %>%
    as.data.frame() %>%
    as.list() %>%
    lapply(function(x) list(x)) %>%
    dplyr::as_tibble()
}) %>% dplyr::bind_rows()
merge_unmerged <- unmerged_langs[1:nrow(unmerged_langs), ] %>% sf::st_join(countries_w_soceco,
  join = sf::st_nearest_feature
)
ISO_to_glottocode <- lang_endangerment_scores %>%
  dplyr::group_by(LANG_ISO) %>%
  summarise(glottocode = glottocode[1])
all_multi_ISOs <- lang_endangerment_scores %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) %>%
  dplyr::arrange(
    dplyr::desc(count),
    LANG_ISO
  ) %>%
  write_csv(file_out("data/all_multi_ISO_languages.csv"))
multilanguage_summary <- lang_endangerment_scores %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(
    count = n(),
    LANG_ISOs = list(LANG_ISO), iso_macro = list(iso_macro),
    endangerment_glottolog = list(AES), endangerment_ethnologue = list(Language_status_ethnologue)
  ) %>%
  dplyr::filter(count > 1)
lang_endangerment_by_ISO <- lang_endangerment_scores %>%
  dplyr::group_by(
    LANG_ISO, AES, EGIDS,
    LEI, UNESCO
  ) %>%
  dplyr::summarise(count = n())
lang_endangerment_by_ISO_AES_EGIDS <- lang_endangerment_scores_AES_EGIDS %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(AES = min(AES, na.rm = TRUE), EGIDS = min(EGIDS,
    na.rm = TRUE
  ), L1_POP = mean(LMP_POP1, na.rm = TRUE))
multi_ISO_summ_AES_EGIDS <- lang_endangerment_scores_AES_EGIDS %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(
    count = n(), num_AES = n_distinct(AES),
    num_EGIDS = n_distinct(EGIDS), AES = list(AES), EGIDS = list(EGIDS),
    IDS = list(ID), LMP_POP1 = list(LMP_POP1)
  )
lang_circle_intersections <- lang_circles %>% sf::st_join(lang_polys_unioned %>% rmapshaper::ms_simplify(keep_shapes = TRUE) %>%
  sf::st_make_valid())
lang_circles_hmr_projected <- st_transform(lang_circles, hmr_proj4string) %>%
  st_as_sf() %>%
  st_cast("POLYGON")
lang_circles_hua_projected <- st_transform(lang_circles, hua_proj4string)
spat_dist_mat <- as.matrix(spat_dists) / 1000
grid_poly_data_poly <- land_use %>%
  dplyr::bind_cols(diffs) %>%
  dplyr::bind_cols(hua_vars_tibble) %>%
  dplyr::bind_cols(
    lang_polys %>% as_tibble() %>% dplyr::select(LANG_ISO),
    .
  ) %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(dplyr::across(.fns = ~ list(do.call(
    c,
    as.list(.x)
  ))), .groups = "keep") %>%
  dplyr::summarise(dplyr::across(.fns = ~ purrr::map_dbl(
    .,
    ~ mean(.x, na.rm = TRUE)
  )))
lang_polys_w_soceco_ids <- lang_polys_w_soceco_ids_initial %>% rbind(merge_unmerged)
glottolog_tree <- reduce_and_cleanup_glottolog_tree(glottolog_trees, ISO_to_glottocode)
check_language_dups <- sapply(multilanguage_summary$endangerment_glottolog, function(x) length(unique(x)))
lang_polys_merged_cea <- lang_polys_merged %>%
  dplyr::left_join(lang_endangerment_by_ISO_AES_EGIDS) %>%
  sf::st_transform(sf::st_crs(new_language_grid))
language_types <- readr::read_tsv(file_in("data/hedvig_output/Contact_lgs.tsv")) %>%
  dplyr::mutate(type = "contact") %>%
  dplyr::bind_rows(readr::read_tsv(file_in("data/hedvig_output/sign_lgs.tsv")) %>%
    dplyr::mutate(type = "sign")) %>%
  dplyr::bind_rows(readr::read_tsv(file_in("data/hedvig_output/other_exclusion.tsv")) %>%
    dplyr::mutate(type = "other")) %>%
  dplyr::select(
    LANG_ISO = ISO639P3code,
    type
  ) %>%
  add_macrolanguages(macro_languages) %>%
  dplyr::left_join(lang_endangerment_by_ISO_AES_EGIDS %>%
    dplyr::left_join(wlms_dat %>% dplyr::select(
      LANG_ISO = ISO,
      IM
    )) %>% dplyr::select(LANG_ISO, IM), .) %>%
  dplyr::left_join(official_languages,
    by = "LANG_ISO"
  ) %>%
  dplyr::mutate(official_in_any_country = ifelse(is.na(official_in_any_country),
    0, 1
  ), type = ifelse(is.na(type), "prevalent", type)) %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(
    type = type[1],
    official_in_any_country = mean(official_in_any_country),
    IM = ifelse("Y" %in% IM, "Y", "N")
  ) %>%
  dplyr::mutate(type = as.factor(type))
new_language_diversity <- make_new_language_diversity(lang_circle_intersections, lang_endangerment_by_ISO_AES_EGIDS)
connectivity_raster_dat_data.HumanFootprint.Maps.Popdensity2010.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Popdensity2010.tif",
  scaling = 1, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.HFP2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/HFP2009.tif",
  scaling = 1, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.croplands2005.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/croplands2005.tif",
  scaling = 7, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.Pasture2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Pasture2009.tif",
  scaling = 4, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.Built2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Built2009.tif",
  scaling = 10, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.NavWater2009.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/NavWater2009.tif",
  scaling = 4, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.NavWater1994.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/NavWater1994.tif",
  scaling = 4, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.Built1994.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Built1994.tif",
  scaling = 10, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.HFP1993.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/HFP1993.tif",
  scaling = 1, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.Popdensity1990.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Popdensity1990.tif",
  scaling = 1, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.Pasture1993.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Pasture1993.tif",
  scaling = 4, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.croplands1992.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/croplands1992.tif",
  scaling = 7, lang_circles_hmr_projected, lang_circles
)
connectivity_raster_dat_data.HumanFootprint.Maps.Roads.tif <- get_hmr_raster_values(
  human_footprint_raster = "data/HumanFootprint/Maps/Roads.tif",
  scaling = 8, lang_circles_hmr_projected, lang_circles
)
hua_vars_nb <- get_hua_raster_values(hua_raster_high_res, lang_circles_hua_projected)
spat_dist_mat_named <- spat_dist_mat %>%
  magrittr::set_colnames(lang_polys_merged_simplified$LANG_ISO) %>%
  magrittr::set_rownames(lang_polys_merged_simplified$LANG_ISO)
soceco_areas <- lang_polys_w_soceco_ids %>%
  dplyr::bind_cols(area = as.vector(sf::st_area(lang_polys_w_soceco_ids))) %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::mutate(prop_area = area / sum(area))
multi_soceco_langs <- lang_polys_w_soceco_ids %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise(num_polys = n())
ts_projections_rast <- raster::stack(
  raster::stack(file_in("data/projections/tempseasonality/wc2.1_2.5m_bioc_CNRM-ESM2-1_ssp370_2041-2060.tif"))[[4]],
  raster::stack(file_in("data/projections/tempseasonality/wc2.1_2.5m_bioc_CNRM-ESM2-1_ssp370_2081-2100.tif"))[[4]]
) %>%
  raster::projectRaster(crs = sp::CRS(sf::st_crs(lang_polys_merged_cea)$proj4string)) %>%
  raster::writeRaster(file_out("data/climate/ts_projections.grd"),
    overwrite = TRUE
  )
nb_net <- spdep::poly2nb(lang_polys_merged_cea, snap = 100)
language_perimeters_merged <- lwgeom::st_perimeter(lang_polys_merged_cea) / 1000
climate_rast <- raster::stack(list(
  mean_temp = raster::raster(file_in("data/climate/wc2.0_bio_2.5m_01.tif")),
  mean_prec = raster::raster(file_in("data/climate/wc2.0_bio_2.5m_12.tif")),
  seas_temp = raster::raster(file_in("data/climate/wc2.0_bio_2.5m_04.tif")),
  seas_prec = raster::raster(file_in("data/climate/wc2.0_bio_2.5m_15.tif"))
)) %>%
  raster::projectRaster(crs = sp::CRS(sf::st_crs(lang_polys_merged_cea)$proj4string)) %>%
  raster::writeRaster(file_out("data/climate/all_clim.grd"),
    overwrite = TRUE
  )
languages_by_grid <- sf::st_intersects(new_language_grid, lang_polys_merged_cea %>%
  dplyr::select(LANG_ISO, LMP_POP1, AES))
alt_rast <- raster::raster(file_in("data/climate/alt.bil")) %>%
  raster::projectRaster(crs = sp::CRS(sf::st_crs(lang_polys_merged_cea)$proj4string)) %>%
  raster::writeRaster(file_out("data/climate/alt.grd"), overwrite = TRUE)
IUCN_data <- make_IUCN_data(
  IUCN_by_grid, IUCN_all_by_grid, IUCN_grid_raster,
  lang_polys_merged_cea
)
land_use_nb <- list(tibble(hfp = connectivity_raster_dat_data.HumanFootprint.Maps.HFP2009.tif %>%
  lapply(as.vector)), tibble(built = connectivity_raster_dat_data.HumanFootprint.Maps.Built2009.tif %>%
  lapply(as.vector)), tibble(croplands = connectivity_raster_dat_data.HumanFootprint.Maps.croplands2005.tif %>%
  lapply(as.vector)), tibble(pasture = connectivity_raster_dat_data.HumanFootprint.Maps.Pasture2009.tif %>%
  lapply(as.vector)), tibble(pop_density = connectivity_raster_dat_data.HumanFootprint.Maps.Popdensity2010.tif %>%
  lapply(as.vector))) %>% dplyr::bind_cols()
diffs_nb <- list(tibble(hfp_change = mapply(
  function(x, y) {
    x - y / (actual_years["hfp_2009"] -
      actual_years["hfp_1993"])
  }, connectivity_raster_dat_data.HumanFootprint.Maps.HFP2009.tif,
  connectivity_raster_dat_data.HumanFootprint.Maps.HFP1993.tif
) %>%
  lapply(as.vector)), tibble(built_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["built_2009"] - actual_years["built_1993"])
  },
  connectivity_raster_dat_data.HumanFootprint.Maps.Built2009.tif,
  connectivity_raster_dat_data.HumanFootprint.Maps.Built1994.tif
) %>%
  lapply(as.vector)), tibble(croplands_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["croplands_2009"] - actual_years["croplands_1993"])
  },
  connectivity_raster_dat_data.HumanFootprint.Maps.croplands2005.tif,
  connectivity_raster_dat_data.HumanFootprint.Maps.croplands1992.tif
) %>%
  lapply(as.vector)), tibble(nav_water_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["navwater_2009"] - actual_years["navwater_1993"])
  },
  connectivity_raster_dat_data.HumanFootprint.Maps.NavWater2009.tif,
  connectivity_raster_dat_data.HumanFootprint.Maps.NavWater1994.tif
) %>%
  lapply(as.vector)), tibble(pasture_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["pasture_2009"] - actual_years["pasture_1993"])
  },
  connectivity_raster_dat_data.HumanFootprint.Maps.Pasture2009.tif,
  connectivity_raster_dat_data.HumanFootprint.Maps.Pasture1993.tif
) %>%
  lapply(as.vector)), tibble(pop_density_change = mapply(
  function(x,
           y) {
    x - y / (actual_years["popdens_2009"] - actual_years["popdens_1993"])
  },
  connectivity_raster_dat_data.HumanFootprint.Maps.Popdensity2010.tif,
  connectivity_raster_dat_data.HumanFootprint.Maps.Popdensity1990.tif
) %>%
  lapply(as.vector))) %>% dplyr::bind_cols()
connectivity_nb <- list(tibble(nav_water_nb = connectivity_raster_dat_data.HumanFootprint.Maps.NavWater2009.tif %>%
  lapply(as.vector)), tibble(pop_density_nb = connectivity_raster_dat_data.HumanFootprint.Maps.Popdensity2010.tif %>%
  lapply(as.vector)), tibble(roads_nb = connectivity_raster_dat_data.HumanFootprint.Maps.Roads.tif %>%
  lapply(as.vector))) %>% dplyr::bind_cols()
hua_vars_nb_tibble <- lapply(hua_vars_nb, function(x) {
  x %>%
    as.data.frame() %>%
    as.list() %>%
    lapply(function(x) list(x)) %>%
    dplyr::as_tibble()
}) %>% dplyr::bind_rows()
world_language_averaged <- soceco_areas %>%
  dplyr::ungroup() %>%
  dplyr::mutate(world_language = purrr::map2(
    world_language,
    prop_area, ~ .x * .y
  )) %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::group_modify(~ dplyr::tibble(world_language = list(do.call(
    rbind,
    .x$world_language
  ) %>% apply(2, sum_w_NA))))
ts_projections_by_lang <- get_projections_data(ts_projections_rast, lang_polys_merged_cea)
nb_net_mat_named <- make_nb_net_mat(nb_net) %>%
  magrittr::set_colnames(lang_polys_merged$LANG_ISO) %>%
  magrittr::set_rownames(lang_polys_merged$LANG_ISO)
bordering_languages <- lang_polys_merged %>%
  dplyr::as_tibble() %>%
  dplyr::select(LANG_ISO) %>%
  dplyr::mutate(neighbours = purrr::map_int(nb_net, ~ sum(. !=
    0)), neighour_list_indices = unclass(nb_net), neighbour_list_ids = purrr::map(
    nb_net,
    ~ lang_polys_merged$LANG_ISO[.] %>% as.character()
  ), neighbour_list_pops = purrr::map(
    nb_net,
    ~ lang_polys_merged$LMP_POP1[.]
  ), perimeter = language_perimeters_merged %>%
    as.numeric())
diversity_raster_480 <- make_diversity_raster_480(
  languages_by_grid, hua_data_480_language_diversity,
  lang_polys_merged_cea
)
climate_by_lang <- get_climate_data(climate_rast, alt_rast, lang_polys_merged_cea)
grid_poly_data_nb <- lang_circles %>%
  dplyr::as_tibble() %>%
  dplyr::select(LANG_ISO) %>%
  dplyr::bind_cols(land_use_nb) %>%
  dplyr::bind_cols(diffs_nb) %>%
  dplyr::mutate_at(dplyr::vars(-LANG_ISO), ~ purrr::map_dbl(
    .,
    ~ mean(., na.rm = TRUE)
  )) %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::summarise_all(~ mean(., na.rm = TRUE))
connectivity_nb_data <- connectivity_nb %>%
  dplyr::bind_cols(hua_vars_nb_tibble) %>%
  dplyr::mutate_all(~ purrr::map_dbl(., ~ mean(., na.rm = TRUE))) %>%
  dplyr::bind_cols(
    lang_polys_unioned %>% as_tibble() %>% dplyr::select(LANG_ISO),
    .
  )
soceco_by_lang <- soceco_areas %>%
  dplyr::summarise_at(dplyr::vars(
    GDP_2010USD_10yr_median,
    Gini_SWIID_10yr_median, Ed_spending_pcGDP_10yr_median, Mean_yr_school_10yr_median,
    Life_exp_birth_10yr_median, Life_exp_sixty_average, urban_change,
    world_language_as_official, minority_education_policy, gdp.pcap.10yrmed,
    expense.pcGDP.10yrmed, ed.sp.prm.pcGDP.10yrmed, ed.sp.sec.pcGDP.10yrmed,
    ed.sp.tert.pcGDP.10yrmed, ed.sp.tot.pcxpd.10yrmed
  ), ~ sum_w_NA(. *
    prop_area)) %>%
  dplyr::left_join(world_language_averaged) %>%
  dplyr::mutate(world_language_as_official = ifelse(sapply(
    world_language,
    length
  ) == 0, NA, world_language_as_official))
projections_data_poly_correct_csv <- readr::write_csv(ts_projections_by_lang, file_out("data/final_data/tempseasonality_projections_final.csv"))
language_bordering_diversity <- bordering_languages %>%
  dplyr::mutate(
    bordering_language_richness = neighbours %>%
      as.numeric(), bordering_language_richness_per_km_perim = bordering_language_richness / perimeter,
    bordering_language_sw = purrr::map_dbl(
      neighbour_list_pops,
      ~ asbio::SW.index(.x)
    ), bordering_language_evenness = bordering_language_sw / log(bordering_language_richness)
  ) %>%
  dplyr::mutate(bordering_language_evenness = ifelse(is.nan(bordering_language_evenness),
    0, bordering_language_evenness
  ))
language_grid_based_data <- get_grid_based_data(lang_polys_cea, diversity_raster_480)
grid_poly_data <- grid_poly_data_poly %>%
  dplyr::filter(!is.finite(hfp)) %>%
  dplyr::select(LANG_ISO) %>%
  dplyr::left_join(grid_poly_data_nb) %>%
  dplyr::mutate(used_nb_for_landuse = 1) %>%
  dplyr::bind_rows(grid_poly_data_poly %>% dplyr::filter(is.finite(hfp)) %>%
    dplyr::select(LANG_ISO:pop_density_change) %>% dplyr::mutate(used_nb_for_landuse = 0)) %>%
  dplyr::left_join(grid_poly_data_poly %>% dplyr::select(-hfp:-pop_density_change))
language_grid_based_data_by_ISO <- language_grid_based_data %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(area = as.vector(language_areas) / (1000 *
    1000)) %>%
  dplyr::group_by(LANG_ISO) %>%
  dplyr::mutate(area_prop = area / sum(area)) %>%
  dplyr::summarise(language_richness = sum(language_richness *
    area_prop), language_evenness = sum(language_evenness *
    area_prop), languages_threatened = sum(languages_threatened *
    area_prop), languages_prop_threatened = sum(languages_prop_threatened *
    area_prop), total_L1_POP = sum(total_L1_POP * area_prop))
bring_it_in <- bring_it_all_in(
  lang_endangerment_by_ISO_AES_EGIDS, soceco_by_lang,
  language_types, grid_poly_data, new_language_diversity, language_bordering_diversity,
  connectivity_nb_data, IUCN_data, climate_by_lang, used_in_ed,
  glottolog_all, language_areas_merged
)
transformation_table <- find_best_power_ladder_transforms(bring_it_in)
endangered_outlier_plot <- make_endangered_outlier_plot(bring_it_in)
miss_vis_1 <- naniar::vis_miss(bring_it_in) + theme(plot.margin = unit(c(
  0.1,
  2, 0.1, 0.1
), "inches"))
transformation_list <- make_transformation_list(transformation_table)
all_data_transformed <- map2_dfc(
  bring_it_in, transformation_list[colnames(bring_it_in)],
  ~ if (is.numeric(.x)) {
    .y$transform(.x)
  } else {
    .x
  }
)
double_check_maps <- make_double_check_maps(
  lang_polys_merged_cea, bring_it_in, base_map,
  transformation_list, transformation_table, multi_soceco_langs
)
final_data_all <- bring_it_in %>% dplyr::left_join(all_data_transformed %>% dplyr::select(
  -AES,
  -EGIDS
), by = "LANG_ISO", suffix = c("", "_tr"))
corr_plots <- make_corr_plots(all_data_transformed)
transformed <- transformation_table$column_name[transformation_table$transformation_name !=
  "identity"] %>% match(colnames(all_data_transformed))
distribution_plots <- make_distribution_plots(bring_it_in, all_data_transformed)
final_data <- final_data_all %>%
  dplyr::filter(category == "Spoken_L1_Language") %>%
  dplyr::filter(!LANG_ISO %in% world_langs_macro) %>%
  dplyr::filter(LANG_ISO %in%
    rownames(spat_dist_mat_named)) %>%
  dplyr::select(
    -nav_water_change,
    -nav_water_change_tr, -popden, -popden_tr, -category, -category_tr,
    -altitude, -altitude_tr, -annual_precipitation, -annual_precipitation_tr,
    -mgs, -mgs_tr
  ) %>%
  tidyr::drop_na(EGIDS)
final_data_w_world <- final_data_all %>%
  dplyr::filter(category == "Spoken_L1_Language") %>%
  dplyr::filter(LANG_ISO %in% rownames(spat_dist_mat_named)) %>%
  tidyr::drop_na(EGIDS)
final_data_missing_EGIDS <- final_data_all %>%
  dplyr::filter(category == "Spoken_L1_Language") %>%
  dplyr::filter(!LANG_ISO %in% unlist(world_langs)) %>%
  dplyr::filter(LANG_ISO %in%
    rownames(spat_dist_mat_named)) %>%
  dplyr::select(
    -nav_water_change,
    -nav_water_change_tr, -popden, -popden_tr, -category, -category_tr,
    -altitude, -altitude_tr, -annual_precipitation, -annual_precipitation_tr,
    -mgs, -mgs_tr
  ) %>%
  dplyr::filter(is.na(EGIDS))
final_data_w_world_and_missing <- final_data_all %>%
  dplyr::filter(category == "Spoken_L1_Language") %>%
  dplyr::filter(LANG_ISO %in% rownames(spat_dist_mat_named))
EGIDS_map_final <- make_EGIDS_map_final(lang_polys_merged_cea, final_data, base_map_unioned) %>%
  ggsave(file_out("figures/big_EGIDS_map_final.pdf"), .,
    width = 45,
    height = 28.125, limitsize = FALSE
  )
tree_ethno <- ape::read.tree(file_in("data/trees/ethno_tree.tre")) %>% ape::drop.tip(which(!.$tip.label %in%
  final_data$LANG_ISO))
phylogenetic_covariance_matrix <- phy_corr_glottolog[final_data$LANG_ISO, final_data$LANG_ISO]
final_L1_POP <- final_data %>% dplyr::select(LANG_ISO, L1_POP)
final_data_polys_cea <- lang_polys_cea %>%
  dplyr::inner_join(final_data) %>%
  sf::st_make_valid()
neighbour_net_covariance_matrix <- nb_net_mat_named[final_data$LANG_ISO, final_data$LANG_ISO]
spatial_distance_matrix <- spat_dist_mat_named[final_data$LANG_ISO, final_data$LANG_ISO]
world_language_sanity_check <- soceco_by_lang %>%
  dplyr::select(
    LANG_ISO, world_language_as_official,
    world_language
  ) %>%
  tidyr::unnest_wider(world_language) %>%
  dplyr::left_join(lang_names) %>%
  dplyr::select(
    LANG_ISO,
    Name, dplyr::everything()
  ) %>%
  dplyr::filter(LANG_ISO %in%
    final_data$LANG_ISO) %>%
  dplyr::arrange(
    !is.na(world_language_as_official),
    world_language_as_official
  ) %>%
  readr::write_csv(file_in("data/world_language_sanity_check.csv"))
final_data_w_names <- final_data %>%
  dplyr::left_join(glotto_main %>% dplyr::select(
    LANG_ISO,
    Name
  )) %>%
  dplyr::select(Name, dplyr::everything())
final_data_missing_csv <- readr::write_csv(final_data_missing_EGIDS, file_out("data/final_data/language_endangerment_data_final_missing_EGIDS.csv"))
phy_corr_ethno <- vcv.phylo(tree_ethno, corr = TRUE)
phylogenetic_covariance_matrix_csv <- write.csv(phylogenetic_covariance_matrix, file_out("data/final_data/phylogenetic_covariance_matrix.csv"))
final_L1_POP_csv <- final_L1_POP %>% readr::write_csv(file_out("data/final_L1_POP.csv"))
continents <- get_continents(final_data_polys_cea, base_map_w_antarctica)
island_assignment <- assign_islands(final_data_polys_cea, base_map_exploded_cea)
neighbour_net_covariance_matrix_csv <- write.csv(neighbour_net_covariance_matrix, file_out("data/final_data/neighbour_net_covariance_matrix.csv"))
spatial_distance_matrix_csv <- write.csv(spatial_distance_matrix, file_out("data/final_data/spatial_distance_matrix.csv"))
island_langs <- island_assignment %>%
  dplyr::mutate(island = ifelse(max_area <
    10000, 1, 0)) %>%
  dplyr::as_tibble() %>%
  dplyr::select(
    LANG_ISO,
    island
  )
final_data_w_info <- final_data %>%
  dplyr::left_join(island_langs) %>%
  dplyr::left_join(glotto_main %>%
    dplyr::select(LANG_ISO, Name) %>% dplyr::group_by(LANG_ISO) %>%
    dplyr::summarise(names = list(unique(Name)), n = length(unique(Name))) %>%
    dplyr::mutate(name = purrr::map_chr(names, ~ paste(.x, collapse = ", "))) %>%
    dplyr::mutate(name = ifelse(n > 1, paste0(
      "Macrolanguage containing: ",
      name
    ), name)) %>% dplyr::select(-n, -names)) %>%
  dplyr::left_join(continents) %>%
  dplyr::select(name, LANG_ISO, dplyr::everything()) %>%
  dplyr::arrange(LANG_ISO) %>%
  dplyr::select(
    id_ISO_lang = LANG_ISO, id_name_lang = name,
    lang_continent_lang = continent, lang_subregion_lang = subregion,
    lang_island_lang = island, response_AES_lang = AES, response_EGIDS_lang = EGIDS,
    soceco_GDP.2010USD.10yr.median_country = GDP_2010USD_10yr_median,
    lang_L1.POP_lang = L1_POP, soceco_Gini.SWIID.10yr.median_country = Gini_SWIID_10yr_median,
    edu_Ed.spending.pcGDP.10yr.median_country = Ed_spending_pcGDP_10yr_median,
    edu_Mean.yr.school.10yr.median_country = Mean_yr_school_10yr_median,
    soceco_Life.exp.birth.10yr.median_country = Life_exp_birth_10yr_median,
    soceco_Life.exp.sixty.average_country = Life_exp_sixty_average,
    soceco_gdp.pcap.10yrmed_country = gdp.pcap.10yrmed, soceco_expense.pcGDP.10yrmed_country = expense.pcGDP.10yrmed,
    edu_ed.sp.prm.pcGDP.10yrmed_country = ed.sp.prm.pcGDP.10yrmed,
    edu_ed.sp.sec.pcGDP.10yrmed_country = ed.sp.sec.pcGDP.10yrmed,
    edu_ed.sp.tert.pcGDP.10yrmed_country = ed.sp.tert.pcGDP.10yrmed,
    edu_ed.sp.tot.pcxpd.10yrmed_country = ed.sp.tot.pcxpd.10yrmed,
    landuse_urban.change_country = urban_change, worldlang_world.language.as.official_country = world_language_as_official,
    edu_minority.education.policy_country = minority_education_policy,
    worldlang_Arabic_country = world_language_Arabic, worldlang_Bahasa_country = world_language_Bahasa,
    worldlang_English_country = world_language_English, worldlang_French_country = world_language_French,
    worldlang_Hindustani_country = world_language_Hindustani,
    worldlang_Mandarin_country = world_language_Mandarin,
    worldlang_Portuguese_country = world_language_Portuguese,
    worldlang_Russian_country = world_language_Russian, worldlang_Spanish_country = world_language_Spanish,
    edu_used.in.education_lang = used_in_education, lang_type_lang = type,
    lang_official.in.any.country_lang = official_in_any_country,
    landuse_hfp_polygon = hfp, landuse_built_polygon = built,
    landuse_croplands_polygon = croplands, landuse_pasture_polygon = pasture,
    landuse_pop.density_polygon = pop_density, shift_hfp.change_polygon = hfp_change,
    shift_built.change_polygon = built_change, shift_croplands.change_polygon = croplands_change,
    shift_pasture.change_polygon = pasture_change, shift_pop.density.change_polygon = pop_density_change,
    enviro_mgs_polygon = mgs_new, enviro_temperature.seasonality_polygon = temperature_seasonality,
    enviro_precipitation.seasonality_polygon = precipitation_seasonality,
    div_language.richness_nb = language_richness, div_threatened.languages_nb = threatened_languages,
    div_prop.threatened.languages_nb = prop_threatened_languages,
    div_language.evenness_nb = language_evenness, conn_nav.water_nb = nav_water_nb,
    landuse_pop.density_nb = pop_density_nb, conn_roads_nb = roads_nb,
    conn_altitude.range_nb = altitude_range, conn_roughness_nb = roughness_nb,
    conn_river_nb = river_nb, div_bordering.language.richness_lang = bordering_language_richness,
    div_bordering.language.richness.per.km.perim_lang = bordering_language_richness_per_km_perim,
    div_bordering.language.evenness_lang = bordering_language_evenness,
    biodiv_species.endangered_polygon = species_endangered,
    biodiv_species.prop.endangered_polygon = species_prop_endangered,
    enviro_mean.annual.temperature.kelvin_polygon = mean_annual_temperature_kelvin,
    div_L1pop.over.nb.popdens_nb = L1pop_over_nb_popdens,
    lang_polygon.area_lang = polygon_area, lang_documentation_lang = documentation,
    soceco_GDP.2010USD.10yr.median_country_tr = GDP_2010USD_10yr_median_tr,
    lang_L1.POP_lang_tr = L1_POP_tr, soceco_Gini.SWIID.10yr.median_country_tr = Gini_SWIID_10yr_median_tr,
    edu_Ed.spending.pcGDP.10yr.median_country_tr = Ed_spending_pcGDP_10yr_median_tr,
    edu_Mean.yr.school.10yr.median_country_tr = Mean_yr_school_10yr_median_tr,
    soceco_Life.exp.birth.10yr.median_country_tr = Life_exp_birth_10yr_median_tr,
    soceco_Life.exp.sixty.average_country_tr = Life_exp_sixty_average_tr,
    soceco_gdp.pcap.10yrmed_country_tr = gdp.pcap.10yrmed_tr,
    soceco_expense.pcGDP.10yrmed_country_tr = expense.pcGDP.10yrmed_tr,
    edu_ed.sp.prm.pcGDP.10yrmed_country_tr = ed.sp.prm.pcGDP.10yrmed_tr,
    edu_ed.sp.sec.pcGDP.10yrmed_country_tr = ed.sp.sec.pcGDP.10yrmed_tr,
    edu_ed.sp.tert.pcGDP.10yrmed_country_tr = ed.sp.tert.pcGDP.10yrmed_tr,
    edu_ed.sp.tot.pcxpd.10yrmed_country_tr = ed.sp.tot.pcxpd.10yrmed_tr,
    landuse_urban.change_country_tr = urban_change_tr, worldlang_world.language.as.official_country_tr = world_language_as_official_tr,
    edu_minority.education.policy_country_tr = minority_education_policy_tr,
    worldlang_Arabic_country_tr = world_language_Arabic_tr,
    worldlang_Bahasa_country_tr = world_language_Bahasa_tr,
    worldlang_English_country_tr = world_language_English_tr,
    worldlang_French_country_tr = world_language_French_tr,
    worldlang_Hindustani_country_tr = world_language_Hindustani_tr,
    worldlang_Mandarin_country_tr = world_language_Mandarin_tr,
    worldlang_Portuguese_country_tr = world_language_Portuguese_tr,
    worldlang_Russian_country_tr = world_language_Russian_tr,
    worldlang_Spanish_country_tr = world_language_Spanish_tr,
    edu_used.in.education_lang_tr = used_in_education_tr,
    lang_type_lang_tr = type_tr, lang_official.in.any.country_lang_tr = official_in_any_country_tr,
    landuse_hfp_polygon_tr = hfp_tr, landuse_built_polygon_tr = built_tr,
    landuse_croplands_polygon_tr = croplands_tr, landuse_pasture_polygon_tr = pasture_tr,
    landuse_pop.density_polygon_tr = pop_density_tr, shift_hfp.change_polygon_tr = hfp_change_tr,
    shift_built.change_polygon_tr = built_change_tr, shift_croplands.change_polygon_tr = croplands_change_tr,
    shift_pasture.change_polygon_tr = pasture_change_tr,
    shift_pop.density.change_polygon_tr = pop_density_change_tr,
    enviro_mgs_polygon_tr = mgs_new_tr, enviro_temperature.seasonality_polygon_tr = temperature_seasonality_tr,
    enviro_precipitation.seasonality_polygon_tr = precipitation_seasonality_tr,
    div_language.richness_nb_tr = language_richness_tr, div_threatened.languages_nb_tr = threatened_languages_tr,
    div_prop.threatened.languages_nb_tr = prop_threatened_languages_tr,
    div_language.evenness_nb_tr = language_evenness_tr, conn_nav.water_nb_tr = nav_water_nb_tr,
    landuse_pop.density_nb_tr = pop_density_nb_tr, conn_roads_nb_tr = roads_nb_tr,
    conn_altitude.range_tr = altitude_range_tr, conn_roughness_nb_tr = roughness_nb_tr,
    conn_river_nb_tr = river_nb_tr, div_bordering.language.richness_lang_tr = bordering_language_richness_tr,
    div_bordering.language.richness.per.km.perim_lang_tr = bordering_language_richness_per_km_perim_tr,
    div_bordering.language.evenness_lang_tr = bordering_language_evenness_tr,
    biodiv_species.endangered_polygon_tr = species_endangered_tr,
    biodiv_species.prop.endangered_polygon_tr = species_prop_endangered_tr,
    enviro_mean.annual.temperature.kelvin_polygon_tr = mean_annual_temperature_kelvin_tr,
    div_L1pop.over.nb.popdens_nb_tr = L1pop_over_nb_popdens_tr,
    lang_polygon.area_lang_tr = polygon_area_tr, lang_documentation_lang_tr = documentation_tr
  ) %>%
  dplyr::mutate(lang_island_lang = ifelse(id_ISO_lang == "fil",
    0, lang_island_lang
  ))
mapping_sf <- lang_polys_merged %>%
  dplyr::left_join(island_langs, by = "LANG_ISO") %>%
  dplyr::mutate(island = ifelse(is.na(island), 0, island)) %>%
  dplyr::filter(island == 0) %>%
  rbind(lang_polys_merged %>%
    dplyr::as_tibble() %>% dplyr::select(-geometry) %>% dplyr::left_join(lang_circles) %>%
    dplyr::left_join(island_langs, by = "LANG_ISO") %>% dplyr::filter(island ==
      1) %>% sf::st_as_sf())
final_data_w_projections <- final_data_w_info %>% dplyr::left_join(ts_projections_by_lang %>%
  dplyr::select(
    id_ISO_lang = LANG_ISO, proj_temperature.seasonality.2060_polygon = ts_2060,
    proj_temperature.seasonality.2100_polygon = ts_2100
  ))
final_data_polys_w_data_cea <- lang_polys_cea %>%
  sf::st_transform("EPSG:4326") %>%
  dplyr::inner_join(final_data_w_info,
    by = c(LANG_ISO = "id_ISO_lang")
  ) %>%
  sf::st_make_valid()
corr_plot_final <- make_corr_plots(final_data_w_info %>% dplyr::mutate(response_EGIDS_lang = as.numeric(response_EGIDS_lang)) %>%
  dplyr::select(response_EGIDS_lang, lang_island_lang, dplyr::ends_with("_tr")))
var_tab <- make_variable_table(
  final_data_w_info, transformation_table,
  var_list
)
final_data_csv <- readr::write_csv(final_data_w_info, file_out("data/final_data/language_endangerment_data_final.csv"))
corr_final <- round(
  cor(final_data_w_info %>% dplyr::mutate(response_EGIDS_lang = as.numeric(response_EGIDS_lang)) %>%
    dplyr::select(response_EGIDS_lang, lang_island_lang, dplyr::ends_with("_tr")) %>%
    dplyr::select_if(is.numeric) %>% dplyr::mutate_all(~ ifelse(is.finite(.),
      ., NA
    )) %>% as.matrix(), use = "pairwise.complete.obs"),
  2
)
voronoi_ids <- readr::write_csv(
  lang_polys_voronoi %>% as.data.frame() %>% dplyr::select(LANG_ISO) %>%
    dplyr::semi_join(final_data_w_info, by = c(LANG_ISO = "id_ISO_lang")),
  file_out("data/list_of_voronoi_languages.csv")
)
check_final_data_regions <- final_data_w_info %>%
  dplyr::left_join(final_data_polys_cea %>%
    dplyr::as_tibble() %>% dplyr::select(LANG_ISO, CNT) %>% dplyr::group_by(LANG_ISO) %>%
    dplyr::summarise(CNT = CNT[1]), by = c(id_ISO_lang = "LANG_ISO")) %>%
  dplyr::filter((lang_continent_lang == "Europe" & CNT != "Europe") |
    (lang_continent_lang == "Asia" & CNT != "Asia") | (lang_continent_lang ==
    "North America" & CNT != "Americas") | (lang_continent_lang ==
    "South America" & CNT != "Americas")) %>%
  dplyr::select(
    id_ISO_lang:lang_island_lang,
    CNT
  )
variable_maps <- make_variable_maps(
  lang_polys_merged_cea, bring_it_in, base_map_unioned,
  transformation_list, transformation_table, mapping_sf
)
map_wo_world_test <- make_variable_maps(
  lang_polys_merged_cea, final_data %>% dplyr::select(
    LANG_ISO,
    EGIDS
  ), base_map_unioned, transformation_list, transformation_table,
  mapping_sf
)
map_w_world_test <- make_variable_maps(
  lang_polys_merged_cea, final_data_w_world %>%
    dplyr::select(LANG_ISO, EGIDS), base_map_unioned, transformation_list,
  transformation_table, mapping_sf
)
final_data_w_projections_csv <- readr::write_csv(final_data_w_projections, file_out("data/final_data/language_endangerment_data_w_projections_final.csv"))
final_data_polys_w_data_cea_shp_file <- sf::st_write(final_data_polys_w_data_cea, file_out("data/final_polys/final_data_polys_w_data_wgs84.geojson"),
  append = FALSE
)
regions_map <- make_regions_map(base_map_w_antarctica, final_data_polys_w_data_cea)
corr_plot_final_pdf <- ggplot2::ggsave(file_out("figures/final_vars_corr_plot.pdf"),
  corr_plot_final,
  width = 16, height = 16
)
var_tab_csv <- readr::write_csv(var_tab, file_out("data/variable_table_base.csv"))
transform_supp_table <- make_transform_supp_table(var_tab)
corr_final_csv <- dplyr::as_tibble(corr_final, rownames = "variable") %>% readr::write_csv(file_out("data/final_variables_corr_matrix.csv"))
high_res_EGIDS_map <-
  {
    variable_maps[["EGIDS"]] + theme(legend.key.size = unit(
      0.05,
      "npc"
    ), legend.text = element_text(size = 28), legend.title = element_text(size = 42))
  } %>% ggsave(file_out("figures/big_EGIDS_map.pdf"), .,
    width = 40,
    height = 25
  )
