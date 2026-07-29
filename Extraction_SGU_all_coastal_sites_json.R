##################################################################### -
# Contaminants data on eelpout and perch from other sources than NRM  -
##################################################################### -

library(httr)
library(jsonlite)
library(tidyverse)
library(fmcom)
library(readxl) 
library(openxlsx) 
library(sf)

# import support files ----
con <- read.csv("contaminants_SGU_NRM_copy.csv") |>
  select(contaminant, substance_group, UNIK_PARAMETERKOD) |>
  rename(contaminant_code_SGU = 'UNIK_PARAMETERKOD')

TC <- read_excel("PFAS_tissue_conversion.xlsx", sheet ='HELCOM') |>
  select(contaminant,perch,eelpout) |>
  filter(!is.na(perch)) |>
  pivot_longer(perch:eelpout,names_to="species_EN", values_to="TC") |>
  filter(!is.na(TC))


# function for extrating SGU data ----
get_species_ogc <- function(species    = c("Abborre", "Tanglake"),
                            from_year,
                            to_year,
                            habitat   = "HAV-BRACKV",  # marine/brackish
                            media     = "Biota",
                            base_url  = "https://api.sgu.se/oppnadata/miljogifter-analysresultat-provplatser/ogc/features/v1") {
  
  species_list <- paste(sprintf("'%s'", species), collapse = ", ")
  
  filter_expr <- sprintf(
    "art IN (%s) AND provplatsmiljo = '%s' AND media = '%s' AND provtagningsdatum >= DATE('%d-01-01') AND provtagningsdatum <= DATE('%d-12-31')",
    species_list, habitat, media, from_year, to_year
  )
  
  all_pages <- list()
  start_index <- 0
  page_limit  <- 1000
  
  repeat {
    resp <- httr::GET(
      url = paste0(base_url, "/collections/analysresultat/items"),
      query = list(
        f             = "application/json",
        filter        = filter_expr,
        `filter-lang` = "cql2-text",
        limit         = page_limit,
        startIndex    = start_index
      )
    )
    httr::stop_for_status(resp)
    parsed <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    n <- if (!is.null(parsed$features)) nrow(parsed$features) else 0
    if (n == 0) break
    all_pages[[length(all_pages) + 1]] <- parsed$features
    start_index <- start_index + page_limit
    if (n < page_limit) break
  }
  
  if (length(all_pages) == 0) return(tibble())
  dplyr::bind_rows(all_pages)
}


# make call to api ----
df <- get_species_ogc(species = c("Abborre", "Tanglake"),
                      from_year = 2014, to_year = 2023) |>
  dplyr::rename_with(~ sub("^properties\\.", "", .x))

# extract station long/lat ----
station_cor <- df |>
  filter(!is.na(e)) |>
  st_as_sf(
    coords = c("e", "n"),
    crs = 3006,
    remove = FALSE
  ) %>%
  st_transform(4326) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude  = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() |>
  filter(provtagningssyfte!='NMO',
         bestalld_undersokning!='Effektscreening miljögifter') |>
  distinct(provplatsnamn, latitude, longitude) |>
  rename(station_name='provplatsnamn')



# massage data ----
df1 <- df |>
  filter(provtagningssyfte!='NMO',
         bestalld_undersokning!='Effektscreening miljögifter') |>
  ### explicit mapping: fmcom column name = SGU_data column name ----
transmute(
  specimen_ID = provkodoriginal,
  year        = provtagningsdatum |> lubridate::year(),
  month       = provtagningsdatum |> lubridate::month() |> 
    stringr::str_pad(width = 2, pad = "0"),
  day         = provtagningsdatum |> lubridate::day() |> 
    stringr::str_pad(width = 2, pad = "0"),
  date        = provtagningsdatum |> lubridate::as_date(),
  station_code_SGU       = nationellt_provplatsid,
  contaminant_code_SGU   = ntlkemiparameterid,
  value       = matvardetal,
  enhet       = enhet,
  matvstd     = matvstd,
  uncertainty = matosakerhet,
  art         = art,
  organ       = organ,
  kon         = kon,
  number_individuals     = antal,
  sample_ID   = rapportkodlabb,
  laboratory  = provplatstyp,
  analysinstrument       = analysinstrument,
  analysmetod = analysmetod,
  station_name = provplatsnamn
) |>
  ### join master table content ----
  left_join(con, by = c("contaminant_code_SGU"))|>
  filter(!is.na(contaminant),
         !is.na(value))


### extract fat and dry weight percentages, specimen specific merge ----
frac_data <- filter(df1, 
                    (contaminant_code_SGU %in% c('CH12/68','CH12/224'))) |>
  select(sample_ID, contaminant, value) |>
  group_by(sample_ID, contaminant) |>
  summarise(value=mean(value)) |>
  ungroup() |>
  pivot_wider(names_from = contaminant, values_from = value)  

df2 <- df1 |>
  ### exclude bio data ----
filter(substance_group != "Biological") |>
  ### initiate new columns ----
mutate(species = NA_character_,
       species_EN = NA_character_,
       class = "Fish",
       unit = NA_character_,
       sex = NA_character_,
       season = NA_character_) |>
  ### add Latin and English species names ----
mutate(
  species = case_when(
    art == "Abborre" ~ "Perca fluviatilis",
    art == "Tanglake" ~ "Zoarces viviparus",
    TRUE ~ species
  ),
  species_EN = case_when(
    art == "Abborre" ~ "perch",
    art == "Tanglake" ~ "eelpout",
    TRUE ~ species_EN
  )) |>
  ### rename units ----
mutate(unit = case_when(
  enhet == 'ug.g-1.tv-1' ~ 'ug.g-1.dw-1', 
  enhet == 'ng.g-1.vv-1' ~ 'ng.g-1.ww-1',
  enhet == 'ng.g-1.lv-1' ~ 'ng.g-1.lw-1',
  enhet == 'pg.g-1.lv-1' ~ 'pg.g-1.lw-1',
  enhet == 'ug.g-1.lv-1' ~ 'ug.g-1.lw-1')) |>
  ### change organ names to English ----
mutate(organ = case_when(
  organ == 'LEVER' ~'Liver',
  organ == 'MUSKEL' ~ 'Muscle',
  TRUE ~ organ)) |>
  ### change sex ----
mutate(sex = case_when(
  kon == 'M' ~ 'Male',
  kon == 'F' ~ 'Female',
  kon == 'X' ~ 'Mixed',
  kon == "H" ~ "Hermaphrodite",
  kon == "I" ~ "Immature",
  kon == 'U' ~ NA_character_,
  TRUE ~ sex
)) |>
  ### add season ----
mutate(season = case_when(
  month %in% c("01", "02") ~ "Winter",
  month %in% c("03", "04", "05", "06") ~ "Spring",
  month %in% c( "08", "09", "10", "11", "12") ~ "Autumn",
  month == "07" & day %in% c(1:15) ~ "Spring",
  month == "07" & day %in% c(16:31) ~ "Autumn",
  TRUE ~ season
)) |>
  ### add LOQ_LOD ----
mutate(LOQ_LOD = case_when(
  is.na(matvstd) ~ '>LOQ',
  matvstd == "q"~ "<LOQ",
  matvstd == "b" ~ "<LOD",
  matvstd == "Ja"~ ">LOD, <LOQ"),
  is_censored = case_when(
    is.na(matvstd) ~ FALSE,
    matvstd %in% c("q", "b", "Ja") ~ TRUE,
    TRUE ~ FALSE
  )) |>
  mutate(laboratory = case_when(
    laboratory == 'PaverkadMiljo' ~ 'Impacted',
    laboratory == 'Ej_bedomd_okand' ~ 'Unknown',
    laboratory == 'Bakgrund' ~ 'Reference',
    laboratory == 'Punktkalla' ~ 'Point source',
    laboratory == 'LokalBakgrund' ~ 'Impacted background',
    laboratory == 'Industrihamn' ~ 'Industrial harbour'
  )) |>
  ### add bio data ----
#left_join(bio_data, by = "specimen_ID") |>
  ### add fat and dry weight percentages ----
left_join(frac_data, by = "sample_ID") |>
left_join(TC, by = c("species_EN","contaminant")) |>
  mutate(value = case_when(
    organ == 'Muscle' & substance_group == 'PFAS' ~ value*TC,
    TRUE ~ value),
    organ = case_when(
      organ == 'Muscle' & substance_group == 'PFAS' ~ 'Liver',
      TRUE ~ organ
  )) |>
  left_join(station_cor, by='station_name')



## guarantee every fmcom column exists, even if lack in SGU_data ----
missing_cols <- setdiff(colnames(fmcom), colnames(df2))
df2[missing_cols] <- NA

## enforce fmcom column order ----
df2 <- df2[, colnames(fmcom)]

# export dataset ----
write.xlsx(df2,"modified_data/SGU_download_contaminated_v2.xlsx",showNA = FALSE, rowNames=FALSE)

