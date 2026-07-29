##################################################################### ----
# Contaminants data on eelpout and perch from other sources than NRM  ----
##################################################################### ----

library(tidyverse)
library(readxl) 
library(openxlsx) 

############################################-
# Extraction of contaminant perch and eelpout marine/brakish data from SGU ----
## read file that links SGU contaminant codes with our contaminant names ----
contaminant_SGU_NRM <- read.csv("contaminants_SGU_NRM_copy.csv") |>
  select(contaminant, substance_group, UNIK_PARAMETERKOD)

## read and adapt SGU data ----
d <- read.csv2('original_data/Utsök_DV_Miljögifter_2026-07-27 13_22/biota.csv') |> #distinct(RAPPORTNAMN)
  filter(RAPPORTNAMN=='Samordnad recipientkontrol vid Oxelösundskusten 2015, Rapport 2016-05-22, slutversion'|
           RAPPORTNAMN=='Samlad recipientkontroll i Örnsköldsvik'|
           RAPPORTNAMN=='Regional screening av prioriterade ämnen och SFÄ, 2022'|
           RAPPORTNAMN=='GDP (X-321)'|RAPPORTNAMN=='') |>
  ### exclude  NRM data and Elenas effectscreening data ----
  filter(!NAMN_PROVPLATS=='Kvädöfjärden', !NAMN_PROVPLATS=='Holmöarna') |>
  filter(!BESTALLD_UNDERSOKNING=='Effektscreening miljögifter') |>
  filter(!PROVTAG_SYFTE=='NMO') |>
  ### de-clutter by removing some columns ----
  select(!c('ANALYS_DAT','PROVTAG_MET','PARAMETERNAMN','DYNTAXA_TAXON_ID','LANSID','PROVDATATYP','PROVPLATS_MILJO',
            'ACKR_PROV','UNDERSOKNINGSTYP','RESULTAT_ID','LEVERANS_ID','LANK_TILL_DIVA','INT_RAPPORT',
            'KOMMENTAR_PROV','URSPRUNG')) 

## combine with our contaminant names and names/layout of data ----
dc <- d |> left_join(contaminant_SGU_NRM, by='UNIK_PARAMETERKOD') |>
  mutate(year= PROVTAG_DAT |> lubridate::year()) |>
  rename(station_name='NAMN_PROVPLATS',
         date=PROVTAG_DAT,
         number_individuals = ANTAL,
         value=MATVARDE,
         sample_ID = RAPPORT_KOD_LABB,
         uncertainty = MATOSAKERHET,) |>
mutate(         unit = NA_character_,
                organ = NA_character_,
                sex = NA_character_,
                station_type = NA_character_) |>
  ### rename units ----
mutate(unit = case_when(
  ENHET == 'ug.g-1.tv-1' ~ 'ug.g-1.dw-1', 
  ENHET == 'ng.g-1.vv-1' ~ 'ng.g-1.ww-1',
  ENHET == 'ng.g-1.lv-1' ~ 'ng.g-1.lw-1',
  ENHET == 'pg.g-1.lv-1' ~ 'pg.g-1.lw-1',
  ENHET == 'ug.g-1.lv-1' ~ 'ug.g-1.lw-1',
  ENHET == 'mg.kg-1.tv-1' ~ 'ug.g-1.dw-1',
  ENHET == 'mg.kg-1.vv-1' ~ 'ug.g-1.ww-1',
  ENHET == 'mg.kg-1.lv-1' ~ 'ug.g-1.lw-1',
  ENHET == 'ug.kg-1.vv-1' ~ 'ng.g-1.ww-1',
  ENHET == 'pg.g-1.vv-1' ~ 'pg.g-1.ww-1',
  ENHET == 'ng.g-1.tv-1' ~ 'ng.g-1.dw-1')) |>
  ### change organ names to English ----
mutate(
  organ = case_when(
  ORGAN == 'LEVER' ~'Liver',
  ORGAN == 'MUSKEL' ~ 'Muscle',
  TRUE ~ organ)) |>
  ### change sex ----
mutate(sex = case_when(
  KON == 'M' ~ 'Male',
  KON == 'F' ~ 'Female',
  KON == 'X' ~ 'Mixed',
  KON == 'U' ~ NA_character_,
  TRUE ~ sex
)) |>
  mutate(species_EN = case_when(
    ART == 'Abborre' ~ 'Perch',
    ART == 'Tanglake' ~ 'Eelpout'
  )) |>
  mutate(station_type = case_when(
    PROVPLATS_TYP == 'PaverkadMiljo' ~ 'Impacted',
    PROVPLATS_TYP == 'Ej_bedomd_okand' ~ 'Unknown',
    PROVPLATS_TYP == 'Bakgrund' ~ 'Reference',
    PROVPLATS_TYP == 'Punktkalla' ~ 'Point source',
    PROVPLATS_TYP == 'LokalBakgrund' ~ 'Impacted background',
    PROVPLATS_TYP == 'Industrihamn' ~ 'Industrial harbour'
  )) |>
  ### make consistent treatment of LOQ values ----
  mutate(value = str_replace_all(value, fixed("<"), "-"),
         is_censored = if_else(value > 0, FALSE, TRUE),
         value = abs(as.numeric(value))) |>
  filter(!is.na(value)) |>
  relocate(station_name,station_type, year,date,species_EN,number_individuals,sample_ID,substance_group,contaminant,value, unit,is_censored, uncertainty,organ, sex)

## remove columns with no information ----
dc1 <- dc[, !sapply(dc, function(x) {
  all(is.na(x)) ||
    (is.numeric(x) && all(is.na(x) | x < 0 | x=='NA'))
})]
  
## remove the biological data ----
dc2 <- dc1 |>  filter(!is.na(contaminant), substance_group != "Biological" )


## NB currently can't be attached as I do not have the specimen_ID information ----
biological <- dc1 |> filter(substance_group=='Biological')

## preliminary dataset ----
write.xlsx(dc2,"modified_data/SGU_download_contaminated.xlsx",showNA = FALSE, rowNames=FALSE)



