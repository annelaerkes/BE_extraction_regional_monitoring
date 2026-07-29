##################################################################### ----
# Contaminants data on eelpout and perch from Stockholm Stad          ----
##################################################################### ----

library(tidyverse)
library(readxl) 
library(openxlsx) 

##############################################-
# Data Stockholm stad ----
s <- read_excel('original_data/fisk-miljogifter-radata-ar-2010-2023-2_stockholm_stad.xlsx', sheet='miljogifter_fisk_2010_2023',
                col_types = c(c('text','text','text','text','numeric','numeric','numeric','numeric','text'),(rep(c('numeric'), 94)))) |>
  select(!c(Vattenförekomst,'SWEREF 99 18 00...5','SWEREF 99 18 00...6','Fångstdatum','Notering','SummaPCB7 µg/kg vv muskel','SummaPCB6_lipidnorm (MB) muskel')) |>
  select(c('Lokal':'PBDE209 µg/kg vv muskel','PBDE28 µg/kg vv muskel':'PBDE209 µg/kg vv muskel',
           'PCB 28 ng/g lipid muskel':'PCB180 ng/g lipid muskel','PBDE 47* ng/g lipid muskel':'PBDE-209* ng/g lipid muskel','HBCD* ng/g lipid muskel',
           'HBCD µg/kg vv muskel','PFOS (MB) µg/kg vv muskel':'Kvicksilver (MB) µg/kg vv muskel')) |>
  filter(Lokal == 'Lilla Värtan'|Lokal =='Strömmen'|Lokal=='Brunnsviken') |>
  arrange(Lokal)

ss <- s[, !sapply(s, function(x) {
  all(is.na(x)) ||
    (is.numeric(x) && all(is.na(x) | x < 0 | x=='NA'))
})]


sss <- ss |>
  pivot_longer('PCB28 µg/kg vv muskel':'Kvicksilver (MB) µg/kg vv muskel',names_to = 'contaminant', values_to='value') |>
  mutate(
    original_name = contaminant,
    
    tissue = str_extract(original_name, "\\S+$"),
    
    weight_unit = str_extract(original_name, "\\S+(?=\\s+\\S+$)"),
    
    unit = str_extract(original_name,
                       "(mg/kg|µg/kg|ug/kg|ng/g|pg/g|ng/kg)(?=\\s+\\S+\\s+\\S+$)"),
    
    contaminant = str_trim(
      str_remove(
        original_name,
        "\\s+(mg/kg|µg/kg|ug/kg|ng/g|pg/g|ng/kg)\\s+\\S+\\s+\\S+$"
      )
    )
  ) |>
  filter(!is.na(value)) |>
  mutate(contaminant = if_else(contaminant=='Kvicksilver (MB)','Hg', contaminant))|>
  mutate(contaminant = if_else(contaminant=='PFOS (MB)','PFOS', contaminant)) |>
  pivot_longer('Fetthalt muskel %':'Fetthalt lever %', names_to='fat_tissue',values_to='fat_percentage') |> 
  filter(!is.na(fat_percentage)) |>
  select(!original_name) |>
  mutate(
    contaminant = contaminant |>
      str_remove_all("[ *-]") |>  # remove spaces, * and -
      str_to_upper(),# convert to uppercase
    contaminant = if_else(contaminant == 'HG', 'Hg', contaminant),
    unit = case_when(
      unit == 'µg/kg' & weight_unit == 'lipid' ~ 'ng.g-1.lw-1',
      unit == 'µg/kg' & weight_unit == 'vv' ~ 'ng.g-1.ww-1',
      unit == 'ng/g' & weight_unit == 'lipid' ~ 'ng.g-1.lw-1',
      unit == 'ng/g' & weight_unit == 'lipid' ~ 'ng.g-1.ww-1',
    ),
    species_EN = 'Perch',
    is_censored = if_else(value > 0, FALSE, TRUE),
    value = abs(as.numeric(value))) |>
  select(!c(weight_unit,fat_tissue,Art, 'Poolat prov Ja/Nej')) |>
  rename(station_name = 'Lokal', year = 'År', number_individuals='Antal fiskar i poolat prov')

write.xlsx(sss,"modified_data/Stokcholm_stad_contaminated.xlsx",showNA = FALSE, rowNames=FALSE)