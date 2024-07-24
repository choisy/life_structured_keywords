library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(magrittr)

# Parameters: -------------------------------------------------------------------------
#site <- "https://www.mammalwatching.com/wp-content/uploads/"
site <- "https://www.mammalwatching.com"
file <- "Mammal-Taxonomy-Jan-14-2024.xlsx"
year <- 2024
month <- "03"

# Functions: --------------------------------------------------------------------------
source("functions.R")

# This function reformats |-separated value into a vector: ----------------------------
reformat_bars_sep <- function(x) {
  x |>
    unique() |> 
    strsplit("\\|") |>
    unlist() |>
    trimws() |> 
    unique() |>
    sort()
}

# This function selects the classification level: -------------------------------------
select_classification_level <- function(df, lv = "order") {
  nbrow <- nrow(df)
  lv <- setNames(1:7, c("domain", "kingdom", "phylum",
                        "class", "order", "family", "genus"))[lv]
  
  out <- tibble(domain = rep("Eukaryota", nbrow), kingdom = rep("Chordata", nbrow),
                phylum = rep("Animalia", nbrow), class = rep("Mammalia", nbrow)) |> 
    bind_cols(df) |>
    extract(lv:9)
  
  nbcol <- ncol(out)
  for (i in 2:(nbcol - 1))
    out[[i]] <- paste0("\n", paste0(rep("\t", i - 1), collapse = ""), out[[i]])
  
  
  tabs <- paste0(rep("\t", nbcol - 1), collapse = "")
  out |>
    mutate(across(`Common Name`, ~ paste0("\n", tabs, "{", ., "}") |>
                    str_replace_all("\\|", paste0("}\n", tabs, "{"))),
           across(1:(nbcol - 2), reformat))
}

#######################################################################################

# Downloading the raw data: -----------------------------------------------------------
if (! file.exists(file))
  download.file(paste0(site, "/wp-content/uploads/", year, "/", month, "/", file), file)

# Loading the raw data: ---------------------------------------------------------------
original_version <- read_excel(file, range = "D5:Z6642") |> 
  separate(`Scientific Name`, c("genus", "species")) |> 
  mutate(species       = paste(genus, species),
         `Common Name` = paste(`Common Name`, `...4`, sep = "|"),
         across(c(Order, Family), str_to_title),
         across(`Biogeographic Realm`,
                ~ str_replace_all(., "Afrotropics", "Afrotropic") |> 
                  str_replace_all("Palearcic", "Palearctic")),
         across(Continent, ~ str_remove_all(., "\\?")),
         across(Country, ~ str_remove_all(., "\\?") |> 
                  str_replace_all("Bosnia & Hercegovina", "Bosnia & Herzegovina") |> 
                  str_replace_all("CuraÃ§ao", "Curaçao") |> 
                  str_replace_all("Japana", "Japan") |> 
                  str_replace_all("Myanma", "Myanmar") |> 
                  str_replace_all("Saint BarthÃ©lemy", "Saint Barthelemy"))) |> 
  select(Order, Family, genus, species, `Common Name`, Country, Continent,
         `Biogeographic Realm`) |> 
  filter(Country != "NA") # removes humans


# Making the wide version of the data: ------------------------------------------------

realms <- original_version |> 
  pull(`Biogeographic Realm`) |> 
  reformat_bars_sep()

continents <- original_version |> 
  pull(Continent) |> 
  reformat_bars_sep()

countries <- original_version |> 
  pull(Country) |> 
  reformat_bars_sep() |> 
  grep2("^$")

wide_version <- original_version

for (i in realms) {
  wide_version[, i] <- grepl(i, wide_version$`Biogeographic Realm`)
}

for (i in continents) {
  wide_version[, i] <- grepl(i, wide_version$Continent)
}

for (i in countries) {
  wide_version[, i] <- grepl(i, wide_version$Country)
}

wide_version %<>% select(-`Biogeographic Realm`, -`Australasia/Oceania`,
                         -Continent, -Country)

# Writing to file: --------------------------------------------------------------------

wide_version |> 
#  do_filtering() |> # according to geography and taxonomy
  select_classification_level("order") |> 
  write_file("Global Mammal Checklist 2024", site, "mammals.txt")


