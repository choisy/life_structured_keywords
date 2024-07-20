library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(magrittr)

# Parameters: -------------------------------------------------------------------------
site <- "https://www.mammalwatching.com/wp-content/uploads/"
file <- "Mammal-Taxonomy-Jan-14-2024.xlsx"
year <- 2024
month <- "03"

# Functions: --------------------------------------------------------------------------
# Tuning grep():
grep2 <- function(x, pattern, ...) grep(pattern, x, value = TRUE, invert = TRUE, ...)

# This function gets the row index where the name of the taxon should be deleted: -----
get_index <- function(x) {
  x |> 
    as_tibble() |> 
    tibble::rownames_to_column() |> 
    group_by(value) |> 
    group_modify(~ first(.)) |> 
    mutate(across(rowname, as.integer)) |> 
    arrange(rowname) |> 
    pull(rowname) %>% 
    { setdiff(seq_along(x), .) }
}

# This function removes the taxon names after the first occurrence: -------------------
reformat <- function(x) {
  x[get_index(x)] <- ""
  x
}

# This function writes the data on file: ----------------------------------------------
write_file <- function(x, file, long = TRUE) {
  x |> 
    select(-(Country:`Biogeographic Realm`)) |> 
    apply(1, paste, collapse = " ") |> 
    write("tmp.txt")
  
  x <- readLines("tmp.txt") |> 
    trimws(whitespace = " ") |> 
    grep2("^$") |> 
    grep2("NA") |> 
    grep2("^(\t)*$")
  file.remove("tmp.txt")
  
  header <- c("[Global Mammal Checklist 2024 https://www.mammalwatching.com]",
              "[© 2024 Marc Choisy https://www.instagram.com/marcchoisy]",
              paste0("[", format(Sys.time(), "%e %b %Y"), "]"))
    
  if (long) {
    x %>%
      paste0("\t\t\t\t", .) %>%
      c(header,
        "Eukaryota", 
        "\t{Eukaryotes}",
        "\tAnimalia",
        "\t\t{Animals}",
        "\t\tChordata",
        "\t\t\tMammalia",
        "\t\t\t\t{Mammals}", .) |> 
      write(file)
  } else {
    write(c(header, x2), file)
  }
}

#######################################################################################

# Downloading the raw data: -----------------------------------------------------------
if (! file.exists(file)) download.file(paste0(site, year, "/", month, "/", file), file)

# Loading the raw data: ---------------------------------------------------------------
original_version <- read_excel(file, range = "D5:Z6642") |> 
  rename(AltCommName = `...4`) |>
  separate(`Scientific Name`, c("genus", "species")) |> 
  mutate(species = paste(genus, species)) |>
  select(Order, Family, genus, species, `Common Name`, AltCommName, Country, Continent,
         `Biogeographic Realm`) |> 
  mutate(across(c(Order, Family), str_to_title)) |> 
  filter(Country != "NA") |> # removes humans
  mutate(across(`Biogeographic Realm`,
                ~ str_replace_all(., "Afrotropics", "Afrotropic"))) |> 
  mutate(across(`Biogeographic Realm`,
                ~ str_replace_all(., "Palearcic", "Palearctic"))) |> 
  mutate(across(Continent, ~ str_remove_all(., "\\?"))) |>
  mutate(across(Country, ~ str_remove_all(., "\\?"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Bosnia & Hercegovina",
                                              "Bosnia & Herzegovina"))) |> 
  mutate(across(Country, ~ str_replace_all(., "CuraÃ§ao", "Curaçao"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Japana", "Japan"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Myanma", "Myanmar"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Saint BarthÃ©lemy",
                                              "Saint Barthelemy")))

# Making the wide version of the data: ------------------------------------------------
realms <- original_version |> 
  pull(`Biogeographic Realm`) |> 
  unique() |> 
  strsplit("\\|") |>
  unlist() |>
  unique() |>
  sort()

continents <- original_version |> 
  pull(Continent) |> 
  unique() |> 
  strsplit("\\|") |>
  unlist() |>
  unique() |>
  sort()

countries <- original_version |> 
  pull(Country) |> 
  unique() |> 
  strsplit("\\|") |>
  unlist() |>
  trimws() |> 
  unique() |>
  sort() |> 
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

wide_version <- select(wide_version,
                       -`Biogeographic Realm`, -`Australasia/Oceania`,
                       -Continent, -Country)

# Writing to file: --------------------------------------------------------------------
original_version |> 
  mutate(across(c(Order, Family, genus), reformat)) |> 
  mutate(across(Family,  ~ paste0("\n\t", .))) |> 
  mutate(across(genus,   ~ paste0("\n\t\t", .))) |> 
  mutate(across(species, ~ paste0("\n\t\t\t", .))) |> 
  mutate(across(c("Common Name", "AltCommName"), ~ paste0("\n\t\t\t\t{", ., "}"))) |> 
  mutate(across(AltCommName, ~ str_replace_all(., "\\|", "}\n\t\t\t\t{"))) |>
  write_file("mammals.txt")
