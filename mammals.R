library(readxl)
library(dplyr)
library(tidyr)

file <- "Mammal-Taxonomy-Jan-14-2024.xlsx"
year <- 2024
month <- "03"
site <- "https://www.mammalwatching.com/wp-content/uploads/"
if (! file.exists(file)) download.file(paste0(site, year, "/", month, "/", file), file)

get_index <- function(x) {
  x |> 
    as_tibble() |> 
    tibble::rownames_to_column() |> 
    group_by(value) |> 
    group_modify(~ first(.)) |> 
    mutate(across(rowname, as.integer)) |> 
    arrange(rowname) |> 
    pull(rowname) %>% 
#    { setdiff(min(.):max(.), .) }
    { setdiff(seq_along(x), .) }
}

reformat <- function(x) {
  x[get_index(x)] <- ""
  x
}

write_file <- function(x, file, long = TRUE) {
  x |> 
    select(-(Country:`Biogeographic Realm`)) |> 
    apply(1, paste, collapse = " ") |> 
    write("tmp.txt")
  
  x <- readLines("tmp.txt") |> 
    trimws(whitespace = " ") %>%
    grep("^$", ., value = TRUE, invert = TRUE) %>%
    grep("NA", ., value = TRUE, invert = TRUE) %>%
    grep("^(\t)*$", ., value = TRUE, invert = TRUE)
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

original_version <- read_excel(file, range = "D5:Z6642") |> 
  rename(AltCommName = `...4`) |>
  separate(`Scientific Name`, c("genus", "species")) |> 
  mutate(species = paste(genus, species)) |>
  select(Order, Family, genus, species, `Common Name`, AltCommName, Country, Continent,
         `Biogeographic Realm`) |> 
  mutate(across(c(Order, Family), str_to_title))

original_version |> 
  mutate(across(c(Order, Family, genus), reformat)) |> 
  mutate(across(Family,  ~ paste0("\n\t", .))) |> 
  mutate(across(genus,   ~ paste0("\n\t\t", .))) |> 
  mutate(across(species, ~ paste0("\n\t\t\t", .))) |> 
  mutate(across(c("Common Name", "AltCommName"), ~ paste0("\n\t\t\t\t{", ., "}"))) |> 
  mutate(across(AltCommName, ~ str_replace_all(., "\\|", "}\n\t\t\t\t{"))) |>
  write_file("mammals.txt")

#######################################################################################

corrected_version <- filter(original_version, Country != "NA")

## Expanding the Biogeographical realms: ##############################################

corrected_version <- corrected_version |> 
  mutate(across(`Biogeographic Realm`,
                ~ str_replace_all(., "Afrotropics", "Afrotropic"))) |> 
  mutate(across(`Biogeographic Realm`,
                ~ str_replace_all(., "Palearcic", "Palearctic")))

realms <- corrected_version |> 
  pull(`Biogeographic Realm`) |> 
  unique() |> 
  strsplit("\\|") |>
  unlist() |>
  unique() |>
  sort()

for (i in realms) {
  corrected_version[, i] <- grepl(i, corrected_version$`Biogeographic Realm`)
}

corrected_version <- select(corrected_version,
                            -`Biogeographic Realm`, -`Australasia/Oceania`)

corrected_version |> 
  filter(Domesticated)

## Expanding the Continents: ##########################################################

corrected_version <- mutate(corrected_version,
                            across(Continent, ~ str_remove_all(., "\\?")))

continents <- corrected_version |> 
  pull(Continent) |> 
  unique() |> 
  strsplit("\\|") |>
  unlist() |>
  unique() |>
  sort()

for (i in continents) {
  corrected_version[, i] <- grepl(i, corrected_version$Continent)
}

corrected_version <- select(corrected_version, -Continent)

corrected_version

## Expanding the countries: ###########################################################

corrected_version <- corrected_version |>
  mutate(across(Country, ~ str_remove_all(., "\\?"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Bosnia & Hercegovina", "Bosnia & Herzegovina"))) |> 
  mutate(across(Country, ~ str_replace_all(., "CuraÃ§ao", "Curaçao"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Japana", "Japan"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Myanma", "Myanmar"))) |> 
  mutate(across(Country, ~ str_replace_all(., "Saint BarthÃ©lemy", "Saint Barthelemy")))

countries <- corrected_version |> 
  pull(Country) |> 
  unique() |> 
  strsplit("\\|") |>
  unlist() |>
  trimws() |> 
  unique() |>
  sort() %>%
  grep("^$", ., value = TRUE, invert = TRUE)

for (i in countries) {
  corrected_version[, i] <- grepl(i, corrected_version$Country)
}

corrected_version <- select(corrected_version, -Country)
