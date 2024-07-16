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
    { setdiff(min(.):max(.), .) }
}

reformat <- function(x) {
  x[get_index2(x)] <- ""
  x
}

a <- read_excel(file, range = "D5:Z6642") |> 
  rename(AltCommName = `...4`) |>
  separate(`Scientific Name`, c("genus", "species")) |> 
  mutate(species = paste(genus, species)) |>
  select(Order, Family, genus, species, `Common Name`, AltCommName, Country, Continent,
         `Biogeographic Realm`) |> 
  mutate(across(c(Order, Family), str_to_title)) |> 
  mutate(across(c(Order, Family, genus), reformat)) |> 
  mutate(across(Family, ~ paste0("\n\t", .))) |> 
  mutate(across(genus, ~ paste0("\n\t\t", .))) |> 
  mutate(across(species, ~ paste0("\n\t\t\t", .))) |> 
  mutate(across(c("Common Name", "AltCommName"), ~ paste0("\n\t\t\t\t{", ., "}"))) |> 
  mutate(across(AltCommName, ~ str_replace_all(., "\\|", "}\n\t\t\t\t{")))

a |> 
  select(-(Country:`Biogeographic Realm`)) |> 
  apply(1, paste, collapse = " ") |>
  write("tmp.txt")

readLines("tmp.txt") |> 
  trimws(whitespace = " ") %>%
  grep("^$", ., value = TRUE, invert = TRUE) %>%
  grep("NA", ., value = TRUE, invert = TRUE) %>%
  grep("^(\t)*$", ., value = TRUE, invert = TRUE) |> 
  write("mammals.txt")

file.remove("tmp.txt")
