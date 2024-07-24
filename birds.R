library(magrittr)
library(stringr)
library(readxl)
library(dplyr)
library(purrr)

site <- "https://worldbirdnames.org"
file <- "master_ioc_list_v14.1.xlsx"

#######################################################################################
source("functions.R")

# This function replaces the missing values in the order, family, and genus columns: --
expand_names <- function(x) {
  i <- x |> 
    is.na() |> 
    not() |> 
    which() |> 
    first()
  
  times <- x |> 
    tail(-i) |> 
    is.na() |>
    as.integer() |> 
    paste(collapse = "") |> 
    strsplit("0") |> 
    unlist() |> 
    map_int(nchar)
  
  x |> 
    na.exclude() |> 
    unique() |> 
    rep(times + 1) %>%
    c(rep(NA, i - 1), .)
}


# The bird version of the select_classification_level() function used for mammals: ----
select_classification_level_birds <- function(df, lv = "order") {
  nbrow <- nrow(df)
  lv <- setNames(1:7, c("domain", "kingdom", "phylum", "class",
                        "order", "family", "genus"))[lv]
  
  out <- tibble(domain = rep("Eukaryota", nbrow), kingdom = rep("Chordata", nbrow),
                phylum = rep("Animalia", nbrow), class = rep("Mammalia", nbrow)) |> 
    bind_cols(df) |>
    extract((c(1:6, 8)[lv]):10)
  
  nbreps <- c(map(6:1, ~ c(1:.x, .x:(.x + 2))), list(1:2))[[lv]]
  for (i in seq_along(nbreps))
    out[[i + 1]] <- paste0("\n", paste0(rep("\t", nbreps[i]), collapse = ""),
                           out[[i + 1]])
  
  mutate(out, across(1:(ncol(out) - 2), reformat))
}



## Reading the excel file from worldbirdnames.org #####################################

if (! file.exists(file)) download.file(paste0(site, "/", file), file)

a <- read_excel(file, range = "A4:K33678") |> 
  tail(-1) |> 
  filter(is.na(Infraclass), is.na(Parvclass)) |> 
  select(-Subspecies, -Authority, -Infraclass, -Parvclass) |> 
  filter(if_any(everything(), ~ !is.na(.))) |> 
  mutate(across(contains("English"), ~ na_if(paste0("{", ., "}"), "{NA}")),
         across(Order, str_to_title),
         across(c(Order, `Family (Scientific)`, `Family (English)`, Genus),
                expand_names)) |> 
  filter(! is.na(`Species (Scientific)`)) |> 
  mutate(`Species (Scientific)` = paste(Genus, `Species (Scientific)`))

# Expanding the breeding ranges in a wide format of the dataset: ----------------------

# AF    Africa + Madagascar
# AN    Antarctica
# AO    Atlantic Ocean
# AU    Australasia
# IO    Indian Ocean
# MA    Central America
# NA    North America
# OR    Oriental Region
# PAL   Palearctic
# PO    Pacific Ocean
# SA    South America
# SO    Southern Ocean
# TrO   Tropical Ocean 
# Worldwide

branges <- a |> 
  pull(`Breeding Range`) |> 
  na.exclude() |> 
  unique() |> 
  paste(collapse = ", ") |> 
  str_remove_all(" ") |> 
  strsplit(",") |> 
  unlist() |> 
  unique() |> 
  sort()

wide_birds <- select(a, -`Breeding Range`)

for (i in branges) {
  wide_birds[, i] <- grepl(i, a$`Breeding Range`)
}

wide_birds %<>% select(- ePAL, -Worldwidebutdisjunctly)

# Writing to file: --------------------------------------------------------------------

wide_birds |> 
  #  do_filtering() |> # according to geography and taxonomy
  select_classification_level_birds("order") |> 
  write_file("IOC World Bird List v14.1", site, "birds.txt")



