library(magrittr)
library(stringr)
library(readxl)
library(dplyr)
library(purrr)

site <- "https://worldbirdnames.org/"
file <- "master_ioc_list_v14.1.xlsx"

## Reading the excel file from worldbirdnames.org #####################################

if (! file.exists(file)) download.file(paste0(site, file), file)

a <- read_excel(file, range = "A4:J33678") |> 
  tail(-1) |> 
  filter(is.na(Infraclass), is.na(Parvclass)) |> 
  select(-Subspecies, -Authority, -Infraclass, -Parvclass) |> 
  filter(if_any(everything(), ~ !is.na(.))) |> 
  mutate(across(contains("English"), ~ na_if(paste0("{", ., "}"), "{NA}"))) |> 
  mutate(across(Order, str_to_title))

## Rewriting the species names with the binomial form #################################

genus <- a$Genus

species <- a |> 
  pull(`Species (Scientific)`) |> 
  tail(-2)

times <- genus |> 
  tail(-2) |> 
  map_lgl(is.na) |> 
  as.integer() |> 
  paste(collapse = "") |> 
  strsplit("0") |> 
  extract2(1) |> 
  tail(-1) |> 
  map_int(nchar)

species2 <- genus |> 
  na.exclude() |> 
  map2(times, ~ c(NA, rep(.x, .y))) |> 
  unlist() |> 
  paste(species) |> 
  na_if("NA NA")

species2[which(is.na(species))] <- NA

a$`Species (Scientific)` <- c(NA, NA, species2)

## Moving the English family names to the right place #################################

idx <- which(! is.na(a$`Family (English)`))
a$Genus[idx] <- a$`Family (English)`[idx]
a$`Family (English)` <- NULL
idx <- idx + cumsum(rep(1, length(idx))) - 1
na1 <- rep(NA, 2)
na2 <- rep(NA, 3)

for (i in idx) {
  a <- bind_cols(rbind(a[1:i, 1:2], na1, a[-(1:i), 1:2]),
                 rbind(a[1:(i - 1), 3:5], na2, a[-(1:(i - 1)), 3:5]))  
}

## Moving the English species names to the right place ################################

idx <- which(! is.na(a$`Species (English)`))
idx <- idx + cumsum(rep(1, length(idx))) - 1
na3 <- rep(NA, 4)
for (i in idx) {
  a <- bind_cols(rbind(a[1:i, 1:4], na3, a[-(1:i), 1:4]),
                 rbind(a[1:(i - 1), 5], NA, a[-(1:(i - 1)), 5]))  
}

## Writing the text file ##############################################################

a |> 
  apply(1, paste, collapse = " ") |>
  str_remove("( NA)*$") |> 
  str_replace_all("NA ", "\t") %>%
  paste0("\t\t\t\t\t", .) %>%
  c("[IOC World Bird List v14.1]",
    "[© 2024 Marc Choisy https://raw.githubusercontent.com/choisy/life_structured_keywords/main/ioc.txt]",
    "[06/07/2024]",
    "Eukaryota", 
    "\t{Eukaryotes}",
    "\tAnimalia",
    "\t\t{Animals}",
    "\t\t{Animaux}",
    "\t\tChordata",
    "\t\t\tAves",
    "\t\t\t\t{Birds}",
    "\t\t\t\t{Oiseaux}", .) |> 
  write("ioc.txt")
