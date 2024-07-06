library(readxl)
library(dplyr)
library(purrr)
library(magrittr)
library(stringr)

a <- read_excel("~/Library/CloudStorage/OneDrive-OxfordUniversityClinicalResearchUnit/GitHub/choisy/life_structured_keywords/master_ioc_list_v14.1.xlsx",
                range = "A4:J33678") |> 
  tail(-1) |> 
  filter(is.na(Infraclass), is.na(Parvclass)) |> 
  select(-Subspecies, -Authority, -Infraclass, -Parvclass) |> 
  filter(if_any(everything(), ~ !is.na(.))) |> 
  mutate(across(contains("English"), ~ na_if(paste0("{", ., "}"), "{NA}")))

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

#################################################

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

##########

idx <- which(! is.na(a$`Species (English)`))
idx <- idx + cumsum(rep(1, length(idx))) - 1
na3 <- rep(NA, 4)
for (i in idx) {
  a <- bind_cols(rbind(a[1:i, 1:4], na3, a[-(1:i), 1:4]),
                 rbind(a[1:(i - 1), 5], NA, a[-(1:(i - 1)), 5]))  
}

############

a |> 
  apply(1, paste, collapse = " ") |>
  str_remove("( NA)*$") |> 
  str_replace_all("NA ", "\t") |> 
  write("marc.txt")
