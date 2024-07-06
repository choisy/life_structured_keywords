library(readxl)
library(dplyr)
library(purrr)
library(magrittr)

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

b <- a$`Family (English)`

add_curly <- function(x) {
  na_if(paste0("{", x, "}"), "{NA}")
}


a |> 
  mutate(across(contains("English"), ~ na_if(paste0("{", ., "}"), "{NA}")))


# Struthioniformes
#   Struthionidae
#     {ostriches}
#     Struthio
#       Struthio camelus
#         {common ostrich}
#       Struthio molybdophanes
#         {Somali ostrich}


