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

a2 <- a
idx <- which(! is.na(a$`Family (English)`))
a2$Genus[idx] <- a2$`Family (English)`[idx]
a2$`Family (English)` <- NULL
idx <- idx + cumsum(rep(1, length(idx))) - 1
na1 <- rep(NA, 2)
na2 <- rep(NA, 3)

for (i in idx) {
  a2 <- bind_cols(rbind(a2[1:i, 1:2], na1, a2[-(1:i), 1:2]),
                  rbind(a2[1:(i - 1), 3:5], na2, a2[-(1:(i - 1)), 3:5]))  
}



# Struthioniformes
#   Struthionidae
#     {ostriches}
#     Struthio
#       Struthio camelus
#         {common ostrich}
#       Struthio molybdophanes
#         {Somali ostrich}


