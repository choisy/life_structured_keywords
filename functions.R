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



# Tuning grep():
grep2 <- function(x, pattern, ...) grep(pattern, x, value = TRUE, invert = TRUE, ...)



# This function writes the data on file: ----------------------------------------------
write_file <- function(x, name, url, file) {
  x |> 
    apply(1, paste, collapse = " ") |> 
    write("tmp.txt")
  
  "tmp.txt" |> 
    readLines() |> 
    trimws(whitespace = " ") |> 
    grep2("^$") |> 
    grep2("NA") |> 
    grep2("^(\t)*$") %>%
    c(paste0("[", name, " ", url, "]"),
      "[© 2024 Marc Choisy https://www.instagram.com/marcchoisy]",
      paste0("[", format(Sys.time(), "%e %b %Y"), "]"), .) |> 
    write(file)
  
  file.remove("tmp.txt")
}
