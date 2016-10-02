library(readr)

write_psv <- function(x, path, col_names = T){
  readr::write_delim(x, 
                     path = path,
                     delim = "|",
                     col_names = col_names)
}

read_psv <- function(file, col_names = T){
  readr::read_delim(file = file,
                     delim = "|",
                     col_names = col_names)
}

read_tsv2 <- function(file, col_names = T){
  readr::read_delim(file = file,
                    delim = "\t",
                    col_names = col_names)
}

read_csv3 <- function(file, 
                      col_names = T, 
                      col_types = NULL, 
                      na = c("", "NA"), 
                      comment = "",
                      skip = 0, 
                      n_max = -1, 
                      progress = interactive()){
  readr::read_delim(file = file,
                    delim = ",",
                    quote = "\"",
                    col_names = col_names,
                    col_types = col_types,
                    na = na,
                    comment = comment,
                    skip = skip,
                    n_max = n_max,
                    progress = progress)
}


duplicated_2 <- function(vec){
  # Shows ALL duplicated items
  duplicated(vec) | duplicated(vec, fromLast=TRUE)
}


make_names <- function(vec) {
  vec <- make.names(vec, unique = T)
  vec <- iconv(vec, from = "utf-8", to = "ASCII//TRANSLIT")
  vec <- stringi::stri_replace_all(regex = "[^a-zA-Z0-9\\_\\.]", replacement = "", str = vec)
  vec <- stringi::stri_replace_all(fixed = ".", replacement = "_", str = vec)
  return(vec)
}


axis_labels_vert <- function() theme(axis.text.x = element_text(angle = 90, hjust = 1))



outlier_range <- function(vec, na.rm = T){
  min_vec <- min(vec, na.rm = na.rm)
  max_vec <- max(vec, na.rm = na.rm)
  cuarts <- quantile(vec, c(0.25, 0.75), na.rm = na.rm)
  iqr <- as.numeric(cuarts[2] - cuarts[1])
  range <- as.numeric(c(cuarts[1] - 1.5*iqr, cuarts[2] + 1.5*iqr))
  range[1] <- ifelse(range[1] < min_vec, min_vec, range[1])
  range[2] <- ifelse(range[1] > max_vec, max_vec, range[2])
  return(range)
}





pretty_print <- function(num) {
  num <- formatC(num, format="d", big.mark=',')
  return(num)
}










