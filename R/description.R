# Parse DESCRIPTION into convenient format
read.description <- function(file) {
  dcf <- read.dcf(file)
  
  dcf_list <- setNames(as.list(dcf[1, ]), colnames(dcf))
  lapply(dcf_list, str_trim)
}

# Write parsed DESCRIPTION back out to disk
write.description <- function(desc, file = "") {
  unlink(file)
  mapply(cat.description, names(desc), desc, MoreArgs = list(file = file))
  invisible()
}

# Print the field-value pair to a given file or standard out.
cat.description <- function(field, value, file='') {
  comma_sep <- any(field %in% c("Suggests", "Depends", "Extends", "Imports"))
  individual_lines <- field %in% c("Collate")
  
  if (comma_sep) {
    value <- strsplit(value, ",\\s+")[[1]]
    value <- gsub("^\\s+|\\s+$", "", value)
    value_string <- paste("    ", value, collapse = ",\n", sep = "")
    out <- paste(field, ":\n", value_string, sep = "")
  } else {
    width <- if (individual_lines) 0 else 60
    out <- strwrap(sprintf('%s: %s', field, value), exdent=4, width = width)    
  }

  cat(out, sep='\n', file=file, append=TRUE)
}
