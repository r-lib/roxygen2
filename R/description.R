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
    width <- if (individual_lines) 0 else 80
    out <- wrap_field_if_necessary(field, value, wrap.threshold = width)
  }

  cat(out, sep='\n', file=file, append=TRUE)
}

# Determine whether a given field is too long and should be text-wrapped
wrap_field_if_necessary <- function(field, value, wrap.threshold) {
   text <- simulate_formatted_text(field, value)
   longest.line <- max(str_length(text))

   if (longest.line > wrap.threshold) {
     text <- str_wrap(str_c(field, ": ", value), exdent = 4, width = wrap.threshold)
   }

   return(text)
}

# Simulate what was probably the user's intended field formatting
simulate_formatted_text <- function(field, value) {
  text     <- str_split(str_c(field, ": ", value), "\n")[[1]]
  text[-1] <- str_c("    ", text[-1]) # indents all *but* the first line

  return(text)
}

