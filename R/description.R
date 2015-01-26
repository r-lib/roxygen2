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

  if (comma_sep) {
    value <- strsplit(value, ",\\s+")[[1]]
    value <- gsub("^\\s+|\\s+$", "", value)
    value_string <- paste("    ", value, collapse = ",\n", sep = "")
    out <- paste(field, ":\n", value_string, sep = "")
  } else {
    width <- 80
    if (field %in% c("Collate")) {
      # Individual lines
      width <- 0L
    } else if (field %in% c("Authors@R")) {
      # No wrapping
      width <- Inf
    }

    out <- wrap_field_if_necessary(field, value, wrap.threshold = width)
  }

  cat(out, sep='\n', file=file, append=TRUE)
}

# Determine whether a given field is too long and should be text-wrapped
wrap_field_if_necessary <- function(field, value, wrap.threshold = 80L) {

  if (identical(wrap.threshold, 0L)) {
    text <- paste0(field, ": ", value)
    # 0 is a special case: this is only used for collate where we want
    # one file name per line. stringr::str_wrap does paragraph wrapping
    # which is not what we want
    return(paste(strwrap(text, exdent = 4, width = 0), collapse = "\n"))
  }

  text <- simulate_formatted_text(field, value)
  longest.line <- max(str_length(text))

  if (longest.line > wrap.threshold) {
    text <- paste0(field, ": ", value)
    text <- str_wrap(text, exdent = 4, width = wrap.threshold)
  }

  return(text)
}

# Simulate what was probably the user's intended field formatting
simulate_formatted_text <- function(field, value) {
  text     <- str_split(paste0(field, ": ", value), "\n")[[1]]
  text[-1] <- paste0("    ", text[-1]) # indents all *but* the first line

  return(text)
}

