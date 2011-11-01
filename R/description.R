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

# Determine whether a given field is too long and should be text-wrapped
wrap_field_if_necessary <- function(field, value) {
   # if (lines lengths exceed wrap threshold) {
   # strwrap(sprintf('%s: %s', field, value), exdent=4, width = width)
   return(0)
}

mock_formatted_text <- function(field, value) {
  text <- str_split(sprintf("%s: %s", field, value), "\n")[[1]]
  number.of.lines <- length(text)
  
  if (number.of.lines > 1) {
    text[2:number.of.lines] <- leftPadNSpaces(text[2:number.of.lines], n = 4)
  }
  text
}

leftPadNSpaces <- function(x, n) {
  sapply(x, FUN = function(x) (str_pad(string = x, width = (nchar(x) + n), side = "left")), USE.NAMES = FALSE)
}

