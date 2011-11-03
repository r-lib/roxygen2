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
    out <- WrapFieldIfNecessary(field, value, wrap.threshold = 80)    
  }

  cat(out, sep='\n', file=file, append=TRUE)
}

# Determine whether a given field is too long and should be text-wrapped
WrapFieldIfNecessary <- function(field, value, wrap.threshold) {
   text <- MockFormattedText(field, value)
   longest.line <- max(nchar(text))
   
   if (longest.line > wrap.threshold) {
     text <- strwrap(sprintf('%s: %s', field, value), exdent = 4, width = wrap.threshold)
   }
   
   return(text)
}

# Simulate what was probably the user's intended field formatting
MockFormattedText <- function(field, value) {
  text <- str_split(sprintf("%s: %s", field, value), "\n")[[1]]
  number.of.lines <- length(text)
  
  if (number.of.lines > 1) {
    text[2:number.of.lines] <- LeftPadFourSpaces(text[2:number.of.lines])
  }
  text
}

LeftPadFourSpaces <- function(x) {
  sapply(x, FUN = function(x) (paste("    ", x, sep = "")), USE.NAMES = FALSE)
}

