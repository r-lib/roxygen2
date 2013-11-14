parse_package <- function(base_path, load_code) {
  env <- load_code(base_path)
  parsed <- lapply(r_files(base_path), parse_file, env = env)
  
  unlist(parsed, recursive = FALSE)
}

parse_text <- function(text) {
  file <- tempfile()
  writeLines(text, file)
  on.exit(unlink(file))

  env <- new.env(parent = parent.env(globalenv()))
  attr(env, "hash") <- suppressWarnings(digest(env))
  setPackageName("roxygen_devtest", env)
  
  sys.source(file, envir = env)
  parse_file(file, env)
}

parse_file <- function(file, env, env_hash = attr(env, "hash")) {
  srcfile <- srcfile(file)
  
  lines <- readLines(file, warn = FALSE)
  hash <- c(env_hash, lines)
  
  if (parse_cache$has_key(hash)) {
    return(parse_cache$get_key(hash))
  }
  
  src_refs <- attributes(parse(srcfile$filename, srcfile = srcfile))$srcref
  pre_refs <- prerefs(srcfile, src_refs)

  if (length(src_refs) == 0) return(list())

  src_parsed <- lapply(src_refs, parse.srcref, env = env)
  pre_parsed <- lapply(pre_refs, parse.preref)

  stopifnot(length(src_parsed) == length(pre_parsed))

  partita <- mapply(c, src_parsed, pre_parsed, SIMPLIFY = FALSE)    
  parse_cache$set_key(hash, partita)
}

