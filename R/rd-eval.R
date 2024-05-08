roxy_eval <- function(expr, env) {
  local_reproducible_output()
  eval(expr, env)
}

roxy_knit <- function(text, envir, options) {
  old_opts <- purrr::exec(opts_chunk$set, options)
  withr::defer(purrr::exec(opts_chunk$set, old_opts))

  local_reproducible_output()
  knit(text = text, quiet = TRUE, envir = envir)
}

# Simplified from testthat::local_reproducible_output
local_reproducible_output <- function(.envir = parent.frame()) {
  withr::local_options(
    crayon.enabled = FALSE,
    cli.unicode = FALSE,
    cli.dynamic = FALSE,
    cli.hyperlink = FALSE,
    rlang_interactive = FALSE,
    width = 80,
    .local_envir = .envir
  )
  withr::local_envvar(RSTUDIO = NA, .local_envir = .envir)
  withr::local_collate("C", .local_envir = .envir)
}
