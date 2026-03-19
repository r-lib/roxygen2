rd_r6_method <- function(
  name,
  class,
  formals,
  description = character(),
  details = character(),
  params = list(),
  return = NULL,
  examples = character()
) {
  structure(
    list(
      name = name,
      class = class,
      formals = formals,
      description = description,
      details = details,
      params = params,
      return = return,
      examples = examples
    ),
    class = "rd_r6_method"
  )
}

#' @export
format.rd_r6_method <- function(x, ...) {
  lines <- character()
  push <- function(...) lines <<- c(lines, ...)
  push_subsection <- function(title, ...) {
    push(
      paste0("  \\subsection{", title, "}{"),
      paste0("    ", c(...)),
      "  }"
    )
  }

  # Anchor and heading
  call <- r6_method_name(x$class, x$name)

  id <- paste0("method-", x$class, "-", x$name)
  push(rd_if_html("<hr>"))
  push(rd_if_html('<a id="', id, '"></a>'))
  push(rd_if_latex("\\hypertarget{", id, "}{}"))

  push(paste0("\\subsection{\\code{", call, "()}}{"))

  # Description
  if (length(x$description) > 0) {
    push(
      paste0(
        "  ",
        sub("\n?\n?$", "\n\n", head(x$description, -1)),
        recycle0 = TRUE
      ),
      paste0("  ", utils::tail(x$description, 1), recycle0 = TRUE)
    )
  }

  # Usage
  fake <- paste(rep("X", nchar(call)), collapse = "")
  usage <- format(function_usage(fake, x$formals))
  push_subsection(
    "Usage",
    rd_if_html('<div class="r">'),
    paste0("\\preformatted{", sub(paste0("^", fake), call, usage), "}"),
    rd_if_html("</div>")
  )

  # Params
  if (length(x$params) > 0) {
    nms <- map_chr(x$params, \(p) p$name)
    vals <- map_chr(x$params, \(p) p$description)
    push_subsection(
      "Arguments",
      rd_if_html('<div class="arguments">'),
      "\\describe{",
      paste0("  \\item{\\code{", nms, "}}{", vals, "}"),
      "}",
      rd_if_html("</div>")
    )
  }

  # Details
  if (length(x$details) > 0) {
    push_subsection(
      "Details",
      sub("\n?\n?$", "\n\n", head(x$details, -1)),
      utils::tail(x$details, 1)
    )
  }

  # Return
  if (!is.null(x$return)) {
    push_subsection("Returns", x$return)
  }

  # Examples
  if (length(x$examples) > 0) {
    push_subsection(
      "Examples",
      rd_if_html('<div class="r example copy">'),
      paste0("\\preformatted{", x$examples, "\n", "}"),
      rd_if_html("</div>")
    )
  }

  # End
  push("}\n")

  lines
}

r6_method_from_row <- function(method, block) {
  tags <- method$tags[[1]]

  desc_tags <- keep(tags, \(t) t$tag == "description")
  description <- map_chr(desc_tags, \(x) x[["val"]])

  det_tags <- keep(tags, \(t) t$tag == "details")
  details <- map_chr(det_tags, \(x) x[["val"]])

  params <- r6_resolve_params(method, block)

  ret_tags <- keep(tags, \(t) t$tag %in% c("return", "returns"))
  if (length(ret_tags) > 1) {
    warn_roxy_block(block, "Must use one @return(s) per R6 method")
  }
  ret <- if (length(ret_tags) > 0) ret_tags[[1]]$val else NULL

  exa_tags <- keep(tags, \(t) t$tag == "examples")
  examples <- map_chr(exa_tags, \(x) x[["val"]])

  rd_r6_method(
    name = method$name,
    class = method$class,
    formals = method$formals[[1]],
    description = description,
    details = details,
    params = params,
    return = ret,
    examples = examples
  )
}

r6_resolve_params <- function(method, block) {
  tags <- method$tags[[1]]
  par <- keep(tags, \(t) t$tag == "param")
  nms <- gsub(",", ", ", map_chr(par, \(x) x[["val"]][["name"]]))

  mnames <- str_trim(unlist(strsplit(nms, ",")))
  dup <- unique(mnames[duplicated(mnames)])
  for (m in dup) {
    warn_roxy_block(
      block,
      c(
        "Must use one @param for each argument",
        x = "${method$name}({m}) is documented multiple times"
      )
    )
  }

  fnames <- names(method$formals[[1]])
  if (length(fnames) == 0) {
    return(list())
  }

  # Add missing from class-level @param
  miss <- setdiff(fnames, mnames)
  is_in_cls <- map_lgl(
    block$tags,
    function(t) {
      !is.na(t$line) &&
        t$line < block$line &&
        t$tag == "param" &&
        t$val$name %in% miss
    }
  )
  par <- c(par, block$tags[is_in_cls])

  # For initialize(), inherit from @field tags for any still-missing params
  if (method$name == "initialize") {
    nms <- gsub(",", ", ", map_chr(par, \(x) x[["val"]][["name"]]))
    mnames <- str_trim(unlist(strsplit(nms, ",")))
    miss <- setdiff(fnames, mnames)

    if (length(miss) > 0) {
      field_tags <- keep(block$tags, function(t) {
        t$tag == "field" && t$val$name %in% miss
      })
      field_as_param <- lapply(field_tags, function(t) {
        val <- list(name = t$val$name, description = t$val$description)
        roxy_generated_tag(block, "param", val)
      })
      par <- c(par, field_as_param)
    }
  }

  # Check if anything is still missing
  nms <- gsub(",", ", ", map_chr(par, \(x) x[["val"]][["name"]]))
  mnames <- str_trim(unlist(strsplit(nms, ",")))
  miss <- setdiff(fnames, mnames)
  for (m in miss) {
    warn_roxy_block(
      block,
      c(
        "Must use one @param for each argument",
        x = "${method$name}({m}) is not documented"
      )
    )
  }

  # Order them according to formals
  firstnames <- str_trim(
    map_chr(strsplit(map_chr(par, \(x) x[["val"]][["name"]]), ","), \(x) x[[1]])
  )
  par <- par[order(match(firstnames, fnames))]

  lapply(par, function(t) {
    list(
      name = gsub(",", ", ", t$val$name),
      description = t$val$description
    )
  })
}

r6_method_name <- function(class, method) {
  paste0(class, "$", ifelse(method == "initialize", "new", method))
}

rd_if_html <- function(...) {
  paste0("\\if{html}{\\out{", ..., "}}")
}

rd_if_latex <- function(...) {
  paste0("\\if{latex}{\\out{", ..., "}}")
}
