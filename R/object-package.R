package_seealso <- function(desc) {
  itemize("Useful links:", package_seealso_urls(desc))
}
package_seealso_urls <- function(desc) {
  if (!is.null(desc$URL)) {
    links <- paste0("\\url{", strsplit(desc$URL, ",\\s+")[[1]], "}")
    links <- gsub("\\url\\{https://doi.org/", "\\doi{", links)
  } else {
    links <- character()
  }
  if (!is.null(desc$BugReports)) {
    links <- c(links, paste0("Report bugs at \\url{", desc$BugReports, "}"))
  }

  links
}


package_authors <- function(desc) {
  authors <- tryCatch(eval(parse(text = desc$`Authors@R` %||% "")),
    error = function(e) {
      warning(e)
      NULL
    }
  )
  if (is.null(authors))
    return()

  desc <- map_chr(unclass(authors), author_desc)
  type <- map_chr(unclass(authors), author_type)
  by_type <- split(desc, type)

  paste(
    c(
      paste0("\\strong{Maintainer}: ", by_type$cre[[1]], "\n"),
      itemize("Authors:", by_type$aut),
      itemize("Other contributors:", by_type$other)
    ),
    collapse = "\n"
  )
}

author_desc <- function(x) {
  if (inherits(x, "person")) {
    stop("person class must be stripped", call. = FALSE)
  }

  desc <- paste0(x$given, collapse = " ")

  if (!is.null(x$family)) {
    desc <- paste0(desc, " ", paste0(x$family, collapse = " "))
  }

  if (!is.null(x$email)) {
    desc <- paste0(desc, " \\email{", x$email, "}")
  }

  if (!is.null(x$comment)) {
    if (has_name(x$comment, "ORCID")) {
      orcid <- x$comment[["ORCID"]]

      if (grepl("https?://", orcid)) {
        desc <- paste0(desc, " (\\href{", orcid, "}{ORCID})")
      } else {
        desc <- paste0(desc, " (\\href{https://orcid.org/", orcid, "}{ORCID})")
      }
      x$comment <- x$comment[!names(x$comment) %in% "ORCID"]
    }

    if (length(x$comment) > 0) {
      desc <- paste0(desc, " (", x$comment, ")")
    }
  }

  extra_roles <- setdiff(x$role, c("cre", "aut"))
  if (length(extra_roles) > 0) {
    desc <- paste0(
      desc, " [", paste0(role_lookup[extra_roles], collapse = ", "), "]"
    )
  }

  desc
}

author_type <- function(x) {
  if ("cre" %in% x$role) {
    "cre"
  } else if ("aut" %in% x$role) {
    "aut"
  } else {
    "other"
  }
}

role_lookup <- c(
  "abr" = "abridger",
  "act" = "actor",
  "adp" = "adapter",
  "rcp" = "addressee",
  "anl" = "analyst",
  "anm" = "animator",
  "ann" = "annotator",
  "apl" = "appellant",
  "ape" = "appellee",
  "app" = "applicant",
  "arc" = "architect",
  "arr" = "arranger",
  "acp" = "art copyist",
  "adi" = "art director",
  "art" = "artist",
  "ard" = "artistic director",
  "asg" = "assignee",
  "asn" = "associated name",
  "att" = "attributed name",
  "auc" = "auctioneer",
  "aut" = "author",
  "aqt" = "author in quotations or text abstracts",
  "aft" = "author of afterword, colophon, etc.",
  "aud" = "author of dialog",
  "aui" = "author of introduction, etc.",
  "ato" = "autographer",
  "ant" = "bibliographic antecedent",
  "bnd" = "binder",
  "bdd" = "binding designer",
  "blw" = "blurb writer",
  "bkd" = "book designer",
  "bkp" = "book producer",
  "bjd" = "bookjacket designer",
  "bpd" = "bookplate designer",
  "bsl" = "bookseller",
  "brl" = "braille embosser",
  "brd" = "broadcaster",
  "cll" = "calligrapher",
  "ctg" = "cartographer",
  "cas" = "caster",
  "cns" = "censor",
  "chr" = "choreographer",
  "cng" = "cinematographer",
  "cli" = "client",
  "cor" = "collection registrar",
  "col" = "collector",
  "clt" = "collotyper",
  "clr" = "colorist",
  "cmm" = "commentator",
  "cwt" = "commentator for written text",
  "com" = "compiler",
  "cpl" = "complainant",
  "cpt" = "complainant-appellant",
  "cpe" = "complainant-appellee",
  "cmp" = "composer",
  "cmt" = "compositor",
  "ccp" = "conceptor",
  "cnd" = "conductor",
  "con" = "conservator",
  "csl" = "consultant",
  "csp" = "consultant to a project",
  "cos" = "contestant",
  "cot" = "contestant-appellant",
  "coe" = "contestant-appellee",
  "cts" = "contestee",
  "ctt" = "contestee-appellant",
  "cte" = "contestee-appellee",
  "ctr" = "contractor",
  "ctb" = "contributor",
  "cpc" = "copyright claimant",
  "cph" = "copyright holder",
  "crr" = "corrector",
  "crp" = "correspondent",
  "cst" = "costume designer",
  "cou" = "court governed",
  "crt" = "court reporter",
  "cov" = "cover designer",
  "cre" = "creator",
  "cur" = "curator",
  "dnc" = "dancer",
  "dtc" = "data contributor",
  "dtm" = "data manager",
  "dte" = "dedicatee",
  "dto" = "dedicator",
  "dfd" = "defendant",
  "dft" = "defendant-appellant",
  "dfe" = "defendant-appellee",
  "dgg" = "degree granting institution",
  "dgs" = "degree supervisor",
  "dln" = "delineator",
  "dpc" = "depicted",
  "dpt" = "depositor",
  "dsr" = "designer",
  "drt" = "director",
  "dis" = "dissertant",
  "dbp" = "distribution place",
  "dst" = "distributor",
  "dnr" = "donor",
  "drm" = "draftsman",
  "dub" = "dubious author",
  "edt" = "editor",
  "edc" = "editor of compilation",
  "edm" = "editor of moving image work",
  "elg" = "electrician",
  "elt" = "electrotyper",
  "enj" = "enacting jurisdiction",
  "eng" = "engineer",
  "egr" = "engraver",
  "etr" = "etcher",
  "evp" = "event place",
  "exp" = "expert",
  "fac" = "facsimilist",
  "fld" = "field director",
  "fmd" = "film director",
  "fds" = "film distributor",
  "flm" = "film editor",
  "fmp" = "film producer",
  "fmk" = "filmmaker",
  "fpy" = "first party",
  "frg" = "forger",
  "fmo" = "former owner",
  "fnd" = "funder",
  "gis" = "geographic information specialist",
  "hnr" = "honoree",
  "hst" = "host",
  "his" = "host institution",
  "ilu" = "illuminator",
  "ill" = "illustrator",
  "ins" = "inscriber",
  "itr" = "instrumentalist",
  "ive" = "interviewee",
  "ivr" = "interviewer",
  "inv" = "inventor",
  "isb" = "issuing body",
  "jud" = "judge",
  "jug" = "jurisdiction governed",
  "lbr" = "laboratory",
  "ldr" = "laboratory director",
  "lsa" = "landscape architect",
  "led" = "lead",
  "len" = "lender",
  "lil" = "libelant",
  "lit" = "libelant-appellant",
  "lie" = "libelant-appellee",
  "lel" = "libelee",
  "let" = "libelee-appellant",
  "lee" = "libelee-appellee",
  "lbt" = "librettist",
  "lse" = "licensee",
  "lso" = "licensor",
  "lgd" = "lighting designer",
  "ltg" = "lithographer",
  "lyr" = "lyricist",
  "mfp" = "manufacture place",
  "mfr" = "manufacturer",
  "mrb" = "marbler",
  "mrk" = "markup editor",
  "med" = "medium",
  "mdc" = "metadata contact",
  "mte" = "metal-engraver",
  "mtk" = "minute taker",
  "mod" = "moderator",
  "mon" = "monitor",
  "mcp" = "music copyist",
  "msd" = "musical director",
  "mus" = "musician",
  "nrt" = "narrator",
  "osp" = "onscreen presenter",
  "opn" = "opponent",
  "orm" = "organizer",
  "org" = "originator",
  "oth" = "other",
  "own" = "owner",
  "pan" = "panelist",
  "ppm" = "papermaker",
  "pta" = "patent applicant",
  "pth" = "patent holder",
  "pat" = "patron",
  "prf" = "performer",
  "pma" = "permitting agency",
  "pht" = "photographer",
  "ptf" = "plaintiff",
  "ptt" = "plaintiff-appellant",
  "pte" = "plaintiff-appellee",
  "plt" = "platemaker",
  "pra" = "praeses",
  "pre" = "presenter",
  "prt" = "printer",
  "pop" = "printer of plates",
  "prm" = "printmaker",
  "prc" = "process contact",
  "pro" = "producer",
  "prn" = "production company",
  "prs" = "production designer",
  "pmn" = "production manager",
  "prd" = "production personnel",
  "prp" = "production place",
  "prg" = "programmer",
  "pdr" = "project director",
  "pfr" = "proofreader",
  "prv" = "provider",
  "pup" = "publication place",
  "pbl" = "publisher",
  "pbd" = "publishing director",
  "ppt" = "puppeteer",
  "rdd" = "radio director",
  "rpc" = "radio producer",
  "rce" = "recording engineer",
  "rcd" = "recordist",
  "red" = "redaktor",
  "ren" = "renderer",
  "rpt" = "reporter",
  "rps" = "repository",
  "rth" = "research team head",
  "rtm" = "research team member",
  "res" = "researcher",
  "rsp" = "respondent",
  "rst" = "respondent-appellant",
  "rse" = "respondent-appellee",
  "rpy" = "responsible party",
  "rsg" = "restager",
  "rsr" = "restorationist",
  "rev" = "reviewer",
  "rbr" = "rubricator",
  "sce" = "scenarist",
  "sad" = "scientific advisor",
  "aus" = "screenwriter",
  "scr" = "scribe",
  "scl" = "sculptor",
  "spy" = "second party",
  "sec" = "secretary",
  "sll" = "seller",
  "std" = "set designer",
  "stg" = "setting",
  "sgn" = "signer",
  "sng" = "singer",
  "sds" = "sound designer",
  "spk" = "speaker",
  "spn" = "sponsor",
  "sgd" = "stage director",
  "stm" = "stage manager",
  "stn" = "standards body",
  "str" = "stereotyper",
  "stl" = "storyteller",
  "sht" = "supporting host",
  "srv" = "surveyor",
  "tch" = "teacher",
  "tcd" = "technical director",
  "tld" = "television director",
  "tlp" = "television producer",
  "ths" = "thesis advisor",
  "trc" = "transcriber",
  "trl" = "translator",
  "tyd" = "type designer",
  "tyg" = "typographer",
  "uvp" = "university place",
  "vdg" = "videographer",
  "vac" = "voice actor",
  "wit" = "witness",
  "wde" = "wood engraver",
  "wdc" = "woodcutter",
  "wam" = "writer of accompanying material",
  "wac" = "writer of added commentary",
  "wal" = "writer of added lyrics",
  "wat" = "writer of added text",
  "win" = "writer of introduction",
  "wpr" = "writer of preface",
  "wst" = "writer of supplementary textual content"
)

itemize <- function(header, x) {
  if (length(x) == 0)
    return()

  paste0(
    header, "\n",
    "\\itemize{\n",
    paste0("  \\item ", x, "\n", collapse = ""),
    "}\n"
  )
}


package_description_urls <- function(x) {

  # DOIs handling
  # target: from <doi:XX.XXX> to \doi{XX.XXX} to avoid CRAN Notes, etc.
  dois <- character()

  # Both structures are valid
  pattern_doi <- "<(DOI:|doi:)(.*?)>"
  extract_dois <- unlist(str_extract_all(
    x,
    pattern = pattern_doi
  ))

  dois <- c(dois, extract_dois)

  # Create replacement:
  replaced_dois <- dois
  if (length(replaced_dois) > 0) {
    replaced_dois <- gsub("^<(DOI|doi):|>$", "", replaced_dois)

    # Decode urls for documentation (see #1164)
    # Use apply for compatibility with older R versions
    replaced_dois <- unlist(lapply(replaced_dois, URLdecode))

    replaced_dois <- paste0("\\doi{", replaced_dois, "}")
  }

  # http(s) handling
  # target: from <http:XX.XXX> to \url{http:XX.XXX}
  urls <- character()

  pattern_url <- "<(http|https):\\/\\/(.*?)>"
  extract_url <- unlist(str_extract_all(
    x,
    pattern = pattern_url
  ))

  urls <- c(urls, extract_url)

  # Create replacement:
  replaced_urls <- urls
  if (length(replaced_urls) > 0) {
    replaced_urls <- gsub("^<|>$", "", replaced_urls)
    # Decode urls for documentation
    replaced_urls <- unlist(lapply(replaced_urls, URLdecode))

    # Additionally, encode just the spaces (CRAN Error if not) and mask the %
    replaced_urls <- gsub(" ", "\\%20", replaced_urls, fixed = TRUE)
    replaced_urls <- paste0("\\url{", replaced_urls, "}")
  }
  # ArXiv
  # target: from <arxiv:XXX> to \href{https://arxiv.org/abs/XXX}{arXiv:XXX}
  # This is how CRAN parses it, see: https://CRAN.R-project.org/package=alpaca
  # See https://github.com/wch/r-source/blob/trunk/src/library/tools/R/Rd2pdf.R

  arxivs <- character()

  # Pattern from https://github.com/wch/r-source, see previous link
  pattern_arxiv <- "<(arXiv:|arxiv:)([[:alnum:]/.-]+)([[:space:]]*\\[[^]]+\\])?>"
  extract_arxiv <- unlist(str_extract_all(
    x,
    pattern = pattern_arxiv
  ))

  arxivs <- c(arxivs, extract_arxiv)

  # Create replacement:
  replaced_arxivs <- arxivs
  if (length(replaced_arxivs) > 0) {
    replaced_arxivs <- gsub("^<(arXiv:|arxiv:)|>$", "", replaced_arxivs)

    # Some special cases has a format <arxiv:id [code]>. This is accepted on
    # CRAN, see https://CRAN.R-project.org/package=ciccr
    # Extract arxiv id, split by space
    arxiv_id <- strsplit(replaced_arxivs, " ")
    arxiv_id <- as.vector(lapply(arxiv_id, "[[", 1))

    replaced_arxivs <- paste0(
      "\\href{https://arxiv.org/abs/",
      arxiv_id,
      "}{arXiv:",
      replaced_arxivs, "}"
    )
  }

  # Final step: replace all
  inits <- c(dois, urls, arxivs)
  replace <- c(
    replaced_dois,
    replaced_urls,
    replaced_arxivs
  )

  # Yes, a loop
  if (length(inits) > 0 & length(inits) == length(replace)) {
    for (i in seq_len(length(inits))) {
      x <- gsub(inits[i], replace[i], x, fixed = TRUE)
    }
  }

  return(x)
}
