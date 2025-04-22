# person turned into meaningful text

    Code
      # Multiple given/family names
      person_desc(c("First", "Second"), c("Family1", "Family2"))
    Output
      [1] "First Second Family1 Family2 \\email{h@w.com}"
    Code
      # Multiple roles
      person_desc(role = "ctb")
    Output
      [1] "H W \\email{h@w.com} [contributor]"
    Code
      # ORCID comments
      person_desc(comment = c(ORCID = "0000-0003-4757-117X"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://orcid.org/0000-0003-4757-117X}{ORCID})"
    Code
      person_desc(comment = c(ORCID = "https://orcid.org/0000-0003-4757-117X"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://orcid.org/0000-0003-4757-117X}{ORCID})"
    Code
      person_desc(comment = c(ORCID = "0000-0003-4757-117X", "extra"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://orcid.org/0000-0003-4757-117X}{ORCID}) (extra)"
    Code
      # ROR comments
      person_desc(comment = c(ROR = "03wc8by49"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://ror.org/03wc8by49}{ROR})"
    Code
      person_desc(comment = c(ROR = "https://ror.org/03wc8by49"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://ror.org/03wc8by49}{ROR})"
    Code
      person_desc(comment = c(ROR = "03wc8by49", "extra"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://ror.org/03wc8by49}{ROR}) (extra)"

# useful message if Authors@R is corrupted

    Code
      package_authors("1 + ")
    Message
      x Failed to evaluate Authors@R.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: 1 + 
         ^
    Output
      NULL
    Code
      package_authors("stop('Uhoh')")
    Message
      x Failed to evaluate Authors@R.
      Caused by error:
      ! Uhoh
    Output
      NULL

