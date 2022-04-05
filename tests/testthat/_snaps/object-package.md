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
      person_desc(comment = c(ORCID = "1234"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://orcid.org/1234}{ORCID})"
    Code
      person_desc(comment = c(ORCID = "https://orcid.org/1234"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://orcid.org/1234}{ORCID})"
    Code
      person_desc(comment = c(ORCID = "1234", "extra"))
    Output
      [1] "H W \\email{h@w.com} (\\href{https://orcid.org/1234}{ORCID}) (extra)"

