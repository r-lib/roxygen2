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

# can autolink urls on package Description

    Code
      parsed
    Output
      [1] "\\url{https://github.com/} Secured \\url{https://github.com/}. No link <www.github.com/>."

# can autolink dois on package Description

    Code
      parsed
    Output
      [1] "\\doi{10.000/ret.234} With ampersands \\doi{aaa&.bbb.&c12}. No link <doi.baddoi>. I can use encoded dois \\doi{10.1175/1520-0469(1981)038<1179:TSLROA>2.0.CO;2}"

# can autolink arxiv on package Description

    Code
      parsed
    Output
      [1] "\\href{https://arxiv.org/abs/somecode}{arXiv:somecode} With upper \\href{https://arxiv.org/abs/somecode}{arXiv:somecode}."

