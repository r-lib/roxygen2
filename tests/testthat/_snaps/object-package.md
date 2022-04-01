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

# package description not affected if no links

    Code
      parsed
    Output
      [1] "A simple description with no links."

# can autolink urls on package Description

    Code
      parsed
    Output
      [1] "\\url{https://github.com/} Secured \\url{https://github.com/}. No link <www.github.com/>. url masked with spaces \\url{https://database.ich.org/sites/default/files/Q1E\\%20Guideline.pdf} url fully masked \\url{https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=OJ:L:2015:012:TOC}"

# can autolink dois on package Description

    Code
      parsed
    Output
      [1] "\\doi{10.000/ret.234} With ampersands \\doi{aaa&.bbb.&c12}. No link <doi.baddoi>. I can use encoded dois \\doi{10.1175/1520-0469(1981)038<1179:TSLROA>2.0.CO;2}"

# can autolink arxiv on package Description

    Code
      parsed
    Output
      [1] "\\href{https://arxiv.org/abs/somecode}{arXiv:somecode} With upper \\href{https://arxiv.org/abs/somecode2}{arXiv:somecode2}. Strange arxiv \\href{https://arxiv.org/abs/2004.08318}{arXiv:2004.08318 [econ.EM]} Old-style arxiv \\href{https://arxiv.org/abs/quant-ph/0208069}{arXiv:quant-ph/0208069}"

