# @R6method tag parser validates input

    Code
      . <- roxy_tag_parse(roxy_tag("R6method", ""))
    Message
      x NA:NA: @R6method requires a value like `Class$method`.
    Code
      . <- roxy_tag_parse(roxy_tag("R6method", "nomethod"))
    Message
      x NA:NA: @R6method must be of the form `Class$method`.
    Code
      . <- roxy_tag_parse(roxy_tag("R6method", "Foo$bar\nextra"))
    Message
      x NA:NA: @R6method must be only 1 line long, not 2.
      i The first line is "Foo$bar"

# @R6method warns on unknown class

    Code
      roc_proc_text(rd_roclet(), text)
    Message
      x <text>:2: @R6method Can't find R6 class <NoSuchClass>.
    Output
      list()

