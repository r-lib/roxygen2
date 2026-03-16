# warns about undocumented fields

    Code
      docs <- r6_doc(text)
    Message
      x <text>:3: Undocumented R6 field: undocumented_field.

# warns about fields documented multiple times

    Code
      docs <- r6_doc(text)
    Message
      x <text>:5: R6 field documented multiple times: x.

# warns about unknown fields

    Code
      docs <- r6_doc(text)
    Message
      x <text>:4: Undocumented R6 field: x.
      x <text>:4: Unknown R6 field: nosuch.

# warns about undocumented active bindings

    Code
      docs <- r6_doc(text)
    Message
      x <text>:3: Undocumented R6 active binding: undocumented.

# warns about active bindings documented multiple times

    Code
      docs <- r6_doc(text)
    Message
      x <text>:5: R6 active binding documented multiple times: b.

# format.rd_r6_field produces \item markup

    Code
      cat(format(rd_r6_field("x", "A number.")))
    Output
      \item{\code{x}}{A number.}

