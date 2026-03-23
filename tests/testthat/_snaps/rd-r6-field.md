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

# format.rd_r6_field produces \item markup

    Code
      cat(format(rd_r6_field("x", "A number.")))
    Output
      \item{\code{x}}{A number.}

# format.rd_r6_fields produces fields & bindings sections

    Code
      cat(format(fields), sep = "\n")
    Output
      \section{Public fields}{
        \if{html}{\out{<div class="r6-fields">}}
        \describe{
          \item{\code{x}}{A number.}
      
          \item{\code{y}}{A string.}
        }
        \if{html}{\out{</div>}}
      }

---

    Code
      cat(format(bindings), sep = "\n")
    Output
      \section{Active bindings}{
        \if{html}{\out{<div class="r6-active-bindings">}}
        \describe{
          \item{\code{val}}{A value.}
        }
        \if{html}{\out{</div>}}
      }

