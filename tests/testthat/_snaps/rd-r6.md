# R6 edge cases, class without (documented) fields

    Code
      topic_add_r6_methods(rd, block, environment())
    Message
      x <text>:7: Undocumented R6 field: undocumented_field.

# warning if no method comes after the docs

    Code
      topic_add_r6_methods(rd, block, environment())
    Message
      x <text>:10: @description Cannot find matching R6 method.

# class with no inherited methods

    Code
      cat(format(rd$get_section("rawRd")))
    Output
      \section{Super class}{
      \code{R_GlobalEnv::C1} -> \code{C2}
      }
      \section{Methods}{
      \subsection{Public methods}{
      \itemize{
      \item \href{#method-C2-meth1}{\code{C2$meth1()}}
      }
      }
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-C2-meth1"></a>}}
      \if{latex}{\out{\hypertarget{method-C2-meth1}{}}}
      \subsection{Method \code{meth1()}}{
      method1
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{C2$meth1()}\if{html}{\out{</div>}}
      }
      
      }
      }

# integration test

    Code
      res <- roclet_process(roc, blocks = blocks, env = env, base_path = test_path())
    Message
      x roxygen-block-3.R:12: Must use one @param for each argument.
      x $meth3(duplicate) is documented multiple times
      x roxygen-block-3.R:12: Must use one @param for each argument.
      x $meth3(missing) is not documented
      x roxygen-block-3.R:12: Must use one @return per R6 method.
      x roxygen-block-3.R:91: Undocumented R6 method: undocumented_method.
      x roxygen-block-3.R:91: Undocumented R6 fields: field2 and undocumented_field.
      x roxygen-block-3.R:91: R6 field documented multiple times: duplicatefield.
      x roxygen-block-3.R:91: Unknown R6 field: nosuchfield.
      x roxygen-block-3.R:91: Undocumented R6 active binding: undocumented_binding.
      x roxygen-block-3.R:91: R6 active binding documented multiple times: duplicate_binding.

