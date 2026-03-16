# format.rd_r6_methods with one method

    Code
      cat(format(methods), sep = "\n")
    Output
      \section{Methods}{
      \subsection{Public methods}{
      \itemize{
      \item \href{#method-Foo-run}{\code{Foo$run()}}
      }
      }
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-Foo-run"></a>}}
      \if{latex}{\out{\hypertarget{method-Foo-run}{}}}
      \subsection{Method \code{run()}}{
      Run it.
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{Foo$run()}
      \if{html}{\out{</div>}}
      }
      
      }
      }

# r6_all_examples aggregates across methods

    Code
      cat(r6_all_examples(docs$methods), sep = "\n")
    Output
      
      ## ------------------------------------------------
      ## Method `C$greet`
      ## ------------------------------------------------
      
      c$greet()
      
      ## ------------------------------------------------
      ## Method `C$stop`
      ## ------------------------------------------------
      
      c$stop()
      c$stop(force = TRUE)

