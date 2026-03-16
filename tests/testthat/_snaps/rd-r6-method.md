# warns about undocumented params

    Code
      docs <- r6_doc(text)
    Message
      x <text>:3: Must use one @param for each argument.
      x $run(x) is not documented
      x <text>:3: Must use one @param for each argument.
      x $run(y) is not documented

# warns about duplicated params

    Code
      docs <- r6_doc(text)
    Message
      x <text>:3: Must use one @param for each argument.
      x $run(x) is documented multiple times

# format.rd_r6_method produces method subsection

    Code
      cat(format(method), sep = "\n")
    Output
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-Person-greet"></a>}}
      \if{latex}{\out{\hypertarget{method-Person-greet}{}}}
      \subsection{Method \code{greet()}}{
      Say hello.
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{Person$greet(who, how = "nicely")}
      \if{html}{\out{</div>}}
      }
      
      \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
      \item{\code{who}}{Name to greet.}
      \item{\code{how}}{Greeting style.}
      }
      \if{html}{\out{</div>}}
      }
      
      }

# format.rd_r6_method renames initialize to new

    Code
      cat(format(method), sep = "\n")
    Output
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-Foo-new"></a>}}
      \if{latex}{\out{\hypertarget{method-Foo-new}{}}}
      \subsection{Method \code{new()}}{
      Create object.
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{Foo$new()}
      \if{html}{\out{</div>}}
      }
      
      }

# format.rd_r6_method includes optional sections

    Code
      cat(format(method), sep = "\n")
    Output
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-Job-run"></a>}}
      \if{latex}{\out{\hypertarget{method-Job-run}{}}}
      \subsection{Method \code{run()}}{
      Run the job.
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{Job$run()}
      \if{html}{\out{</div>}}
      }
      
      \subsection{Details}{
      Some details.
      }
      
      \subsection{Returns}{
      The result.
      }
      
      \subsection{Examples}{
      \if{html}{\out{<div class="r example copy">}}
      \preformatted{job$run()
      }
      \if{html}{\out{</div>}}
      }
      
      }

# format.rd_r6_method omits empty optional sections

    Code
      cat(format(method), sep = "\n")
    Output
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-Job-run"></a>}}
      \if{latex}{\out{\hypertarget{method-Job-run}{}}}
      \subsection{Method \code{run()}}{
      Run.
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{Job$run()}
      \if{html}{\out{</div>}}
      }
      
      }

