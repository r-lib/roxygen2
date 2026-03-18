# warns about unmatched components 

    Code
      docs <- r6_doc(text)
    Message
      x <text>:6: @description Cannot find matching R6 method.

# format.rd_r6_class with fields

    Code
      cat(format(docs), sep = "\n")
    Output
      \section{Public fields}{
      \if{html}{\out{<div class="r6-fields">}}
      \describe{
      \item{\code{x}}{A number.}
      
      \item{\code{y}}{A string.}
      }
      \if{html}{\out{</div>}}
      }

# format.rd_r6_class with active bindings

    Code
      cat(format(docs), sep = "\n")
    Output
      \section{Active bindings}{
      \if{html}{\out{<div class="r6-active-bindings">}}
      \describe{
      \item{\code{val}}{A value.}
      }
      \if{html}{\out{</div>}}
      }

# format.rd_r6_class with no inherited methods

    Code
      cat(format(docs), sep = "\n")
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
      \if{html}{\out{<div class="r">}}
      \preformatted{C2$meth1()}
      \if{html}{\out{</div>}}
      }
      
      }
      }

# format.rd_r6_class with inherited methods

    Code
      cat(format(docs), sep = "\n")
    Output
      \section{Super class}{
      \code{R_GlobalEnv::A} -> \code{B}
      }
      \section{Methods}{
      \subsection{Public methods}{
      \itemize{
      \item \href{#method-B-shared}{\code{B$shared()}}
      \item \href{#method-B-clone}{\code{B$clone()}}
      }
      }
      \if{html}{\out{<details open><summary>Inherited methods</summary>
      <ul>
      <li><code>R_GlobalEnv::A$only_a()</code></li>
      </ul>
      </details>}}
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-B-shared"></a>}}
      \if{latex}{\out{\hypertarget{method-B-shared}{}}}
      \subsection{Method \code{shared()}}{
      Method from B.
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{B$shared()}
      \if{html}{\out{</div>}}
      }
      
      }
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-B-clone"></a>}}
      \if{latex}{\out{\hypertarget{method-B-clone}{}}}
      \subsection{Method \code{clone()}}{
      The objects of this class are cloneable with this method.
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{B$clone(deep = FALSE)}
      \if{html}{\out{</div>}}
      }
      
      \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
      \item{\code{deep}}{Whether to make a deep clone.}
      }
      \if{html}{\out{</div>}}
      }
      
      }
      }

# format.rd_r6_class with markdown sections

    Code
      cat(format(docs), sep = "\n")
    Output
      \section{Methods}{
      \subsection{Public methods}{
      \itemize{
      \item \href{#method-C-meth}{\code{C$meth()}}
      }
      }
      \if{html}{\out{<hr>}}
      \if{html}{\out{<a id="method-C-meth"></a>}}
      \if{latex}{\out{\hypertarget{method-C-meth}{}}}
      \subsection{Method \code{meth()}}{
      Method description.
      
      \subsection{Description section}{
      Description section body.
      }
      \subsection{Usage}{
      \if{html}{\out{<div class="r">}}
      \preformatted{C$meth()}
      \if{html}{\out{</div>}}
      }
      
      \subsection{Details}{
      \subsection{Details section}{
      Details section body.
      }
      }
      
      }
      }

