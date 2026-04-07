# @prop creates Additional properties section

    Code
      out$get_section("prop")
    Output
      \section{Additional properties}{
      
      \describe{
      \item{\code{@a}}{prop a}
      
      \item{\code{@b}}{prop b}
      }}
       

# @prop class@name groups by class

    Code
      out$get_section("prop")
    Output
      \section{Additional properties}{
      
      \subsection{Parent}{
      
      \describe{
      \item{\code{@x}}{prop x}
      }
      }
      
      \subsection{Child}{
      
      \describe{
      \item{\code{@y}}{prop y}
      }
      }
      }
       

# @prop with mismatched braces warns and doesn't crash

    Code
      . <- roc_proc_text(rd_roclet(), text)
    Message
      x <text>:4: @prop has mismatched braces or quotes.

# @prop class@name warns on invalid spec

    Code
      . <- roc_proc_text(rd_roclet(), text)
    Message
      x <text>:4: @prop must have form class@prop.

