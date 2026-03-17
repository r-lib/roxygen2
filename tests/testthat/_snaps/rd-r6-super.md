# format.rd_r6_super with one superclass

    Code
      cat(format(supers), sep = "\n")
    Output
      \section{Super class}{
      \code{mypkg::Parent} -> \code{Child}
      }

# format.rd_r6_super omits pkg:: for same-package classes

    Code
      cat(format(supers), sep = "\n")
    Output
      \section{Super classes}{
      \code{otherpkg::GrandParent} -> \code{Parent} -> \code{Child}
      }

