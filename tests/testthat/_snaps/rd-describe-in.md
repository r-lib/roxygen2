# Multiple @describeIn functions combined into one

    Code
      out$get_section("minidesc")
    Output
      \section{Related functions and methods}{
      \subsection{Functions}{
      \itemize{
        \item \code{square}: Square a number
        \item \code{cube}: Cube a number
      }
      }}
       

# multiple methods and others are combined into a generic

    Code
      out$get_section("minidesc")
    Output
      \section{Related functions and methods}{
      \subsection{Functions}{
      \itemize{
        \item \code{print.qux}: function (method for different generic)
        \item \code{zap_helper}: function
      }
      }
      \subsection{Methods extending \code{zap} generic (by class)}{
      \itemize{
        \item \code{numeric}: method
        \item \code{character}: method
      }
      }}
       

# multiple methods and others are combined into a class constructor

    Code
      out$get_section("minidesc")
    Output
      \section{Related functions and methods}{
      \subsection{Functions}{
      \itemize{
        \item \code{format.bar}: function (method for different class)
        \item \code{is_foo}: function
      }
      }
      \subsection{Methods extending \code{foo} class (by generic)}{
      \itemize{
        \item \code{print}: method
        \item \code{format}: method
      }
      }}
       

---

    Code
      out$get_section("minidesc")
    Output
      \section{Related functions and methods}{
      \subsection{Functions}{
      \itemize{
        \item \code{format.quuz_baz}: function (method for another class)
      }
      }
      \subsection{Methods extending \code{roxygen2_baz} class (by generic)}{
      \itemize{
        \item \code{print}: method
      }
      }}
       

# complains about bad usage

    [<text>:6] @describeIn must be used with an object

---

    [<text>:6] @describeIn can not be used with @name

---

    [<text>:6] @describeIn can not be used with @rdname

