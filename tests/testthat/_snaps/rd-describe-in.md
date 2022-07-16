# Multiple @describeIn functions combined into one

    Code
      out$get_section("minidesc")
    Output
      \section{Functions}{
      \itemize{
      \item \code{square()}: Square a number
      
      \item \code{cube()}: Cube a number
      
      }} 

# multiple methods and others are combined into a generic

    Code
      out$get_section("minidesc")
    Output
      \section{Methods (by class)}{
      \itemize{
      \item \code{zap(numeric)}: method
      
      \item \code{zap(character)}: method
      
      }}
      \section{Functions}{
      \itemize{
      \item \code{print(qux)}: function (method for different generic)
      
      \item \code{zap_helper()}: function
      
      }} 

# multiple methods and others are combined into a class constructor

    Code
      out$get_section("minidesc")
    Output
      \section{Methods (by generic)}{
      \itemize{
      \item \code{print(foo)}: method
      
      \item \code{format(foo)}: method
      
      }}
      \section{Functions}{
      \itemize{
      \item \code{format(bar)}: function (method for different class)
      
      \item \code{is_foo()}: function
      
      }} 

---

    Code
      out$get_section("minidesc")
    Output
      \section{Methods (by generic)}{
      \itemize{
      \item \code{print(roxygen2_baz)}: method
      
      }}
      \section{Functions}{
      \itemize{
      \item \code{format(quuz_baz)}: function (method for another class)
      
      }} 

# infix and replacement names get nice label

    Code
      out$get_section("minidesc")
    Output
      \section{Functions}{
      \itemize{
      \item \code{x \%foo\% y}: infix
      
      \item \code{foo(x) <- value}: replacement for foo
      
      }} 

# s4 methods get nice label

    Code
      out$get_section("minidesc")
    Output
      \section{Methods (by generic)}{
      \itemize{
      \item \code{m_id(foo1)}: function
      
      }}
      \section{Functions}{
      \itemize{
      \item \code{m_id()}: generic
      
      }} 

---

    Code
      out$get_section("minidesc")
    Output
      \section{Methods (by class)}{
      \itemize{
      \item \code{bar1(x = foo2, y = foo3)}: method1
      
      \item \code{bar1(x = foo3, y = foo2)}: method2
      
      }} 

# complains about bad usage

    [<text>:6] @describeIn must be used with an object

---

    [<text>:6] @describeIn can not be used with @name

---

    [<text>:6] @describeIn can not be used with @rdname

