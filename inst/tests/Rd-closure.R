f <- function(){
   .a <- 0
   function(x = 1){
     .a <<- .a + x
     .a
   }
}

#' Addition function.
f2 <- f()
rm(f)
