# Name --------------------------------------------------------------------

object_name <- function(x) UseMethod("object_name")
object_name.s4class <-   function(x) x$value@className
object_name.s4generic <- function(x) x$value@generic
object_name.s4method <-  function(x) x$value@generic
object_name.rcclass <-   function(x) x$value@className
object_name.rcmethod <-  function(x) x$value@name
object_name.s3generic <- function(x) stop("Unsupported type", call. = FALSE)
object_name.s3method <-  function(x) attr(x$value, "s3method")
object_name.function <-  function(x) x$alias
object_name.default <-   function(x) NULL

# Topic name --------------------------------------------------------------

object_topic <- function(x) UseMethod("object_topic")

object_topic.s4method <- function(x) {
  sig <- paste0(x$value@defined, collapse = ",")
  paste0(x$value@generic, ",", sig, "-method")
}

object_topic.s4class <- function(x) {
  paste0(x$value@className, "-class")
}

object_topic.rcclass <- function(x) {
  paste0(x$value@className, "-class")
}

object_topic.rcmethod <- function(x) {
  x@name
}

object_topic.default <- function(x) {
  if (length(x$alias) == 1) return(x$alias)
  object_name(x)
}
