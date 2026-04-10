test_that("r6_method_from_row extracts all components", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Say hello.
        #' @details Be polite.
        #' @param who Name.
        #' @return A greeting.
        #' @examples c$greet('world')
        greet = function(who) paste('hi', who)
      )
    )"
  docs <- r6_doc(text)
  method <- docs$methods$self[[1]]

  expect_equal(method$name, "greet")
  expect_equal(method$class, "C")
  expect_equal(method$description, "Say hello.")
  expect_equal(method$details, "Be polite.")
  expect_equal(method$params, list(list(name = "who", description = "Name.")))
  expect_equal(method$return, "A greeting.")
  expect_equal(method$examples, "c$greet('world')")
})

test_that("@returns works as method-level tag (#1148)", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Run.
        #' @returns The result.
        run = function() 1
      )
    )"
  docs <- r6_doc(text)
  expect_equal(docs$methods$self[[1]]$return, "The result.")
})

test_that("warns about multiple @return(s) tags", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Run.
        #' @return First.
        #' @returns Second.
        run = function() 1
      )
    )"
  expect_snapshot(docs <- r6_doc(text))
  expect_equal(docs$methods$self[[1]]$return, "First.")
})

test_that("class-level @param x,y inherited by methods (#1600)", {
  text <- "
    #' Class
    #' @param x,y Numbers.
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Method.
        meth = function(x, y) {}
      )
    )"
  expect_silent(docs <- r6_doc(text))
  expect_equal(
    docs$methods$self[[1]]$params,
    list(list(name = "x, y", description = "Numbers."))
  )
})

test_that("warns about undocumented params", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Run.
        run = function(x, y) NULL
      )
    )"
  expect_snapshot(docs <- r6_doc(text))
})

test_that("warns about duplicated params", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Run.
        #' @param x First.
        #' @param x Second.
        run = function(x) NULL
      )
    )"
  expect_snapshot(docs <- r6_doc(text))
})

test_that("initialize() inherits params from @field", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @field x A field.
        x = NULL,
        #' @field y Another field.
        y = NULL,
        #' @description Create.
        initialize = function(x, y) {
          self$x <- x
          self$y <- y
        }
      )
    )"
  docs <- r6_doc(text)
  init <- docs$methods$self[[1]]
  expect_equal(
    init$params,
    list(
      list(name = "x", description = "A field."),
      list(name = "y", description = "Another field.")
    )
  )
})

test_that("initialize() @param takes precedence over @field", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @field x A field.
        x = NULL,
        #' @description Create.
        #' @param x Custom description.
        initialize = function(x) {
          self$x <- x
        }
      )
    )"
  docs <- r6_doc(text)
  init <- docs$methods$self[[1]]
  expect_equal(
    init$params,
    list(list(name = "x", description = "Custom description."))
  )
})

test_that("format.rd_r6_method produces method subsection", {
  method <- rd_r6_method(
    name = "greet",
    class = "Person",
    formals = as.pairlist(alist(who = , how = "nicely")),
    description = "Say hello.",
    params = list(
      list(name = "who", description = "Name to greet."),
      list(name = "how", description = "Greeting style.")
    )
  )
  expect_snapshot(cat(format(method), sep = "\n"))
})

test_that("format.rd_r6_method renames initialize to new", {
  method <- rd_r6_method(
    name = "initialize",
    class = "Foo",
    formals = NULL,
    description = "Create object."
  )
  expect_snapshot(cat(format(method), sep = "\n"))
})

test_that("format.rd_r6_method includes optional sections", {
  method <- rd_r6_method(
    name = "run",
    class = "Job",
    formals = NULL,
    description = "Run the job.",
    details = "Some details.",
    return = "The result.",
    examples = "job$run()"
  )
  expect_snapshot(cat(format(method), sep = "\n"))
})

test_that("format.rd_r6_method strips \\dontrun etc from examples (#1072)", {
  method <- rd_r6_method(
    name = "run",
    class = "Job",
    formals = NULL,
    description = "Run the job.",
    examples = "
      \\dontrun{donrun()}
      \\donttest{donttest()}
      \\dontshow{dontshow()}
    "
  )
  expect_snapshot(cat(format(method), sep = "\n"))
})

test_that("format.rd_r6_method omits empty optional sections", {
  method <- rd_r6_method(
    name = "run",
    class = "Job",
    formals = NULL,
    description = "Run."
  )
  expect_snapshot(cat(format(method), sep = "\n"))
})

# Superclass param inheritance -----------------------------------------------

test_that("method inherits params from superclass method", {
  text <- "
    #' Parent
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @description Do something.
        #' @param x An x value.
        #' @param y A y value.
        run = function(x, y) NULL
      )
    )

    #' Child
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        #' @description Do something else.
        run = function(x, y) NULL
      )
    )"
  docs <- r6_doc(text)
  method <- docs$methods$self[[1]]
  expect_equal(
    method$params,
    list(
      list(name = "x", description = "An x value."),
      list(name = "y", description = "A y value.")
    )
  )
})

test_that("method inherits only missing params from superclass", {
  text <- "
    #' Parent
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @description Do something.
        #' @param x An x value.
        #' @param y A y value.
        run = function(x, y) NULL
      )
    )

    #' Child
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        #' @description Do something else.
        #' @param x Overridden x.
        run = function(x, y) NULL
      )
    )"
  docs <- r6_doc(text)
  method <- docs$methods$self[[1]]
  expect_equal(
    method$params,
    list(
      list(name = "x", description = "Overridden x."),
      list(name = "y", description = "A y value.")
    )
  )
})

test_that("method inherits class-level params from superclass", {
  text <- "
    #' Parent
    #' @param x An x value.
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @description Do something.
        run = function(x) NULL
      )
    )

    #' Child
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        #' @description Do something else.
        run = function(x) NULL
      )
    )"
  docs <- r6_doc(text)
  method <- docs$methods$self[[1]]
  expect_equal(
    method$params,
    list(list(name = "x", description = "An x value."))
  )
})

test_that("initialize() inherits params from superclass initialize()", {
  text <- "
    #' Parent
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @description Create.
        #' @param x An x value.
        #' @param y A y value.
        initialize = function(x, y) NULL
      )
    )

    #' Child
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        #' @description Create child.
        #' @param z A z value.
        initialize = function(x, y, z) NULL
      )
    )"
  docs <- r6_doc(text)
  init <- docs$methods$self[[1]]
  expect_equal(
    init$params,
    list(
      list(name = "x", description = "An x value."),
      list(name = "y", description = "A y value."),
      list(name = "z", description = "A z value.")
    )
  )
})

test_that("child initialize() inherits params from superclass @field", {
  text <- "
    #' Parent
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @field x An x value.
        x = NULL,
        #' @field y A y value.
        y = NULL,
        #' @description Create.
        initialize = function(x, y) {
          self$x <- x
          self$y <- y
        }
      )
    )

    #' Child
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        #' @description Create child.
        #' @param z A z value.
        initialize = function(x, y, z) {
          super$initialize(x, y)
        }
      )
    )"
  docs <- r6_doc(text)
  init <- docs$methods$self[[1]]
  expect_equal(
    init$params,
    list(
      list(name = "x", description = "An x value."),
      list(name = "y", description = "A y value."),
      list(name = "z", description = "A z value.")
    )
  )
})

test_that("method inherits params through multiple levels", {
  text <- "
    #' Grandparent
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @description Do something.
        #' @param x An x value.
        run = function(x) NULL
      )
    )

    #' Parent
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        #' @description Do something.
        #' @param x An x value.
        run = function(x) NULL
      )
    )

    #' Child
    C <- R6::R6Class('C', cloneable = FALSE,
      inherit = B,
      public = list(
        #' @description Do something else.
        run = function(x) NULL
      )
    )"
  docs <- r6_doc(text)
  method <- docs$methods$self[[1]]
  expect_equal(
    method$params,
    list(list(name = "x", description = "An x value."))
  )
})
