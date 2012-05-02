context("Rd - S4")
roc <- rd_roclet()

test_that("Method documentation has correct defaults", {
  
			
  .test <- function(chunk, ...){
	  out <- roc_proc_text(roc, chunk)[[1]]	  
	  expect_equal(get_tag(out, "alias")$values, "show,numeric-method", info="Alias is correct", ...)
	  expect_equal(names(get_tag(out, "arguments")$values), c("object"), info="Argument names are correct", ...)
  }

  # named arguments to setMethod
  .test("
	  #' Title.
	  setMethod('show', 'numeric', function(object){ show(NA) })
	  ", label="unnamed arguments")

  # unnamed arguments to setMethod
  .test("
		#' Title.
		setMethod(f='show', signature='numeric', function(object){ show(NA) })
	", label="named arguments")
	
})

test_that("setReplaceMethod for base generic: documentation has correct defaults", {
	
	.test <- function(chunk, args=NULL
			, sig="A,ANY,ANY", usage="(fun, envir = environment(fun)) <- value", ...){
		clear_caches()
		chunk <- str_c("setClass('A')\n\n", chunk)
		out <- roc_proc_text(roc, chunk)[[1]]
		# check alias
		expect_equal(get_tag(out, "alias")$values, str_c("body<-,", sig,'-method')
				, info="Alias is correct", ...)
		# check usage
		expect_equal(get_tag(out, "usage")$values, str_c("\\S4method{body}{", sig, "}", usage)
				, info="Usage is correct", ...)

		# check arguments
		if( is.null(args) )
			expect_identical(names(get_tag(out, "arguments")$values), c("fun", "envir", "value"), info="Argument names are correctly inherited from generic", ...)
		else{
			expect_identical(get_tag(out, "arguments")$values, args, info="Argument documentation is correct", ...)
		}
	}
	
	# named arguments to setMethod
	.test("
		#' Title.
		setReplaceMethod('body', 'A', function(fun, envir, value){ fun })
		", label="unnamed arguments")
	
	# unnamed arguments to setMethod
	.test("
		#' Title.
		setReplaceMethod(f='body', signature='A', function(fun, envir, value){ fun })
		", label="named arguments")
	
	# overriding argument documentation
	.test("		
		#' Title.
		#' @param fun an object of class A
		#' @param envir a crazy environment
		#' @param value replacement
		setReplaceMethod(f='body', 'A', function(fun, envir, value){ fun })
		", args=c(fun="an object of class A", envir="a crazy environment", value="replacement")
		, label="override arguments")
	
	# specific signatures
	.test("
		#' Title.
		setReplaceMethod(f='body', signature('A', 'character', 'numeric'), function(fun, envir, value){ fun })
		", label="Fully specified ordered signature"
		, sig = "A,character,numeric")
	.test("
		#' Title.
		setReplaceMethod(f='body', signature('A', 'numeric'), function(fun, envir, value){ fun })
	", label="Partially specified ordered signature"
	, sig = "A,numeric,ANY")
	.test("
		#' Title.
		setReplaceMethod(f='body', signature(value='A', fun='numeric'), function(fun, envir, value){ fun })
		", label="Partially specified unordered signature"
	, sig = "numeric,ANY,A")

})
	

test_that("setReplaceMethod for local documented generic: documentation is correct", {
			
	.test <- function(chunk, args=NULL
					, sig="numeric,ANY,ANY", usage="(object, other) <- value", ...){
		
		chunk <- str_c("#' Dummy Replace Generic
			#' @param object some object
			#' @param other other argument
			#' @param value replacement
			setGeneric('dummy<-', function(object, other, value) standardGeneric('dummy<-'))\n\n"
			, chunk)
		out <- roc_proc_text(roc, chunk)[[2]]
		# check alias
		expect_equal(get_tag(out, "alias")$values, str_c("dummy<-,", sig,'-method')
					, info="Alias is correct", ...)
		# check usage
		expect_equal(get_tag(out, "usage")$values, str_c("\\S4method{dummy}{", sig, "}", usage)
					, info="Usage is correct", ...)
		# check arguments
		if( is.null(args) ){
			expect_identical(get_tag(out, "arguments")$values
					, c(object="some object", other="other argument", value="replacement")
					, info="Argument documentation are correctly inherited from generic", ...)
		}else{
			expect_identical(get_tag(out, "arguments")$values, args
					, info="Argument documentation is correct", ...)
		}
	}
	
	# named arguments to setMethod
	.test("
		#' Title.
		setReplaceMethod('dummy', 'numeric', function(object, other, value){ object })
		", label="unnamed arguments")

	# unnamed arguments to setMethod
	.test("		
		#' Title.
		setReplaceMethod(f='dummy', signature='numeric', function(object, other, value){ object })
		", label="named arguments")

	# overriding argument documentation
	.test("		
		#' Title.
		#' @param object a numeric vector
		setReplaceMethod(f='dummy', 'numeric', function(object, other, value){ object })
		", args=c(object="a numeric vector", other="other argument", value="replacement")
		, label="Partial override of generic arguments")
	.test("		
		#' Title.
		#' @param object a numeric vector
		#' @param other completely different argument
		#' @param value nothing
		setReplaceMethod(f='dummy', 'numeric', function(object, other, value){ object })
		", args=c(object="a numeric vector", other="completely different argument", value="nothing")
		, label="Complete override of generic arguments")

	# specific signatures
	.test("
		#' Title.
		setReplaceMethod(f='dummy', signature('character', 'function', 'numeric'), function(object, other, value){ object })
		", label="Fully specified ordered signature"
		, sig = "character,function,numeric")
	.test("
		#' Title.
		setReplaceMethod(f='dummy', signature('character', 'numeric'), function(object, other, value){ object })
		", label="Partially specified ordered signature"
		, sig = "character,numeric,ANY")
	.test("
		#' Title.
		setReplaceMethod(f='dummy', signature(value='character', object='numeric'), function(object, other, value){ object })
		", label="Partially specified unordered signature"
		, sig = "numeric,ANY,character")
	
})

test_that("Method documentation has no defaults when generic NOT documented", {
  chunk <- "
    setGeneric('blah', function(object, ...){
      standardGeneric('blah')
    })
    #' Title.
    setMethod('blah', 'numeric', function(object, extra){ show(NA) })
    "
  
  expect_warning(out <- roc_proc_text(roc, chunk)[[1]], "can't find parent topic .*blah"
	, info="Warning is thrown if target topic for @inheritParams is not found") 
  expect_equal(get_tag(out, "alias")$values, "blah,numeric-method")
  expect_equal(get_tag(out, "arguments")$values, NULL)
})

test_that("Method documentation of inheritParams", {
	chunk <- "
		#' @name blah
		#' @rdname toto
		myfun <- function(){}

		#' A generic
		setGeneric('blah', function(object, ...){ standardGeneric('blah') })

		#' Title.
		setMethod('blah', 'numeric', function(object, extra){ show(NA) })
		"

	#expect_warning(out <- roc_proc_text(roc, chunk)[[3]], "multiple matches for parent topic .*blah"
	#		, info="Warning is thrown if target topic for @inheritParams is matched multiple times")
	out <- roc_proc_text(roc, chunk)[[3]]
	expect_equal(get_tag(out, "alias")$values, "blah,numeric-method")
	expect_equal(get_tag(out, "arguments")$values, NULL)
})

test_that("Method documentation has correct defaults when generic is documented", {
	out <- roc_proc_text(roc, "
	#' Blah.
	#'
	#' @param object a nice blah object 
	setGeneric('blah', function(object){
	standardGeneric('blah')
	})
	#' Title.
	setMethod('blah', 'numeric', function(object){ show(NA) })
	")[[2]]
	
	expect_equal(get_tag(out, "alias")$values, "blah,numeric-method")
	expect_equal(get_tag(out, "arguments")$values, c(object="a nice blah object"))
})


test_that("generic documentation generated correctly", {
  out <- roc_proc_text(roc, "
    #' My foo function.
    #'
    #' Foo will have S4 methods
    #' @param object my object
    setGeneric('foo', function(object){
        standardGeneric('foo')
    })    
    ")[[1]]
  expect_equal(get_tag(out, "usage")$values, "foo(object)")
  expect_equal(get_tag(out, "alias")$values, c("foo", "foo-methods"))
})

test_that("Replacement generic documentation generated correctly", {
	out <- roc_proc_text(roc, "
	#' My foo function.
	#'
	#' Foo will have S4 methods
	#' @param object my object
	#' @param value replacement value
	setGeneric('foo<-', function(object, value){ standardGeneric('foo<-')})")[[1]]

	expect_equal(get_tag(out, "usage")$values, "foo(object) <- value"
				, info="Usage is correct")
	expect_equal(get_tag(out, "alias")$values, c("foo<-", "foo<--methods")
				, info="Aliases are correct")
	expect_equal(get_tag(out, "arguments")$values, c(object="my object", value="replacement value")
				, info="Arguments are correct")
		
	out <- roc_proc_text(roc, "
	#' My foo function.	
	setGeneric('foo<-', function(object, value2, value){ standardGeneric('foo<-')})")[[1]]
	expect_equal(get_tag(out, "usage")$values, "foo(object, value2) <- value"
		, info="Usage with other argument named 'value2' is correct")

   # with '...' arguments
	out <- roc_proc_text(roc, "
	#' My foo function.	
	setGeneric('foo<-', function(object, ..., value){ standardGeneric('foo<-')})")[[1]]
	expect_equal(get_tag(out, "usage")$values, "foo(object, ...) <- value"
			, info="Usage with other argument '...' is correct")

		
})

stripUTF8 <- function(x){
	gsub("\u{A0}"," ",x, fixed=TRUE)
}

test_that("@usage and alias for S4methods", {
			
  out <- roc_proc_text(roc, "
    #' Title.
    setMethod('show', signature = c(object = 'array'), function (object) {})
    ")[[1]]
  expect_equal(get_tag(out, "usage")$values,
    "\\S4method{show}{array}(object)")
  expect_equal(get_tag(out, "alias")$values,
    c("show,array-method"))

	# with arguments '...'
	out <- roc_proc_text(roc, "
	#' Title.
	setMethod('summary', 'array', function (object, ...) {})
	")[[1]]
	expect_equal(get_tag(out, "usage")$values,
			"\\S4method{summary}{array}(object, ...)"
			, info="Usage is correct if argument '...'")
	expect_equal(get_tag(out, "alias")$values,
			c("summary,array-method"), info="Alias are correct if argument '...'")
	
	# with other extra arguments
	out <- roc_proc_text(roc, "
	#' New Generic
	setGeneric('gen', function(a, b=3, ...) standardGeneric('gen'))
	#' Title.
	setMethod('gen', signature('numeric', 'numeric'), function (a=1, b, c, d=NULL, ...) {})
	")[[2]]
	expect_identical(stripUTF8(get_tag(out, "usage")$values)
			, "\\S4method{gen}{numeric,numeric}(a = 1, b = 3, c, d = NULL, ...)"
			, info="Usage is correct if other extra arguments")
	expect_equal(get_tag(out, "alias")$values,
			c("gen,numeric,numeric-method"), info="Alias are correct if other extra arguments")
})

test_that("S4 classes have correct aliases", {
	
	# check only default alias NAME-class if a function exists with same name
	.test <- function(chunk, aliases, ...){
		
		chunk <- str_c("
		#' Important class.
		#' 
		setClass('AAA')
		", chunk, "\n")
		out <- roc_proc_text(roc, chunk)[[1]]
		
		expect_equal(get_tag(out, "alias")$values, aliases
				, info="Aliases are correct", ...)
	}
	
	.test(""
		, aliases=c("AAA", "AAA-class")
		, label="No function exists with same name")
	.test("AAA <- function(){}"
		, aliases=c("AAA", "AAA-class")
		, label="A standard function exists with same name")
	.test("setGeneric('AAA', function(object) standardGenric('AAA'))"
		, aliases=c("AAA", "AAA-class")
		, label="An S4 generic exists with same name")
	
})

test_that("@slot creates a new section and lists slots", {
  out <- roc_proc_text(roc, "
    #' Important class.
    #' 
    #' @slot a This is slot a
    #' @slot b This other slot is b
    setClass('test')
  ")[[1]]
  
  expect_equal(get_tag(out, "slot")$values, c(a="This is slot a", b="This other slot is b"))
})


test_that("function allFormals works correctly", {
			
	e <- new.env()
	eval(parse(text="
	fun <- function(a, b, c, d=4, ...){}
	e <- environment()
	setGeneric('gen', function(a, b=10, ...) standardGeneric('gen'), where=e)
	setMethod('gen', signature('numeric', 'numeric'), fun, where=e)
	meth <- getMethod('gen', signature('numeric', 'numeric'), where=e)
	
	setGeneric('gen2', function(a=3, b=10, ...) standardGeneric('gen2'), where=e)
	fun2 <- function(a, b=5, c, d=4, ...){}
	setMethod('gen2', signature('numeric', 'numeric'), fun2, where=e)
	meth2 <- getMethod('gen2', signature('numeric', 'numeric'), where=e)
	"), e)
	
	expect_identical(allFormals(e$gen), formals(function(a, b=10, ...){})
		, info="On the generic: returns external arguments")
	expect_identical(formals(e$meth), formals(function(a, b=10, ...){})
		, info="On method: normal formal does not see internal arguments")
	
	arg <- formals(e$fun)
	marg <- allFormals(e$meth)
	expect_identical(marg[names(marg) != 'b'], arg[names(arg) != 'b']
		, info="On method WITHOUT default on dispatch: allFormals sees internal arguments with no defaults")
	expect_identical(marg$b, 10
		, info="On method WITHOUT default on dispatch: allFormals uses defaults values from generic")
	
	arg <- formals(e$fun2)
	marg <- allFormals(e$meth2)
	expect_identical(marg[!names(marg) %in% c('a','b')], arg[!names(arg)  %in% c('a','b')]
		, info="On method WITH default on dispatch: allFormals sees internal arguments with no defaults")
	expect_identical(marg$a, 3
		, info="On method WITH default on dispatch: allFormals sees internal arguments and use default values from generic if none is defined on method")
	expect_identical(marg$b, 5
		, info="On method WITH default on dispatch: allFormals sees internal arguments and uses defaults values from method if defined")
})
